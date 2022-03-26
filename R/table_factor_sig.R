#' Build pretty table of factor significance and level counts
#' @param df_table output from mits_selection_factor_tables()
#' @param print_columns specific columns from the nested tibbles in the table column to print. Defaults to "MITS" and "non-MITS+DSS-only". "DSS-only" and "non-MITS" are the other two columns that can be printed.
#' @param percent_digits the number of digits to round the percent columns to. Defaults to 1.
#' @export
table_factor_sig_stats <- function(
  df_table,
  print_columns = c("MITS", "non-MITS+DSS-only"),
  percent_digits = 1
) {
  spacer <- ""
  if ("dss" %in% names(df_table)) {
    dss_str <- ifelse(df_table$dss[1],
      "<span style='color: gray;'>(DSS)</span>",
      "<span style='color: gray;'>(non-DSS)</span>"
    )
    if (!df_table$dss[1])
      spacer <- "<br>"
  } else {
    dss_str <- ""
  }

  if (!any(grepl("DSS", names(df_table$table[[1]]))) &&
    all(c("MITS", "non-MITS") %in% names(df_table$table[[1]]))) {
    print_columns <- c("MITS", "non-MITS")
  }

  # identify drop columns
  count_columns <- c("DSS-only", "MITS", "non-MITS", "non-MITS+DSS-only")
  drop_columns <- count_columns[!count_columns %in% print_columns]
  alternating_bg <- "#efefef"
  gt_id <- gt::random_id()

  # expand the nested tables
  dat <- suppressWarnings(df_table %>%
    dplyr::select(site, catchment, factor, table) %>%
    tidyr::unnest(cols = c("table")) %>%
    dplyr::select(-dplyr::one_of(drop_columns)))

  if ("non-MITS+DSS-only" %in% names(dat)) {
    dat <- dat %>%
      dplyr::rename(
        "non-MITS +<br>DSS-only" = "non-MITS+DSS-only",
        "<br>MITS" = "MITS"
      )
  }
  if (spacer != "") {
    nms <- names(dat)
    names(dat)[nms == "MITS"] <- "<br>MITS"
    names(dat)[nms == "non-MITS"] <- "<br>non-MITS"
  }

  # add percent columns
  dat_out <- dat %>%
    dplyr::left_join(
      # percent table calculations to join
      dat %>%
        dplyr::group_by(factor, .drop = FALSE) %>%
        dplyr::mutate(dplyr::across(
          tidyselect:::where(is.numeric),
          function(x) {
            round(100 * proportions(x), percent_digits)
          }
        )) %>%
        dplyr::rename_if(is.numeric,
          .funs = function(x) {
            paste0(x, "_percent")
          }
        ) %>%
        dplyr::mutate(
          dplyr::across(tidyselect:::where(is.numeric),
            ~tidyr::replace_na(.x, 0))
        ),
      by = c("site", "catchment", "factor", "level")
    )

  # need all levels of all factors
  factor_groups <- attr(df_table, "factor_groups")
  lnms <- names(valid_levels)
  all_facs <- dplyr::bind_rows(lapply(seq_along(valid_levels), function(idx) {
    if (!is.null(factor_groups[[lnms[idx]]])) {
      cur_lvls <- names(factor_groups[[lnms[idx]]])
    } else {
      cur_lvls <- valid_levels[[idx]]
    }
    dplyr::tibble(
      site = dat_out$site[1],
      catchment = dat_out$catchment[1],
      factor = lnms[idx],
      level = cur_lvls,
      factor_order = idx,
      level_order = seq_along(cur_lvls)
    )
  }))
  dat_out <- dplyr::left_join(all_facs, dat_out,
    by = c("site", "catchment", "factor", "level")) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(flevel = paste0(factor, "_", level)) %>%
    dplyr::arrange_at(c("factor_order", "level_order")) %>%
    dplyr::select(-c("factor_order", "level_order"))

  faclvls <- valid_factors
  grays <- faclvls[seq(1, length(faclvls), by = 2)]

  # start dt table structure.
  dat_gt <- dat_out %>%
    gt::gt(rowname_col = "flevel", id = gt_id) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = alternating_bg)
      ),
      locations = gt::cells_body(
        rows = (.data$factor %in% grays)
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = alternating_bg)
      ),
      locations = gt::cells_stub(
        rows = (.data$factor %in% grays)
      )
    )

  # Using while loops to move through dataframes of an unpsecified number
  # - of columns to combine percentages.
  # - factors
  # - of columns to rename with counts

  # First while loop creates the count/percent columns

  # Start with the percent columns in the dat_out object
  while_names <- colnames(dat_out)[stringr::str_detect(
    colnames(dat_out),
    "_percent"
  )]

  while (length(while_names) > 0) {
    pct_name <- while_names[1]
    n_name <- stringr::str_remove(pct_name, "_percent")

    dat_gt <- dat_gt %>%
      gt::cols_merge(
        columns = c(dplyr::all_of(n_name), dplyr::all_of(pct_name)),
        pattern = "{1} ({2})"
      )

    while_names <- while_names[-1]
  }

  # Second while loop to build factor grouping of the table
  #
  while_factors <- rev(dplyr::pull(df_table, factor))

  while (length(while_factors) > 0) {
    df_row <- dplyr::filter(df_table, factor == while_factors[1])

    # create pvalue_str
    pvalue <- dplyr::pull(df_row, pval)

    pvalue_str <- ifelse(
      pvalue < 0.001,
      "<0.001",
      paste0(round(pvalue, digits = 3))
    )

    # create factor_title
    factor_title <- stringr::str_to_title(df_row$factor)
    if (df_row$factor == "va")
      factor_title <- "VA CoD"

    # create percent_str
    percent_str <- paste0(round(df_row$pct_na, 2))

    # identify rows for grouping
    combine_rows <- dat_out %>%
      dplyr::filter(factor == df_row$factor) %>%
      dplyr::pull(flevel)

    # build formatting of table
    factor_info <- paste0(
      "__", factor_title, "__ _P-value: ",
      pvalue_str, "_ _, Missing: ", percent_str, "%_"
    )

    dat_gt <- dat_gt %>%
      gt::tab_row_group(
        label = gt::md(factor_info), rows = as.character(combine_rows),
        id = as.character(df_row$factor)
      )

    # remove previously built factor
    while_factors <- while_factors[-1]
  }

  # add other adornments to table.
  # N is the max observed over each of the factors
  dat_sum <- dat %>%
    group_by(factor, .drop = FALSE) %>%
    summarise_if(is.numeric, sum) %>%
    summarise_if(is.numeric, max)

  # Third while add column count details and rename columns to n (%)

  gt_while_columns <- colnames(dat_sum)

  while (length(gt_while_columns) > 0) {
    create_name <- gt_while_columns[1]
    column_title <- paste0(
      create_name, "<br><em>N = ",
      prettyNum(pull(dat_sum, create_name), big.mark = ","), "</em>"
    )

    dat_gt <- dat_gt %>%
      gt::tab_spanner(
        label = gt::md(column_title),
        columns = dplyr::all_of(create_name)
      ) %>%
      gt::cols_label({{ create_name }} := "n (%)")

    gt_while_columns <- gt_while_columns[-1]
  }
  dat_gt %>%
    gt::cols_hide(columns = c(site, catchment, factor, level)) %>%
    # gt::tab_source_note(
    #   source_note = gt::md("Built using the champsmortality R package.")
    # ) %>% # nolint
    gt::tab_stubhead(label = gt::md("__Factors__")) %>%
    gt::tab_header(
      title = gt::md(paste0("__", df_row$site, "__")),
      subtitle = gt::html(paste(df_row$catchment, dss_str))
    ) %>%
    gt::text_transform(
      locations = gt::cells_stub(),
      fn = function(x) {
        stringr::str_split_fixed(x, n = 2, pattern = "_") %>%
        .[, 2]
      }
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = alternating_bg)
      ),
      locations = gt::cells_row_groups(
        grays
      )
    ) %>%
    gt::tab_options(
      table.font.size = gt::px(12),
      data_row.padding = gt::px(3),
      heading.padding = gt::px(0),
      heading.subtitle.font.size = gt::pct(100),
      container.overflow.x = "unset",
      container.overflow.y = "unset",
      table.font.names = c("Poppins", "sans-serif")
    )
}
