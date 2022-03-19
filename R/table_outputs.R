#' Build pretty table of factor significance and level counts
#' @param df_table output from mits_selection_factor_tables()
#' @param print_columns specific columns from the nested tibbles in the table column to print. Defaults to "MITS" and "non-MITS+DSS-only". "DSS-only" and "non-MITS" are the other two columns that can be printed.
#' @export
build_table_location <- function(
  df_table,
  print_columns = c("MITS", "non-MITS+DSS-only")
) {
  # identify drop columns
  count_columns <- c("DSS-only", "non-MITS", "MITS", "non-MITS+DSS-only")
  drop_columns <- count_columns[!count_columns %in% print_columns]

  # expand the nested tables
  dat <- df_table %>%
    dplyr::select(site, catchment, factor, table) %>%
    tidyr::unnest(cols = c("table")) %>%
    dplyr::select(-dplyr::one_of(drop_columns))

  # add percent columns
  dat_out <- dat %>%
    dplyr::left_join(
      # percent table calculations to join
      dat %>%
        dplyr::group_by(factor) %>%
        dplyr::mutate(dplyr::across(
          tidyselect:::where(is.numeric),
          function(x) {
            round(100 * proportions(x), 2)
          }
        )) %>%
        dplyr::rename_if(is.numeric,
          .funs = function(x) {
            paste0(x, "_percent")
          }
        ),
      by = c("site", "catchment", "factor", "level")
    )

  # start dt table structure.
  dat_gt <- dat_out %>%
    gt::gt(rowname_col = "level")

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
      gt::cols_merge_n_pct(
        col_n = dplyr::all_of(n_name),
        col_pct = dplyr::all_of(pct_name)
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

    # create percent_str
    percent_str <- paste0(round(df_row$pct_na, 2))

    # identify rows for grouping
    combine_rows <- dat_out %>%
      dplyr::filter(factor == df_row$factor) %>%
      dplyr::pull(level)

    # build formatting of table
    factor_info <- paste0(
      "__", factor_title, "__ _P-value: ",
      pvalue_str, "_ _, Missing (%): ", percent_str, "_"
    )

    dat_gt <- dat_gt %>%
      gt::tab_row_group(
        label = gt::md(factor_info), rows = as.character(combine_rows)
      )

    # remove previously built factor
    while_factors <- while_factors[-1]
  }

  # add other adornments to table.
  # N is the max observed over each of the factors
  dat_sum <- dat %>%
    group_by(factor) %>%
    summarise_if(is.numeric, sum) %>%
    summarise_if(is.numeric, max)

  # Third while add column count details and rename columns to n (%)

  gt_while_columns <- colnames(dat_sum)

  while (length(gt_while_columns) > 0) {
    create_name <- gt_while_columns[1]
    column_title <- paste0(
      create_name, "<br>N = ",
      pull(dat_sum, create_name)
    )

    dat_gt <- dat_gt %>%
      gt::tab_spanner(
        label = gt::md(column_title),
        columns = create_name
      ) %>%
      gt::cols_label({{ create_name }} := "n (%)")

    gt_while_columns <- gt_while_columns[-1]
  }

  dat_gt %>%
    gt::cols_hide(columns = c(site, catchment, factor)) %>%
    gt::tab_source_note(
      source_note = gt::md("Built using the champsmortality R package.")
    ) %>% # nolint
    gt::tab_stubhead(label = gt::md("__Factors__")) %>%
    gt::tab_header(
      title = gt::md(paste0("__", df_row$site, "__")),
      subtitle = df_row$catchment
    )
}
