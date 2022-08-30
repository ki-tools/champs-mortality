#' Selected variables decision table
#' @param obj an object that comes from [get_rates_and_fractions()]
#' @param alpha_value the threshold to compare against the p-value.
#' Cells are colored based on this value
#' @param pmissing the percent missing allowed for a factor to be considered
#' in the decision table. Cells are colored using this value.
#' @param percent_digits the number of digits to round the percent missing to.
#' @importFrom gt cell_borders cell_fill cell_text cells_body
#' cells_column_spanners cols_label cols_merge fmt_number gt html md
#' opt_align_table_header opt_css px tab_footnote tab_header tab_options
#' tab_style text_transform
#' @export
table_adjust_decision <- function(
  obj,
  alpha_value = 0.1,
  pmissing = 20,
  percent_digits = 1
) {
  full_color <- "#4E79A7"
  partial_color <- "#4E79A7AA"

  if (inherits(obj, "rate_frac_site")) {
    tmp1 <- obj$mits %>%
      filter(!is.na(factor)) %>%
      mutate(dss = obj$can_use_dss) %>%
      dplyr::arrange_at(c("site", "catchment"))
    tmp2 <- obj$cond %>%
      filter(!is.na(factor)) %>%
      mutate(dss = obj$can_use_dss) %>%
      dplyr::arrange_at(c("site", "catchment"))

    cond_name_short <- obj$cond_name_short
  } else if (inherits(obj, "rate_frac_multi_site")) {
    tmp1 <- lapply(obj, function(x) {
        x$mits %>%
          filter(!is.na(factor)) %>%
          dplyr::mutate(dss = x$can_use_dss)}) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange_at(c("site", "catchment"))
    tmp2 <- lapply(obj, function(x) {
        x$cond %>%
          filter(!is.na(factor)) %>%
          dplyr::mutate(dss = x$can_use_dss) }) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange_at(c("site", "catchment"))
    cond_name_short <- obj[[1]]$inputs$cond_name_short
  } else {
    stop("'obj' must come from get_rates_and_fractions()")
  }

  tmp <- list(
    MITS = tmp1,
    other = tmp2
  )
  names(tmp)[2] <- cond_name_short
  tbl <- combine_decision_tables(tmp)

  tmp <- levels(tbl$factor)
  tmp <- stringr::str_to_title(tmp)
  tmp[tmp == "Va"] <- "VA CoD"
  levels(tbl$factor) <- tmp

  if (! "dss" %in% names(tbl)) {
    has_non_dss <- FALSE
    tbl$dss <- ""
  } else {
    has_non_dss <- TRUE
    tbl$dss <- ifelse(tbl$dss, "", "*")
  }

  # build the wide table that has location by row and
  # all the missing and pvalue columns for every factor by column
  dat_wide <- tbl %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("value")),
      .funs = function(x) {
        ifelse(x < .001, .001, x)
      }
    ) %>%
    tidyr::pivot_wider(
      id_cols = c("site", "catchment", "start_year", "end_year", "dss"),
      names_from = "factor",
      values_from = c(
        dplyr::contains("value"),
        dplyr::contains("Missing")
      ),
      names_sort = TRUE
    ) %>%
    dplyr::mutate(
      location_name = paste0(.data$site, ": ", .data$catchment, sep = ""),
      year_period = paste0(.data$start_year, "-", .data$end_year, sep = "")
    ) %>%
    dplyr::select(
      dplyr::all_of(c("location_name", "year_period", "dss")),
      dplyr::contains("value"),
      dplyr::contains("Missing")
    ) %>%
    dplyr::rename_if(is.numeric, .funs = function(x) {
      values <- stringr::str_split_fixed(x, "_", n = 3) %>%
        data.frame()
      values %>%
        dplyr::mutate(new_name = paste0(
          .data$X3, ".", .data$X1, ".", .data$X2
        )) %>%
        dplyr::pull(.data$new_name)
    })

  # Tease out the two list names assigned to the column
  # names for using in naming below
  # first name will be the first list item from the combine_decision_tables()
  table_names <- dat_wide %>%
    dplyr::select(dplyr::contains("value")) %>%
    colnames(.) %>%
    stringr::str_split_fixed("\\.", n = 3) %>%
    .[, 2] %>%
    unique()

  # start base table with initial formatting.
  gt_table <- dat_wide %>%
    gt::gt() %>%
    gt::fmt_number(
      columns = dplyr::contains("value"),
      decimals = 3,
      use_seps = FALSE
    ) %>%
    gt::fmt_number(
      columns = dplyr::contains("Missing"),
      decimals = percent_digits,
      use_seps = FALSE
    ) %>%
    # This doesn't work when we join the columns
    # need to figure this part out.
    gt::text_transform(
      locations = gt::cells_body(columns = dplyr::contains("value")),
      fn = function(x) {
        ifelse(x <= .001, "<0.001", x)
      }
    ) %>%
    gt::cols_merge(
      columns = c("location_name", "dss", "year_period"),
      pattern = "<b>{1}</b> <span style='color: gray;'>{2}</span><br><em>{3}</em>"
    ) |>
    # new code here assumes that these are the only
    # variable groupings.
    gt::tab_spanner(
      label = "Age",
      columns = contains("Age")
      ) |>
    gt::tab_spanner(
      label = "Sex",
      columns = contains("Sex")
    ) |> gt::tab_spanner(
      label = "Education",
      columns = contains("Education")
      ) |>
    gt::tab_spanner(
      label = "Season",
      columns = contains("Season") 
    ) |>
    gt::tab_spanner(
      label = "Location",
      columns = contains("Location", ignore.case = FALSE)
    ) |>
    gt::tab_spanner(
      label = "VA CoD",
      columns = contains("VA CoD")
    )

  # since missing columns and p-value columns are the same name with the
  # the exception of the label.
  # Will use P-value column for while loops that do both.
  pvalue_columns <- dat_wide %>%
    dplyr::select(dplyr::contains("P-value")) %>%
    colnames(.)

  # map the columns into the while columns for table formatting
  while_columns <- pvalue_columns

  # This while loop cleans up all the column names and
  # joins the p-value and missing columns into one column
  while (length(while_columns) > 0) {
    col1 <- while_columns[1]
    col2 <- stringr::str_replace(col1, "P-value", "Missing")
    group <- ifelse(
      stringr::str_detect(col1, table_names[1]),
      paste0(
        table_names[1],
        "<br><em><span style='font-size:smaller'>P-value<br>(Missing)</span></em>"
      ),
      paste0(
        table_names[2],
        "<br><em><span style='font-size:smaller'>P-value<br>(Missing)</span></em>"
      )
    )

    gt_table <- gt_table %>%
      gt::cols_merge(
        columns = c(dplyr::all_of(col1), dplyr::all_of(col2)),
        # might have to think through this part to create <0.001.
        pattern = "{1}<br>({2}%)"
      ) %>%
      gt::cols_label(
        {{ col1 }} := gt::html(group)
      )

    while_columns <- while_columns[-1]
  }

  # Start the next while loop to color code cells.
  fill_names <- pvalue_columns

  # set color styling for the cells.
  while (length(fill_names) > 0) {
    col1 <- fill_names[1]
    col2 <- stringr::str_replace(col1, "P-value", "Missing")

    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#eeeeee")
        ),
        locations = gt::cells_body(
          columns = {{ col1 }}
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = partial_color),
          gt::cell_text(color = "white")
        ),
        locations = gt::cells_body(
          columns = {{ col1 }},
          rows = (.data[[col1]] < alpha_value)
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = full_color),
          gt::cell_text(color = "white")
        ),
        locations = gt::cells_body(
          columns = {{ col1 }},
          rows = (
            (.data[[col1]] < alpha_value) &
              (.data[[col2]] < pmissing))
        )
      )

      if (grepl(paste0("\\.", table_names[2], "\\."), col1)) {
        gt_table <- gt_table %>%
          gt::tab_style(
            style = gt::cell_borders(
              sides = "right",
              color = "white",
              style = "solid",
              weight = gt::px(8)
            ),
            locations = gt::cells_body(columns = {{ col1 }})
          )
      }

    fill_names <- fill_names[-1]
  }

  if (has_non_dss) {
    gt_table <- gt_table %>%
      gt::tab_footnote(footnote = gt::html(
        "* includes catchments with no DSS data \u2014 see <a href='https://ki-tools.github.io/champs-mortality/articles/methodology.html' target='_blank'>here</a> for details about the methodology",
      ))
  }

  # final formatting of the table for output.
  gt_table %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_spanners()
    ) %>%
    gt::cols_label(
      location_name = ""
    ) %>%
    gt::tab_header(
      title = gt::md(paste0("__Potential adjustment factors__")),
      subtitle = gt::html(glue::glue(
        '<span style="background-color:{full_color};color:White">Blue: </span>\
                <em>&nbsp; P-value < 0.1 & Missing < 20%</em> <b>,</b>
                <span style="background-color:{partial_color};color:White"> Light blue: </span>\
                <em>&nbsp; P-value < 0.1</em>'
      ))
    ) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::tab_options(table.font.names = c("Poppins", "sans-serif")) %>%
    gt::opt_css(
      css = ".gt_col_heading { line-height: 18px; }"
    )
}

#' Combine mits_factor_tables() and cond_factor_tables() output to one
#' decision dataframe
#' @param tables_dat a named list object with a table
#' from mits_factor_tables() and a table from cond_factor_tables().
#' The names assigned to each table will be used in naming the table columns.
combine_decision_tables <- function(tables_dat) {
  append_names <- names(tables_dat)

  join_cols <- c(
    "site", "catchment", "factor",
    "start_year", "end_year", "dss"
  )
  # only need these two columns for tables
  unique_cols <- c("pval", "pct_na")
  # Clean names for columns to take advantage of dt::tab_spanner_delim()
  unique_cols_new <- c("P-value", "Missing")

  # now downselect the two tables to necessary columns and join
  t1 <- tables_dat[[append_names[1]]] %>%
    dplyr::select(dplyr::any_of(c(join_cols, unique_cols))) %>%
    dplyr::rename_with(~unique_cols_new, dplyr::all_of(unique_cols)) %>%
    dplyr::rename_with(
      .fn = function(x) {
        paste(append_names[1], x, sep = "_")
      },
      .cols = dplyr::any_of(unique_cols_new)
    )

  t2 <- tables_dat[[append_names[2]]] %>%
    dplyr::select(dplyr::any_of(c(join_cols, unique_cols))) %>%
    dplyr::rename_with(~unique_cols_new, dplyr::all_of(unique_cols)) %>%
    dplyr::rename_with(
      .fn = function(x) {
        paste(append_names[2], x, sep = "_")
      },
      .cols = dplyr::any_of(unique_cols_new)
    )

  if (! "dss" %in% names(t1))
    join_cols <- setdiff(join_cols, "dss")
  t1 %>%
    dplyr::left_join(t2, by = join_cols)
}