#' Combine mits_selection_factor_tables() and cond_factor_tables() output to one decision dataframe
#' @param tables_dat a named list object with a table from mits_selection_factor_tables() and a table from cond_factor_tables().  The names assigned to each table will be used in naming the table columns.
combine_decision_tables <- function(tables_dat) {

    append_names <- names(tables_dat)

    join_cols <- c("site", "catchment", "factor",
        "start_year", "end_year")
    # only need these two columns for tables
    unique_cols <- c("pval", "pct_na")
    # Clean names for columns to take advantage of dt::tab_spanner_delim()
    unique_cols_new <- c("P-value", "Missing")

    # now downselect the two tables to necessary columns and join
    t1 <- tables_dat[[append_names[1]]] %>%
        dplyr::select(dplyr::any_of(c(join_cols, unique_cols))) %>%
        dplyr::rename_with(~unique_cols_new, dplyr::all_of(unique_cols)) %>%
        dplyr::rename_with(.fn = function(x) {
            paste(append_names[1], x, sep = "_")},
            .cols = dplyr::any_of(unique_cols_new))

    t2 <- tables_dat[[append_names[2]]] %>%
        dplyr::select(dplyr::any_of(c(join_cols, unique_cols))) %>%
        dplyr::rename_with(~unique_cols_new, dplyr::all_of(unique_cols)) %>%
        dplyr::rename_with(.fn = function(x) {
            paste(append_names[2], x, sep = "_")},
            .cols = dplyr::any_of(unique_cols_new))

    t1 %>%
        dplyr::left_join(t2, by = join_cols)
}

# tables_dat <- list(
#     MITS = fac_tbl, # table 5a
#     CBD = cbd_tbl # table 6a
# )

# tables_dat_b <- list(
#     MITS = fac_tbl_b, # table 5b
#     CBD = cbd_tbl_b # table 6b
# )
# a_dat <- combine_decision_tables(tables_dat)
# b_dat <- combine_decision_tables(tables_dat_b)



#' Selected variables decision table
#' @param df_table a dataframe with the output from combine_decision_tables()
#' @param alpha_value the threshold to compare against the p-value. Cells are colored based on this value
#' @param pmissing the percent missing allowed for a factor to be considered in the decision table. Cells are colored using this value.
#' @param percent_digits the number of digits to round the percent missing to.
#' @export
build_table_selected <- function(
    df_table,
    alpha_value = 0.1,
    pmissing = 20,
    percent_digits = 1) {

    # build the wide table that has location by row and 
    #all the missing and pvalue columns for every factor by column
    dat_wide <- df_table %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("value")),
            .funs = function(x) {ifelse(x < .001, .001, x)}) %>%
        tidyr::pivot_wider(
            site:end_year,
            names_from = factor,
            values_from = c(dplyr::contains('value'),
                dplyr::contains('Missing')),
            names_sort = TRUE
        ) %>%
        dplyr::mutate(
            location_name = paste0(site, ": ", catchment, sep = ""),
            year_period = paste0(start_year, "-", end_year, sep = "")
        ) %>%
        dplyr::select(location_name, year_period,
            dplyr::contains("value"),
            dplyr::contains("Missing")) %>%
        dplyr::rename_if(is.numeric, .funs = function(x) {
            values <- stringr::str_split_fixed(x, "_", n = 3) %>% data.frame()
            values %>%
                dplyr::mutate(new_name = paste0(
                    stringr::str_to_title(X3), ".", X1, ".", X2)) %>%
                dplyr::pull(new_name)

        })
    # Tease out the two list names assigned to the column
    # names for using in naming below
    # first name will be the first list item from the combine_decision_tables()
    table_names <- dat_wide %>%
        dplyr::select(dplyr::contains("value")) %>%
        colnames(.) %>%
        stringr::str_split_fixed("\\.", n = 3) %>%
        .[,2] %>%
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
            columns = c("location_name", "year_period"),
            pattern = "<b>{1}</b><br><em>{2}</em>"
        ) %>%
        gt::tab_spanner_delim(
            delim = ".",
            split = "first"
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
            paste0(table_names[1],
                "<br><em><span style='font-size:smaller'>P-value<br>(Missing)</span></em>"),
            paste0(table_names[2],
                "<br><em><span style='font-size:smaller'>P-value<br>(Missing)</span></em>"))

        gt_table <- gt_table %>%
            gt::cols_merge(
                columns = c(dplyr::all_of(col1), dplyr::all_of(col2)),
                # might have to think through this part to create <0.001.
                pattern = "{1}<br>({2}%)") %>%
            gt::cols_label(
                {{col1}} := gt::html(group))

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
                    gt::cell_fill(color = "#808080"),
                    gt::cell_text(color = "white")
                ),
                locations = gt::cells_body(
                    columns = {{col1}},
                    rows = (.data[[col1]] < alpha_value)
                )) %>%
            gt::tab_style(
                style = list(
                    gt::cell_fill(color = "black"),
                    gt::cell_text(color = "white")
                ),
                locations = gt::cells_body(
                    columns = {{col1}},
                    rows = (
                        (.data[[col1]] < alpha_value) &
                        (.data[[col2]] < pmissing))
                ))

        fill_names <- fill_names[-1]
    }

    # final formatting of the table for output.
    gt_table %>%
        gt::tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_column_spanners()
        ) %>%
        gt::cols_label(
            location_name = ""
        ) %>%
        gt::tab_header(
            title = gt::md(paste0("__Potential adjustment factors__")),
            subtitle = gt::html(
                '<span style="background-color:Black;color:White">Black: </span>\
                 <em>&nbsp; P-value < 0.1 & Missing < 20%</em> <b>,</b> 
                <span style="background-color:#808080;color:White"> Grey: </span>\
                <em>&nbsp; P-value < 0.1</em>'
                )
        ) %>%
        gt::opt_align_table_header(align = "left") %>%
        gt::tab_style(
            style = cell_borders(
                sides = c("right"),
                color = "lightgrey"
            ),
            locations = cells_body()
        )
}

# tables_dat <- list(
#     MITS = fac_tbl, # table 5a
#     CBD = cbd_tbl # table 6a
# )

# tables_dat_b <- list(
#     MITS = fac_tbl_b, # table 5b
#     CBD = cbd_tbl_b # table 6b
# )
# a_dat <- combine_decision_tables(tables_dat)
# b_dat <- combine_decision_tables(tables_dat_b)
# build_table_selected(a_dat, alpha_value = 0.1, pmissing = 20, percent_digits = 1)
# build_table_selected(b_dat, alpha_value = 0.1, pmissing = 20)