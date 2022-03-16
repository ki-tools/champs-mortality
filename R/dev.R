
#' Combine levels in the level column of counts tables
#' @param x tibbles from table column found in output of mits_selection_factor_tables()
#' @param level_definitions is a named list that identifies the levels to combine.
#' @example table_example = structure(list(level = structure(1:4, .Label = c("Stillbirth", "Neonate", "Infant", "Child"), class = "factor"), `DSS-only` = c(4, 22, 40, 36), `non-MITS` = c(9, 12, 3, 3), MITS = c(73, 52, 31, 43), `non-MITS+DSS-only` = c(13, 62, 43, 91)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))
#' new_levels = list("Stillbirth" = "Stillbirth", "Neonate" = "Neonate", "Infants and Children" = c("Infant", "Child"))
#' new_levels_drop = list("Neonate" = "Neonate", "Infants and Children" = c("Infant", "Child"))
#' combine_levels(table_example, new_levels)
#' combine_levels(table_example, new_levels_drop)
#' @export
combine_levels <- function(dat, level_definitions = NULL) {

    # check that level is a factor. If not create
    if (is.null(level_definitions)) {
        out <- dat
        warning("No updated levels defined.\
         Using default levels from the input data.")
    } else {
        out <- level_definitions %>%
            purrr::map(~tibble::as_tibble_col(.x,column_name = "level")) %>%
            dplyr::bind_rows(.id = 'new_levels') %>%
            dplyr::right_join(dat) %>%
            dplyr::select(-level, level = new_levels) %>%
            tidyr::drop_na(level) %>%
            dplyr::mutate(level = factor(
                level,
                levels = names(level_definitions))
            ) %>%
            dplyr::group_by(level) %>%
            dplyr::summarise_all(sum)
    }
    out
}
