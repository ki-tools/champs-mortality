#' @importFrom utils tail
table_overview <- function(obj) {
  tmp1 <- obj$rate %>%
    dplyr::mutate(
      interval = paste0(
        round(.data$est, 1), " (",
        round(.data$lower, 1), ", ",
        round(.data$upper, 1), ")"),
      allcauseTU5MR = round(.data$allcauseTU5MR, 0)
    ) %>%
    dplyr::select(dplyr::all_of(
      c("site", "catchments", "var", "allcauseTU5MR", "interval")
    )) %>%
    tidyr::pivot_wider(id_cols = c("site", "catchments"),
      names_from = "var", values_from = c("allcauseTU5MR", "interval")) %>%
    dplyr::select(-c("allcauseTU5MR_aTU5MR"))

  names(tmp1) <- c("Site", "Catchment", "ACTU5MR",
    "cCSMR (per 10k)<br>(Bayesian CrI)", "aCSMR (per 10k)<br>(Bayesian CrI)")


  tmp2 <- obj$frac %>%
    dplyr::mutate(interval = paste0(
      round(.data$est, 1), " (",
      round(.data$lower, 1), ", ",
      round(.data$upper, 1), ")")) %>%
    dplyr::select(dplyr::all_of(
      c("site", "catchments", "var", "interval")
     )) %>%
    tidyr::pivot_wider(id_cols = c("site", "catchments"),
      names_from = "var", values_from = c("interval"))

  names(tmp2) <- c("Site", "Catchment", "cCSMF (%)<br>(Bayesian CrI)",
    "aCSMF (%)<br>(Bayesian CrI)")

  tmp <- dplyr::left_join(tmp2, tmp1, by = c("Site", "Catchment"))
  labs <- lapply(names(tmp), function(x) gt::html(paste0("<b>", x, "</b>")))
  names(labs) <- names(tmp)

  gt::gt(tmp) %>%
    gt::cols_label(.list = labs) %>%
    gt::cols_align(align = "right", columns = utils::tail(names(tmp), 5)) %>%
    gt::tab_options(
      data_row.padding = gt::px(6),
      data_row.padding.horizontal = gt::px(20),
      column_labels.padding.horizontal = gt::px(20),
      table.font.size = gt::px(14)
    )
}
