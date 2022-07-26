#' Create an overview table of results
#' @param obj an object [from get_rates_and_fractions]()
#' @importFrom utils tail
#' @export
table_overview <- function(obj) {
  assertthat::assert_that(inherits(obj, "rate_frac_multi_site"),
    msg = cli::format_error("'obj' must come from get_rates_and_fractions()")
  )

  datr <- lapply(obj, function(x) x$rate) %>%
    dplyr::bind_rows()
  datf <- lapply(obj, function(x) x$frac) %>%
    dplyr::bind_rows()

  tmp1 <- datr %>%
    dplyr::mutate(
      interval = paste0(
        round(.data$est, 1), " (",
        round(.data$lower, 1), ", ",
        round(.data$upper, 1), ")"),
      allcauseMR = round(.data$allcauseMR, 0)
    ) %>%
    dplyr::select(dplyr::all_of(
      c("site", "catchments", "var", "allcauseMR", "interval")
    )) %>%
    tidyr::pivot_wider(id_cols = c("site", "catchments"),
      names_from = "var", values_from = c("allcauseMR", "interval")) %>%
    dplyr::select(-c("allcauseMR_aTU5MR"))

  names(tmp1) <- c("Site", "Catchment", "ACTU5MR",
    "cCSMR (per 10k)<br>(Bayesian CrI)", "aCSMR (per 10k)<br>(Bayesian CrI)")


  tmp2 <- datf %>%
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

  adj_vars <- lapply(obj, function(x) dplyr::tibble(Site = x$site,
    adjust_vars = if (is.null(x$adjust_vars)) "none" else
      paste(x$adjust_vars, collapse = ", "))) %>%
    dplyr::bind_rows()
  names(adj_vars)[2] <- "Adjustment<br>Factor(s)"

  tmp <- dplyr::left_join(tmp2, tmp1, by = c("Site", "Catchment")) %>%
    dplyr::left_join(adj_vars, by = "Site") %>%
    dplyr::relocate(8, .before = 3)
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
