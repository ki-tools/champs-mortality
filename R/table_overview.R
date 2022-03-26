table_overview <- function(obj) {
  tmp1 <- obj$rate %>%
    mutate(
      interval = paste0(
        round(est, 1), " (", round(lower, 1), ", ", round(upper, 1), ")"),
      allcauseTU5MR = round(allcauseTU5MR, 0)
    ) %>%
    select(site, catchments, var, allcauseTU5MR, interval) %>%
    pivot_wider(id_cols = c("site", "catchments"),
      names_from = "var", values_from = c("allcauseTU5MR", "interval")) %>%
    select(-c("allcauseTU5MR_aTU5MR"))

  names(tmp1) <- c("Site", "Catchment", "ACTU5MR",
    "cCSMR (per 10k)<br>(Bayesian CrI)", "aCSMR (per 10k)<br>(Bayesian CrI)")


  tmp2 <- obj$frac %>%
    mutate(interval = paste0(
      round(est, 1), " (", round(lower, 1), ", ", round(upper, 1), ")")) %>%
    select(site, catchments, var, interval) %>%
    pivot_wider(id_cols = c("site", "catchments"),
      names_from = "var", values_from = c("interval"))

  names(tmp2) <- c("Site", "Catchment", "cCSMF (%)<br>(Bayesian CrI)",
    "aCSMF (%)<br>(Bayesian CrI)")

  tmp <- left_join(tmp2, tmp1, by = c("Site", "Catchment"))
  labs <- lapply(names(tmp), function(x) gt::html(paste0("<b>", x, "</b>")))
  names(labs) <- names(tmp)

  gt::gt(tmp) %>%
    gt::cols_label(.list = labs) %>%
    gt::cols_align(align = "right", columns = tail(names(tmp), 5)) %>%
    gt::tab_options(
      data_row.padding = gt::px(6),
      data_row.padding.horizontal = gt::px(20),
      column_labels.padding.horizontal = gt::px(20)
    )
}
