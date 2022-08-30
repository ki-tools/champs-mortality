#' Create an overview table of results
#' @param obj an object [from get_rates_and_fractions]()
#' @importFrom utils tail
#' @importFrom dplyr recode
#' @export
table_rates_fracs <- function(obj) {
  assertthat::assert_that(inherits(obj, "rate_frac_multi_site"),
    msg = cli::format_error("'obj' must come from get_rates_and_fractions()")
  )

  # TODO: make sure all entries of obj have same "condition",
  #   same "per", etc. and do this with the other output functions

  per <- format(obj[[1]]$inputs$per, big.mark = ",")

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

  names(tmp1) <- c("Site", "Catchment",
    "+ All-Cause<br>Mortality Rate",
    paste0("+ Crude CSMR<br>(90% Bayesian CrI)"),
    paste0("+ Adjusted CSMR<br>(90% Bayesian CrI)")
  )

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

  names(tmp2) <- c("Site", "Catchment", "Crude CSMF (%)<br>(90% Bayesian CrI)",
    "Adjusted CSMF (%)<br>(90% Bayesian CrI)")

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

  can_use_dss <- sapply(obj, function(x) x$can_use_dss)
  sites <- names(can_use_dss)
  sites2 <- paste0(sites, ifelse(can_use_dss, "", " *"))
  names(sites2) <- sites
  tmp$Site <- dplyr::recode(tmp$Site, !!!sites2)

  foot <- paste0("+ per ", per, " live births")
  if (!all(can_use_dss))
    foot <- paste0(foot, " \u2014 * includes catchments with no DSS data")

  foot <- paste0(foot, " \u2014 see <a href='https://ki-tools.github.io/champs-mortality/articles/methodology.html' target='_blank'>here</a> for details about the methodology")

  tbl <- gt::gt(tmp) %>%
    gt::cols_label(.list = labs) %>%
    gt::cols_align(align = "right", columns = utils::tail(names(tmp), 5)) %>%
    gt::tab_options(
      data_row.padding = gt::px(6),
      data_row.padding.horizontal = gt::px(20),
      column_labels.padding.horizontal = gt::px(20),
      table.font.size = gt::px(14)
    )

  tbl %>%
    gt::tab_footnote(footnote = gt::html(foot))
}
