#' Compute counts of MITS and non-MITS+DSS-only deaths by factors
#' @param x processed CHAMPS dataset
#' @param sites a vector of site names to include in the calculations
#' @param catchments a vector of catchments to include in the calculations
#' @importFrom dplyr relocate
#' @importFrom purrr map2 map_dbl
#' @importFrom tidyr pivot_longer pivot_wider nest
#' @export
mits_selection_factor_tables <- function(
  x, sites, catchments
) {
  assertthat::assert_that(inherits(x, "champs_processed"),
    msg = cli::format_error("Data must come from process_data()")
  )
  dss <- dss_transform(x$dss)

  ctch <- dplyr::bind_rows(
      x$ads %>%
        dplyr::select(.data$site, .data$catchment),
      dss %>%
        dplyr::select(.data$site, .data$catchment)
    ) %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments) %>%
    dplyr::group_by(.data$site) %>%
    dplyr::summarise(catchments = paste(sort(unique(.data$catchment)),
      collapse = ", "))

  ads_ct <- x$ads %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments) %>%
    dplyr::select(dplyr::any_of(c("site", "catchment", "sex", "religion",
      "education", "season", "location", "va", "age", "mits_flag"))) %>%
    tidyr::pivot_longer(cols = -all_of(c("site", "catchment", "mits_flag")),
      names_to = "factor", values_to = "level") %>%
    dplyr::count(.data$site, .data$mits_flag, .data$factor, .data$level) %>%
    dplyr::filter(!is.na(.data$level))

  dss_ct <- dss %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments) %>%
    dplyr::group_by(.data$site, .data$factor, .data$level) %>%
    dplyr::summarise(n = sum(.data$n), .groups = "drop") %>%
    dplyr::mutate(mits_flag = -1)

  # this will give the total DSS subjects at each site so we can compute missing
  tots <- dss %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments,
      .data$factor == "age") %>%
    dplyr::group_by(.data$site) %>%
    dplyr::summarise(n_tot = sum(.data$n))

  miss <- dss_ct %>%
    dplyr::group_by(.data$site, .data$factor) %>%
    dplyr::summarise(n = sum(.data$n), .groups = "drop") %>%
    dplyr::left_join(tots, by = "site") %>%
    dplyr::mutate(
      n_na = .data$n_tot - .data$n,
      pct_na = 100 * .data$n_na / .data$n_tot)

  tbls <- dplyr::bind_rows(ads_ct, dss_ct) %>%
    dplyr::group_by(.data$site, .data$factor, .data$level, .data$mits_flag) %>%
    dplyr::summarise(n = sum(.data$n), .groups = "drop")

  tblsn <- tbls %>%
    tidyr::nest(data = -c("site", "factor")) %>%
    dplyr::mutate(
      table = purrr::map2(.data$data, .data$factor, function(x, fac) {
        x <- x %>%
          tidyr::pivot_wider(names_from = "mits_flag",
            values_from = "n", values_fill = 0)
        if (is.null(x[["1"]]))
          x[["1"]] <- 0
        if (is.null(x[["0"]]))
          x[["0"]] <- 0
        if (is.null(x[["-1"]]))
          x[["-1"]] <- 0
        if (fac == "age")
          x$level <- factor(x$level,
            levels = c("Stillbirth", "Neonate", "Infant", "Child"))
        x <- x %>%
          dplyr::mutate("non-MITS+DSS-only" = .data[["0"]] + .data[["-1"]]) %>%
          dplyr::rename("MITS" = "1", "non-MITS" = "0", "DSS-only" = "-1")
        dplyr::arrange(x, .data$level)
      }),
      pval = purrr::map_dbl(table, function(x) {
        fisher_test(as.matrix(x[, c("MITS", "non-MITS+DSS-only")]))
      })
    ) %>%
    dplyr::left_join(miss, by = c("site", "factor")) %>%
    dplyr::select(-c("data")) %>%
    dplyr::left_join(ctch, by = "site") %>%
    dplyr::relocate("catchments", .after = "site")

  tblsn
}

#' Compute counts of MITS cases and non-cases for a given condition by factors
#' @param x processed CHAMPS dataset
#' @param sites a vector of site names to include in the calculations
#' @param catchments a vector of catchments to include in the calculations
#' @param champs_group CHAMPS group specifying the condition
#' @export
cond_factor_tables <- function(
  x, sites, catchments, champs_group
) {
  assertthat::assert_that(inherits(x, "champs_processed"),
    msg = cli::format_error("Data must come from process_data()")
  )

  ctch <- x$ads %>%
    dplyr::select(.data$site, .data$catchment) %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments) %>%
    dplyr::group_by(.data$site) %>%
    dplyr::summarise(catchments = paste(sort(unique(.data$catchment)),
      collapse = ", "))

  tbls <- x$ads %>%
    dplyr::mutate(cc = as.numeric(
      has_champs_group(.data, !!champs_group))) %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments,
      .data$mits_flag == 1, .data$decoded == 1) %>%
    dplyr::select(dplyr::any_of(c("site", "catchment", "sex", "religion",
      "education", "season", "location", "va", "age", "cc"))) %>%
    tidyr::pivot_longer(cols = -all_of(c("site", "catchment", "cc")),
      names_to = "factor", values_to = "level") %>%
    dplyr::count(.data$site, .data$cc, .data$factor, .data$level) %>%
    dplyr::filter(!is.na(.data$cc))

  miss <- tbls %>%
    dplyr::group_by(.data$site, .data$factor) %>%
    dplyr::summarise(
      n_na = sum(.data$n[is.na(.data$level)]),
      n = sum(.data$n[!is.na(.data$level)]) + .data$n_na,
      pct_na = 100 * .data$n_na / .data$n,
      .groups = "drop"
    )

  tblsn <- tbls %>%
    dplyr::filter(!is.na(.data$level)) %>%
    tidyr::nest(data = -c("site", "factor")) %>%
    dplyr::mutate(
      table = purrr::map2(.data$data, .data$factor, function(x, fac) {
        x <- x %>%
          tidyr::pivot_wider(names_from = "cc",
            values_from = "n", values_fill = 0)
        if (is.null(x[["1"]]))
          x[["1"]] <- 0
        if (is.null(x[["0"]]))
          x[["0"]] <- 0
        x <- x %>%
          dplyr::rename("{champs_group}+" := "1", "{champs_group}-" := "0")
        if (fac == "age")
          x$level <- factor(x$level,
            levels = c("Stillbirth", "Neonate", "Infant", "Child"))
        x <- dplyr::arrange(x, .data$level)
        nms <- names(x)
        nnms <- c(nms[1], sort(nms[-1], decreasing = TRUE))
        x[, nnms]
      }),
      pval = purrr::map_dbl(table, function(x) {
        fisher_test(as.matrix(x[, -1]))
      })
    ) %>%
    dplyr::left_join(miss, by = c("site", "factor")) %>%
    dplyr::select(-c("data")) %>%
    dplyr::left_join(ctch, by = "site") %>%
    dplyr::relocate("catchments", .after = "site")

  tblsn
}
