#' Compute counts of MITS and non-MITS+DSS-only deaths by factors
#' @param x processed CHAMPS dataset
#' @param sites a vector of site names to include in the calculations
#' @param catchments a vector of catchments to include in the calculations.
#' If NULL, all catchments with DSS data will be used
#' @param factor_groups a named list that specifies how to group factors
#' @importFrom dplyr relocate
#' @importFrom purrr map2 map_dbl
#' @importFrom tidyr pivot_longer pivot_wider nest
#' @export
mits_selection_factor_tables <- function(
  x, sites = NULL, catchments = NULL, group_catchments = TRUE,
  factor_groups = NULL
) {
  assertthat::assert_that(inherits(x, "champs_processed"),
    msg = cli::format_error("Data must come from process_data()")
  )
  dss <- dss_transform(x$dss)

  obj <- get_ctch(x, sites, catchments)
  sites <- obj$sites
  ctch <- obj$ctch
  catchments <- obj$catchments
  gctch <- obj$gctch

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "mits_flag",
    "factor",
    "level"
  )

  ads_ct <- x$ads %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments) %>%
    dplyr::select(dplyr::any_of(c("site", "catchment", "sex", "religion",
      "education", "season", "location", "va", "age", "mits_flag", "year"))) %>%
    tidyr::pivot_longer(cols = -all_of(
      c("site", "catchment", "mits_flag", "year")),
      names_to = "factor", values_to = "level") %>%
    dplyr::left_join(ctch, by = c("site", "catchment")) %>%
    # make sure we are looking at the right years between DSS and ADS
    dplyr::filter(
      .data$year >= .data$start_year,
      .data$year <= .data$end_year
    ) %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::filter(!is.na(.data$level)) %>%
    dplyr::ungroup()

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "factor",
    "level"
  )

  dss_ct <- dss %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments) %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(
      n = sum(.data$n),
      catchment = paste(sort(unique(.data$catchment)),
        collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(mits_flag = -1)

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment"
  )

  # this will give the total DSS subjects at each site so we can compute missing
  tots <- dss %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments,
      .data$factor == "age") %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(n_tot = sum(.data$n))

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "factor"
  )
  join_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment"
  )

  miss <- dss_ct %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(n = sum(.data$n), .groups = "drop") %>%
    dplyr::left_join(tots, by = join_vars) %>%
    dplyr::mutate(
      n_na = .data$n_tot - .data$n,
      pct_na = 100 * .data$n_na / .data$n_tot)

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "factor",
    "level",
    "mits_flag"
  )

  tbls <- dplyr::bind_rows(ads_ct, dss_ct) %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(n = sum(.data$n), .groups = "drop")

  if (group_catchments) {
    tbls <- dplyr::left_join(tbls, gctch, by = "site")
  } else {
    tbls <- dplyr::left_join(tbls, ctch, by = c("site", "catchment"))
  }

  tmp <- tbls %>%
    tidyr::nest(data = -c("site", "catchment", "factor",
      "start_year", "end_year")) %>%
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
        x$level <- factor(x$level,
          levels = valid_levels[[fac]])
        x <- x %>%
          dplyr::mutate("non-MITS+DSS-only" = .data[["0"]] + .data[["-1"]]) %>%
          dplyr::rename("MITS" = "1", "non-MITS" = "0", "DSS-only" = "-1")
        dplyr::arrange(x, .data$level)
      })
    )

  for (ii in seq_len(nrow(tmp))) {
    cur_fac <- tmp$factor[ii]
    if (!is.null(factor_groups[[cur_fac]]))
      tmp$table[[ii]] <- combine_levels(tmp$table[[ii]],
        factor_groups[[cur_fac]])
  }

  join_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "factor"
  )
  tblsn <- tmp %>%
    mutate(
      pval = purrr::map_dbl(table, function(x) {
        fisher_test(as.matrix(x[, c("MITS", "non-MITS+DSS-only")]))
      })
    ) %>%
    dplyr::left_join(miss, by = join_vars) %>%
    dplyr::select(-c("data")) %>%
    dplyr::relocate("catchment", .after = "site")

  tblsn$pval[is.na(tblsn$pval)] <- 1

  tblsn
}

#' Compute counts of MITS cases and non-cases for a given condition by factors
#' @param x processed CHAMPS dataset
#' @param sites a vector of site names to include in the calculations
#' @param catchments a vector of catchments to include in the calculations
#' @param champs_group CHAMPS group specifying the condition
#' @export
cond_factor_tables <- function(
  x, champs_group, sites = NULL, catchments = NULL, group_catchments = TRUE,
  causal_chain = TRUE, factor_groups = NULL
) {
  assertthat::assert_that(inherits(x, "champs_processed"),
    msg = cli::format_error("Data must come from process_data()")
  )

  obj <- get_ctch(x, sites, catchments)
  sites <- obj$sites
  ctch <- obj$ctch
  catchments <- obj$catchments
  gctch <- obj$gctch

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "cc",
    "factor",
    "level"
  )

  tbls <- x$ads %>%
    dplyr::mutate(cc = as.numeric(
      has_champs_group(.data, !!champs_group))) %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments,
      .data$mits_flag == 1, .data$decoded == 1) %>%
    dplyr::select(dplyr::any_of(c("site", "catchment", "sex", "religion",
      "education", "season", "location", "va", "age", "cc", "year"))) %>%
    tidyr::pivot_longer(cols = -all_of(c("site", "catchment", "cc", "year")),
      names_to = "factor", values_to = "level") %>%
    dplyr::left_join(ctch, by = c("site", "catchment")) %>%
    # make sure we are looking at the right years between DSS and ADS
    dplyr::filter(
      .data$year >= .data$start_year,
      .data$year <= .data$end_year
    ) %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::count(.data$site, .data$cc, .data$factor, .data$level) %>%
    dplyr::filter(!is.na(.data$cc)) %>%
    dplyr::ungroup()

  if (group_catchments) {
    tbls <- dplyr::left_join(tbls, gctch, by = "site")
  } else {
    tbls <- dplyr::left_join(tbls, ctch, by = c("site", "catchment"))
  }

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "factor"
  )

  miss <- tbls %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(
      n_na = sum(.data$n[is.na(.data$level)]),
      n = sum(.data$n[!is.na(.data$level)]) + .data$n_na,
      pct_na = 100 * .data$n_na / .data$n,
      .groups = "drop"
    )

  tmp <- tbls %>%
    dplyr::filter(!is.na(.data$level)) %>%
    tidyr::nest(data = -c("site", "catchment", "factor",
      "start_year", "end_year")) %>%
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
      })
    )

  for (ii in seq_len(nrow(tmp))) {
    cur_fac <- tmp$factor[ii]
    if (!is.null(factor_groups[[cur_fac]]))
      tmp$table[[ii]] <- combine_levels(tmp$table[[ii]],
        factor_groups[[cur_fac]])
  }

  join_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "factor"
  )
  tblsn <- tmp %>%
    mutate(
      pval = purrr::map_dbl(table, function(x) {
        fisher_test(as.matrix(x[, -1]))
      })
    ) %>%
    dplyr::left_join(miss, by = join_vars) %>%
    dplyr::select(-c("data")) %>%
    dplyr::relocate("catchment", .after = "site")

  tblsn$pval[is.na(tblsn$pval)] <- 1

  tblsn
}


get_ctch <- function(x, sites, catchments, dss = TRUE) {
  usite <- unique(x$dss$site)
  if (is.null(sites))
    sites <- usite

  # assumption is that any catchment that has DSS data shows up in DSS
  ucatch <- unique(x$dss$catchment)
  if (!is.null(catchments)) {
    not_supported <- setdiff(catchments, ucatch)
    if (length(not_supported) > 0)
      cli::cli_alert_info("The following catchments are not found in DSS \\
        and will be removed from the calculations: \\
        {paste(not_supported, collapse = ', ')}", wrap = TRUE)
    catchments <- intersect(catchments, ucatch)

    ctch <- x$dss %>%
      dplyr::select(dplyr::all_of(c("site", "catchment",
        "period_start_year", "period_end_year"))) %>%
      dplyr::rename(
        start_year = "period_start_year",
        end_year = "period_end_year"
      ) %>%
      dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments) %>%
      dplyr::distinct()

    # x$dss %>%
    #   select(site, catchment, period_start_year, period_end_year) %>%
    #   distinct()

    not_in_sites <- setdiff(ctch$catchment, catchments)
    if (length(not_in_sites) > 0)
      cli::cli_alert_info("The following catchments are not found in DSS \\
        for the sites that were specified: \\
        {paste(not_in_sites, collapse = ', ')}", wrap = TRUE)
  } else {
    ctch <- x$dss %>%
      dplyr::select(dplyr::all_of(c("site", "catchment",
        "period_start_year", "period_end_year"))) %>%
      dplyr::rename(
        start_year = "period_start_year",
        end_year = "period_end_year"
      ) %>%
      dplyr::filter(.data$site %in% sites) %>%
      dplyr::distinct()
    catchments <- unique(ctch$catchment)
  }

  gctch <- ctch %>%
  dplyr::group_by(.data$site) %>%
  dplyr::summarise(
    catchment = paste(sort(unique(.data$catchment)),
      collapse = ", "),
    start_year = min(start_year),
    end_year = min(end_year)
  )

  list(ctch = ctch, catchments = catchments, gctch = gctch, sites = sites)
}
