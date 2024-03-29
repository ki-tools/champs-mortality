#' Compute counts of MITS and non-MITS+DSS-only deaths by factors
#' @param x Processed CHAMPS dataset.
#' @param sites A vector of site names to include in the calculations. If NULL,
#' all sites with data will be used.
#' @param catchments A vector of catchments to include in the calculations.
#' If NULL, all catchments with data will be used.
#' @param factor_groups A named list that specifies how to group factors
#' @param ctch_info An object that tracks information about catchments in the data.
#' This is used internally and should be ignored by users.
#' @importFrom dplyr relocate group_by_at full_join n
#' @importFrom purrr map2 map_dbl
#' @importFrom tidyr pivot_longer pivot_wider nest
#' @noRd
mits_factor_tables <- function(
  x, sites = NULL, catchments = NULL, factor_groups = NULL, ctch_info = NULL
) {
  assertthat::assert_that(inherits(x, "champs_processed"),
    msg = cli::format_error("Data must come from process_data()")
  )
  dss <- dss_transform(x$dss)
  group_catchments <- TRUE # used to be a parameter

  if (is.null(ctch_info) || !inherits(ctch_info, "get_ctch"))
    ctch_info <- get_ctch(x, sites, catchments)

  sites <- ctch_info$sites
  ctch <- ctch_info$ctch
  catchments <- ctch_info$catchments
  gctch <- ctch_info$gctch
  can_use_dss <- ctch_info$can_use_dss

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "mits_flag",
    "factor",
    "level"
  )

  ads_ct <- x$ads %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments) %>%
    dplyr::select(dplyr::any_of(c("site", "catchment", "sex",
      # "religion",
      "education", "season", "location", "va", "age", "mits_flag", "year"))) %>%
    tidyr::pivot_longer(
      cols = -all_of(c("site", "catchment", "mits_flag", "year")),
      names_to = "factor",
      values_to = "level"
    ) %>%
    dplyr::left_join(ctch, by = c("site", "catchment")) %>%
    # make sure we are looking at the right years between DSS and ADS
    dplyr::filter(
      .data$year >= .data$start_year,
      .data$year <= .data$end_year
    ) %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::ungroup()

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "factor"
  )
  tots_ads <- ads_ct %>%
    dplyr::filter(!is.na(.data$level)) %>%
    dplyr::select(-c("level")) %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(
      n_mits = sum(.data$n[.data$mits_flag == 1]),
      n_non_mits = sum(.data$n[.data$mits_flag == 0])
    ) %>%
    dplyr::ungroup()

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "factor"
  )
  miss_ads <- ads_ct %>%
    dplyr::filter(is.na(.data$level)) %>%
    dplyr::select(-c("level")) %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(
      na_mits = sum(.data$n[.data$mits_flag == 1]),
      na_non_mits = sum(.data$n[.data$mits_flag == 0])
    ) %>%
    dplyr::ungroup()

  ads_tbl <- dplyr::full_join(tots_ads, miss_ads, by = group_vars)

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

  if (can_use_dss) {
    group_vars <- c(
      "site",
      if (group_catchments) NULL else "catchment"
    )
    # this will give the total DSS subjects at each site so we can compute missing
    # assume age always has the full counts (never have unknown age in DSS)
    tots_dss <- dss %>%
      dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments,
        .data$factor == "age") %>%
      dplyr::group_by_at(group_vars) %>%
      dplyr::summarise(n_dss_tot = sum(.data$n))

    group_vars <- c(
      "site",
      if (group_catchments) NULL else "catchment",
      "factor"
    )
    join_vars <- c(
      "site",
      if (group_catchments) NULL else "catchment"
    )
    dss_tbl <- dss_ct %>%
      dplyr::group_by_at(group_vars) %>%
      dplyr::summarise(n_dss = sum(.data$n), .groups = "drop") %>%
      dplyr::left_join(tots_dss, by = join_vars) %>%
      dplyr::mutate(na_dss = .data$n_dss_tot - .data$n_dss) %>%
      dplyr::select(-c("n_dss_tot"))
  } else {
    dss_tbl <- dplyr::tibble(
      site = character(0),
      factor = character(0),
      n_dss = numeric(0),
      na_dss = numeric(0)
    )

    dss_ct <- dplyr::tibble(
      site = character(0),
      factor = character(0),
      level = character(0),
      n = numeric(0),
      catchment = character(0),
      mits_flag = numeric(0)
    )
  }

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "factor"
  )

  tots <- dplyr::full_join(dss_tbl, ads_tbl, by = group_vars) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(
      n_na = .data$na_dss + .data$na_mits + .data$na_non_mits,
      n_tot = .data$n_na + .data$n_dss + .data$n_mits + .data$n_non_mits,
      pct_na = 100 * .data$n_na / .data$n_tot
    )

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

  ages <- valid_levels$age
  if (!is.null(factor_groups$age))
    ages <- unlist(factor_groups$age)

  pop_mits <- dplyr::bind_rows(ads_ct, dss_ct) %>%
    dplyr::filter(.data$factor == "age", .data$level %in% ages) %>%
    dplyr::group_by_at("site") %>%
    dplyr::summarise(
      stillbirths = sum(.data$n[.data$level == "Stillbirth"]),
      u5d_sb = sum(.data$n)
    )

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
        col_ord <- intersect(
          c("level", "MITS", "non-MITS", "DSS-only", "non-MITS+DSS-only"),
          names(x))
        x <- x[, col_ord]
        if (!can_use_dss) {
          x[["DSS-only"]] <- NULL
          x[["non-MITS+DSS-only"]] <- NULL
        }
        x %>%
          dplyr::filter(!is.na(.data$level)) %>%
          dplyr::arrange(.data$level)
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
        if (can_use_dss) {
          fisher_test(as.matrix(x[, c("MITS", "non-MITS+DSS-only")]))
        } else {
          fisher_test(as.matrix(x[, c("MITS", "non-MITS")]))
        }
      })
    ) %>%
    dplyr::left_join(tots, by = join_vars) %>%
    dplyr::select(-c("data")) %>%
    dplyr::relocate("catchment", .after = "site") %>%
    dplyr::mutate(factor = factor(.data$factor, levels = valid_factors)) %>%
    dplyr::arrange_at(c("site", "catchment", "factor"))

  tblsn$pval[is.na(tblsn$pval)] <- 1
  attr(tblsn, "factor_groups") <- factor_groups
  attr(tblsn, "pop_mits") <- pop_mits
  attr(tblsn, "can_use_dss") <- can_use_dss
  attr(tblsn, "cm_class") <- c("factor_table", "mits_factor_table")

  tblsn
}

#' Compute counts of MITS cases and non-cases for a given condition by factors
#' @param x Processed CHAMPS dataset.
#' @param sites A vector of site names to include in the calculations. If NULL,
#' all sites with data will be used.
#' @param catchments A vector of catchments to include in the calculations.
#' If NULL, all catchments with data will be used.
#' @param factor_groups A named list that specifies how to group factors
#' @param condition CHAMPS group specifying the condition
#' @param icd10_regex An optional regular expression specifying ICD10 codes
#' that define a condition.
#' @param maternal Are we searching for a maternal condition or ICD10
#' specification? Default is FALSE.
#' @param cond_name_short The name of the condition to use in outputs (e.g. if
#' the condition is "Congenital birth defects", cond_name_short could be "CBD").
#' Defaults to value of `condition` if not specified. Required if only
#' `icd10_regex` is specified.
#' @param causal_chain if TRUE, the causal chain is searched, if
#' FALSE, the underlying cause is searched
#' @param ctch_info An object that tracks information about catchments in the data.
#' This is used internally and should be ignored by users.
#' @noRd
cond_factor_tables <- function(
  x, sites = NULL, catchments = NULL,
  factor_groups = NULL,
  condition = NULL, icd10_regex = NULL, maternal = FALSE,
  cond_name_short = condition[1],
  causal_chain = TRUE,
  ctch_info = NULL
) {
  group_catchments <- TRUE

  assertthat::assert_that(inherits(x, "champs_processed"),
    msg = cli::format_error("Data must come from process_data()")
  )

  assertthat::assert_that(!(is.null(condition) && is.null(icd10_regex)),
    msg = cli::format_error("Must specify at least one of 'condition' \\
      and icd10_regex")
  )

  if (!is.null(condition)) {
    # assertthat::assert_that(length(condition) == 1)
    if (maternal) {
      conds <- valid_maternal_conditions(x)
    } else {
      conds <- valid_conditions(x)
    }
    assertthat::assert_that(all(condition %in% conds$condition),
      msg = cli::format_error("Must provide a valid condition. See \\
        valid_conditions() for a list.")
    )
  }

  assertthat::assert_that(!(is.null(cond_name_short)),
    msg = cli::format_error("Must specify cond_name_short")
  )

  if (is.null(ctch_info) || !inherits(ctch_info, "get_ctch"))
    ctch_info <- get_ctch(x, sites, catchments)

  sites <- ctch_info$sites
  ctch <- ctch_info$ctch
  catchments <- ctch_info$catchments
  gctch <- ctch_info$gctch
  can_use_dss <- ctch_info$can_use_dss

  group_vars <- c(
    "site",
    if (group_catchments) NULL else "catchment",
    "cc",
    "factor",
    "level"
  )

  tbls <- x$ads %>%
    dplyr::mutate(cc = as.numeric(
      check_cond_switch(
        .data, condition, icd10_regex, causal_chain, maternal
      ))) %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments,
      .data$mits_flag == 1, .data$decoded == 1) %>%
    dplyr::select(dplyr::any_of(c("site", "catchment", "sex",
      # "religion",
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
        x <- x[, rev(sort(names(x)))]
        x <- x %>%
          dplyr::rename(
            "{cond_name_short}+" := "1",
            "{cond_name_short}-" := "0")
        if (fac == "age")
          x$level <- factor(x$level,
            levels = c("Stillbirth", "Neonate", "Infant", "Child"))
        x %>%
          dplyr::filter(!is.na(.data$level)) %>%
          dplyr::arrange(.data$level)
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
    dplyr::relocate("catchment", .after = "site") %>%
    dplyr::mutate(factor = factor(.data$factor, levels = valid_factors)) %>%
    dplyr::arrange_at(c("site", "catchment", "factor"))

  tblsn$pval[is.na(tblsn$pval)] <- 1
  attr(tblsn, "factor_groups") <- factor_groups
  attr(tblsn, "cm_class") <- c("factor_table", "cond_factor_table")

  tblsn
}

# get all catchments associated with a site, as well as
# whether we can use DSS or not (all catchments have DSS data)
get_ctch <- function(x, sites = NULL, catchments = NULL) {
  usite <- unique(c(x$dss$site, x$ads$site))
  if (is.null(sites))
    sites <- usite

  # assumption is that any catchment that has DSS data shows up in DSS
  dss_ucatch <- x$dss %>%
    dplyr::filter(.data$site %in% sites) %>%
    dplyr::pull("catchment") %>%
    unique()
  non_dss_ucatch <- x$ads %>%
    dplyr::filter(.data$site %in% sites) %>%
    dplyr::pull("catchment") %>%
    unique() %>%
    setdiff(dss_ucatch)

  if (is.null(catchments)) {
    catchments <- c(dss_ucatch, non_dss_ucatch)
  } else {
    not_supported <- setdiff(catchments, c(dss_ucatch, non_dss_ucatch))
    if (length(not_supported) > 0)
      cli::cli_alert_info("The following catchments are not found in the data \\
        for site {sites} and will be removed from the calculations \\
        for this site: {paste(not_supported, collapse = ', ')}", wrap = TRUE)
    catchments <- intersect(catchments, c(dss_ucatch, non_dss_ucatch))
  }

  # if any catchments specified don't have DSS
  # then we have to treat the whole site as if it doesn't have DSS
  can_use_dss <- length(intersect(catchments, non_dss_ucatch)) == 0

  dss_catch <- intersect(catchments, dss_ucatch)
  dss_ctch <- x$dss %>%
    dplyr::select(dplyr::all_of(c("site", "catchment",
      "period_start_year", "period_end_year"))) %>%
    dplyr::rename(
      start_year = "period_start_year",
      end_year = "period_end_year"
    ) %>%
    dplyr::filter(
      .data$site %in% sites,
      .data$catchment %in% dss_catch
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(has_dss = TRUE)

  non_dss_catch <- intersect(catchments, non_dss_ucatch)
  non_dss_ctch <- x$ads %>%
    dplyr::group_by_at(c("site", "catchment")) %>%
    dplyr::summarise(
      start_year = min(.data$year),
      end_year = max(.data$year),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      .data$site %in% sites,
      .data$catchment %in% non_dss_catch
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(has_dss = FALSE)

  ctch <- dplyr::bind_rows(dss_ctch, non_dss_ctch)

  not_in_sites <- setdiff(catchments,
    c(dss_ctch$catchment, non_dss_ctch$catchment))
  if (length(not_in_sites) > 0)
    cli::cli_alert_info("The following catchments are not found \\
      for the sites that were specified: \\
      {paste(not_in_sites, collapse = ', ')}", wrap = TRUE)

  assertthat::assert_that(nrow(ctch) > 0,
    msg = cli::format_error("There are no sites or catchments \\
    found in the data based on provided inputs"))

  # collapse all catchments and take lowest common denominator
  gctch <- ctch %>%
    dplyr::group_by(.data$site) %>%
    dplyr::summarise(
      catchment = paste(sort(unique(.data$catchment)),
        collapse = ", "),
      start_year = min(.data$start_year),
      end_year = min(.data$end_year),
      .groups = "drop"
    )

  res <- list(
    gctch = gctch,
    ctch = ctch,
    sites = unique(ctch$site),
    catchments = unique(ctch$catchment),
    can_use_dss = can_use_dss
  )

  class(res) <- c("list", "get_ctch")

  res
}
