#' @param x Processed CHAMPS dataset.
#' @param sites A vector of site names to include in the calculations. If NULL,
#' all sites with data (corresponding to value of use_dss) will be used.
#' @param catchments A vector of catchments to include in the calculations.
#' If NULL, all catchments with data (corresponding to value of use_dss)
#' will be used.
#' @param group_catchments Should all catchments within a site be grouped
#' together?
#' @param factor_groups A named list that specifies how to group factors
#' @param use_dss Should the calculations be done only for catchments that
#' @param condition CHAMPS group specifying the condition
#' @param icd10_regex An optional regular expression specifying ICD10 codes
#' that define a condition.
#' @param cond_name The name of the condition to use in outputs (e.g. if
#' the condition is "Congenital birth defects", cond_name could be "CBD").
#' Defaults to `condition` if not specified.
#' @param causal_chain Should the search for the condition be
#' across the causal chain?
#' @param adjust_vars_override An optional vector of adjustment
#' variables that will override the automatic adjustment. Cannot be
#' more than two variables and must include age.
#' @param pval_cutoff P-value for which variables are considered
#' for adjustment. Ignored if adjust_vars_override is specified.
#' @param prop_na_cutoff Proportion of missingness for which
#' variables are considered for adjustment. Ignored if
#' adjust_vars_override is specified.
#' @note One or both of `icd10_regex` and `condition` must be specified
#' @export
get_rates_and_fractions <- function(
  x,
  sites = NULL,
  catchments = NULL,
  group_catchments = TRUE,
  condition = NULL,
  icd10_regex = NULL,
  cond_name = condition,
  causal_chain = TRUE,
  adjust_vars_override = NULL,
  factor_groups = NULL,
  pval_cutoff = 0.2,
  prop_cutoff = 0.2
) {
  tbl1a <- mits_selection_factor_tables(x,
    sites = sites,
    catchments = catchments,
    group_catchments = group_catchments,
    factor_groups = factor_groups,
    use_dss = TRUE
  )

  tbl1b <- mits_selection_factor_tables(x,
    sites = sites,
    catchments = catchments,
    group_catchments = group_catchments,
    factor_groups = factor_groups,
    use_dss = FALSE
  )

  tbl2a <- cond_factor_tables(x,
    sites = sites,
    catchments = catchments,
    group_catchments = group_catchments,
    factor_groups = factor_groups,
    condition = condition,
    icd10_regex = icd10_regex,
    cond_name = condition,
    causal_chain = causal_chain,
    use_dss = TRUE
  )

  tbl2b <- cond_factor_tables(x,
    sites = sites,
    catchments = catchments,
    group_catchments = group_catchments,
    factor_groups = factor_groups,
    condition = condition,
    icd10_regex = icd10_regex,
    cond_name = condition,
    causal_chain = causal_chain,
    use_dss = FALSE
  )

  vars <- c("site", "catchment", "factor", "pval", "pct_na")
  crit <- dplyr::bind_rows(
    dplyr::select(tbl1a, dplyr::all_of(vars)),
    dplyr::select(tbl1b, dplyr::all_of(vars)),
    dplyr::select(tbl2a, dplyr::all_of(vars)),
    dplyr::select(tbl2b, dplyr::all_of(vars))
  )

  crit %>%
    dplyr::filter(pct_na < prop_cutoff, pval < pval_cutoff) %>%
    dplyr::group_by_at(c("site", "catchment", "factor")) %>%
    dplyr::summarise(n = dplyr::n())

  rate_data <- get_rate_data(x,
    site = sites,
    catchments = catchments,
    condition = condition,
    icd10_regex = icd10_regex,
    adjust_vars = adjust_vars_override, # TODO: update
    use_dss = TRUE
  )

  browser()



}


# automatically determine the factor(s) to adjust by (if any)
# compute the crude and adjusted mortality fractions
# combine groups with zeros
# combine the rates and fractions across all catchments (if broken down by catchment) to provide site-level statistics
# outputs:
# Crude and adjusted mortality rates and fractions with confidence intervals
# All underlying tables and data which can be optionally presented for diagnostic purposes (e.g. show what was adjusted for and why, etc.)


# should we support multiple conditions?

#' Get data necessary to compute adjusted mortality fractions and rates
#' @param x an object read in from [read_and_validate_data()]
#' @param site a vector of sites to include in the calculations 
#' @param catchments a vector of catchments to include in the calculations
#' @param condition a CHAMPS condition (see [valid_conditions()])
#' @param icd10_regex an optional regular expression specifying ICD10 codes
#' that define a condition
#' @param adjust_vars a vector of variables to adjust by
#' @param factor_groups A named list that specifies how to group factors
#' @param use_dss Should the calculations be done only for catchments that
#' @note One or both of `icd10_regex` and `condition` must be specified
#' @export
get_rate_data <- function(x,
  sites,
  catchments = NULL,
  condition = NULL,
  icd10_regex = NULL,
  adjust_vars = NULL,
  factor_groups = NULL,
  use_dss = TRUE
) {
  assertthat::assert_that(inherits(x, "champs_processed"),
    msg = cli::format_error("Data must come from process_data()")
  )

  assertthat::assert_that(!(is.null(condition) && is.null(icd10_regex)),
    msg = cli::format_error("Must specify at least one of 'condition' \\
      and icd10_regex")
  )

  if (!is.null(condition)) {
    assertthat::assert_that(length(condition) == 1)
    conds <- valid_conditions(x)
    assertthat::assert_that(condition %in% conds$condition,
      msg = cli::format_error("Must provide a valid condition. See \\
        valid_conditions() for a list.")
    )
  }

  obj <- get_ctch(x, sites, catchments)
  type <- paste0(ifelse(use_dss, "", "non_"), "dss")
  sites <- obj[[type]]$sites
  ctch <- obj[[type]]$ctch
  catchments <- obj[[type]]$catchments
  gctch <- obj[[type]]$gctch

  assertthat::assert_that(length(adjust_vars) <= 2,
    msg = cli::format_error("Can only provide up to two adjustment variables.")
  )

  if (length(adjust_vars) == 2) {
    assertthat::assert_that("age" %in% adjust_vars,
      msg = cli::format_error("When specifying two adjustment variables, \\
        one of them must be 'age'")
    )
  }

  other_var <- setdiff(adjust_vars, "age")
  if (length(other_var) > 0) {
    assertthat::assert_that(other_var %in% valid_factors,
      msg = cli::format_error("Variables to adjust by can only be the \\
        following: {paste(valid_factors, collapse = ', ')}")
    )
  }

  # TODO: if catchments with different year coverage are specified
  #   filter all data down to years that all have in common and give a warning

  if (is.null(adjust_vars)) {
    if (use_dss) {
      # use the max counts within each factor to get total
      dss_only <- x$dss %>%
        dplyr::filter(
          .data$site %in% sites,
          .data$catchment %in% catchments
        ) %>%
        dplyr::group_by(.data$factor) %>%
        dplyr::summarise(n = sum(.data$n)) %>%
        dplyr::ungroup() %>%
        dplyr::summarise(n = max(.data$n)) %>%
        dplyr::rename("dss_only" = "n")
    } else {
      dss_only <- dplyr::tibble(dss_only = 0)
    }
  } else if (length(adjust_vars) == 1) {
    if (adjust_vars[1] == "age") {
      if (use_dss) {
        dss_only <- x$dss %>%
          dplyr::filter(
            .data$site %in% sites,
            .data$catchment %in% catchments
          ) %>%
          dplyr::group_by(.data$age, .data$factor, .drop = FALSE) %>%
          dplyr::summarise(n = sum(.data$n), .groups = "drop") %>%
          dplyr::group_by(.data$age) %>%
          dplyr::summarise(n = max(.data$n)) %>%
          dplyr::rename("dss_only" = "n")
      } else {
        dss_only <- dplyr::tibble(age = levels(x$ads$age), dss_only = 0)
      }
    } else {
      if (use_dss) {
        dss_only <- x$dss %>%
          dplyr::filter(
            .data$site %in% sites,
            .data$catchment %in% catchments,
            .data$factor == adjust_vars
          ) %>%
          dplyr::mutate(level = factor(.data$level,
            levels = valid_levels[[other_var]])) %>%
          dplyr::group_by(.data$level, .drop = FALSE) %>%
          dplyr::summarise(n = sum(.data$n)) %>%
          dplyr::rename("{adjust_vars}" := "level", "dss_only" = "n")
      } else {
        dss_only <- dplyr::tibble(
          level = levels(x$ads[[adjust_vars]]), dss_only = 0) %>%
            dplyr::rename("{adjust_vars}" := "level")
      }
    }
  } else {
    if (use_dss) {
      dss_only <- x$dss %>%
        dplyr::filter(
          .data$site %in% sites,
          .data$catchment %in% catchments,
          .data$factor == other_var
        ) %>%
        dplyr::mutate(level = factor(.data$level,
          levels = valid_levels[[other_var]])) %>%
        dplyr::group_by_at(c("age", "level"), .drop = FALSE) %>%
        dplyr::summarise(n = sum(.data$n), .groups = "drop") %>%
        dplyr::rename("{other_var}" := "level", "dss_only" = "n")
    } else {
      dss_only <- x$ads %>%
        dplyr::group_by_at(c(adjust_vars), .drop = FALSE) %>%
        dplyr::summarise(dss_only = 0, .groups = "drop") %>%
        tidyr::drop_na()
    }
  }

  if (length(other_var) == 1) {
    x$ads[[other_var]] <- factor(x$ads[[other_var]],
      levels = valid_levels[[other_var]])
  }

  chmps <- x$ads %>%
    dplyr::filter(
      .data$site %in% sites,
      .data$catchment %in% catchments
    ) %>%
    dplyr::group_by_at(adjust_vars, .drop = FALSE) %>%
    dplyr::count() %>%
    dplyr::rename("champs" = "n")

  decode <- x$ads %>%
    dplyr::filter(
      .data$site %in% sites,
      .data$catchment %in% catchments,
      .data$mits_flag == 1,
      .data$decoded == 1
    ) %>%
    dplyr::group_by_at(adjust_vars, .drop = FALSE) %>%
    dplyr::count() %>%
    dplyr::rename("decode" = "n")

  check_cond <- function(., group, rgx) {
    if (is.null(group))
      return(has_icd10(., rgx))
    if (is.null(rgx))
      return(has_champs_group(., group))
    has_icd10(., rgx) | has_champs_group(., group)
  }

  cond <- x$ads %>%
    dplyr::filter(
      .data$site %in% sites,
      .data$catchment %in% catchments,
      .data$mits_flag == 1,
      .data$decoded == 1) %>%
    mutate(cond_cc = check_cond(.data, condition, icd10_regex)) %>%
    dplyr::filter(.data$cond_cc) %>%
    dplyr::group_by_at(adjust_vars, .drop = FALSE) %>%
    dplyr::count() %>%
    dplyr::rename("condition" = "n")

  if (is.null(adjust_vars)) {
    tbldat <- cbind(
      dss_only, chmps,
      dplyr::tibble(target = dss_only[[1]] + chmps[[1]]),
      decode, cond
    )
  } else {
    tbldat <- dss_only %>%
      dplyr::left_join(chmps, by = adjust_vars) %>%
      dplyr::left_join(decode, by = adjust_vars) %>%
      dplyr::left_join(cond, by = adjust_vars) %>%
      dplyr::mutate(target = .data$dss_only + .data$champs) %>%
      dplyr::relocate(.data$target, .before = decode)
  }

  ld <- x$lb %>%
    dplyr::filter(.data$site %in% sites, .data$catchment %in% catchments)

  year_range <- ctch

  if (!is.null(factor_groups)) {
    for (cur_fac in adjust_vars)
      if (!is.null(factor_groups[[cur_fac]]))
        tbldat <- combine_levels(tbldat,
          factor_groups[[cur_fac]], varname = cur_fac, summ = FALSE)
    tbldat <- tbldat %>%
      dplyr::group_by_at(adjust_vars, .drop = FALSE) %>%
      dplyr::summarise_all(sum)
  }

  list(
    data = tbldat,
    sites = sites,
    catchments = catchments,
    year_range = year_range,
    live_birth_data = ld,
    total_live_births = sum(ld$live_births),
    dss = use_dss
  )
}
