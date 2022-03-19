
#' @param x an object read in from [read_and_validate_data()]
#' @param catchments a vector of catchments to include in the calculations
#' @param condition a CHAMPS condition (see [valid_conditions()])
#' @param icd10_regex an optional regular expression specifying ICD10 codes
#' that define a condition
#' @param adjust_vars a vector of variables to adjust by
#' @note One or both of `icd10_regex` and `condition` must be specified
#' @export
get_rates_fractions <- function(
  x,
  sites,
  catchments = NULL,
  group_catchments = FALSE,
  condition = NULL,
  icd10_regex = NULL,
  causal_chain = TRUE,
  adjust_var_override = NULL,
  factor_groups = NULL
) {
  mits_selection_factor_tables(x, sites, catchments, factor_groups = NULL)

}


# •	Build a master function that is the main entry point to the package that, for a given set of inputs (site, condition, whether to search in causal chain, age groupings or subsets, and whether to break down by catchment or not) does the following:
# o	automatically determine the factor(s) to adjust by (if any) 
# o	compute the crude and adjusted mortality fractions
# o	combine groups with zeros
# o	combine the rates and fractions across all catchments (if broken down by catchment) to provide site-level statistics
# o	outputs:
# 	Crude and adjusted mortality rates and fractions with confidence intervals
# 	All underlying tables and data which can be optionally presented for diagnostic purposes (e.g. show what was adjusted for and why, etc.)



# should we support multiple conditions?

#' Get data necessary to compute adjusted mortality fractions and rates
#' @param x an object read in from [read_and_validate_data()]
#' @param site a vector of sites to include in the calculations 
#' @param catchments a vector of catchments to include in the calculations
#' @param condition a CHAMPS condition (see [valid_conditions()])
#' @param icd10_regex an optional regular expression specifying ICD10 codes
#' that define a condition
#' @param adjust_vars a vector of variables to adjust by
#' @note One or both of `icd10_regex` and `condition` must be specified
#' @export
get_rate_data <- function(x,
  site,
  catchments = NULL,
  condition = NULL,
  icd10_regex = NULL,
  adjust_vars = NULL
) {
  assertthat::assert_that(inherits(x, "champs_processed"),
    msg = cli::format_error("Data must come from process_data()")
  )

  assertthat::assert_that(!(is.null(condition) && is.null(icd10_regex)),
    msg = cli::format_error("Must specify at least one of 'condition' \\
      and icd10_regex")
  )

  if (is.null(catchments))
    catchments <- x$ads$catchment[x$ads$site %in% "site"]

  if (!is.null(condition)) {
    assertthat::assert_that(length(condition) == 1)
    conds <- valid_conditions(x)
    assertthat::assert_that(condition %in% conds$condition,
      msg = cli::format_error("Must provide a valid condition. See \\
        valid_conditions() for a list.")
    )
  }

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
      msg = cli::format_error("Variables to adjust by must include the \\
        following: {paste(valid_factors, collapse = ', ')}")
    )
  }

  # TODO: if catchments with different year coverage are specified
  #   filter all data down to years that all have in common and give a warning

  if (is.null(adjust_vars)) {
    # use the max counts within each factor to get total
    dss_only <- x$dss %>%
      dplyr::filter(
        .data$site %in% site,
        .data$catchment %in% catchments
      ) %>%
      dplyr::group_by(.data$factor) %>%
      dplyr::summarise(n = sum(.data$n)) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(n = max(.data$n)) %>%
      dplyr::rename("dss_only" = "n")
  } else if (length(adjust_vars) == 1) {
    if (adjust_vars[1] == "age") {
      dss_only <- x$dss %>%
        dplyr::filter(
          .data$site %in% site,
          .data$catchment %in% catchments
        ) %>%
        dplyr::group_by(.data$age, .data$factor, .drop = FALSE) %>%
        dplyr::summarise(n = sum(.data$n), .groups = "drop") %>%
        dplyr::group_by(.data$age) %>%
        dplyr::summarise(n = max(.data$n)) %>%
        dplyr::rename("dss_only" = "n")
    } else {
      dss_only <- x$dss %>%
        dplyr::filter(
          .data$site %in% site,
          .data$catchment %in% catchments,
          .data$factor == adjust_vars
        ) %>%
        dplyr::mutate(level = factor(.data$level,
          levels = valid_levels[[other_var]])) %>%
        dplyr::group_by(.data$level, .drop = FALSE) %>%
        dplyr::summarise(n = sum(.data$n)) %>%
        dplyr::rename("{adjust_vars}" := "level", "dss_only" = "n")
    }
  } else {
    dss_only <- x$dss %>%
      dplyr::filter(
        .data$site %in% site,
        .data$catchment %in% catchments,
        .data$factor == other_var
      ) %>%
      dplyr::mutate(level = factor(.data$level,
        levels = valid_levels[[other_var]])) %>%
      dplyr::group_by_at(c("age", "level"), .drop = FALSE) %>%
      dplyr::summarise(n = sum(.data$n)) %>%
      dplyr::rename("{other_var}" := "level", "dss_only" = "n")
  }

  if (length(other_var) == 1) {
    x$ads[[other_var]] <- factor(x$ads[[other_var]],
      levels = valid_levels[[other_var]])
  }

  chmps <- x$ads %>%
    dplyr::filter(
      .data$site %in% site,
      .data$catchment %in% catchments
    ) %>%
    dplyr::group_by_at(adjust_vars, .drop = FALSE) %>%
    dplyr::count() %>%
    dplyr::rename("champs" = "n")

  mits <- x$ads %>%
    dplyr::filter(
      .data$site %in% site,
      .data$catchment %in% catchments,
      .data$mits_flag == 1,
      .data$decoded == 1
    ) %>%
    dplyr::group_by_at(adjust_vars, .drop = FALSE) %>%
    dplyr::count() %>%
    dplyr::rename("mits" = "n")

  check_cond <- function(., group, rgx) {
    if (is.null(group))
      return(has_icd10(., rgx))
    if (is.null(rgx))
      return(has_champs_group(., group))
    has_icd10(., rgx) | has_champs_group(., group)
  }

  cond <- x$ads %>%
    dplyr::filter(
      .data$site %in% site,
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
      mits, cond
    )
  } else {
    tbldat <- dss_only %>%
      left_join(chmps, by = adjust_vars) %>%
      left_join(mits, by = adjust_vars) %>%
      left_join(cond, by = adjust_vars) %>%
      mutate(target = .data$dss_only + .data$champs) %>%
      relocate(.data$target, .before = mits)
  }

  ld <- x$lb %>%
    dplyr::filter(.data$site %in% site, .data$catchment %in% catchments)

  year_range <- x$dss %>%
    dplyr::filter(.data$site %in% site, .data$catchment %in% catchments) %>%
    dplyr::select(dplyr::all_of(c("site", "catchment",
      "period_start_year", "period_end_year"))) %>%
    dplyr::distinct()

  list(
    data = tbldat,
    site = site,
    catchment = catchments,
    year_range = year_range,
    live_birth_data = ld,
    total_live_births = sum(ld$live_births)
  )
}
