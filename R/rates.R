#' Get rate and fraction data
#' @param x Processed CHAMPS dataset.
#' @param sites A vector of site names to include in the calculations. If NULL,
#' all sites with data (corresponding to value of use_dss) will be used.
#' @param catchments A vector of catchments to include in the calculations.
#' If NULL, all catchments with data (corresponding to value of use_dss)
#' will be used.
#' @param group_catchments Should all catchments within a site be grouped
#' together?
#' @param factor_groups A named list that specifies how to group factors
#' @param condition CHAMPS group specifying the condition
#' @param icd10_regex An optional regular expression specifying ICD10 codes
#' that define a condition.
#' @param cond_name_short The name of the condition to use in outputs (e.g. if
#' the condition is "Congenital birth defects", cond_name_short could be "CBD").
#' Defaults to `condition` if not specified.
#' @param causal_chain if TRUE, the causal chain is searched, if
#' FALSE, the underlying cause is searched
#' @param adjust_vars_override An optional vector of adjustment
#' variables that will override the automatic adjustment. Cannot be
#' more than two variables and must include age.
#' @param pval_cutoff P-value for which variables are considered
#' for adjustment. Ignored if adjust_vars_override is specified.
#' @param pct_na_cutoff Proportion of missingness for which
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
  cond_name_short = condition[1],
  causal_chain = TRUE,
  adjust_vars_override = NULL,
  factor_groups = NULL,
  pval_cutoff = 0.1,
  pct_na_cutoff = 20
) {
  if (is.null(sites))
    sites <- sort(unique(x$ads$site))

  res <- lapply(sites, function(st) {
    if (length(sites) > 1)
      message(st)
    get_rates_and_fractions_site(x,
      sites = st,
      catchments = catchments,
      group_catchments = group_catchments,
      condition = condition,
      icd10_regex = icd10_regex,
      cond_name_short = cond_name_short,
      causal_chain = causal_chain,
      adjust_vars_override = adjust_vars_override,
      factor_groups = factor_groups,
      pval_cutoff = pval_cutoff,
      pct_na_cutoff = pct_na_cutoff
    )
  })

  names(res) <- sapply(res, function(x) x$site)
  class(res) <- c("list", "rate_frac_multi_site")

  res
}

get_rates_and_fractions_site <- function(
  x,
  sites = NULL,
  catchments = NULL,
  group_catchments = TRUE,
  condition = NULL,
  icd10_regex = NULL,
  cond_name_short = condition[1],
  causal_chain = TRUE,
  adjust_vars_override = NULL,
  factor_groups = NULL,
  pval_cutoff = 0.1,
  pct_na_cutoff = 20
) {

  tbl1_dss <- mits_factor_tables(x,
    sites = sites,
    catchments = catchments,
    group_catchments = group_catchments,
    factor_groups = factor_groups,
    use_dss = TRUE
  )

  tbl1_non_dss <- mits_factor_tables(x,
    sites = sites,
    catchments = catchments,
    group_catchments = group_catchments,
    factor_groups = factor_groups,
    use_dss = FALSE
  )

  tbl2_dss <- cond_factor_tables(x,
    sites = sites,
    catchments = catchments,
    group_catchments = group_catchments,
    factor_groups = factor_groups,
    condition = condition,
    icd10_regex = icd10_regex,
    cond_name_short = cond_name_short,
    causal_chain = causal_chain,
    use_dss = TRUE
  )

  tbl2_non_dss <- cond_factor_tables(x,
    sites = sites,
    catchments = catchments,
    group_catchments = group_catchments,
    factor_groups = factor_groups,
    condition = condition,
    icd10_regex = icd10_regex,
    cond_name_short = cond_name_short,
    causal_chain = causal_chain,
    use_dss = FALSE
  )

  vars <- c("site", "catchment", "factor", "pval", "pct_na")
  crit <- dplyr::bind_rows(
    dplyr::select(tbl1_dss, dplyr::all_of(vars)),
    dplyr::select(tbl1_non_dss, dplyr::all_of(vars)),
    dplyr::select(tbl2_dss, dplyr::all_of(vars)),
    dplyr::select(tbl2_non_dss, dplyr::all_of(vars))
  )

  if (!is.null(adjust_vars_override)) {
    message("  using override adjustment variables:",
      paste0(adjust_vars_override, collapse = ", "))
    adjust_vars <- adjust_vars_override
  } else {
    # we need p-value and pct missing to meet critaria
    # for both dss and non-dss -- so need 4 records to meet criteria
    # if site has both dss and non-dss, or just 2 if dss-only or non-dss only
    n_to_select <- (nrow(tbl1_dss) > 0) * 2 + (nrow(tbl1_non_dss) > 0) * 2
    adj_cand <- crit %>%
      dplyr::filter(
        .data$pct_na < pct_na_cutoff, .data$pval < pval_cutoff
      ) %>%
      dplyr::group_by_at(c("site", "catchment", "factor")) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(.data$n == n_to_select) %>%
      dplyr::pull(.data$factor) %>%
      as.character()

    if (length(adj_cand) == 0) {
      message("  no adjustment variables")
      adjust_vars <- NULL
    } else if (length(adj_cand) == 1) {
      adjust_vars <- adj_cand
      message("  using adjustment variable: ", adjust_vars)
    } else {
      adj_order <- c("age" = 1, "season" = 2, "location" = 3,
        "va" = 4, "sex" = 5, "education" = 6)
      adj_cand_ord <- names(sort(adj_order[adj_cand]))
      if (! "age" %in% adj_cand) {
        adjust_vars <- adj_cand_ord[1]
        message("  using adjustment variable: ", adjust_vars)
      } else {
        adjust_vars <- adj_cand_ord[1:2]
        message("  using adjustment variables: ",
          paste0(adjust_vars, collapse = ", "))
      }
      leftover <- setdiff(adj_cand_ord, adjust_vars)
      if (length(leftover) > 0)
        message("  other significant factors not adjusted for: ",
          paste0(leftover, collapse = ", "))
    }
  }

  pop_mits <- dplyr::bind_rows(
    attr(tbl1_dss, "pop_mits"),
    attr(tbl1_non_dss, "pop_mits")
  ) %>%
    dplyr::group_by_at("site") %>%
    dplyr::summarise_all(sum)

  if (nrow(tbl1_dss) > 0) {
    rd_dss <- get_rate_frac_data(x,
      site = sites,
      catchments = catchments,
      condition = condition,
      icd10_regex = icd10_regex,
      causal_chain = causal_chain,
      factor_groups = factor_groups,
      adjust_vars = adjust_vars,
      use_dss = TRUE
    )
    lb <- sum(rd_dss$live_birth_data$live_births)
    u5d_sb <- pop_mits$u5d_sb
    acTU5MR <- 10000 * u5d_sb / (lb + pop_mits$stillbirths)
    acTU5MR_dss <- acTU5MR
    rd <- rd_dss
  }

  if (nrow(tbl1_non_dss) > 0) {
    rd_ndss <- get_rate_frac_data(x,
      site = sites,
      catchments = catchments,
      condition = condition,
      icd10_regex = icd10_regex,
      causal_chain = causal_chain,
      factor_groups = factor_groups,
      adjust_vars = adjust_vars,
      use_dss = FALSE
    )
    acTU5MR <- x$dhs %>%
      dplyr::filter(
        .data$site %in% rd_ndss$sites,
        .data$catchment %in% rd_ndss$catchments
      ) %>%
      dplyr::left_join(rd_ndss$year_range, by = c("site", "catchment")) %>%
      dplyr::filter(.data$year >= .data$start_year &
        .data$year <= .data$end_year) %>%
      # dplyr::group_by(.data$site, .data$catchment) %>%
      # dplyr::slice(which.max(.data$year)) %>%
      dplyr::pull(.data$rate) %>%
      mean()
    acTU5MR_ndss <- acTU5MR
    rd <- rd_ndss
  }

  # need to combine if it has dss and non-dss
  if (nrow(tbl1_dss) > 0 && nrow(tbl1_non_dss) > 0) {
    # browser()
    tmp <- rd_ndss$data
    tmp$target <- tmp$target + rd_dss$data$champs
    tmp$decode <- tmp$decode + rd_dss$data$decode
    tmp$condition <- tmp$condition + rd_dss$data$condition
    acTU5MR <- (acTU5MR_dss + acTU5MR_ndss) / 2
    rd$data <- tmp
    rd$sites <- unique(c(rd_dss$sites, rd_ndss$sites))
    rd$catchments <- unique(c(rd_dss$catchments, rd_ndss$catchments))
    rd$year_range <- dplyr::bind_rows(rd_dss$year_range, rd_ndss$year_range)
  }

  tmp <- x$ads %>%
    filter(site %in% rd$site, catchment %in% rd$catchments) %>%
    filter(mits_flag == 1, decoded == 1)
  tmp$cond_idx <- check_cond(tmp, condition, icd10_regex,
    causal_chain)
  crude_decoded <- nrow(tmp)
  crude_condition <- length(which(tmp$cond_idx == 1))

  res <- c(
    list(
      site = sites,
      catchments = catchments,
      group_catchments = group_catchments,
      condition = condition,
      icd10_regex = icd10_regex,
      cond_name_short = cond_name_short,
      causal_chain = causal_chain,
      adjust_vars = adjust_vars,
      factor_groups = factor_groups,
      pval_cutoff = pval_cutoff,
      pct_na_cutoff = pct_na_cutoff,
      mits_dss = tbl1_dss,
      mits_non_dss = tbl1_non_dss,
      cond_dss = tbl2_dss,
      cond_non_dss = tbl2_non_dss,
      pop_mits = pop_mits,
      crude_decoded = crude_decoded,
      crude_condition = crude_condition
    ),
    calculate_rates_fractions(rd, acTU5MR,
      adjust_vars = adjust_vars,
      crude_decoded = crude_decoded, crude_condition = crude_condition)
  )
  class(res) <- c("list", "rate_frac_site")
  res
}

#' Get data necessary to compute adjusted mortality fractions and rates
#' @param x an object read in from [read_and_validate_data()]
#' @param site a site name to get rate data for
#' @param catchments a vector of catchments to include in the calculations
#' @param condition a CHAMPS condition (see [valid_conditions()])
#' @param icd10_regex an optional regular expression specifying
#' ICD10 codes that define a condition
#' @param causal_chain if TRUE, the causal chain is searched, if
#' FALSE, the underlying cause is searched
#' @param adjust_vars a vector of variables to adjust by
#' @param factor_groups A named list that specifies how to group factors
#' @param use_dss Should the calculations be done only for catchments that
#' @note One or both of `icd10_regex` and `condition` must be specified
# @export
get_rate_frac_data <- function(x,
  site,
  catchments = NULL,
  condition = NULL,
  icd10_regex = NULL,
  causal_chain = TRUE,
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
    # assertthat::assert_that(length(condition) == 1)
    conds <- valid_conditions(x)
    assertthat::assert_that(all(condition %in% conds$condition),
      msg = cli::format_error("Must provide a valid condition. See \\
        valid_conditions() for a list.")
    )
  }

  obj <- get_ctch(x, site, catchments)
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

  cond <- x$ads %>%
    dplyr::filter(
      .data$site %in% sites,
      .data$catchment %in% catchments,
      .data$mits_flag == 1,
      .data$decoded == 1) %>%
    mutate(
      cond_cc = check_cond(.data, condition, icd10_regex, causal_chain)
    ) %>%
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

  res <- list(
    data = tbldat,
    sites = sites,
    catchments = catchments,
    year_range = year_range,
    live_birth_data = ld,
    total_live_births = sum(ld$live_births),
    dss = use_dss
  )

  class(res) <- c("list", "rate_frac_data")
  res
}

calculate_rates_fractions <- function(
  rd, acTU5MR, adjust_vars, crude_decoded, crude_condition, ci_limit = 90
) {
  # ci_limit <- 90

  # figure out how to group
  tmp <- rd$data %>%
    dplyr::mutate(
      selprob = ifelse(.data$target == 0, 0, .data$decode / .data$target)
    ) %>%
    dplyr::arrange(.data$selprob)

  # browser()

  will_adjust <- !is.null(adjust_vars)

  add_count <- FALSE
  if (will_adjust) {
    if (length(which(tmp$condition > 0)) == 1) {
      newrd <- rd$data %>%
        dplyr::select_if(is.numeric) %>%
        dplyr::mutate(group = condition == 0) %>%
        dplyr::group_by(.data$group) %>%
        dplyr::summarise_all(sum)
      add_count <- newrd$condition == 0
    } else if (length(which(tmp$condition == 0)) > 0) {
      nonzero <- which(tmp$condition != 0)
      zero <- which(tmp$condition == 0)
      newcls <- sapply(zero, function(idx) {
        nonzero[which.min(abs(tmp$selprob[idx] - tmp$selprob[nonzero]))]
      })

      tmp$group <- NA
      tmp$group[nonzero] <- nonzero
      tmp$group[zero] <- newcls

      newrd <- tmp %>%
        dplyr::select_if(is.numeric) %>%
        dplyr::group_by(.data$group) %>%
        dplyr::summarise_all(sum) %>%
        dplyr::mutate(
          group = as.numeric(factor(.data$group)),
          selprob = .data$decode / .data$target
          # adjust = condition / mits
        )
    } else {
      newrd <- rd$data
      newrd$group <- seq_len(nrow(newrd))
    }
  } else {
    newrd <- rd$data
  }
  # CSMF
  decode <- sum(newrd$decode)
  condition <- sum(newrd$condition)
  # crude mortality fraction
  cCSMF <- 100 * crude_condition / crude_decoded
  cCSMF_CrI <- get_interval(
    crude_condition / crude_decoded, crude_decoded, ci_limit)
  # print_ci(cCSMF, cCSMF_CrI)

  # adjusted CSMF
  # if no adjustment, adjusted is crude
  if (!will_adjust) {
    aCSMF <- cCSMF
    aCSMF_CrI <- cCSMF_CrI
  } else {
    ns <- newrd$decode + 0.5 * add_count
    n <- newrd$condition + 0.5 * add_count
    N <- newrd$target + 0.5 * add_count
    aCSMF <- 100 * (n / ns) %*% (N / sum(N)) %>% as.vector()
    aCSMF_CrI <- get_interval(aCSMF / 100, sum(N))
    # print_ci(aCSMF, aCSMF_CrI)
  }

  frac <- dplyr::tibble(
    site = rd$site,
    catchments = paste(rd$catchments, collapse = ", "),
    var = c("cCSMF", "aCSMF"),
    label = c(
      "Crude cause-specific mortality fraction",
      "Adjusted cause-specific mortality fraction"
    ),
    decode = c(crude_decoded, decode),
    condition = c(crude_condition, condition),
    est = c(cCSMF, aCSMF),
    lower = c(cCSMF_CrI[1], aCSMF_CrI[1]),
    upper = c(cCSMF_CrI[2], aCSMF_CrI[2])
  )

  # crude and adjusted mortality rates
  cTU5MR <- (cCSMF / 100) * acTU5MR
  cTU5MR_CrI <- (cCSMF_CrI / 100) * acTU5MR
  # print_ci(cTU5MR, cTU5MR_CrI)

  if (!will_adjust) {
    aTU5MR <- cTU5MR
    aTU5MR_CrI <- cTU5MR_CrI
  } else {
    aTU5MR <- (aCSMF / 100) * acTU5MR
    aTU5MR_CrI <- (aCSMF_CrI / 100) * acTU5MR
    # print_ci(aTU5MR, aTU5MR_CrI)
  }

  rate <- dplyr::tibble(
    site = rd$site,
    catchments = paste(rd$catchments, collapse = ", "),
    var = c("cTU5MR", "aTU5MR"),
    label = c(
      "Crude total under-5 mortality rate",
      "Adjusted total under-5 mortality rate"
    ),
    allcauseTU5MR = acTU5MR,
    est = c(cTU5MR, aTU5MR),
    lower = c(cTU5MR_CrI[1], aTU5MR_CrI[1]),
    upper = c(cTU5MR_CrI[2], aTU5MR_CrI[2])
  )

  res <- list(
    frac = frac,
    rate = rate,
    rate_data = rd,
    rate_data_grouped = newrd
  )

  class(res) <- c("list", "rate_frac_calc")
  res
}

check_cond <- function(., group, rgx, causal_chain) {
  if (is.null(group))
    return(has_icd10(., rgx, cc = causal_chain))
  if (is.null(rgx))
    return(has_champs_group(., group, cc = causal_chain))
  has_icd10(., rgx, cc = causal_chain) |
    has_champs_group(., group, cc = causal_chain)
}
