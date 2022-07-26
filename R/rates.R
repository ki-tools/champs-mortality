#' Get rate and fraction data
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
  # group_catchments = TRUE,
  condition = NULL,
  icd10_regex = NULL,
  maternal = FALSE,
  cond_name_short = condition[1],
  causal_chain = TRUE,
  adjust_vars_override = NULL,
  factor_groups = NULL,
  pval_cutoff = 0.1,
  pct_na_cutoff = 20
) {
  if (is.null(sites))
    sites <- sort(unique(x$ads$site))

  if (is.null(factor_groups)) {
    x$dhs <- dplyr::filter(x$dhs, .data$age == "U5")
  }
  # apply any filtering that may be specified in factor_groups
  for (nm in names(factor_groups)) {
    cur_group <- factor_groups[[nm]]
    lvls <- unlist(unname(cur_group))
    if (nm %in% names(x$ads)) {
      idx <- which(x$ads[[nm]] %in% lvls)
      x$ads <- x$ads[idx, ]
    }
    if (nm %in% names(x$dss)) {
      idx <- which(x$dss[[nm]] %in% lvls)
      x$dss <- x$dss[idx, ]
    }
    if (nm == "age") {
      # DHS data doesn't have stillbirths
      lvls2 <- setdiff(lvls, "Stillbirth")
      # need to match up DHS to specified age levels
      # (DHS has Neonate, Infant, Child, Neonate + Infant, U5)
      if (all(c("Neonate", "Infant", "Child") %in% lvls2)) {
        filt_val <- "U5"
      } else if (all(c("Neonate", "Infant") %in% lvls2)) {
        filt_val <- "Neonate + Infant"
      } else {
        filt_val <- lvls2
      }
      x$dhs <- x$dhs %>%
        dplyr::filter(.data$age %in% filt_val)
    }
  }

  res <- lapply(sites, function(st) {
    if (length(sites) > 1)
      message(st)
    get_rates_and_fractions_site(x,
      sites = st,
      catchments = catchments,
      # group_catchments = group_catchments,
      condition = condition,
      icd10_regex = icd10_regex,
      maternal = maternal,
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
  # group_catchments = TRUE,
  condition = NULL,
  icd10_regex = NULL,
  maternal = FALSE,
  cond_name_short = condition[1],
  causal_chain = TRUE,
  adjust_vars_override = NULL,
  factor_groups = NULL,
  pval_cutoff = 0.1,
  pct_na_cutoff = 20
) {
  tbl1 <- mits_factor_tables(x,
    sites = sites,
    catchments = catchments,
    # group_catchments = group_catchments,
    factor_groups = factor_groups
  )

  tbl2 <- cond_factor_tables(x,
    sites = sites,
    catchments = catchments,
    # group_catchments = group_catchments,
    factor_groups = factor_groups,
    condition = condition,
    icd10_regex = icd10_regex,
    maternal = maternal,
    cond_name_short = cond_name_short,
    causal_chain = causal_chain
  )

  vars <- c("site", "catchment", "factor", "pval", "pct_na")
  crit <- dplyr::bind_rows(
    dplyr::select(tbl1, dplyr::all_of(vars)),
    dplyr::select(tbl2, dplyr::all_of(vars)),
  )

  if (!is.null(adjust_vars_override)) {
    message("  using override adjustment variables:",
      paste0(adjust_vars_override, collapse = ", "))
    adjust_vars <- adjust_vars_override
  } else {
    # we need p-value and pct missing to meet critaria
    adj_cand <- crit %>%
      dplyr::filter(
        .data$pct_na < pct_na_cutoff, .data$pval < pval_cutoff
      ) %>%
      dplyr::group_by_at(c("site", "catchment", "factor")) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(.data$n == 2) %>%
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
    attr(tbl1, "pop_mits")
  ) %>%
    dplyr::group_by_at("site") %>%
    dplyr::summarise_all(sum)

  can_use_dss <- attr(tbl1, "can_use_dss")
  has_dhs <- !can_use_dss

  rd <- get_rate_frac_data(x,
    site = sites,
    catchments = catchments,
    condition = condition,
    icd10_regex = icd10_regex,
    maternal = maternal,
    causal_chain = causal_chain,
    factor_groups = factor_groups,
    adjust_vars = adjust_vars
  )

  # if a mix of DSS/non-DSS, calculate for DSS and use DHS for non-DSS and avg
  acMR <- NULL
  acMR_dss <- NULL
  acMR_dhs <- NULL
  if (nrow(rd$live_birth_data) > 0) {
    lb <- rd$live_birth_data %>%
      dplyr::left_join(rd$year_range, by = c("site", "catchment")) %>%
      dplyr::filter(.data$year >= .data$start_year &
        .data$year <= .data$end_year) %>%
      dplyr::pull(.data$live_births) %>%
      sum()
    u5d_sb <- pop_mits$u5d_sb
    acMR_dss <- 10000 * u5d_sb / (lb + pop_mits$stillbirths)
    acMR <- acMR_dss
  }

  if (has_dhs) {
    acMR_dhs <- x$dhs %>%
      dplyr::filter(
        .data$site %in% rd$sites,
        .data$catchment %in% rd$catchments
      ) %>%
      dplyr::left_join(rd$year_range, by = c("site", "catchment")) %>%
      dplyr::filter(.data$year >= .data$start_year &
        .data$year <= .data$end_year) %>%
      dplyr::group_by_at(c("site", "catchment", "year")) %>%
      dplyr::summarise(rate = sum(.data$rate)) %>%
      # dplyr::group_by(.data$site, .data$catchment) %>%
      # dplyr::slice(which.max(.data$year)) %>%
      dplyr::pull(.data$rate) %>%
      mean()

    if (!is.null(acMR)) {
      acMR <- mean(c(acMR, acMR_dhs))
    } else {
      acMR <- acMR_dhs
    }
  }

  # # need to combine if it has dss and non-dss
  # if (nrow(tbl1_dss) > 0 && nrow(tbl1_non_dss) > 0) {
  #   browser()
  #   tmp <- rd_ndss$data
  #   tmp$target <- tmp$target + rd_dss$data$champs
  #   tmp$decode <- tmp$decode + rd_dss$data$decode
  #   tmp$condition <- tmp$condition + rd_dss$data$condition
  #   acMR <- (acMR_dss + acMR_ndss) / 2
  #   rd$data <- tmp
  #   rd$sites <- unique(c(rd_dss$sites, rd_ndss$sites))
  #   rd$catchments <- unique(c(rd_dss$catchments, rd_ndss$catchments))
  #   rd$year_range <- dplyr::bind_rows(rd_dss$year_range, rd_ndss$year_range)
  # }

  tmp <- x$ads %>%
    dplyr::filter(
      .data$site %in% rd$site,
      .data$catchment %in% rd$catchments) %>%
    dplyr::filter(.data$mits_flag == 1, .data$decoded == 1)
  tmp$cond_idx <- check_cond_switch(tmp, condition, icd10_regex,
    causal_chain, maternal)
  crude_decoded <- nrow(tmp)
  crude_condition <- length(which(tmp$cond_idx == 1))

  res <- c(
    list(
      site = sites,
      catchments = catchments,
      # group_catchments = group_catchments,
      condition = condition,
      icd10_regex = icd10_regex,
      maternal = maternal,
      cond_name_short = cond_name_short,
      causal_chain = causal_chain,
      adjust_vars = adjust_vars,
      factor_groups = factor_groups,
      pval_cutoff = pval_cutoff,
      pct_na_cutoff = pct_na_cutoff,
      mits = tbl1,
      cond = tbl2,
      pop_mits = pop_mits,
      crude_decoded = crude_decoded,
      crude_condition = crude_condition,
      can_use_dss = can_use_dss,
      acMR_dss = acMR_dss,
      acMR_dhs = acMR_dhs
    ),
    calculate_rates_fractions(rd, acMR,
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
#' @param maternal Are we searching for a maternal condition or ICD10
#' specification? Default is FALSE.
#' @param causal_chain if TRUE, the causal chain is searched, if
#' FALSE, the underlying cause is searched
#' @param adjust_vars a vector of variables to adjust by
#' @param factor_groups A named list that specifies how to group factors
#' @note One or both of `icd10_regex` and `condition` must be specified
# @export
get_rate_frac_data <- function(x,
  site,
  catchments = NULL,
  condition = NULL,
  icd10_regex = NULL,
  maternal = FALSE,
  causal_chain = TRUE,
  adjust_vars = NULL,
  factor_groups = NULL
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

  obj <- get_ctch(x, site, catchments)
  sites <- obj$sites
  ctch <- obj$ctch
  catchments <- obj$catchments
  gctch <- obj$gctch
  use_dss <- obj$can_use_dss

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
      cond_cc = check_cond_switch(
        .data, condition, icd10_regex, causal_chain, maternal
      )
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
    total_live_births = sum(ld$live_births)
  )

  class(res) <- c("list", "rate_frac_data")
  res
}

calculate_rates_fractions <- function(
  rd, acMR, adjust_vars, crude_decoded, crude_condition, ci_limit = 90
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
  cTU5MR <- (cCSMF / 100) * acMR
  cTU5MR_CrI <- (cCSMF_CrI / 100) * acMR
  # print_ci(cTU5MR, cTU5MR_CrI)

  if (!will_adjust) {
    aTU5MR <- cTU5MR
    aTU5MR_CrI <- cTU5MR_CrI
  } else {
    aTU5MR <- (aCSMF / 100) * acMR
    aTU5MR_CrI <- (aCSMF_CrI / 100) * acMR
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
    allcauseMR = acMR,
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
