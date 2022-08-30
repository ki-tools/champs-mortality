#' @title Wrapper for get_rates_and_fractions()
#' @description Converts detailed output `get_rates_and_fractions()` to a
#' data frame of the major output statistics.
#' @param dat A list of outputs from either get_rates_and_fractions() or
#' `batch_rates_and_fractions()`.
#' @export
rates_fracs_to_df <- function(dat) {

  if (inherits(dat, "rate_frac_site")) {
    dat <- list(dat)
    class(dat) <- c("list", "rate_frac_multi_site")
  }

  check_multi_site_output(dat, "rates_fracs_to_df()")

  collapse_vec <- function(x) {
    ifelse(is.null(x), "None", paste(x, collapse = ","))
  }

  res <- lapply(dat, function(x) {
    # not sure how adjust vars is reported. May not need collapse
    adjust_vars <- ifelse(is.null(x$adjust_vars), "None",
      paste(x$adjust_vars, collapse = ","))

    age <- list(
      Neonate = "Neonate",
      Infant = "Infant",
      Child = "Child"
    )
    age <- list(
      Infant = c("Neonate", "Infant"),
      Child = "Child"
    )
    age <- x$inputs$factor_groups$age
    if (is.null(age)) {
      age <- paste(valid_levels$age, collapse = ",")
    } else {
      pref <- names(age)
      post <- unname(sapply(age, function(x) paste(x, collapse = "+")))
      pref <- ifelse(pref == post, "", paste0(pref, "="))
      age <- paste(paste0(pref, post), collapse = ",")
    }

    # Two primary tables for outputs
    fracs <- x$frac
    rates <- x$rate

    out <- dplyr::tibble(
      # inputs
      site = collapse_vec(x$site),
      catchments = collapse_vec(x$catchments),
      age = age,
      condition = collapse_vec(x$inputs$condition),
      icd10_regex = x$inputs$icd10_regex,
      cond_name_short = x$inputs$cond_name_short,
      dss_only = x$can_use_dss,
      # fracs
      DeCoDe = fracs$decode[1],
      n = fracs$condition[1],
      cCSMF = fracs$est[1],
      cCSMF_LL = fracs$lower[1],
      cCSMF_UL = fracs$upper[1],
      aCSMF = fracs$est[2],
      aCSMF_LL = fracs$lower[2],
      aCSMF_UL = fracs$upper[2],
      # rates
      cCSMR = rates$est[1],
      cCSMR_LL = rates$lower[1],
      cCSMR_UL = rates$upper[1],
      aCSMR = rates$est[2],
      aCSMR_LL = rates$lower[2],
      aCSMR_UL = rates$upper[2],
      # Factors
      adj_factors = adjust_vars
    )
    out
  })

  dplyr::bind_rows(res)
}

#' @title Calculate rates and fraction of an input list of values.
#' @description Allows user to provide an input table in CSV format
#' to iteratively calculate results using get_rates_and_fractions()
#' with the output returned in one row per set of inputs.
#' @param dat Processed CHAMPS dataset.
#' @param inputs Path to a csv file specifying each scenario to run with
#' columns 'site','catchment', 'age', 'condition', 'icd10_regex',
#' 'causal_chain'. The columns 'age', 'condition' ,'site', and 'catchment'
#' can have multiple values separated by a semicolon. These variables
#' specify parameters that will be passed to `get_rates_and_fractions()`
#' Alternately, this can be a list of named lists of parameters with the
#' same names of the csv file. See examples below.
#' @export
batch_rates_and_fractions <- function(
  dat,
  inputs
) {
  if (is.character(inputs)) {
    input_list <- readr::read_csv(inputs, show_col_types = FALSE) %>%
      purrr::transpose()
  } else {
    input_list <- inputs
  }

  valid_nms <- c("site", "catchment", "age", "condition",
    "icd10_regex", "causal_chain")

  check <- FALSE
  if (is.list(input_list))
    check <- sapply(input_list, function(x) all(valid_nms %in% names(x)))
  if (!all(check))
    stop("Inputs must be a path to a csv file or list of lists with names: ",
      paste(valid_nms, collapse = ", "))

  # change any NA entry to NULL
  # and expand any semicolon-separated
  input_list <- lapply(input_list, function(x) {
    nms <- names(x)
    if (is.null(nms))
      stop("Input list must be a list of lists with names: ",
        paste(valid_nms, collapse = ", "))
    res <- lapply(nms, function(nm) {
      a <- x[[nm]]
      if (length(a) == 1 && is.na(a))
        return(NULL)
      if (length(a) == 1 && grepl(";", a)
        && nm %in% c("age", "catchment", "site", "condition")
      ) {
          a <- strsplit(a, ";")[[1]] %>% trimws()
        }
      if (nm == "age")
        a <- structure(as.list(a), names = a)
      return(a)
    })
    names(res) <- nms
    res
  })

  res <- lapply(input_list, function(x) {
    get_rates_and_fractions(dat,
      sites = x$site,
      catchments = x$catchment,
      causal_chain = x$causal_chain,
      condition = x$condition,
      icd10_regex = x$icd10_regex,
      factor_groups = list(age = x$age),
      maternal = x$maternal
    )
  })

  res <- unlist(res, recursive = FALSE)
  class(res) <- c("list", "rate_frac_multi_site")

  res
}
