#' @title Wrapper for get_rates_and_fractions()
#' @description Uses the get_rates_and_fractions() function but outputs only the
#' rates and fractions in one row instead of multiple tables in a list.
#' @param inputs is a list object with of the following named items
#' in the list - Catchment, Site, Age, Condition , UC_or_CC, DSS.
#' The UC_or_CC can be 'UC' or 'CC' and DS is 'Yes' or 'No'
#' get_results_wide
#' @param dat is the object returned from process_data()
#' @export
get_results_wide <- function(inputs, dat) {

    # handle manual or lists of lists from purrr
    if (length(inputs) == 1) {
        inputs <- inputs[[1]]
        if (length(inputs) != 6) stop("Inputs more than six")
    } else if (length(inputs) != 6) {
        stop("Inputs more than six")
    }

    # filter to age group
    dat$ads <- filter(dat$ads, .data$age == inputs$Age)
    dat$dss <- filter(dat$dss, .data$age == inputs$Age)

    # causal chain input
    if (inputs$UC_or_CC == "CC") {
        ccp <- TRUE
        } else if (inputs$UC_or_CC == "UC") {
        ccp <- FALSE
        } else {
        stop("UC_or_CC not one of 'UC' or 'CC'")
        }

    # Expand semi-colon seperated vars
    icatchments <- inputs$Catchment %>%
        strsplit(";") %>%
        unlist() %>%
        trimws()

    isites <- inputs$Site %>%
        strsplit(";") %>%
        unlist() %>%
        trimws()

    # Do calculations
    dat_calc <- get_rates_and_fractions(dat,
        sites = isites,
        catchments = icatchments,
        causal_chain = ccp, #FALSE if underlying
        pval_cutoff = 0.1, #Fixed
        pct_na_cutoff = 20, #Fixed
        condition = inputs$Condition #Condition name
    )
    # not sure how ajdust vars is reported. May not need collapse
    ivars <- dat_calc[[inputs$Site]][["adjust_vars"]]
    if (is.null(ivars)) {
        adjust_vars <- "None"
    } else if (length(ivars) == 1) {
        adjust_vars <- ivars
    } else {
        adjust_vars <- paste(
            dat_calc[[inputs$Site]][["adjust_vars"]],
            collapse = ",")
    }

    # site and catchment output of function
    isites <- dat_calc[[inputs$Site]][['rate_data']][['sites']]
    isites <- ifelse(
        length(isites) == 1,
        isites,
        paste(isites, collapse = "; "))

    icatchments <- dat_calc[[inputs$Site]][['rate_data']][['catchments']]
    icatchments <- ifelse(
        length(icatchments) == 1,
        icatchments,
        paste(icatchments, collapse = "; "))

    # Two primary tables for outputs
    fracs <- dat_calc[[inputs$Site]][['frac']]
    rates <- dat_calc[[inputs$Site]][['rate']]

    out <- dplyr::tibble(
        # inputs
        Catchment = inputs$Catchment,
        Site = inputs$Site,
        Age = inputs$Age,
        Condition = inputs$Condition,
        UC_or_CC = inputs$UC_or_CC,
        DSS = inputs$DSS,
        # actual catcment, site
        Site_calc = isites,
        Catchment_calc = icatchments,
        # fracs
        DeCoDe = fracs$decode[1], # always first row?
        n = fracs$condition[1], # always first row?
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
        Factors = adjust_vars
    )
    print(out[, 1:9])
    out
}


#' @title Calculate rates and fraction of an input list of values.
#' @description Allows user to provide an input table in CSV format
#' to itteratively calculate results using get_rates_and_fractions()
#' with the output returned in one row per set of inputs.
#' @param inputs_csv is a path to your file.
#' @param dat_folder is a path for use with read_and_validate_data()
#' @param start_year is the start_year argument passed to
#' read_and_validate_data()
#' @param end_year is the end_year argument passed to
#' read_and_validate_data()
#' @export
input_rates_and_fractions <- function(
    inputs_csv,
    dat_folder,
    start_year, end_year) {

    input_list <- read_csv(inputs_csv) %>%
        purrr::transpose()

    d <- read_and_validate_data(dat_folder) %>%
        process_data(start_year = start_year, end_year = end_year)

    out_df <- purrr::map_df(input_list, get_results_wide, dat = d)
    out_df
}