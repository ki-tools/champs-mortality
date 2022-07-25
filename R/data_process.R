#' Process a set of CHAMPS data files
#' @param x an object read in from [read_and_validate_data()]
#' @param start_year start year for filtering the data
#' @param end_year end year for filtering the data
#' @importFrom dplyr all_of case_when count distinct filter group_by left_join
#' mutate one_of recode rename row_number select ungroup %>%
#' @importFrom utils capture.output
#' @importFrom rlang .data :=
#' @export
process_data <- function(x, start_year, end_year) {
  assertthat::assert_that(inherits(x, "champs_files"),
    msg = cli::format_error("Data must come from read_and_validate_data()")
  )

  voc <- x$voc
  ads_raw <- x$ads
  mreg <- x$mreg
  dss <- x$dss
  # rlgn <- x$rlgn
  seas <- x$seas
  catlkp <- x$catlkp
  lb <- x$lb
  dhs <- x$dhs

  valid_sites <- unique(ads_raw$site_name)
  valid_catchments <- unique(catlkp$catchment)
  valid_locations <- unique(ads_raw$calc_location)

  # ------------------- analysis dataset ------------------- #
  cli::cli_h1("processing analysis dataset")

  ads_vars_keep <- snakecase::to_snake_case(ads_names)

  missing_ads_vars <- setdiff(ads_vars_keep, names(ads_raw))
  n_missing <- length(missing_ads_vars)
  assertthat::assert_that(n_missing == 0,
    msg = cli::format_error("
      {n_missing} necessary variable{?s} not found in the analysis dataset: \\
      {commas(missing_ads_vars)}"))

  # ads_raw$age_group <- ads_raw$case_type_calc

  nrm <- length(setdiff(names(ads_raw), ads_vars_keep))
  if (nrm > 0) {
    ads_raw <- dplyr::select(ads_raw, dplyr::one_of(ads_vars_keep))
    cli::cli_alert_success("Removed {nrm} unnecessary variables from analysis \\
      dataset", wrap = TRUE)
  }

  ads_raw$calc_dod <- as.Date(ads_raw$calc_dod)
  ads_raw$year <- lubridate::year(ads_raw$calc_dod)
  cli::cli_alert_success("Calculated year of death from 'calc_dod'",
    wrap = TRUE)

  dups <- ads_raw %>%
    dplyr::count(.data$champsid) %>%
    dplyr::filter(.data$n > 1)
  n_dups <- nrow(dups)
  if (n_dups > 0) {
    ads_raw <- ads_raw %>%
      dplyr::group_by(.data$champsid) %>%
      filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
    cli::cli_alert_success("The following {n_dups} CHAMPS ID{?s} have \\
      duplicate entries in the maternal registry dataset and only the first \\
      record for each was kept: {commas(dups$champsid)}", wrap = TRUE)
  }

  ads_raw <- voc_lookup(ads_raw, voc, "age_group", "analysis dataset")
  nna <- length(which(is.na(ads_raw$age_group)))
  if (nna > 0) {
    cli::cli_alert_warning("NOTE: Found {nna} missing values for \\
      'age_group'... removing these records", wrap = TRUE)
    ads_raw <- filter(ads_raw, !is.na(.data$age_group))
  }

  tmp <- ads_raw$age_group
  tmp[ads_raw$age_group_code %in% c("CH01404", "CH01405", "CH01406")] <-
    "Neonate"
  ads_raw$age <- factor(tmp, levels = valid_levels$age)
  cli::cli_alert_success("Created a new variable 'age' that rolls up \\
    all neonates into one category to be compatible with DSS data",
    wrap = TRUE)

  # ads_raw <- voc_lookup(ads_raw, voc, "caretakers_religion", "analysis dataset")

  ads_raw <- voc_lookup(ads_raw, voc, "calc_sex", "analysis dataset")

  ### ignore religion
  # ads_raw <- ads_raw %>%
  #   dplyr::mutate(caretakers_religion_code =
  #     substr(.data$caretakers_religion_code, 1, 7)) %>%
  #   dplyr::left_join(
  #     dplyr::select(rlgn,
  #       dplyr::one_of(c("champs_local_code", "religion"))),
  #     by = c(caretakers_religion_code = "champs_local_code"))
  # cli::cli_alert_success("Created a new variable 'religion' that \\
  #   rolls up religions into categories compatible with DSS data",
  #   wrap = TRUE)

  # not_handled <- setdiff(unique(
  #   ads_raw$caretakers_religion_code[is.na(ads_raw$religion)]), NA)
  # nnh <- length(not_handled)
  # if (nnh > 0) {
  #   cli::cli_alert_warning("There are {nnh} religion code{?s} that \\
  #     do not match with any religion codes found in the \\
  #     religion_lookup file: {commas(not_handled)} - it may be useful \\
  #     to look these up and add them to the religion_lookup dataset",
  #     wrap = TRUE)
  # }

  tmp <- suppressWarnings(
    as.numeric(substr(ads_raw$va_cause_1_iva, 1, 5))
  )

  ads_raw <- ads_raw %>%
    dplyr::mutate(
      va_cod_iva = dplyr::case_when(
        tmp >= 12 & tmp < 13     ~ "Trauma",
        tmp >= 1 & tmp < 2       ~ "Infection",
        tmp >= 10.3 & tmp < 10.6 ~ "Infection",
        !is.na(tmp)              ~ "Other",
        TRUE                     ~ as.character(NA)
      )
    )
  cli::cli_alert_success("Classified Verbal Autopsy Inter-VA Cause 1 codes \\
    as 'Infection', 'Trauma', 'Other', or NA", wrap = TRUE)

  nr1 <- nrow(ads_raw)
  ads_raw <- filter(ads_raw, .data$year <= end_year, .data$year >= start_year)
  nd <- nr1 - nrow(ads_raw)
  if (nd > 0) {
    cli::cli_alert_success("Removed {nd} record{?s} that occur before \\
      {start_year} or after {end_year} or have a missing date of death",
      wrap = TRUE)
  }

  ads_raw <- dplyr::left_join(ads_raw,
    catlkp, by = c("site_name", "catchment_id"))
  cli::cli_alert_success("Resolved catchment names from catchment \\
    IDs using catchment lookup table")
  tmp <- ads_raw %>%
    dplyr::filter(is.na(.data$catchment)) %>%
    dplyr::select(dplyr::all_of(c("site_name", "catchment_id"))) %>%
    dplyr::distinct()
  if (nrow(tmp) > 0) {
    cli::cli_alert_warning("The following {nrow(tmp)} catchment ID{?s} \\
      were not found in the lookup table:")
      lns <- utils::capture.output(print(tmp))
      lns <- lns[-c(1, 3)]
      # cli::cli_alert(paste(lns, collapse = "\n"))
      lapply(lns, cli::cli_alert)
  }

  check_valid_vals(ads_raw, "catchment", valid_catchments,
    "catchment values", "analysis dataset")

  # ads_raw %>%
  #   mutate(ch4 = substr(champsid, 1, 4)) %>%
  #   select(site_name, catchment_id, ch4) %>%
  #   arrange(site_name, catchment_id) %>%
  #   distinct()

  # nr1 <- nrow(ads_raw)
  # ads_raw <- filter(ads_raw, mits_flag == 1)
  # nd <- nr1 - nrow(ads_raw)
  # if (nd > 0) {
  #   cli::cli_alert_success("Removed {nd} record{?s} that do not have \\
  #     MITS flag = 1", wrap = TRUE)
  # }

  ads_raw <- dplyr::rename(ads_raw, decoded = "m_00060")
  # nr1 <- nrow(ads_raw)
  # ads_raw <- filter(ads_raw, m_00060 == 1)
  # nd <- nr1 - nrow(ads_raw)
  # if (nd > 0) {
  #   cli::cli_alert_success("Removed {nd} record{?s} that were not \\
  #     DeCoDed", wrap = TRUE)
  # }

  ads_raw <- ads_raw %>%
    dplyr::mutate(
      # religion = dplyr::recode(.data$religion,
      #   Unknown = as.character(NA)
      # ),
      calc_sex = dplyr::recode(.data$calc_sex,
        Indeterminate = as.character(NA),
        Unknown = as.character(NA)
      ),
      calc_location = dplyr::recode(.data$calc_location,
        community = "Community",
        facility = "Facility",
        other = as.character(NA)
      )
    )

  cli::cli_alert_success("Modified some analysis set variables to match \\
    what is found in DSS data:")
  ul <- cli::cli_ul()
  # cli::cli_li("religion: set 'Unknown' to NA")
  cli::cli_li("sex: set 'Indeterminate' and 'Unknown' to NA")
  cli::cli_li("location: 'other' to NA")
  cli::cli_end(ul)

  check_valid_vals(ads_raw, "age", valid_levels$age,
    "age values", "analysis dataset")

  # check_valid_vals(ads_raw, "religion", valid_levels$religion,
  #   "religion values", "analysis dataset")

  check_valid_vals(ads_raw, "calc_sex", valid_levels$sex,
    "sex values", "analysis dataset")

  check_valid_vals(ads_raw, "calc_location", valid_levels$location,
    "location values", "analysis dataset")

  check_valid_vals(ads_raw, "va_cod_iva", valid_levels$va,
    "va values", "analysis dataset")

  check_valid_vals(seas, "site_name", valid_sites,
    "site values", "seasons lookup")

  ads_raw$season <- NA
  for (ii in seq_len(nrow(seas))) {
    tmp <- seas[ii, ]
    idx <- ads_raw$site_name == tmp$site &
      ads_raw$calc_dod >= tmp$start & ads_raw$calc_dod <= tmp$end
    ads_raw$season[idx] <- tmp$season
  }
  cli::cli_alert_success("Calculated season of death")

  check_valid_vals(seas, "season", valid_levels$season,
    "season values", "seasons lookup")

  ads_raw <- dplyr::rename(ads_raw,
      location = "calc_location",
      sex = "calc_sex",
      va = "va_cod_iva",
      site = "site_name"
    )

  # ------------------- maternal registry ------------------ #
  cli::cli_h1("processing maternal registry")

  reg_vars_keep <- snakecase::to_snake_case(mreg_names)

  missing_reg_vars <- setdiff(reg_vars_keep, names(mreg))
  n_missing <- length(missing_reg_vars)
  assertthat::assert_that(n_missing == 0,
    msg = cli::format_error("
      {n_missing} necessary variable{?s} not found in the maternal registry \\
        dataset: {commas(missing_reg_vars)}"))

  nrm <- length(setdiff(names(mreg), reg_vars_keep))
  if (nrm > 0) {
    mreg <- mreg %>%
      dplyr::select(dplyr::all_of(reg_vars_keep))

    cli::cli_alert_success("Removed {nrm} unnecessary variables from maternal \\
      registry dataset", wrap = TRUE)
  }

  mreg <- mreg %>%
    dplyr::rename(
        champsid = "mort_id",
        est_mat_age = "mat_0010",
        known_mat_age = "mat_0011",
        education = "mat_0013"
      )

  nna <- length(which(is.na(mreg$champsid)))
  if (nna > 0) {
    mreg <- dplyr::filter(mreg, !is.na(.data$champsid))
    cli::cli_alert_success("Removed {nna} record{?s} with missing 'champsid' \\
      from maternal registry dataset", wrap = TRUE)
  }

  dups <- mreg %>%
    dplyr::count(.data$champsid) %>%
    dplyr::filter(.data$n > 1)
  n_dups <- nrow(dups)
  if (n_dups > 0) {
    mreg <- mreg %>%
      dplyr::group_by(.data$champsid) %>%
      filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
    cli::cli_alert_success("The following {n_dups} CHAMPS ID{?s} have \\
      duplicate entries in the maternal registry dataset and only the first \\
      record for each was kept: {commas(dups$champsid)}", wrap = TRUE)
  }

  mreg <- voc_lookup(mreg, voc, "education", "maternal registry dataset")
  # TODO: make sure these match valid_education

  mreg <- mreg %>%
    dplyr::mutate(education = dplyr::recode(.data$education,
      "No Education" = "None"
    )
  )
  # cli::cli_li("education: 'No Education' to 'None'")

  check_valid_vals(mreg, "education", valid_levels$education,
    "education values", "maternal registry dataset")

  ads <- dplyr::left_join(ads_raw, mreg, by = "champsid")
  cli::cli_alert_success("Joined analysis dataset and maternal registry")

  # ads$religion <- factor(ads$religion, levels = valid_levels$religion)
  ads$education <- factor(ads$education, levels = valid_levels$education)
  ads$sex <- factor(ads$sex, levels = valid_levels$sex)
  ads$season <- factor(ads$season, levels = valid_levels$season)
  ads$location <- factor(ads$location, levels = valid_levels$location)
  ads$va <- factor(ads$va, levels = valid_levels$va)

  # -------------------------- live births ------------------------- #

  cli::cli_h1("checking live births")

  check_valid_vals(lb, "site", valid_sites, "sites", "live births data")
  check_valid_vals(lb, "catchment", valid_catchments,
    "catchments", "live births data")

# -------------------------- DHS ------------------------- #

  cli::cli_h1("checking DHS data")

  check_valid_vals(dhs, "site", valid_sites, "sites", "DHS data")
  check_valid_vals(dhs, "catchment", valid_catchments,
    "catchments", "DHS data")

  # -------------------------- dss ------------------------- #

  cli::cli_h1("checking DSS")
  dss <- dplyr::filter(dss, factor != "religion")

  check_valid_vals(dss, "site", valid_sites, "sites", "DSS data")
  check_valid_vals(dss, "catchment", valid_catchments,
    "catchments", "DSS data")

  check_valid_vals(dss, "age", valid_levels$age, "age", "DSS data")

  dss$age <- factor(dss$age, levels = valid_levels$age)

  check_valid_vals(dplyr::filter(dss, factor == "education"),
    "level", valid_levels$education, "education values", "DSS data")

  check_valid_vals(dplyr::filter(dss, factor == "location"),
    "level", valid_levels$location, "locations", "DSS data")

  # check_valid_vals(dplyr::filter(dss, factor == "religion"),
  #   "level", valid_levels$religion, "religion values", "DSS data")

  check_valid_vals(dplyr::filter(dss, factor == "sex"),
    "level", valid_levels$sex, "sex values", "DSS data")

  check_valid_vals(dplyr::filter(dss, factor == "season"),
    "level", valid_levels$season, "seasons", "DSS data")

  # TODO: make sure period_start_year and period_end_year are constant
  #   within a given site/catchment

  check_valid_vals(dplyr::filter(dss, factor == "va"),
    "va", valid_levels$va, "verbal autopsy categories", "DSS data")

  res <- list(
    ads = ads,
    dss = dss,
    lb = lb,
    dhs = dhs
  )
  class(res) <- c("list", "champs_processed")

  return(res)
}
