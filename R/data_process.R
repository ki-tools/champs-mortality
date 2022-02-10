process_data <- function(dd, start_year, end_year) {
  voc <- dd$voc
  ads_raw <- dd$ads
  mreg <- dd$mreg
  dss <- dd$dss
  rlgn <- dd$rlgn
  seas <- dd$seas

  valid_sites <- unique(ads_raw$site_name)
  # valid_catchments <- unique(ads_raw$catchment_id)
  # valid_catchments
  # [1] "001" "002" "003" "004" "006" "005" "007"
  valid_locations <- unique(ads_raw$calc_location)

  valid_levels <- list(
    # catchment = c(),
    age = c("Stillbirth", "Neonate", "Infant", "Child"),
    education = c("None", "Primary", "Secondary", "Tertiary"),
    religion = c("Christian", "Hindu", "Muslim", "Other"),
    sex = c("Male", "Female"),
    season = c("Dry", "Rainy"),
    location = c("facility", "community"),
    va = c("Infection", "Trauma")
  )
  valid_factors <- names(valid_levels)

  # ------------------- analysis dataset ------------------- #
  cli::cli_h1("processing analysis dataset")

  ads_vars_keep <- snakecase::to_snake_case(ads_names)

  missing_ads_vars <- setdiff(ads_vars_keep, names(ads_raw))
  n_missing <- length(missing_ads_vars)
  assertthat::assert_that(n_missing == 0,
    msg = cli::format_error("
      {n_missing} necessary variable{?s} not found in the analysis dataset: \\
      {commas(missing_ads_vars)}"))

  nrm <- length(setdiff(names(ads_raw), ads_vars_keep))
  if (nrm > 0) {
    ads_raw <- select(ads_raw, one_of(ads_vars_keep))
    cli::cli_alert_success("Removed {nrm} unnecessary variables from analysis \\
      dataset", wrap = TRUE)
  }

  ads_raw$year <- lubridate::year(ads_raw$calc_dod)
  cli::cli_alert_success("Calculated year of death from 'calc_dod'",
    wrap = TRUE)

  dups <- ads_raw %>% count(champsid) %>% filter(n > 1)
  n_dups <- nrow(dups)
  if (n_dups > 0) {
    mreg <- mreg %>%
      group_by(champsid) %>%
      filter(row_number() == 1) %>%
      ungroup()
    cli::cli_alert_success("The following {n_dups} CHAMPS ID{?s} have \\
      duplicate entries in the maternal registry dataset and only the first \\
      record for each was kept: {commas(dups$champsid)}", wrap = TRUE)
  }

  ads_raw <- voc_lookup(ads_raw, voc, "age_group", "analysis dataset")
  nna <- length(which(is.na(ads_raw$age_group)))
  if (nna > 0) {
    cli::cli_alert_warning("NOTE: Found {nna} missing values for \\
      'age_group'... removing these records", wrap = TRUE)
    ads_raw <- filter(ads_raw, !is.na(age_group))
  }

  tmp <- ads_raw$age_group
  tmp[ads_raw$age_group_code %in% c("CH01404", "CH01405", "CH01406")] <-
    "Neonate"
  ads_raw$age <- factor(tmp, levels = valid_levels$age)
  cli::cli_alert_success("Created a new variable 'age' that rolls up \\
    all neonates into one category to be compatible with DSS data",
    wrap = TRUE)
  # TODO: make sure there are not any extra categories for age

  ads_raw <- voc_lookup(ads_raw, voc, "caretakers_religion", "analysis dataset")

  ads_raw <- voc_lookup(ads_raw, voc, "calc_sex", "analysis dataset")

  ads_raw <- ads_raw %>%
    mutate(caretakers_religion_code =
      substr(caretakers_religion_code, 1, 7)) %>%
    left_join(
      select(rlgn, one_of(c("champs_local_code", "religion"))),
      by = c(caretakers_religion_code = "champs_local_code"))
  cli::cli_alert_success("Created a new variable 'religion' that \\
    rolls up religions into categories compatible with DSS data",
    wrap = TRUE)

  not_handled <- setdiff(unique(
    ads_raw$caretakers_religion_code[is.na(ads_raw$religion)]), NA)
  nnh <- length(not_handled)
  if (nnh > 0) {
    cli::cli_alert_warning("There are {nnh} religion code{?s} that \\
      do not match with any religion codes found in the \\
      religion_lookup file: {commas(not_handled)} — it may be useful \\
      to look these up and add them to the religion_lookup dataset",
      wrap = TRUE)
  }

  tmp <- suppressWarnings(
    as.numeric(substr(ads_raw$va_cause_1_iva, 1, 5))
  )
  ads_raw <- ads_raw %>%
    mutate(
      va_cod_iva = case_when(
        tmp >= 12 & tmp < 13     ~ "Trauma",
        tmp >= 1 & tmp < 2       ~ "Infection",
        tmp >= 10.3 & tmp < 10.6 ~ "Infection",
        TRUE                     ~ as.character(NA)
      )
    )
  cli::cli_alert_success("Classified Verbal Autopsy Inter-VA Cause 1 codes \\
    as 'Infection', 'Trauma', or NA", wrap = TRUE)

  nr1 <- nrow(ads_raw)
  ads_raw <- filter(ads_raw, year <= end_year, year >= start_year)
  nd <- nr1 - nrow(ads_raw)
  if (nd > 0) {
    cli::cli_alert_success("Removed {nd} record{?s} that occur before \\
      {start_year} or after {end_year} or have a missing date of death",
      wrap = TRUE)
  }

  nr1 <- nrow(ads_raw)
  ads_raw <- filter(ads_raw, mits_flag == 1)
  nd <- nr1 - nrow(ads_raw)
  if (nd > 0) {
    cli::cli_alert_success("Removed {nd} record{?s} that do not have \\
      MITS flag = 1", wrap = TRUE)
  }

  nr1 <- nrow(ads_raw)
  ads_raw <- filter(ads_raw, m_00060 == 1)
  nd <- nr1 - nrow(ads_raw)
  if (nd > 0) {
    cli::cli_alert_success("Removed {nd} record{?s} that were not \\
      DeCoDed", wrap = TRUE)
  }

  ads_raw <- ads_raw %>%
    mutate(
      religion = recode(religion,
        Unknown = as.character(NA)
      ),
      calc_sex = recode(calc_sex,
        Indeterminate = as.character(NA)
      ),
      calc_location = recode(calc_location,
        other = as.character(NA)
      )
    )

  cli::cli_alert_success("Modified some analysis set variables to match \\
    what is found in DSS data:")
  ul <- cli::cli_ul()
  cli::cli_li("religion: set 'Unknown' to NA")
  cli::cli_li("sex: set 'Indeterminate' to NA")
  cli::cli_li("location: 'other' to NA")
  cli::cli_end(ul)

  check_valid_vals(ads_raw, "age", valid_levels$age,
    "age values", "analysis dataset")

  check_valid_vals(ads_raw, "religion", valid_levels$religion,
    "religion values", "analysis dataset")

  check_valid_vals(ads_raw, "sex", valid_levels$sex,
    "sex values", "analysis dataset")

  check_valid_vals(ads_raw, "location", valid_levels$location,
    "location values", "analysis dataset")

  check_valid_vals(ads_raw, "va", valid_levels$va,
    "va values", "analysis dataset")

  # check_valid_vals(ads_raw, "season", valid_levels$season,
  #   "season values", "analysis dataset")

  ads_raw <- rename(ads_raw,
      location = "calc_location",
      sex = "calc_sex",
      va = "va_cod_iva",
      site = "site_name"
    )

  cli::cli_alert_success("Calculated season of death")

  # ------------------- maternal registry ------------------ #
  cli::cli_h1("processing maternal registry")

  reg_vars_keep <- snakecase::to_snake_case(mreg_names)

  missing_reg_vars <- setdiff(reg_vars_keep, names(mreg))
  n_missing <- length(missing_reg_vars)
  assertthat::assert_that(n_missing == 0,
    msg = cli::format_error("
      {n_missing} necessary variable{?s} not found in the maternal registry \\
        dataset: {commas(missing_reg_vars)}", wrap = TRUE))

  nrm <- length(setdiff(names(mreg), reg_vars_keep))
  if (nrm > 0) {
    mreg <- mreg %>%
      select(all_of(reg_vars_keep)) %>%
      rename(
        champsid = mort_id,
        est_mat_age = mat_0010,
        known_mat_age = mat_0011,
        education = mat_0013
      )
    cli::cli_alert_success("Removed {nrm} unnecessary variables from maternal \\
      registry dataset", wrap = TRUE)
  }

  nna <- length(which(is.na(mreg$champsid)))
  if (nna > 0) {
    mreg <- filter(mreg, !is.na(champsid))
    cli::cli_alert_success("Removed {nna} record{?s} with missing 'champsid' \\
      from maternal registry dataset", wrap = TRUE)
  }

  dups <- mreg %>% count(champsid) %>% filter(n > 1)
  n_dups <- nrow(dups)
  if (n_dups > 0) {
    mreg <- mreg %>%
      group_by(champsid) %>%
      filter(row_number() == 1) %>%
      ungroup()
    cli::cli_alert_success("The following {n_dups} CHAMPS ID{?s} have \\
      duplicate entries in the maternal registry dataset and only the first \\
      record for each was kept: {commas(dups$champsid)}", wrap = TRUE)
  }

  mreg <- voc_lookup(mreg, voc, "education", "maternal registry dataset")
  # TODO: make sure these match valid_education

  mreg <- mreg %>%
    mutate(education = recode(education,
      "No Education" = "None"
    )
  )
  # cli::cli_li("education: 'No Education' to 'None'")

  check_valid_vals(mreg, "education", valid_levels$education,
    "education values", "maternal registry dataset")

  ads <- left_join(ads_raw, mreg, by = "champsid")
  cli::cli_alert_success("Joined analysis dataset and maternal registry")

  # -------------------------- dss ------------------------- #

  cli::cli_h1("checking DSS")


  check_valid_vals(dss, "site", valid_sites, "sites", "DSS data")

  # check_valid_vals(dss, "catchment", valid_catchments, "catchments")

  check_valid_vals(dss, "age", valid_levels$age, "age", "DSS data")

  check_valid_vals(filter(dss, factor == "education"),
    "education", valid_levels$education, "education values", "DSS data")

  check_valid_vals(filter(dss, factor == "location"),
    "location", valid_levels$locations, "locations", "DSS data")

  check_valid_vals(filter(dss, factor == "religion"),
    "religion", valid_levels$religions, "religion values", "DSS data")

  check_valid_vals(filter(dss, factor == "sex"),
    "sex", valid_levels$sex, "sex values", "DSS data")

  # check_valid_vals(filter(dss, factor == "season"),
  #   "level", valid_levels$season, "seasons", "DSS data")

  check_valid_vals(filter(dss, factor == "va"),
    "va", valid_levels$va, "verbal autopsy categories", "DSS data")

  return(list(
    ads = ads,
    dss = dss
  ))

}