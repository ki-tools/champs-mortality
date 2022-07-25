ds_names <- c("champs_analytics_dataset", "maternal_registry_dataset",
  "champs_vocabulary_dataset", "dss_dataset", "season_lookup",
  # "religion_lookup",
  "catchment_lookup", "live_births_dataset")

ads_names <- c(
  "Champsid",
  "Site Name",
  "Catchment Id",
  "Calc Location",
  "Calc Sex",
  "Age Group",
  "IC Champs Group Desc",
  "UC Champs Group Desc",
  "Morbid Cond 01 Champs Group Desc",
  "Morbid Cond 02 Champs Group Desc",
  "Morbid Cond 03 Champs Group Desc",
  "Morbid Cond 04 Champs Group Desc",
  "Morbid Cond 05 Champs Group Desc",
  "Morbid Cond 06 Champs Group Desc",
  "Morbid Cond 07 Champs Group Desc",
  "Morbid Cond 08 Champs Group Desc",
  "Underlying Cause Calc",
  "Immediate COD",
  "Morbid Condition 01",
  "Morbid Condition 02",
  "Morbid Condition 03",
  "Morbid Condition 04",
  "Morbid Condition 05",
  "Morbid Condition 06",
  "Morbid Condition 07",
  "Morbid Condition 08",
  "Main Maternal Disease Condition",
  "Main Maternal Champs Group Desc",
  "MITS Flag",
  "M00060",
  "Calc Dod",
  "Caretakers Religion",
  "VA Cause1 Iva"
)

mreg_names <- c(
  "Mort Id",
  "Mat 0010",
  "Mat 0011",
  "Mat 0013"
)

voc_names <- c(
  "champs_local_code",
  "c_name",
  "c_pref_name"
)

dss_names <- c(
  "site",
  "catchment",
  "age",
  "factor",
  "level",
  "n",
  "period_start_year",
  "period_end_year"
)

rlgn_names <- c(
  "champs_local_code",
  "religion"
)

seas_names <- c(
  "site",
  "season",
  "start",
  "end"
)

catlkp_names <- c(
  "site_name",
  "catchment",
  "catchment_id"
)

live_birth_names <- c(
  "site",
  "catchment",
  "year",
  "live_births"
)

dhs_names <- c("site", "catchment", "year", "rate")

valid_levels <- list(
  age = c("Stillbirth", "Neonate", "Infant", "Child"),
  sex = c("Female", "Male"),
  # religion = c("Christian", "Hindu", "Muslim", "Other"),
  education = c("None", "Primary", "Secondary", "Tertiary"),
  season = c("Dry", "Rainy"),
  location = c("Community", "Facility"),
  va = c("Infection", "Trauma", "Other")
)

valid_factors <- names(valid_levels)
