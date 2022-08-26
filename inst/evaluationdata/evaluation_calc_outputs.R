# devtools::install_github("ki-tools/champs-mortality")
# devtools::load_all()
# library(champsmortality)

version <- readLines("inst/testdata/version.txt")
dr <- list()

### Data ingestion

d <- read_and_validate_data("inst/testdata")

# process
dd <- process_data(d, start_year = 2017, end_year = 2020)
dr$process_data <- dd

### Checks: valid_ and has_ functions

# check valids
dr$valid_conditions <- valid_conditions(dd)
dr$valid_maternal_conditions <- valid_maternal_conditions(dd)

# check has
dr$has_champs_group_sepsis <- has_champs_group(d$ads, group = "Sepsis")
dr$has_champs_group_sepsis_cct <- has_champs_group(d$ads,
  group = "Sepsis", cc = FALSE)
dr$has_champs_group_malaria <- has_champs_group(d$ads, group =  "Malaria")

# has_maternal_champs_group
dr$has_maternal_champs_group_s <- has_maternal_champs_group(
  d$ads, group = "Sepsis")
dr$has_maternal_champs_group_m <- has_maternal_champs_group(
  d$ads, group =  "Malaria")

# has_icd10
dr$has_icd10 <- has_icd10(d$ads, rgx = "^Q00|^Q01|^Q05")
dr$has_icd10_cct <- has_icd10(d$ads,
  rgx = "^Q00|^Q01|^Q05", cc = FALSE)

# has_maternal_icd10
dr$has_maternal_icd10 <- has_maternal_icd10(d$ads, rgx = "^A32")

### Table calculations
sites_use <- c("S6", "S5", "S7")
catch_use <- c("C1", "C4", "C3", "C5", "C6", "C7")

# mits_factor_tables
mft <- mits_factor_tables(dd,
  sites = sites_use,
  catchments = catch_use
)

mft1 <- mits_factor_tables(dd,
  sites = sites_use[1],
  catchments = catch_use[1]
)

dr$mits_factor_tables <- mft
dr$mits_factor_tables1 <- mft1

# cond_factor_tables
cftb <- cond_factor_tables(
  dd,
  sites = sites_use,
  catchments = catch_use,
  condition = "Congenital birth defects")

cftb1 <- cond_factor_tables(
  dd,
  sites = sites_use[1],
  catchments = catch_use[1],
  condition = "Congenital birth defects")

dr$cond_factor_tables_births <- cftb
dr$cond_factor_tables_births1 <- cftb1

dr$cond_factor_tables_m <- cond_factor_tables(
  dd,
  sites = sites_use,
  catchments = catch_use,
  condition = "Malnutrition")

dr$combine_decion_tables <- combine_decision_tables(
  list(first = mft, second = cftb))

dr$get_rate_frac_data <- get_rate_frac_data(
  dd,
  site = sites_use,
  catchments = catch_use,
  causal_chain = FALSE,
  condition = "Lower respiratory infections")

# get_rates_and_fractions
graf <- get_rates_and_fractions(
  dd,
  sites = sites_use,
  catchments = catch_use,
  causal_chain = FALSE, 
  pval_cutoff = 0.1, #Fixed
  pct_na_cutoff = 20, #Fixed
  condition = "Lower respiratory infections")

dr$get_rates_and_fractions <- graf

dr$get_site_info <- get_site_info(dd)

### HTML tables
# functions on get_rates_and_fractions
dr$table_overview <- table_overview(graf)

dr$table_factor_sig_stats_mits <- table_factor_sig_stats(
  mft1,
  print_columns = c("MITS", "non-MITS+DSS-only"),
  percent_digits = 1
)

dr$table_factor_sig_stats_cond <- table_factor_sig_stats(
  cftb1,
  print_columns = c("MITS", "non-MITS+DSS-only"),
  percent_digits = 1
)

# gt tab_spanner_delim looks broken.
# using Feb, 2022 version of funciton in package.
dr$table_adjust_decision <- table_adjust_decision(graf)


### Added functions for repeat calculations
inputs1 <- "inst/testdata/inputs.csv"

dr$bat1 <- batch_rates_and_fractions(dd, inputs1)

# # may need to check function uses subsets
# dr$rates_and_fractions_wide <- rates_and_fractions_wide(
#   input_list[[2]],
#   dat = dd)

# dr$batch_rates_and_fractions <- batch_rates_and_fractions(
#   "inst/testdata/inputs_wide.csv",
#   "inst/testdata",
#   start_year = 2017,
#   end_year = 2020
#   )

### Calculations
# should get interval check for one input on each
# should this be hidden
dr$get_interval <- get_interval(1 / 100, 1000, 95)

saveRDS(dr, paste0("inst/evaluationdata/evaluation_results_v", version, ".rds"))

# This functions writes files
# make_outputs(graf)
