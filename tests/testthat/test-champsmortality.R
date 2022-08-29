# comp <- readRDS("tests/testthat/evaluation_results_v1_82022.rds")
# dat_folder <- "inst/testdata"
# path_wide <- "inst/testdata/inputs_wide.csv"

get_file <- function(x)
  file.path(system.file(package = "champsmortality"), x)

comp <- readRDS(get_file("evaluationdata/evaluation_results_v1_82022.rds"))

dat_folder <- get_file("testdata")
sites_use <- c("S6", "S5", "S7")
catch_use <- c("C1", "C4", "C3", "C5", "C6", "C7")

test_that("Package works", {

  d <- read_and_validate_data(dat_folder)
  dd <- process_data(d, start_year = 2017, end_year = 2020)

  # data objects from process_data()
  expect_true(
    all(unlist(dd$ads) == unlist(comp$process_data$ads),
    na.rm = TRUE)
  )

  expect_true(
    all(unlist(dd$dss) == unlist(comp$process_data$dss),
    na.rm = TRUE)
  )

  expect_true(
    all(unlist(dd$dhs) == unlist(comp$process_data$dhs),
    na.rm = TRUE)
  )

  expect_true(
    all(unlist(dd$lb) == unlist(comp$process_data$lb),
    na.rm = TRUE)
  )

  # valid_conditions()
  expect_true(
    all(unlist(valid_conditions(dd)) == unlist(comp$valid_conditions))
  )

  expect_true(
    all(unlist(valid_maternal_conditions(dd)) ==
      unlist(comp$valid_maternal_conditions))
  )

  # has functions
  expect_true(
    all(has_champs_group(d$ads, group = "Sepsis") ==
      comp$has_champs_group_sepsis)
  )

  expect_true(
    all(has_champs_group(d$ads, group = "Sepsis", cc = FALSE) ==
      comp$has_champs_group_sepsis_cct)
  )

  expect_true(
    all(has_champs_group(d$ads, group = "Malaria") ==
      comp$has_champs_group_malaria)
  )

  expect_true(
    all(has_maternal_champs_group(d$ads, group = "Sepsis") ==
      comp$has_maternal_champs_group_sepsis)
  )

  expect_true(
    all(has_maternal_champs_group(d$ads, group = "Malaria") ==
      comp$has_maternal_champs_group_malaria)
  )

  expect_true(
    all(has_icd10(d$ads, rgx = "^Q00|^Q01|^Q05") ==
      comp$has_icd10)
  )

  expect_true(
    all(has_icd10(d$ads, rgx = "^Q00|^Q01|^Q05", cc = FALSE) ==
      comp$has_icd10_cct)
  )

  expect_true(
    all(has_maternal_icd10(d$ads, rgx = "^A32") ==
      comp$has_maternal_icd10)
  )

  # mits_factor_tables
  mft <- mits_factor_tables(dd,
    sites = sites_use,
    catchments = catch_use
  )

  mft1 <- mits_factor_tables(dd,
    sites = sites_use[1],
    catchments = catch_use[1]
  )

  # cond_factor_tables
  cftb <- cond_factor_tables(dd,
    sites = sites_use,
    catchments = catch_use,
    condition = "Congenital birth defects")

  cftb1 <- cond_factor_tables(dd,
    sites = sites_use[1],
    catchments = catch_use[1],
    condition = "Congenital birth defects")

  cftm <- cond_factor_tables(dd,
    sites = sites_use,
    catchments = catch_use,
    condition = "Malnutrition")

  # dput(cftb)
  # dput(comp$cond_factor_tables_births)

  expect_equal(
    cftb, comp$cond_factor_tables_births, tolerance = 4
  )

  expect_equal(
    cftm, comp$cond_factor_tables_m, tolerance = 4
  )

  expect_equal(
    cftb1, comp$cond_factor_tables_births1, tolerance = 4
  )

  expect_equal(
    mft, comp$mits_factor_tables, tolerance = 4
  )

  expect_equal(
    mft1, comp$mits_factor_tables1
  )

  # This is an internal function and is alread tested inside other functions
  # expect_equal(
  #   combine_decision_tables(list(first = mft, second = cftb)),
  #   comp$combine_decion_tables,
  #   tolerance = 4
  # )

  # get_site_info
  expect_equal(
    get_site_info(dd), comp$get_site_info, tolerance = 4
  )

  # This is an internal function and is alread tested inside other functions
  # grfd <- get_rate_frac_data(
  #   dd,
  #   site = sites_use,
  #   catchments = catch_use,
  #   causal_chain = FALSE,
  #   condition = "Lower respiratory infections")

  graf <- get_rates_and_fractions(
    dd,
    sites = sites_use,
    catchments = catch_use,
    causal_chain = FALSE,
    pval_cutoff = 0.1, #Fixed
    pct_na_cutoff = 20, #Fixed
    condition = "Lower respiratory infections")

  # expect_equal(
  #   grfd, comp$get_rate_frac_data, tolerance = 4
  # )

  expect_equal(
    graf, comp$get_rates_and_fractions, tolerance = 4
  )

  # calculate interval
  expect_equal(
    get_interval(1 / 100, 1000, 95),
    comp$get_interval,
    tolerance = 4
  )

  inputs1 <- get_file("testdata/inputs.csv")
  bat1 <- batch_rates_and_fractions(dd, inputs1)

  expect_equal(bat1, comp$bat1)

  inputs2 <- I("site,catchment,age,condition,icd10_regex,causal_chain,maternal
S6,C1;C2,Neonate;Infant;Child,Perinatal asphyxia/hypoxia,,TRUE,FALSE
S6,C1,Stillbirth,Congenital birth defects,,FALSE,FALSE
")

  bat2 <- batch_rates_and_fractions(dd, inputs2)

  inputs3 <- list(
    list(
      site = "S6",
      catchment = c("C1", "C2"),
      age = c("Neonate", "Infant", "Child"),
      condition = "Perinatal asphyxia/hypoxia",
      icd10_regex = NULL,
      causal_chain = TRUE,
      maternal = FALSE
    ),
    list(
      site = "S6",
      catchment = "C1",
      age = "Stillbirth",
      condition = "Congenital birth defects",
      icd10_regex = NULL,
      causal_chain = FALSE,
      maternal = FALSE
    )
  )

  bat3 <- batch_rates_and_fractions(dd, inputs3)

  expect_equal(bat2, bat3, tolerance = 4)

  expect_true({ rates_and_fractions_table(graf); TRUE })

  expect_true({ champs_web_report(graf); TRUE })

  # make sure plotting functions aren't broken (not checking actual values)
  expect_true({ plot_rates_fracs(graf, type = "rate", plotly = FALSE); TRUE})
  expect_true({ plot_rates_fracs(graf, type = "frac", plotly = TRUE); TRUE})
  expect_true({ table_overview(graf); TRUE})
  expect_true({ table_adjust_decision(graf); TRUE})
  expect_true({ table_factor_sig_stats(graf, which = "mits"); TRUE})
  expect_true({ table_factor_sig_stats(graf, which = "cond"); TRUE})
})
