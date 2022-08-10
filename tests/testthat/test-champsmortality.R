comp <- readRDS("tests/testthat/evaluation_results_v1_82022.rds")
dat_folder <- "inst/testdata"
path_wide <- "inst/testdata/inputs_wide.csv"

comp <- readRDS("evaluation_results_v1_82022.rds")

dat_folder <- "../../inst/testdata"
path_wide <- "../../inst/testdata/inputs_wide.csv"
sites_use <- c("S6", "S5", "S7")
catch_use <- c("C1", "C4", "C3", "C5", "C6", "C7")

input_list <- readr::read_csv(path_wide) |>
    purrr::transpose()

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

  expect_true(
    all(unlist(cftb) == unlist(comp$cond_factor_tables_births))
  )

  expect_true(
    all(unlist(cftm) == unlist(comp$cond_factor_tables_m))
  )

  expect_true(
    all(unlist(cftb1) == unlist(comp$cond_factor_tables_births1))
  )

  expect_true(
    all(unlist(mft) == unlist(comp$mits_factor_tables), na.rm = TRUE)
  )

  expect_true(
    all(unlist(mft1) == unlist(comp$mits_factor_tables1), na.rm = TRUE)
  )

  expect_true(
    all(unlist(combine_decision_tables(list(first = mft, second = cftb))) ==
      unlist(comp$combine_decion_tables), na.rm = TRUE)
  )

  # get_site_info
  expect_true(
    all(unlist(get_site_info(dd) == unlist(comp$get_site_info)))
  )

  # Get rates and fractions
  grfd <- get_rate_frac_data(dd,
            site = sites_use,
            catchments = catch_use,
            causal_chain = FALSE,
            condition = "Lower respiratory infections")

  graf <- get_rates_and_fractions(dd,
            sites = sites_use,
            catchments = catch_use,
            causal_chain = FALSE, 
            pval_cutoff = 0.1, #Fixed
            pct_na_cutoff = 20, #Fixed
            condition = "Lower respiratory infections")

  expect_true(
    all(unlist(grfd) == unlist(comp$get_rate_frac_data))
  )

  expect_true(
    all(unlist(graf) == unlist(comp$get_rate_fractions))
  )

  # html tables on mft1 and cftb1
  expect_true(
    all(unlist(table_overview(graf)) ==
      unlist(comp$table_overview), na.rm = TRUE)
  )

  tfssm <- table_factor_sig_stats(
    mft1,
    print_columns = c("MITS", "non-MITS+DSS-only"),
    percent_digits = 1)

  tfssc <- table_factor_sig_stats(
    cftb1,
    print_columns = c("MITS", "non-MITS+DSS-only"),
    percent_digits = 1)

  tad <- table_adjust_decision(graf)

  expect_true(
    all(
      unlist(tfssm$`_spanners`) ==
      unlist(comp$table_factor_sig_stats_mits$`_spanners`), 
      na.rm = TRUE
    )
  )

    expect_true(
    all(
      unlist(tfssm$`_boxhead`) ==
      unlist(comp$table_factor_sig_stats_mits$`_boxhead`), 
      na.rm = TRUE
    )
  )

  expect_true(
    all(
      unlist(tfssc$`_spanners`) ==
      unlist(comp$table_factor_sig_stats_cond$`_spanners`), 
      na.rm = TRUE
    )
  )

    expect_true(
    all(
      unlist(tfssc$`_boxhead`) ==
      unlist(comp$table_factor_sig_stats_cond$`_boxhead`), 
      na.rm = TRUE
    )
  )

  expect_true(
    all(
      unlist(tad$`_spanners`) ==
      unlist(comp$table_factor_adjust_decision$`_spanners`), 
      na.rm = TRUE
    )
  )

  expect_true(
    all(
      unlist(tad$`_boxhead`) ==
      unlist(comp$table_adjust_decision$`_boxhead`), 
      na.rm = TRUE
    )
  )

  #calculate interval
  expect_true(
    all(get_interval(1 / 100, 1000, 95) == comp$get_interval)
  )

  #repeat calculation functions
  expect_true(
    all(
      unlist(rates_and_fractions_wide(input_list[[2]], dat = dd)) ==
      unlist(comp$rates_and_fractions_wide)
    )
  )

  expect_true(
    all(
      unlist(batch_rates_and_fractions(
        path_wide,
        dat_folder,
        start_year = 2017,
        end_year = 2020)) ==
      unlist(comp$batch_rates_and_fractions)
    )
  )

})
