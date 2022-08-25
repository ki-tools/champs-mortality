
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- devtools::build_readme() -->

# champsmortality

<!-- badges: start -->

[![R-CMD-check](https://github.com/ki-tools/champs-mortality/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ki-tools/champs-mortality/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ki-tools/champs-mortality/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ki-tools/champs-mortality?branch=main)
<!-- badges: end -->

The goal of champsmortality is to provide functions for calculating
mortality fractions and rates at CHAMPS sites for various causes.

Below is a high-level walkthrough of some of the major pacakge
functionality and workflow. Read the additional articles for more depth:

-   [Getting
    Started](https://ki-tools.github.io/champs-mortality/articles/getting-started.html)
-   [Datasets](https://ki-tools.github.io/champs-mortality/articles/datasets.html)
-   [Computing Adjusted Rates and
    Fractions](https://ki-tools.github.io/champs-mortality/articles/analysis.html)
-   [Creating
    Reports](https://ki-tools.github.io/champs-mortality/articles/reports.html)
-   [Methodology](https://ki-tools.github.io/champs-mortality/articles/methodology.html)

## Installation

You can install the development version of champsmortality with the
following:

``` r
install.packages("remotes") # one time only
remotes::install_github("ki-tools/champs-mortality")
```

## Example

This package is most useful with real data obtainable from
[CHAMPS](https://champshealth.org/data/). For the purposes of
documentation and examples, the package ships with synthetic datasets.
As such, keep in mind that the results of the examples in this
documentation not meaningful.

``` r
library(champsmortality)
```

### Data setup

The first time you use this package, you need to place the appropriate
data files in a data directory that the package will pull from to
perform the calculations. A function `create_dataset_directory()` is
provided to help get this set up.

``` r
data_dir <- tempfile()
create_dataset_directory(data_dir)
#> ✔ The directory
#>   '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//RtmpJ41PFB/file80d045c5ad26' is ready
#>   for the appropriate data files to be placed in it. The following datasets should be placed
#>   in this directory:
#> 
#> 1. CHAMPS Analytics Dataset: This dataset is available as a downloadable file from LabKey and
#>    is continuously updated. It contains most of the CHAMPS variables that are needed for the
#>    analysis.
#> 2. Maternal Registry Forms table: This dataset is also available as a downloadable file from
#>    LabKey and contains information about maternal age and education.
#> 3. CHAMPS vocabulary: This dataset provides a lookup table for all CHAMPS codes, providing a
#>    corresponding 'name' and 'preferred name' for each. This file is accessible from the
#>    CHAMPS L2 dataset from dataverse.
#> 4. DSS: This dataset contains counts of cases from the demographic surveillance system (DSS)
#>    corresponding to each CHAMPS site and catchment area, only for DSS cases that are not in
#>    the CHAMPS data. These counts are broken down by age group, year, location of death,
#>    season of death, maternal education, sex of child, and verbal autopsy cause of death.
#> 5. Season definition: This dataset is a csv file containing rainy and dry season date ranges
#>    for each site, which will be used to classify the season in which each case occurs. A
#>    dataset with known season definitions,
#>    '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//RtmpJ41PFB/file80d045c5ad26/seasons.csv',
#>    has been provided. Please update that file if necessary.
#> 6. Catchment lookup: This dataset is a csv file containing mappings from catchment codes to
#>    catchment names, used to link the DSS data, which uses catchment names, to the CHAMPS
#>    analysis dataset, which uses catchment IDs. A dataset with known catchment lookups,
#>    '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//RtmpJ41PFB/file80d045c5ad26/catchment_lookup.csv',
#>    has been provided. Please update that file if necessary.
#> 7. Live births: This dataset is a csv file containing yearly live births by site and
#>    catchment from DSS. A dataset with known live birth statistics by site, catchment, and
#>    year,
#>    '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//RtmpJ41PFB/file80d045c5ad26/live_births.csv',
#>    has been provided. Please update that file if necessary.
#> 8. Live births: This dataset is a csv file containing yearly DHS all-cause mortality data by
#>    site and catchment from DSS. A dataset with known DHS statistics by site, catchment, year,
#>    and age,
#>    '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//RtmpJ41PFB/file80d045c5ad26/dhs.csv',
#>    has been provided. Please update that file if necessary.
#> 
#> → Once the files are in place, edit the file
#>   /var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//RtmpJ41PFB/file80d045c5ad26/config.yaml
#>   to provide the file names corresponding to each of these datasets.
#> 
#> The config.yaml template looks like this:
#>     ┌──────────────────────────────────────────┐
#>     │  champs_analytics_dataset: ''            │
#>     │  maternal_registry_dataset: ''           │
#>     │  champs_vocabulary_dataset: ''           │
#>     │  dss_dataset: ''                         │
#>     │  season_lookup: seasons.csv              │
#>     │  catchment_lookup: catchment_lookup.csv  │
#>     │  live_births_dataset: live_births.csv    │
#>     │  dhs_dataset: dhs.csv                    │
#>     └──────────────────────────────────────────┘
#> 
#> So for example, if the CHAMPS Analytics Dataset you placed in this directory is named
#> 'Analytics_Dataset_2021-09-01.xlsx', you would edit the corresponding line in config.yaml as
#> follows:
#>     ┌─────────────────────────────────────────────────────────────────┐
#>     │  champs_analytics_dataset: 'Analytics_Dataset_2021-09-01.xlsx'  │
#>     └─────────────────────────────────────────────────────────────────┘
```

This is something that only needs to be done once. Public data that
comes with the package will be placed here, and additional private
datasets will need to be added. More about the required datasets and
expected formats can be found [in this article](articles/datasets.html).

For the purpose of these examples, we will change `data_dir` to point to
a directory containing synthetic data. SKIP THIS STEP IF YOU ARE WORKING
WITH REAL DATA.

``` r
data_dir <- file.path(system.file(package = "champsmortality"), "testdata")
```

### Read the data

Once this is set up and the appropriate files are placed and mapped in
the `config.yaml` file, you can read in the data with the following:

``` r
d <- read_and_validate_data(data_dir)
#> ✔ analytics_2022-05-24.csv
#> ✔ vocabulary_2022-05-24.csv
#> ✔ registry_2022-05-24.csv
#> ✔ dss_2022-05-24.csv
#> ✔ seasons.csv
#> ✔ catchment_lookup.csv
#> ✔ live_births.csv
#> ✔ dhs.csv
```

This will read in the data files and ensure that all of the variables
required to perform the calculations are present. If they are not, you
will see an error message and will need to correct the error before
being able to use the package.

### Process the data

A function, `process_data()` takes the data that has been read and joins
it together to create a dataset ready analysis.

``` r
dd <- process_data(d, start_year = 2017, end_year = 2020)
#> 
#> ── processing analysis dataset ───────────────────────────────────────────────────────────────
#> ✔ Calculated year of death from 'calc_dod'
#> ✔ Used CHAMPS vocabulary to transform 'age_group' in analysis dataset
#> ✔ Created a new variable 'age' that rolls up all neonates into one category to be compatible
#> with DSS data
#> ✔ Used CHAMPS vocabulary to transform 'calc_sex' in analysis dataset
#> ✔ Classified Verbal Autopsy Inter-VA Cause 1 codes as 'Infection', 'Trauma', 'Other', or NA
#> ✔ Removed 2879 records that occur before 2017 or after 2020 or have a missing date of death
#> ✔ Resolved catchment names from catchment IDs using catchment lookup table
#> ✔ Checked that values for 'catchment' in the analysis dataset are correct
#> ✔ Modified some analysis set variables to match what is found in DSS data:
#> • sex: set 'Indeterminate' and 'Unknown' to NA
#> • location: 'other' to NA
#> ✔ Checked that values for 'age' in the analysis dataset are correct
#> ✔ Checked that values for 'calc_sex' in the analysis dataset are correct
#> ✔ Checked that values for 'calc_location' in the analysis dataset are correct
#> ✔ Checked that values for 'va_cod_iva' in the analysis dataset are correct
#> ✔ Checked that values for 'site_name' in the seasons lookup are correct
#> ✔ Calculated season of death
#> ✔ Checked that values for 'season' in the seasons lookup are correct
#> 
#> ── processing maternal registry ──────────────────────────────────────────────────────────────
#> ✔ Removed 244 records with missing 'champsid' from maternal registry dataset
#> ✔ The following 47 CHAMPS IDs have duplicate entries in the maternal registry dataset and
#> only the first record for each was kept: SEAA00272, SFAA00050, SHAA00046, SHAA00052,
#> SHAA00700, SHAA00882, SHAA00958, SHAA01135, SHAA01553, SHAA01898, SHAA01935, SHAA02086,
#> SHAA02123, SHCC00294, SIAA00004, SIAA00076, SIAA00104, SIAA00327, SIAA00428, SIAA00518,
#> SIAA00605, SIAA00851, SIAA00921, SIAA00935, SIAA01277, SIAA01735, SIAA01841, SIAA01891,
#> SOAA00370, SOAA00831, SOAA00833, SOAA01383, SOCC00033, SOCC00043, SOCC00091, SOCC00207,
#> SOCC00209, SOCC00225, SOCC00236, SOCC00249, SOCC00308, SSAA00025, SSAA00267, SSAA00476,
#> SSAA01016, SSAA02207, STAA01650
#> ✔ Used CHAMPS vocabulary to transform 'education' in maternal registry dataset
#> ✔ Checked that values for 'education' in the maternal registry dataset are correct
#> ✔ Joined analysis dataset and maternal registry
#> 
#> ── checking live births ──────────────────────────────────────────────────────────────────────
#> ✔ Checked that values for 'site' in the live births data are correct
#> ✔ Checked that values for 'catchment' in the live births data are correct
#> 
#> ── checking DHS data ─────────────────────────────────────────────────────────────────────────
#> ✔ Checked that values for 'site' in the DHS data are correct
#> ✔ Checked that values for 'catchment' in the DHS data are correct
#> 
#> ── checking DSS ──────────────────────────────────────────────────────────────────────────────
#> ✔ Checked that values for 'site' in the DSS data are correct
#> ✔ Checked that values for 'catchment' in the DSS data are correct
#> ✔ Checked that values for 'age' in the DSS data are correct
#> ✔ Checked that values for 'level' in the DSS data are correct
#> ✔ Checked that values for 'level' in the DSS data are correct
#> ✔ Checked that values for 'level' in the DSS data are correct
#> ✔ Checked that values for 'level' in the DSS data are correct
#> ✔ Checked that values for 'va' in the DSS data are correct
```

### Valid conditions

Computations with this data typically have the goal of finding adjusted
mortality fractions and rates for a given condition found in the causal
chain. As you will see, these can be specified by either using the
condition name or a [regular
expression](https://www.sitepoint.com/learn-regex/) indicating ICD10
codes that indicate the condition.

A convenience function that lists all available conditions in the data
is provided, `valid_conditions()`:

``` r
valid_conditions(dd)
#> # A tibble: 54 × 2
#>    condition                            `causal chain rank`
#>    <chr>                                              <int>
#>  1 Perinatal asphyxia/hypoxia                             1
#>  2 Neonatal preterm birth complications                   2
#>  3 Lower respiratory infections                           3
#>  4 Neonatal sepsis                                        4
#>  5 Sepsis                                                 5
#>  6 Congenital birth defects                               6
#>  7 Other neonatal disorders                               7
#>  8 Malnutrition                                           8
#>  9 Meningitis/Encephalitis                                9
#> 10 Malaria                                               10
#> # … with 44 more rows
```

This searches the CHAMPS data and finds all unique condition values
found anywhere in the causal chain. A ranking is also provided where a
higher ranking indicates that the condition is found more frequently in
the data than a condition with a lower ranking.

### Getting Rates and Fractions

The main function of this package computes factor-adjusted mortality
rates and fractions for a specified set of sites and catchments. More
details about the methodology can be found [in this
article](methodology.html).

``` r
graf <- get_rates_and_fractions(
  dd,
  sites = c("S6", "S5", "S7"),
  catchments = c("C1", "C4", "C3", "C5", "C6", "C7"),
  causal_chain = FALSE,
  pval_cutoff = 0.1,
  pct_na_cutoff = 20,
  condition = "Lower respiratory infections")
#> Processing site:  S6
#> ℹ The following catchments are not found in the data for site S6 and will be removed from the
#> calculations for this site: C4, C3, C5, C6, C7
#>   no adjustment variables
#> Processing site:  S5
#> ℹ The following catchments are not found in the data for site S5 and will be removed from the
#> calculations for this site: C1, C6, C7
#>   using adjustment variables: age, location
#> Processing site:  S7
#> ℹ The following catchments are not found in the data for site S7 and will be removed from the
#> calculations for this site: C1, C4, C3, C5
#>   using adjustment variable: location
#>   other significant factors not adjusted for: va
```

The output of this function contains many pieces of information for each
site including underlying statistics that went into the calculations,
but the most interesting outputs are the rates and fractions.

For site S6, for example, we can extract the computed rates and
fractions with the following:

``` r
graf$S6$rate
#> # A tibble: 2 × 8
#>   site  catchments var    label                                 allcauseMR   est  lower upper
#>   <chr> <chr>      <chr>  <chr>                                      <dbl> <dbl>  <dbl> <dbl>
#> 1 S6    C1         cTU5MR Crude total under-5 mortality rate          591.     0 0.0140  13.5
#> 2 S6    C1         aTU5MR Adjusted total under-5 mortality rate       591.     0 0.0140  13.5
graf$S6$frac
#> # A tibble: 2 × 9
#>   site  catchments var   label                              decode condi…¹   est   lower upper
#>   <chr> <chr>      <chr> <chr>                               <int>   <int> <dbl>   <dbl> <dbl>
#> 1 S6    C1         cCSMF Crude cause-specific mortality fr…     83       0     0 0.00236  2.28
#> 2 S6    C1         aCSMF Adjusted cause-specific mortality…     83       0     0 0.00236  2.28
#> # … with abbreviated variable name ¹​condition
```
