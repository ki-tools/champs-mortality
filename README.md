
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- devtools::build_readme() -->

# champsmortality

<!-- badges: start -->
<!-- badges: end -->

The goal of champsmortality is to provide functions for calculating
mortality fractions and rates at CHAMPS sites for various causes.

## Installation

You can install the development version of champsmortality with the
following:

``` r
install.packages("remotes") # one time only
remotes::install_github("ki-tools/champsmortality")
```

## Example

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
#> ✔ The directory '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//RtmpU2ilEG/file72510e597db'
#>   is ready for the appropriate data files to be placed in it. The following datasets should be
#>   placed in this directory:
#> 
#> 1. CHAMPS Analytics Dataset: This dataset is available as a downloadable file from LabKey and
#>    is continuously updated. It contains most of the CHAMPS variables that are needed for the
#>    analysis.
#> 2. Maternal Registry Forms table: This dataset is also available as a downloadable file from
#>    LabKey and contains information about maternal age and education.
#> 3. CHAMPS vocabulary: This dataset provides a lookup table for all CHAMPS codes, providing a
#>    correspond 'name' and 'preferred name' for each. This file is accessible from the CHAMPS L2
#>    dataset from dataverse.
#> 4. DSS: This dataset contains counts of cases from the demographic surveillance system (DSS)
#>    corresponding to each CHAMPS site and catchment area, only for DSS cases that are not in
#>    the CHAMPS data. These counts are broken down by age group, year, location of death, season
#>    of death, maternal education, sex of child, and verbal autopsy cause of death.
#> 5. Season definition: This dataset is a spreadsheet containing rainy and dry season date
#>    ranges for each site, which will be used to classify the season in which each case occurs.
#> 
#> → Once the files are in place, edit the file
#>   /var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//RtmpU2ilEG/file72510e597db/config.yaml to
#>   provide the file names corresponding to each of these datasets.
#> 
#> The config.yaml template looks like this:
#>     ┌─────────────────────────────────┐
#>     │  champs_analytics_dataset: ''   │
#>     │  maternal_registry_dataset: ''  │
#>     │  champs_vocabulary_dataset: ''  │
#>     │  dss_dataset: ''                │
#>     │  religion_lookup: ''            │
#>     │  season_lookup: ''              │
#>     │  catchment_lookup: ''           │
#>     └─────────────────────────────────┘
#> 
#> 
#> So for example, if the CHAMPS Analytics Dataset you placed in this directory is named
#> 'Analytics_Dataset_2021-09-01.xlsx', you would edit the corresponding line in config.yaml as
#> follows:
#> 
#>     ┌─────────────────────────────────────────────────────────────────┐
#>     │  champs_analytics_dataset: 'Analytics_Dataset_2021-09-01.xlsx'  │
#>     └─────────────────────────────────────────────────────────────────┘
```

This is something that only needs to be done once.

### Read the data

Once this is set up and the appropriate files are placed and mapped in
the `config.yaml` file, you can read in the data with the following:

``` r
d <- read_and_validate_data(data_dir)
#> ✔ _ignore/datasets/Analytics_Dataset_2021-09-01.xlsx
#> ✔ _ignore/datasets/CHAMPS_vocabulary.csv
#> ✔ _ignore/datasets/MaternalRegistry_2021-09-01.xlsx
#> ✔ _ignore/datasets/DSS.csv
#> ✔ _ignore/datasets/seasons.csv
#> ✔ _ignore/datasets/religion_lookup.csv
#> ✔ _ignore/datasets/catchment_lookup.csv
```

This will read in the data files and ensure that all of the variables
required to perform the calculations are present. If they are not, you
will see an error message and will need to correct the error before
being able to use the package.

### Process the data

A function, `process_data()` takes the data that has been read and joins
it together to create an analysis dataset and DSS dataset ready for
analysis.

``` r
dd <- process_data(d, start_year = 2017, end_year = 2020)
#> 
#> ── processing analysis dataset ────────────────────────────────────────────────────────────────
#> ✔ Removed 218 unnecessary variables from analysis dataset
#> ✔ Calculated year of death from 'calc_dod'
#> ✔ Used CHAMPS vocabulary to transform 'age_group' in analysis dataset
#> ✔ Created a new variable 'age' that rolls up all neonates into one category to be compatible
#> with DSS data
#> ✔ Used CHAMPS vocabulary to transform 'caretakers_religion' in analysis dataset
#> ✔ Used CHAMPS vocabulary to transform 'calc_sex' in analysis dataset
#> ✔ Created a new variable 'religion' that rolls up religions into categories compatible with
#> DSS data
#> ! There are 2 religion codes that do not match with any religion codes found in the
#> religion_lookup file: CH00029, CH00003 - it may be useful to look these up and add them to the
#> religion_lookup dataset
#> ✔ Classified Verbal Autopsy Inter-VA Cause 1 codes as 'Infection', 'Trauma', 'Other', or NA
#> ✔ Removed 1198 records that occur before 2017 or after 2020 or have a missing date of death
#> ✔ Resolved catchment names from catchment IDs using catchment lookup table
#> ✔ Checked that values for 'catchment' in the analysis dataset are correct
#> ✔ Modified some analysis set variables to match what is found in DSS data:
#> • religion: set 'Unknown' to NA
#> • sex: set 'Indeterminate' to NA
#> • location: 'other' to NA
#> ✔ Checked that values for 'age' in the analysis dataset are correct
#> ✔ Checked that values for 'religion' in the analysis dataset are correct
#> ✔ Checked that values for 'sex' in the analysis dataset are correct
#> ✔ Checked that values for 'location' in the analysis dataset are correct
#> ✔ Checked that values for 'va' in the analysis dataset are correct
#> ✔ Checked that values for 'site' in the seasons lookup are correct
#> ✔ Checked that values for 'season' in the seasons lookup are correct
#> ✔ Calculated season of death
#> 
#> ── processing maternal registry ───────────────────────────────────────────────────────────────
#> ✔ Removed 90 unnecessary variables from maternal registry dataset
#> ✔ Removed 5025 records with missing 'champsid' from maternal registry dataset
#> ✔ The following 18 CHAMPS IDs have duplicate entries in the maternal registry dataset and only
#> the first record for each was kept: BDAA00687, BDAA01749, BDAA02424, ETAA00082, ETAA00384,
#> ETAA00398, ETAA00513, ETAA00658, ETAA00664, ETAA00911, ETAA00919, MLAA01706, MZAA01702,
#> SLAA00745, SLAA00912, SLAA01271, SLCC00029, ZAAA00477
#> ✔ Used CHAMPS vocabulary to transform 'education' in maternal registry dataset
#> ✔ Checked that values for 'education' in the maternal registry dataset are correct
#> ✔ Joined analysis dataset and maternal registry
#> 
#> ── checking DSS ───────────────────────────────────────────────────────────────────────────────
#> ✔ Checked that values for 'site' in the DSS data are correct
#> ✔ Checked that values for 'catchment' in the DSS data are correct
#> ✔ Checked that values for 'age' in the DSS data are correct
#> ✔ Checked that values for 'education' in the DSS data are correct
#> ✔ Checked that values for 'location' in the DSS data are correct
#> ✔ Checked that values for 'religion' in the DSS data are correct
#> ✔ Checked that values for 'sex' in the DSS data are correct
#> ✔ Checked that values for 'va' in the DSS data are correct
```

### Computing statistics

The following code can be used to copute the total number of deaths in
Baliakandi Bangladesh, by age and MITS and non-MITS+DSS-only.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

bind_rows(
  dd$ads %>%
    filter(site == "Bangladesh", catchment == "Baliakandi") %>%
    count(mits_flag, age),
  dd$dss %>%
    filter(site == "Bangladesh", catchment == "Baliakandi",
      factor == "season") %>%
    group_by(age) %>%
    summarise(n = sum(n)) %>%
    mutate(mits_flag = 0)
) %>%
  group_by(mits_flag, age) %>%
  summarise(n = sum(n), .groups = "drop")
#> # A tibble: 7 × 3
#>   mits_flag age            n
#>       <dbl> <fct>      <dbl>
#> 1         0 Stillbirth   487
#> 2         0 Neonate      426
#> 3         0 Infant        90
#> 4         0 Child         73
#> 5         1 Stillbirth    31
#> 6         1 Neonate       51
#> 7         1 Child          1
```

The following computes the number of MITS deaths with and without neural
tube defects and congenital birth defects by age. This uses functions
`has_icd10_cc()` to check if neural tube defects are in the causal chain
using a regular expression indicating ICD10, and `has_champs_group_cc()`
to check if congenital birth defects are in the causal chain using a
CHAMPS group.

``` r
cbd_ntd <- dd$ads %>%
  mutate(
    ntd_cc = has_icd10_cc(., "^Q00|^Q01|^Q05"),
    cbd_cc = has_champs_group_cc(., "Congenital birth defects")
  )

cbd_ntd %>%
  filter(site == "Bangladesh", catchment == "Baliakandi") %>%
  count(cbd_cc, age) %>%
  filter(!is.na(cbd_cc)) %>%
  arrange(cbd_cc, age)
#> # A tibble: 5 × 3
#>   cbd_cc age            n
#>   <lgl>  <fct>      <int>
#> 1 FALSE  Stillbirth    30
#> 2 FALSE  Neonate       48
#> 3 FALSE  Child          1
#> 4 TRUE   Stillbirth     1
#> 5 TRUE   Neonate        3
```

This code is part of the calculations of mortality rates and fractions
and is being worked into functions that perform those operations.

More to come…
