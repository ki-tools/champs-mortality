
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
remotes::install_github("ki-tools/champs-mortality")
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
#> ✔ The directory
#>   '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//Rtmpbwux7d/file24bf45a789f9' is ready
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
#>    '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//Rtmpbwux7d/file24bf45a789f9/seasons.csv',
#>    has been provided. Please update that file if necessary.
#> 6. Religion lookup: This dataset is a csv file containing mappings from religion CHAMPS codes
#>    to religion categories. A dataset with known religion lookups,
#>    '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//Rtmpbwux7d/file24bf45a789f9/religion_lookup.csv',
#>    has been provided. Please update that file if necessary.
#> 7. Catchment lookup: This dataset is a csv file containing mappings from catchment codes to
#>    catchment names, used to link the DSS data, which uses catchment names, to the CHAMPS
#>    analysis dataset, which uses catchment IDs. A dataset with known catchment lookups,
#>    '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//Rtmpbwux7d/file24bf45a789f9/catchment_lookup.csv',
#>    has been provided. Please update that file if necessary.
#> 8. Live births: This dataset is a csv file containing yearly live births by site and
#>    catchment from DSS. A dataset with known live birth statistics by site, catchment, and
#>    year,
#>    '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//Rtmpbwux7d/file24bf45a789f9/live_births.csv',
#>    has been provided. Please update that file if necessary.
#> 
#> → Once the files are in place, edit the file
#>   /var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//Rtmpbwux7d/file24bf45a789f9/config.yaml
#>   to provide the file names corresponding to each of these datasets.
#> 
#> The config.yaml template looks like this:
#>     ┌──────────────────────────────────────────┐
#>     │  champs_analytics_dataset: ''            │
#>     │  maternal_registry_dataset: ''           │
#>     │  champs_vocabulary_dataset: ''           │
#>     │  dss_dataset: ''                         │
#>     │  season_lookup: seasons.csv              │
#>     │  religion_lookup: religion_lookup.csv    │
#>     │  catchment_lookup: catchment_lookup.csv  │
#>     │  live_births_dataset: live_births.csv    │
#>     └──────────────────────────────────────────┘
#> 
#> 
#> So for example, if the CHAMPS Analytics Dataset you placed in this directory is named
#> 'Analytics_Dataset_2021-09-01.xlsx', you would edit the corresponding line in config.yaml as
#> follows:
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
#> ✔ _ignore/datasets_unique/Analytics_Dataset_2021-09-01.csv
#> ✔ _ignore/datasets_unique/CHAMPS_vocabulary.csv
#> ✔ _ignore/datasets_unique/MaternalRegistry_2021-09-01.csv
#> ✔ _ignore/datasets_unique/dss.csv
#> ✔ _ignore/datasets_unique/seasons.csv
#> ✔ _ignore/datasets_unique/religion_lookup.csv
#> ✔ _ignore/datasets_unique/catchment_lookup.csv
#> ✔ _ignore/datasets_unique/live_births.csv
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
#> ── processing analysis dataset ───────────────────────────────────────────────────────────────
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
#> religion_lookup file: CH00029, CH00003 - it may be useful to look these up and add them to
#> the religion_lookup dataset
#> ✔ Classified Verbal Autopsy Inter-VA Cause 1 codes as 'Infection', 'Trauma', 'Other', or NA
#> ✔ Removed 1204 records that occur before 2017 or after 2020 or have a missing date of death
#> ✔ Resolved catchment names from catchment IDs using catchment lookup table
#> ✔ Checked that values for 'catchment' in the analysis dataset are correct
#> ✔ Modified some analysis set variables to match what is found in DSS data:
#> • religion: set 'Unknown' to NA
#> • sex: set 'Indeterminate' and 'Unknown' to NA
#> • location: 'other' to NA
#> ✔ Checked that values for 'age' in the analysis dataset are correct
#> ✔ Checked that values for 'religion' in the analysis dataset are correct
#> ✔ Checked that values for 'calc_sex' in the analysis dataset are correct
#> ✔ Checked that values for 'calc_location' in the analysis dataset are correct
#> ✔ Checked that values for 'va_cod_iva' in the analysis dataset are correct
#> ✔ Checked that values for 'site_name' in the seasons lookup are correct
#> ✔ Calculated season of death
#> ✔ Checked that values for 'season' in the seasons lookup are correct
#> 
#> ── processing maternal registry ──────────────────────────────────────────────────────────────
#> ✔ Removed 90 unnecessary variables from maternal registry dataset
#> ✔ Removed 5025 records with missing 'champsid' from maternal registry dataset
#> ✔ Used CHAMPS vocabulary to transform 'education' in maternal registry dataset
#> ✔ Checked that values for 'education' in the maternal registry dataset are correct
#> ✔ Joined analysis dataset and maternal registry
#> 
#> ── checking live births ──────────────────────────────────────────────────────────────────────
#> ✔ Checked that values for 'site' in the live births data are correct
#> ✔ Checked that values for 'catchment' in the live births data are correct
#> 
#> ── checking DSS ──────────────────────────────────────────────────────────────────────────────
#> ✔ Checked that values for 'site' in the DSS data are correct
#> ✔ Checked that values for 'catchment' in the DSS data are correct
#> ✔ Checked that values for 'age' in the DSS data are correct
#> ✔ Checked that values for 'education' in the DSS data are correct
#> ✔ Checked that values for 'location' in the DSS data are correct
#> ✔ Checked that values for 'religion' in the DSS data are correct
#> ✔ Checked that values for 'sex' in the DSS data are correct
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
#>  8 Meningitis/Encephalitis                                8
#>  9 Malnutrition                                           9
#> 10 Malaria                                               10
#> # … with 44 more rows
```

This searches the CHAMPS data and finds all unique condition values
found anywhere in the causal chain. A ranking is also provided where a
higher ranking indicates that the condition is found more frequently in
the data than a condition with a lower ranking.

### Computing statistics

We will use the `dplyr` package for some of the examples below so let’s
load it:

``` r
library(dplyr)
```

#### Tables of MITS / non-MITS+DSS-only counts by site and factor

A function `mits_selection_factor_tables()` exists in this package that
computes tables of MITS / non-MITS+DSS-only counts by site and factor
and associated statistics.

This function aims to replicate table 5a in “Supplemental Results (OLD)”
as part of determining what factors to adjust for.

To call the function for sites “Bangladesh”, “Ethiopia”, “Kenya” and
their associated catchments:

``` r
fac_tbl <- mits_selection_factor_tables(dd,
  sites = c("Bangladesh", "Ethiopia", "Kenya"),
  catchments = c("Baliakandi", "Haramaya", "Harar", "Kersa",
    "Manyatta", "Siaya")
)
```

The output looks like this:

``` r
print(fac_tbl, n = 21)
#> # A tibble: 21 × 9
#>    site       catchments             factor    table        pval     n n_tot  n_na   pct_na
#>    <chr>      <chr>                  <chr>     <list>      <dbl> <dbl> <dbl> <dbl>    <dbl>
#>  1 Bangladesh Baliakandi             age       <tibble> 2.50e- 5   163   163     0   0     
#>  2 Bangladesh Baliakandi             education <tibble> 1.52e- 2   131   163    32  19.6   
#>  3 Bangladesh Baliakandi             location  <tibble> 4.36e-15   160   163     3   1.84  
#>  4 Bangladesh Baliakandi             religion  <tibble> 1.57e- 1   140   163    23  14.1   
#>  5 Bangladesh Baliakandi             season    <tibble> 5.64e- 1   161   163     2   1.23  
#>  6 Bangladesh Baliakandi             sex       <tibble> 3.01e- 1   152   163    11   6.75  
#>  7 Bangladesh Baliakandi             va        <tibble> 1.27e- 5   105   163    58  35.6   
#>  8 Ethiopia   Haramaya, Harar, Kersa age       <tibble> 1.26e-57  1156  1156     0   0     
#>  9 Ethiopia   Haramaya, Harar, Kersa education <tibble> 2.54e-50  1075  1156    81   7.01  
#> 10 Ethiopia   Haramaya, Harar, Kersa location  <tibble> 8.11e-68   991  1156   165  14.3   
#> 11 Ethiopia   Haramaya, Harar, Kersa religion  <tibble> 1.60e-19  1156  1156     0   0     
#> 12 Ethiopia   Haramaya, Harar, Kersa season    <tibble> 2.65e- 1  1153  1156     3   0.260 
#> 13 Ethiopia   Haramaya, Harar, Kersa sex       <tibble> 4.48e- 1   622  1156   534  46.2   
#> 14 Ethiopia   Haramaya, Harar, Kersa va        <tibble> 1   e+ 0     0  1156  1156 100     
#> 15 Kenya      Manyatta, Siaya        age       <tibble> 5.00e- 4  1394  1394     0   0     
#> 16 Kenya      Manyatta, Siaya        education <tibble> 1.02e-23   737  1394   657  47.1   
#> 17 Kenya      Manyatta, Siaya        location  <tibble> 3.59e-17  1018  1394   376  27.0   
#> 18 Kenya      Manyatta, Siaya        religion  <tibble> 7.50e-25   782  1394   612  43.9   
#> 19 Kenya      Manyatta, Siaya        season    <tibble> 2.16e- 2  1394  1394     0   0     
#> 20 Kenya      Manyatta, Siaya        sex       <tibble> 2.33e- 1  1393  1394     1   0.0717
#> 21 Kenya      Manyatta, Siaya        va        <tibble> 7.03e-10   757  1394   637  45.7
```

Each row is for one site/factor combination and contains the table of
counts (as a nested data frame), the p-value of the chi-square test, and
percent missing values.

To view the table of counts for the first record (Bangladesh/age):

``` r
fac_tbl$table[[1]]
#> # A tibble: 4 × 5
#>   level      `DSS-only` `non-MITS`  MITS `non-MITS+DSS-only`
#>   <fct>           <dbl>      <dbl> <dbl>               <dbl>
#> 1 Stillbirth         87        401    31                 488
#> 2 Neonate            55        372    51                 427
#> 3 Infant             10         82     0                  92
#> 4 Child              11         62     1                  73
```

To see the table of counts for a specific record
(e.g. Bangladesh/location):

``` r
filter(fac_tbl, site == "Bangladesh", factor == "location")$table
#> [[1]]
#> # A tibble: 2 × 5
#>   level     `DSS-only` `non-MITS`  MITS `non-MITS+DSS-only`
#>   <chr>          <dbl>      <dbl> <dbl>               <dbl>
#> 1 community         58        418     4                 476
#> 2 facility         102        492    79                 594
```

To look at which sites/factors pass the adjustment criterion, we can
filter the table based on these:

``` r
filter(fac_tbl, pval < 0.1, pct_na < 20)
#> # A tibble: 9 × 9
#>   site       catchments             factor    table        pval     n n_tot  n_na pct_na
#>   <chr>      <chr>                  <chr>     <list>      <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1 Bangladesh Baliakandi             age       <tibble> 2.50e- 5   163   163     0   0   
#> 2 Bangladesh Baliakandi             education <tibble> 1.52e- 2   131   163    32  19.6 
#> 3 Bangladesh Baliakandi             location  <tibble> 4.36e-15   160   163     3   1.84
#> 4 Ethiopia   Haramaya, Harar, Kersa age       <tibble> 1.26e-57  1156  1156     0   0   
#> 5 Ethiopia   Haramaya, Harar, Kersa education <tibble> 2.54e-50  1075  1156    81   7.01
#> 6 Ethiopia   Haramaya, Harar, Kersa location  <tibble> 8.11e-68   991  1156   165  14.3 
#> 7 Ethiopia   Haramaya, Harar, Kersa religion  <tibble> 1.60e-19  1156  1156     0   0   
#> 8 Kenya      Manyatta, Siaya        age       <tibble> 5.00e- 4  1394  1394     0   0   
#> 9 Kenya      Manyatta, Siaya        season    <tibble> 2.16e- 2  1394  1394     0   0
```

Suppose we want to compute the table separately for just one site in
Kenya:

``` r
# compute just for Kenya/Manyatta
ke_man_tbl <- mits_selection_factor_tables(dd,
  sites = "Kenya",
  catchments = "Manyatta"
)
```

The by-age table:

``` r
ke_man_tbl$table[[1]]
#> # A tibble: 4 × 5
#>   level      `DSS-only` `non-MITS`  MITS `non-MITS+DSS-only`
#>   <fct>           <dbl>      <dbl> <dbl>               <dbl>
#> 1 Stillbirth          4          9    69                  13
#> 2 Neonate            52         12    95                  64
#> 3 Infant             40          3    71                  43
#> 4 Child              36          3    43                  39
```

#### Tables of MITS cases with and without a specified condition by factor

Another table used to determine what factors to adjust for is MITS cases
with and without the condition for which we are calculating mortality
for by factor. This can be computed using `cc_factor_tables()`.

It has the same arguments as the previous function but also has the
`champs_group` argument that we use do define the cause we want to
compute the statistics for.

``` r
# table of MITS cases with and without a specified condition by factor
cbd_tbl <- cc_factor_tables(dd,
  sites = c("Bangladesh", "Ethiopia", "Kenya"),
  catchments = c("Baliakandi", "Haramaya", "Harar", "Kersa",
    "Manyatta", "Siaya"),
  champs_group = "Congenital birth defects"
)
```

The output structure is similar as shown before:

``` r
cbd_tbl
#> # A tibble: 21 × 8
#>    site       catchments             factor    table             pval  n_na     n pct_na
#>    <chr>      <chr>                  <chr>     <list>           <dbl> <int> <int>  <dbl>
#>  1 Bangladesh Baliakandi, Faridpur   age       <tibble [3 × 3]> 1         0    83   0   
#>  2 Bangladesh Baliakandi, Faridpur   education <tibble [4 × 3]> 0.384    30    83  36.1 
#>  3 Bangladesh Baliakandi, Faridpur   location  <tibble [2 × 3]> 1         0    83   0   
#>  4 Bangladesh Baliakandi, Faridpur   religion  <tibble [2 × 3]> 0.534     1    83   1.20
#>  5 Bangladesh Baliakandi, Faridpur   season    <tibble [2 × 3]> 1         0    83   0   
#>  6 Bangladesh Baliakandi, Faridpur   sex       <tibble [2 × 3]> 0.627     0    83   0   
#>  7 Bangladesh Baliakandi, Faridpur   va        <tibble [2 × 3]> 1         4    83   4.82
#>  8 Ethiopia   Haramaya, Harar, Kersa age       <tibble [4 × 3]> 0.308     0   172   0   
#>  9 Ethiopia   Haramaya, Harar, Kersa education <tibble [4 × 3]> 0.601   143   172  83.1 
#> 10 Ethiopia   Haramaya, Harar, Kersa location  <tibble [2 × 3]> 0.315     0   172   0   
#> # … with 11 more rows
```

The table for Bangladesh/age:

``` r
cbd_tbl$table[[1]]
#> # A tibble: 3 × 3
#>   level      `Congenital birth defects-` `Congenital birth defects+`
#>   <fct>                            <int>                       <int>
#> 1 Stillbirth                          30                           1
#> 2 Neonate                             48                           3
#> 3 Child                                1                           0
```

Which sites/factors pass the adjustment criterion:

``` r
filter(cbd_tbl, pval < 0.1, pct_na < 20)
#> # A tibble: 4 × 8
#>   site     catchments             factor table                pval  n_na     n pct_na
#>   <chr>    <chr>                  <chr>  <list>              <dbl> <int> <int>  <dbl>
#> 1 Ethiopia Haramaya, Harar, Kersa sex    <tibble [2 × 3]> 0.00583      0   172   0   
#> 2 Ethiopia Haramaya, Harar, Kersa va     <tibble [2 × 3]> 0.0759      15   172   8.72
#> 3 Kenya    Manyatta, Siaya        age    <tibble [4 × 3]> 0.000269     0   480   0   
#> 4 Kenya    Manyatta, Siaya        va     <tibble [3 × 3]> 0.00342     52   480  10.8
```

We can try other causes as well, such as malnutrition:

``` r
mal_tbl <- cc_factor_tables(dd,
  sites = c("Bangladesh", "Ethiopia", "Kenya"),
  catchments = c("Baliakandi", "Haramaya", "Harar", "Kersa",
    "Manyatta", "Siaya"),
  champs_group = "Malnutrition"
)

mal_tbl
#> # A tibble: 21 × 8
#>    site       catchments             factor    table                pval  n_na     n pct_na
#>    <chr>      <chr>                  <chr>     <list>              <dbl> <int> <int>  <dbl>
#>  1 Bangladesh Baliakandi, Faridpur   age       <tibble [3 × 3]> 1.20e- 2     0    83   0   
#>  2 Bangladesh Baliakandi, Faridpur   education <tibble [4 × 3]> 1   e+ 0    30    83  36.1 
#>  3 Bangladesh Baliakandi, Faridpur   location  <tibble [2 × 3]> 1   e+ 0     0    83   0   
#>  4 Bangladesh Baliakandi, Faridpur   religion  <tibble [2 × 3]> 1   e+ 0     1    83   1.20
#>  5 Bangladesh Baliakandi, Faridpur   season    <tibble [2 × 3]> 3.86e- 1     0    83   0   
#>  6 Bangladesh Baliakandi, Faridpur   sex       <tibble [2 × 3]> 1   e+ 0     0    83   0   
#>  7 Bangladesh Baliakandi, Faridpur   va        <tibble [2 × 3]> 1.27e- 2     4    83   4.82
#>  8 Ethiopia   Haramaya, Harar, Kersa age       <tibble [4 × 3]> 4.40e-16     0   172   0   
#>  9 Ethiopia   Haramaya, Harar, Kersa education <tibble [4 × 3]> 1.00e+ 0   143   172  83.1 
#> 10 Ethiopia   Haramaya, Harar, Kersa location  <tibble [2 × 3]> 9.70e- 6     0   172   0   
#> # … with 11 more rows
```

``` r
mal_tbl$table[[1]]
#> # A tibble: 3 × 3
#>   level      `Malnutrition-` `Malnutrition+`
#>   <fct>                <int>           <int>
#> 1 Stillbirth              31               0
#> 2 Neonate                 51               0
#> 3 Child                    0               1
```

#### Fraction and rate calculation data

A function, `get_rate_data()`, is available to compute the frequency of
deaths by the categories necessary for computing crude and adjusted
mortality rates and fractions (DSS only, CHAMPS, target, MITS, and
condition) and broken down by any specified adjustment variables. It
returns data sufficient to compute crude and adjusted mortality
fractions and rates.

As inputs, it takes the data returned from `process_data()`, the site(s)
and catchment(s) for which we want to compute the rates for, one or both
of `icd10_regex` or `condition` to specify what condition we are looking
at, and `adjust_vars`, specifying any variables we would like to adjust
by.

For example, to get the rate computation data for Ethiopia/Harar for
neural tube defects (defined by the ICD10 regular expression
“<sup>Q00\|</sup>Q01\|^Q05”) with no adjustment variables, we can do the
following:

``` r
get_rate_data(
  dd,
  site = "Ethiopia",
  catchments = "Harar",
  icd10_regex = "^Q00|^Q01|^Q05",
  adjust_vars = NULL
)
#> $data
#>   dss_only champs target mits condition
#> 1       36    104    140   82         5
#> 
#> $site
#> [1] "Ethiopia"
#> 
#> $catchment
#> [1] "Harar"
#> 
#> $year_range
#> # A tibble: 1 × 4
#>   site     catchment period_start_year period_end_year
#>   <chr>    <chr>                 <dbl>           <dbl>
#> 1 Ethiopia Harar                  2019            2020
#> 
#> $live_birth_data
#> # A tibble: 2 × 4
#>   site     catchment  year live_births
#>   <chr>    <chr>     <dbl>       <dbl>
#> 1 Ethiopia Harar      2019         469
#> 2 Ethiopia Harar      2020         768
#> 
#> $total_live_births
#> [1] 1237
```

This returns a data frame, `data` containing the pertinent data, the
site, catchment, and year range for this data, as well as the total live
births relate to these.

If we want to get the data grouped by age:

``` r
get_rate_data(
  dd,
  site = "Ethiopia",
  catchments = "Harar",
  icd10_regex = "^Q00|^Q01|^Q05",
  adjust_vars = "age"
)
#> $data
#> # A tibble: 4 × 6
#>   age        dss_only champs target  mits condition
#>   <fct>         <dbl>  <int>  <dbl> <int>     <int>
#> 1 Stillbirth       12     53     65    42         2
#> 2 Neonate           2     48     50    37         3
#> 3 Infant            7      3     10     3         0
#> 4 Child            15      0     15     0         0
#> 
#> $site
#> [1] "Ethiopia"
#> 
#> $catchment
#> [1] "Harar"
#> 
#> $year_range
#> # A tibble: 1 × 4
#>   site     catchment period_start_year period_end_year
#>   <chr>    <chr>                 <dbl>           <dbl>
#> 1 Ethiopia Harar                  2019            2020
#> 
#> $live_birth_data
#> # A tibble: 2 × 4
#>   site     catchment  year live_births
#>   <chr>    <chr>     <dbl>       <dbl>
#> 1 Ethiopia Harar      2019         469
#> 2 Ethiopia Harar      2020         768
#> 
#> $total_live_births
#> [1] 1237
```

Grouped by sex:

``` r
get_rate_data(
  dd,
  site = "Ethiopia",
  catchments = "Harar",
  icd10_regex = "^Q00|^Q01|^Q05",
  adjust_vars = "sex"
)
#> $data
#> # A tibble: 2 × 6
#>   sex    dss_only champs target  mits condition
#>   <fct>     <dbl>  <int>  <dbl> <int>     <int>
#> 1 Male         17     54     71    44         0
#> 2 Female        7     50     57    38         5
#> 
#> $site
#> [1] "Ethiopia"
#> 
#> $catchment
#> [1] "Harar"
#> 
#> $year_range
#> # A tibble: 1 × 4
#>   site     catchment period_start_year period_end_year
#>   <chr>    <chr>                 <dbl>           <dbl>
#> 1 Ethiopia Harar                  2019            2020
#> 
#> $live_birth_data
#> # A tibble: 2 × 4
#>   site     catchment  year live_births
#>   <chr>    <chr>     <dbl>       <dbl>
#> 1 Ethiopia Harar      2019         469
#> 2 Ethiopia Harar      2020         768
#> 
#> $total_live_births
#> [1] 1237
```

Grouped by age and sex:

``` r
get_rate_data(
  dd,
  site = "Ethiopia",
  catchments = "Harar",
  icd10_regex = "^Q00|^Q01|^Q05",
  adjust_vars = c("age", "sex")
)
#> $data
#> # A tibble: 8 × 7
#> # Groups:   age [4]
#>   age        sex    dss_only champs target  mits condition
#>   <fct>      <fct>     <dbl>  <int>  <dbl> <int>     <int>
#> 1 Stillbirth Male          0     26     26    22         0
#> 2 Stillbirth Female        0     27     27    20         2
#> 3 Neonate    Male          2     26     28    20         0
#> 4 Neonate    Female        0     22     22    17         3
#> 5 Infant     Male          5      2      7     2         0
#> 6 Infant     Female        2      1      3     1         0
#> 7 Child      Male         10      0     10     0         0
#> 8 Child      Female        5      0      5     0         0
#> 
#> $site
#> [1] "Ethiopia"
#> 
#> $catchment
#> [1] "Harar"
#> 
#> $year_range
#> # A tibble: 1 × 4
#>   site     catchment period_start_year period_end_year
#>   <chr>    <chr>                 <dbl>           <dbl>
#> 1 Ethiopia Harar                  2019            2020
#> 
#> $live_birth_data
#> # A tibble: 2 × 4
#>   site     catchment  year live_births
#>   <chr>    <chr>     <dbl>       <dbl>
#> 1 Ethiopia Harar      2019         469
#> 2 Ethiopia Harar      2020         768
#> 
#> $total_live_births
#> [1] 1237
```

For congenital birth defects instead of NTDs:

``` r
get_rate_data(
  dd,
  site = "Ethiopia",
  catchments = "Harar",
  condition = "Congenital birth defects",
  adjust_vars = "age"
)
#> $data
#> # A tibble: 4 × 6
#>   age        dss_only champs target  mits condition
#>   <fct>         <dbl>  <int>  <dbl> <int>     <int>
#> 1 Stillbirth       12     53     65    42         2
#> 2 Neonate           2     48     50    37         7
#> 3 Infant            7      3     10     3         0
#> 4 Child            15      0     15     0         0
#> 
#> $site
#> [1] "Ethiopia"
#> 
#> $catchment
#> [1] "Harar"
#> 
#> $year_range
#> # A tibble: 1 × 4
#>   site     catchment period_start_year period_end_year
#>   <chr>    <chr>                 <dbl>           <dbl>
#> 1 Ethiopia Harar                  2019            2020
#> 
#> $live_birth_data
#> # A tibble: 2 × 4
#>   site     catchment  year live_births
#>   <chr>    <chr>     <dbl>       <dbl>
#> 1 Ethiopia Harar      2019         469
#> 2 Ethiopia Harar      2020         768
#> 
#> $total_live_births
#> [1] 1237
```

Note that we can now use this data to compute the rates and fractions.
Currently we show how to do this manually. We are working on more
general-purpose functions that will compute the rates automatically.

Let’s look at NTDs for Ethiopia/Harar adjusted for age. First, we can
compute the crude cause-specific mortality, `cCSMF` as follows. Note
that a function `get_interval()` is made available to compute bayesian
credible intervals for the estimate.

``` r
et_harar <- get_rate_data(
  dd,
  site = "Ethiopia",
  catchments = "Harar",
  icd10_regex = "^Q00|^Q01|^Q05",
  adjust_vars = "age"
)

mits <- sum(et_harar$data$mits)
ntd <- sum(et_harar$data$condition)

cCSMF <- 100 * ntd / mits
cCSMF
#> [1] 6.097561

# 90% credible intervals
cCSMF_CrI <- get_interval(ntd / mits, mits)
cCSMF_CrI
#> [1]  2.828201 11.610284
```

To compute the adjusted fraction, `aCSMF`, for this data we need to
group some of the age groups before making the adjustment due to the
zero counts of NTDs in the infant and child groups. We will group infant
and child into stillborn as follows:

``` r
# combine stillbirth, infant, and child into same group
rate_data <- et_harar$data %>%
  mutate(group = c(1, 2, 1, 1)) %>%
  select(-age) %>%
  group_by(group) %>%
  summarise_all(sum)

rate_data
#> # A tibble: 2 × 6
#>   group dss_only champs target  mits condition
#>   <dbl>    <dbl>  <int>  <dbl> <int>     <int>
#> 1     1       34     56     90    45         2
#> 2     2        2     48     50    37         3
```

Now we can compute the weights and take the dot product to get the
aCSMF:

``` r
ns <- rate_data$mits
n <- rate_data$condition
N <- rate_data$target

aCSMF <- 100 * (n / ns) %*% (N / sum(N)) %>% as.vector()
aCSMF
#> [1] 5.752896

aCSMF_CrI <- get_interval(aCSMF / 100, sum(N))
aCSMF_CrI
#> [1] 3.161020 9.679697
```

Now we can use this to get the all-cause total under-5 mortality rate
and the cause-specific crude TU5MR and adjusted TU5MR as follows:

``` r
# (stillbirths + under-five deaths) / (live-births + stillbirths) x 10,000
lb <- et_harar$total_live_births
sb <- filter(et_harar$data, age == "Stillbirth") %>% pull(target)
u5d_sb <- sum(et_harar$data$target)

acTU5MR <- 10000 * u5d_sb / (lb + sb)
acTU5MR
#> [1] 1075.269

cTU5MR <- (cCSMF / 100) * acTU5MR
cTU5MR
#> [1] 65.56517

cTU5MR_CrI <- (cCSMF_CrI / 100) * acTU5MR
cTU5MR_CrI
#> [1]  30.41076 124.84176


aTU5MR <- (aCSMF / 100) * acTU5MR
aTU5MR
#> [1] 61.85909

aTU5MR_CrI <- (aCSMF_CrI / 100) * acTU5MR
aTU5MR_CrI
#> [1]  33.98947 104.08276
```

A function that provides all of this is in development, but there are
difficuties in dealing with how to roll up zero-count categories that
are being worked out.

#### Ad hoc computations

The following code can be used to compute the total number of deaths in
Baliakandi Bangladesh, by age and MITS and non-MITS+DSS-only. This is
part of the computation that is provided automatically by
`mits_selection_factor_tables()` described above.

``` r
bind_rows(
  dd$ads %>%
    filter(site == "Bangladesh", catchment == "Baliakandi") %>%
    count(mits_flag, age),
  dd$dss %>%
    filter(site == "Bangladesh", catchment == "Baliakandi",
      factor == "age") %>%
    group_by(level) %>%
    summarise(n = sum(n)) %>%
    rename(age = "level") %>%
    mutate(mits_flag = 0)
) %>%
  group_by(mits_flag, age) %>%
  summarise(n = sum(n), .groups = "drop")
#> # A tibble: 7 × 3
#>   mits_flag age            n
#>       <dbl> <chr>      <dbl>
#> 1         0 Child         62
#> 2         0 Infant        82
#> 3         0 Neonate      372
#> 4         0 Stillbirth   401
#> 5         1 Child          1
#> 6         1 Neonate       51
#> 7         1 Stillbirth    31
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
automatically provided by the function `cc_factor_tables()` described
previously.

More to come…
