
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- devtools::build_readme() -->

# champsmortality

<!-- badges: start -->
<!-- badges: end -->

The goal of champsmortality is to …

## Installation

You can install the development version of champsmortality with the following:

``` r
install.packages("remotes") # one time only
remotes::install_github("ki-tools/champsmortality")
```

## Example

``` r
library(champsmortality)
```

The first time you use this package, you need to place the appropriate
data files in a data directory that the package will pull from to
perform the calculations. A function `create_dataset_directory()` is
provided to help get this set up.

``` r
data_dir <- tempfile()
create_dataset_directory(data_dir)
#> 
#> The directory
#> '/var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//RtmpEE8fUv/file135674d252b54'
#> is ready for the appropriate data files to be placed in it. The following
#> datasets should be placed in this directory:
#> 
#> 1. CHAMPS Analytics Dataset: This dataset is available as a downloadable file
#>      from LabKey and is continuously updated. It contains most of the CHAMPS
#>      variables that are needed for the analysis.
#> 2. Maternal Registry Forms table: This dataset is also available as a
#>      downloadable file from LabKey and contains information about maternal
#>      age and education.
#> 3. CHAMPS vocabulary: This dataset provides a lookup table for all CHAMPS
#>      codes, providing a correspond 'name' and 'preferred name' for each. This
#>      file is accessible from the CHAMPS L2 dataset from dataverse.
#> 4. DSS: This dataset contains counts of cases from the demographic
#>      surveillance system (DSS) corresponding to each CHAMPS site and
#>      catchment area, only for DSS cases that are not in the CHAMPS data.
#>      These counts are broken down by age group, year, location of death,
#>      season of death, maternal education, sex of child, and verbal autopsy
#>      cause of death.
#> 5. Season definition: This dataset is a spreadsheet containing rainy and dry
#>      season date ranges for each site, which will be used to classify the
#>      season in which each case occurs.
#> 
#> Once the files are in place, edit the file /var/folders/7b/thg__1xx7w98wc4rs8t3djrw0000gn/T//RtmpEE8fUv/file135674d252b54/config.yaml to provide the file names corresponding to each of these datasets.
#> The config.yaml template looks like this:
#> 
#> champs_analytics_dataset: ''
#> maternal_registry_dataset: ''
#> champs_vocabulary_dataset: ''
#> dss_dataset: ''
#> season_dataset: ''
#> So for example, if the CHAMPS Analytics Dataset you placed in this directory is named 'Analytics_Dataset_2021-09-01.xlsx', you would edit the corresponding line in config.yaml as follows:
#> 
#> champs_analytics_dataset: 'Analytics_Dataset_2021-09-01.xlsx'
```

This is something that only needs to be done once.

Once this is set up and the appropriate files are placed and mapped in
the `config.yaml` file, you can read in the data with the following:

``` r
d <- read_and_validate_data(data_dir)
```

This will read in the data files and ensure that all of the variables
required to perform the calculations are present. If they are not, you
will see an error message and will need to correct the error before
being able to use the package.

More to come…
