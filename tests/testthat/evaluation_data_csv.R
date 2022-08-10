## In package
# "seasons.csv"
# "dhs_allcause_u5.csv"
# "catchment_lookup.csv"
# "config.yaml"
## Proposed in package
# "live_births.csv"
# "vocabulary_2022-05-24.csv"
# "vocabulary_2022-05-24.csv"
## Not in package
# "analytics_2022-05-24.csv"
# "registry_2022-05-24.csv"

### This script exemplifies the build.
### Paths for read and write are not correct.
### Write is commented

## Here are the two main files that are sensitive

# champs_analytics_dataset: 'analytics_2022-05-24.csv'
# maternal_registry_dataset: 'registry_2022-05-24.csv'
library(tidyverse)
set.seed(1976)

cl <- read_csv("data/catchment_lookup.csv") |>
  mutate(
   site_name = case_when(
      site_name == "Sierra Leone" ~ "S1",
      site_name == "Mali" ~ "S2",
      site_name == "Mozambique" ~ "S3", 
      site_name == "South Africa" ~ "S4",
      site_name == "Ethiopia" ~ "S5",
      site_name == "Bangladesh" ~ "S6",
      site_name == "Kenya" ~ "S7"
      ),
    catchment = case_when(
      catchment == "Baliakandi" ~ "C1",
      catchment == "Faridpur" ~ "C2",
      catchment == "Harar" ~ "C3",
      catchment == "Haramaya" ~ "C4",
      catchment == "Kersa" ~ "C5",
      catchment == "Manyatta" ~ "C6",
      catchment == "Siaya" ~ "C7",
      catchment == "Bamako" ~ "C8",
      catchment == "Manhica" ~ "C9",
      catchment == "Quelimane" ~ "C10",
      catchment == "Makeni" ~ "C11",
      catchment == "Soweto" ~ "C12"
    )
  )

cl |>
head(5) |>
knitr::kable()

# |site_name |catchment |catchment_id |
# |:---------|:---------|:------------|
# |S6        |C1        |001          |
# |S6        |C2        |002          |
# |S6        |C2        |003          |
# |S6        |C2        |004          |
# |S6        |C2        |005          |

# write_csv(cl, "obfuscated_data/catchment_lookup.csv")

# LoR for dhs_allcause_u5
# need to figure out rate
dhs <- read_csv("data/dhs.csv") |>
  mutate(
   site = case_when(
      site == "Sierra Leone" ~ "S1",
      site == "Mali" ~ "S2",
      site == "Mozambique" ~ "S3", 
      site == "South Africa" ~ "S4",
      site == "Ethiopia" ~ "S5",
      site == "Bangladesh" ~ "S6",
      site == "Kenya" ~ "S7"
      ),
    catchment = case_when(
      catchment == "Baliakandi" ~ "C1",
      catchment == "Faridpur" ~ "C2",
      catchment == "Harar" ~ "C3",
      catchment == "Haramaya" ~ "C4",
      catchment == "Kersa" ~ "C5",
      catchment == "Manyatta" ~ "C6",
      catchment == "Siaya" ~ "C7",
      catchment == "Bamako" ~ "C8",
      catchment == "Manhica" ~ "C9",
      catchment == "Quelimane" ~ "C10",
      catchment == "Makeni" ~ "C11",
      catchment == "Soweto" ~ "C12"
    )
  )

dhs |>
head(4) |>
knitr::kable()

# |site |catchment | year|age     |  rate|note |
# |:----|:---------|----:|:-------|-----:|:----|
# |S6   |C2        | 2017|U5      | 414.0|NA   |
# |S6   |C2        | 2017|Neonate | 132.0|NA   |
# |S6   |C2        | 2017|Infant  |   0.1|NA   |
# |S6   |C2        | 2017|Child   | 449.0|NA   |

# write_csv(dhs, "obfuscated_data/dhs.csv")


# dss
dss <- read_csv("data/dss_2022-05-24.csv") |>
  mutate(
   site = case_when(
      site == "Sierra Leone" ~ "S1",
      site == "Mali" ~ "S2",
      site == "Mozambique" ~ "S3",
      site == "South Africa" ~ "S4",
      site == "Ethiopia" ~ "S5",
      site == "Bangladesh" ~ "S6",
      site == "Kenya" ~ "S7"
      ),
    catchment = case_when(
      catchment == "Baliakandi" ~ "C1",
      catchment == "Faridpur" ~ "C2",
      catchment == "Harar" ~ "C3",
      catchment == "Haramaya" ~ "C4",
      catchment == "Kersa" ~ "C5",
      catchment == "Manyatta" ~ "C6",
      catchment == "Siaya" ~ "C7",
      catchment == "Bamako" ~ "C8",
      catchment == "Manhica" ~ "C9",
      catchment == "Quelimane" ~ "C10",
      catchment == "Makeni" ~ "C11",
      catchment == "Soweto" ~ "C12"
    )
  )

dss |>
head(5) |>
knitr::kable()

# |site |catchment |age        |factor   |level     |  n| period_start_year| period_end_year|
# |:----|:---------|:----------|:--------|:---------|--:|-----------------:|---------------:|
# |S6   |C1        |Stillbirth |religion |Christian |  0|              2017|            2020|
# |S6   |C1        |Neonate    |religion |Christian |  0|              2017|            2020|
# |S6   |C1        |Infant     |religion |Christian |  0|              2017|            2020|
# |S6   |C1        |Child      |religion |Christian |  0|              2017|            2020|
# |S5   |C4        |Stillbirth |religion |Christian |  0|              2017|            2020|

# write_csv(dss, "obfuscated_data/dss_2022-05-24.csv")

# live births
lb <- read_csv("data/live_births.csv") |>
  mutate(
   site = case_when(
      site == "Sierra Leone" ~ "S1",
      site == "Mali" ~ "S2",
      site == "Mozambique" ~ "S3", 
      site == "South Africa" ~ "S4",
      site == "Ethiopia" ~ "S5",
      site == "Bangladesh" ~ "S6",
      site == "Kenya" ~ "S7"
      ),
    catchment = case_when(
      catchment == "Baliakandi" ~ "C1",
      catchment == "Faridpur" ~ "C2",
      catchment == "Harar" ~ "C3",
      catchment == "Haramaya" ~ "C4",
      catchment == "Kersa" ~ "C5",
      catchment == "Manyatta" ~ "C6",
      catchment == "Siaya" ~ "C7",
      catchment == "Bamako" ~ "C8",
      catchment == "Manhica" ~ "C9",
      catchment == "Quelimane" ~ "C10",
      catchment == "Makeni" ~ "C11",
      catchment == "Soweto" ~ "C12"
    )
  )

lb |>
    head(5) |>
    knitr::kable()

# |site |catchment | year| live_births|
# |:----|:---------|----:|-----------:|
# |S6   |C1        | 2017|        4787|
# |S6   |C1        | 2018|        4078|
# |S6   |C1        | 2019|        5193|
# |S6   |C1        | 2020|        5090|
# |S5   |C4        | 2020|        3210|

# write_csv(lb, "obfuscated_data/live_births.csv")

# Seasons
seasons <- read_csv("data/seasons.csv") |>
  mutate(
    site = case_when(
    site == "Sierra Leone" ~ "S1",
    site == "Mali" ~ "S2",
    site == "Mozambique" ~ "S3", 
    site == "South Africa" ~ "S4",
    site == "Ethiopia" ~ "S5",
    site == "Bangladesh" ~ "S6",
    site == "Kenya" ~ "S7"
    )
  )

seasons |>
    head(5) |>
    knitr::kable()

# |site |season |start      |end        |
# |:----|:------|:----------|:----------|
# |S6   |Dry    |2017-01-01 |2017-05-31 |
# |S6   |Rainy  |2017-06-01 |2017-10-31 |
# |S6   |Dry    |2017-11-01 |2018-05-31 |
# |S6   |Rainy  |2018-06-01 |2018-10-31 |
# |S6   |Dry    |2018-11-01 |2019-05-31 |


# write_csv(seasons, "obfuscated_data/seasons.csv") 

# no changes to vocabulary
# read_csv("data/vocabulary_2022-05-24.csv") |>
#   write_csv("obfuscated_data/vocabulary_2022-05-24.csv")

read_csv("data/vocabulary_2022-05-24.csv")  |>
    tail(2) |>
    knitr::kable()

# |Champs Local Code |C Name                                               |C Pref Name                                          |C Code  | #nolint
# |:-----------------|:----------------------------------------------------|:----------------------------------------------------|:-------| #nolint
# |CH03159           |Severe microcephaly* confirm with all available data |Severe microcephaly* confirm with all available data |CH03159 | #nolint
# |CH03160           |Severe malnutrition* confirm with all available data |Severe malnutrition* confirm with all available data |CH03160 | #nolint


# copy the YAML
# file.copy("data/config.yaml", "obfuscated_data/config.yaml")

## Analytics and registry need more edits as shown below
### Registry data
# - completely resorted  "Mat 0013"
# - Adjusted ages in Mat 0010 and Mat011 by adding random normal
#   values with an standard deviation of 2 to the current ages.
# - Created new generic subject IDs that have a one-to-one mapping
#   to the IDs created in the Analytics data set.

### Analytics Data
# - Completely reshuffled the `Calc Sex` (sample(`Calc Sex`, n))
# - Completely reshuffled the `Age Group` (sample(`Age Group`, n))
# - Changed the IDs to have the same structure, SLAA00110. 
#    - Two letters that map to the new generic locations
#    - Two additional letters stayed the same.
#    - Completely reshuffled the five digit numbers within sbut kept the
#      the same numbers.

# only columns used from the registry file. Maternal age and education.
mreg_names <- c(
  "Mort Id",
  "Mat 0010",
  "Mat 0011",
  "Mat 0013"
)

## The only columns used in the package for the analytics dataset
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

adat <- read_csv("data/analytics_2022-05-24.csv",
    guess_max = 10000000) |>
    select(any_of(ads_names)) |>
    mutate_if(is.logical, as.character)

new_ids <- adat |>
  select(cid = Champsid, sn = `Site Name`, catch = `Catchment Id`) |>
  mutate(
    cid_text = stringr::str_replace_all(cid, "[:digit:]*", ""),
    cid_number = stringr::str_extract_all(cid,
      "[:digit:]{5}", simplify = TRUE)[, 1],
    cid_loc = stringr::str_sub(cid_text, 1, 2),
    cid_letters = stringr::str_sub(cid_text, 3, 4),
    sn_new = case_when(
      sn == "Sierra Leone" ~ "S1",
      sn == "Mali" ~ "S2",
      sn == "Mozambique" ~ "S3", 
      sn == "South Africa" ~ "S4",
      sn == "Ethiopia" ~ "S5",
      sn == "Bangladesh" ~ "S6",
      sn == "Kenya" ~ "S7"
      ),
      cid_loc_new = case_when(
      sn == "Sierra Leone" ~ "SO",
      sn == "Mali" ~ "ST",
      sn == "Mozambique" ~ "SH", 
      sn == "South Africa" ~ "SF",
      sn == "Ethiopia" ~ "SI",
      sn == "Bangladesh" ~ "SS",
      sn == "Kenya" ~ "SE"
      ),
    cid_text_new = stringr::str_c(cid_loc_new, cid_letters)
  ) |>
  group_by(sn, catch) |>
  mutate(cid_number_new = sample(cid_number, n())) |>
  ungroup() |>
  mutate(cid_new = stringr::str_c(cid_text_new, cid_number_new)) |>
  select(cid, cid_new, sn_new, catch, everything())


adat <- adat |>
  group_by(`Site Name`, `Catchment Id`) |>
  mutate(
    `Calc Sex` = sample(`Calc Sex`, n()),
    `Age Group` = sample(`Age Group`, n())
    ) |>
  left_join(select(new_ids, Champsid = cid, cid_new, sn_new)) |>
  select(Champsid, `Site Name`, cid_new, sn_new, everything()) |>
  select(-Champsid, -`Site Name`, Champsid = cid_new, `Site Name` = sn_new)

adat |>
    select(Champsid, `Site Name`, `Catchment Id`, `Calc Sex`, `Age Group`) |>
    tail(5) |>
    knitr::kable()

# |Champsid  |Site Name |Catchment Id |Calc Sex |Age Group |
# |:---------|:---------|:------------|:--------|:---------|
# |SEAA00287 |S7        |002          |CH00030  |CH01404   |
# |SEAA00889 |S7        |001          |CH00030  |CH00716   |
# |SEAA01287 |S7        |002          |CH00033  |CH00716   |
# |SEAA00910 |S7        |002          |CH00031  |CH00718   |
# |SEAA01333 |S7        |001          |CH00030  |CH00716   |


# write_csv(adat, "obfuscated_data/analytics_2022-05-24.csv")
  
    	


# maternal ages in these two columns
# mat 0013 is maternal education
mdat <- read_csv("data/registry_2022-05-24.csv",
    guess_max = 10000000) |>
    select(any_of(mreg_names)) |>
    filter(if_any(everything(), purrr::negate(is.na))) |>
    mutate(
      `Mat 0013` = sample(`Mat 0013`, n()),
      `Mat 0010r` = round(`Mat 0010` +
        rnorm(n(), sd = 2), 0),
      `Mat 0011r` = round(`Mat 0011` +
       rnorm(n(), sd = 2), 0)
    ) |>
    left_join(select(new_ids, `Mort Id` = cid, cid_new)) |>
    select(-`Mort Id`, `Mort Id` = cid_new) |>
    select(`Mort Id`, everything())

mdat |>
    mutate(diff = `Mat 0010` - `Mat 0010r`) |>
    ggplot(aes(x = `Mat 0010`, y = `Mat 0010r`)) +
    geom_point() +
    scale_x_continuous(breaks = seq(13, 50, by = 3)) +
    scale_y_continuous(breaks = seq(13, 50, by = 3))

mdat <- mdat |>
    select(-`Mat 0010`, -`Mat 0011`) |>
    select(`Mort Id`,
        `Mat 0010` = `Mat 0010r`,
        `Mat 0011` = `Mat 0011r`,
        `Mat 0013`)

mdat |>
    head(5) |>
    knitr::kable()

# |Mort Id   | Mat 0010| Mat 0011|Mat 0013 |
# |:---------|--------:|--------:|:--------|
# |SOAA00263 |       NA|       29|CH01092  |
# |SEAA01013 |       NA|       27|4        |
# |SSAA03308 |       NA|       20|4        |
# |STAA02286 |       34|       NA|CH01092  |
# |SIAA00180 |       18|       NA|CH01093  |

# write_csv(mdat, "obfuscated_data/registry_2022-05-24.csv")