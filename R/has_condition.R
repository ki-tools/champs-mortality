has_icd10_cc <- function(ads, rgx) {
    grepl(rgx, ads$underlying_cause_calc) |
    grepl(rgx, ads$immediate_cod) |
    grepl(rgx, ads$morbid_condition_01) |
    grepl(rgx, ads$morbid_condition_02) |
    grepl(rgx, ads$morbid_condition_03) |
    grepl(rgx, ads$morbid_condition_04) |
    grepl(rgx, ads$morbid_condition_05) |
    grepl(rgx, ads$morbid_condition_06) |
    grepl(rgx, ads$morbid_condition_07) |
    grepl(rgx, ads$morbid_condition_08)
}

has_champs_group_cc <- function(ads, group) {
  ads$ic_champs_group_desc == group |
  ads$uc_champs_group_desc == group |
  ads$morbid_cond_01_champs_group_desc == group |
  ads$morbid_cond_02_champs_group_desc == group |
  ads$morbid_cond_03_champs_group_desc == group |
  ads$morbid_cond_04_champs_group_desc == group |
  ads$morbid_cond_05_champs_group_desc == group |
  ads$morbid_cond_06_champs_group_desc == group |
  ads$morbid_cond_07_champs_group_desc == group |
  ads$morbid_cond_08_champs_group_desc == group
}
