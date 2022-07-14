# or that returns FALSE when comparing FALSE with NA
'%or%' <- function(a, b) {
  !((is.na(a) & !b) | (is.na(b) & !a) | (!a & !b))
}
# or(
#   c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA),
#   c(TRUE, FALSE, NA, TRUE, FALSE, NA, NA))

#' Check to see if an ICD10 regular expression is in the causal chain
#' @param ads CHAMPS analysis dataset
#' @param rgx regular expression representing ICD10 codes
#' @param cc if TRUE: searches across causal chain, if FALSE: searches in
#' underlying cause only
#' @export
has_icd10 <- function(ads, rgx, cc = TRUE) {
  if (cc) {
    grepl(rgx, ads$underlying_cause_calc) %or%
    grepl(rgx, ads$immediate_cod) %or%
    grepl(rgx, ads$morbid_condition_01) %or%
    grepl(rgx, ads$morbid_condition_02) %or%
    grepl(rgx, ads$morbid_condition_03) %or%
    grepl(rgx, ads$morbid_condition_04) %or%
    grepl(rgx, ads$morbid_condition_05) %or%
    grepl(rgx, ads$morbid_condition_06) %or%
    grepl(rgx, ads$morbid_condition_07) %or%
    grepl(rgx, ads$morbid_condition_08)
  } else {
    grepl(rgx, ads$underlying_cause_calc)
  }
}

#' Check to see if a CHAMPS group is in the causal chain
#' @param ads CHAMPS analysis dataset
#' @param group string representing a CHAMPS group
#' @param cc if TRUE: searches across causal chain, if FALSE: searches in
#' underlying cause only
#' @export
has_champs_group <- function(ads, group, cc = TRUE) {
  if (cc) {
    (ads$ic_champs_group_desc %in% group) %or%
    (ads$uc_champs_group_desc %in% group) %or%
    (ads$morbid_cond_01_champs_group_desc %in% group) %or%
    (ads$morbid_cond_02_champs_group_desc %in% group) %or%
    (ads$morbid_cond_03_champs_group_desc %in% group) %or%
    (ads$morbid_cond_04_champs_group_desc %in% group) %or%
    (ads$morbid_cond_05_champs_group_desc %in% group) %or%
    (ads$morbid_cond_06_champs_group_desc %in% group) %or%
    (ads$morbid_cond_07_champs_group_desc %in% group) %or%
    (ads$morbid_cond_08_champs_group_desc %in% group)
  } else {
    ads$uc_champs_group_desc %in% group
  }
}

#' Check to see if an ICD10 regular expression is in the main maternal cause
#' @param ads CHAMPS analysis dataset
#' @param rgx regular expression representing ICD10 codes
#' @export
has_maternal_icd10 <- function(ads, rgx) {
  grepl(rgx, ads$main_maternal_disease_condition)
}

#' Check to see if a CHAMPS group is in the main maternal cause
#' @param ads CHAMPS analysis dataset
#' @param group string representing a CHAMPS group
#' @export
has_maternal_champs_group <- function(ads, group) {
  ads$main_maternal_champs_group_desc %in% group
}
