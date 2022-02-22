commas <- function(x) paste(x, collapse = ", ")

voc_lookup <- function(x, voc, join_var, ds_name) {
  join_by <- "champs_local_code"
  names(join_by) <- join_var
  old_var_new_name <- paste0(join_var, "_code")

  res <- x %>%
    mutate(
      "{join_var}" := substr(.data[[join_var]], 1, 7),
      "{old_var_new_name}" := .data[[join_var]]
    ) %>%
    left_join(select(voc, one_of(c("champs_local_code", "c_name"))),
      by = join_by) %>%
    select(-.data[[join_var]]) %>%
    rename("{join_var}" := "c_name")
  cli::cli_alert_success("Used CHAMPS vocabulary to transform \\
    '{join_var}' in {ds_name}", wrap = TRUE)

  not_handled <- setdiff(unique(
    x[[old_var_new_name]][is.na(x[[join_var]])]), NA)
  nnh <- length(not_handled)
  if (nnh > 0) {
    cli::cli_alert_warning("After transforming {join_var}, found {nnh} \\
      code{?s} that do not match with any codes found in the \\
      CHAMPS vocabulary file: {commas(not_handled)}",
      wrap = TRUE)
  }

  res
}

check_valid_vals <- function(x, variable, valid, name, ds_name) {
  var_diff <- setdiff(unique(x[[variable]]), c(valid, NA))
  assertthat::assert_that(length(var_diff) == 0,
    msg = cli::format_error("{ds_name} contains {name} that are not in \\
      expected: {commas(var_diff)}"))
  cli::cli_alert_success("Checked that values for '{variable}' in the \\
    {ds_name} are correct", wrap = TRUE)
}
