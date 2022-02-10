#' Read and validate data files from a specified directory
#' @param data_path path containing config.yaml and data files
#' @importFrom assertthat assert_that
#' @importFrom yaml read_yaml
#' @importFrom cli format_error
read_and_validate_data <- function(data_path) {
  assertthat::assert_that(dir.exists(data_path),
    msg = cli::format_error(
      "The 'data_path' provided does not exist: '{data_path}'"))

  yaml_path <- file.path(data_path, "config.yaml")
  assertthat::assert_that(file.exists(yaml_path),
    msg = cli::format_error(
      "Configuration file does not exist: '{yaml_path}'"))

  cfg <- yaml::read_yaml(yaml_path)

  dff <- setdiff(ds_names, names(cfg))
  dff_str <- paste(dff, collapse = ", ")
  assertthat::assert_that(length(dff) == 0,
    msg = cli::format_error("
      The following required field(s) are not found in config.yaml: {dff_str}"))

  ## champs_analytics_dataset
  ads  <- read_file(data_path, cfg, ads_names,  "champs_analytics_dataset")
  voc  <- read_file(data_path, cfg, voc_names,  "champs_vocabulary_dataset")
  mreg <- read_file(data_path, cfg, mreg_names, "maternal_registry_dataset")
  dss  <- read_file(data_path, cfg, dss_names,  "dss_dataset")
  seas <- read_file(data_path, cfg, seas_names, "season_lookup")
  rlgn <- read_file(data_path, cfg, rlgn_names, "religion_lookup")

  list(
    ads  = ads,
    voc  = voc,
    mreg = mreg,
    seas = seas,
    dss = dss,
    rlgn = rlgn
  )
}

#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom snakecase to_snake_case
read_file <- function(data_path, cfg, nms, ds_name) {
  path <- file.path(data_path, cfg[[ds_name]])

  assertthat::assert_that(file.exists(path),
    msg = cli::format_error(
      "The file specified in config.yaml for {ds_name} does not exist: {path}"))

  ext <- tools::file_ext(path)

  assertthat::assert_that(ext %in% c("csv", "xls", "xlsx"),
    msg = cli::format_error(
      "File must have extension 'csv', 'xlsx', or 'xls': {path}"))

  if (ext == "csv") {
    x <- readr::read_csv(path, show_col_types = FALSE, guess_max = 1e6)
  } else {
    x <- readxl::read_excel(path, guess_max = 1e6)
  }

  names(x) <- snakecase::to_snake_case(names(x))
  nms2 <- snakecase::to_snake_case(nms)

  idx <- which(!nms2 %in% names(x))
  vars <- paste0("      ", nms[idx], collapse = "\n")
  assertthat::assert_that(length(idx) == 0,
    msg = cli::format_error(
      "Expecting variables like the following in {path}:\n{vars}"))

  cli::cli_alert_success(path)

  x
}
