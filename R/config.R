#' Create a dataset directory with the shell of a config.yaml file
#' @param path path where input data will be stored (must be a non-existing
#' path or an existing directory)
#' @export
#' @importFrom yaml write_yaml
#' @importFrom cli cli_alert_danger cli_alert_info cli_div boxx
#' cli_alert_success cli_ol cli_li cli_end cli_alert cli_text
create_dataset_directory <- function(path) {
  if (!file.exists(path))
    dir.create(path, recursive = TRUE)

  cli::cli_div(theme = list(
    .alert = list("text-exdent" = 2),
    `ol li` = list("text-exdent" = 3)
  ))

  assertthat::assert_that(dir.exists(path),
    msg = cli::cli_alert_danger("The path provided to \\
      create_dataset_directory() exists but is not a directory", wrap = TRUE))

  ff <- list.files(path)
  if (length(ff) > 0) {
    cli::cli_alert_info("NOTE: Files already exist in the path provided to \\
      create_dataset_directory()", wrap = TRUE)
    cli::cli_text("")
  }

  yaml_path <- file.path(path, "config.yaml")
  if (file.exists(yaml_path)) {
    cli::cli_alert_info("NOTE: config.yaml already exists... It will not be \\
      overwritten.", wrap = TRUE)
    cli::cli_text("")
  } else {
    content <- structure(rep("", length(ds_names)), names = ds_names)
    yaml::write_yaml(as.list(content), yaml_path)
  }

  cli::cli_alert_success("The directory '{path}' is ready for the \\
    appropriate data files to be placed in it. The following datasets \\
    should be placed in this directory:", wrap = TRUE)
  cli::cli_text("")
  olid <- cli::cli_ol()
  cli::cli_li("CHAMPS Analytics Dataset: This dataset is \\
    available as a downloadable file from LabKey and is continuously \\
    updated. It contains most of the CHAMPS variables that are needed for \\
    the analysis.")
  cli::cli_li("Maternal Registry Forms table: This \\
    dataset is also available as a downloadable file from LabKey and \\
    contains information about maternal age and education.")
  cli::cli_li("CHAMPS vocabulary: This dataset provides a \\
    lookup table for all CHAMPS codes, providing a correspond 'name' and \\
    'preferred name' for each. This file is accessible from the CHAMPS L2 \\
    dataset from dataverse.")
  cli::cli_li("DSS: This dataset contains counts of cases \\
    from the demographic surveillance system (DSS) corresponding to each \\
    CHAMPS site and catchment area, only for DSS cases that are not in the \\
    CHAMPS data. These counts are broken down by age group, year, location \\
    of death, season of death, maternal education, sex of child, and verbal \\
    autopsy cause of death.")
  cli::cli_li("Season definition: This dataset is a \\
    spreadsheet containing rainy and dry season date ranges for each site, \\
    which will be used to classify the season in which each case occurs.")
  cli::cli_end(olid)

  cli::cli_text("")

  cli::cli_alert("Once the files are in place, edit the file \\
    {yaml_path} to provide the file names corresponding to each of these \\
    datasets.", wrap = TRUE)

  cli::cli_text("")
  cli::cli_inform("The config.yaml template looks like this:")
  cli::cat_boxx(
    c(
      "champs_analytics_dataset: ''",
      "maternal_registry_dataset: ''",
      "champs_vocabulary_dataset: ''",
      "dss_dataset: ''",
      "season_dataset: ''"
    ),
    padding = c(0, 2, 0, 2),
    margin = c(0, 4, 0, 0)
  )
  cli::cli_text("")
  cli::cli_inform("So for example, if the CHAMPS Analytics \\
    Dataset you placed in this directory is named \\
    'Analytics_Dataset_2021-09-01.xlsx', you would edit the corresponding \\
    line in config.yaml as follows:", wrap = TRUE)
  cli::cli_text("")
  cli::cat_boxx(
    "champs_analytics_dataset: 'Analytics_Dataset_2021-09-01.xlsx'",
    padding = c(0, 2, 0, 2),
    margin = c(0, 4, 0, 0)
  )
  cli::cli_end()
}
