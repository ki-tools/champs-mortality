#' Create a dataset directory with the shell of a config.yaml file
#' @param path path where input data will be stored (must be a non-existing
#' path or an existing directory)
#' @export
#' @importFrom yaml write_yaml
#' @importFrom cli cli_alert_danger cli_alert_info cli_div boxx
#' cli_alert_success cli_ol cli_li cli_end cli_alert cli_text
#' @importFrom glue glue
create_dataset_directory <- function(path) {
  if (!file.exists(path))
    dir.create(path, recursive = TRUE)

  cli::cli_div(theme = list(
    .alert = list("text-exdent" = 2),
    `ol li` = list("text-exdent" = 3)
  ))

  assertthat::assert_that(dir.exists(path),
    msg = cli::format_error("The path provided to \\
      create_dataset_directory() exists but is not a directory"))

  ff <- list.files(path)
  if (length(ff) > 0) {
    cli::cli_alert_info("NOTE: Files already exist in the path provided to \\
      create_dataset_directory()", wrap = TRUE)
    cli::cli_text("")
  }

  has_catchment_lookup <- file.exists(file.path("path",
    "catchment_lookup.csv"))
  catchment_file <- ""
  catchment_str <- ""
  # has_religion_lookup <- file.exists(file.path("path", "religion_lookup.csv"))
  # religion_file <- ""
  # religion_str <- ""
  has_seasons <- file.exists(file.path("path", "seasons.csv"))
  season_file <- ""
  season_str <- ""
  has_live_births <- file.exists(file.path("path", "live_births.csv"))
  live_birth_file <- ""
  live_birth_str <- ""
  live_birth_file <- ""
  live_birth_str <- ""
  has_dhs_data <- file.exists(file.path("path", "dhs.csv"))
  dhs_file <- ""
  dhs_str <- ""

  if (!has_catchment_lookup) {
    file.copy(system.file("datasets/catchment_lookup.csv",
      package = "champsmortality"), path)
    catchment_file <- "catchment_lookup.csv"
    catchment_str <- glue::glue("A dataset with known catchment lookups, \\
      '{path}/catchment_lookup.csv', has been provided. Please update that \\
      file if necessary.")
  }
  # if (!has_religion_lookup) {
  #   file.copy(system.file("datasets/religion_lookup.csv",
  #     package = "champsmortality"), path)
  #   religion_file <- "religion_lookup.csv"
  #   religion_str <- glue::glue("A dataset with known religion lookups, \\
  #     '{path}/religion_lookup.csv', has been provided. Please update that \\
  #     file if necessary.")
  # }
  if (!has_seasons) {
    file.copy(system.file("datasets/seasons.csv",
      package = "champsmortality"), path)
    season_file <- "seasons.csv"
    season_str <- glue::glue("A dataset with known season definitions, \\
      '{path}/seasons.csv', has been provided. Please update that \\
      file if necessary.")
  }
  if (!has_live_births) {
    file.copy(system.file("datasets/live_births.csv",
      package = "champsmortality"), path)
    live_birth_file <- "live_births.csv"
    live_birth_str <- glue::glue("A dataset with known live birth \\
      statistics by site, catchment, and year, '{path}/live_births.csv', \\
      has been provided. Please update that file if necessary.")
  }
  if (!has_dhs_data) {
    file.copy(system.file("datasets/dhs.csv",
      package = "champsmortality"), path)
    dhs_file <- "dhs.csv"
    dhs_str <- glue::glue("A dataset with known DHS \\
      statistics by site, catchment, year, and age, '{path}/dhs.csv', \\
      has been provided. Please update that file if necessary.")
  }

  yaml_path <- file.path(path, "config.yaml")
  if (file.exists(yaml_path)) {
    cli::cli_alert_info("NOTE: config.yaml already exists... It will not be \\
      overwritten.", wrap = TRUE)
    cli::cli_text("")
  } else {
    content <- as.list(structure(rep("", length(ds_names)), names = ds_names))
    content$catchment_lookup <- catchment_file
    # content$religion_lookup <- religion_file
    content$season_lookup <- season_file
    content$dhs_dataset <- dhs_file
    content$live_births_dataset <- live_birth_file
    yaml::write_yaml(content, yaml_path)
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
    lookup table for all CHAMPS codes, providing a corresponding 'name' and \\
    'preferred name' for each. This file is accessible from the CHAMPS L2 \\
    dataset from dataverse.")
  cli::cli_li("DSS: This dataset contains counts of cases \\
    from the demographic surveillance system (DSS) corresponding to each \\
    CHAMPS site and catchment area, only for DSS cases that are not in the \\
    CHAMPS data. These counts are broken down by age group, year, location \\
    of death, season of death, maternal education, sex of child, and verbal \\
    autopsy cause of death.")
  cli::cli_li("Season definition: This dataset is a csv file \\
    containing rainy and dry season date ranges for each site, \\
    which will be used to classify the season in which each case occurs. \\
    {season_str}")
  # cli::cli_li("Religion lookup: This dataset is a csv file containing \\
  #   mappings from religion CHAMPS codes to religion categories. \\
  #   {religion_str}")
  cli::cli_li("Catchment lookup: This dataset is a csv file containing \\
    mappings from catchment codes to catchment names, used to link the \\
    DSS data, which uses catchment names, to the CHAMPS analysis dataset, \\
    which uses catchment IDs. {catchment_str}")
  cli::cli_li("Live births: This dataset is a csv file containing \\
    yearly live births by site and catchment from DSS. {live_birth_str}")
  cli::cli_li("Live births: This dataset is a csv file containing \\
    yearly DHS all-cause mortality data by site and catchment \\
    from DSS. {dhs_str}")
  cli::cli_end(olid)

  cli::cli_text("")

  cli::cli_alert("Once the files are in place, edit the file \\
    {yaml_path} to provide the file names corresponding to each of these \\
    datasets.", wrap = TRUE)

  cli::cli_text("")
  cli::cli_inform("The config.yaml template looks like this:")
  cli::cat_boxx(
    readLines(file.path(path, "config.yaml")),
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
