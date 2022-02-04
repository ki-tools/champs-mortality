#' Create a dataset directory with the shell of a config.yaml file
#' @param path path where input data will be stored (must be a non-existing
#' path or an existing directory)
#' @export
#' @importFrom yaml write_yaml
create_dataset_directory <- function(path) {
  if (!file.exists(path))
    dir.create(path, recursive = TRUE)

  assertthat::assert_that(dir.exists(path),
    msg = msg_wrap("The path provided to create_dataset_directory() ",
      "exists but is not a directory"))

  ff <- list.files(path)
  if (length(ff) > 0)
    msg_wrap("NOTE: Files already exist in the path provided to ",
      "create_dataset_directory()")

  yaml_path <- file.path(path, "config.yaml")
  if (file.exists(yaml_path)) {
    msg_wrap("NOTE: config.yaml already exists... It will not be overwritten.")
  } else {
    content <- structure(rep("", length(ds_names)), names = ds_names)
    yaml::write_yaml(as.list(content), yaml_path)
  }

  message(msg_wrap(glue::glue("The directory '{path}' is ready for the \\
    appropriate data files to be placed in it. The following datasets \\
    should be placed in this directory:")))
  message("")
  message(msg_wrap(glue::glue("1. CHAMPS Analytics Dataset: This dataset is \\
    available as a downloadable file from LabKey and is continuously \\
    updated. It contains most of the CHAMPS variables that are needed for \\
    the analysis."),
    exdent = 5, blank = FALSE))
  message(msg_wrap(glue::glue("2. Maternal Registry Forms table: This \\
    dataset is also available as a downloadable file from LabKey and \\
    contains information about maternal age and education."),
    exdent = 5, blank = FALSE))
  message(msg_wrap(glue::glue("3. CHAMPS vocabulary: This dataset provides a \\
    lookup table for all CHAMPS codes, providing a correspond 'name' and \\
    'preferred name' for each. This file is accessible from the CHAMPS L2 \\
    dataset from dataverse."),
    exdent = 5, blank = FALSE))
  message(msg_wrap(glue::glue("4. DSS: This dataset contains counts of cases \\
    from the demographic surveillance system (DSS) corresponding to each \\
    CHAMPS site and catchment area, only for DSS cases that are not in the \\
    CHAMPS data. These counts are broken down by age group, year, location \\
    of death, season of death, maternal education, sex of child, and verbal \\
    autopsy cause of death."),
    exdent = 5, blank = FALSE))
  message(msg_wrap(glue::glue("5. Season definition: This dataset is a \\
    spreadsheet containing rainy and dry season date ranges for each site, \\
    which will be used to classify the season in which each case occurs."),
    exdent = 5, blank = FALSE))

  message("")

  message(msg_wrap(glue::glue("Once the files are in place, edit the file \\
    {yaml_path} to provide the file names corresponding to each of these \\
    datasets.")))

  message(glue::glue("The config.yaml template looks like this:"))
  message("
champs_analytics_dataset: ''
maternal_registry_dataset: ''
champs_vocabulary_dataset: ''
dss_dataset: ''
season_dataset: ''
")
  message("")
  message(msg_wrap(glue::glue("So for example, if the CHAMPS Analytics \\
    Dataset you placed in this directory is named \\
    'Analytics_Dataset_2021-09-01.xlsx', you would edit the corresponding \\
    line in config.yaml as follows:")))
  message("")
  message("champs_analytics_dataset: 'Analytics_Dataset_2021-09-01.xlsx'")
}
