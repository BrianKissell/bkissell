#' adjust_file_path_to_current_machine
#' This code converts any path so that it works on the current computerfor the user.
#'
#' @param file_path file_path
#'
#' @return goal_file_path
#' @export
#'
adjust_file_path_to_current_machine <- function(file_path){

  # Obtain the working directory from the user's computer.
  local_working_directory <- getwd()

  # Split the working directory into multiple parts to make it easier to work with.
  parts_of_wd <- local_working_directory %>%
    strsplit(.Platform$file.sep) %>%
    unlist()

  # Split the file_path into multiple parts to make it easier to work with.
  parts_of_file_path <- {{file_path}} %>%
    strsplit(.Platform$file.sep) %>%
    unlist()

  # Check if the first element of the working directory is a drive
  if(parts_of_wd[[1]] == parts_of_file_path[[1]] & parts_of_wd[[1]] == parts_of_file_path[[1]]){

    # Switch the computer name
    parts_of_file_path[[3]] <- parts_of_wd[[3]]

    # Recreate the file path
    goal_file_path <- paste0(parts_of_file_path, collapse = "/")
    # goal_file_path <- file.path(parts_of_file_path, fsep= "/")
    print(goal_file_path)
    return(goal_file_path)

  } else {

    # Otherwise, throw an error
    stop("The program is unable to detect the files needed to adjust the file path.")
  }
}


#' prepare_version_directory_paths
#'
#' @param survey_directory_path path to the surveymonkeydata folder
#'
#' @return version_directories
#' @export
#'
prepare_version_directory_paths <- function(survey_directory_path){
  # Adjust path to be correct no matter what computer is being used
  survey_data_raw_directory <- bkissell::adjust_file_path_to_current_machine(survey_directory_path)

  # Get the file names that are provided in this directory
  survey_data_raw_directory_filenames <- survey_data_raw_directory %>%
    list.files()

  # Test whether each file is a directory or not (This folder should only have
  # directories that represent the different versions of the surveys)
  # One exception is the processed_data folder
  is_project_names <- survey_data_raw_directory_filenames %>%
    tools::file_ext() %>%
    stringr::str_detect("^$")

  # Create a vector with only the directory names
  version_names <- survey_data_raw_directory_filenames[is_project_names]

  # Remove the processed_data folder from the list of versions
  version_names <- version_names[version_names != "processed_data"]

  # Create paths for each version directory
  version_directories <- survey_data_raw_directory %>%
    paste0("/", version_names)

  # Return version directories
  return(version_directories)
}

#' FILES_find_newest_file
#' Function to take a path to a directory, and then it provides the most recent version of the file
#'
#' @param directory directory
#' @param file_type file_type
#' @param format_pattern format_pattern
#' @param based_on_modiication_date based_on_modiication_date
#'
#' @return newest_file
#' @export
#'
FILES_find_newest_file <- function(directory, file_type = ".csv", format_pattern = "_[0-9]{8}_[0-9]{4}", based_on_modiication_date = TRUE){

  # If indicated in the arguments, use the modification date to select the newest file
  if(based_on_modiication_date == TRUE) {

    # Obtain the information on the files
    df <- file.info(list.files(directory, full.names = T, pattern = file_type))

    if(nrow(df) == 0) {
      stop(glue::glue("No files were found at {directory}."))
    }

    # Obtain the newest file
    newest_file <- rownames(df)[which.max(df$mtime)]

  } else {
    # Get a list of the files in the directory
    file_names <- list.files(path = directory, pattern = file_type)

    # Create paths to the files
    file_paths <- paste0(directory, "/", file_names)

    # Check that the number format matches the number
    fits_pattern <- file_names %>%
      stringr::str_detect(format_pattern)

    # If there is an error, stop the program
    if(any(!fits_pattern)){
      stop("Please ensure the date is in the following format - mmddyyyy_hhmm")
    }

    file_dates <- file_names %>%
      stringr::str_extract(format_pattern)

    # Extract all numbers and then convert them to numeric
    file_dates_number <- file_dates %>%
      stringr::str_replace_all("[^0-9]", "") %>%
      as.numeric()

    # Extract the additional numbers that show it is a duplicate version and then convert them to numeric
    file_dates_number_to_add <- file_names %>%
      stringr::str_replace(format_pattern, "") %>%
      stringr::str_replace_all("[^0-9]", "") %>%
      as.numeric() %>%
      tidyr::replace_na(0)

    # Add the above numbers to prioritize the files
    file_number <- file_dates_number + file_dates_number_to_add

    # Find the index for the file with the highest number
    newest_file_index <- which(file_number == max(file_number))

    # Extract that file name from the list
    newest_file <- file_paths[newest_file_index]
  }

  # Return the newest file
  return(newest_file)
}



#' obtain_newest_file_per_directory
#'
#' @param version_directories version_directories
#'
#' @return newest_files_for_project
#' @export
#'
obtain_newest_file_per_directory <- function(version_directories, file_ext = ".zip", format_pattern = "_[0-9]{8}_[0-9]{4}") {
  # These directories will have all downloaded copies of the data
  # They will have the date and time in the file name, and should be zip files.
  # Naming structure is "{Org Initials}_{mmddyyyy}_{hhmm}.zip"
  # The following will obtain the most recent copy of the data
  newest_files_for_project <- purrr::map_chr(
    version_directories, ~ {
      bkissell::FILES_find_newest_file(
        .x,
        file_type = file_ext,
        format_pattern = format_pattern
      )
    }
  )

  # Return path
  return(newest_files_for_project)
}

#' obtain_column_name_paths_for_all_survey_versions
#'
#' @param version_directories version_directories
#'
#' @return version_column_names_paths
#' @export
#'
obtain_column_name_paths_for_all_survey_versions <- function(version_directories) {
  # Create path for the column names workbook
  version_column_names_paths <- version_directories %>%
    paste0("/", basename(version_directories), "_column_names.xlsx")

  return(version_column_names_paths)
}



#' obtain_initial_column_names_for_version
#'
#' @param version_column_names_paths version_column_names_paths
#' @param the_column_names_file_exists the_column_names_file_exists
#'
#' @return initial_column_names_for_version
#' @export
#'
obtain_initial_column_names_for_version <- function(version_column_names_paths, the_column_names_file_exists) {

  list_of_column_name_parameters <- list()

  for(i in seq_along(version_column_names_paths)) {

    if(the_column_names_file_exists[[i]]){
      #  Create a vector with the column names
      column_names <- version_column_names_paths[[i]] %>%
        readxl::read_excel() %>%
        dplyr::pull(column_names) %>%
        list()

    } else {
      # make the column names NULL
      column_names <- TRUE
    }

    list_of_column_name_parameters <- append(list_of_column_name_parameters, column_names)
  }

  return(list_of_column_name_parameters)
}

#' obtain_connection_to_zip_files_for_versions
#'
#' @param newest_files_for_project newest_files_for_project
#'
#' @return connection_to_zip_files
#' @export
#'
obtain_connection_to_zip_files_for_versions <- function(newest_files_for_project) {
  # Get the name of the file from the zip file
  path_inside_zip <- purrr::map_chr(newest_files_for_project, ~{
    file_location <- .x

    file_names <- unzip(zipfile = file_location, list = TRUE) %>% dplyr::pull(Name)

    do_not_keep <- stringr::str_detect(file_names, "PageOrder")

    the_path_inside_zip <- file_names[!do_not_keep]

    the_path_inside_zip
  })

  # Create the connection to the zip files
  connection_to_zip_files <-  purrr::map2(
    newest_files_for_project,
    path_inside_zip,
    ~ unz(.x, .y)
  )

  return(connection_to_zip_files)
}


#' read_csv_in_zip
#'
#' @param connection_to_zip_files connection_to_zip_files
#' @param initial_column_names_for_version initial_column_names_for_version
#'
#' @return survey_data
#' @export
#'
read_csv_in_zip <- function(connection_to_zip_files, initial_column_names_for_version) {

  list_of_file_information <- list(connection_to_zip_files, initial_column_names_for_version)
  # %>%
  #   purrr::flatten()

  # Read in the data
  survey_data <- purrr::pmap(list_of_file_information,  ~ {

    df <- readr::read_csv(..1, col_names = ..2, show_col_types = FALSE)

    if(length(..2) == 1) {

      df <- dplyr::slice(df, -1)

    } else if(length(..2) > 1){

      df <- dplyr::slice(df, -1:-2)

    }

    df

  })

  return(survey_data)
}

#' if_no_csv_for_colnames_make_one
#'
#' @param survey_data_list survey_data_list
#' @param the_column_names_file_exists the_column_names_file_exists
#' @param version_column_names_paths version_column_names_paths
#'
#' @export
#'
if_no_csv_for_colnames_make_one <- function(
    survey_data_list,
    the_column_names_file_exists,
    version_column_names_paths
) {
  list_of_parameters <- list(survey_data_list, the_column_names_file_exists, version_column_names_paths)

  for(i in seq_along(survey_data_list)) {
    if(the_column_names_file_exists[[i]] == FALSE){
      column_names <- colnames(survey_data_list[[i]])
      df_column_names <- data.frame("column_names" = column_names)
      df_column_names$type <- NA
      df_column_names$who <- NA
      df_column_names$question <- NA
      df_column_names$sa_label <- NA
      df_column_names$len_char <- NA
      df_column_names$warn_len <- NA

      writexl::write_xlsx(df_column_names, version_column_names_paths[[i]])

      warning(glue::glue("There was not a column names excel file, so We have created one. It is at {version_column_names_paths[[i]]}. Please go to that file, and add the correct names."))

    }
  }
}


#' read_survey_data
#'
#' @param survey_monkey_raw_data_path
#'
#' @return survey_data_list
#' @export
#'
read_survey_data <- function(survey_monkey_raw_data_path){
  # Create Version Directories
  version_directories <- bkissell::prepare_version_directory_paths(survey_monkey_raw_data_path)

  # Obtain the newest version from each directory
  newest_files_for_project <- bkissell::obtain_newest_file_per_directory(version_directories)

  # Obtain the paths for the column names excel files
  version_column_names_paths <- bkissell::obtain_column_name_paths_for_all_survey_versions(version_directories)

  # Check if the column names excel file exists for each version
  the_column_names_file_exists <- file.exists(version_column_names_paths)

  # Obtain appropriate initial column names
  initial_column_names_for_version <- bkissell::obtain_initial_column_names_for_version(
    version_column_names_paths,
    the_column_names_file_exists
  )

  directory_path_for_versions <- dirname(newest_files_for_project)
  file_name <- basename(newest_files_for_project)
  current_wd <- getwd()
  setwd(directory_path_for_versions)

  # Create Connection to zip files
  connection_to_zip_files <- bkissell::obtain_connection_to_zip_files_for_versions(file_name)

  # Read in data stored in the the csv
  survey_data_list <- bkissell::read_csv_in_zip(connection_to_zip_files, initial_column_names_for_version)

  setwd(current_wd)

  # Stop the program if column files do not exist.
  bkissell::if_no_csv_for_colnames_make_one(
    survey_data_list,
    the_column_names_file_exists,
    version_column_names_paths
  )

  version_name <- basename(version_directories)

  survey_data_list <- purrr::map2(survey_data_list, version_name, ~{
    data_frame <- .x
    data_frame$version_name <- .y
    data_frame
  })

  # survey_data_list <- purrr::map(survey_data_list, ~{
  #   df <- .x
  #
  #   df$RID
  #   #
  #   # df$RID <- df$respondent_id
  #   #
  #   # return(df)
  # })

  survey_data_list <- survey_data_list %>% purrr::set_names(version_name)
  # # If you indicate that the columns of the versions must match, then it throws an error if they do not, otherwise it combines the data.
  # # If you indicate that the columns do not need to match, it just combines them and labels the missing columns as NA.
  # survey_data <- process_version_columns_must_match(version_columns_must_match = version_columns_must_match, df_list = survey_data_list)
  #
  # survey_data$version_names <- version_names
  #
  # if(is.null(survey_data$RID)) {
  #   survey_data$RID <- survey_data$respondent_id
  # }

  # Return the df
  return(survey_data_list)
}
