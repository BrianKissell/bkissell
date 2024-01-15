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
