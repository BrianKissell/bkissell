
# abort_bad_argument ------------------------------------------------------

#' Easily create error messages for when arguments do not meet expectations
#'
#' @param arg Argument, which will be a character string containing the name of the parameter.
#' @param must Character string which tells the requirement, such as 'be numeric'
#' @param not Argument variable, which will be used to tell if the parameter matches the requirement.
#'
#' @export

abort_bad_argument <- function(arg, must, not = NULL) {

  # Check to ensure that the arg is a character
  if(!is.character(arg)) {
    bkissell::abort_bad_argument(arg = "arg", must = "be character", not = arg)
  }

  # Check to ensure that the must is a character
  if(!is.character(must)) {
    bkissell::abort_bad_argument(arg = "must", must = "be character", not = must)
  }

  # Create the error message
  msg <- glue::glue("`{arg}` must {must}")

  # If you use the not variable
  if (!is.null(not)) {

    # Check the data type of the not
    not <- typeof(not)

    # Create the portion of the error message related to the not.
    msg <- glue::glue("{msg}; not {not}.")

  }

  # Throw the error that contains the updated error message
  rlang::abort(
    "error_bad_argument",
    message = msg,
    arg = arg,
    must = must,
    not = not
  )
}



# adjust_file_path_to_current_machine -------------------------------------

#' Adjust file path to current machine
#' This code converts any path so that it works appropriately for the user. I have previously avoided using projects for file management, for various reasons, but I am moving in that direction, and thus this function may become defunct.
#'
#' @param file_path File path as a string. Usually this is the full path to the file or directory.
#'
#' @return goal_file_path
#' @export
#'
adjust_file_path_to_current_machine <- function(file_path){

  # Check to ensure that the arg is a character
  if(!is.character(file_path)) {
    bkissell::abort_bad_argument(arg = "file_path", must = "be character", not = file_path)
  }

  # file_path <- "C:/Users/Brian/TCM Dropbox/Brian Kissell/04 MDM Neuro-Fundraising Lab/00 Jobs"
  # Obtain the working directory from the user's computer.
  local_working_directory <- getwd()

  # Split the working directory into multiple parts to make it easier to work with.
  parts_of_wd <- local_working_directory |>
    strsplit(.Platform$file.sep) |>
    unlist()

  # Split the file_path into multiple parts to make it easier to work with.
  parts_of_file_path <- {{file_path}} |>
    strsplit(.Platform$file.sep) |>
    unlist()

  # Check if the first element of the working directory is a drive
  if(parts_of_wd[[1]] == parts_of_file_path[[1]] & parts_of_wd[[1]] == parts_of_file_path[[1]]){

    # Switch the computer name
    parts_of_file_path[[3]] <- parts_of_wd[[3]]

    # Recreate the file path
    goal_file_path <- paste0(parts_of_file_path, collapse = "/")
    # goal_file_path <- file.path(parts_of_file_path, fsep= "/")
    return(goal_file_path)

  } else {

    # Otherwise, throw an error
    stop("The program is unable to detect the files needed to adjust the file path.")
  }
}


# combine_file_string_with_time -------------------------------------------

#' Combine the string with the time from the system
#' Function to take in a string, and then add the data and time to it
#'
#' @param file_string file_string
#' @param specifier String that specifies the format of the date. https://www.geeksforgeeks.org/how-to-use-date-formats-in-r/
#'
#' @return combined_string
#' @export
#'
combine_file_string_with_time <- function(
    file_string,
    specifier = "%m%d%Y_%H%M%S"
  ){

  # Check for the Extension
  ext <- tools::file_ext(file_string)

  # Check for extention
  if(ext != "") {

    # Add a period to the extension
    ext <- paste0(".", ext)

    # Isolate it from the string
    file_string <- stringr::str_replace(file_string, ext, "")
  }

  # Create the date time string
  date_time_as_string <- format(Sys.time(), specifier)

  # Combine date time string with the file string
  combined_string <- paste0(file_string, "_", date_time_as_string)

  # If there is an extention, add it back to the string
  if(ext != ""){
    combined_string <- paste0(combined_string, ext)
  }

  # Return the new string
  return(combined_string)
}


# convert_specific_column_names_to_data_type ------------------------------

#' Create a vector of data types based on column names. Built so we can add in as many types as we want.
#'
#' @param column_name_vector Column names vector for the dataframe
#' @param text_names Which column names should be labeled as text?
#' @param numeric_names Which column names should be labeled as numeric?
#' @param skip_names Which column names should be labeled as skip?
#' @param guess_names Which column names should be labeled as guess?
#' @param logical_names Which column names should be labeled as logical?
#' @param date_names Which column names should be labeled as date?
#' @param list_names Which column names should be labeled as list?
#'
#' @return data_types
#' @export
#'
convert_specific_column_names_to_data_type <- function(
    column_name_vector,
    text_names = NULL,
    numeric_names = NULL,
    skip_names = NULL,
    guess_names = NULL,
    logical_names = NULL,
    date_names = NULL,
    list_names = NULL
    ) {

  # Create named list of parameters
  list_of_paramaters <- list(
    "text_names" = text_names,
    "numeric_names" = numeric_names,
    "skip_names" = skip_names,
    "guess_names" = guess_names,
    "logical_names" = logical_names,
    "date_names" = date_names,
    "list_names" = list_names
    )

  # Obtain the possible types as strings
  possible_types <- stringr::str_replace(names(list_of_paramaters), "_names$", "")

  # Initiate vector to iterate on
  column_name_vector_and_data_type <- column_name_vector

  # For all of the type parameters, if a column name matches on in the
  # list of parameters, add the data type to the vector.
  purrr::walk2(list_of_paramaters, possible_types, ~ {
    if(!is.null(.x)){
      column_name_vector_and_data_type <<- ifelse(
        column_name_vector_and_data_type %in% .x,
        {{.y}},
        column_name_vector_and_data_type
      )
    }
  })

  # Return the vector
  return(column_name_vector_and_data_type)
}


#' create_excel_file_paths_df
#'
#' @param data_folder_location data_folder_location
#' @param file_name_part file_name_part
#' @param unique_name_part unique_name_part
#' @param file_ext file_ext
#' @param sheets_to_exclude sheets_to_exclude
#'
#' @return file_paths_df_all_paths
#' @export
#'
create_excel_file_paths_df <- function(
    data_folder_location,
    file_name_part,
    unique_name_part,
    file_ext,
    sheets_to_exclude
) {

  # Provide all of the names to the coding worksheets
  names_of_all_docs <- paste0(
    file_name_part,
    unique_name_part,
    file_ext
  )

  # Create the paths for the coding documents
  excel_file_paths <- file.path(
    data_folder_location,
    names_of_all_docs
  )

  # Create a data frame that contains the links and the sheet names
  file_paths_df_all_paths <-
    bkissell::get_file_paths_from_excel_workbooks_df_all_paths(
      excel_file_paths,
      sheets_to_exclude
    )

  # initial_read_data <- readxl::read_excel(
  #   file_paths_df_all_paths[1, "file_path"],
  #   file_paths_df_all_paths[1, "sheet_name"],
  #   n_max = 1)
  #
  # # Change the variable names to snakecase
  # names(initial_read_data) <- snakecase::to_snake_case(names(initial_read_data))
  #
  # column_types <- bkissell::convert_specific_column_names_to_data_type(
  #   names(initial_read_data), text_names, numeric_names)
  #
  # file_paths_df_all_paths$column_names_list <- rep(list(names(initial_read_data)), nrow(file_paths_df_all_paths))
  # file_paths_df_all_paths$column_types_list <- rep(list(column_types), nrow(file_paths_df_all_paths))
  # file_paths_df_all_paths$multiple_choice_variables_list <- rep(list(multiple_choice_variables), nrow(file_paths_df_all_paths))
  # file_paths_df_all_paths$category_variables_list <- rep(list(category_variables), nrow(file_paths_df_all_paths))
  # file_paths_df_all_paths$text_names_list <- rep(list(text_names), nrow(file_paths_df_all_paths))
  # file_paths_df_all_paths$numeric_names_list <- rep(list(numeric_names), nrow(file_paths_df_all_paths))
  #
  return(file_paths_df_all_paths)
}


### Special Note for Sarah. This has most of the checks done, so you should just need to go through make make sure it is thorough enough.
### When you write the test, you can use the following as the parameter (although you will need to change it to match your file directory)
### path_to_column_workbook <-  "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research Tools/datacollectiontools/dev/misc/Example_1 - Condition_1_column_names.xlsx"
### I will add that we might want to provide additional instructions regarding how the excel file needs to be set up. We thus might want to meet to go over all of that, and then you can make the documentation for that.

#' Create the table that contains the information related to the variable names and column details
#'
#' @param path_to_column_workbook Path to where the column details workbook is located. This needs to be a character string and it needs to point to an existing file.
#' @param name_of_column_details Name of the column details sheet, which is defaulted to "column_details". This only needs to be changed if you do not want to use column details as the sheet name in the column details workbook.
#'
#' @return named_vectors_list
#' @export
#'

create_column_details_and_named_vectors_list <- function(
    path_to_column_workbook,
    name_of_column_details = "column_details"
){

  # Check if path_to_column_workbook is a character string
  if(!is.character(path_to_column_workbook)) {
    abort_bad_argument("path_to_column_workbook", must = "be character", not = path_to_column_workbook)
  }

  # Check if path_to_column_workbook exists
  if(!file.exists(path_to_column_workbook)) {
    rlang::abort(glue::glue("`path_to_column_workbook` does not exist as a file on this system."))
  }

  # Read in the name of the sheets contained in the excel workbook
  named_vectors_workbook_sheets <- readxl::excel_sheets(path_to_column_workbook)

  # Check if there are sheets in the file
  if(!length(named_vectors_workbook_sheets) > 0) {
    rlang::abort(glue::glue("The file located at {path_to_column_workbook} is not formated correctly."))
  }

  # Check if all of the sheets are unique
  if(!length(named_vectors_workbook_sheets) == length(unique(named_vectors_workbook_sheets))) {
    rlang::abort(glue::glue("The file located at {path_to_column_workbook} has sheets that are not unique."))
  }

  # Loop through each sheet
  named_vectors_list <- purrr::map(named_vectors_workbook_sheets, ~{

    # Read in the data for it
    df <- readxl::read_excel(path_to_column_workbook, sheet = .x)

    # Create named vectors for all sheets except for the column_details sheet
    if(.x != {{name_of_column_details}}) {
      named_vector <- df$vector_of_factor_levels
      names(named_vector) <- df$names_of_factor_levels
    } else {
      named_vector <- df
    }

    return(named_vector)

    # Name all sheets so they can be easily accessed
  }, path_to_column_workbook) |> purrr::set_names(named_vectors_workbook_sheets)

  # Return the list with all of this information
  return(named_vectors_list)
}


#' create_path_to_lab_directory_in_dropbox
#'
#' @param drop_box_name Character string that contains the name of the dropbox directory you are using.
#' @param dropbox_path_to_the_directory Character string that contains the rest of the file path - once you are in dropbox
#'
#' @return created_path
#' @export
#'
create_path_to_lab_directory_in_dropbox <- function(drop_box_name = "TCM Dropbox", dropbox_path_to_the_directory = "04 MDM Neuro-Fundraising Lab" ) {

  # Assign the users who are allowed to use the program
  appropriate_user_names <- c("Brian", "benja")

  # Assign the c drive
  c_drive <- "C:/"

  # Assign the name of the users folder
  users <- "Users"

  # Check to see if the c drive exists
  if(file.exists(c_drive)) {

    # Check to see if the user's name is pre-approved
    if(users %in% list.files(c_drive)) {

      # Extract the approved name
      user_names <- purrr::map(appropriate_user_names, ~{
        stringr::str_extract(.x, list.files(file.path(c_drive, users)))
      }) |> unlist()

      # Remove the nas
      user_name <- user_names[!is.na(user_names)]

      # Check to see if the dropbox folder exists
      if(file.exists(file.path(c_drive, users, user_name, drop_box_name))) {

        # Obtain the dropbox usernames
        dropbox_usernames <- list.files(file.path(c_drive, users, user_name, drop_box_name))

        # Remove the additional file
        dropbox_username <- dropbox_usernames[dropbox_usernames != "desktop.ini"]

        # Create the path
        created_path <- file.path(c_drive, users, user_name, drop_box_name, dropbox_username, dropbox_path_to_the_directory)

        # Return this path
        return(created_path)
      } else { rlang::abort("Dropbox username is not found")}
    } else { rlang::abort("User is not in pre-approved list")}
  } else {rlang::abort("There is no c drive on this computer")}
}




#' create_time_chr_string_for_file_names
#'
#' @param format_string format_string
#'
#' @return time_stamp
#' @export
#'
create_time_chr_string_for_file_names <- function(format_string = '%m%d%Y_%H%M') {

  # Obtain the current time and put it into our preferred file format
  time_stamp <- Sys.time() |>

    # and put it into the proper format
    format({{format_string}})

  # Return the time_stamp
  return(time_stamp)
}

### Special Note for Sarah. This has most of the checks done, so you should just need to go through make make sure it is thorough enough.

#' Pull rows from the column details that are indicated as multiple choice.
#'
#' @param column_details_table column_details_table
#' @param question_type_indicator Character string that tells the program what data type is being evaluated and processed.
#'
#' @return column_details_question_type_df
#' @export
#'

extract_column_details_question_type_df <- function(column_details_table, question_type_indicator = "mc") {

  # Check that column details is a data frame
  if(!is.data.frame(column_details_table)) {
    rlang::abort(glue::glue("The column_details_table object is not a data frame."))
  }

  # Check that the indicator is a character string
  if(!is.character(question_type_indicator)) {
    rlang::abort(glue::glue("The question_type_indicator object is not a character string."))
  }

  # Check that indicator is a valid option
  appropriate_choice_indicators <- c("mc", "sa")
  if(!(question_type_indicator %in% appropriate_choice_indicators)) {
    rlang::abort(glue::glue("`question_type_indicator` is '{question_type_indicator}', which is not an appropriate type indicator. Please use one of the following: {paste0(appropriate_choice_indicators, collapse = ", ")}"))
  }

  if(question_type_indicator == "mc") {
    # Filter for multiple choice questions
    column_details_question_type_df <- column_details_table |>
      dplyr::filter(.data[["type"]] == {{question_type_indicator}})
  }

  if(question_type_indicator == "sa") {
    column_details_question_type_df <- column_details_table |>
      dplyr::filter(
        # Find all rows that start with sa
        stringr::str_detect(column_details_table$type, "^sa")
      )
  }

  # Return the df
  return(column_details_question_type_df)
}


#' Find the newest file from a directory
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
    fits_pattern <- file_names |>
      stringr::str_detect(format_pattern)

    # If there is an error, stop the program
    if(any(!fits_pattern)){
      stop("Please ensure the date is in the following format - mmddyyyy_hhmm")
    }

    file_dates <- file_names |>
      stringr::str_extract(format_pattern)

    # Extract all numbers and then convert them to numeric
    file_dates_number <- file_dates |>
      stringr::str_replace_all("[^0-9]", "") |>
      as.numeric()

    # Extract the additional numbers that show it is a duplicate version and then convert them to numeric
    file_dates_number_to_add <- file_names |>
      stringr::str_replace(format_pattern, "") |>
      stringr::str_replace_all("[^0-9]", "") |>
      as.numeric() |>
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


#' Fully read and process the global coding data.
#'
#' @param man_wd man_wd
#' @param Global_Coding_REFERENCE_file_path Global_Coding_REFERENCE_file_path
#' @param Global_Coding_CHANGE_LOG_file_path Global_Coding_CHANGE_LOG_file_path
#' @param file_part__sm_raw_folder file_part__sm_raw_folder
#'
#' @return Global_Coding_REFERENCE
#' @export
#'
FULL_global_coding <- function(
    man_wd = NULL,
    file_part__sm_raw_folder = "Qualitative Coding/Version 2/global_variables/raw_data",
    Global_Coding_REFERENCE_file_path,
    Global_Coding_CHANGE_LOG_file_path
) {

  Global_Coding_REFERENCE_file_path |> file.remove()

  Sys.sleep(5)

  # Read in the data from survey monkey
  survey_monkey_data <- bkissell::read_survey_monkey_data(
    man_wd = man_wd,
    file_part__sm_raw_folder = file_part__sm_raw_folder
  )[[1]]


  # Convert particular NAs to 0s, and other responses to 1s
  survey_monkey_data <- survey_monkey_data |>
    dplyr::mutate(
      dplyr::across(
        c(tidyselect::starts_with("animals__"),
          tidyselect::starts_with("mc_"),
          tidyselect::starts_with("sc_"),
          tidyselect::starts_with("bc_"),
          tidyselect::starts_with("cel_"),
          tidyselect::starts_with("premium__")
        ),
        ~{ifelse(is.na(.x), 0, 1)})
    )

  # Adjust organization of the data
  survey_monkey_data <- survey_monkey_data |>
    dplyr::select(-c("collector_id", "email_address", "first_name", "last_name", "custom_data_1"))

  # Create id so it with a letter so it is read correctly in excel
  survey_monkey_data$respondent_id <- paste0("id_", survey_monkey_data$respondent_id)

  #### Update Global Variables with Editable - Survey Monkey

  # If a manual working directory is provided,
  if(!is.null(man_wd)) {
    # Save current working directory
    current_wd <- getwd()
    # Change the working directory
    setwd(man_wd)
  }

  # Read in the editable file
  Global_Coding_REFERENCE <- survey_monkey_data

  Global_Coding_REFERENCE <- Global_Coding_REFERENCE |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))

  Global_Coding_CHANGE_LOG <- readxl::read_excel(Global_Coding_CHANGE_LOG_file_path, "Change Log")
  Global_Coding_CHANGE_LOG <- dplyr::filter(Global_Coding_CHANGE_LOG, !is.na(.data[["respondent_id"]]))


  purrr::walk(seq_along(Global_Coding_CHANGE_LOG[[1]]), ~{
    CHANGE_LOG_respondent_id <- Global_Coding_CHANGE_LOG[.x,][["respondent_id"]]
    CHANGE_LOG_video_name <- Global_Coding_CHANGE_LOG[.x,][["video_name"]]
    CHANGE_LOG_Column_to_edit <- Global_Coding_CHANGE_LOG[.x,][["column_being_edited"]]
    CHANGE_LOG_correct_value <- Global_Coding_CHANGE_LOG[.x,][["correct_value"]]

    row_to_change <- which(Global_Coding_REFERENCE[["respondent_id"]] == {{CHANGE_LOG_respondent_id}})
    column_to_change <- which(names(Global_Coding_REFERENCE) == {{CHANGE_LOG_Column_to_edit}})
    # print({{CHANGE_LOG_correct_value}})
    # print(Global_Coding_CHANGE_LOG[.x,][["respondent_id"]])
    # print({{row_to_change}})
    # print({{column_to_change}})
    # print(Global_Coding_REFERENCE[row_to_change, column_to_change][[1]])
    Global_Coding_REFERENCE[row_to_change, column_to_change][[1]] <<- as.character({{CHANGE_LOG_correct_value}})
  })

  # Organize the data by end date
  Global_Coding_REFERENCE <- dplyr::arrange(Global_Coding_REFERENCE, .data[["end_date"]])

  # Write all of the files
  writexl::write_xlsx(Global_Coding_REFERENCE, Global_Coding_REFERENCE_file_path)

  # If a manual working directory is provided, reset to original
  if(!is.null(man_wd)) {
    setwd(current_wd)
  }

  # Return the object
  return(Global_Coding_REFERENCE)
}


#' get_file_paths_from_excel_workbooks_df_all_paths
#'
#' @param video_coding_docs_file_paths video_coding_docs_file_paths
#' @param sheets_to_exclude sheets_to_exclude
#'
#' @return file_paths_df_all_paths
#' @export
#'
get_file_paths_from_excel_workbooks_df_all_paths <- function(video_coding_docs_file_paths, sheets_to_exclude) {
  # For every path
  file_paths_df_all_paths <- purrr::map_df(video_coding_docs_file_paths, ~{
    # Obtain the sheet names and create a df for them
    bkissell::get_file_paths_from_excel_workbooks_df(.x, sheets_to_exclude)
  }, sheets_to_exclude)
  # Return the object
  return(file_paths_df_all_paths)
}

#' get_file_paths_from_excel_workbooks_list
#'
#' @param path_to_excel_file path_to_excel_file
#' @param sheets_to_exclude sheets_to_exclude
#'
#' @return file_paths_df
#' @export
#'
get_file_paths_from_excel_workbooks_df <- function(
    path_to_excel_file,
    sheets_to_exclude = NULL
){
  # Obtain the list of sheets
  list_of_sheets <- readxl::excel_sheets(path_to_excel_file)

  # If sheets to exlude has a vector to remove
  if(!is.null(sheets_to_exclude)) {
    # Remove those variables from the list
    list_of_sheets <- list_of_sheets[!(list_of_sheets %in% {{sheets_to_exclude}})]
  }

  # Create a vector with all of the links
  file_path <- rep(path_to_excel_file, length(list_of_sheets))

  # Combine as a data frame
  file_paths_df <- data.frame(file_path = file_path, sheet_name = list_of_sheets)

  # Return the variable
  return(file_paths_df)
}

#' get_file_paths_and_column_data_from_excel_workbooks_list
#'
#' @param video_coding_docs_file_paths video_coding_docs_file_paths
#' @param text_names text_names
#' @param numeric_names numeric_names
#' @param sheets_to_exclude sheets_to_exclude
#'
#' @return file_paths_df_all_paths
#' @export
#'
get_file_paths_and_column_data_from_excel_workbooks_list <- function(
    video_coding_docs_file_paths, text_names, numeric_names, sheets_to_exclude
){

  # Obtain a data frame with the link and the sheet name
  file_paths_df_all_paths <- bkissell::get_file_paths_from_excel_workbooks_df_all_paths(video_coding_docs_file_paths, sheets_to_exclude)

  all_column_names_list <- purrr::map(seq_along(file_paths_df_all_paths[[1]]), ~{
    initial_data <- bkissell::get_initial_read_data_from_excel_workbooks_with_converted_columns(file_paths_df_all_paths[.x, "file_path"], file_paths_df_all_paths[.x, "sheet_name"])
    names(initial_data)
  }, file_paths_df_all_paths)

  file_paths_df_all_paths$column_names_list <- all_column_names_list

  file_paths_df_all_paths$column_types_list <- purrr::map(all_column_names_list, ~{
    convert_specific_column_names_to_data_type(.x, text_names, numeric_names)

  }, text_names, numeric_names)

  # Return the variable
  return(file_paths_df_all_paths)
}


#' get_initial_read_data_from_excel_workbooks
#'
#' @param path_to_excel_file path_to_excel_file
#' @param sheet_name sheet_name
#'
#' @return initial_read_data
#' @export
#'
get_initial_read_data_from_excel_workbooks <- function(
    path_to_excel_file, sheet_name
){

  initial_read_data <- readxl::read_excel(path_to_excel_file, sheet_name) |>
    suppressWarnings()

  # Return the variable
  return(initial_read_data)
}


#' get_initial_read_data_from_excel_workbooks_with_converted_columns
#'
#' @param path_to_excel_file path_to_excel_file
#' @param sheet_name sheet_name
#'
#' @return initial_data
#' @export
#'
get_initial_read_data_from_excel_workbooks_with_converted_columns <- function(
    path_to_excel_file, sheet_name
){

  initial_data <- bkissell::get_initial_read_data_from_excel_workbooks(path_to_excel_file, sheet_name)

  # Change the variable names to snakecase
  names(initial_data) <- snakecase::to_snake_case(names(initial_data))

  # Return object
  return(initial_data)
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


### Special Note for Sarah. This has most of the checks done, so you should just need to go through make make sure it is thorough enough.

#' Extract the table that contains the column_details
#'
#' @param column_workbook_list List created by `create_column_details_and_named_vectors_list`, which has the column names along with all named vectors. This list should only contain the data for 1 column details file.
#' @param name_of_column_details Name of the column details sheet, which is defaulted to "column_details". This only needs to be changed if you do not want to use column details as the sheet name in the column details workbook.
#'
#' @return column_details_table
#' @export
#'

obtain_column_details_table <- function(column_workbook_list, name_of_column_details = "column_details") {

  # Check the column_workbook_list to see if the column details are in it
  if(!is.data.frame(column_workbook_list[[{{name_of_column_details}}]])) {
    rlang::abort(glue::glue("The column details sheet is missing, or there is an issue with the formatting of the workbook."))
  }

  # Extract the column details
  column_details_table <- column_workbook_list[[{{name_of_column_details}}]] |>

    # Filter out any columns with NA for the type
    dplyr::filter(!is.na(.data[["type"]])) |>

    # Convert the strings in type to lower case
    dplyr::mutate(type = stringr::str_to_lower(.data[["type"]]))

  # Return the column details
  return(column_details_table)
}



#' Obtain column name paths for all survey versions
#'
#' @param version_directories version_directories
#'
#' @return version_column_names_paths
#' @export
#'
obtain_column_name_paths_for_all_survey_versions <- function(version_directories) {
  # Create path for the column names workbook
  version_column_names_paths <- version_directories |>
    paste0("/", basename(version_directories), "_column_names.xlsx")

  return(version_column_names_paths)
}


#' Obtain connection to zip files for versions
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

    file_names <- unzip(zipfile = file_location, list = TRUE) |> dplyr::pull(Name)

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


#' obtain_initial_column_names_for_version
#'
#' @param version_column_names_paths version_column_names_paths
#' @param the_column_names_file_exists the_column_names_file_exists
#'
#' @return initial_column_names_for_version
#' @export
#'
obtain_initial_column_names_for_version <- function(version_column_names_paths, the_column_names_file_exists) {
  # Initiate a list for column names
  list_of_column_name_parameters <- list()

  # Loop through all paths, read in data, and grab the names
  for(i in seq_along(version_column_names_paths)) {
    if(the_column_names_file_exists[[i]]){
      #  Create a vector with the column names
      column_names <- version_column_names_paths[[i]] |>
        readxl::read_excel() |>
        dplyr::pull(column_names) |>
        list()

    } else {
      # make the column names NULL
      column_names <- TRUE
    }

    # Add the new list to the initiated list
    list_of_column_name_parameters <- append(list_of_column_name_parameters, column_names)
  }

  # Return the object
  return(list_of_column_name_parameters)
}


#' For every directory, obtain the newest file
#'
#' @param version_directories version_directories
#' @param file_ext file_ext
#' @param format_pattern format_pattern
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




#' Prepare survey version directory paths
#'
#' @param survey_directory_path path to the surveymonkeydata folder
#'
#' @return version_directories
#' @export
#'
prepare_version_directory_paths <- function(survey_directory_path){
  # # Adjust path to be correct no matter what computer is being used
  # survey_data_raw_directory <- bkissell::adjust_file_path_to_current_machine(survey_directory_path)

  # Get the file names that are provided in this directory
  survey_data_raw_directory_filenames <- survey_directory_path |>
    list.files()

  # Test whether each file is a directory or not (This folder should only have
  # directories that represent the different versions of the surveys)
  # One exception is the processed_data folder
  is_project_names <- survey_data_raw_directory_filenames |>
    tools::file_ext() |>
    stringr::str_detect("^$")

  # Create a vector with only the directory names
  version_names <- survey_data_raw_directory_filenames[is_project_names]

  # Remove the processed_data folder from the list of versions
  version_names <- version_names[version_names != "processed_data"]

  # Create paths for each version directory
  version_directories <- survey_directory_path |>
    paste0("/", version_names)

  # Return version directories
  return(version_directories)
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
  # |>
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


#' read_survey_monkey_data
#'
#' @param man_wd man_wd
#' @param file_part__sm_raw_folder file_part__sm_raw_folder
#'
#' @return survey_data_list
#' @export
#'
read_survey_monkey_data <- function(
    man_wd = NULL,
    file_part__sm_raw_folder = "Qualitative Coding/Version 2/global_variables/raw_data"
){

  # If a manual working directory is provided,
  if(!is.null(man_wd)) {
    # Save current working directory
    current_wd <- getwd()
    # Change the working directory
    setwd(man_wd)
  }

  # Create Version Directories
  version_directories <- bkissell::prepare_version_directory_paths({{file_part__sm_raw_folder}})

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

  # Prepare to read in the zip files
  directory_path_for_versions <- dirname(newest_files_for_project)
  file_name <- basename(newest_files_for_project)
  return_wd <- getwd()

  # Temporarily adjust the working directory
  setwd(directory_path_for_versions)

  # Create Connection to zip files
  connection_to_zip_files <- bkissell::obtain_connection_to_zip_files_for_versions(file_name)

  # Read in data stored in the the csv
  survey_data_list <- bkissell::read_csv_in_zip(connection_to_zip_files, initial_column_names_for_version)

  # Change the working directory back
  setwd(return_wd)

  # Stop the program if column files do not exist.
  bkissell::if_no_csv_for_colnames_make_one(
    survey_data_list,
    the_column_names_file_exists,
    version_column_names_paths
  )

  # What are the version names
  version_name <- basename(version_directories)

  # Add the version names to the data frames
  survey_data_list <- purrr::map2(survey_data_list, version_name, ~{
    data_frame <- .x
    data_frame$version_name <- .y
    data_frame
  })

  # Name the dfs with the version names
  survey_data_list <- survey_data_list |> purrr::set_names(version_name)

  # If a manual working directory is provided, reset to original
  if(!is.null(man_wd)) {
    setwd(current_wd)
  }

  # Return the df
  return(survey_data_list)
}














































































#
#
#
# man_wd = "C:/Users/Brian/TCM Dropbox/Brian Kissell/04 MDM Neuro-Fundraising Lab/Research and Development/00 Jobs/2024/003_RD_CodingVideoContent"
# data_folder_location = "Qualitative Coding/Version 2"
# file_name_part = "Video_Coding_V2__"
# coder_names = c("Ben","Brian", "Jill", "Sarah", "Talia")
# ext__initial_name_qual_coding = ".xlsx"
# text_names = c(
#   "video_name", "section", "visual_type", "phone_and_url_present",
#   "type_of_text_on_screen", "story_chapter",
#   "global_variables_have_been_entered", "notes")
# numeric_names = c(
#   "time_point", "direct_eye_contact_with_camera_for_any_animals_or_people",
#   "visual_type", "qr_code_present", "logo_present", "lower_third_present",
#   "credit_card_symbols_present", "trust_indicator_present",
#   "donor_directed_language")
# multiple_choice_variables = c(
#   "visual_type", "phone_and_url_present", "type_of_text_on_screen",
#   "story_chapter", "global_variables_have_been_entered")
# category_variables = c(
#   "direct_eye_contact_with_camera_for_any_animals_or_people",
#   "qr_code_present", "logo_present", "lower_third_present",
#   "credit_card_symbols_present", "trust_indicator_present",
#   "donor_directed_language")
# remove_empty_sheets = TRUE
# other_vars_that_should_not_be_counted = c(
#   "time_point", "section_label", "video_name", "section", "coded_by",
#   "notes", "duration_of_video", "global_variables_have_been_entered__yes",
#   "visual_type", "phone_and_url_present", "type_of_text_on_screen",
#   "story_chapter", "global_variables_have_been_entered", "original_video_name", "global_variables_entered")
# sheets_to_exclude = c("Template","Data Validation")
#
# column_names_details_name = "Version 2_column_names.xlsx"
#























# FULL_qualitative_coding_data(
#   man_wd = NULL,
#   folder_location__qualitative_coding = "Qualitative Coding/Version 2",
#   file_part__initial_name_qual_coding = "Video_Coding_V2__",
#   coder_names = c("Ben","Brian", "Jill", "Sarah", "Talia"),
#   ext__initial_name_qual_coding = ".xlsx",
#   text_names = c(
#     "video_name", "section", "visual_type", "phone_and_url_present",
#     "type_of_text_on_screen", "story_chapter",
#     "global_variables_have_been_entered", "notes"),
#   numeric_names = c(
#     "time_point", "direct_eye_contact_with_camera_for_any_animals_or_people",
#     "visual_type", "qr_code_present", "logo_present", "lower_third_present",
#     "credit_card_symbols_present", "trust_indicator_present",
#     "donor_directed_language"),
#   multiple_choice_variables = c(
#     "visual_type", "phone_and_url_present", "type_of_text_on_screen",
#     "story_chapter", "global_variables_have_been_entered"),
#   category_variables = c(
#     "direct_eye_contact_with_camera_for_any_animals_or_people",
#     "qr_code_present", "logo_present", "lower_third_present",
#     "credit_card_symbols_present", "trust_indicator_present",
#     "donor_directed_language"),
#   remove_empty_sheets = TRUE,
#   other_vars_that_should_not_be_counted = c(
#     "time_point", "section_label", "video_name", "section", "coded_by",
#     "notes", "duration_of_video", "global_variables_have_been_entered__yes",
#     "visual_type", "phone_and_url_present", "type_of_text_on_screen",
#     "story_chapter", "global_variables_have_been_entered")
# )

#
# man_wd = "C:/Users/Brian/TCM Dropbox/Brian Kissell/04 MDM Neuro-Fundraising Lab/Research and Development/00 Jobs/2024/003_RD_CodingVideoContent"
#
#
# global_coding_to_save <- bkissell::FULL_global_coding(
#   man_wd = NULL,
#   file_part__sm_raw_folder = "Qualitative Coding/Version 2/global_variables/raw_data",
#   Global_Coding_REFERENCE_file_path = "C:/Users/Brian/Moore DM Group/MNF Lab R&D Projects - Content Evaluation - Global Coding -/Global_Coding_REFERENCE.xlsx",
#   Global_Coding_CHANGE_LOG_file_path = "C:/Users/Brian/Moore DM Group/MNF Lab R&D Projects - Content Evaluation - Global Coding -/Global_Coding_CHANGE_LOG.xlsx"
# )




