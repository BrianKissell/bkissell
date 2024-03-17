
# A -----------------------------------------------------------------------

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


#' Adjust Order for Other Labels
#'
#' @param table table
#' @param grouping_var_levels_included grouping_var_levels_included
#' @param response_var_levels_included response_var_levels_included
#'
#' @return table
#' @export
#'
adjust_order_for_other_labels <- function(table, grouping_var_levels_included = TRUE, response_var_levels_included = TRUE) {

  # Run Checks
  if(!is.data.frame(table)) abort_bad_argument("table", must = "be data.frame", not = table)
  if(!is.logical(grouping_var_levels_included)) abort_bad_argument("grouping_var_levels_included", must = "be logical", not = grouping_var_levels_included)
  if(!is.logical(response_var_levels_included)) abort_bad_argument("response_var_levels_included", must = "be logical", not = response_var_levels_included)

  if(grouping_var_levels_included == TRUE) {
    table$grouping_var_order <- table$grouping_var_order + (table$grouping_var_used_order * 100)

    table$grouping_var_order <- ifelse(
      table$grouping_var_levels == "Other",
      max(table$grouping_var_order) + 1,
      table$grouping_var_order
    )

    table <- table %>%
      dplyr::group_by(grouping_var_levels) %>%
      mutate(grouping_var_order = max(grouping_var_order))
  }

  if(response_var_levels_included == TRUE) {
    table$response_var_order <- table$response_var_order + (table$response_var_used_order * 100)

    table$response_var_order <- ifelse(
      table$response_var_levels == "Other",
      max(table$response_var_order) + 1,
      table$response_var_order
    )

    table <- table %>%
      dplyr::group_by(response_var_levels) %>%
      mutate(response_var_order = max(response_var_order))
  }

  return(table)
}

# B -----------------------------------------------------------------------


# C -----------------------------------------------------------------------

# function to process the net_promoter data POWERBI
#' capwcw_process_net_promoter
#'
#' @param data data
#' @param column_details column_details
#'
#' @return data
#' @export
#'
capwcw_process_net_promoter <- function(data, column_details) {

  # See if net promoter should be included
  net_promoter_included = "net_promoter" %in% column_details$column_names

  if(net_promoter_included == TRUE) {
    # Get the column details for net promoter
    net_promoter_df <- column_details %>% dplyr::filter(column_names == "net_promoter")
    net_promoter_name <- net_promoter_df$column_names

    # Prep categorical net promoter
    data$NUM_net_promoter <- as.numeric(data$net_promoter)

    data$net_promoter <- dplyr::case_when(
      data$NUM_net_promoter %in% 0:6 ~ "Detractors",
      data$NUM_net_promoter %in% 7:8 ~ "Passives",
      data$NUM_net_promoter %in% 9:10 ~ "Promoter"
    )

    # Prep numeric net_promoter_coded
    data$net_promoter_coded <- dplyr::case_when(
      data$net_promoter == "Detractors" ~ -1,
      data$net_promoter == "Passives" ~ 0,
      data$net_promoter == "Promoter" ~ 1
    )

    # Turn the net_promoter into a factor
    data$net_promoter <- factor(data$net_promoter, c("Detractors", "Passives", "Promoter"), ordered = TRUE)
  }

  # Return the data used
  return(data)
}

#' check_if_all_are_missing_from_row
#'
#' @param df df
#' @param list_of_variable_names list_of_variable_names
#'
#' @return is_all_column_data_missing_for_row
#' @export
#'
check_if_all_are_missing_from_row <- function(df, list_of_variable_names) {
  selected_columns <- df %>% dplyr::select(tidyselect::all_of(list_of_variable_names))

  is_all_column_data_missing_for_row <- purrr::map_lgl(seq_along(df[[1]]), ~{
    selected_columns[.x,] |> purrr::every(is.na)
  })

  return(is_all_column_data_missing_for_row)
}


#  Deprecated as I re-wrote the code to do this. survey_data_for_power_bi_single
#' Clean the survey data
#'
#' @param data data frame with the survey data
#' @param path_to_column_workbook Path to where the column details workbook is located.
#' @param likert_indicator character string that is used at the start of each likert variable name.
#' @param name_of_column_details column details df
#'
#' @return data_used
#' @export
#'
clean_and_process_with_column_workbook_list <- function(
    data,
    path_to_column_workbook,
    likert_indicator = NULL,
    name_of_column_details = "column_details") {

  # Run Checks
  if(!is.data.frame(data)) abort_bad_argument("data", must = "be data.frame", not = data)
  if(!is.character(path_to_column_workbook)) abort_bad_argument("path_to_column_workbook", must = "be character", not = path_to_column_workbook)
  if(!is.null(likert_indicator)){
    if(!is.character(likert_indicator)) abort_bad_argument("likert_indicator", must = "be character", not = likert_indicator)
  }
  if(!is.character(name_of_column_details)) abort_bad_argument("name_of_column_details", must = "be character", not = name_of_column_details)

  # Prep the data
  data_used <- data

  # Create the column details and named vectors list
  column_workbook_list <- create_column_details_and_named_vectors_list(path_to_column_workbook = path_to_column_workbook, name_of_column_details = name_of_column_details)

  # Create the df that just contains the column details
  column_details <- obtain_column_details_table(column_workbook_list = column_workbook_list, name_of_column_details = name_of_column_details)

  # function to process the net_promoter data
  capwcw_process_net_promoter <- function(data_used, column_details) {

    if(!is.data.frame(data_used)) abort_bad_argument("data_used", must = "be data.frame", not = data_used)
    if(!is.data.frame(column_details)) abort_bad_argument("column_details", must = "be data.frame", not = column_details)

    # See if net promoter should be included
    net_promoter_included = "net_promoter" %in% column_details$column_names

    if(net_promoter_included == TRUE) {
      # Get the column details for net promoter
      net_promoter_df <- column_details %>% dplyr::filter(column_names == "net_promoter")
      net_promoter_name <- net_promoter_df$column_names

      # Prep categorical net promoter
      data_used$NUM_net_promoter <- as.numeric(data_used$net_promoter)
      data_used$net_promoter <- dplyr::case_when(
        data_used$NUM_net_promoter %in% 0:6 ~ "Detractors",
        data_used$NUM_net_promoter %in% 7:8 ~ "Passives",
        data_used$NUM_net_promoter %in% 9:10 ~ "Promoter"
      )

      # Prep numeric net_promoter_coded
      data_used$net_promoter_coded <- dplyr::case_when(
        data_used$net_promoter == "Detractors" ~ -1,
        data_used$net_promoter == "Passives" ~ 0,
        data_used$net_promoter == "Promoter" ~ 1
      )

      # Turn the net_promoter into a factor
      data_used$net_promoter <- factor(data_used$net_promoter, c("Detractors", "Passives", "Promoter"), ordered = TRUE)
    }

    # Return the data used
    return(data_used)
  }

  data_used <- capwcw_process_net_promoter(data_used, column_details)

  capwcw_process_multiple_choice <- function(data_used, column_details) {
    # Check that data_used is a data frame
    if(!is.data.frame(data_used)) {
      rlang::abort(glue::glue("The `data_used` object is not a data frame."))
    }

    # Check that column_details is a df
    if(!is.data.frame(column_details)) {
      rlang::abort(glue::glue("The `column_details` object is not a data frame."))
    }

    # Get the names of the mc variable
    multiple_choice_included <- "mc" %in% column_details$type

    # If mc is included
    if(multiple_choice_included == TRUE) {
      # Get the column details for multiple choice
      multiple_choice_df <- extract_column_details_question_type_df(column_details_table = column_details, question_type_indicator = "mc")
      # Process multiple choice
      data_used <- use_column_workbook_to_convert_vars_to_factors(df = data_used, column_details_question_type_df = multiple_choice_df, column_workbook_list = column_workbook_list, question_type_indicator = "mc")
    }

    return(data_used)
  }

  data_used <- capwcw_process_multiple_choice(data_used, column_details)

  capwcw_process_select_all <- function(data_used, column_details) {
    # Check that data_used is a data frame
    if(!is.data.frame(data_used)) {
      rlang::abort(glue::glue("The `data_used` object is not a data frame."))
    }

    # Check that column_details is a df
    if(!is.data.frame(column_details)) {
      rlang::abort(glue::glue("The `column_details` object is not a data frame."))
    }

    # See if sa is included in the study
    select_all_included <- "sa" %in% column_details$type

    if(select_all_included == TRUE) {
      # Get the column details for select all questions
      select_all_df <- extract_column_details_question_type_df(column_details_table = column_details, question_type_indicator = "sa")

      # Process the select all questions
      data_used <- process_select_all_vars(data_used, select_all_df, column_workbook_list)
    }

    return(data_used)
  }

  data_used <- capwcw_process_select_all(data_used, column_details)

  capwcw_process_numeric <- function(data_used, column_details) {
    # Check that data_used is a data frame
    if(!is.data.frame(data_used)) {
      rlang::abort(glue::glue("The `data_used` object is not a data frame."))
    }

    # Check that column_details is a df
    if(!is.data.frame(column_details)) {
      rlang::abort(glue::glue("The `column_details` object is not a data frame."))
    }

    # See if numeric type should be run
    numeric_included <- "numeric" %in% column_details$type | "likert" %in% column_details$type

    if(numeric_included == TRUE) {
      # Process the numeric questions
      data_used <- process_numeric_vars(data_used = data_used, column_details = column_details, likert_indicator = likert_indicator)
    }

    return(data_used)
  }

  data_used <- capwcw_process_numeric(data_used, column_details)

  # Return the data frame
  return(data_used)
}


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

#' convert_age_to_generation
#'
#' @param age_numeric age_numeric
#' @param year_of_data_collection year_of_data_collection
#'
#' @return age_group
#' @export
#'
convert_age_to_generation <- function(age_numeric, year_of_data_collection = NULL) {
  # Make sure age is numeric
  age <- as.numeric(age_numeric)

  # Obtain the current year
  current_year <- lubridate::today() %>% lubridate::year()

  # Get the lowest valid year
  lowest_valid_year <- current_year - 20

  # If Null, use current year
  if(is.null(year_of_data_collection)) {
    year_of_data_collection <- current_year
  }

  # If the year of data collection is valid
  if(year_of_data_collection > lowest_valid_year & year_of_data_collection <= current_year) {

    # Use the year information to create generation labels
    age_group <- dplyr::case_when(
      age >= current_year - 1945 ~ "Seniors Traditional",
      age >= current_year - 1964 & age <= current_year - 1946 ~ "Baby Boomers",
      age >= current_year - 1980 & age <= current_year - 1965 ~ "Gen X",
      age >= current_year - 1996 & age <= current_year - 1981 ~ "Millennials",
      age <= current_year - 1995 ~ "Gen Z"
    ) %>% factor(levels = c("Gen Z", "Millennials", "Gen X", "Baby Boomers", "Seniors Traditional"), ordered = TRUE)
  } else {
    # Otherwise throw an error
    stop("`year_of_data_collection` is not a valid year")
  }

  # Return the age_group object
  return(age_group)
}


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

# POWERBI
#' convert_vars_to_factors_with_workbook
#'
#' @param data data
#' @param column_workbook_list column_workbook_list
#' @param name_of_column_details name_of_column_details
#'
#' @return df_to_use
#' @export
#'
convert_vars_to_factors_with_workbook <- function(data, column_workbook_list, name_of_column_details) {
  df_to_use <- data
  column_details <- column_workbook_list[[name_of_column_details]]

  # Get the names of the mc variable
  multiple_choice_included <- "mc" %in% column_details$type

  # If mc is included
  if(multiple_choice_included == TRUE) {
    # Get the column details for multiple choice
    column_details_question_type_df <- dplyr::filter(column_details, .data[["type"]] == "mc")

    multiple_choice_names <- column_details_question_type_df$column_names
    multiple_choice_label_names <- column_details_question_type_df$label_info

    purrr::walk(seq_along(multiple_choice_names), ~{

      df_to_use[[multiple_choice_names[[.x]]]] <<- factor(
        x =  df_to_use[[multiple_choice_names[[.x]]]],
        levels = column_workbook_list[[multiple_choice_label_names[[.x]]]],
        labels = names(column_workbook_list[[multiple_choice_label_names[[.x]]]]),
        ordered = TRUE
      )
    })

    return(df_to_use)
  }
}







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



#' Create the table that contains the information related to the variable names and column details
#'
#' @param path_to_column_workbook Path to where the column details workbook is located. This needs to be a character string and it needs to point to an existing file.
#' @param name_of_column_details Name of the column details sheet, which is defaulted to "column_details". This only needs to be changed if you do not want to use column details as the sheet name in the column details workbook.
#'
#' @return named_vectors_list
#' @export
#'
# create_column_details_and_named_vectors_list <- function(path_to_column_workbook, name_of_column_details = "column_details"){
#   # Check if path_to_column_workbook is a character string
#   if(!is.character(path_to_column_workbook)) {
#     # rlang::abort(glue::glue("`path_to_column_workbook` must be a character string, but it is currently a {class(path_to_column_workbook)}")
#     abort_bad_argument("path_to_column_workbook", must = "be character", not = path_to_column_workbook)
#   }
#
#   # Check if path_to_column_workbook exists
#   if(!file.exists(path_to_column_workbook)) {
#     rlang::abort(glue::glue("`path_to_column_workbook` does not exist as a file on this system."))
#   }
#
#   # Read in the name of the sheets contained in the excel workbook
#   named_vectors_workbook_sheets <- readxl::excel_sheets(path_to_column_workbook)
#
#   # Check if there are sheets in the file
#   if(!length(named_vectors_workbook_sheets) > 0) {
#     rlang::abort(glue::glue("The file located at {path_to_column_workbook} is not formated correctly."))
#   }
#
#   # Check if all of the sheets are unique
#   if(!length(named_vectors_workbook_sheets) == length(unique(named_vectors_workbook_sheets))) {
#     rlang::abort(glue::glue("The file located at {path_to_column_workbook} has sheets that are not unique."))
#   }
#
#   # Loop through each sheet
#   named_vectors_list <- purrr::map(named_vectors_workbook_sheets, ~{
#
#     # Read in the data for it
#     df <- readxl::read_excel(path_to_column_workbook, sheet = .x)
#
#     # Create named vectors for all sheets except for the column_details sheet
#     if(.x != {{name_of_column_details}}) {
#       named_vector <- df$vector_of_factor_levels
#       names(named_vector) <- df$names_of_factor_levels
#     } else {
#       named_vector <- df
#     }
#
#     return(named_vector)
#
#     # Name all sheets so they can be easily accessed
#   }, path_to_column_workbook) %>% purrr::set_names(named_vectors_workbook_sheets)
#
#   # Return the list with all of this information
#   return(named_vectors_list)
# }





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


#' create_nonexistant_files
#'
#' @param list_of_paths list_of_paths
#' @param list_of_file_types list_of_file_types
#'
#' @export
#'
create_nonexistant_files <- function(list_of_paths, list_of_file_types) {
  # Loop through the list of files
  purrr::walk2(list_of_paths, list_of_file_types, ~{
    # Check if the path is for a directory
    if(.y == "dir") {
      # Check if the directory exists
      dir_exists <- dir.exists(.x)
      # If it does not exist
      if(!dir_exists) {
        # Create it
        dir.create(.x)
        # and send a warning that this occurred
        warning_message <- glue::glue("Directory was created: {.x}")
        warning(warning_message)
      }
      # For all other file type
    } else {
      # Check if the file exists
      path_exists <- file.exists(.x)
      # If it does not exist
      if(!path_exists){
        # Create it
        file.create(.x)
        # and send a warning that this occurred
        warning_message <- glue::glue("File was created: {.x}")
        warning(warning_message)
      }
    }
  })
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

#' Reshape the text data so that it works with power bi
#'
#' @param text_survey_data survey data with only text related columns
#' @param variables_to_include Character vector with names of the variables to keep
#'
#' @return power_bi_text
#' @export
#'
create_power_bi_text_data <- function(text_survey_data, variables_to_include) {
  variables_to_include <- c(variables_to_include, "study_version")
  # Run Checks
  if(!is.data.frame(text_survey_data)) abort_bad_argument("text_survey_data", must = "be data.frame", not = text_survey_data)
  if(!is.character(variables_to_include)) abort_bad_argument("variables_to_include", must = "be character", not = variables_to_include)

  # Shape and filter the textual data
  power_bi_text <- text_survey_data %>%
    tidyr::pivot_longer(
      cols = -c(tidyselect::all_of(variables_to_include)),
      names_to = "response_var_used",
      names_prefix = "text_",
      values_to = "textual_responses"
    ) %>%
    dplyr::filter(!is.na(textual_responses))

  # Remove the "'" as the symbol is entered incorrectly in excel
  power_bi_text$textual_responses <- stringr::str_replace_all(power_bi_text$textual_responses, "'", "")

  power_bi_text$wave <- "Wave 1"
  power_bi_text <- power_bi_text <- power_bi_text %>% dplyr::select(wave, everything())
  # Return this new table
  return(power_bi_text)
}


#' create_power_bi_data_mc
#'
#' @param df df
#' @param single_path_to_column_workbook single_path_to_column_workbook
#' @param grouping_vars grouping_vars
#'
#' @return table
#' @export
#'
create_power_bi_data_mc <- function(
    df,
    single_path_to_column_workbook,
    grouping_vars
) {

  library(progress)

  name_of_column_details = "column_details"

  column_workbook_list <- create_column_details_and_named_vectors_list(path_to_column_workbook = single_path_to_column_workbook, name_of_column_details = name_of_column_details)

  column_details <- obtain_column_details_table(column_workbook_list = column_workbook_list, name_of_column_details = name_of_column_details)

  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    ifelse(
      !(.x %in% column_details$column_names),
      .x,
      column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info))
  })

  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  multiple_choice_df <- extract_column_details_question_type_df(column_details_table = column_details, question_type_indicator = "mc")

  response_var_names <- multiple_choice_df$column_names

  response_var_used_vector_names <- multiple_choice_df$label_info

  response_var_used_order <- factor(response_var_used_vector_names, unique(response_var_used_vector_names)) %>% as.numeric()

  rv_names_and_order_df <- data.frame(
    response_var_names = response_var_names,
    response_var_used_order = response_var_used_order
  )

  gv_names_and_order_df <- data.frame(
    grouping_vars = grouping_vars,
    grouping_var_used_order = grouping_var_used_order
  )

  parameter_df <- expand.grid(response_var_names, grouping_vars)
  colnames(parameter_df) <- c("response_var_names", "grouping_vars")

  parameter_df <- parameter_df %>%
    dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
    dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

  n_iterations <- length(parameter_df$response_var_names)
  pb <- progress::progress_bar$new(total = n_iterations)

  table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{
    t <- create_table_single(
      df = df,
      response_var_name = parameter_df$response_var_names[[.x]],
      grouping_var_name = parameter_df$grouping_vars[[.x]],
      response_var_type = "mc")

    pb$tick()

    t$response_var_used <- parameter_df$response_var_names[[.x]]
    t$response_var_used_order <- parameter_df$response_var_used_order[[.x]]
    t$grouping_var_used <- parameter_df$grouping_vars[[.x]]
    t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[.x]]
    t
  })

  table <- table %>%
    dplyr::filter(!is.na(response_var_levels)) %>%
    dplyr::filter(!is.na(grouping_var_levels))

  table <- fix_levels_and_order_in_table(table = table, var_type = "response_var", column_details = column_details, column_workbook_list = column_workbook_list)
  table <- fix_levels_and_order_in_table(table = table, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

  table$grouping_var_used  <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>% stringr::str_to_title()

  table$response_var_used  <- stringr::str_replace_all(table$response_var_used, "_", " ") %>% stringr::str_to_title()

  table$wave <- "Wave 1"

  table <- table %>% dplyr::select(
    "wave", "grouping_var_levels", "response_var_levels", "grouping_var_order",
    "response_var_order", "counts", "group_n", "percentage",
    "grouping_var_used", "grouping_var_used_order", "response_var_used", "response_var_used_order", "overall_sample_size_for_response_var")

  return(table)
}



#' create_survey_related_dir_list
#'
#' @param survey_version_name survey_version_name
#' @param survey_monkey_used survey_monkey_used
#' @param wave_names wave_names
#' @param should_create_nonexistant_dirs should_create_nonexistant_dirs
#'
#' @return invisible(list_of_paths)
#' @export
#'
create_survey_related_dir_list <- function(
    survey_version_name = character(),
    survey_monkey_used = TRUE,
    wave_names = character(),
    should_create_nonexistant_dirs = TRUE
){

  # If Survey Monkey was used
  if(survey_monkey_used){

    # Create all of the needed survey directory paths
    p_path_dc_sm <- "Data Collection/survey_monkey_data"
    p_path_dc_sm_svn <- file.path(p_path_dc_sm, survey_version_name)
    p_path_dc_sm_svn_processed_data <- file.path(p_path_dc_sm_svn, "processed_data")
    p_path_dc_sm_svn_wave_names_no_list_names <- file.path(p_path_dc_sm_svn, wave_names)
    p_path_dc_sm_svn_wave_names_list_names <- paste0("p_path_dc_sm_svn_wave_names_", wave_names)
    p_path_dc_sm_svn_wave_names <- p_path_dc_sm_svn_wave_names_no_list_names
    names(p_path_dc_sm_svn_wave_names) <- p_path_dc_sm_svn_wave_names_list_names
    p_path_a_ri_version_name <- file.path("Analysis", "Respondent Investigation", survey_version_name)
    p_path_a_ri_version_name_power_bi_deck <- file.path(p_path_a_ri_version_name, "power_bi_deck")
    p_path_a_ri_version_name_processed_data <- file.path(p_path_a_ri_version_name, "processed_data")
    p_path_a_ri_version_name_processed_text <- file.path(p_path_a_ri_version_name, "processed_text")

    # Put them together as a list
    list_of_paths <- list(
      "p_path_dc_sm" = p_path_dc_sm,
      "p_path_dc_sm_svn" = p_path_dc_sm_svn,
      "p_path_dc_sm_svn_processed_data" = p_path_dc_sm_svn_processed_data,
      "p_path_a_ri_version_name" = p_path_a_ri_version_name,
      "p_path_a_ri_version_name_power_bi_deck" = p_path_a_ri_version_name_power_bi_deck,
      "p_path_a_ri_version_name_processed_data" = p_path_a_ri_version_name_processed_data,
      "p_path_a_ri_version_name_processed_text" = p_path_a_ri_version_name_processed_text
    ) %>%
      append(p_path_dc_sm_svn_wave_names)

    # Create the path types as dir
    list_of_file_types <- rep("dir", length(list_of_paths)) %>% as.list()

    # If desired
    if(should_create_nonexistant_dirs == TRUE) {

      # Check if they are missing and if so create them.
      create_nonexistant_files(list_of_paths, list_of_file_types)
    }

    # Invisibly return the paths
    return(invisible(list_of_paths))
  }
}


#' Create a single table
#'
#' @param df data frame with the survey data
#' @param response_var_name character with the response var name
#' @param grouping_var_name character with the grouping var name
#' @param response_var_type character string with the type of variable that is being processed
#'
#' @return table
#' @export
#'
create_table_single <- function(df, response_var_name, grouping_var_name, response_var_type){

  # Run Checks
  if(!is.data.frame(df)) abort_bad_argument("df", must = "be data.frame", not = df)
  if(!is.character(response_var_name)) abort_bad_argument("response_var_name", must = "be character", not = response_var_name)
  if(!is.character(grouping_var_name)) abort_bad_argument("grouping_var_name", must = "be character", not = grouping_var_name)
  if(!is.character(response_var_type)) abort_bad_argument("response_var_name", must = "be character", not = response_var_name)

  ## Create Helper Functions
  cts_create_grouping_and_order_var <- function(df, grouping_var_name, order_grouping_var) {
    df <- df %>%
      dplyr::mutate(
        grouping_var_levels = as.character(.data[[{{grouping_var_name}}]]),
        grouping_var_order = .data[[order_grouping_var]]
      )
    return(df)
  }

  cts_create_response_and_order_var <- function(df, response_var_name, order_variable) {
    df <- df %>%
      dplyr::mutate(
        response_var_levels = as.character(.data[[{{response_var_name}}]]),
        response_var_order = .data[[{{order_variable}}]]
      )
    return(df)
  }

  cts_filter_out_NAs_from_df <- function(df, var_name) {
    df <- df %>%
      dplyr::filter(!is.na(.data[[{{var_name}}]]), .data[[{{var_name}}]] != "NA")
    return(df)
  }

  cts_add_group_by <- function(df, var_name) {
    df <- df %>% dplyr::group_by(pick(all_of(c({{var_name}}))), .add = TRUE)
    return(df)
  }

  cts_calculate_counts <- function(df) {
    table <- df %>%
      # Get the counts for the grouping variable by the response variables
      dplyr::summarize(counts = dplyr::n(), .groups = "drop")
    return(table)
  }

  cts_calculate_group_counts_and_percentages <- function(table) {
    table <- table %>%
      dplyr::mutate(
        group_n = sum(counts),
        percentage = (counts / group_n)
      )
    return(table)
  }

  cts_calculate_overall_sample_size <- function(df, response_var_name) {
    df %>% dplyr::filter(!is.na(.data[[{{response_var_name}}]])) %>% nrow()
  }

  find_var_names_that_start_with <- function(df, starts_with) {
    df %>% dplyr::select(
      tidyselect::starts_with({{starts_with}})) %>%
      colnames()
  }

  cts_pivot_longer_count_type <- function(df, response_var_name, count_type) {
    table_counts <- df  %>%
      tidyr::pivot_longer(
        cols = all_of(list_of_variable_names),
        names_to = "response_var_levels",
        values_to = {{count_type}})
    return(table_counts)
  }

  if(response_var_type == "mc") {
    # Create Grouped Table
    df <- df %>% ungroup()
    df <- prepare_VAR_ORDER_variables_single(df, response_var_name, grouping_var_name)
    df <- cts_create_grouping_and_order_var(df, grouping_var_name, order_grouping_var)
    df <- cts_create_response_and_order_var(df, response_var_name, order_variable)
    variable_names_to_group <- c("grouping_var_levels", "response_var_levels", "grouping_var_order", "response_var_order")
    for(i in variable_names_to_group) {df <- cts_filter_out_NAs_from_df(df, var_name = i)}
    for(j in variable_names_to_group) {df <- cts_add_group_by(df, var_name = j)}
    table <- cts_calculate_counts(df)
    table <- cts_add_group_by(table, var_name = "grouping_var_levels")
    table <- cts_calculate_group_counts_and_percentages(table)
    table$grouping_var_used <- {{grouping_var_name}}
    table$response_var_used <- {{response_var_name}}
    table <- table %>% dplyr::ungroup()

    # Create Overall Table
    df <- df %>% ungroup()
    variable_names_to_group_for_all <- c("response_var_levels", "response_var_order")
    for(i in variable_names_to_group_for_all) {df <- cts_filter_out_NAs_from_df(df, var_name = i)}
    for(j in variable_names_to_group_for_all) {df <- cts_add_group_by(df, var_name = j)}
    table_all <- cts_calculate_counts(df)
    table_all <- cts_calculate_group_counts_and_percentages(table_all)
    table_all$grouping_var_used <- "All"
    table_all$response_var_used <- {{response_var_name}}
    table_all <- table_all %>% dplyr::ungroup()
    table_all$grouping_var_levels <- "All"
    table_all$grouping_var_order <- 0

    # join Tables
    table_var_order <- c("grouping_var_levels", "response_var_levels", "grouping_var_order", "response_var_order", "counts", "group_n", "percentage", "grouping_var_used", "response_var_used")
    table <- table %>% dplyr::select(dplyr::all_of(table_var_order))
    table_all <- table_all %>% dplyr::select(dplyr::all_of(table_var_order))
    table <- table %>% rbind(table_all)
    df <- df %>% dplyr::ungroup()
    table$overall_sample_size_for_response_var <- cts_calculate_overall_sample_size(df, response_var_name)
  }

  if(response_var_type == "sa"){


    list_of_variable_names <- find_var_names_that_start_with(df, starts_with = response_var_name)
    df <- prepare_VAR_ORDER_variables_single(df, response_var_name = NULL, grouping_var_name)
    order_grouping_var <- paste0("VAR_ORDER__", {{grouping_var_name}})
    df <- cts_create_grouping_and_order_var(df, grouping_var_name, order_grouping_var)
    df <- df  %>% dplyr::select("grouping_var_levels", "grouping_var_order", all_of(list_of_variable_names))

    table_counts <- cts_pivot_longer_count_type(df, response_var_name, count_type = "counts")
    table_counts <- table_counts %>% dplyr::mutate(counts = as.numeric(counts))
    table_counts <- table_counts %>% dplyr::filter(!is.na(counts))

    variable_names_to_group <- c("grouping_var_levels", "response_var_levels", "grouping_var_order")
    for(i in variable_names_to_group) {table_counts <- cts_filter_out_NAs_from_df(table_counts, var_name = i)}
    for(j in variable_names_to_group) {table_counts <- cts_add_group_by(table_counts, var_name = j)}
    table_counts <- table_counts %>% dplyr::summarize(counts = sum(counts), .groups = "drop")

    table_n <- df %>%
      dplyr::select(grouping_var_levels, grouping_var_order, all_of(list_of_variable_names)) %>%
      tidyr::pivot_longer(
        cols = -c("grouping_var_levels", "grouping_var_order"),
        names_to = "response_var_levels",
        values_to = "group_n"
      ) %>%
      dplyr::mutate(group_n = as.numeric(group_n)) %>%
      dplyr::filter(!is.na(group_n)) %>%
      dplyr::group_by(
        .data[["grouping_var_levels"]],
        .data[["grouping_var_order"]],
        .data[["response_var_levels"]]
      ) %>%
      dplyr::summarize(group_n = dplyr::n(), .groups = "drop")

    table_percent <- table_counts %>%
      dplyr::left_join(table_n, by = c("grouping_var_levels", "response_var_levels", "grouping_var_order")) %>%
      dplyr::mutate(percentage = counts / group_n)

    table_percent$percentage <- ifelse(table_percent$counts == 0 & table_percent$group_n == 0, 0, table_percent$percentage)

    table_percent$grouping_var_used_order <- NA
    table_percent$response_var_used_order <- NA
    table_percent$grouping_var_used = {{grouping_var_name}}
    table_percent$grouping_var_levels <- as.character(table_percent$grouping_var_levels)

    table_percent$response_var_used = {{response_var_name}}
    table_percent$response_var_order <- NA

    table_percent <- table_percent %>%
      dplyr::select(grouping_var_levels, response_var_levels, grouping_var_order,
                    response_var_order, counts, group_n, percentage,
                    grouping_var_used, grouping_var_used_order, response_var_used
      )

    all_table_count <- df %>%
      dplyr::select(all_of(list_of_variable_names)) %>%
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "response_var_levels",
        values_to = "counts")  %>%
      dplyr::mutate(counts = as.numeric(counts)) %>%
      dplyr::filter(!is.na(counts)) %>%
      dplyr::group_by(
        .data[["response_var_levels"]]) %>%
      dplyr::summarize(counts = sum(counts))

    all_table_n <- df %>%
      dplyr::select(all_of(list_of_variable_names)) %>%
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "response_var_levels",
        values_to = "group_n") %>%
      dplyr::mutate(group_n = as.numeric(group_n)) %>%
      dplyr::filter(!is.na(group_n)) %>%
      dplyr::group_by(
        .data[["response_var_levels"]]) %>%
      dplyr::summarize(group_n = dplyr::n())

    all_table_percent <- all_table_count %>%
      dplyr::left_join(all_table_n, by = c("response_var_levels")) %>%
      dplyr::mutate(percentage = counts / group_n)

    all_table_percent$percentage <- ifelse(all_table_percent$counts == 0 & all_table_percent$group_n == 0, 0, all_table_percent$percentage)

    all_table_percent <- all_table_percent %>%
      dplyr::mutate(grouping_var_levels = "All",
                    grouping_var_order = 0,
                    grouping_var_used = grouping_var_name,
                    grouping_var_used_order = 0)

    all_table_percent$response_var_used = {{response_var_name}}
    all_table_percent$response_var_order <- NA

    all_table_percent <- all_table_percent %>%
      dplyr::select(grouping_var_levels, response_var_levels, grouping_var_order,
                    response_var_order, counts, group_n, percentage,
                    grouping_var_used, grouping_var_used_order, response_var_used
      )

    table <- table_percent %>% rbind(all_table_percent)

    table$overall_sample_size_for_response_var <- all_table_n %>% dplyr::pull(group_n) %>% max()
  }

  if(response_var_type == "numeric") {

    variable_used <- response_var_name

    if(!(is.numeric(df[[variable_used]]))){
      variable_used <- paste0("NUM__", variable_used)
    }

    # Rename the variable as all
    df$all <- df[[{{variable_used}}]]

    # Obtain the order for all
    df <- df %>%
      mutate(dplyr::across(tidyselect::all_of(grouping_var_name), as.factor)) %>%
      mutate(dplyr::across(tidyselect::all_of(grouping_var_name), as.numeric, .names = "VAR_ORDER__{.col}"))

    # Create the name of the grouping_variable's order var
    order_grouping_var <- paste0("VAR_ORDER__", {{grouping_var_name}})

    table <- df %>%
      # Rename these variables so that we can combine everything into the same column
      dplyr::mutate(
        grouping_var_levels = as.character(.data[[{{grouping_var_name}}]]),
        grouping_var_order = .data[[order_grouping_var]]) %>%
      # Group by these variables
      dplyr::group_by(
        grouping_var_levels,
        grouping_var_order) %>%

      dplyr::filter(!is.na(.data[[{{variable_used}}]])) %>%
      # Get the counts for the grouping variable by the response variables
      dplyr::summarize(
        mean = mean(.data[[{{variable_used}}]], na.rm = TRUE),
        sd = sd(.data[[{{variable_used}}]], na.rm = TRUE),
        n = dplyr::n(),
        se = sd/sqrt(n),
        ci_limit = se * 1.96,
        ci_upper = mean + ci_limit,
        ci_lower = mean - ci_limit, .groups = "drop")

    # Get the order for the grouping variables
    table$grouping_var_used <- {{grouping_var_name}}

    table$grouping_var_used_order <- NA

    table <- table %>%
      dplyr::select(
        grouping_var_levels, grouping_var_order,
        mean, sd, n, se, ci_limit, ci_upper, ci_lower,
        grouping_var_used,
        grouping_var_used_order
      )

    table_all <- df %>%
      dplyr::mutate(
        grouping_var_levels = "All",
        grouping_var_order = 0) %>%
      # Group by these variables
      dplyr::group_by(
        grouping_var_levels,
        grouping_var_order) %>%
      dplyr::filter(!is.na(.data[[{{variable_used}}]])) %>%
      # Calculate the counts
      dplyr::summarize(
        mean = mean(.data[[{{variable_used}}]], na.rm = TRUE),
        sd = sd(.data[[{{variable_used}}]], na.rm = TRUE),
        n = dplyr::n(),
        se = sd/sqrt(n),
        ci_limit = se * 1.96,
        ci_upper = mean + ci_limit,
        ci_lower = mean - ci_limit, .groups = "drop")

    table_all$grouping_var_used <- "All"
    table_all$grouping_var_levels <- "All"
    table_all$grouping_var_order <- 0
    table_all$grouping_var_used_order <- 0

    # Order the variables
    table_all <- table_all %>%
      dplyr::select(
        grouping_var_levels, grouping_var_order,
        mean, sd, n, se, ci_limit, ci_upper, ci_lower,
        grouping_var_used,
        grouping_var_used_order
      )

    # Combine the two data frames
    table <- table %>% rbind(table_all)

    table$overall_sample_size_for_response_var <- table_all %>% dplyr::pull(n) %>% max()
  }

  if(response_var_type == "net_promoter") {

    # Obtain the order for all
    df$VAR_ORDER__all <- as.numeric(df[[{{"net_promoter"}}]])

    df <- df %>%
      mutate(dplyr::across(tidyselect::all_of(grouping_var_name), as.factor)) %>%
      mutate(dplyr::across(tidyselect::all_of(grouping_var_name), as.numeric, .names = "VAR_ORDER__{.col}"))

    # Create the name of the grouping_variable's order var
    order_grouping_var <- paste0("VAR_ORDER__", {{grouping_var_name}})

    table <- df %>%
      # Rename these variables so that we can combine everything into the same column
      dplyr::mutate(
        grouping_var_levels = as.character(.data[[{{grouping_var_name}}]]),
        grouping_var_order = .data[[order_grouping_var]]) %>%
      # Group by these variables
      dplyr::group_by(
        grouping_var_levels,
        grouping_var_order) %>%
      dplyr::filter(!is.na(.data[[{{"net_promoter"}}]])) %>%
      # Get the counts for the grouping variable by the response variables
      dplyr::summarize(
        sum = sum(.data[[{{"net_promoter_coded"}}]], na.rm = TRUE),
        n = dplyr::n(),
        average = sum / n,
        NPS = average * 100, .groups = "drop")

    # Get the order for the grouping variables
    table$grouping_var_used <- grouping_var_name

    table$grouping_var_used_order <- NA

    table <- table %>%
      dplyr::select(
        grouping_var_levels, grouping_var_order,
        sum, n, average, NPS,
        grouping_var_used,
        grouping_var_used_order
      )

    table_all <- df %>%
      dplyr::mutate(
        grouping_var_levels = "All",
        grouping_var_order = 0) %>%
      # Group by these variables
      dplyr::group_by(
        grouping_var_levels,
        grouping_var_order) %>%
      dplyr::filter(!is.na(.data[[{{"net_promoter"}}]])) %>%
      # Calculate the counts
      dplyr::summarize(
        sum = sum(.data[[{{"net_promoter_coded"}}]], na.rm = TRUE),
        n = dplyr::n(),
        average = sum / n,
        NPS = average * 100, .groups = "drop")

    table_all$grouping_var_used <- "All"
    table_all$grouping_var_levels <- "All"
    table_all$grouping_var_order <- 0
    table_all$grouping_var_used_order <- 0

    # Order the variables
    table_all <- table_all %>%
      dplyr::select(
        grouping_var_levels, grouping_var_order,
        sum, n, average, NPS,
        grouping_var_used,
        grouping_var_used_order
      )

    # Combine the two data frames
    table <- table %>% rbind(table_all)

    table$overall_sample_size_for_response_var <- table_all %>% dplyr::pull(n) %>% max()
  }

  return(table)
}


#' Create a single table Multiple Choice
#'
#' @param df data frame with the survey data
#' @param response_var_name character with the response var name
#' @param grouping_var_name character with the grouping var name
#' @param response_var_type character string with the type of variable that is being processed
#'
#' @return table
#' @export
#'
create_table_single_mc <- function(df, response_var_name, grouping_var_name){

  # Run Checks
  if(!is.data.frame(df)) abort_bad_argument("df", must = "be data.frame", not = df)
  if(!is.character(response_var_name)) abort_bad_argument("response_var_name", must = "be character", not = response_var_name)
  if(!is.character(grouping_var_name)) abort_bad_argument("grouping_var_name", must = "be character", not = grouping_var_name)

  ## Create Helper Functions
  cts_create_grouping_and_order_var <- function(df, grouping_var_name, order_grouping_var) {
    df <- df %>%
      dplyr::mutate(
        grouping_var_levels = as.character(.data[[{{grouping_var_name}}]]),
        grouping_var_order = .data[[order_grouping_var]]
      )
    return(df)
  }

  cts_create_response_and_order_var <- function(df, response_var_name, order_variable) {
    df <- df %>%
      dplyr::mutate(
        response_var_levels = as.character(.data[[{{response_var_name}}]]),
        response_var_order = .data[[{{order_variable}}]]
      )
    return(df)
  }

  cts_filter_out_NAs_from_df <- function(df, var_name) {
    df <- df %>%
      dplyr::filter(!is.na(.data[[{{var_name}}]]), .data[[{{var_name}}]] != "NA")
    return(df)
  }

  cts_add_group_by <- function(df, var_name) {
    df <- df %>% dplyr::group_by(pick(all_of(c({{var_name}}))), .add = TRUE)
    return(df)
  }

  cts_calculate_counts <- function(df) {
    table <- df %>%
      # Get the counts for the grouping variable by the response variables
      dplyr::summarize(counts = dplyr::n(), .groups = "drop")
    return(table)
  }

  cts_calculate_group_counts_and_percentages <- function(table) {
    table <- table %>%
      dplyr::mutate(
        group_n = sum(counts),
        percentage = (counts / group_n)
      )
    return(table)
  }

  cts_calculate_overall_sample_size <- function(df, response_var_name) {
    df %>% dplyr::filter(!is.na(.data[[{{response_var_name}}]])) %>% nrow()
  }

  find_var_names_that_start_with <- function(df, starts_with) {
    df %>% dplyr::select(
      tidyselect::starts_with({{starts_with}})) %>%
      colnames()
  }

  cts_pivot_longer_count_type <- function(df, response_var_name, count_type) {
    table_counts <- df  %>%
      tidyr::pivot_longer(
        cols = all_of(list_of_variable_names),
        names_to = "response_var_levels",
        values_to = {{count_type}})
    return(table_counts)
  }


  # Make sure the df is not grouped by anything
  df <- df %>% ungroup()

  # Add VAR_ORDER for both grouping and response vars to the df it is working on
  df <- prepare_VAR_ORDER_variables_single(df, response_var_name, grouping_var_name)

  df <- cts_create_grouping_and_order_var(df, grouping_var_name, order_grouping_var)
  df <- cts_create_response_and_order_var(df, response_var_name, order_variable)

  variable_names_to_group <- c("grouping_var_levels", "response_var_levels", "grouping_var_order", "response_var_order")
  for(i in variable_names_to_group) {df <- cts_filter_out_NAs_from_df(df, var_name = i)}
  for(j in variable_names_to_group) {df <- cts_add_group_by(df, var_name = j)}
  table <- cts_calculate_counts(df)
  table <- cts_add_group_by(table, var_name = "grouping_var_levels")
  table <- cts_calculate_group_counts_and_percentages(table)
  table$grouping_var_used <- {{grouping_var_name}}
  table$response_var_used <- {{response_var_name}}
  table <- table %>% dplyr::ungroup()

  # Create Overall Table
  df <- df %>% ungroup()
  variable_names_to_group_for_all <- c("response_var_levels", "response_var_order")
  for(i in variable_names_to_group_for_all) {df <- cts_filter_out_NAs_from_df(df, var_name = i)}
  for(j in variable_names_to_group_for_all) {df <- cts_add_group_by(df, var_name = j)}
  table_all <- cts_calculate_counts(df)
  table_all <- cts_calculate_group_counts_and_percentages(table_all)
  table_all$grouping_var_used <- "All"
  table_all$response_var_used <- {{response_var_name}}
  table_all <- table_all %>% dplyr::ungroup()
  table_all$grouping_var_levels <- "All"
  table_all$grouping_var_order <- 0

  # join Tables
  table_var_order <- c("grouping_var_levels", "response_var_levels", "grouping_var_order", "response_var_order", "counts", "group_n", "percentage", "grouping_var_used", "response_var_used")
  table <- table %>% dplyr::select(dplyr::all_of(table_var_order))
  table_all <- table_all %>% dplyr::select(dplyr::all_of(table_var_order))
  table <- table %>% rbind(table_all)
  df <- df %>% dplyr::ungroup()
  table$overall_sample_size_for_response_var <- cts_calculate_overall_sample_size(df, response_var_name)

  return(table)
}

















#' create_power_bi_data
#'
#' @param df df
#' @param single_path_to_column_workbook single_path_to_column_workbook
#' @param grouping_vars grouping_vars
#' @param response_var_type response_var_type
#' @param qualitative_type qualitative_type
#'
#' @return combined_final_table
#' @export
#'
create_power_bi_data <- function(
    df,
    single_path_to_column_workbook,
    grouping_vars,
    response_var_type,
    qualitative_type = NULL
) {

  name_of_column_details = "column_details"

  column_workbook_list <- create_column_details_and_named_vectors_list(path_to_column_workbook = single_path_to_column_workbook, name_of_column_details = name_of_column_details)

  column_details <- obtain_column_details_table(column_workbook_list = column_workbook_list, name_of_column_details = name_of_column_details)

  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    ifelse(
      !(.x %in% column_details$column_names),
      .x,
      column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info))
  })

  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  if(response_var_type == "mc"){

    multiple_choice_df <- extract_column_details_question_type_df(column_details_table = column_details, question_type_indicator = "mc")

    response_var_names <- multiple_choice_df$column_names

    response_var_used_vector_names <- multiple_choice_df$label_info

    response_var_used_order <- factor(response_var_used_vector_names, unique(response_var_used_vector_names)) %>% as.numeric()

    power_bi_multiple_choice <- run_create_table_with_var_names_and_orders(
      df = df, response_var_names = response_var_names,
      response_var_used_order = response_var_used_order,
      response_var_type = "mc", grouping_vars = grouping_vars,
      grouping_var_used_order = grouping_var_used_order)

    power_bi_multiple_choice <- power_bi_multiple_choice %>%
      dplyr::filter(!is.na(response_var_levels)) %>%
      dplyr::filter(!is.na(grouping_var_levels))

    power_bi_multiple_choice <- fix_levels_and_order_in_table(table = power_bi_multiple_choice, var_type = "response_var", column_details = column_details, column_workbook_list = column_workbook_list)
    power_bi_multiple_choice <- fix_levels_and_order_in_table(table = power_bi_multiple_choice, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

    power_bi_multiple_choice$grouping_var_used  <- stringr::str_replace_all(power_bi_multiple_choice$grouping_var_used, "_", " ") %>% stringr::str_to_title()

    power_bi_multiple_choice$response_var_used  <- stringr::str_replace_all(power_bi_multiple_choice$response_var_used, "_", " ") %>% stringr::str_to_title()

    combined_final_table <- power_bi_multiple_choice
  }

  if(response_var_type == "sa"){

    select_all_df <- extract_column_details_question_type_df(column_details_table = column_details, question_type_indicator = "sa")

    response_var_names <- select_all_df$column_names %>% stringr::str_extract( "^.+__") %>% unique()

    response_var_used_order <- factor(response_var_names, unique(response_var_names)) %>% as.numeric()

    power_bi_select_all <- run_create_table_with_var_names_and_orders(
      df = df, response_var_names = response_var_names,
      response_var_used_order = response_var_used_order,
      response_var_type = "sa", grouping_vars = grouping_vars,
      grouping_var_used_order = grouping_var_used_order)

    label_info_var <- stringr::str_replace({{response_var_names}}, "__$", "")

    combined_named_vector_for_select_all <- purrr::map(label_info_var, ~{
      column_workbook_list[[.x]]
    }) %>% unlist()

    power_bi_select_all$response_var_levels <- factor(power_bi_select_all$response_var_levels, levels = combined_named_vector_for_select_all, labels = names(combined_named_vector_for_select_all))

    power_bi_select_all$response_var_order <- as.numeric(power_bi_select_all$response_var_levels)

    power_bi_select_all <- power_bi_select_all %>%
      dplyr::filter(!is.na(response_var_levels))%>%
      dplyr::filter(!is.na(grouping_var_levels))

    power_bi_select_all <- fix_levels_and_order_in_table(table = power_bi_select_all, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

    power_bi_select_all$grouping_var_used  <- stringr::str_replace_all(power_bi_select_all$grouping_var_used, "_", " ") %>%
      stringr::str_to_title()

    power_bi_select_all$response_var_used  <- stringr::str_replace_all(power_bi_select_all$response_var_used, "__$", "") %>%
      stringr::str_to_lower()

    unique_select_all_items <- power_bi_select_all$response_var_used %>% unique()

    question_text_df <- select_all_df %>% dplyr::filter(label_info %in% unique_select_all_items) %>% dplyr::select(label_info, question) %>% dplyr::distinct()

    for(i in seq_along(question_text_df$label_info)) {
      power_bi_select_all$response_var_used <- ifelse(
        power_bi_select_all$response_var_used == question_text_df$label_info[[i]],
        question_text_df$question[[i]],
        power_bi_select_all$response_var_used
      )
    }

    combined_final_table <- power_bi_select_all
  }

  if(response_var_type == "numeric"){

    additional_text_vars <- column_details$column_names[column_details$type == "numeric"]

    likert_variables <- df %>%
      select( starts_with("NUM__ec_")) %>%
      colnames()

    response_var_names <- c(additional_text_vars, likert_variables)

    response_var_used_order <- factor(response_var_names, unique(response_var_names)) %>% as.numeric()

    power_bi_descr_table_num <- run_create_table_with_var_names_and_orders(
      df = df, response_var_names = response_var_names,
      response_var_used_order = response_var_used_order,
      response_var_type = "numeric", grouping_vars = grouping_vars,
      grouping_var_used_order = grouping_var_used_order)

    power_bi_descr_table_num <- power_bi_descr_table_num %>%
      dplyr::filter(!is.na(grouping_var_levels))

    power_bi_descr_table_num <- fix_levels_and_order_in_table(table = power_bi_descr_table_num, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

    power_bi_descr_table_num$grouping_var_used  <- stringr::str_replace_all(power_bi_descr_table_num$grouping_var_used, "_", " ") %>%
      stringr::str_to_title()

    power_bi_descr_table_num$response_var_used  <- stringr::str_replace_all(power_bi_descr_table_num$response_var_used, "NUM__ec__", "") %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_to_title()

    combined_final_table <- power_bi_descr_table_num %>%
      dplyr::select(grouping_var_levels,	grouping_var_order,	mean,	sd,	n,	se,
                    ci_limit,	ci_upper,	ci_lower,	grouping_var_used,
                    grouping_var_used_order,	response_var_used,
                    response_var_used_order, overall_sample_size_for_response_var)
  }

  if(response_var_type == "net_promoter"){

    response_var_names <- "net_promoter"

    response_var_used_order <- 1

    power_bi_net_promoter <- run_create_table_with_var_names_and_orders(df, response_var_names, response_var_used_order, response_var_type = "net_promoter", grouping_vars, grouping_var_used_order)

    power_bi_net_promoter <- fix_levels_and_order_in_table(table = power_bi_net_promoter, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

    power_bi_net_promoter$grouping_var_used  <- stringr::str_replace_all(power_bi_net_promoter$grouping_var_used, "_", " ") %>%
      stringr::str_to_title()

    combined_final_table <- power_bi_net_promoter %>%
      dplyr::select(grouping_var_levels,	grouping_var_order,
                    sum,	n,	average,	NPS,	grouping_var_used,
                    grouping_var_used_order, overall_sample_size_for_response_var)

  }

  return(combined_final_table)
}

#' create_power_bi_data_sa
#'
#' @param df df
#' @param single_path_to_column_workbook single_path_to_column_workbook
#' @param grouping_vars grouping_vars
#'
#' @return table
#' @export
#'
create_power_bi_data_sa <- function(
    df,
    single_path_to_column_workbook,
    grouping_vars
) {

  library(progress)

  name_of_column_details = "column_details"

  column_workbook_list <- create_column_details_and_named_vectors_list(path_to_column_workbook = single_path_to_column_workbook, name_of_column_details = name_of_column_details)

  column_details <- obtain_column_details_table(column_workbook_list = column_workbook_list, name_of_column_details = name_of_column_details)

  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    ifelse(
      !(.x %in% column_details$column_names),
      .x,
      column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info))
  })

  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  select_all_df <- extract_column_details_question_type_df(column_details_table = column_details, question_type_indicator = "sa")

  response_var_names <- select_all_df$column_names %>% stringr::str_extract( "^.+__") %>% unique()

  response_var_used_order <- factor(response_var_names, unique(response_var_names)) %>% as.numeric()

  rv_names_and_order_df <- data.frame(
    response_var_names = response_var_names,
    response_var_used_order = response_var_used_order
  )

  gv_names_and_order_df <- data.frame(
    grouping_vars = grouping_vars,
    grouping_var_used_order = grouping_var_used_order
  )

  parameter_df <- expand.grid(response_var_names, grouping_vars)

  colnames(parameter_df) <- c("response_var_names", "grouping_vars")

  parameter_df <- parameter_df %>%
    dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
    dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

  n_iterations <- length(parameter_df$response_var_names)

  pb <- progress::progress_bar$new(total = n_iterations)

  table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{
    t <- create_table_single(
      df = df,
      response_var_name = parameter_df$response_var_names[[.x]],
      grouping_var_name = parameter_df$grouping_vars[[.x]],
      response_var_type = "sa")

    pb$tick()

    t$response_var_used <- parameter_df$response_var_names[[.x]]
    t$response_var_used_order <- parameter_df$response_var_used_order[[.x]]
    t$grouping_var_used <- parameter_df$grouping_vars[[.x]]
    t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[.x]]
    t
  })

  label_info_var <- stringr::str_replace({{response_var_names}}, "__$", "")

  combined_named_vector_for_select_all <- purrr::map(label_info_var, ~{
    column_workbook_list[[.x]]
  }) %>% unlist()

  table$response_var_levels <- factor(table$response_var_levels, levels = combined_named_vector_for_select_all, labels = names(combined_named_vector_for_select_all))

  table$response_var_order <- as.numeric(table$response_var_levels)

  table <- table %>%
    dplyr::filter(!is.na(response_var_levels))%>%
    dplyr::filter(!is.na(grouping_var_levels))

  table <- fix_levels_and_order_in_table(table = table, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

  table$grouping_var_used  <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>%
    stringr::str_to_title()

  table$response_var_used  <- stringr::str_replace_all(table$response_var_used, "__$", "") %>%
    stringr::str_to_lower()

  unique_select_all_items <- table$response_var_used %>% unique()

  question_text_df <- select_all_df %>% dplyr::filter(label_info %in% unique_select_all_items) %>% dplyr::select(label_info, question) %>% dplyr::distinct()

  for(i in seq_along(question_text_df$label_info)) {
    table$response_var_used <- ifelse(
      table$response_var_used == question_text_df$label_info[[i]],
      question_text_df$question[[i]],
      table$response_var_used
    )
  }

  table$wave <- "Wave 1"

  table <- table %>% dplyr::select("wave", "grouping_var_levels",	"response_var_levels",
                                   "grouping_var_order",	"response_var_order",	"counts",
                                   "group_n",	"percentage",	"grouping_var_used",
                                   "grouping_var_used_order",	"response_var_used",
                                   "response_var_used_order", "overall_sample_size_for_response_var")

  return(table)
}


#' create_power_bi_data_num
#'
#' @param df  df
#' @param single_path_to_column_workbook single_path_to_column_workbook
#' @param grouping_vars grouping_vars
#'
#' @return table
#' @export
#'
create_power_bi_data_num <- function(
    df,
    single_path_to_column_workbook,
    grouping_vars
) {

  library(progress)

  name_of_column_details = "column_details"

  column_workbook_list <- create_column_details_and_named_vectors_list(path_to_column_workbook = single_path_to_column_workbook, name_of_column_details = name_of_column_details)

  column_details <- obtain_column_details_table(column_workbook_list = column_workbook_list, name_of_column_details = name_of_column_details)

  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    ifelse(
      !(.x %in% column_details$column_names),
      .x,
      column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info))
  })

  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  additional_num_vars <- column_details$column_names[column_details$type == "numeric"]

  likert_variables <- df %>%
    select( tidyselect::starts_with("NUM__")) %>%
    colnames()

  response_var_names <- c(additional_num_vars, likert_variables)

  response_var_used_order <- factor(response_var_names, unique(response_var_names)) %>% as.numeric()

  rv_names_and_order_df <- data.frame(
    response_var_names = response_var_names,
    response_var_used_order = response_var_used_order
  )

  gv_names_and_order_df <- data.frame(
    grouping_vars = grouping_vars,
    grouping_var_used_order = grouping_var_used_order
  )

  parameter_df <- expand.grid(response_var_names, grouping_vars)

  colnames(parameter_df) <- c("response_var_names", "grouping_vars")

  parameter_df <- parameter_df %>%
    dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
    dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

  n_iterations <- length(parameter_df$response_var_names)

  pb <- progress::progress_bar$new(total = n_iterations)

  table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{
    t <- create_table_single(
      df = df,
      response_var_name = parameter_df$response_var_names[[.x]],
      grouping_var_name = parameter_df$grouping_vars[[.x]],
      response_var_type = "numeric")

    pb$tick()

    t$response_var_used <- parameter_df$response_var_names[[.x]]
    t$response_var_used_order <- parameter_df$response_var_used_order[[.x]]
    t$grouping_var_used <- parameter_df$grouping_vars[[.x]]
    t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[.x]]

    t
  })

  table <- table %>%
    dplyr::filter(!is.na(grouping_var_levels))

  table <- fix_levels_and_order_in_table(table = table, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

  table$grouping_var_used  <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>%
    stringr::str_to_title()

  table$response_var_used  <- stringr::str_replace_all(table$response_var_used, "NUM__ec__", "") %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_title()

  table$wave <- "Wave 1"

  table <- table %>%
    dplyr::select("wave", "grouping_var_levels",	"grouping_var_order",	"mean",	"sd",	"n",	"se",
                  "ci_limit",	"ci_upper",	"ci_lower",	"grouping_var_used",
                  "grouping_var_used_order",	"response_var_used",
                  "response_var_used_order", "overall_sample_size_for_response_var")

  table <- table %>%
    dplyr::filter(!is.na(grouping_var_levels))

  table$ci_limit <- tidyr::replace_na(table$ci_limit, 0)
  table$ci_upper <- tidyr::replace_na(table$ci_upper, 0)
  table$ci_lower <- tidyr::replace_na(table$ci_lower, 0)

  return(table)
}


#' create_power_bi_data_nps
#'
#' @param df df
#' @param single_path_to_column_workbook single_path_to_column_workbook
#' @param grouping_vars grouping_vars
#'
#' @return table
#' @export
#'
create_power_bi_data_nps <- function(
    df,
    single_path_to_column_workbook,
    grouping_vars
) {

  library(progress)

  name_of_column_details = "column_details"

  column_workbook_list <- create_column_details_and_named_vectors_list(path_to_column_workbook = single_path_to_column_workbook, name_of_column_details = name_of_column_details)

  column_details <- obtain_column_details_table(column_workbook_list = column_workbook_list, name_of_column_details = name_of_column_details)

  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    ifelse(
      !(.x %in% column_details$column_names),
      .x,
      column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info))
  })

  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  response_var_names <- "net_promoter"

  response_var_used_order <- 1

  rv_names_and_order_df <- data.frame(
    response_var_names = response_var_names,
    response_var_used_order = response_var_used_order
  )

  gv_names_and_order_df <- data.frame(
    grouping_vars = grouping_vars,
    grouping_var_used_order = grouping_var_used_order
  )

  parameter_df <- expand.grid(response_var_names, grouping_vars)

  colnames(parameter_df) <- c("response_var_names", "grouping_vars")

  parameter_df <- parameter_df %>%
    dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
    dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

  n_iterations <- length(parameter_df$response_var_names)

  pb <- progress::progress_bar$new(total = n_iterations)

  table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{
    t <- create_table_single(
      df = df,
      response_var_name = parameter_df$response_var_names[[.x]],
      grouping_var_name = parameter_df$grouping_vars[[.x]],
      response_var_type = "net_promoter")

    pb$tick()

    t$response_var_used <- parameter_df$response_var_names[[.x]]
    t$response_var_used_order <- parameter_df$response_var_used_order[[.x]]
    t$grouping_var_used <- parameter_df$grouping_vars[[.x]]
    t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[.x]]

    t
  })

  table <- fix_levels_and_order_in_table(table = table, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

  table$grouping_var_used  <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>%
    stringr::str_to_title()

  table$response_var_used  <- stringr::str_replace_all(table$response_var_used, "_", " ") %>%
    stringr::str_to_title()

  table$wave <- "Wave 1"


  table <- table %>%
    dplyr::select("wave", "grouping_var_levels",	"grouping_var_order",
                  "sum",	"n",	"average",	"NPS",	"grouping_var_used",
                  "grouping_var_used_order", "overall_sample_size_for_response_var")

  table <- table %>%
    dplyr::filter(!is.na(table$grouping_var_levels))

  return(table)
}


#' create_power_bi_data_qualitative
#'
#' @param df df
#' @param single_path_to_column_workbook single_path_to_column_workbook
#' @param grouping_vars grouping_vars
#' @param qualitative_type qualitative_type
#' @param identifier identifier
#'
#' @return combined_final_table
#' @export
#'
create_power_bi_data_qualitative <- function(
    df,
    single_path_to_column_workbook,
    grouping_vars,
    qualitative_type = NULL,
    identifier = "respondent_id"
) {

  df_used <- df

  library(progress)

  name_of_column_details = "column_details"

  column_workbook_list <- create_column_details_and_named_vectors_list(path_to_column_workbook = single_path_to_column_workbook, name_of_column_details = name_of_column_details)

  column_details <- obtain_column_details_table(column_workbook_list = column_workbook_list, name_of_column_details = name_of_column_details)

  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    ifelse(
      !(.x %in% column_details$column_names),
      .x,
      column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info))
  })

  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  response_var_names <- column_details$column_names[column_details$type == "qualitative"]

  path_to_qualitative_coding_data <- paste0(dirname(single_path_to_column_workbook), "/qualitative_coding_data.xlsx")

  workbook_sheets_qualitative_names_all <- readxl::excel_sheets(path_to_qualitative_coding_data)

  qualitative_coding_details <- readxl::read_excel(path_to_qualitative_coding_data, "qual_column_details")

  workbook_sheets_qualitative_names <- workbook_sheets_qualitative_names_all[workbook_sheets_qualitative_names_all != "qual_column_details"]

  # Loop through each sheet
  workbook_sheets_qualitative <- purrr::map(workbook_sheets_qualitative_names, ~{

    # Read in the data for it
    coding_data <- readxl::read_excel(path_to_qualitative_coding_data, sheet = .x)

    qual_coding_data <- df_used %>%
      dplyr::left_join(coding_data, by = {{identifier}})

    return(qual_coding_data)

    # Name all sheets so they can be easily accessed
  }) %>% purrr::set_names(workbook_sheets_qualitative_names)

  qualitative_coding_details_df_list <- purrr::map(workbook_sheets_qualitative_names,  ~{
    single_sheet_qualitative_coding_details <- qualitative_coding_details %>%
      dplyr::filter(question == .x)

    single_sheet_qualitative_coding_details
  })

  overall_qualitative_coding_details_df_list <- purrr::map(qualitative_coding_details_df_list, ~{
    overall_qualitative_coding_details <- .x %>%
      dplyr::filter(section_type == "overall_coding_labels")

    overall_qualitative_coding_details
  })

  overall_code_labels_list <- purrr::map(overall_qualitative_coding_details_df_list, ~{
    .x$column_labels
  })

  overall_code_indicator_list <- purrr::map(overall_qualitative_coding_details_df_list, ~{
    response_var_names_overall <- .x$column_names %>% stringr::str_extract("^.+__") %>% unique()
    response_var_names_overall
  })

  overall_code_indicator_order_list <- purrr::map(seq_along(overall_code_indicator_list), ~{
    response_var_used_order_overall <- .x
    response_var_used_order_overall
  })

  grouping_vars_selected_list <- purrr::map(workbook_sheets_qualitative, ~{
    .x %>% dplyr::select(tidyselect::any_of(grouping_vars)) %>% colnames()
  })

  grouping_vars_selected_order_list <- purrr::map(grouping_vars_selected_list, ~{
    .x %>% as.factor() %>% as.numeric()
  })

  obtain_the_parameters_for_overall <- function(overall_code_indicator, overall_code_indicator_order, grouping_vars_selected, grouping_vars_selected_order) {

    rv_names_and_order_df <- data.frame(
      response_var_names = overall_code_indicator,
      response_var_used_order = overall_code_indicator_order
    )

    gv_names_and_order_df <- data.frame(
      grouping_vars = grouping_vars_selected,
      grouping_var_used_order = grouping_vars_selected_order
    )

    parameter_df <- expand.grid(rv_names_and_order_df$response_var_names, gv_names_and_order_df$grouping_vars)

    colnames(parameter_df) <- c("response_var_names", "grouping_vars")

    parameter_df <- parameter_df %>%
      dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
      dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

    parameter_df

  }

  overall_parameter_list <- list(overall_code_indicator_list, overall_code_indicator_order_list, grouping_vars_selected_list, grouping_vars_selected_order_list)

  parameter_df_list <- purrr::pmap(overall_parameter_list, obtain_the_parameters_for_overall)

  table_list <- purrr::map2(parameter_df_list, workbook_sheets_qualitative, ~{
    overall_code_data <- .y

    single_parameter_df <- .x

    n_iterations <- length(single_parameter_df$response_var_names)

    pb <- progress::progress_bar$new(total = n_iterations)

    table <- purrr::map_df(seq_along(single_parameter_df$response_var_names), ~{
      number_for_row <- .x
      t <- create_table_single(
        df = overall_code_data,
        response_var_name = single_parameter_df$response_var_names[[number_for_row]],
        grouping_var_name = single_parameter_df$grouping_vars[[number_for_row]],
        response_var_type = "sa")

      pb$tick()

      t$response_var_used <- single_parameter_df$response_var_names[[number_for_row]]
      t$response_var_used_order <- single_parameter_df$response_var_used_order[[number_for_row]]
      t$grouping_var_used <- single_parameter_df$grouping_vars[[number_for_row]]
      t$grouping_var_used_order <- single_parameter_df$grouping_var_used_order[[number_for_row]]

      t
    })

    table
  })

  overall_table_list <- purrr::map2(table_list, overall_qualitative_coding_details_df_list, ~{
    table <- .x
    table$response_var_levels <- factor(table$response_var_levels, .y$column_names, .y$column_labels)
    table$response_var_order <- as.numeric(table$response_var_levels)
    table
  })

  overall_table_list <- purrr::map(overall_table_list, ~{
    table <- .x
    table$grouping_var_used <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>% stringr::str_to_title()
    table
  })

  break_down_qualitative_coding_details <- purrr::map2(qualitative_coding_details_df_list, overall_code_labels_list, ~{
    .x %>%
      dplyr::filter(section_type %in% .y)
  })

  break_down_categories_list <- purrr::map(break_down_qualitative_coding_details, ~{
    break_down_categories <- .x$section_type %>% unique()
    break_down_categories
  })

  break_down_categories_details_list <- purrr::map2(break_down_qualitative_coding_details, break_down_categories_list, ~{
    break_down_qualitative_coding_details_1 <- .x
    break_down_categories <- .y
    purrr::pmap(list(seq_along(break_down_categories)), ~{
      break_down_qualitative_coding_details_1 %>% dplyr::filter(section_type == break_down_categories[[..1]])
    })

  })

  break_down_code_labels_list <- purrr::map(break_down_categories_details_list, ~{
    break_down_categories_details <- .x

    purrr::pmap(list(seq_along(break_down_categories_details)), ~{
      break_down_categories_details[[.x]]$column_labels %>% as.character()
    })
  })

  break_down_code_indicator_list <- purrr::map(break_down_categories_details_list, ~{
    break_down_categories_details <- .x

    purrr::pmap(list(seq_along(break_down_categories_details)), ~{
      break_down_categories_details[[.x]]$column_names %>% stringr::str_extract("^.+__.+__") %>% unique()
    })
  })

  break_down_code_indicator_order_list <- purrr::map(break_down_categories_details_list, ~{
    break_down_code_indicator_order <- .x

    purrr::pmap(list(seq_along(break_down_code_indicator_order)), ~{
      ..1
    })
  })

  break_down_parameter_df_list <- purrr::map(seq_along(break_down_code_indicator_list), ~{
    first_level <- .x

    purrr::map(seq_along(break_down_code_indicator_list[[first_level]]), ~{
      second_level <- .x

      break_down_parameter_list <- list(
        break_down_code_indicator_list[[first_level]][[second_level]],
        break_down_code_indicator_order_list[[first_level]][[second_level]],
        grouping_vars_selected_list[[first_level]],
        grouping_vars_selected_order_list[[first_level]])

      purrr::pmap_df(break_down_parameter_list, obtain_the_parameters_for_overall)

    })})

  break_down_table_list <- purrr::map2(break_down_parameter_df_list, workbook_sheets_qualitative, ~{
    overall_code_data <- .y

    single_parameter_df_list <- .x

    purrr::map_df(single_parameter_df_list, ~{
      single_parameter_df <- .x

      table <- purrr::map_df(seq_along(single_parameter_df$response_var_names), ~{
        number_for_row <- .x
        t <- create_table_single(
          df = overall_code_data,
          response_var_name = single_parameter_df$response_var_names[[number_for_row]],
          grouping_var_name = single_parameter_df$grouping_vars[[number_for_row]],
          response_var_type = "sa")


        t$response_var_used <- single_parameter_df$response_var_names[[number_for_row]]
        t$response_var_used_order <- single_parameter_df$response_var_used_order[[number_for_row]]
        t$grouping_var_used <- single_parameter_df$grouping_vars[[number_for_row]]
        t$grouping_var_used_order <- single_parameter_df$grouping_var_used_order[[number_for_row]]

        t
      })

      table
    })
  })

  break_down_table_list <- purrr::map2(break_down_table_list, break_down_qualitative_coding_details, ~{
    table <- .x
    table$response_var_levels <- factor(table$response_var_levels, .y$column_names, .y$column_labels)
    table$response_var_order <- as.numeric(table$response_var_levels)
    table
  })

  break_down_table_list <- purrr::map(break_down_table_list, ~{
    table <- .x
    table$grouping_var_used <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>% stringr::str_to_title()
    table
  })

  if(qualitative_type == "overall_coding") {
    combined_final_table <- overall_table_list
  } else if(qualitative_type == "break_down_coding"){
    combined_final_table <- break_down_table_list
  }

  return(combined_final_table)
}


#' create_survey_folder_structure_paths_list_power_bi
#'
#' @param project_year project_year
#' @param project_folder_name project_folder_name
#' @param list_of_survey_version_names list_of_survey_version_names
#' @param all_versions_in_same_analysis_folder all_versions_in_same_analysis_folder
#' @param storage_directory storage_directory
#' @param should_create_survey_folders_if_they_do_not_exist should_create_survey_folders_if_they_do_not_exist
#'
#' @return survey_data_list
#' @export
#'
create_survey_folder_structure_paths_list_power_bi <- function(
    project_year,
    project_folder_name,
    list_of_survey_version_names = list("All_Versions"),
    all_versions_in_same_analysis_folder = FALSE,
    storage_directory = "Dropbox (TCM)",
    should_create_survey_folders_if_they_do_not_exist = TRUE
) {
  # Get the parts of your working directory
  parts_of_wd <- unlist(strsplit(getwd(), .Platform$file.sep))

  # Re-Combine the first 3 parts
  path_part_to_adjust <- paste0(parts_of_wd[1:3], collapse = "/")

  # Create the home directory for the current project
  home_dir <- paste0(path_part_to_adjust, "/", storage_directory, "/04 MDM Neuro-Fundraising Lab/00 Jobs/", as.character(project_year), "/", as.character(project_folder_name))

  # Analysis Folder
  project_analysis_folder_location <- paste0(home_dir, "/Analysis")
  project_analysis_respondent_investigation_folder_location <- paste0(home_dir, "/Analysis/Respondent Investigation")

  # Archive Folder
  project_archive_folder_location <- paste0(home_dir, "/Archive")

  # Assets Folder
  project_assets_folder_location <- paste0(home_dir, "/Assets")

  # Data Collection
  project_data_collection_folder_location <- paste0(home_dir, "/Data Collection")
  project_data_collection_survey_monkey_data_folder_location <- paste0(home_dir, "/Data Collection/survey_monkey_data")
  project_data_collection_survey_monkey_data_processed_data_folder_location <- paste0(home_dir, "/Data Collection/survey_monkey_data/processed_data")

  snake_version_names <- snakecase::to_snake_case(list_of_survey_version_names %>% unlist())

  version_folders_location_names <- paste0(
    "project_data_collection_survey_monkey_data_version_folder_",
    snake_version_names
  )

  project_data_collection_survey_monkey_data_version_folders_location <-
    purrr::map(list_of_survey_version_names, ~{
      paste0(home_dir, "/Data Collection/survey_monkey_data/", .x)
    }) %>% purrr::set_names(version_folders_location_names)

  # Instructions
  project_instructions_folder_location <- paste0(home_dir, "/Instructions")

  # Presentation
  project_presentation_folder_location <- paste0(home_dir, "/Presentation")

  # Presentation
  project_proposal_folder_location <- paste0(home_dir, "/Proposal")

  if(all_versions_in_same_analysis_folder == TRUE){
    list_of_survey_version_names <- list("All_Versions")
    snake_version_names <- snakecase::to_snake_case(list_of_survey_version_names %>% unlist())
  }

  survey_version_analysis_folder_names <- paste0("survey_version_analysis_folder_", snake_version_names)

  project_power_bi_version_folders_location <-
    purrr::map(list_of_survey_version_names, ~{
      paste0(project_analysis_respondent_investigation_folder_location, "/", .x)
    }) %>% purrr::set_names(survey_version_analysis_folder_names)

  survey_version_analysis_deck_names <- paste0(survey_version_analysis_folder_names, "_Power_BI_Deck_folder")
  survey_version_analysis_processed_data_names <- paste0(survey_version_analysis_folder_names, "_processed_data_folder")
  survey_version_analysis_processed_text_names <- paste0(survey_version_analysis_folder_names, "_processed_text_folder")

  survey_version_analysis_deck_folder_location <- purrr::map(project_power_bi_version_folders_location, ~{
    paste0(.x, "/Power_BI_Deck")
  }) %>% purrr::set_names(survey_version_analysis_deck_names)

  survey_version_analysis_processed_data_folder_location <- purrr::map(project_power_bi_version_folders_location, ~{
    paste0(.x, "/processed_data")
  }) %>% purrr::set_names(survey_version_analysis_processed_data_names)

  survey_version_analysis_processed_text_folder_location <- purrr::map(project_power_bi_version_folders_location, ~{
    paste0(.x, "/processed_text")
  }) %>% purrr::set_names(survey_version_analysis_processed_text_names)

  # Maybe add TCM Folders Later

  # Put these together as a list
  list_of_folders_paths <- list(
    "home_dir" = home_dir,
    "project_analysis_folder_location" = project_analysis_folder_location,
    "project_analysis_respondent_investigation_folder_location" = project_analysis_respondent_investigation_folder_location,
    "project_archive_folder_location, project_assets_folder_location" = project_archive_folder_location,
    "project_assets_folder_location" = project_assets_folder_location,
    "project_data_collection_folder_location" = project_data_collection_folder_location,
    "project_data_collection_survey_monkey_data_folder_location" = project_data_collection_survey_monkey_data_folder_location,
    "project_data_collection_survey_monkey_data_processed_data_folder_location" = project_data_collection_survey_monkey_data_processed_data_folder_location,
    "project_instructions_folder_location" = project_instructions_folder_location,
    "project_presentation_folder_location" = project_presentation_folder_location,
    "project_proposal_folder_location" = project_proposal_folder_location
  ) %>% append(project_data_collection_survey_monkey_data_version_folders_location) %>%
    append(project_power_bi_version_folders_location) %>%
    append(survey_version_analysis_deck_folder_location) %>%
    append(survey_version_analysis_processed_data_folder_location) %>%
    append(survey_version_analysis_processed_text_folder_location)

  if(should_create_survey_folders_if_they_do_not_exist == TRUE) {
    # If theses do not exist, create them.
    create_survey_folders_if_they_do_not_exist(list_of_folders_paths)
  }

  return(list_of_folders_paths)
}


#' Create time stamps to add to file names
#' This function allows us to keep track of every version of a file, which can be important for record keeping and troubleshooting purposes.
#'
#' @param format_string Character string to indicate how the date/time object should be formatted. The default is '%m%d%Y_%H%M'.
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


#' create_traditional_project_structure
#'
#' @param should_create_nonexistant_dirs should_create_nonexistant_dirs
#'
#' @return invisible(list_of_paths)
#' @export
#'
create_traditional_project_dir_list <- function(
    should_create_nonexistant_dirs = TRUE
) {

  # Create all needed paths
  p_path_analysis <- "Analysis"
  p_path_analysis_ri <- paste0(p_path_analysis, "/Respondent Investigation")
  p_path_analysis_immersion <- paste0(p_path_analysis, "/Immersion")
  p_path_archive <- "Archive"
  p_path_assets <- "Assets"
  p_path_data_collection <- "Data Collection"
  p_path_instructions <- "Instructions"
  p_path_presentation <- "Presentation"
  p_path_proposal <- "Proposal"

  # Put these together as a list
  list_of_paths <- list(
    "p_path_analysis" = p_path_analysis,
    "p_path_analysis_ri" = p_path_analysis_ri,
    "p_path_analysis_immersion" = p_path_analysis_immersion,
    "p_path_archive" = p_path_archive,
    "p_path_assets" = p_path_assets,
    "p_path_data_collection" = p_path_data_collection,
    "p_path_instructions" = p_path_instructions,
    "p_path_presentation" = p_path_presentation,
    "p_path_proposal" = p_path_proposal
  )

  # Create the path types as dir
  list_of_file_types <- rep("dir", length(list_of_paths)) %>% as.list()

  # If desired
  if(should_create_nonexistant_dirs == TRUE) {

    # Check if they are missing and if so create them.
    create_nonexistant_files(list_of_paths, list_of_file_types)
  }

  # Invisibly return the paths
  return(invisible(list_of_paths))
}



# D -----------------------------------------------------------------------


# E -----------------------------------------------------------------------

#' encode_text_columns_correctly
#'
#' @param data df that has text columns
#'
#' @return data_used
#' @export
#'
encode_text_columns_correctly <- function(data) {
  # Prep data
  data_used <- data

  # Helper function to encode text
  encode_text_correctly <- function(x){
    Encoding(x) <- "UTF-8"
    x
  }

  # Encode the text columns correctly
  data_used <- data_used %>%
    mutate(across(starts_with("text_"), encode_text_correctly))

  # Return the df
  return(data_used)
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



# F -----------------------------------------------------------------------


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


# This seems to have some bugs.

#' fix_levels_and_order_in_table
#'
#' @param table table
#' @param var_type var_type
#' @param column_details column_details
#' @param column_workbook_list column_workbook_list
#' @param title_form title_form
#'
#' @return table
#' @export
#'
fix_levels_and_order_in_table <- function(table, var_type, column_details, column_workbook_list, title_form = FALSE) {

  label_for_var_used <- paste0({{var_type}}, "_used")
  label_for_var_levels <- paste0({{var_type}}, "_levels")
  label_for_var_order <- paste0({{var_type}}, "_order")

  var_names_that_need_levels <- table[[label_for_var_used]] %>% unique()

  if(title_form == TRUE) {
    var_names_that_need_levels <- stringr::str_replace_all(var_names_that_need_levels, " ", "_") %>% stringr::str_to_lower()
  }

  var_vector_names <- column_details %>%
    dplyr::filter(column_names %in% var_names_that_need_levels) %>%
    dplyr::pull(label_info)

  var_names_that_need_levels_details_names <- column_details %>%
    dplyr::filter(column_names %in% var_names_that_need_levels) %>%
    dplyr::pull(column_names)

  var_vector_names <- purrr::map_chr(var_names_that_need_levels, ~{
    ifelse(
      .x %in% var_names_that_need_levels_details_names,
      column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info),
      .x
    )
  })

  named_vectors_for_factor <- purrr::map(var_vector_names, ~{
    column_workbook_list[[.x]]
  }) %>% unlist()

  All_label <- c("All")
  names(All_label) <- All_label

  named_vectors_for_factor <- c(All_label,  named_vectors_for_factor)

  table[[label_for_var_levels]] <- factor(table[[label_for_var_levels]], levels = names(named_vectors_for_factor), labels = names(named_vectors_for_factor))

  table[[label_for_var_order]] <- as.numeric(table[[label_for_var_levels]])

  table[[label_for_var_order]] <- ifelse(
    table[[label_for_var_levels]] == "Other",
    9999,
    table[[label_for_var_order]]
  )

  table[[label_for_var_order]] <- ifelse(
    table[[label_for_var_levels]] == "None",
    9998,
    table[[label_for_var_order]]
  )

  return(table)
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




# G -----------------------------------------------------------------------





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

#' get_file_paths_from_excel_workbooks_df
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


#' get_full_column_details_from_dir
#'
#' @param survey_directory_path survey_directory_path
#'
#' @return column_details
#' @export
#'

get_full_column_details_from_dir <- function(
    survey_directory_path) {

  column_names_path <- file.path(survey_directory_path, paste0(basename(survey_directory_path), "_column_names.xlsx"))

  # Obtain the newest version from each directory
  newest_files_for_project <- try(
    {bkissell::obtain_newest_file_per_directory(
      survey_directory_path,
      file_ext = file_ext, format_pattern = format_pattern)},
    silent = TRUE
  )

  if(class(newest_files_for_project) != "try-error"){
    # Obtain the paths for the column names excel files
    version_column_names_path <- bkissell::obtain_column_name_path(survey_directory_path)

    # Create the column details list
    column_details <- readxl::read_excel(version_column_names_path, "column_details")

    # Return the df
    return(column_details)
  }
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




#' get_power_bi_parameters
#'
#' @param project_year project_year
#' @param project_folder_name project_folder_name
#' @param list_of_survey_version_names list_of_survey_version_names
#' @param all_versions_in_same_analysis_folder all_versions_in_same_analysis_folder
#' @param other_vars_to_include other_vars_to_include
#' @param grouping_vars grouping_vars
#' @param spellcheck_replace_columns spellcheck_replace_columns
#'
#' @export
#'
get_power_bi_parameters <- function(
    project_year = 2023,
    project_folder_name = "230389_SDZ_BC__Quarterly Survey_OID1771",
    list_of_survey_version_names = list("Wave 1 - Member", "Wave 1 - Donor"),
    all_versions_in_same_analysis_folder = FALSE,
    other_vars_to_include,
    grouping_vars,
    spellcheck_replace_columns = TRUE,
    version_name_style,
    item_everyone_must_complete,
    remove_rids = FALSE,
    keep_rids = FALSE
) {

  # Create the paths for the folders that need to exist for the survey project
  survey_folder_structure_paths_list <<- create_survey_folder_structure_paths_list_power_bi(
    project_year = project_year,
    project_folder_name = project_folder_name,
    list_of_survey_version_names = list_of_survey_version_names,
    all_versions_in_same_analysis_folder = all_versions_in_same_analysis_folder
  )

  # Point to where the survey data is located
  survey_directory_path <<- survey_folder_structure_paths_list$project_data_collection_survey_monkey_data_folder_location

  # Create path for home dir
  home_dir <<- survey_folder_structure_paths_list$home_dir

  # Read the survey data as a list
  survey_data_list <<- read_survey_data_for_power_bi(survey_directory_path = survey_directory_path)

  # Obtain the version names
  version_names_from_list_data_collection <<- names(survey_data_list)

  # Create Version Directories
  version_directories_data_collection_path <<- prepare_version_directory_paths(survey_directory_path)

  spellcheck_column_paths <<- paste0(version_directories_data_collection_path, "/spellchecked_text_columns.xlsx")

  # Create the path where the column information workbook will be located
  path_to_column_workbook <<- obtain_column_name_paths_for_all_survey_versions(version_directories_data_collection_path)

  path_to_column_workbook_list <<- path_to_column_workbook

  version_names_from_list_analysis <<- names(survey_data_list)

  rids_to_remove_path <<- paste0(survey_directory_path, "/rids_to_remove.csv")

  rids_to_keep_path <<- paste0(survey_directory_path, "/rids_to_keep.csv")

  power_bi_clean_data_path_list <<- purrr::map(version_names_from_list_analysis, ~{
    paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/clean_data.csv")
  }, home_dir)

  processed_data_clean_data_path_list <<- purrr::map(version_names_from_list_analysis, ~{
    paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/processed_data/PROCESSED_", snakecase::to_snake_case(.x), "_data_", create_time_chr_string_for_file_names(), ".csv")
  }, home_dir)

  # Combine other and group variable names
  variables_to_include_in_all_tables <<- c(other_vars_to_include, grouping_vars)


  survey_data_list <<- process_and_clean_power_bi_survey_data(
    survey_directory_path,
    spellcheck_replace_columns,
    spellcheck_column_paths,
    all_versions_in_same_analysis_folder,
    path_to_column_workbook,
    version_name_style,
    likert_indicator = "ec__",
    item_everyone_must_complete,
    remove_rids,
    keep_rids
  )


  # Filter list to match the variables contained in the df
  variables_to_include_list <<- purrr::map(survey_data_list, ~{
    data <- .x
    #
    #     if(!("age_group" %in% colnames(data))) {
    #       data$age_group <- NA
    #     }
    #
    #     if(!("study_version" %in% colnames(data))) {
    #       data$study_version <- NA
    #     }


    data %>% dplyr::select(any_of(variables_to_include_in_all_tables)) %>% colnames()
  })

  # Filter to list only the grouping vars contained in each data frame
  grouping_vars_list <<- purrr::map(survey_data_list, ~{
    .x %>% dplyr::select(any_of(grouping_vars)) %>% colnames()
  })

  text_survey_data_path_list <<- purrr::map(version_names_from_list_analysis, ~{
    paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/processed_text/TEXT_PROCESSED_", snakecase::to_snake_case(.x), "_", create_time_chr_string_for_file_names(), ".csv")
  }, home_dir)

  # Save it to the powerbi folder
  power_bi_text_path_list <<- purrr::map(version_names_from_list_analysis, ~{
    paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_text_survey_data.csv")
  }, home_dir)


  if(spellcheck_replace_columns == TRUE) {
    text_selected_example_text_survey_data_path_list <<- purrr::map_chr(version_names_from_list_analysis, ~{
      paste0(home_dir, "/Data Collection/survey_monkey_data/", .x, "/selected_example_text.xlsx")
    }, home_dir)

    text_selected_example_response_vars_list <<- purrr::map(text_selected_example_text_survey_data_path_list, ~{
      readxl::excel_sheets(.x)
    })

    create_VAR_df_with_single_row <- purrr::as_mapper(
      function(path, response_var_to_use) {
        data.frame(Var1 = response_var_to_use, Var2 = path)
      })

    create_VAR_df_with_multiple_rows <- purrr::as_mapper(
      function(path, response_vars_to_use) {
        path <- path
        response_vars_to_use <- response_vars_to_use
        purrr::map_df(response_vars_to_use,  ~ {
          create_VAR_df_with_single_row(path, .x)
        }, path)
      })

    parameters_for_example_read_list <<- purrr::map2(text_selected_example_text_survey_data_path_list, text_selected_example_response_vars_list, ~ {
      create_VAR_df_with_multiple_rows(.x, .y)
    })

    text_selected_example_text_survey_data_path_list <<- purrr::map_chr(version_names_from_list_analysis, ~{
      paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_selected_example_text.csv")
    }, home_dir)
  }

  power_bi_mc_path_list <<- purrr::map(version_names_from_list_analysis, ~{
    paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_member_multiple_choice.csv")
  }, home_dir)

  power_bi_sa_path_list <<- purrr::map(version_names_from_list_analysis, ~{
    paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_member_select_all.csv")
  }, home_dir)

  power_bi_descr_table_num_path_list <<- purrr::map(version_names_from_list_analysis, ~{
    paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_member_descr_table_num.csv")
  }, home_dir)

  power_bi_net_promoter_path_list <<- purrr::map(version_names_from_list_analysis, ~{
    paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_net_promoter.csv")
  }, home_dir)

  column_workbook_list <<- purrr::map(path_to_column_workbook_list, ~{
    create_column_details_and_named_vectors_list(path_to_column_workbook = .x, name_of_column_details = "column_details")
  })

  column_details_list <<- purrr::map(column_workbook_list, ~{
    obtain_column_details_table(column_workbook_list = .x, name_of_column_details = "column_details")
  })

  power_bi_overall_qualitative_path_list <<- purrr::map(version_names_from_list_analysis, ~{
    paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_overall_qualitative.csv")
  }, home_dir)

  power_bi_break_down_qualitative_path_list <<- purrr::map(version_names_from_list_analysis, ~{
    paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")
  }, home_dir)
}

# H -----------------------------------------------------------------------


# I -----------------------------------------------------------------------



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


# J -----------------------------------------------------------------------


# K -----------------------------------------------------------------------


# L -----------------------------------------------------------------------


# M -----------------------------------------------------------------------


# N -----------------------------------------------------------------------


# O -----------------------------------------------------------------------


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
#' @param survey_directory_path survey_directory_path
#'
#' @return column_name_path
#' @export
#'
obtain_column_name_path <- function(survey_directory_path) {
  # Create path for the column names workbook
  file_name <- paste0(basename(survey_directory_path), "_column_names.xlsx")

  column_name_path <- file.path(survey_directory_path, file_name)

  return(column_name_path)
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

    do_not_keep_page_order <- stringr::str_detect(file_names, "PageOrder")

    the_path_inside_zip <- file_names[!do_not_keep_page_order]

    do_not_keep_collector_list <- stringr::str_detect(the_path_inside_zip, "CollectorList")

    the_path_inside_zip <- file_names[!do_not_keep_collector_list]

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



#' obtain_the_connection_to_zip_file_collector_list
#'
#' @param newest_files_for_project newest_files_for_project
#'
#' @return connection_to_zip_files
#' @export
#'
obtain_the_connection_to_zip_file_collector_list <- function(newest_files_for_project) {

  # Get the name of the file from the zip file
  file_names <- unzip(zipfile = newest_files_for_project, list = TRUE) |> dplyr::pull(Name)

  keep_collector_list <- stringr::str_detect(file_names, "CollectorList")

  # Just obtain the file that should be read
  the_path_inside_zip <- file_names[keep_collector_list]

  # Create the connection to the zip files
  connection_to_zip_file <-  unz(newest_files_for_project,  the_path_inside_zip)

  # if(!any( keep_collector_list)) {
  #   # warning("CollectorList does not exist in this zip file")
  # }

  # Return the connection
  return(connection_to_zip_file)
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

#' obtain_the_connection_to_zip_file
#'
#' @param newest_files_for_project newest_files_for_project
#'
#' @return connection_to_zip_files
#' @export
#'
obtain_the_connection_to_zip_file <- function(newest_files_for_project) {

  # Get the name of the file from the zip file
  file_names <- unzip(zipfile = newest_files_for_project, list = TRUE) |> dplyr::pull(Name)

  # Which files should not be read
  do_not_keep_page_order <- stringr::str_detect(file_names, "PageOrder")

  # Just obtain the file that should be read
  the_path_inside_zip <- file_names[!do_not_keep_page_order]

  do_not_keep_collector_list <- stringr::str_detect(the_path_inside_zip, "CollectorList")

  the_path_inside_zip <- the_path_inside_zip[!do_not_keep_collector_list]
  # Create the connection to the zip files
  connection_to_zip_file <-  unz(newest_files_for_project,  the_path_inside_zip)

  # Return the connection
  return(connection_to_zip_file)
}

# P -----------------------------------------------------------------------



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


#' Prepare the grouping and response variables
#'
#' @param df data frame with the survey data
#' @param response_var_name character with the response var name
#' @param grouping_var_name character with the grouping var name
#'
#' @return df
#' @export
#'
prepare_VAR_ORDER_variables_single <- function(df, response_var_name, grouping_var_name){

  # Check if df is a data frame
  if(!is.data.frame(df)) abort_bad_argument("df", must = "be data.frame", not = df)

  # Allow response_var_name and grouping_var_name to be null, but if not
  # Check to see if they are character strings, and then prepare the ordering variables
  if(!is.null(response_var_name)) {
    if(!is.character(response_var_name)) abort_bad_argument("response_var_name", must = "be character", not = response_var_name)
    df[[paste0("VAR_ORDER__", {{response_var_name}})]] <- df[[{{response_var_name}}]] %>% as.factor() %>% as.numeric()
    order_variable <<- paste0("VAR_ORDER__", {{response_var_name}})
  }

  if(!is.null(grouping_var_name)) {
    if(!is.character(grouping_var_name)) abort_bad_argument("grouping_var_name", must = "be character", not = grouping_var_name)
    df[[paste0("VAR_ORDER__", {{grouping_var_name}})]] <- df[[{{grouping_var_name}}]] %>% as.factor() %>% as.numeric()
    order_grouping_var <<- paste0("VAR_ORDER__", {{grouping_var_name}})
  }

  # Return the df
  return(df)
}

#' process_a_select_all_response_as_ones_and_zeros
#'
#' @param is_all_column_data_missing_for_row is_all_column_data_missing_for_row
#' @param variable variable
#'
#' @return processed_var
#' @export
#'
process_a_select_all_response_as_ones_and_zeros <- function(is_all_column_data_missing_for_row, variable) {
  processed_var <- dplyr::case_when(
    is_all_column_data_missing_for_row == TRUE ~ NA,
    variable == 0 ~ 0,
    !is.na(variable) ~ 1,
    is.na(variable) ~ 0
  )

  return(processed_var)
}


#' process_and_clean_power_bi_survey_data
#'
#' @param survey_directory_path survey_directory_path
#' @param spellcheck_replace_columns spellcheck_replace_columns
#' @param spellcheck_column_paths spellcheck_column_paths
#' @param all_versions_in_same_analysis_folder all_versions_in_same_analysis_folder
#' @param path_to_column_workbook path_to_column_workbook
#' @param version_name_style version_name_style
#' @param likert_indicator likert_indicator
#' @param item_everyone_must_complete item_everyone_must_complete
#' @param remove_rids remove_rids
#' @param keep_rids keep_rids
#'
#' @return survey_data_list
#' @export
#'
process_and_clean_power_bi_survey_data <- function(
    survey_directory_path,
    spellcheck_replace_columns = TRUE,
    spellcheck_column_paths = NULL,
    all_versions_in_same_analysis_folder = TRUE,
    path_to_column_workbook = NULL,
    version_name_style = "wave - condition",
    likert_indicator = "ec__",
    item_everyone_must_complete = "annual_household_income",
    remove_rids = FALSE,
    keep_rids = FALSE
) {

  # Read the survey data as a list
  survey_data_list <- read_survey_data_for_power_bi(survey_directory_path = survey_directory_path)

  # If there are spellcheck columns to replace, this function will make those adjustments (if set up correctly)
  rsdfpi_spellcheck_replace <- function(survey_data_list, spellcheck_replace_columns, spellcheck_column_paths) {
    if(spellcheck_replace_columns == TRUE) {
      if(is.null(spellcheck_column_paths)){ stop("Please add spellcheck_column_paths list.")}
      survey_data_list <- purrr::map2(survey_data_list, spellcheck_column_paths, ~{
        df <- .x
        named_spellcheck_workbook_sheets <- readxl::excel_sheets(.y)
        get_spellchecked_data <- function(df, path, sheet_name) {
          sc_df <- readxl::read_excel(path, sheet = sheet_name)
          df <- df %>% dplyr::select(!{{sheet_name}})
          df$respondent_id <- as.numeric(df$respondent_id)
          df <- df %>% dplyr::left_join(sc_df, by = "respondent_id")
          return(df)
        }
        for(i in named_spellcheck_workbook_sheets) {
          df <- get_spellchecked_data(df, .y, i)
        }
        return(df)
      })
    }
  }

  survey_data_list <- rsdfpi_spellcheck_replace(survey_data_list, spellcheck_replace_columns, spellcheck_column_paths)

  rsdfpi_combine_survey_versions <- function(survey_data_list, path_to_column_workbook) {
    if(all_versions_in_same_analysis_folder == TRUE) {
      survey_data_list <- survey_data_list %>% purrr::map_df(~.x) %>% list()
      survey_data_list <- survey_data_list %>% purrr::set_names("All_Versions")
      path_to_column_workbook <<- path_to_column_workbook[[1]]
    }
    return(survey_data_list)
  }

  survey_data_list <- rsdfpi_combine_survey_versions(survey_data_list, path_to_column_workbook)

  rsdfpi_extract_info_for_version_names <- function(version_name_style, survey_data_list) {
    if(version_name_style == "wave - condition"){
      survey_data_list <- purrr::map(survey_data_list, ~{
        revised_data <- .x
        revised_data$wave <- stringr::str_extract(revised_data$version_name, "^.+-") %>% stringr::str_replace(" -$", "")
        revised_data$study_version <- stringr::str_extract(revised_data$version_name, "-.+$") %>% stringr::str_replace("^- ", "")
        revised_data
      })
    } else if(version_name_style == "none") {
      survey_data_list <- purrr::map(survey_data_list, ~{
        revised_data <- .x
        revised_data$wave <- "Wave 1"
        revised_data$study_version <- revised_data$version_name
        revised_data
      })
    }
    return(survey_data_list)
  }

  survey_data_list <- rsdfpi_extract_info_for_version_names(version_name_style, survey_data_list)

  rsdfpi_process_age <- function(survey_data_list) {
    survey_data_list <- purrr::map(survey_data_list, ~{
      revised_data <- .x
      if("age" %in% colnames(revised_data)) {
        revised_data$age <- as.numeric(revised_data$age)
        current_year <- lubridate::today() %>% lubridate::year()
        revised_data$age_group <- dplyr::case_when(
          revised_data$age >= current_year - 1945 ~ "Seniors_Traditional",
          revised_data$age >= current_year - 1964 & revised_data$age <= current_year - 1946 ~ "Baby Boomers",
          revised_data$age >= current_year - 1980 & revised_data$age <= current_year - 1965 ~ "Gen X",
          revised_data$age >= current_year - 1996 & revised_data$age <= current_year - 1981 ~ "Millennials",
          revised_data$age <= current_year - 1995 ~ "Gen Z"
        ) %>% factor(levels = c("Gen Z", "Millennials", "Gen X", "Baby Boomers", "Seniors_Traditional"), ordered = TRUE)
      }
      revised_data
    })
    return(survey_data_list)
  }

  survey_data_list <- rsdfpi_process_age(survey_data_list)

  rsdfpi_do_initial_processing <- function(survey_data_list, path_to_column_workbook) {
    survey_data_list <- purrr::map2(survey_data_list, path_to_column_workbook, ~{
      # Do the initial cleaning of the data, which includes converting numeric, categorical, and select all to their proper formats.
      revised_data <- clean_and_process_with_column_workbook_list(
        data = .x,
        path_to_column_workbook = .y,
        likert_indicator = likert_indicator,
        name_of_column_details = "column_details")
      revised_data
    })
    return(survey_data_list)
  }

  survey_data_list <- rsdfpi_do_initial_processing(survey_data_list, path_to_column_workbook)

  # If parameters indicate to do so,
  if(!is.null(item_everyone_must_complete)){
    # Filter out respondents who did not complete this question
    survey_data_list <- purrr::map(survey_data_list, ~{
      revised_data <- .x %>%
        dplyr::filter(!is.na(.data[[{{item_everyone_must_complete}}]]))
      revised_data
    })
  }

  if(remove_rids == TRUE){
    rids_to_remove_path <- paste0(survey_directory_path, "/rids_to_remove.csv")

    rids_to_remove <- readr::read_csv(rids_to_remove_path, show_col_types = FALSE)[[1]]

    survey_data_list <- purrr::map(survey_data_list, ~{
      revised_data <- .x %>%
        dplyr::filter(!(.data[[{{what_identifier_was_used}}]] %in% rids_to_remove))
      revised_data
    })
  }

  if(keep_rids == TRUE){
    rids_to_keep_path <- paste0(survey_directory_path, "/rids_to_keep.csv")

    rids_to_keep <- readr::read_csv(rids_to_keep_path, show_col_types = FALSE)[[1]]

    survey_data_list <- purrr::map(survey_data_list, ~{
      revised_data <- .x %>%
        dplyr::filter((.data[[{{what_identifier_was_used}}]] %in% rids_to_keep))
      revised_data
    })
  }

  return(survey_data_list)
}



#' Process the data for all of the numeric variables
#'
#' @param data_used data frame with the survey data
#' @param column_details column details df
#' @param likert_indicator character string that is used at the start of each likert variable name.
#'
#' @return data_used
#' @export
#'
process_numeric_vars <- function(data_used, column_details, likert_indicator = likert_indicator) {

  # Check that data_used is a data frame
  if(!is.data.frame(data_used)) {
    rlang::abort(glue::glue("The `data_used` object is not a data frame."))
  }

  # Check that column_details is a df
  if(!is.data.frame(column_details)) {
    rlang::abort(glue::glue("The `column_details` object is not a data frame."))
  }

  # Check that the likert_indicator is a character string
  if(!is.null(likert_indicator)) {
    if(!is.character(likert_indicator)) {
      rlang::abort(glue::glue("The likert_indicator object is not a character string."))
    }
  }

  # Detect numeric questions
  numeric_variable_names <- column_details %>%
    dplyr::filter(stringr::str_to_lower(type) == "numeric") %>%
    dplyr::pull(column_names)

  # If likert inficator is null, make it a empty vector
  if(is.null(likert_indicator)) {
    likert_variable_names <- c()
  } else {
    # Otherwise detect any variables that start with the likert indicator
    filter_for_likert_var_names <- stringr::str_detect(column_details$column_names, paste0("^", stringr::str_to_lower(likert_indicator)))

    # Obtain those variable names
    likert_variable_names <- column_details$column_names[filter_for_likert_var_names]
  }

  # combine the numeric and likert variables
  numeric_variable_names <- c(numeric_variable_names, likert_variable_names)

  # Convert variables from that vector to numeric
  data_used <- data_used %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(numeric_variable_names),
        as.numeric,
        # This provides the naming convention to use
        .names = "NUM__{.col}")
    )

  if(!is.null(likert_indicator)) {
    # Calculate total for likert scale
    data_used$NUM__likert_total <- data_used %>%
      select(starts_with(paste0("NUM__", likert_indicator))) %>%
      rowMeans()
  }

  # Return the data frame for the survey data
  return(data_used)
}

# POWER BI
#' process_numeric_vars_power_bi
#'
#' @param data data
#' @param column_workbook_list column_workbook_list
#' @param name_of_column_details name_of_column_details
#'
#' @return data_used
#' @export
#'
process_numeric_vars_power_bi <- function(data, column_workbook_list, name_of_column_details) {

  # Prep the data
  data_used <- data

  # Create the df that just contains the column details
  column_details <- column_workbook_list[[name_of_column_details]]

  numeric_column_details <- column_details %>%
    dplyr::filter(!is.na(.data[["scale_names"]]) | .data$type == "numeric")

  numeric_column_details_names <- numeric_column_details %>% dplyr::pull(column_names)

  # See if numeric type should be run
  numeric_included <- "numeric" %in% column_details$type | any(!is.na(column_details$scale_names))

  if(numeric_included == TRUE) {

    purely_numeric_vars <- numeric_column_details %>%
      dplyr::filter(is.na(.data[["label_info"]])) %>%
      dplyr::pull(column_names)

    data_used <- data_used %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(numeric_column_details_names),
          as.numeric,
          # This provides the naming convention to use
          .names = "NUM__{.col}")
      )

    variables_needs_to_be_reverse_coded <- numeric_column_details %>%
      dplyr::filter(stringr::str_detect(.data[["scale_names"]], "_r$")) %>%
      dplyr::pull(column_names)

    variables_needs_to_be_reverse_coded_labels <- numeric_column_details %>%
      dplyr::filter(stringr::str_detect(.data[["scale_names"]], "_r$")) %>%
      dplyr::pull(label_info)

    for(i in variables_needs_to_be_reverse_coded) {
      label_for_revers_coded_var <- paste0("NUM__REVERSE_CODED__", i)
      label_for_original_numeric <- paste0("NUM__", i)
      fix_label_for_orig <- paste0 <- paste0("ORIG_NUM__", i)
      length_levels_for_factor <- length(levels(data_used[[i]]))

      numeric_variable <- as.numeric(data_used[[i]])

      reordered_numeric_variable <- (length_levels_for_factor + 1) - numeric_variable

      data_used[fix_label_for_orig] <- data_used[label_for_original_numeric]
      data_used <- data_used %>% dplyr::select(-c(all_of(label_for_original_numeric)))
      data_used[label_for_revers_coded_var] <- reordered_numeric_variable
    }

    scales_details <- numeric_column_details %>%
      dplyr::filter(!is.na(.data[["scale_names"]]))

    scales_details$var_names_for_scaling <- dplyr::case_when(
      stringr::str_detect(scales_details$scale_names, "_r$") ~ paste0("NUM__REVERSE_CODED__", scales_details$column_names),
      !is.na(scales_details$scale_names) ~ paste0("NUM__", scales_details$column_names)
    )

    scales_details$final_scale_names <- stringr::str_replace(scales_details$scale_names, "_r$","")
    unique_scale_names_to_use <- unique(scales_details$final_scale_names)

    for(i in unique_scale_names_to_use) {
      label_for_the_score <- paste0("AVERAGE_SCORE__", snakecase::to_snake_case(i))
      final_scale_variable_names <- scales_details %>%
        dplyr::filter(.data[["final_scale_names"]] == i) %>%
        dplyr::pull(var_names_for_scaling)

      data_used[label_for_the_score] <- data_used %>%
        dplyr::select(starts_with(final_scale_variable_names)) %>%
        rowMeans()
    }

    orig_num_var_names <- data_used %>%
      dplyr::select(
        starts_with("ORIG_NUM")
      ) %>% colnames()

    if(length(orig_num_var_names) > 0) {
      for(i in seq_along(orig_num_var_names)){
        iteration <- i
        converted_label <- orig_num_var_names[[iteration]]
        add_label <- stringr::str_replace(converted_label, "ORIG_", "")
        data_used[[add_label]] <- data_used[[converted_label]]
        data_used <- data_used %>% dplyr::select(-all_of(converted_label))
      }
    }

    return(data_used)
  }
}


#' Process the data for all of the select all questions
#'
#' @param column_workbook_list List with all of the named vectors
#' @param df data frame that contains the survey data
#' @param column_details_question_type_df data frame that contains the column details for a specified question type
#'
#' @return df
#' @export
#'
process_select_all_vars <- function(df, column_details_question_type_df, column_workbook_list) {
  # Check that df is a data frame
  if(!is.data.frame(df)) {
    rlang::abort(glue::glue("The `df` object is not a data frame."))
  }

  # Check the column details df data fram
  if(!is.data.frame(column_details_question_type_df)) {
    rlang::abort(glue::glue("The `column_details_question_type_df` object is not a data frame."))
  }

  # Check if it is a list
  if(!is.list(column_workbook_list)) {
    rlang::abort(glue::glue("The `column_workbook_list` object is not a list."))
  }

  # Detect sa questions with text responses
  select_all_text_vars <- column_details_question_type_df %>%
    dplyr::filter(type == "sa_other") %>%
    dplyr::pull(column_names)

  # If there are sa other variables
  if(length(select_all_text_vars) > 0) {
    # Copy them under a new name
    for(i in select_all_text_vars){
      df[[paste0("text_", i)]] <- df[[i]]
    }
  }

  # Extract the names of the select all questions
  SELECT_ALL_VARS <- column_details_question_type_df$column_names %>%
    stringr::str_extract("^.+__") %>%
    unique()

  # Loop through all select all questions and process them
  for(i in SELECT_ALL_VARS) {
    df <- process_select_all_question(df, i)
  }

  #Return the df
  return(df)
}


#POWER BI
#' process_select_all_vars_power_bi
#'
#' @param data data
#' @param column_workbook_list column_workbook_list
#' @param name_of_column_details name_of_column_details
#'
#' @return df_to_use
#' @export
#'

process_select_all_vars_power_bi <- function(
    data,
    column_workbook_list,
    name_of_column_details) {

  df_to_use <- data
  column_details <- column_workbook_list[[name_of_column_details]]

  # Detect sa questions with text responses
  select_all_text_vars <- column_details %>%
    dplyr::filter(type == "sa_other") %>%
    dplyr::pull(column_names)

  # If there are sa other variables
  if(length(select_all_text_vars) > 0) {
    # Copy them under a new name
    for(i in select_all_text_vars){
      df_to_use[[paste0("text_", i)]] <- df_to_use[[i]]
    }
  }

  # Extract the names of the select all questions
  SELECT_ALL_var_names <- column_details %>%
    dplyr::filter(stringr::str_detect(.data[["type"]], "^sa")) %>%
    dplyr::pull(column_names)

  SELECT_ALL_var_labels <- column_details %>%
    dplyr::filter(stringr::str_detect(.data[["type"]], "^sa")) %>%
    dplyr::pull(label_info)

  unique_SELECT_ALL_var_labels <- unique(SELECT_ALL_var_labels)

  # Loop through all select all questions and process them
  for(i in unique_SELECT_ALL_var_labels) {

    # Obtain the variable names for this question
    list_of_variable_names <- df_to_use %>%
      dplyr::select(tidyselect::starts_with(i)) %>% colnames()

    # Check if all columns are missing data
    is_all_column_data_missing_for_row <- bkissell::check_if_all_are_missing_from_row(df_to_use, list_of_variable_names)

    # Go though each variable
    purrr::walk(list_of_variable_names, ~ {

      # Get the index for the var
      index_for_var_to_change <- which(colnames(df_to_use) == .x)

      # Select the variable
      vector_to_change <- df_to_use[,index_for_var_to_change][[1]]

      # Convert those that are missing everything to NA, but change all other NAs to zero
      df_to_use[,index_for_var_to_change] <<- bkissell::process_a_select_all_response_as_ones_and_zeros(
        is_all_column_data_missing_for_row,
        df_to_use[[.x]]
      )
    })
  }

  return(df_to_use)
}


#' process_survey_data_for_single_power_bi
#'
#' @param survey_directory_path_pb survey_directory_path_pb
#' @param column_workbook_list_pb column_workbook_list_pb
#' @param survey_file_ext survey_file_ext
#' @param survey_datetime_format_pattern survey_datetime_format_pattern
#' @param survey_version_name survey_version_name
#' @param convert_numeric_age_to_age_group convert_numeric_age_to_age_group
#' @param spellcheck_column_path_pb spellcheck_column_path_pb
#' @param name_of_column_details name_of_column_details
#'
#' @return survey_data_for_power_bi_single
#' @export
#'

process_survey_data_for_single_power_bi <- function(
    survey_directory_path_pb,
    column_workbook_list_pb,
    survey_file_ext,
    survey_datetime_format_pattern,
    survey_version_name,
    convert_numeric_age_to_age_group,
    spellcheck_column_path_pb,
    name_of_column_details = "column_details"
) {

  # Obtain the column details
  column_details <- column_workbook_list_pb[[name_of_column_details]]

  # Read in the file
  survey_data_for_power_bi_single <- read_survey_data_for_power_bi_single(
    survey_directory_path = survey_directory_path_pb,
    file_ext = survey_file_ext, format_pattern = survey_datetime_format_pattern,
    survey_version_name = survey_version_name,
    column_workbook_list = column_workbook_list_pb
  ) %>% suppressWarnings()

  # Convert Age
  if(convert_numeric_age_to_age_group == TRUE) {
    survey_data_for_power_bi_single$age_group <- convert_age_to_generation(age_numeric = survey_data_for_power_bi_single$age, year_of_data_collection = project_year)
  }

  # Fix Spell checked columns
  survey_data_for_power_bi_single <- bkissell::replace_spelling_with_excel_workbook(survey_data_for_power_bi_single, spellcheck_column_path_pb)

  # Process the net promotor score
  survey_data_for_power_bi_single <- bkissell::capwcw_process_net_promoter(data = survey_data_for_power_bi_single, column_details)

  # Process the Multiple Choice
  survey_data_for_power_bi_single <- bkissell::convert_vars_to_factors_with_workbook(
    data = survey_data_for_power_bi_single,
    column_workbook_list = column_workbook_list_pb,
    name_of_column_details = name_of_column_details)

  # Process the Select All Variables
  survey_data_for_power_bi_single <- bkissell::process_select_all_vars_power_bi(
    data = survey_data_for_power_bi_single,
    column_workbook_list = column_workbook_list_pb,
    name_of_column_details = name_of_column_details)

  # Process the Numeric Variables
  survey_data_for_power_bi_single <- bkissell::process_numeric_vars_power_bi(
    data = survey_data_for_power_bi_single,
    column_workbook_list = column_workbook_list_pb,
    name_of_column_details = name_of_column_details)

  # Return the processed data
  return(survey_data_for_power_bi_single)
}



# Q -----------------------------------------------------------------------


# R -----------------------------------------------------------------------




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


#' read_csv_in_zip_single
#'
#' @param connection_to_zip_files connection_to_zip_files
#' @param initial_column_names_for_version initial_column_names_for_version
#'
#' @return survey_data
#' @export
#'
read_csv_in_zip_single <- function(connection_to_zip_file, initial_column_names_for_version) {

  df <- readr::read_csv(connection_to_zip_file, col_names = initial_column_names_for_version, show_col_types = FALSE)
  #
  #
  # list_of_file_information <- list(connection_to_zip_files, initial_column_names_for_version)
  # # |>
  # #   purrr::flatten()
  #
  # # Read in the data
  # survey_data <- purrr::pmap(list_of_file_information,  ~ {
  #
  #
  #   if(length(..2) == 1) {
  #
  #     df <- dplyr::slice(df, -1)
  #
  #   } else if(length(..2) > 1){
  #
  #     df <- dplyr::slice(df, -1:-2)
  #
  #   }
  #
  #   df
  #
  # })

  return(survey_data)
}

#' read_survey_data_for_power_bi_single
#'
#' @param survey_directory_path survey_directory_path
#'
#' @return survey_data_list
#' @export
#'
read_survey_data_for_power_bi_single <- function(
    survey_directory_path,
    file_ext = ".zip", format_pattern = "_[0-9]{8}_[0-9]{4}",
    survey_version_name,
    column_workbook_list
){

  # Obtain the newest version from each directory
  newest_files_for_project <- bkissell::obtain_newest_file_per_directory(
    survey_directory_path,
    file_ext = file_ext, format_pattern = format_pattern)

  # Obtain the paths for the column names excel files
  version_column_names_path <- bkissell::obtain_column_name_path(survey_directory_path)

  # Create the column details list
  column_details <- column_workbook_list$column_details

  col_names_to_use <- column_details$column_names
  col_types_to_use <- column_details$csv_data_type

  # Create Connection to zip files
  connection_to_zip_file <- obtain_the_connection_to_zip_file(newest_files_for_project)

  survey_data_read <- readr::read_csv(
    connection_to_zip_file,
    col_names = col_names_to_use,
    col_types = paste0(col_types_to_use, collapse = ""),
    show_col_types = FALSE,
    skip = 2)

  survey_data_read$version_name <- survey_version_name

  survey_data_read$wave_info <- basename(survey_directory_path)

  connection_to_zip_file_collector_list <- obtain_the_connection_to_zip_file_collector_list(newest_files_for_project)

  collector_list_read <- try(readr::read_csv(connection_to_zip_file_collector_list, show_col_types = FALSE), silent = TRUE)

  if(!any(class(collector_list_read) == "try-error")) {
    collector_list_read <- collector_list_read %>%
      dplyr::select(CollectorID, audience_type = Title)

    collector_list_read$CollectorID <- as.character(collector_list_read$CollectorID)

    survey_data_read <- survey_data_read %>%
      dplyr::left_join(collector_list_read, by = c("collector_id" = "CollectorID"))
  } else {
    survey_data_read$audience_type <- "Audience Type Was Not Collected in Wave 1"
  }

  if(!"RID" %in% names(survey_data_read)) {
    stop("RID must be included in the column names file. Please change the name of the respondent_id to the RID")
  }

  # Return the df
  # return(survey_data_read)
  survey_data_read

}


#' read_survey_data_for_power_bi
#'
#' @param survey_directory_path survey_directory_path
#'
#' @return survey_data_list
#' @export
#'
read_survey_data_for_power_bi <- function(survey_directory_path){

  # Create Version Directories
  version_directories <- prepare_version_directory_paths(survey_directory_path)

  # Obtain the newest version from each directory
  newest_files_for_project <- obtain_newest_file_per_directory(version_directories)

  # Obtain the paths for the column names excel files
  version_column_names_paths <- obtain_column_name_paths_for_all_survey_versions(version_directories)

  # Check if the column names excel file exists for each version
  the_column_names_file_exists <- check_if_the_column_names_file_exists(version_column_names_paths)

  # Obtain appropriate initial column names
  initial_column_names_for_version <- obtain_initial_column_names_for_version(
    version_column_names_paths,
    the_column_names_file_exists
  )

  # Create Connection to zip files
  connection_to_zip_files <- obtain_connection_to_zip_files_for_versions(newest_files_for_project)

  # Read in data stored in the the csv
  survey_data_list <- read_csv_in_zip(connection_to_zip_files, initial_column_names_for_version)

  # Stop the program if column files do not exist.
  if_no_csv_for_colnames_make_one(
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

  survey_data_list <- purrr::map(survey_data_list, ~{
    df <- .x

    if("RID" %in% names(df)) {
      df$RID <- df$RID
    } else if("Respondent ID" %in% names(df)) {
      df$RID <- df$`Respondent ID`
    } else if("respondent_id" %in% names(df)) {
      df$RID <- df$respondent_id
    }

    df
  })

  survey_data_list <- survey_data_list %>% purrr::set_names(version_name)

  # Return the df
  return(survey_data_list)
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

# POWERBI
#' replace_spelling_with_excel_workbook
#'
#' @param df df
#' @param spellchecked_path spellchecked_path
#'
#' @return df_to_fix
#' @export
#'
replace_spelling_with_excel_workbook <- function(df, spellchecked_path) {
  if(file.exists(spellchecked_path)) {

    df_to_fix <- df
    named_spellcheck_workbook_sheets <- readxl::excel_sheets(spellchecked_path)

    get_spellchecked_data <- function(df, path, sheet_name) {
      # Read in the spellchecked data
      sc_df <- readxl::read_excel(path, sheet = sheet_name)

      # Remove the column that will be replaced
      df <- df %>% dplyr::select(!{{sheet_name}})

      # Convert the RID to numeric for both files
      df$RID <- as.numeric(df$RID)
      sc_df$RID <- as.numeric(sc_df$RID)

      # Join the files back together
      df <- df %>% dplyr::left_join(sc_df, by = "RID")

      # Return the object
      return(df)
    }

    for(i in named_spellcheck_workbook_sheets) {
      df_to_fix <- get_spellchecked_data(df = df_to_fix, spellchecked_path, i)
    }

    df_to_fix$RID <- as.character(df_to_fix$RID)
    return(df_to_fix)
  } else {
    # If the spellcheck files does not exist, just return the original df.
    return(df)
  }
}

### CHECK THIS ONE aS IT SEEMS WRONG
#' run_create_table_with_var_names_and_orders
#'
#' @param response_var_type response_var_type
#' @param df df
#' @param response_var_names response_var_names
#' @param response_var_used_order response_var_used_order
#' @param grouping_vars grouping_vars
#' @param grouping_var_used_order grouping_var_used_order
#'
#' @return power_bi_table
#' @export
#'
run_create_table_with_var_names_and_orders <- function(
    response_var_type,
    df,
    response_var_names,
    response_var_used_order,
    grouping_vars,
    grouping_var_used_order) {

  rv_names_and_order_df <- data.frame(
    response_var_names = response_var_names,
    response_var_used_order = response_var_used_order
  )

  gv_names_and_order_df <- data.frame(
    grouping_vars = grouping_vars,
    grouping_var_used_order = grouping_var_used_order
  )

  parameter_df <- expand.grid(response_var_names, grouping_vars)
  colnames(parameter_df) <- c("response_var_names", "grouping_vars")

  parameter_df <- parameter_df %>%
    dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
    dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

  n_iterations <- length(parameter_df$response_var_names)
  pb <- progress::progress_bar$new(total = n_iterations)

  table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{
    t <- create_table_single(
      df = df,
      response_var_name = parameter_df$response_var_names[[.x]],
      grouping_var_name = parameter_df$grouping_vars[[.x]],
      response_var_type = response_var_type)

    pb$tick()

    t$response_var_used <- parameter_df$response_var_names[[.x]]
    t$response_var_used_order <- parameter_df$response_var_used_order[[.x]]
    t$grouping_var_used <- parameter_df$grouping_vars[[.x]]
    t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[.x]]
    t
  })

  power_bi_table <- purrr::map2_df(response_var_names, response_var_used_order, ~{
    single_tables <- run_create_table_with_var_names_and_orders_part(
      df = df, response_variable_name = .x,
      response_var_type = response_var_type, grouping_vars = grouping_vars,
      grouping_var_used_order = grouping_var_used_order)

    single_tables$response_var_used <- .x
    single_tables$response_var_used_order <- .y
    single_tables
  })
  power_bi_table
}


# S -----------------------------------------------------------------------

#' separate_most_least_single
#'
#' @param data data
#' @param grouping_vars grouping_vars
#'
#' @return most_least_table
#' @export
#'
separate_most_least_single <- function(data, grouping_vars){

  most_least_table <- data %>%
    tidyr::pivot_longer(
      cols = !c("RID", all_of(grouping_vars)),
      values_to = "Rating_selection",
      names_to = "Question_name"
    )

  most_least_table$condition_name <- most_least_table$Question_name %>%
    stringr::str_replace("^ml__", "") %>%
    stringr::str_extract("^.+_") %>%
    stringr::str_replace_all("_", "") %>%
    stringr::str_to_title()

  most_least_table$item_name <- most_least_table$Question_name %>%
    stringr::str_replace("^ml__", "") %>%
    stringr::str_replace("^.+_", "") %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_title()

  most_least_table$least <- ifelse(stringr::str_detect(most_least_table$Rating_selection, "^Least"), 1, 0)
  most_least_table$most <- ifelse(stringr::str_detect(most_least_table$Rating_selection, "^Most"), 1, 0)

  most_least_table <- most_least_table %>%
    tidyr::pivot_longer(cols = c("least", "most"), names_to = "direction", values_to = "selection")

  return(most_least_table)
}

#' separate_most_least_multiple
#'
#' @param data data
#' @param ml_indicator ml_indicator
#' @param grouping_vars grouping_vars
#'
#' @return most_least_data_frame
#' @export
#'
separate_most_least_multiple <- function(data, ml_indicator, grouping_vars) {
  response_item_names <- data %>% dplyr::select(tidyselect::starts_with({{ml_indicator}})) %>% colnames()

  most_least_data_frame <- purrr::map_df(response_item_names, ~ {
    separate_most_least_single(data = data, var_name = .x, grouping_vars = grouping_vars)
  })

  return(most_least_data_frame)
}


#' set_project_working_directory
#'
#' @param project_year project_year
#' @param storage_platform storage_platform
#' @param storage_platform_name storage_platform_name
#' @param group_dir_name group_dir_name
#' @param jobs_folder_name jobs_folder_name
#' @param project_folder_name project_folder_name
#'
#' @return working_directory_path
#' @export
#'
set_project_working_directory <- function(
    storage_platform = "dropbox",
    storage_platform_name = "TCM Dropbox",
    group_dir_name = "04 MDM Neuro-Fundraising Lab",
    jobs_folder_name = "00 Jobs",
    project_year,
    project_folder_name
) {
  # What is the name of the username for the person running the program?
  computer_user <- Sys.info()[["user"]]

  # Talia's computer is set up in a weird way, where it is under her name and Steves. This should fix the problem
  computer_user <- ifelse(computer_user == "Talia Abbott", "SJAga", computer_user)

  # Create the path to the storage platform
  storage_platform_path <- paste0("C:/Users/", computer_user, "/", storage_platform_name)

  # When dropbox is used, you must include an additional username into the path
  if(storage_platform == "dropbox") {
    drop_box_user <- list.files(storage_platform_path)[!list.files(storage_platform_path) %in% c("desktop.ini", "TCM Team Folder")]
    storage_platform_path <- paste0(storage_platform_path, "/", drop_box_user)
  }

  # Create the working directory that will be used
  working_directory_path <- file.path(
    storage_platform_path, group_dir_name, jobs_folder_name,
    project_year, project_folder_name)

  # If this path exists
  if(file.exists(working_directory_path)) {
    # set it as the working directory
    setwd(working_directory_path)
  } else {
    # Otherwise throw an error
    stop(paste0("File directory was not found: ", working_directory_path))
  }

  # Return the path that will be used
  return(working_directory_path)
}


#' Select all of the data that should be provided in the text df
#'
#' @param data data frame with the survey data
#' @param variables_to_include Character vector with names of the variables to keep
#'
#' @return text_survey_data
#' @export
#'
select_text_survey_data <- function(data, variables_to_include) {
  variables_to_include <- c(variables_to_include, "study_version")
  # Run Checks
  if(!is.data.frame(data)) abort_bad_argument("data", must = "be data.frame", not = data)
  if(!is.character(variables_to_include)) abort_bad_argument("variables_to_include", must = "be character", not = variables_to_include)

  # Filter for needed variables
  text_survey_data <- data %>%
    dplyr::select(
      tidyselect::all_of(variables_to_include),
      starts_with("text_")
    )

  #Return the survey data
  return(text_survey_data)
}



#' Simple function to convert first to numeric and then to sum the numbers
#'
#' @param x numeric variable
#'
#' @return the_sum
#' @export
#'
sum2 <- function(x) {
  # Convert to numeric
  x <- as.numeric(x)

  # Sum the numbers
  the_sum <- sum(x, na.rm = TRUE)

  # Return the sum
  return(the_sum)
}

# T -----------------------------------------------------------------------


# U -----------------------------------------------------------------------

#' Use column workbook to convert vars to factors
#'
#' @param df data frame that contains the survey data
#' @param column_details_question_type_df data frame that contains the column details for a specified question type
#' @param column_workbook_list ist that contains the named vectors
#' @param question_type_indicator Character string that tells the program what data type is being evaluated and processed.
#'
#' @return df
#' @export
#'
use_column_workbook_to_convert_vars_to_factors <- function(df, column_details_question_type_df, column_workbook_list, question_type_indicator){

  # Check that df is a data frame
  if(!is.data.frame(df)) {
    rlang::abort("not_a_data_frame", glue::glue("The `df` object is not a data frame."))
  }

  # Check that column_details_question_type_df is a df
  if(!is.data.frame(column_details_question_type_df)) {
    rlang::abort(glue::glue("The `column_details_question_type_df` object is not a data frame."))
  }

  #Check if column_workbook_list is a list
  if(!is.list(column_workbook_list)) {
    rlang::abort(glue::glue("The `column_workbook_list` object is not a list."))
  }

  # Check that the indicator is a character string
  if(!is.character(question_type_indicator)) {
    rlang::abort(glue::glue("The question_type_indicator object is not a character string."))
  }

  # Check that indicator is a valid option
  appropriate_choice_indicators <- c("mc")
  if(!(question_type_indicator %in% appropriate_choice_indicators)) {
    rlang::abort(glue::glue("`question_type_indicator` is '{question_type_indicator}', which is not an appropriate type indicator. Please use one of the following: {paste0(appropriate_choice_indicators, collapse = ", ")}"))
  }

  # Create a list that contains all variables that you want to turn into factors
  variables_names <- column_details_question_type_df$column_names

  # Create a second list that contains the named vector that specifies the appropriate levels
  named_vectors <- column_workbook_list[column_details_question_type_df$label_info]

  # How many are in the vectors list
  named_vectors_length <- length(named_vectors)

  # How many are in the variable names column
  variables_names_length <- length(variables_names)

  # Make sure the names and the vector list are of the same length
  if(!(named_vectors_length == variables_names_length)) {
    rlang::abort(glue::glue("The `named_vectors_length` ({named_vectors_length}) is not the same as the `variables_names_length` ({variables_names_length})."))
  }

  # If the indicator is "mc"
  if(question_type_indicator == "mc") {
    # Convert variables to factors - Note that'ord' is an ordered factor
    df <- factor_vars_with_named_vectors(
      df = df,
      list_with_variable_names_to_reorder = variables_names,
      list_with_named_vectors = named_vectors
    )
  }

  # Return the df
  return(df)
}
# V -----------------------------------------------------------------------


# W -----------------------------------------------------------------------


# X -----------------------------------------------------------------------


# Y -----------------------------------------------------------------------


# Z -----------------------------------------------------------------------











































# Development -------------------------------------------------------------




#' create_order_information_lookup_table
#'
#' @param column_workbook_list column_workbook_list
#' @param name_of_column_details name_of_column_details
#'
#' @return full_lookup_table
#' @export
#'
create_order_information_lookup_table <- function(column_workbook_list, name_of_column_details) {
  # Obtain the column details
  column_details <- column_workbook_list[[name_of_column_details]]

  # Obtain a list of all of the variable label types
  names_of_all_sheets_in_column_workbook_list <- names(column_workbook_list)

  # Remove the column details sheet
  names_of_all_sheets_for_variable_info <-
    names_of_all_sheets_in_column_workbook_list[names_of_all_sheets_in_column_workbook_list != name_of_column_details]

  # If there are duplicates in the sheets, throw an error
  if(length(names_of_all_sheets_for_variable_info) != length(unique(names_of_all_sheets_for_variable_info))){
    stop("Duplicates in column_workbook_list")
  }

  # Loop through every sheet in the column details workbook and create the lookup table
  lookup_table <- purrr::map_df(names_of_all_sheets_for_variable_info, ~{

    # Get the levels for that sheet
    var_label_info_levels <- names(column_workbook_list[[.x]])

    # Save which sheet was used
    var_label_info_used  <- rep(.x, length(var_label_info_levels))

    # Create a data frame with that info
    df_label_info <- data.frame(
      var_label_info_used = var_label_info_used,
      var_label_info_levels = var_label_info_levels
    )

    # and return it
    df_label_info
  })

  # Make sure there are no duplicates
  lookup_table <- dplyr::distinct(lookup_table)

  # Create the number for each level
  lookup_table$id_var_order <- factor(lookup_table$var_label_info_levels, unique(lookup_table$var_label_info_levels)) %>% as.numeric()

  # Custom adjustment of other label
  lookup_table$id_var_order <- ifelse(
    lookup_table$var_label_info_levels == "Other",
    9999,
    lookup_table$id_var_order
  )

  # Custom adjustement of none label
  lookup_table$id_var_order <- ifelse(
    lookup_table$var_label_info_levels == "None",
    9998,
    lookup_table$id_var_order
  )

  # Because the label and the name can be different, lets use the names in the lookup table
  df_all_vars_and_label_info <- column_details %>%
    dplyr::filter(!is.na(label_info)) %>%
    dplyr::select(column_names, label_info)

  # Go through each name
  full_lookup_table <- purrr::map_df(seq_along(df_all_vars_and_label_info$column_names), ~{
    # Filter for the relevant levels
    temp_lookup_table <- lookup_table %>%
      dplyr::filter(.data[["var_label_info_used"]] == df_all_vars_and_label_info$label_info[[.x]])

    # Add the correct name
    temp_lookup_table$variable_used <- df_all_vars_and_label_info$column_names[[.x]]

    # Return the table
    temp_lookup_table
  }, lookup_table)


  # Add the variable used column to the lookup table
  lookup_table$variable_used <- lookup_table$var_label_info_used

  # Join these together and make sure there are no duplicates
  full_lookup_table <- full_lookup_table %>%
    rbind(lookup_table) %>% dplyr::distinct()

  # Create All versions for the grouping variables
  All_label_table <- data.frame(
    var_label_info_used = unique(full_lookup_table$variable_used),
    var_label_info_levels= rep("All", length(unique(full_lookup_table$variable_used))),
    id_var_order = rep(0, length(unique(full_lookup_table$variable_used))),
    variable_used =  unique(full_lookup_table$variable_used)
  )

  # Join these together and make sure there are no duplicates
  full_lookup_table <- full_lookup_table %>%
    rbind(All_label_table) %>%
    dplyr::distinct()

  # Return the table
  return(full_lookup_table)
}



#' adjust_order_information_lookup_table_per_type
#'
#' @param table table
#' @param order_information_lookup_table order_information_lookup_table
#' @param type type
#'
#' @return table_2
#' @export
#'
adjust_order_information_lookup_table_per_type <- function(table, order_information_lookup_table, type = "grouping_var_"){

  # Prepare the variables you will be ajusting
  table_2 <- table
  order_information_lookup_table_2 <- order_information_lookup_table

  # Create the labels according to whether it is response or grouping
  order_label <- paste0(type, "order")
  levels_label <- paste0(type, "levels")
  used_label <- paste0(type, "used")

  # Create an id for the table
  table_2$var_id_to_match <- paste0(table_2[[used_label]], "_", table_2[[levels_label]])

  # Create an id for the lookup table
  order_information_lookup_table_2$var_id_to_match <- paste0(order_information_lookup_table_2$variable_used, "_", order_information_lookup_table_2$var_label_info_levels)

  # Rename the order variable
  order_information_lookup_table_2[[order_label]] <- order_information_lookup_table_2[["id_var_order"]]

  # Select only needed variables
  order_information_lookup_table_2 <- order_information_lookup_table_2 %>% dplyr::select("var_id_to_match", all_of(order_label))

  # Detect missing variables
  missing_variables <- setdiff(table_2$var_id_to_match, order_information_lookup_table_2$var_id_to_match)

  # Join the two tables together
  table_2 <- table_2 %>%
    dplyr::select(-any_of(order_label)) %>%
    dplyr::left_join(order_information_lookup_table_2, by = "var_id_to_match")

  # Create an order number for the missing variables so it does not get messed up
  counter = 1
  for(i in missing_variables){
    table_2[[order_label]][table_2[["var_id_to_match"]] == i] <- max(table_2[[order_label]], na.rm = TRUE) + counter
    counter <- counter + 1
  }

  # Return the adjusted table
  return(table_2)
}






# I want to rewrite this one. I do not think I am currently using it.
#' Create a single table Multiple Choice
#'
#' @param df data frame with the survey data
#' @param response_var_name character with the response var name
#' @param grouping_var_name character with the grouping var name
#'
#' @return table
#' @export
#'
create_table_single_mc_DEV <- function(df, response_var_name, grouping_var_name){

  # Run Checks
  if(!is.data.frame(df)) abort_bad_argument("df", must = "be data.frame", not = df)
  if(!is.character(response_var_name)) abort_bad_argument("response_var_name", must = "be character", not = response_var_name)
  if(!is.character(grouping_var_name)) abort_bad_argument("grouping_var_name", must = "be character", not = grouping_var_name)

  ## Create Helper Functions
  cts_create_grouping_and_order_var <- function(df, grouping_var_name, order_grouping_var) {
    df <- df %>%
      dplyr::mutate(
        grouping_var_levels = as.character(.data[[{{grouping_var_name}}]]),
        grouping_var_order = .data[[order_grouping_var]]
      )
    return(df)
  }

  cts_create_response_and_order_var <- function(df, response_var_name, order_variable) {
    df <- df %>%
      dplyr::mutate(
        response_var_levels = as.character(.data[[{{response_var_name}}]]),
        response_var_order = .data[[{{order_variable}}]]
      )
    return(df)
  }

  cts_filter_out_NAs_from_df <- function(df, var_name) {
    df <- df %>%
      dplyr::filter(!is.na(.data[[{{var_name}}]]), .data[[{{var_name}}]] != "NA")
    return(df)
  }

  cts_add_group_by <- function(df, var_name) {
    df <- df %>% dplyr::group_by(pick(all_of(c({{var_name}}))), .add = TRUE)
    return(df)
  }

  cts_calculate_counts <- function(df) {
    table <- df %>%
      # Get the counts for the grouping variable by the response variables
      dplyr::summarize(counts = dplyr::n(), .groups = "drop")
    return(table)
  }

  cts_calculate_group_counts_and_percentages <- function(table) {
    table <- table %>%
      dplyr::mutate(
        group_n = sum(counts),
        percentage = (counts / group_n)
      )
    return(table)
  }

  cts_calculate_overall_sample_size <- function(df, response_var_name) {
    df %>% dplyr::filter(!is.na(.data[[{{response_var_name}}]])) %>% nrow()
  }

  find_var_names_that_start_with <- function(df, starts_with) {
    df %>% dplyr::select(
      tidyselect::starts_with({{starts_with}})) %>%
      colnames()
  }

  cts_pivot_longer_count_type <- function(df, response_var_name, count_type) {
    table_counts <- df  %>%
      tidyr::pivot_longer(
        cols = all_of(list_of_variable_names),
        names_to = "response_var_levels",
        values_to = {{count_type}})
    return(table_counts)
  }

  # Make sure the df is not grouped by anything
  df <- df %>% ungroup()

  table(df[[response_var_name]], df[[grouping_var_name]])
  ptable(df[[response_var_name]], df[[grouping_var_name]])
  grouping_var_levels
  response_var_levels
  counts
  # Add VAR_ORDER for both grouping and response vars to the df it is working on
  df <- prepare_VAR_ORDER_variables_single(df, response_var_name, grouping_var_name)

  df <- cts_create_grouping_and_order_var(df, grouping_var_name, order_grouping_var)
  df <- cts_create_response_and_order_var(df, response_var_name, order_variable)

  variable_names_to_group <- c("grouping_var_levels", "response_var_levels", "grouping_var_order", "response_var_order")
  for(i in variable_names_to_group) {df <- cts_filter_out_NAs_from_df(df, var_name = i)}
  for(j in variable_names_to_group) {df <- cts_add_group_by(df, var_name = j)}
  table <- cts_calculate_counts(df)
  table <- cts_add_group_by(table, var_name = "grouping_var_levels")
  table <- cts_calculate_group_counts_and_percentages(table)
  table$grouping_var_used <- {{grouping_var_name}}
  table$response_var_used <- {{response_var_name}}
  table <- table %>% dplyr::ungroup()

  # Create Overall Table
  df <- df %>% ungroup()
  variable_names_to_group_for_all <- c("response_var_levels", "response_var_order")
  for(i in variable_names_to_group_for_all) {df <- cts_filter_out_NAs_from_df(df, var_name = i)}
  for(j in variable_names_to_group_for_all) {df <- cts_add_group_by(df, var_name = j)}
  table_all <- cts_calculate_counts(df)
  table_all <- cts_calculate_group_counts_and_percentages(table_all)
  table_all$grouping_var_used <- "All"
  table_all$response_var_used <- {{response_var_name}}
  table_all <- table_all %>% dplyr::ungroup()
  table_all$grouping_var_levels <- "All"
  table_all$grouping_var_order <- 0

  # join Tables
  table_var_order <- c("grouping_var_levels", "response_var_levels", "grouping_var_order", "response_var_order", "counts", "group_n", "percentage", "grouping_var_used", "response_var_used")
  table <- table %>% dplyr::select(dplyr::all_of(table_var_order))
  table_all <- table_all %>% dplyr::select(dplyr::all_of(table_var_order))
  table <- table %>% rbind(table_all)
  df <- df %>% dplyr::ungroup()
  table$overall_sample_size_for_response_var <- cts_calculate_overall_sample_size(df, response_var_name)

  return(table)
}




#' create_power_bi_data_mc_CALCULATED_TABLES
#'
#' @param df df
#' @param column_workbook_list column_workbook_list
#' @param grouping_vars grouping_vars
#' @param name_of_column_details name_of_column_details
#'
#' @return wave_tables_df
#' @export
#'
create_power_bi_data_mc_CALCULATED_TABLES <- function(
    df,
    column_workbook_list,
    grouping_vars,
    name_of_column_details
) {

  # Bring in the column details
  column_details <- column_workbook_list[[name_of_column_details]]

  # Create a list of variables that includes those not in the details sheet
  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    if(!(.x %in% column_details$column_names)){
      var <- .x
    } else {
      var <- column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info)
    }
    var
  })

  # Obtain the grouping var order
  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  # Get the multiple choice variables df
  multiple_choice_df <- extract_column_details_question_type_df(column_details_table = column_details, question_type_indicator = "mc")

  # get the mc column names
  response_var_names <- multiple_choice_df$column_names

  # Get the mc vector names
  response_var_used_vector_names <- multiple_choice_df$label_info

  # Get the response var order
  response_var_used_order <- factor(response_var_used_vector_names, unique(response_var_used_vector_names)) %>% as.numeric()

  # Create df with response var data
  rv_names_and_order_df <- data.frame(
    response_var_names = response_var_names,
    response_var_used_order = response_var_used_order
  )

  # Create df with groupiong var data
  gv_names_and_order_df <- data.frame(
    grouping_vars = grouping_vars,
    grouping_var_used_order = grouping_var_used_order
  )

  # Put together all combinations
  parameter_df <- expand.grid(response_var_names, grouping_vars)

  # Rename the columns
  colnames(parameter_df) <- c("response_var_names", "grouping_vars")

  # Join the tables creating the parameters df
  parameter_df <- parameter_df %>%
    dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
    dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

  # Make a function to obtain the mc calcualtions
  perform_calc_create_table_single_mc <- function(df, WaveName = "All Waves", parameter_df){
    # Obtain the number of iterations
    n_iterations <- length(parameter_df$response_var_names)
    # Create progress bar
    pb <- progress::progress_bar$new(total = n_iterations, format = glue::glue("Performing Calculations for Multiple Choice PBT for Wave: {WaveName}  [:bar] :percent eta: :eta"))
    # Loop through every possibility from the parameter df
    purrr::map_df(seq_along(parameter_df$response_var_names), ~{
      iteration <- .x
      # Obtain Grouped percentages
      t <- create_grouped_percentages_table(
        df = df,
        grouping_var_name = parameter_df$grouping_vars[[iteration]],
        response_var_name = parameter_df$response_var_names[[iteration]])

      # Add additional needed info to df
      t$response_var_used <- parameter_df$response_var_names[[iteration]]
      t$response_var_used_order <- parameter_df$response_var_used_order[[iteration]]
      t$grouping_var_used <- parameter_df$grouping_vars[[iteration]]
      t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[iteration]]
      t$wave <- WaveName

      # Indicate progress
      pb$tick()
      # Return the table that was made
      t
    })
  }

  # Make a df for every wave
  split_wave_dfs <- split(df, as.factor(df$wave_info))

  # Add the df with all data to that list
  all_dfs <- append(list("All Waves" = df), split_wave_dfs)

  # Obtain the calculations for every df in the list
  wave_tables_df <- purrr::map2_df(all_dfs, as.list(names(all_dfs)), ~{
    perform_calc_create_table_single_mc(df = .x, WaveName = .y, parameter_df = parameter_df)
  }, parameter_df)

  # Temp create these
  wave_tables_df$grouping_var_order <- NA
  wave_tables_df$response_var_order <- NA

  # Get rid of any nas
  wave_tables_df <- wave_tables_df %>%
    dplyr::filter(!is.na(response_var_levels)) %>%
    dplyr::filter(!is.na(grouping_var_levels))

  #Obtain the order information lookup table
  order_information_lookup_table <- create_order_information_lookup_table(column_workbook_list, name_of_column_details)

  # Fix the order numbers for response vars
  wave_tables_df <- adjust_order_information_lookup_table_per_type(
    table = wave_tables_df,
    order_information_lookup_table = order_information_lookup_table,
    type = "response_var_")

  # Fix the order numbers for grouping vars
  wave_tables_df <- adjust_order_information_lookup_table_per_type(wave_tables_df, order_information_lookup_table, type = "grouping_var_")
  # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "response_var", column_details = column_details, column_workbook_list = column_workbook_list)
  # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

  wave_tables_df$grouping_var_used  <- stringr::str_replace_all(wave_tables_df$grouping_var_used, "_", " ") %>% stringr::str_to_title()

  wave_tables_df$response_var_used  <- stringr::str_replace_all(wave_tables_df$response_var_used, "_", " ") %>% stringr::str_to_title()

  wave_tables_df$percentage[is.nan(wave_tables_df$percentage)] <- 0

  wave_tables_df <- wave_tables_df %>% dplyr::select(
    "wave", "grouping_var_levels", "response_var_levels", "grouping_var_order",
    "response_var_order", "counts", "group_n", "percentage",
    "grouping_var_used", "grouping_var_used_order", "response_var_used", "response_var_used_order", "overall_sample_size_for_response_var")

  return(wave_tables_df)
}


#' create_grouped_percentages_table
#'
#' @param df df
#' @param grouping_var_name grouping_var_name
#' @param response_var_name response_var_name
#' @param type type
#' @param group_n_table group_n_table
#'
#' @return grouped_percentage_table
#' @export
#'
create_grouped_percentages_table <- function(df, grouping_var_name, response_var_name, type = NULL, group_n_table = NULL) {
  # grouping_var_name = "ethnicity"
  # response_var_name = "gender"

  # # How many responses are there
  # if(type == "sa") {
  #   group_n_table
  #   df
  #   n_responses <- df[["n_responses"]][[1]]
  #   df <- df %>% dplyr::select(-n_responses)
  # } else {
  #   n_responses <- length(df[[response_var_name]])
  # }

  n_rows <- length(df[[response_var_name]])

  # What are the levels in the grouping variable
  grouping_var <- df[[grouping_var_name]]

  # What are the levels in the response variable
  response_var <- df[[response_var_name]]

  # Obtain the grouped counts
  group_level_counts <- table(grouping_var, response_var) %>% as.data.frame()

  # Rename the columns in the table
  colnames(group_level_counts) <- c("grouping_var_levels", "response_var_levels", "counts")

  # Create the All levels
  combined_group_levels <- rep("All",  n_rows)

  # Get the counts for the alls
  combined_group_level_counts <- table(combined_group_levels, response_var) %>% as.data.frame()

  # Rename the columns in the table
  colnames(combined_group_level_counts) <- c("grouping_var_levels", "response_var_levels", "counts")

  # Join these two tables together
  grouped_count_table <- group_level_counts %>%
    rbind(combined_group_level_counts)


  # Get the counts for the groups
  group_n_table_2 <- grouped_count_table |>
    dplyr::group_by(grouping_var_levels) |>
    dplyr::summarise(group_n = sum(counts))


  # Convert the grouping_var_levels_to_character
  group_n_table_2[["grouping_var_levels"]] <- as.character(group_n_table_2[["grouping_var_levels"]])

  n_responses <- group_n_table_2$group_n[group_n_table_2$grouping_var_levels == "All"]
  #   sum(group_n_table_2$group_n, na.rm = TRUE)
  #
  # df_add_all_group_n <- data.frame(
  #   group_n = n_responses
  # )
  #
  # df_add_all_group_n[[grouping_var_name]] <- "All"

  # group_n_table_2 <- group_n_table_2 %>%
  #   rbind(df_add_all_group_n)

  grouped_percentage_table <- grouped_count_table %>%
    dplyr::left_join(group_n_table_2, by = "grouping_var_levels")


  # if(type == "sa") {
  #   n_responses <- df[["n_responses"]][[1]]
  #   df <- df %>% dplyr::select(-n_responses)
  # }
  #
  # # Add these group counts to the table
  # grouped_percentage_table <- grouped_count_table %>%
  #   dplyr::left_join(group_n_table, by = "grouping_var_levels")

  # Add the total sample size as a column
  grouped_percentage_table$overall_sample_size_for_response_var <- n_responses

  # Obtain a rounded group percentage
  grouped_percentage_table$percentage <- round(grouped_percentage_table$counts / grouped_percentage_table$group_n, 4)

  # Name the variables appropriately
  grouped_percentage_table$grouping_var_used <- grouping_var_name
  grouped_percentage_table$response_var_used <- response_var_name

  # Return the table
  return(grouped_percentage_table)
}






# results <- microbenchmark(
#   create_grouped_percentages_table(df, grouping_var_name, response_var_name),
#   times = 50)



#' create_power_bi_data_sa_CALCULATED_TABLES
#'
#' @param df df
#' @param grouping_vars grouping_vars
#' @param column_workbook_list column_workbook_list
#' @param name_of_column_details name_of_column_details
#' @param use_the_question_text_for_variable_name use_the_question_text_for_variable_name
#'
#' @return table
#' @export
#'
create_power_bi_data_sa_CALCULATED_TABLES  <- function(
    df,
    column_workbook_list,
    grouping_vars,
    name_of_column_details,
    use_the_question_text_for_variable_name = TRUE
) {

  # Bring in the column details
  column_details <- column_workbook_list[[name_of_column_details]]

  # Create a list of variables that includes those not in the details sheet
  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    if(!(.x %in% column_details$column_names)){
      var <- .x
    } else {
      var <- column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info)
    }
    var
  })

  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  select_all_df <- extract_column_details_question_type_df(column_details_table = column_details, question_type_indicator = "sa")

  response_var_names <- select_all_df$column_names %>% stringr::str_extract( "^.+__") %>% unique() %>% stringr::str_replace("__$", "")

  response_var_used_order <- factor(response_var_names, unique(response_var_names)) %>% as.numeric()

  rv_names_and_order_df <- data.frame(
    response_var_names = response_var_names,
    response_var_used_order = response_var_used_order
  )

  gv_names_and_order_df <- data.frame(
    grouping_vars = grouping_vars,
    grouping_var_used_order = grouping_var_used_order
  )

  parameter_df <- expand.grid(response_var_names, grouping_vars)

  colnames(parameter_df) <- c("response_var_names", "grouping_vars")

  parameter_df <- parameter_df %>%
    dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
    dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")


  perform_calc_create_table_single_sa <- function(df, WaveName = "All Waves", parameter_df, column_workbook){
    # Obtain the number of iterations
    n_iterations <- length(parameter_df$response_var_names)
    # Create progress bar
    pb <- progress::progress_bar$new(total = n_iterations, format = glue::glue("Performing Calculations for Select All PBT for Wave: {WaveName}  [:bar] :percent eta: :eta"))
    # Loop through every possibility from the parameter df
    table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{

      iteration <- .x

      list_of_variable_names <- df %>% dplyr::select(
        tidyselect::starts_with(parameter_df$response_var_names[[iteration]])) %>%
        colnames()

      df_filtered <- df %>%
        dplyr::select(parameter_df$grouping_vars[[iteration]], all_of(list_of_variable_names)) %>%
        na.omit()

      df_filtered[[parameter_df$grouping_vars[[iteration]]]] <- as.character(df_filtered[[parameter_df$grouping_vars[[iteration]]]])

      df_long <- df_filtered %>%
        tidyr::pivot_longer(cols = - parameter_df$grouping_vars[[iteration]], names_to = parameter_df$response_var_names[[iteration]], values_to = "values") %>%
        dplyr::filter(values == 1) %>% dplyr::select(-values)

      new_response_var_labels <- column_workbook_list[[parameter_df$response_var_names[[iteration]]]]

      df_long[[parameter_df$response_var_names[[iteration]]]] <- factor(df_long[[parameter_df$response_var_names[[iteration]]]], new_response_var_labels, names(new_response_var_labels)) %>% as.character()

      # group_n_table <- df_filtered |>
      #   dplyr::group_by(.data[[parameter_df$grouping_vars[[iteration]]]]) |>
      #   dplyr::summarise(group_n = n())

      if(nrow(df_long) > 0 ) {
        # Obtain Grouped percentages
        t <- create_grouped_percentages_table(
          df = df_long,
          grouping_var_name = parameter_df$grouping_vars[[iteration]],
          response_var_name = parameter_df$response_var_names[[iteration]],
          type = "sa",
          group_n_table = NULL)

        # Add additional needed info to df
        t$response_var_used <- parameter_df$response_var_names[[iteration]]
        t$response_var_used_order <- parameter_df$response_var_used_order[[iteration]]
        t$grouping_var_used <- parameter_df$grouping_vars[[iteration]]
        t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[iteration]]
        t$wave <- WaveName

        # Indicate progress
        pb$tick()
        # Return the table that was made
        t
      } else {
        pb$tick()
        return(data.frame())
      }

    })
    return(table)
  }

  # Make a df for every wave
  split_wave_dfs <- split(df, as.factor(df$wave_info))

  # Add the df with all data to that list
  all_dfs <- append(list("All Waves" = df), split_wave_dfs)

  # Obtain the calculations for every df in the list
  wave_tables_df <- purrr::map2_df(all_dfs, as.list(names(all_dfs)), ~{
    perform_calc_create_table_single_sa(df = .x, WaveName = .y, parameter_df, column_workbook = column_workbook_list)
    # perform_calc_create_table_single_sa(df = all_dfs[[3]], WaveName = as.list(names(all_dfs))[[3]], parameter_df, column_workbook = column_workbook_list)
  }, parameter_df, column_workbook)

  # Temp create these
  wave_tables_df$grouping_var_order <- NA
  wave_tables_df$response_var_order <- NA

  # Get rid of any nas
  wave_tables_df <- wave_tables_df %>%
    dplyr::filter(!is.na(response_var_levels)) %>%
    dplyr::filter(!is.na(grouping_var_levels))

  # Obtain the order information lookup table
  order_information_lookup_table <- create_order_information_lookup_table(column_workbook_list, name_of_column_details)

  # Fix the order numbers for response vars
  wave_tables_df <- adjust_order_information_lookup_table_per_type(
    table = wave_tables_df,
    order_information_lookup_table = order_information_lookup_table,
    type = "response_var_")

  # Fix the order numbers for grouping vars
  wave_tables_df <- adjust_order_information_lookup_table_per_type(wave_tables_df, order_information_lookup_table, type = "grouping_var_")
  # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "response_var", column_details = column_details, column_workbook_list = column_workbook_list)
  # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

  unique_sa_variable_names <- unique(wave_tables_df$response_var_used)

  question_text_df <- column_details %>%
    dplyr::select(all_of(c("label_info", "question"))) %>%
    na.omit() %>%
    unique(by = "label_info") %>%
    dplyr::filter(.data[["label_info"]] %in% unique_sa_variable_names)

  if(use_the_question_text_for_variable_name == TRUE) {
    wave_tables_df$response_var_used <- factor(wave_tables_df$response_var_used, levels = question_text_df$label_info, labels = question_text_df$question)
  }

  wave_tables_df$grouping_var_used  <- stringr::str_replace_all(wave_tables_df$grouping_var_used, "_", " ") %>% stringr::str_to_title()

  wave_tables_df$response_var_used  <- stringr::str_replace_all(wave_tables_df$response_var_used, "_", " ") %>% stringr::str_to_title()

  wave_tables_df$percentage[is.nan(wave_tables_df$percentage)] <- 0

  wave_tables_df <- wave_tables_df %>% dplyr::select(
    "wave", "grouping_var_levels", "response_var_levels", "grouping_var_order",
    "response_var_order", "counts", "group_n", "percentage",
    "grouping_var_used", "grouping_var_used_order", "response_var_used", "response_var_used_order", "overall_sample_size_for_response_var")

  return(wave_tables_df)

}





#' create_power_bi_data_num_CALCULATED_TABLES
#'
#' @param df  df
#' @param grouping_vars grouping_vars
#' @param column_workbook_list column_workbook_list
#' @param name_of_column_details name_of_column_details
#'
#' @return table
#' @export
#'
create_power_bi_data_num_CALCULATED_TABLES <- function(
    df,
    column_workbook_list,
    grouping_vars,
    name_of_column_details
) {

  # Bring in the column details
  column_details <- column_workbook_list[[name_of_column_details]]

  # Create a list of variables that includes those not in the details sheet
  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    if(!(.x %in% column_details$column_names)){
      var <- .x
    } else {
      var <- column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info)
    }
    var
  })

  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  additional_num_vars <- column_details$column_names[column_details$type == "numeric" & !is.na(column_details$type)]

  likert_variables <- df %>%
    dplyr::select( tidyselect::starts_with("NUM__")) %>%
    colnames()

  response_var_names <- c(additional_num_vars, likert_variables)

  response_var_used_order <- factor(response_var_names, unique(response_var_names)) %>% as.numeric()

  rv_names_and_order_df <- data.frame(
    response_var_names = response_var_names,
    response_var_used_order = response_var_used_order
  )

  gv_names_and_order_df <- data.frame(
    grouping_vars = grouping_vars,
    grouping_var_used_order = grouping_var_used_order
  )

  parameter_df <- expand.grid(response_var_names, grouping_vars)

  colnames(parameter_df) <- c("response_var_names", "grouping_vars")

  parameter_df <- parameter_df %>%
    dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
    dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

  perform_calc_create_table_single_numeric <- function(df, WaveName = "All Waves", parameter_df, column_workbook){
    # Obtain the number of iterations
    n_iterations <- length(parameter_df$response_var_names)
    # Create progress bar
    pb <- progress::progress_bar$new(total = n_iterations, format = glue::glue("Performing Calculations for Numeric Stats Table for Wave: {WaveName}  [:bar] :percent eta: :eta"))
    # Loop through every possibility from the parameter df
    table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{

      iteration <- .x

      # Obtain Grouped stats
      t <- create_grouped_des_stats_table(
        df = df,
        grouping_var_name = parameter_df$grouping_vars[[iteration]],
        response_var_name = parameter_df$response_var_names[[iteration]]
      )

      # Add additional needed info to df
      t$response_var_used <- parameter_df$response_var_names[[iteration]]
      t$response_var_used_order <- parameter_df$response_var_used_order[[iteration]]
      t$grouping_var_used <- parameter_df$grouping_vars[[iteration]]
      t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[iteration]]
      t$wave <- WaveName

      # Indicate progress
      pb$tick()
      # Return the table that was made
      t

    })

    table <- table %>%
      dplyr::filter(!is.na(grouping_var_levels))
    table$ci_limit <- tidyr::replace_na(table$ci_limit, 0)
    table$ci_upper <- tidyr::replace_na(table$ci_upper, 0)
    table$ci_lower <- tidyr::replace_na(table$ci_lower, 0)
    table$sd <- tidyr::replace_na(table$sd, 0)
    table$se <- tidyr::replace_na(table$se, 0)

    return(table)
  }

  # Make a df for every wave
  split_wave_dfs <- split(df, as.factor(df$wave_info))

  # Add the df with all data to that list
  all_dfs <- append(list("All Waves" = df), split_wave_dfs)

  # Obtain the calculations for every df in the list
  wave_tables_df <- purrr::map2_df(all_dfs, as.list(names(all_dfs)), ~{
    perform_calc_create_table_single_numeric(df = .x, WaveName = .y, parameter_df, column_workbook = column_workbook_list)
    # perform_calc_create_table_single_sa(df = all_dfs[[3]], WaveName = as.list(names(all_dfs))[[3]], parameter_df, column_workbook = column_workbook_list)
  }, parameter_df, column_workbook)

  # Temp create these
  wave_tables_df$grouping_var_order <- NA
  wave_tables_df$response_var_order <- 0

  # Get rid of any nas
  wave_tables_df <- wave_tables_df %>%
    dplyr::filter(!is.na(response_var_levels)) %>%
    dplyr::filter(!is.na(grouping_var_levels))

  # Obtain the order information lookup table
  order_information_lookup_table <- create_order_information_lookup_table(column_workbook_list, name_of_column_details)


  # Fix the order numbers for grouping vars
  wave_tables_df <- adjust_order_information_lookup_table_per_type(wave_tables_df, order_information_lookup_table, type = "grouping_var_")
  # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "response_var", column_details = column_details, column_workbook_list = column_workbook_list)
  # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

  wave_tables_df$grouping_var_used  <- stringr::str_replace_all(wave_tables_df$grouping_var_used, "_", " ") %>% stringr::str_to_title()

  wave_tables_df$response_var_used  <- stringr::str_replace_all(wave_tables_df$response_var_used, "NUM__ec__", "") %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_title()

  wave_tables_df <- wave_tables_df %>% dplyr::select("wave", "grouping_var_levels",	"grouping_var_order",	"mean",	"sd",	"n",	"se",
                                                     "ci_limit",	"ci_upper",	"ci_lower",	"grouping_var_used",
                                                     "grouping_var_used_order",	"response_var_used",
                                                     "response_var_used_order", "overall_sample_size_for_response_var", "response_var_order", "response_var_levels")

  return(wave_tables_df)
}




#' create_power_bi_data_nps_CALCULATED_TABLES
#'
#' @param df df
#' @param grouping_vars grouping_vars
#' @param column_workbook_list column_workbook_list
#' @param name_of_column_details name_of_column_details
#'
#' @return wave_tables_df
#' @export
#'
create_power_bi_data_nps_CALCULATED_TABLES <- function(
    df,
    column_workbook_list,
    grouping_vars,
    name_of_column_details
) {

  # Bring in the column details
  column_details <- column_workbook_list[[name_of_column_details]]

  # Create a list of variables that includes those not in the details sheet
  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    if(!(.x %in% column_details$column_names)){
      var <- .x
    } else {
      var <- column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info)
    }
    var
  })

  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  response_var_names <- "net_promoter"

  response_var_used_order <- 1

  rv_names_and_order_df <- data.frame(
    response_var_names = response_var_names,
    response_var_used_order = response_var_used_order
  )

  gv_names_and_order_df <- data.frame(
    grouping_vars = grouping_vars,
    grouping_var_used_order = grouping_var_used_order
  )

  parameter_df <- expand.grid(response_var_names, grouping_vars)

  colnames(parameter_df) <- c("response_var_names", "grouping_vars")

  parameter_df <- parameter_df %>%
    dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
    dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

  perform_calc_create_table_single_nps<- function(df, WaveName = "All Waves", parameter_df, column_workbook){
    # Obtain the number of iterations
    n_iterations <- length(parameter_df$response_var_names)
    # Create progress bar
    pb <- progress::progress_bar$new(total = n_iterations, format = glue::glue("Performing Calculations for Numeric Stats Table for Wave: {WaveName}  [:bar] :percent eta: :eta"))
    # Loop through every possibility from the parameter df
    table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{

      iteration <- .x

      # Obtain Grouped stats
      t <- bkissell::create_grouped_nps_stats_table(
        df = df,
        grouping_var_name = parameter_df$grouping_vars[[iteration]],
        response_var_name = parameter_df$response_var_names[[iteration]]
      )

      # Add additional needed info to df
      t$response_var_used <- parameter_df$response_var_names[[iteration]]
      t$response_var_used_order <- parameter_df$response_var_used_order[[iteration]]
      t$grouping_var_used <- parameter_df$grouping_vars[[iteration]]
      t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[iteration]]
      t$wave <- WaveName

      # Indicate progress
      pb$tick()
      # Return the table that was made
      t

    })

    table <- table %>%
      dplyr::filter(!is.na(grouping_var_levels))

    return(table)
  }

  # Make a df for every wave
  split_wave_dfs <- split(df, as.factor(df$wave_info))

  # Add the df with all data to that list
  all_dfs <- append(list("All Waves" = df), split_wave_dfs)

  # Obtain the calculations for every df in the list
  wave_tables_df <- purrr::map2_df(all_dfs, as.list(names(all_dfs)), ~{
    perform_calc_create_table_single_nps(df = .x, WaveName = .y, parameter_df, column_workbook = column_workbook_list)
    # perform_calc_create_table_single_sa(df = all_dfs[[3]], WaveName = as.list(names(all_dfs))[[3]], parameter_df, column_workbook = column_workbook_list)
  }, parameter_df, column_workbook)

  # Temp create these
  wave_tables_df$grouping_var_order <- NA
  wave_tables_df$response_var_order <- 0

  # Get rid of any nas
  wave_tables_df <- wave_tables_df %>%
    dplyr::filter(!is.na(response_var_levels)) %>%
    dplyr::filter(!is.na(grouping_var_levels))

  # Obtain the order information lookup table
  order_information_lookup_table <- create_order_information_lookup_table(column_workbook_list, name_of_column_details)


  # Fix the order numbers for grouping vars
  wave_tables_df <- adjust_order_information_lookup_table_per_type(wave_tables_df, order_information_lookup_table, type = "grouping_var_")


  wave_tables_df$grouping_var_used  <- stringr::str_replace_all(wave_tables_df$grouping_var_used, "_", " ") %>% stringr::str_to_title()

  wave_tables_df$response_var_used  <- stringr::str_replace_all(wave_tables_df$response_var_used, "_", " ") %>%
    stringr::str_to_title()

  wave_tables_df <- wave_tables_df %>%
    dplyr::select("wave", "grouping_var_levels",	"grouping_var_order",
                  "sum",	"n",	"average",	"NPS",	"grouping_var_used",
                  "grouping_var_used_order", "overall_sample_size_for_response_var", "response_var_used", "response_var_levels")

  return(wave_tables_df)
}




#' create_power_bi_data_qualitative_CALCULATED_TABLES
#'
#' @param df df
#' @param grouping_vars grouping_vars
#' @param qualitative_type qualitative_type
#' @param identifier identifier
#' @param column_workbook_list column_workbook_list
#' @param name_of_column_details name_of_column_details
#' @param path_to_qual_coding_data path_to_qual_coding_data
#'
#' @return combined_final_table
#' @export
#'
create_power_bi_data_qualitative_CALCULATED_TABLES <- function(
    df = survey_data_for_power_bi_df,
    column_workbook_list,
    grouping_vars,
    name_of_column_details,
    path_to_qual_coding_data,
    qualitative_type = NULL,
    identifier
) {

  df_used <- df

  # Bring in the column details
  column_details <- column_workbook_list[[name_of_column_details]]

  # Create a list of variables that includes those not in the details sheet
  grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
    if(!(.x %in% column_details$column_names)){
      var <- .x
    } else {
      var <- column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info)
    }
    var
  })

  grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

  response_var_names <- column_details$column_names[column_details$type == "qualitative" & !is.na(column_details$type)]
  ##### old

  check_if_qual_data_path_exists <- file.exists(path_to_qual_coding_data)

  if(!check_if_qual_data_path_exists | is.null(qualitative_type)) {
    return(data.frame())
  } else {
    path_to_qualitative_coding_data <- path_to_qual_coding_data

    workbook_sheets_qualitative_names_all <- readxl::excel_sheets(path_to_qualitative_coding_data)

    qualitative_coding_details <- readxl::read_excel(path_to_qualitative_coding_data, "qual_column_details")

    workbook_sheets_qualitative_names <- workbook_sheets_qualitative_names_all[workbook_sheets_qualitative_names_all != "qual_column_details"]

    # Loop through each sheet
    workbook_sheets_qualitative <- purrr::map(workbook_sheets_qualitative_names, ~{

      current_sheet_name <- .x
      # Read in the data for it
      coding_data <- readxl::read_excel(path_to_qualitative_coding_data, sheet = current_sheet_name)

      df_used$RID <- as.numeric(df_used$RID)
      coding_data$RID <- as.numeric(coding_data$RID)

      qual_coding_data <- df_used %>%
        dplyr::left_join(coding_data, by = {{identifier}})

      qual_coding_data$RID <- as.character(qual_coding_data$RID)
      return(qual_coding_data)

      # Name all sheets so they can be easily accessed
    }) %>% purrr::set_names(workbook_sheets_qualitative_names)

    qualitative_coding_details_df_list <- purrr::map(workbook_sheets_qualitative_names,  ~{
      single_sheet_qualitative_coding_details <- qualitative_coding_details %>%
        dplyr::filter(question == .x)

      single_sheet_qualitative_coding_details
    })

    overall_qualitative_coding_details_df_list <- purrr::map(qualitative_coding_details_df_list, ~{
      overall_qualitative_coding_details <- .x %>%
        dplyr::filter(section_type == "overall_coding_labels")

      overall_qualitative_coding_details
    })

    overall_code_labels_list <- purrr::map(overall_qualitative_coding_details_df_list, ~{
      .x$column_labels
    })

    overall_code_indicator_list <- purrr::map(overall_qualitative_coding_details_df_list, ~{
      response_var_names_overall <- .x$column_names %>% stringr::str_extract("^.+__") %>% unique()
      response_var_names_overall
    })

    overall_code_indicator_order_list <- purrr::map(seq_along(overall_code_indicator_list), ~{
      response_var_used_order_overall <- .x
      response_var_used_order_overall
    })

    grouping_vars_selected_list <- purrr::map(workbook_sheets_qualitative, ~{
      .x %>% dplyr::select(tidyselect::any_of(grouping_vars)) %>% colnames()
    })

    grouping_vars_selected_order_list <- purrr::map(grouping_vars_selected_list, ~{
      .x %>% as.factor() %>% as.numeric()
    })

    obtain_the_parameters_for_overall <- function(overall_code_indicator, overall_code_indicator_order, grouping_vars_selected, grouping_vars_selected_order) {

      rv_names_and_order_df <- data.frame(
        response_var_names = overall_code_indicator,
        response_var_used_order = overall_code_indicator_order
      )

      gv_names_and_order_df <- data.frame(
        grouping_vars = grouping_vars_selected,
        grouping_var_used_order = grouping_vars_selected_order
      )

      parameter_df <- expand.grid(rv_names_and_order_df$response_var_names, gv_names_and_order_df$grouping_vars)

      colnames(parameter_df) <- c("response_var_names", "grouping_vars")

      parameter_df <- parameter_df %>%
        dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
        dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

      parameter_df

    }

    overall_parameter_list <- list(overall_code_indicator_list, overall_code_indicator_order_list, grouping_vars_selected_list, grouping_vars_selected_order_list)

    parameter_df_list <- purrr::pmap(overall_parameter_list, obtain_the_parameters_for_overall)

    table_list <- purrr::map2(parameter_df_list, workbook_sheets_qualitative, ~{
      overall_code_data <- .y

      single_parameter_df <- .x

      n_iterations <- length(single_parameter_df$response_var_names)

      pb <- progress::progress_bar$new(total = n_iterations)

      table <- purrr::map_df(seq_along(single_parameter_df$response_var_names), ~{
        number_for_row <- .x
        t <- create_table_single(
          df = overall_code_data,
          response_var_name = single_parameter_df$response_var_names[[number_for_row]],
          grouping_var_name = single_parameter_df$grouping_vars[[number_for_row]],
          response_var_type = "sa")

        pb$tick()

        t$response_var_used <- single_parameter_df$response_var_names[[number_for_row]]
        t$response_var_used_order <- single_parameter_df$response_var_used_order[[number_for_row]]
        t$grouping_var_used <- single_parameter_df$grouping_vars[[number_for_row]]
        t$grouping_var_used_order <- single_parameter_df$grouping_var_used_order[[number_for_row]]

        t
      })

      table
    })

    overall_table_list <- purrr::map2(table_list, overall_qualitative_coding_details_df_list, ~{
      table <- .x
      table$response_var_levels <- factor(table$response_var_levels, .y$column_names, .y$column_labels)
      table$response_var_order <- as.numeric(table$response_var_levels)
      table
    })

    overall_table_list <- purrr::map(overall_table_list, ~{
      table <- .x
      table$grouping_var_used <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>% stringr::str_to_title()
      table
    })

    break_down_qualitative_coding_details <- purrr::map2(qualitative_coding_details_df_list, overall_code_labels_list, ~{
      .x %>%
        dplyr::filter(section_type %in% .y)
    })

    break_down_categories_list <- purrr::map(break_down_qualitative_coding_details, ~{
      break_down_categories <- .x$section_type %>% unique()
      break_down_categories
    })

    break_down_categories_details_list <- purrr::map2(break_down_qualitative_coding_details, break_down_categories_list, ~{
      break_down_qualitative_coding_details_1 <- .x
      break_down_categories <- .y
      purrr::pmap(list(seq_along(break_down_categories)), ~{
        break_down_qualitative_coding_details_1 %>% dplyr::filter(section_type == break_down_categories[[..1]])
      })

    })

    break_down_code_labels_list <- purrr::map(break_down_categories_details_list, ~{
      break_down_categories_details <- .x

      purrr::pmap(list(seq_along(break_down_categories_details)), ~{
        break_down_categories_details[[.x]]$column_labels %>% as.character()
      })
    })

    break_down_code_indicator_list <- purrr::map(break_down_categories_details_list, ~{
      break_down_categories_details <- .x

      purrr::pmap(list(seq_along(break_down_categories_details)), ~{
        break_down_categories_details[[.x]]$column_names %>% stringr::str_extract("^.+__.+__") %>% unique()
      })
    })

    break_down_code_indicator_order_list <- purrr::map(break_down_categories_details_list, ~{
      break_down_code_indicator_order <- .x

      purrr::pmap(list(seq_along(break_down_code_indicator_order)), ~{
        ..1
      })
    })

    break_down_parameter_df_list <- purrr::map(seq_along(break_down_code_indicator_list), ~{
      first_level <- .x

      purrr::map(seq_along(break_down_code_indicator_list[[first_level]]), ~{
        second_level <- .x

        break_down_parameter_list <- list(
          break_down_code_indicator_list[[first_level]][[second_level]],
          break_down_code_indicator_order_list[[first_level]][[second_level]],
          grouping_vars_selected_list[[first_level]],
          grouping_vars_selected_order_list[[first_level]])

        purrr::pmap_df(break_down_parameter_list, obtain_the_parameters_for_overall)

      })})

    break_down_table_list <- purrr::map2(break_down_parameter_df_list, workbook_sheets_qualitative, ~{
      overall_code_data <- .y

      single_parameter_df_list <- .x

      purrr::map_df(single_parameter_df_list, ~{
        single_parameter_df <- .x

        table <- purrr::map_df(seq_along(single_parameter_df$response_var_names), ~{
          number_for_row <- .x
          t <- create_table_single(
            df = overall_code_data,
            response_var_name = single_parameter_df$response_var_names[[number_for_row]],
            grouping_var_name = single_parameter_df$grouping_vars[[number_for_row]],
            response_var_type = "sa")


          t$response_var_used <- single_parameter_df$response_var_names[[number_for_row]]
          t$response_var_used_order <- single_parameter_df$response_var_used_order[[number_for_row]]
          t$grouping_var_used <- single_parameter_df$grouping_vars[[number_for_row]]
          t$grouping_var_used_order <- single_parameter_df$grouping_var_used_order[[number_for_row]]

          t
        })

        table
      })
    })

    break_down_table_list <- purrr::map2(break_down_table_list, break_down_qualitative_coding_details, ~{
      table <- .x
      table$response_var_levels <- factor(table$response_var_levels, .y$column_names, .y$column_labels)
      table$response_var_order <- as.numeric(table$response_var_levels)
      table
    })

    break_down_table_list <- purrr::map(break_down_table_list, ~{
      table <- .x
      table$grouping_var_used <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>% stringr::str_to_title()
      table
    })

    if(qualitative_type == "overall_coding") {
      combined_final_table <- overall_table_list
    } else if(qualitative_type == "break_down_coding"){
      combined_final_table <- break_down_table_list
    }

    return(combined_final_table)
  }


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










#' create_grouped_des_stats_table
#'
#' @param df df
#' @param grouping_var_name grouping_var_name
#' @param response_var_name response_var_name
#'
#' @return table
#' @export
#'
create_grouped_des_stats_table <- function(
    df,
    grouping_var_name,
    response_var_name){

  variable_used <- response_var_name

  if(!(is.numeric(df[[variable_used]]))){
    variable_used <- paste0("NUM__", variable_used)
  }

  # Rename the variable as all
  df$all <- df[[{{variable_used}}]]

  table <- df %>%
    # Rename these variables so that we can combine everything into the same column
    dplyr::mutate(
      grouping_var_levels = as.character(.data[[{{grouping_var_name}}]])) %>%
    # Group by these variables
    dplyr::group_by(
      grouping_var_levels) %>%

    dplyr::filter(!is.na(.data[[{{variable_used}}]])) %>%
    # Get the counts for the grouping variable by the response variables
    dplyr::summarize(
      mean = mean(.data[[{{variable_used}}]], na.rm = TRUE),
      sd = sd(.data[[{{variable_used}}]], na.rm = TRUE),
      n = dplyr::n(),
      se = sd/sqrt(n),
      ci_limit = se * 1.96,
      ci_upper = mean + ci_limit,
      ci_lower = mean - ci_limit, .groups = "drop")

  # Get the order for the grouping variables
  table$grouping_var_used <- {{grouping_var_name}}


  table <- table %>%
    dplyr::select(
      grouping_var_levels,
      mean, sd, n, se, ci_limit, ci_upper, ci_lower,
      grouping_var_used
    )

  table_all <- df %>%
    dplyr::mutate(
      grouping_var_levels = "All") %>%
    # Group by these variables
    dplyr::group_by(
      grouping_var_levels) %>%
    dplyr::filter(!is.na(.data[[{{variable_used}}]])) %>%
    # Calculate the counts
    dplyr::summarize(
      mean = mean(.data[[{{variable_used}}]], na.rm = TRUE),
      sd = sd(.data[[{{variable_used}}]], na.rm = TRUE),
      n = dplyr::n(),
      se = sd/sqrt(n),
      ci_limit = se * 1.96,
      ci_upper = mean + ci_limit,
      ci_lower = mean - ci_limit, .groups = "drop")


  table_all$grouping_var_used <- {{grouping_var_name}}
  table_all$grouping_var_levels <- "All"


  # Order the variables
  table_all <- table_all %>%
    dplyr::select(
      grouping_var_levels,
      mean, sd, n, se, ci_limit, ci_upper, ci_lower,
      grouping_var_used
    )

  # Combine the two data frames
  table <- table %>% rbind(table_all)
  table$grouping_var_order <- NA
  table$response_var_order <- NA
  table$response_var_used <- variable_used
  table$response_var_levels <- "All"
  table$overall_sample_size_for_response_var <- table_all %>% dplyr::pull(n) %>% max()

  return(table)
}


#' create_grouped_nps_stats_table
#'
#' @param df df
#' @param grouping_var_name grouping_var_name
#' @param response_var_name response_var_name
#'
#' @return table
#' @export
#'
create_grouped_nps_stats_table <- function(
    df,
    grouping_var_name,
    response_var_name){

  # Obtain the order for all
  df$VAR_ORDER__all <- as.numeric(df[[{{"net_promoter"}}]])

  table <- df %>%
    # Rename these variables so that we can combine everything into the same column
    dplyr::mutate(
      grouping_var_levels = as.character(.data[[{{grouping_var_name}}]])) %>%
    # Group by these variables
    dplyr::group_by(
      grouping_var_levels) %>%
    dplyr::filter(!is.na(.data[[{{"net_promoter"}}]])) %>%
    # Get the counts for the grouping variable by the response variables
    dplyr::summarize(
      sum = sum(.data[[{{"net_promoter_coded"}}]], na.rm = TRUE),
      n = dplyr::n(),
      average = sum / n,
      NPS = average * 100, .groups = "drop")

  # Get the order for the grouping variables
  table$grouping_var_used <- grouping_var_name

  table$grouping_var_used_order <- NA

  table <- table %>%
    dplyr::select(
      grouping_var_levels,
      sum, n, average, NPS,
      grouping_var_used
    )

  table_all <- df %>%
    dplyr::mutate(
      grouping_var_levels = "All") %>%
    # Group by these variables
    dplyr::group_by(
      grouping_var_levels) %>%
    dplyr::filter(!is.na(.data[[{{"net_promoter"}}]])) %>%
    # Calculate the counts
    dplyr::summarize(
      sum = sum(.data[[{{"net_promoter_coded"}}]], na.rm = TRUE),
      n = dplyr::n(),
      average = sum / n,
      NPS = average * 100, .groups = "drop")

  table_all$grouping_var_used <- grouping_var_name
  table_all$grouping_var_levels <- "All"
  table_all$grouping_var_order <- 0

  # Order the variables
  table_all <- table_all %>%
    dplyr::select(
      grouping_var_levels,
      sum, n, average, NPS,
      grouping_var_used
    )

  # Combine the two data frames
  table <- table %>% rbind(table_all)
  table$grouping_var_order <- NA
  table$response_var_order <- NA
  table$response_var_used <- response_var_name
  table$response_var_levels <- "All"

  table$overall_sample_size_for_response_var <- table_all %>% dplyr::pull(n) %>% max()

  return(table)
}








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



#' create_survey_monkey_design_form_list
#'
#' @param path path
#' @param name_of_form name_of_form
#' @param name_of_logic name_of_logic
#' @param names_of_others_to_ignore names_of_others_to_ignore
#'
#' @return named_vectors_list
#' @export
#'
create_survey_monkey_design_form_list <- function(
    path,
    name_of_form = "Design Form",
    name_of_logic = "Logic",
    names_of_others_to_ignore = c("IGNORE")
){
  # Read in the name of the sheets contained in the excel workbook
  named_vectors_workbook_sheets <- readxl::excel_sheets(path)

  # Loop through each sheet
  named_vectors_list <- purrr::map(seq_along(named_vectors_workbook_sheets), ~{
    iteration = .x
    # Read in the data for it
    df <- readxl::read_excel(path, sheet = named_vectors_workbook_sheets[[iteration]])

    # Create named vectors for all sheets except for the column_details sheet
    if(!(named_vectors_workbook_sheets[[iteration]]  %in% c(name_of_form, name_of_logic, names_of_others_to_ignore))) {
      named_vector <- df$vector_of_factor_levels
      names(named_vector) <- df$names_of_factor_levels
    } else {
      named_vector <- df
    }

    return(named_vector)

    # Name all sheets so they can be easily accessed
  }, path) |> purrr::set_names(named_vectors_workbook_sheets)

  # Return the list with all of this information
  return(named_vectors_list)
}




#' set_up_project_environment
#'
#' @param storage_platform storage_platform
#' @param storage_platform_name storage_platform_name
#' @param group_dir_name group_dir_name
#' @param jobs_folder_name jobs_folder_name
#' @param project_year project_year
#' @param project_folder_name project_folder_name
#' @param should_create_nonexistant_dirs should_create_nonexistant_dirs
#' @param survey_version_name survey_version_name
#' @param survey_monkey_used survey_monkey_used
#' @param wave_names wave_names
#'
#' @return my_current_env
#' @export
#'
set_up_project_environment <- function(
    storage_platform,
    storage_platform_name,
    group_dir_name,
    jobs_folder_name,
    project_year,
    project_folder_name,
    should_create_nonexistant_dirs,
    survey_version_name,
    survey_monkey_used,
    wave_names

) {
  # Set the initial environment
  my_current_env <<- rlang::current_env()

  # Use the computer information to create the working directory according to
  # the information included as a parameter. This will be used instead of using
  # projects.
  working_directory_path <- bkissell::set_project_working_directory(
    storage_platform = storage_platform,
    storage_platform_name = storage_platform_name,
    group_dir_name = group_dir_name,
    jobs_folder_name = jobs_folder_name,
    project_year = project_year,
    project_folder_name = project_folder_name
  )

  rlang::env_poke(env = my_current_env, "working_directory_path", working_directory_path)

  # Check that the traditional project directories exist.
  # There is an option to create them if they do not exist
  traditional_project_dir_list <- bkissell::create_traditional_project_dir_list(should_create_nonexistant_dirs = should_create_nonexistant_dirs)

  for(iteration in seq_along(traditional_project_dir_list)){
    value_to_add <- traditional_project_dir_list[[iteration]]
    name_to_add <- names(traditional_project_dir_list)[[iteration]]
    rlang::env_poke(env = my_current_env, name_to_add, value_to_add)
  }

  # Check that the directories related to surveys exist according to the
  # included parameters. There is an option to create them if they do not exist
  survey_related_dir_list <- create_survey_related_dir_list(
    survey_version_name = survey_version_name,
    survey_monkey_used = survey_monkey_used,
    wave_names = wave_names,
    should_create_nonexistant_dirs = should_create_nonexistant_dirs
  )

  for(iteration in seq_along(survey_related_dir_list)){
    value_to_add <- survey_related_dir_list[[iteration]]
    name_to_add <- names(survey_related_dir_list)[[iteration]]
    rlang::env_poke(env = my_current_env, name_to_add, value_to_add)
  }

  # Obtain the names where the directories for the different survey versions
  survey_directory_path_names <- names(survey_related_dir_list)[stringr::str_detect(names(survey_related_dir_list), "^p_path_dc_sm_svn_wave_names_")]

  # Obtain the actual paths for the survey version directories
  survey_directory_paths <- survey_related_dir_list[survey_directory_path_names]

  rlang::env_poke(env = my_current_env, "survey_directory_paths", survey_directory_paths)

  # Obtain the paths where the column names documents should be
  column_names_paths <- purrr::map(survey_directory_paths, ~{
    file.path(.x, paste0(basename(.x), "_column_names.xlsx"))
  })

  rlang::env_poke(env = my_current_env, "column_names_paths", column_names_paths)

  for(iteration in seq_along(column_names_paths)){
    value_to_add <- column_names_paths[[iteration]]
    name_to_add <- names(column_names_paths)[[iteration]]
    rlang::env_poke(env = my_current_env, name_to_add, value_to_add)
  }

  # Obtain the paths that contain the data where the text has gone through custom spell check process
  spellcheck_column_paths <- purrr::map(survey_directory_paths, ~paste0(.x, "/spellchecked_text_columns.xlsx"))

  for(iteration in seq_along(spellcheck_column_paths)){
    value_to_add <- spellcheck_column_paths[[iteration]]
    name_to_add <- names(spellcheck_column_paths)[[iteration]]
    rlang::env_poke(env = my_current_env, name_to_add, value_to_add)
  }

  # Create the path for the cleaned survey data that will be stored in the power bi directory
  power_bi_clean_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/power_bi_deck/clean_data.csv")
  rlang::env_poke(env = my_current_env, "power_bi_clean_data_path", power_bi_clean_data_path)

  # Create the path for the cleaned survey data that will be stored in the processed clean data folder
  processed_data_clean_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_data/PROCESSED_", snakecase::to_snake_case(survey_version_name), "_data_", bkissell::create_time_chr_string_for_file_names("%Y%m%d_%H%M"), ".csv")
  rlang::env_poke(env = my_current_env, "processed_data_clean_data_path", processed_data_clean_data_path)

  # Create path for the text data from the survey
  text_survey_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_text/TEXT_PROCESSED_", snakecase::to_snake_case(survey_version_name), "_", bkissell::create_time_chr_string_for_file_names("%Y%m%d_%H%M"), ".csv")
  rlang::env_poke(env = my_current_env, "text_survey_data_path", text_survey_data_path)

  # Create the path for the text of the selected responses that we are wanting to highlight, which will be stored in the power bi folder
  power_bi_text_selected_example_text_survey_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_selected_example_text.csv")
  rlang::env_poke(env = my_current_env, "power_bi_text_selected_example_text_survey_data_path", power_bi_text_selected_example_text_survey_data_path)

  # Create the path for the text data that will be stored in the power bi folder
  power_bi_text_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_text_survey_data.csv")
  rlang::env_poke(env = my_current_env, "power_bi_text_path", power_bi_text_path)

  # Create the path for the selected example text survey data
  text_selected_example_text_survey_data_path <- paste0("Data Collection/survey_monkey_data/", survey_version_name, "/selected_example_text.xlsx")
  rlang::env_poke(env = my_current_env, "text_selected_example_text_survey_data_path", text_selected_example_text_survey_data_path)

  # Create the path for the mc power bi
  power_bi_mc_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_multiple_choice.csv")
  rlang::env_poke(env = my_current_env, "power_bi_mc_path", power_bi_mc_path)

  # Create the path for the power bi sa
  power_bi_sa_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_select_all.csv")
  rlang::env_poke(env = my_current_env, "power_bi_sa_path", power_bi_sa_path)

  # Create the path for the power bi desc table
  power_bi_descr_table_num_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_descr_table_num.csv")
  rlang::env_poke(env = my_current_env, "power_bi_descr_table_num_path", power_bi_descr_table_num_path)

  # Create the path for the net promoter
  power_bi_net_promoter_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_net_promoter.csv")
  rlang::env_poke(env = my_current_env, "power_bi_net_promoter_path", power_bi_net_promoter_path)

  # Create the path for the quality coding
  qualitative_coding_data_path_list <- paste0(survey_directory_paths, "/qualitative_coding_data.xlsx") %>% as.list()
  rlang::env_poke(env = my_current_env, "qualitative_coding_data_path_list", qualitative_coding_data_path_list)

  # Create the path for the power bi overall qual
  power_bi_overall_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_overall_qualitative.csv")
  rlang::env_poke(env = my_current_env, "power_bi_overall_qualitative_path", power_bi_overall_qualitative_path)

  # Create the path for the power bi breakdown qual
  power_bi_break_down_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")
  rlang::env_poke(env = my_current_env, "power_bi_break_down_qualitative_path", power_bi_break_down_qualitative_path)

  # Create the path for the power bi breakdown list qual
  power_bi_break_down_qualitative_path_list <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")
  rlang::env_poke(env = my_current_env, "power_bi_break_down_qualitative_path_list", power_bi_break_down_qualitative_path_list)

  # Get the example text repsonse sheets and put them together as a df in the env
  text_selected_example_response_vars <- readxl::excel_sheets(text_selected_example_text_survey_data_path)
  parameters_for_example_read <- data.frame(Var1 = text_selected_example_response_vars)
  parameters_for_example_read$Var2 <- text_selected_example_text_survey_data_path
  rlang::env_poke(env = my_current_env, "parameters_for_example_read", parameters_for_example_read)

  # Invisibly return the environment
  return(invisible(my_current_env))
}


#' create_multiple_column_workbook_lists
#'
#' @param my_env my_env
#' @param column_names_paths column_names_paths
#' @param name_of_column_details name_of_column_details
#'
#' @return column_workbook_lists
#' @export
#'
create_multiple_column_workbook_lists <- function(
    my_env = my_current_env,
    column_names_paths = my_current_env$column_names_paths,
    name_of_column_details) {
  column_workbook_lists <- purrr::map(column_names_paths, ~{
    column_workbook_list_attempt <- try({bkissell::create_column_details_and_named_vectors_list(
      path_to_column_workbook = .x,
      name_of_column_details = name_of_column_details)}, silent = TRUE)

    if(class(column_workbook_list_attempt) != "try-error") return(column_workbook_list_attempt)
  })

  rlang::env_poke(env = my_env, "column_workbook_lists", column_workbook_lists)

  return(invisible(column_workbook_lists))
}


#' create_survey_data_for_power_bi_df
#'
#' @param my_env my_env
#' @param write_data write_data
#'
#' @return survey_data_for_power_bi_df
#' @export
#'
create_survey_data_for_power_bi_df <- function(my_env = my_current_env, write_data = write_data) {

  survey_data_for_power_bi_df <- purrr::map(seq_along(my_env$survey_directory_paths), ~{
    iteration <- .x

    survey_data_for_power_bi_single <- try({
      bkissell::process_survey_data_for_single_power_bi(
        survey_directory_path_pb = my_env$survey_directory_paths[[iteration]],
        column_workbook_list_pb = my_env$column_workbook_lists[[iteration]],
        survey_file_ext = survey_file_ext,
        survey_datetime_format_pattern = survey_datetime_format_pattern,
        survey_version_name = survey_version_name,
        convert_numeric_age_to_age_group = convert_numeric_age_to_age_group,
        spellcheck_column_path_pb = my_env$spellcheck_column_paths[[iteration]],
        name_of_column_details = name_of_column_details
      )
    }, silent = TRUE)

    if(any(class(survey_data_for_power_bi_single) == "try-error") ){
      warning("data could not be processed")
      return(data.frame())
    } else {
      return(survey_data_for_power_bi_single)
    }
  }) %>%
    suppressWarnings() %>%
    purrr::reduce(dplyr::bind_rows)

  rlang::env_poke(env = my_env, "survey_data_for_power_bi_df", survey_data_for_power_bi_df)

  # Save the cleaned data to the power bi folder
  if(write_data){
    readr::write_csv(my_env$survey_data_for_power_bi_df, my_env$power_bi_clean_data_path)
    Sys.sleep(1)
    readr::write_csv(my_env$survey_data_for_power_bi_df, my_env$processed_data_clean_data_path)
  }

  return(invisible(survey_data_for_power_bi_df))
}


#' process_all_of_the_text_stuff_for_power_bi
#'
#' @param my_env my_env
#'
#' @return combined_df_selected_examples_text_df
#' @export
#'
process_all_of_the_text_stuff_for_power_bi <- function(my_env = my_current_env) {

  # Filter for needed variables
  text_survey_data <-  my_env$survey_data_for_power_bi_df %>%
    dplyr::select(
      "RID",
      tidyselect::any_of(variables_to_include_with_text),
      starts_with("text_")
    )

  rlang::env_poke(env = my_env, "text_survey_data", text_survey_data)

  if(write_data){
    # Save a copy of the text data as a backup
    readr::write_csv(my_env$text_survey_data, my_env$text_survey_data_path)
  }

  # Shape and filter the textual data
  power_bi_text <- my_env$text_survey_data %>%
    tidyr::pivot_longer(
      cols = -c("RID", tidyselect::any_of(variables_to_include_with_text)),
      names_to = "response_var_used",
      names_prefix = "text_",
      values_to = "textual_responses"
    ) %>%
    dplyr::filter(!is.na(.data[["textual_responses"]]))

  # Remove the "'" as the symbol is entered incorrectly in excel
  power_bi_text$textual_responses <- stringr::str_replace_all(power_bi_text$textual_responses, "'", "")

  rlang::env_poke(env = my_env, "power_bi_text", power_bi_text)

  if(write_data){
    # Save it to the powerbi folder
    readr::write_csv(my_env$power_bi_text, my_env$power_bi_text_path)
  }

  if(file.exists(my_env$text_selected_example_text_survey_data_path)){

    read_and_add_param_to_column <- purrr::as_mapper(
      function(param_df) {
        purrr::map_df(seq_along(param_df$Var1), ~{
          iteration <- .x
          selected_examples_df <- readxl::read_excel(param_df$Var2[[iteration]], param_df$Var1[[iteration]])
          selected_examples_df$response_var_used <- param_df$Var1[[iteration]]
          selected_examples_df
        }, param_df)
      })

    selected_examples_text_df <- read_and_add_param_to_column(param_df = my_env$parameters_for_example_read)
    selected_examples_text_df$RID <- as.numeric(selected_examples_text_df$RID)

    text_survey_data <- my_env$text_survey_data
    text_survey_data$RID <- as.numeric(text_survey_data$RID)

    combined_df_selected_examples_text_df <- selected_examples_text_df %>%
      dplyr::left_join(text_survey_data, by = "RID") %>%
      dplyr::select(!tidyselect::starts_with("text_"))

    combined_df_selected_examples_text_df$wave <- combined_df_selected_examples_text_df$wave_info

    selected_examples_text_df$RID <- as.character(selected_examples_text_df$RID)
    text_survey_data$RID <- as.character(text_survey_data$RID)
    combined_df_selected_examples_text_df$RID <- as.character(combined_df_selected_examples_text_df$RID)

    combined_df_selected_examples_text_df <- combined_df_selected_examples_text_df %>%
      dplyr::select(
        dplyr::any_of(
          c("wave", "RID",	"Text",	"Category",	"response_var_used",	"start_date",
            "end_date",	"h_c__no_children",	"h_c__under_12",	"h_c__12_to_17",
            "h_c__18_to_65",	"h_c__65_and_up",	"zip_code",	"gender",	"age_group",
            "ethnicity",	"marital_status",	"annual_household_income",
            "net_promoter",	"length_of_membership",	"number_of_visits_last_year",
            "number_of_planned_visits",	"likelihood_of_renewing",
            "donated_to_sdzwa_in_past", "confident_donation_will_help",
            "monthly_donor",	"support_sdzwa_again","study_version"
          )
        )
      )

    rlang::env_poke(env = my_env, "combined_df_selected_examples_text_df", combined_df_selected_examples_text_df)

    if(write_data){
      readr::write_csv(my_env$combined_df_selected_examples_text_df, my_env$power_bi_text_selected_example_text_survey_data_path)
    }
  }

  return(invisible(combined_df_selected_examples_text_df))
}


#' create_power_bi_multiple_choice
#'
#' @param my_env my_env
#'
#' @return power_bi_multiple_choice
#' @export
#'
create_power_bi_multiple_choice <- function(my_env = my_current_env) {

  if(any(my_env$column_workbook_lists_single[[name_of_column_details]]$type == "mc")) {

    power_bi_multiple_choice <- create_power_bi_data_mc_CALCULATED_TABLES(
      df = my_env$survey_data_for_power_bi_df,
      column_workbook_list = my_env$column_workbook_lists_single,
      grouping_vars = grouping_vars,
      name_of_column_details = name_of_column_details)

    rlang::env_poke(env = my_env, "power_bi_multiple_choice", power_bi_multiple_choice)

    if(write_data){
      readr::write_csv(my_env$power_bi_multiple_choice, my_env$power_bi_mc_path)
    }

    return(invisible(power_bi_multiple_choice))
  }
}



#' create_power_bi_select_all
#'
#' @param my_env my_env
#'
#' @return power_bi_select_all
#' @export
#'
create_power_bi_select_all <- function(my_env = my_current_env) {
  check_for_select_all_vars <- stringr::str_detect(my_env$column_workbook_lists_single[[name_of_column_details]]$type, "^sa")

  if(any(check_for_select_all_vars)){
    power_bi_select_all <- create_power_bi_data_sa_CALCULATED_TABLES(
      df = my_env$survey_data_for_power_bi_df,
      column_workbook_list = my_env$column_workbook_lists_single,
      grouping_vars = grouping_vars,
      name_of_column_details = name_of_column_details)

    rlang::env_poke(env = my_env, "power_bi_select_all", power_bi_select_all)

    if(write_data){
      readr::write_csv(my_env$power_bi_select_all, my_env$power_bi_sa_path)
    }
    return(invisible(power_bi_select_all))
  }
}


#' create_power_bi_descr_table_num
#'
#' @param my_env my_env
#'
#' @return power_bi_descr_table_num
#' @export
#'
create_power_bi_descr_table_num <- function(my_env = my_current_env) {
  check_for_numeric_vars <- column_workbook_lists_single[[name_of_column_details]]$type == "numeric"

  check_for_scale_vars <- !is.na(column_workbook_lists_single[[name_of_column_details]]$scale_names)

  if(any(check_for_numeric_vars) | any(check_for_scale_vars)){
    power_bi_descr_table_num <- create_power_bi_data_num_CALCULATED_TABLES(
      df = survey_data_for_power_bi_df,
      column_workbook_list = column_workbook_lists_single,
      grouping_vars = grouping_vars,
      name_of_column_details = name_of_column_details)

    rlang::env_poke(env = my_env, "power_bi_descr_table_num", power_bi_descr_table_num)

    if(write_data){
      readr::write_csv(my_env$power_bi_descr_table_num, my_env$power_bi_descr_table_num_path)
    }

    return(power_bi_descr_table_num)
  }
}


#' create_power_bi_net_promoter
#'
#' @param my_env my_env
#'
#' @return power_bi_net_promoter
#' @export
#'
create_power_bi_net_promoter <- function(my_env = my_current_env) {
  check_for_net_promoter_vars <- my_env$column_workbook_lists_single[[name_of_column_details]]$type == "numeric"

  if(any(check_for_net_promoter_vars)){
    power_bi_net_promoter <- create_power_bi_data_nps_CALCULATED_TABLES(
      df = my_env$survey_data_for_power_bi_df,
      column_workbook_list = my_env$column_workbook_lists_single,
      grouping_vars = grouping_vars,
      name_of_column_details = name_of_column_details)

    rlang::env_poke(env = my_env, "power_bi_net_promoter", power_bi_net_promoter)

    if(write_data){
      readr::write_csv(my_env$power_bi_net_promoter, my_env$power_bi_net_promoter_path)
    }

    return(invisible(power_bi_net_promoter))
  }
}

#' create_power_bi_overall_qualitative
#'
#' @param my_env my_env
#'
#' @return power_bi_overall_qualitative_combined
#' @export
#'
create_power_bi_overall_qualitative <- function(my_env = my_current_env) {

  # If I end up needing a combined set, I will need to add an optional piece that does those calculations
  power_bi_overall_qualitative_combined <- purrr::map(seq_along(my_env$qualitative_coding_data_path_list), ~ {
    iteration <- .x
    if(file.exists(my_env$qualitative_coding_data_path_list[[iteration]])){
      power_bi_overall_qualitative <- create_power_bi_data_qualitative_CALCULATED_TABLES(
        df = my_env$survey_data_for_power_bi_df,
        column_workbook_list = my_env$column_workbook_lists_single,
        grouping_vars = grouping_vars,
        name_of_column_details,
        path_to_qual_coding_data = my_env$qualitative_coding_data_path_list[[iteration]],
        qualitative_type = "overall_coding",
        identifier = "RID")

      power_bi_overall_qualitative <- power_bi_overall_qualitative[[1]]

      order_information_lookup_table <- bkissell::create_order_information_lookup_table(my_env$column_workbook_lists_single, name_of_column_details)

      power_bi_overall_qualitative$grouping_var_used <- snakecase::to_snake_case(power_bi_overall_qualitative$grouping_var_used)

      power_bi_overall_qualitative <- adjust_order_information_lookup_table_per_type(table = power_bi_overall_qualitative, order_information_lookup_table, type = "grouping_var_")
      power_bi_overall_qualitative <- power_bi_overall_qualitative %>%
        dplyr::filter(!is.na(grouping_var_levels))

      power_bi_overall_qualitative$wave <- basename(dirname(my_env$qualitative_coding_data_path_list[[iteration]]))

      return(power_bi_overall_qualitative)
    } else {
      return(data.frame())
    }
  }) %>% purrr::reduce(dplyr::bind_rows)

  rlang::env_poke(env = my_env, "power_bi_overall_qualitative_combined", power_bi_overall_qualitative_combined)

  if(write_data){
    readr::write_csv(my_env$power_bi_overall_qualitative_combined, my_env$power_bi_overall_qualitative_path)
  }

  return(invisible(power_bi_overall_qualitative_combined))
}


#' create_power_bi_break_down_qualitative_combined
#'
#' @param my_env my_env
#'
#' @return power_bi_break_down_qualitative_combined
#' @export
#'
create_power_bi_break_down_qualitative_combined <- function(my_env = my_current_env) {

  power_bi_break_down_qualitative_combined <- purrr::map(my_env$qualitative_coding_data_path_list, ~ {
    if(file.exists(.x)){

      power_bi_break_down_qualitative <- create_power_bi_data_qualitative_CALCULATED_TABLES(
        df = my_env$survey_data_for_power_bi_df,
        column_workbook_list = my_env$column_workbook_lists_single,
        grouping_vars = grouping_vars,
        name_of_column_details,
        path_to_qual_coding_data = .x,
        qualitative_type = "break_down_coding",
        identifier = "RID")

      power_bi_break_down_qualitative <- power_bi_break_down_qualitative[[1]]

      order_information_lookup_table <- create_order_information_lookup_table(my_env$column_workbook_lists_single, name_of_column_details)

      power_bi_break_down_qualitative$grouping_var_used <- snakecase::to_snake_case(power_bi_break_down_qualitative$grouping_var_used)

      power_bi_break_down_qualitative <- adjust_order_information_lookup_table_per_type(table = power_bi_break_down_qualitative, order_information_lookup_table, type = "grouping_var_")
      power_bi_break_down_qualitative <- power_bi_break_down_qualitative %>%
        dplyr::filter(!is.na(grouping_var_levels))

      power_bi_break_down_qualitative$wave <- basename(dirname(.x))

      return(power_bi_break_down_qualitative)
    } else {
      return(data.frame())
    }

  }) %>% purrr::reduce(dplyr::bind_rows)

  rlang::env_poke(env = my_env, "power_bi_break_down_qualitative_combined", power_bi_break_down_qualitative_combined)

  if(write_data){
    readr::write_csv(my_env$power_bi_break_down_qualitative_combined, my_env$power_bi_break_down_qualitative_path)
  }

  return(invisible(power_bi_break_down_qualitative_combined))
}




#' set_up_project_environment
#'
#' @param storage_platform storage_platform
#' @param storage_platform_name storage_platform_name
#' @param group_dir_name group_dir_name
#' @param jobs_folder_name jobs_folder_name
#' @param project_year project_year
#' @param project_folder_name project_folder_name
#' @param should_create_nonexistant_dirs should_create_nonexistant_dirs
#' @param survey_version_name survey_version_name
#' @param survey_monkey_used survey_monkey_used
#' @param wave_names wave_names
#'
#' @return my_current_env
#' @export
#'
set_up_project_environment_qualitative_coding <- function(
    storage_platform,
    storage_platform_name,
    group_dir_name,
    jobs_folder_name,
    project_year,
    project_folder_name,
    should_create_nonexistant_dirs,
    survey_version_name,
    survey_monkey_used,
    wave_names

) {
  # Set the initial environment
  my_current_env <<- rlang::current_env()

  # Use the computer information to create the working directory according to
  # the information included as a parameter. This will be used instead of using
  # projects.
  working_directory_path <- bkissell::set_project_working_directory(
    storage_platform = storage_platform,
    storage_platform_name = storage_platform_name,
    group_dir_name = group_dir_name,
    jobs_folder_name = jobs_folder_name,
    project_year = project_year,
    project_folder_name = project_folder_name
  )

  rlang::env_poke(env = my_current_env, "working_directory_path", working_directory_path)

  #
  # # Check that the traditional project directories exist.
  # # There is an option to create them if they do not exist
  # traditional_project_dir_list <- bkissell::create_traditional_project_dir_list(should_create_nonexistant_dirs = should_create_nonexistant_dirs)
  #
  # rlang::env_poke(env = my_current_env, "traditional_project_dir_list", traditional_project_dir_list)
  #
  # for(iteration in seq_along(traditional_project_dir_list)){
  #   value_to_add <- traditional_project_dir_list[[iteration]]
  #   name_to_add <- names(traditional_project_dir_list)[[iteration]]
  #   rlang::env_poke(env = my_current_env, name_to_add, value_to_add)
  # }
  #
  # return(invisible(my_current_env))
#
#   # Check that the directories related to surveys exist according to the
#   # included parameters. There is an option to create them if they do not exist
#   survey_related_dir_list <- create_survey_related_dir_list(
#     survey_version_name = survey_version_name,
#     survey_monkey_used = survey_monkey_used,
#     wave_names = wave_names,
#     should_create_nonexistant_dirs = should_create_nonexistant_dirs
#   )
#
#   for(iteration in seq_along(survey_related_dir_list)){
#     value_to_add <- survey_related_dir_list[[iteration]]
#     name_to_add <- names(survey_related_dir_list)[[iteration]]
#     rlang::env_poke(env = my_current_env, name_to_add, value_to_add)
#   }
#
#   # Obtain the names where the directories for the different survey versions
#   survey_directory_path_names <- names(survey_related_dir_list)[stringr::str_detect(names(survey_related_dir_list), "^p_path_dc_sm_svn_wave_names_")]
#
#   # Obtain the actual paths for the survey version directories
#   survey_directory_paths <- survey_related_dir_list[survey_directory_path_names]
#
#   rlang::env_poke(env = my_current_env, "survey_directory_paths", survey_directory_paths)
#
#   # Obtain the paths where the column names documents should be
#   column_names_paths <- purrr::map(survey_directory_paths, ~{
#     file.path(.x, paste0(basename(.x), "_column_names.xlsx"))
#   })
#
#   rlang::env_poke(env = my_current_env, "column_names_paths", column_names_paths)
#
#   for(iteration in seq_along(column_names_paths)){
#     value_to_add <- column_names_paths[[iteration]]
#     name_to_add <- names(column_names_paths)[[iteration]]
#     rlang::env_poke(env = my_current_env, name_to_add, value_to_add)
#   }
#
#   # Obtain the paths that contain the data where the text has gone through custom spell check process
#   spellcheck_column_paths <- purrr::map(survey_directory_paths, ~paste0(.x, "/spellchecked_text_columns.xlsx"))
#
#   for(iteration in seq_along(spellcheck_column_paths)){
#     value_to_add <- spellcheck_column_paths[[iteration]]
#     name_to_add <- names(spellcheck_column_paths)[[iteration]]
#     rlang::env_poke(env = my_current_env, name_to_add, value_to_add)
#   }
#
#   # Create the path for the cleaned survey data that will be stored in the power bi directory
#   power_bi_clean_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/power_bi_deck/clean_data.csv")
#   rlang::env_poke(env = my_current_env, "power_bi_clean_data_path", power_bi_clean_data_path)
#
#   # Create the path for the cleaned survey data that will be stored in the processed clean data folder
#   processed_data_clean_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_data/PROCESSED_", snakecase::to_snake_case(survey_version_name), "_data_", bkissell::create_time_chr_string_for_file_names("%Y%m%d_%H%M"), ".csv")
#   rlang::env_poke(env = my_current_env, "processed_data_clean_data_path", processed_data_clean_data_path)
#
#   # Create path for the text data from the survey
#   text_survey_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_text/TEXT_PROCESSED_", snakecase::to_snake_case(survey_version_name), "_", bkissell::create_time_chr_string_for_file_names("%Y%m%d_%H%M"), ".csv")
#   rlang::env_poke(env = my_current_env, "text_survey_data_path", text_survey_data_path)
#
#   # Create the path for the text of the selected responses that we are wanting to highlight, which will be stored in the power bi folder
#   power_bi_text_selected_example_text_survey_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_selected_example_text.csv")
#   rlang::env_poke(env = my_current_env, "power_bi_text_selected_example_text_survey_data_path", power_bi_text_selected_example_text_survey_data_path)
#
#   # Create the path for the text data that will be stored in the power bi folder
#   power_bi_text_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_text_survey_data.csv")
#   rlang::env_poke(env = my_current_env, "power_bi_text_path", power_bi_text_path)
#
#   # Create the path for the selected example text survey data
#   text_selected_example_text_survey_data_path <- paste0("Data Collection/survey_monkey_data/", survey_version_name, "/selected_example_text.xlsx")
#   rlang::env_poke(env = my_current_env, "text_selected_example_text_survey_data_path", text_selected_example_text_survey_data_path)
#
#   # Create the path for the mc power bi
#   power_bi_mc_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_multiple_choice.csv")
#   rlang::env_poke(env = my_current_env, "power_bi_mc_path", power_bi_mc_path)
#
#   # Create the path for the power bi sa
#   power_bi_sa_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_select_all.csv")
#   rlang::env_poke(env = my_current_env, "power_bi_sa_path", power_bi_sa_path)
#
#   # Create the path for the power bi desc table
#   power_bi_descr_table_num_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_descr_table_num.csv")
#   rlang::env_poke(env = my_current_env, "power_bi_descr_table_num_path", power_bi_descr_table_num_path)
#
#   # Create the path for the net promoter
#   power_bi_net_promoter_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_net_promoter.csv")
#   rlang::env_poke(env = my_current_env, "power_bi_net_promoter_path", power_bi_net_promoter_path)
#
#   # Create the path for the quality coding
#   qualitative_coding_data_path_list <- paste0(survey_directory_paths, "/qualitative_coding_data.xlsx") %>% as.list()
#   rlang::env_poke(env = my_current_env, "qualitative_coding_data_path_list", qualitative_coding_data_path_list)
#
#   # Create the path for the power bi overall qual
#   power_bi_overall_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_overall_qualitative.csv")
#   rlang::env_poke(env = my_current_env, "power_bi_overall_qualitative_path", power_bi_overall_qualitative_path)
#
#   # Create the path for the power bi breakdown qual
#   power_bi_break_down_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")
#   rlang::env_poke(env = my_current_env, "power_bi_break_down_qualitative_path", power_bi_break_down_qualitative_path)
#
#   # Create the path for the power bi breakdown list qual
#   power_bi_break_down_qualitative_path_list <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")
#   rlang::env_poke(env = my_current_env, "power_bi_break_down_qualitative_path_list", power_bi_break_down_qualitative_path_list)
#
#   # Get the example text repsonse sheets and put them together as a df in the env
#   text_selected_example_response_vars <- readxl::excel_sheets(text_selected_example_text_survey_data_path)
#   parameters_for_example_read <- data.frame(Var1 = text_selected_example_response_vars)
#   parameters_for_example_read$Var2 <- text_selected_example_text_survey_data_path
#   rlang::env_poke(env = my_current_env, "parameters_for_example_read", parameters_for_example_read)

  # Invisibly return the environment
  return(invisible(my_current_env))
}












#' initiate_my_current_env
#'
#' @param storage_platform storage_platform
#' @param storage_platform_name storage_platform_name
#' @param group_dir_name group_dir_name
#' @param jobs_folder_name jobs_folder_name
#' @param project_year project_year
#' @param project_folder_name project_folder_name
#'
#' @return invisible(my_current_env)
#' @export
#'
initiate_my_current_env <- function(
    storage_platform,
    storage_platform_name,
    group_dir_name,
    jobs_folder_name,
    project_year,
    project_folder_name
) {
  # Set the initial environment
  my_current_env <<- rlang::current_env()

  message("my_current_env has been initiated")

  # Use the computer information to create the working directory according to
  # the information included as a parameter. This will be used instead of using
  # projects.
  working_directory_path <- bkissell::set_project_working_directory(
    storage_platform = storage_platform,
    storage_platform_name = storage_platform_name,
    group_dir_name = group_dir_name,
    jobs_folder_name = jobs_folder_name,
    project_year = project_year,
    project_folder_name = project_folder_name
  )

  rlang::env_poke(env = my_current_env, "working_directory_path", working_directory_path)

  return(invisible(my_current_env))
}


#' add_traditional_project_dir_list_to_env
#'
#' @param my_current_env my_current_env
#' @param should_create_nonexistant_dirs should_create_nonexistant_dirs
#'
#' @return invisible(my_current_env)
#' @export
#'
add_traditional_project_dir_list_to_env <- function(my_current_env, should_create_nonexistant_dirs) {
  # Check that the traditional project directories exist.
  # There is an option to create them if they do not exist
  traditional_project_dir_list <- bkissell::create_traditional_project_dir_list(should_create_nonexistant_dirs = should_create_nonexistant_dirs)

  rlang::env_poke(env = my_current_env, "traditional_project_dir_list", traditional_project_dir_list)

  for(iteration in seq_along(traditional_project_dir_list)){
    value_to_add <- traditional_project_dir_list[[iteration]]
    name_to_add <- names(traditional_project_dir_list)[[iteration]]
    rlang::env_poke(env = my_current_env, name_to_add, value_to_add)
  }

  return(invisible(my_current_env))
}



#' add_survey_related_dir_list_to_env
#'
#' @param my_current_env my_current_env
#' @param survey_version_name survey_version_name
#' @param survey_monkey_used survey_monkey_used
#' @param wave_names wave_names
#' @param should_create_nonexistant_dirs should_create_nonexistant_dirs
#'
#' @return invisible(my_current_env)
#' @export
#'
add_survey_related_dir_list_to_env <- function(my_current_env, survey_version_name, survey_monkey_used, wave_names, should_create_nonexistant_dirs) {

  # Check that the directories related to surveys exist according to the
  # included parameters. There is an option to create them if they do not exist
  survey_related_dir_list <- create_survey_related_dir_list(
    survey_version_name = survey_version_name,
    survey_monkey_used = survey_monkey_used,
    wave_names = wave_names,
    should_create_nonexistant_dirs = should_create_nonexistant_dirs
  )

  rlang::env_poke(env = my_current_env, "survey_related_dir_list", survey_related_dir_list)

  for(iteration in seq_along(survey_related_dir_list)){
    value_to_add <- survey_related_dir_list[[iteration]]
    name_to_add <- names(survey_related_dir_list)[[iteration]]
    rlang::env_poke(env = my_current_env, name_to_add, value_to_add)
  }

  # Obtain the names where the directories for the different survey versions
  survey_directory_path_names <- names(survey_related_dir_list)[stringr::str_detect(names(survey_related_dir_list), "^p_path_dc_sm_svn_wave_names_")]

  rlang::env_poke(env = my_current_env, "survey_directory_path_names", survey_directory_path_names)

  # Obtain the actual paths for the survey version directories
  survey_directory_paths <- survey_related_dir_list[survey_directory_path_names]

  rlang::env_poke(env = my_current_env, "survey_directory_paths", survey_directory_paths)

  return(invisible(my_current_env))
}

#
#   function(
#            should_create_nonexistant_dirs,
#            survey_version_name,
#            survey_monkey_used,
#            wave_names) {
#
#
#
#
#
# }




#' qualitative_coding_system
#'
#' @param qual_path_list qual_path_list
#' @param processed_survey_data processed_survey_data
#' @param column_workbook_lists_single column_workbook_lists_single
#' @param grouping_vars grouping_vars
#' @param name_of_column_details name_of_column_details
#' @param path_to_qual_coding_data_list path_to_qual_coding_data_list
#' @param write_data write_data
#' @param power_bi_overall_qualitative_path power_bi_overall_qualitative_path
#'
#' @return power_bi_overall_qualitative_combined
#' @export
#'
qualitative_coding_system <- function(
    qual_path_list = my_current_env$qualitative_coding_data_path_list,
    processed_survey_data = my_current_env$survey_data_for_power_bi_df,
    column_workbook_lists_single = my_current_env$column_workbook_lists_single,
    grouping_vars,
    name_of_column_details,
    path_to_qual_coding_data_list = my_current_env$qualitative_coding_data_path_list,
    write_data,
    power_bi_overall_qualitative_path = my_current_env$power_bi_overall_qualitative_path,
    power_bi_break_down_qualitative_path = my_current_env$power_bi_break_down_qualitative_path
    # ,
    # combine_processed_qualified_lists = TRUE

    ) {

  # If I end up needing a combined set, I will need to add an optional piece that does those calculations
  power_bi_overall_qualitative_combined <- purrr::map(seq_along(qual_path_list), ~ {

    # Set the number for the iteration
    iteration <- .x

    # Check to see if this file exists
    if(file.exists(qual_path_list[[iteration]])){

      # Filter the processed data for the specific wave
      ind_processed_survey_data <- processed_survey_data %>%
        dplyr::filter(wave_info == basename(dirname(qual_path_list[[iteration]])))

      # Create the calculated Overall Power BI table
      power_bi_overall_qualitative <- bkissell::create_power_bi_data_qualitative_CALCULATED_TABLES(
        df = ind_processed_survey_data,
        column_workbook_list = column_workbook_lists_single,
        grouping_vars = grouping_vars,
        name_of_column_details,
        path_to_qual_coding_data = path_to_qual_coding_data_list[[iteration]],
        qualitative_type = "overall_coding",
        identifier = "RID")[[1]]

      order_information_lookup_table <- bkissell::create_order_information_lookup_table(column_workbook_lists_single, name_of_column_details)

      power_bi_overall_qualitative$grouping_var_used <- snakecase::to_snake_case(power_bi_overall_qualitative$grouping_var_used)

      power_bi_overall_qualitative <- adjust_order_information_lookup_table_per_type(
        table = power_bi_overall_qualitative,
        order_information_lookup_table,
        type = "grouping_var_"
        )

      power_bi_overall_qualitative <- power_bi_overall_qualitative %>%
        dplyr::filter(!is.na(grouping_var_levels))

      power_bi_overall_qualitative$wave <- basename(dirname(qual_path_list[[iteration]]))

      return(power_bi_overall_qualitative)
    } else {
      return(data.frame())
    }
  }) %>% purrr::reduce(dplyr::bind_rows)


  power_bi_break_down_qualitative_combined <- purrr::map(seq_along(qual_path_list), ~ {

    # Set the number for the iteration
    iteration <- .x

    # Check to see if this file exists
    if(file.exists(qual_path_list[[iteration]])){

      # Filter the processed data for the specific wave
      ind_processed_survey_data <- processed_survey_data %>%
        dplyr::filter(wave_info == basename(dirname(qual_path_list[[iteration]])))


      # Create the calculated Break Down Power BI table
      power_bi_break_down_qualitative <- bkissell::create_power_bi_data_qualitative_CALCULATED_TABLES(
        df = ind_processed_survey_data,
        column_workbook_list = column_workbook_lists_single,
        grouping_vars = grouping_vars,
        name_of_column_details,
        path_to_qual_coding_data = path_to_qual_coding_data_list[[iteration]],
        qualitative_type = "break_down_coding",
        identifier = "RID")[[1]]


      order_information_lookup_table <- bkissell::create_order_information_lookup_table(column_workbook_lists_single, name_of_column_details)

      power_bi_break_down_qualitative$grouping_var_used <- snakecase::to_snake_case(power_bi_break_down_qualitative$grouping_var_used)


      power_bi_break_down_qualitative <- adjust_order_information_lookup_table_per_type(
        table = power_bi_break_down_qualitative,
        order_information_lookup_table,
        type = "grouping_var_"
      )

      power_bi_break_down_qualitative <- power_bi_break_down_qualitative %>%
        dplyr::filter(!is.na(grouping_var_levels))

      power_bi_break_down_qualitative$wave <- basename(dirname(qual_path_list[[iteration]]))

      return(power_bi_break_down_qualitative)
    } else {
      return(data.frame())
    }
  }) %>% purrr::reduce(dplyr::bind_rows)


  if(write_data){
    readr::write_csv(power_bi_overall_qualitative_combined, power_bi_overall_qualitative_path)
    readr::write_csv(power_bi_break_down_qualitative_combined, power_bi_break_down_qualitative_path)
  }

  return(
    invisible(
      list(
        power_bi_overall_qualitative_combined = power_bi_overall_qualitative_combined,
        power_bi_break_down_qualitative_combined = power_bi_break_down_qualitative_combined
        )
      )
  )
}





#' create_specified_file_paths
#'
#' @param my_env my_env
#' @param vector_of_path_names_or_objects_to_create vector_of_path_names_or_objects_to_create
#' @param survey_directory_paths survey_directory_paths
#' @param column_names_paths column_names_paths
#' @param name_of_column_details name_of_column_details
#'
#' @return invisible(my_env)
#' @export
#'
create_specified_file_paths <- function(
    my_env = NULL,
    vector_of_path_names_or_objects_to_create = NULL,
    survey_directory_paths = NULL,
    column_names_paths = NULL,
    name_of_column_details = NULL
) {

  # qualitative_coding_data_path_list ---------------------------------------
  if("qualitative_coding_data_path_list" %in% vector_of_path_names_or_objects_to_create){
    if(!is.null(survey_directory_paths)){
      qualitative_coding_data_path_list <- paste0(survey_directory_paths, "/qualitative_coding_data.xlsx") %>% as.list()
      if(!is.null(my_env)){
        rlang::env_poke(env = my_env, "qualitative_coding_data_path_list", qualitative_coding_data_path_list)
      }
    } else {
      stop("`survey_directory_paths` must be specificed to create `qualitative_coding_data_path_list`")
    }
  }

  # column_names_paths ------------------------------------------------------
  if("column_names_paths" %in% vector_of_path_names_or_objects_to_create){
    if(!is.null(survey_directory_paths)){
      column_names_paths <- purrr::map(survey_directory_paths, ~{
        file.path(.x, paste0(basename(.x), "_column_names.xlsx"))
      })
      if(!is.null(my_env)){
        rlang::env_poke(env = my_env, "column_names_paths", column_names_paths)

        for(iteration in seq_along(column_names_paths)){
          value_to_add <- column_names_paths[[iteration]]
          name_to_add <- names(column_names_paths)[[iteration]]
          rlang::env_poke(env = my_env, name_to_add, value_to_add)
        }
      }
    } else {
      stop("`survey_directory_paths` must be specificed to create `column_names_paths`")
    }
  }

  # column_workbook_lists ------------------------------------------------------
  if("column_workbook_lists" %in% vector_of_path_names_or_objects_to_create){
    if(!is.null(column_names_paths) & !is.null(name_of_column_details)){
      bkissell::create_multiple_column_workbook_lists(
        my_env = my_env,
        column_names_paths = column_names_paths,
        name_of_column_details = name_of_column_details
      )
      if(!is.null(my_env)){
        # There is a chance that I need to make a column workbook list specifically for the combined files. At this point, I will just take the first one.
        rlang::env_poke(env = my_env, "column_workbook_lists_single", my_env$column_workbook_lists[[1]])
      }
    } else {
      stop("`column_names_paths` and `name_of_column_details` must be specificed to create `column_workbook_lists`")
    }
  }


  # spellcheck_column_paths ------------------------------------------------------
  if("spellcheck_column_paths" %in% vector_of_path_names_or_objects_to_create){
    if(!is.null(survey_directory_paths)){

      # Obtain the paths that contain the data where the text has gone through custom spell check process
      spellcheck_column_paths <- purrr::map(survey_directory_paths, ~paste0(.x, "/spellchecked_text_columns.xlsx"))

      if(!is.null(my_env)){
        rlang::env_poke(env = my_env, "spellcheck_column_paths", spellcheck_column_paths)

        for(iteration in seq_along(spellcheck_column_paths)){
          value_to_add <- spellcheck_column_paths[[iteration]]
          name_to_add <- names(spellcheck_column_paths)[[iteration]]
          rlang::env_poke(env = my_env, name_to_add, value_to_add)
        }
      }
    } else {
      stop("`survey_directory_paths` must be specificed to create `spellcheck_column_paths`")
    }
  }

  add_object_to_env <- function(
    my_env,
    name_of_object_to_add,
    object_to_add,
    vector_of_path_names_or_objects_to_create
  ) {
    if(name_of_object_to_add %in% vector_of_path_names_or_objects_to_create){
      if(!is.null(my_env)){
        rlang::env_poke(env = my_env, name_of_object_to_add, object_to_add)
      }
    }
    return(invisible(my_env))
  }

  # Create the path for the cleaned survey data that will be stored in the power bi directory
  power_bi_clean_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/power_bi_deck/clean_data.csv")
  add_object_to_env(my_env,"power_bi_clean_data_path", power_bi_clean_data_path, vector_of_path_names_or_objects_to_create)

  # Create the path for the cleaned survey data that will be stored in the processed clean data folder
  processed_data_clean_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_data/PROCESSED_", snakecase::to_snake_case(survey_version_name), "_data_", bkissell::create_time_chr_string_for_file_names("%Y%m%d_%H%M"), ".csv")
  add_object_to_env(my_env,"processed_data_clean_data_path", processed_data_clean_data_path, vector_of_path_names_or_objects_to_create)

  # Create path for the text data from the survey
  text_survey_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_text/TEXT_PROCESSED_", snakecase::to_snake_case(survey_version_name), "_", bkissell::create_time_chr_string_for_file_names("%Y%m%d_%H%M"), ".csv")
  add_object_to_env(my_env,"text_survey_data_path", text_survey_data_path, vector_of_path_names_or_objects_to_create)

  # Create the path for the text of the selected responses that we are wanting to highlight, which will be stored in the power bi folder
  power_bi_text_selected_example_text_survey_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_selected_example_text.csv")
  add_object_to_env(my_env,"power_bi_text_selected_example_text_survey_data_path", power_bi_text_selected_example_text_survey_data_path, vector_of_path_names_or_objects_to_create)

  # Create the path for the text data that will be stored in the power bi folder
  power_bi_text_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_text_survey_data.csv")
  add_object_to_env(my_env,"power_bi_text_path", power_bi_text_path, vector_of_path_names_or_objects_to_create)

  # Create the path for the selected example text survey data
  text_selected_example_text_survey_data_path <- paste0("Data Collection/survey_monkey_data/", survey_version_name, "/selected_example_text.xlsx")
  add_object_to_env(my_env,"text_selected_example_text_survey_data_path", text_selected_example_text_survey_data_path, vector_of_path_names_or_objects_to_create)


  if(file.exists(text_selected_example_text_survey_data_path)){

    # Obtain the paths that contain the data where the text has gone through custom spell check process
    spellcheck_column_paths <- purrr::map(survey_directory_paths, ~paste0(.x, "/spellchecked_text_columns.xlsx"))

    if(!is.null(my_env)){

      # Get the example text repsonse sheets and put them together as a df in the env
      text_selected_example_response_vars <- readxl::excel_sheets(text_selected_example_text_survey_data_path)
      parameters_for_example_read <- data.frame(Var1 = text_selected_example_response_vars)
      parameters_for_example_read$Var2 <- text_selected_example_text_survey_data_path
      rlang::env_poke(env = my_current_env, "parameters_for_example_read", parameters_for_example_read)
    }
  } else {
    stop("`text_selected_example_text_survey_data_path` must exist to create `parameters_for_example_read`")
  }

  # Create the path for the mc power bi
  power_bi_mc_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_multiple_choice.csv")
  add_object_to_env(my_env,"power_bi_mc_path", power_bi_mc_path, vector_of_path_names_or_objects_to_create)

  # Create the path for the power bi sa
  power_bi_sa_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_select_all.csv")
  add_object_to_env(my_env,"power_bi_sa_path", power_bi_sa_path, vector_of_path_names_or_objects_to_create)

  # Create the path for the power bi desc table
  power_bi_descr_table_num_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_descr_table_num.csv")
  add_object_to_env(my_env,"power_bi_descr_table_num_path", power_bi_descr_table_num_path, vector_of_path_names_or_objects_to_create)

  # Create the path for the net promoter
  power_bi_net_promoter_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_net_promoter.csv")
  add_object_to_env(my_env,"power_bi_net_promoter_path", power_bi_net_promoter_path, vector_of_path_names_or_objects_to_create)

  # Create the path for the quality coding
  qualitative_coding_data_path_list <- paste0(survey_directory_paths, "/qualitative_coding_data.xlsx") %>% as.list()
  add_object_to_env(my_env,"qualitative_coding_data_path_list", qualitative_coding_data_path_list, vector_of_path_names_or_objects_to_create)

  # Create the path for the power bi overall qual
  power_bi_overall_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_overall_qualitative.csv")
  add_object_to_env(my_env,"power_bi_overall_qualitative_path", power_bi_overall_qualitative_path, vector_of_path_names_or_objects_to_create)

  # Create the path for the power bi breakdown qual
  power_bi_break_down_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")
  add_object_to_env(my_env,"power_bi_break_down_qualitative_path", power_bi_break_down_qualitative_path, vector_of_path_names_or_objects_to_create)

  # Create the path for the power bi breakdown list qual
  power_bi_break_down_qualitative_path_list <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")
  add_object_to_env(my_env,"power_bi_break_down_qualitative_path_list", power_bi_break_down_qualitative_path_list, vector_of_path_names_or_objects_to_create)

  return(invisible(my_env))
}










# Create the URLs for the participant data pages ---------------------------

#' create_user_participant_urls
#'
#' @param project_numbers Project numbers
#'
#' @return user_participant_urls
#' @export
#'
create_user_participant_urls <- function(project_numbers) {
  # Create a url for every project number
  user_participant_urls <- purrr::map_chr(project_numbers, ~{
    paste0("https://www.userinterviews.com/projects/", .x, "/participants")
  })

  # return the vector
  return(user_participant_urls)
}



################################################################################
# Initiate User Credentials -----------------------------------------------

#' initiate_credentials_for_user
#' Provide Credentials for User account through the use of selenium
#'
#' @param remDr Selenium client used to control the browser
#'
#' @return remDr
#' @export
#'

initiate_credentials_for_user <- function(remDr){

  # Obtain the credentials
  user_credentials <- initiate_project_and_design_details()

  # Set username for user
  user_credentials_username <- user_credentials$username

  # Set password for user
  user_credentials_password <- user_credentials$password

  # Navigate to User's sign-in page
  remDr$navigate("https://www.userinterviews.com/accounts/signin?source=signin-page")

  # Give the page a half second to load
  Sys.sleep(.5)

  # Enter your username
  remDr$findElement('xpath', '//*[@id="email"]')$sendKeysToElement(list(user_credentials_username))

  # Enter your password
  remDr$findElement('xpath', '//*[@id="password"]')$sendKeysToElement(list(user_credentials_password))

  # Submit your credentials to initialize the two step process
  remDr$findElements("css", "button.Button:nth-child(1)")[[1]]$clickElement()

  # To finish, you will need to mannually enter your verification code into the browser
  message("Please go to your email and obtain the verification code. Manually enter the code into the selenium web-browser and click enter")

  return(remDr)
}
