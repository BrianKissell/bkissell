#' ################################################################################
#'
#'
#' ################################################################################
#'
#'
#' ################################################################################
#'
#'
#' ################################################################################
#'
#' #' check_if_the_column_names_file_exists
#' #'
#' #' @param version_column_names_paths version_column_names_paths
#' #'
#' #' @return the_column_names_file_exist
#' #' @export
#' #'
#' check_if_the_column_names_file_exists <- function(version_column_names_paths) {
#'
#'   # Check if the doc that contains the column names exists in the file system
#'   the_column_names_file_exist <- purrr::map_lgl(version_column_names_paths, file.exists)
#'
#'   return(the_column_names_file_exist)
#' }
#'
#' ################################################################################
#'
#'
#' ################################################################################
#'
#' #' obtain_connection_to_zip_files_for_versions
#' #'
#' #' @param newest_files_for_project newest_files_for_project
#' #'
#' #' @return connection_to_zip_files
#' #' @export
#' #'
#' obtain_connection_to_zip_files_for_versions <- function(newest_files_for_project) {
#'   # Get the name of the file from the zip file
#'   path_inside_zip <- purrr::map_chr(newest_files_for_project, ~{
#'     file_location <- .x
#'
#'     file_names <- unzip(zipfile = file_location, list = TRUE) %>% dplyr::pull(Name)
#'
#'     do_not_keep <- stringr::str_detect(file_names, "PageOrder")
#'
#'     the_path_inside_zip <- file_names[!do_not_keep]
#'
#'     the_path_inside_zip
#'   })
#'
#'   # Create the connection to the zip files
#'   connection_to_zip_files <-  purrr::map2(
#'     newest_files_for_project,
#'     path_inside_zip,
#'     ~ unz(.x, .y)
#'   )
#'
#'   return(connection_to_zip_files)
#' }
#'
#'
#' # path_inside_zip <- purrr::map_chr(newest_files_for_project, ~{
#' #
#' #   file_names <- unzip(zipfile = i, list = TRUE) %>% dplyr::pull(Name)
#' #
#' #   do_not_keep <- stringr::str_detect(file_names, "PageOrder")
#' #
#' #   the_path_inside_zip <- file_names[!do_not_keep]
#' #
#' #   the_path_inside_zip
#' # })
#'
#' ################################################################################
#'
#'
#' ################################################################################
#' #
#' # check_for_and_create_column_names_excel <- function(
#'     #     the_column_names_file_exists,
#' #     survey_data_list,
#' #     version_column_names_paths
#' #     ) {
#' #
#' #   list_of_informtaion <- list(
#' #     the_column_names_file_exists,
#' #     survey_data_list,
#' #     version_column_names_paths
#' #     )
#' #
#' #   purrr::pmap(list_of_informtaion, ~{
#' #     if(..1 == FALSE) {
#' #       #Create it.
#' #     }
#' #
#' #   })
#' #
#' #
#' # }
#'
#'
#' ################################################################################
#' #
#' # read_survey_data <- function(survey_directory_path, version_columns_must_match = TRUE){
#' #
#' #   # Create Version Directories
#' #   version_directories <- prepare_version_directory_paths(survey_directory_path)
#' #
#' #   # Obtain the newest version from each directory
#' #   newest_files_for_project <- obtain_newest_file_per_directory(version_directories)
#' #
#' #   # Obtain the paths for the column names excel files
#' #   version_column_names_paths <- obtain_column_name_paths_for_all_survey_versions(version_directories)
#' #
#' #   # Check if the column names excel file exists for each version
#' #   the_column_names_file_exists <- check_if_the_column_names_file_exists(version_column_names_paths)
#' #
#' #   # Obtain appropriate initial column names
#' #   initial_column_names_for_version <- obtain_initial_column_names_for_version(
#' #     version_column_names_paths,
#' #     the_column_names_file_exists
#' #   )
#' #
#' #   # Create Connection to zip files
#' #   connection_to_zip_files <- obtain_connection_to_zip_files_for_versions(newest_files_for_project)
#' #
#' #   # Read in data stored in the the csv
#' #   survey_data_list <- read_csv_in_zip(connection_to_zip_files, initial_column_names_for_version)
#' #
#' #   # Stop the program if column files do not exist.
#' #   if_no_csv_for_colnames_make_one(
#' #     survey_data_list,
#' #     the_column_names_file_exists,
#' #     version_column_names_paths
#' #   )
#' #
#' #   version_name <- basename(version_directories)
#' #
#' #   version_names <- purrr::map2(survey_data_list, version_name, ~{
#' #     data_frame <- .x
#' #     data_frame$version_name <- .y
#' #     data_frame$version_name
#' #   }) %>% unlist()
#' #
#' #   # If you indicate that the columns of the versions must match, then it throws an error if they do not, otherwise it combines the data.
#' #   # If you indicate that the columns do not need to match, it just combines them and labels the missing columns as NA.
#' #   survey_data <- process_version_columns_must_match(version_columns_must_match = version_columns_must_match, df_list = survey_data_list)
#' #
#' #   survey_data$version_names <- version_names
#' #
#' #   if(is.null(survey_data$RID)) {
#' #     survey_data$RID <- survey_data$respondent_id
#' #   }
#' #
#' #   # Return the df
#' #   return(survey_data)
#' # }
#'
#' #' read_survey_data
#' #'
#' #' @param survey_directory_path survey_directory_path
#' #'
#' #' @return survey_data
#' #' @export
#' #'
#' read_survey_data <- function(survey_directory_path){
#'
#'   # Create Version Directories
#'   version_directories <- prepare_version_directory_paths(survey_directory_path)
#'
#'   # Obtain the newest version from each directory
#'   newest_files_for_project <- obtain_newest_file_per_directory(version_directories)
#'
#'   # Obtain the paths for the column names excel files
#'   version_column_names_paths <- obtain_column_name_paths_for_all_survey_versions(version_directories)
#'
#'   # Check if the column names excel file exists for each version
#'   the_column_names_file_exists <- check_if_the_column_names_file_exists(version_column_names_paths)
#'
#'   # Obtain appropriate initial column names
#'   initial_column_names_for_version <- obtain_initial_column_names_for_version(
#'     version_column_names_paths,
#'     the_column_names_file_exists
#'   )
#'
#'   # Create Connection to zip files
#'   connection_to_zip_files <- obtain_connection_to_zip_files_for_versions(newest_files_for_project)
#'
#'   # Read in data stored in the the csv
#'   survey_data_list <- read_csv_in_zip(connection_to_zip_files, initial_column_names_for_version)
#'
#'   # Stop the program if column files do not exist.
#'   if_no_csv_for_colnames_make_one(
#'     survey_data_list,
#'     the_column_names_file_exists,
#'     version_column_names_paths
#'   )
#'
#'   version_name <- basename(version_directories)
#'
#'   survey_data_list <- purrr::map2(survey_data_list, version_name, ~{
#'     data_frame <- .x
#'
#'     data_frame$version_name <- .y
#'
#'     data_frame
#'   })
#'
#'   survey_data_list <- purrr::map(survey_data_list, ~{
#'     df <- .x
#'
#'     if("RID" %in% names(df)) {
#'       df$RID <- df$RID
#'     } else if("Respondent ID" %in% names(df)) {
#'       df$RID <- df$`Respondent ID`
#'     } else if("respondent_id" %in% names(df)) {
#'       df$RID <- df$respondent_id
#'     }
#'
#'     df
#'   })
#'
#'   survey_data_list <- survey_data_list %>% purrr::set_names(version_name)
#'   # # If you indicate that the columns of the versions must match, then it throws an error if they do not, otherwise it combines the data.
#'   # # If you indicate that the columns do not need to match, it just combines them and labels the missing columns as NA.
#'   # survey_data <- process_version_columns_must_match(version_columns_must_match = version_columns_must_match, df_list = survey_data_list)
#'   #
#'   # survey_data$version_names <- version_names
#'   #
#'   # if(is.null(survey_data$RID)) {
#'   #   survey_data$RID <- survey_data$respondent_id
#'   # }
#'
#'   # Return the df
#'   return(survey_data_list)
#' }
#'
#'
#'
#'
#'
#' #' if_no_csv_for_colnames_change_names_of_df
#' #' Defunct. I actually do not think this is needed.
#' #'
#' #' @param survey_data_list survey_data_list
#' #' @param the_column_names_file_exists the_column_names_file_exists
#' #' @param version_column_names_paths version_column_names_paths
#' #'
#' #' @return survey_data_list
#' #' @export
#' #'
#' if_no_csv_for_colnames_change_names_of_df <- function(
#'     survey_data_list,
#'     the_column_names_file_exists,
#'     version_column_names_paths
#' ) {
#'
#'   list_of_parameters <- list(survey_data_list, the_column_names_file_exists, version_column_names_paths)
#'
#'   survey_data_list <- purrr::pmap(list_of_parameters, ~{
#'
#'     if(..2 == FALSE) {
#'
#'       column_names <- ..3 %>%
#'         readxl::read_excel() %>%
#'         dplyr::pull(column_names) %>%
#'         list()
#'
#'       df <- ..1
#'
#'       colnames(df) <- column_names
#'
#'       df
#'     } else {
#'
#'       df <- ..1
#'
#'     }
#'   })
#'
#'   return(survey_data_list)
#' }
#'
#'
#'
#' #' check_if_all_columns_match
#' #'
#' #' @param df_list df_list
#' #'
#' #' @return all_columns_match_lgl
#' #' @export
#' #'
#' check_if_all_columns_match <- function(df_list) {
#'
#'   list_with_column_names <- purrr::map(df_list, ~ colnames(.x))
#'
#'   all_columns_match_lgl <- length(unique(list_with_column_names)) == 1
#'
#'   return(all_columns_match_lgl)
#' }
#'
#'
#'
#' #' process_version_columns_must_match
#' #'
#' #' @param version_columns_must_match version_columns_must_match
#' #' @param df_list df_list
#' #'
#' #' @return df
#' #' @export
#' #'
#' process_version_columns_must_match <- function(version_columns_must_match, df_list) {
#'
#'   if(version_columns_must_match == TRUE) {
#'
#'     all_columns_match_lgl <- check_if_all_columns_match(df_list)
#'
#'     if(all_columns_match_lgl == TRUE) {
#'
#'       df <- purrr::map_df(df_list, ~ .x)
#'
#'     } else {
#'
#'       stop(glue::glue("You have indicated that column names of versions must match, but they do not. Please check you column name excel files."))
#'
#'     }
#'
#'   } else {
#'
#'     df <- df_list %>%
#'       dplyr::bind_rows(.id = "column_label")
#'
#'   }
#'
#'   return(df)
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#' ################################################################################
#'
#' #' add_duration_to_df
#' #'
#' #' @param df df
#' #'
#' #' @return df
#' #' @export
#' #'
#' add_duration_to_df <- function(df) {
#'   # Format start_date as time variable
#'   df$start_date <- lubridate::mdy_hms(df$start_date)
#'
#'   # Format end_date as time variable
#'   df$end_date <- lubridate::mdy_hms(df$end_date)
#'
#'   # Create duration_of_study
#'   df$duration_of_study <- df$end_date - df$start_date
#'
#'   # Return the df
#'   return(df)
#' }
#'
#' ################################################################################
#'
#' #' make_ranged_numbers_in_string_ordered_factor
#' #'
#' #' @param variable variable
#' #'
#' #' @return variable_as_factor
#' #' @export
#' #'
#' make_ranged_numbers_in_string_ordered_factor <- function(variable) {
#'
#'   if(is.character(variable)){
#'     levels <- unique(variable)
#'
#'     initial_value_of_levels <- stringr::str_extract(levels, "[0-9\\,]+") %>%
#'       stringr::str_replace_all(",", "") %>%
#'       as.numeric()
#'
#'     levels_df <- data.frame(
#'       levels = levels,
#'       initial_value_of_levels = initial_value_of_levels
#'     )
#'
#'     levels_in_order <- levels_df %>%
#'       dplyr::arrange(initial_value_of_levels) %>%
#'       dplyr::pull(levels)
#'
#'     variable_as_factor <- factor(variable, levels = levels_in_order, ordered = TRUE)
#'
#'   } else {
#'     warning("'variable' must be a character string for it to be converted, variable was left alone")
#'     variable_as_factor <- variable
#'   }
#'
#'   return(variable_as_factor)
#' }
#'
#' ################################################################################
#'
#' #' obtain_named_vector_for_US_state_region
#' #'
#' #' @return US_States_Regions
#' #' @export
#' #'
#' obtain_named_vector_for_US_state_region <- function() {
#'   # Create state vectors
#'   Northeast_states <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont", "Washington D.C.", "Northeast")
#'   Midwest_states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin", "Midwest")
#'   West_states <- c("Alaska", "Arizona", "California", "Colorado", "Hawaii", "Idaho", "Montana", "Nevada", "New Mexico", "Oregon", "Utah", "Washington", "Wyoming", "West")
#'   South_states <- c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia", "South")
#'
#'   US_States <- c(Northeast_states, Midwest_states, West_states, South_states)
#'
#'   names_Northeast_states <- rep("Northeast", length(Northeast_states))
#'   names_Midwest_states <- rep("Midwest", length(Midwest_states))
#'   names_West_states <- rep("West", length(West_states))
#'   names_South_states <- rep("South", length(South_states))
#'
#'   names_US_Regions <- c(names_Northeast_states, names_Midwest_states, names_West_states, names_South_states)
#'
#'   names(US_States) <- names_US_Regions
#'
#'   US_States_Regions <- US_States
#'
#'   return(US_States_Regions)
#' }
#'
#' ################################################################################
#'
#' #' obtain_named_vector_for_US_states
#' #'
#' #' @return US_States
#' #' @export
#' #'
#' obtain_named_vector_for_US_states <- function() {
#'   # Create state vectors
#'   Northeast_states <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont", "Washington D.C.", "Northeast")
#'   Midwest_states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin", "Midwest")
#'   West_states <- c("Alaska", "Arizona", "California", "Colorado", "Hawaii", "Idaho", "Montana", "Nevada", "New Mexico", "Oregon", "Utah", "Washington", "Wyoming", "West")
#'   South_states <- c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia", "South")
#'
#'   US_States <- c(Northeast_states, Midwest_states, West_states, South_states)
#'
#'   names(US_States) <- US_States
#'
#'   return(US_States)
#' }
#'
#' ################################################################################
#'
#' #' recode_as_factor
#' #'
#' #' @param variable variable
#' #' @param named_vec_levels named_vec_levels
#' #'
#' #' @return variable
#' #' @export
#' #'
#' recode_as_factor <- function(variable, named_vec_levels) {
#'   variable <- factor(variable, levels = named_vec_levels, labels = names(named_vec_levels), ordered = TRUE)
#'   return(variable)
#' }
#'
#' ################################################################################
#'
#' #' name_vector
#' #'
#' #' @param vector_of_factor_levels vector_of_factor_levels
#' #' @param names_of_factor_levels names_of_factor_levels
#' #'
#' #' @return named_vector
#' #' @export
#' #'
#' name_vector <- function(vector_of_factor_levels, names_of_factor_levels) {
#'   named_vector <- vector_of_factor_levels
#'   names(named_vector) <- names_of_factor_levels
#'   return(named_vector)
#' }
#'
#' ################################################################################
#'
#' #' factor_vars_with_named_vectors
#' #'
#' #' @param df df
#' #' @param list_with_variable_names_to_reorder list_with_variable_names_to_reorder
#' #' @param list_with_named_vectors list_with_named_vectors
#' #'
#' #' @return df
#' #' @export
#' #'
#' factor_vars_with_named_vectors <- function(df, list_with_variable_names_to_reorder, list_with_named_vectors) {
#'   purrr::walk2(list_with_variable_names_to_reorder, list_with_named_vectors, ~ {
#'
#'     index_for_var_to_change <- which(colnames(df) == {{.x}})
#'     # index_for_var_to_change <- which(colnames(df) == "version")
#'     vector_to_change <- df[,index_for_var_to_change][[1]]
#'
#'     df[,index_for_var_to_change] <<- recode_as_factor(variable = vector_to_change, named_vec_levels = .y)
#'   })
#'
#'   return(df)
#' }
#'
#' ################################################################################
#'
#' #' check_if_all_are_missing_from_row
#' #'
#' #' @param df df
#' #' @param list_of_variable_names list_of_variable_names
#' #'
#' #' @return is_all_column_data_missing_for_row
#' #' @export
#' #'
#' check_if_all_are_missing_from_row <- function(df, list_of_variable_names) {
#'   selected_columns <- df %>% dplyr::select(tidyselect::all_of(list_of_variable_names))
#'
#'   is_all_column_data_missing_for_row <- purrr::map_lgl(seq_along(df[[1]]), ~{
#'     selected_columns[.x,] |> purrr::every(is.na)
#'   })
#'
#'   return(is_all_column_data_missing_for_row)
#' }
#'
#' ################################################################################
#'
#' #' process_a_select_all_response_as_ones_and_zeros
#' #'
#' #' @param is_all_column_data_missing_for_row is_all_column_data_missing_for_row
#' #' @param variable variable
#' #'
#' #' @return processed_var
#' #' @export
#' #'
#' process_a_select_all_response_as_ones_and_zeros <- function(is_all_column_data_missing_for_row, variable) {
#'   processed_var <- dplyr::case_when(
#'     is_all_column_data_missing_for_row == TRUE ~ NA,
#'     variable == 0 ~ 0,
#'     !is.na(variable) ~ 1,
#'     is.na(variable) ~ 0
#'   )
#'
#'   return(processed_var)
#' }
#'
#' ################################################################################
#'
#' #' process_select_all_question
#' #'
#' #' @param item_set_selector item_set_selector
#' #' @param df df
#' #'
#' #' @return df
#' #' @export
#' #'
#' process_select_all_question <- function(df, item_set_selector) {
#'
#'   # Obtain the variable names for this question
#'   list_of_variable_names <- df %>%
#'     dplyr::select(tidyselect::starts_with({{item_set_selector}})) %>% colnames()
#'
#'   # Check if all columns are missing data
#'   is_all_column_data_missing_for_row <- check_if_all_are_missing_from_row(df, list_of_variable_names)
#'
#'   # Go though each variable
#'   purrr::walk(list_of_variable_names, ~ {
#'
#'     # Get the index for the var
#'     index_for_var_to_change <- which(colnames(df) == .x)
#'
#'     # Select the variable
#'     vector_to_change <- df[,index_for_var_to_change][[1]]
#'
#'     # Convert those that are missing everything to NA, but change all other NAs to zero
#'     df[,index_for_var_to_change] <<- process_a_select_all_response_as_ones_and_zeros(
#'       is_all_column_data_missing_for_row,
#'       df[[.x]]
#'     )
#'   })
#'
#'   return(df)
#' }
#'
#' ################################################################################
#'
#' #' process_an_allocation_response_as_values_and_zeros
#' #'
#' #' @param is_all_column_data_missing_for_row is_all_column_data_missing_for_row
#' #' @param variable variable
#' #'
#' #' @return processed_var
#' #' @export
#' #'
#' process_an_allocation_response_as_values_and_zeros <- function(is_all_column_data_missing_for_row, variable) {
#'   processed_var <- dplyr::case_when(
#'     is_all_column_data_missing_for_row == TRUE ~ NA,
#'     !is.na(variable) ~ as.numeric(variable),
#'     is.na(variable) ~ 0
#'   )
#'
#'   return(processed_var)
#' }
#'
#' ################################################################################
#'
#' #' process_allocation_question
#' #'
#' #' @param item_set_selector item_set_selector
#' #' @param df df
#' #'
#' #' @return df
#' #' @export
#' #'
#' process_allocation_question <- function(df, item_set_selector) {
#'
#'   list_of_variable_names <- survey_data %>%
#'     dplyr::select(tidyselect::starts_with({{item_set_selector}})) %>% colnames()
#'
#'   is_all_column_data_missing_for_row <- check_if_all_are_missing_from_row(df, list_of_variable_names)
#'
#'   # process_a_select_all_response_as_ones_and_zeros(is_all_column_data_missing_for_row, variable)
#'
#'   purrr::walk(list_of_variable_names, ~ {
#'
#'     index_for_var_to_change <- which(colnames(df) == .x)
#'
#'     vector_to_change <- df[,index_for_var_to_change][[1]]
#'
#'     df[,index_for_var_to_change] <<- process_an_allocation_response_as_values_and_zeros(
#'       is_all_column_data_missing_for_row,
#'       df[[.x]]
#'     )
#'   })
#'
#'   return(df)
#' }
#'
#' ################################################################################
#'
#' #' process_ordered_factor_set_item_set
#' #'
#' #' @param df df
#' #' @param item_set_selector item_set_selector
#' #'
#' #' @return df
#' #' @export
#' #'
#' process_ordered_factor_set_item_set <- function(df, item_set_selector) {
#'
#'   var_names <- survey_data %>%
#'     dplyr::select(tidyselect::starts_with({{item_set_selector}})) %>% colnames()
#'
#'   df <- df %>%
#'     dplyr::mutate(
#'       dplyr::across(
#'         var_names,
#'         ~{as.numeric(.x)},
#'         .names = "numeric_{.col}"))
#'
#'   numeric_var_names <- paste0("numeric_", var_names)
#'
#'   total_score <- df %>%
#'     dplyr::select(tidyselect::all_of(numeric_var_names)) %>%
#'     rowMeans()
#'
#'   total_score_var_name <- paste0(
#'     "total_score_",
#'     stringr::str_replace(item_set_selector, "[_]{1,2}$", "")
#'   )
#'
#'   df <- df %>% dplyr::mutate({{total_score_var_name}} := total_score)
#'
#'   return(df)
#' }
#'
#' ################################################################################
#'
#' #' create_survey_folder_structure_paths_list
#' #'
#' #' @param project_year project_year
#' #' @param project_folder_name project_folder_name
#' #' @param list_of_survey_version_names list_of_survey_version_names
#' #' @param storage_directory storage_directory
#' #' @param create_survey_folders_if_they_do_not_exist create_survey_folders_if_they_do_not_exist
#' #'
#' #' @return list_of_folders_paths
#' #' @export
#' #'
#' create_survey_folder_structure_paths_list <- function(
#'     project_year,
#'     project_folder_name,
#'     list_of_survey_version_names = list("All_Versions"),
#'     all_versions_in_same_analysis_folder = FALSE,
#'     storage_directory = "Dropbox (TCM)",
#'     should_create_survey_folders_if_they_do_not_exist = TRUE
#' ) {
#'   # Get the parts of your working directory
#'   parts_of_wd <- unlist(strsplit(getwd(), .Platform$file.sep))
#'
#'   # Re-Combine the first 3 parts
#'   path_part_to_adjust <- paste0(parts_of_wd[1:3], collapse = "/")
#'
#'   # Create the home directory for the current project
#'   home_dir <- paste0(path_part_to_adjust, "/", storage_directory, "/04 MDM Neuro-Fundraising Lab/00 Jobs/", as.character(project_year), "/", as.character(project_folder_name))
#'
#'   # Analysis Folder
#'   project_analysis_folder_location <- paste0(home_dir, "/Analysis")
#'   project_analysis_respondent_investigation_folder_location <- paste0(home_dir, "/Analysis/Respondent Investigation")
#'
#'   # Archive Folder
#'   project_archive_folder_location <- paste0(home_dir, "/Archive")
#'
#'   # Assets Folder
#'   project_assets_folder_location <- paste0(home_dir, "/Assets")
#'
#'   # Data Collection
#'   project_data_collection_folder_location <- paste0(home_dir, "/Data Collection")
#'   project_data_collection_survey_monkey_data_folder_location <- paste0(home_dir, "/Data Collection/survey_monkey_data")
#'   project_data_collection_survey_monkey_data_processed_data_folder_location <- paste0(home_dir, "/Data Collection/survey_monkey_data/processed_data")
#'
#'   snake_version_names <- snakecase::to_snake_case(list_of_survey_version_names %>% unlist())
#'
#'   version_folders_location_names <- paste0(
#'     "project_data_collection_survey_monkey_data_version_folder_",
#'     snake_version_names
#'   )
#'
#'   project_data_collection_survey_monkey_data_version_folders_location <-
#'     purrr::map(list_of_survey_version_names, ~{
#'       paste0(home_dir, "/Data Collection/survey_monkey_data/", .x)
#'     }) %>% purrr::set_names(version_folders_location_names)
#'
#'   # Instructions
#'   project_instructions_folder_location <- paste0(home_dir, "/Instructions")
#'
#'   # Presentation
#'   project_presentation_folder_location <- paste0(home_dir, "/Presentation")
#'
#'   # Presentation
#'   project_proposal_folder_location <- paste0(home_dir, "/Proposal")
#'
#'   if(all_versions_in_same_analysis_folder == TRUE){
#'     list_of_survey_version_names <- list("All_Versions")
#'     snake_version_names <- snakecase::to_snake_case(list_of_survey_version_names %>% unlist())
#'   }
#'
#'   survey_version_analysis_folder_names <- paste0("survey_version_analysis_folder_", snake_version_names)
#'
#'
#'
#'
#'   project_power_bi_version_folders_location <-
#'     purrr::map(list_of_survey_version_names, ~{
#'       paste0(project_analysis_respondent_investigation_folder_location, "/", .x)
#'     }) %>% purrr::set_names(survey_version_analysis_folder_names)
#'
#'
#'   survey_version_analysis_deck_names <- paste0(survey_version_analysis_folder_names, "_Power_BI_Deck_folder")
#'   survey_version_analysis_processed_data_names <- paste0(survey_version_analysis_folder_names, "_processed_data_folder")
#'   survey_version_analysis_processed_text_names <- paste0(survey_version_analysis_folder_names, "_processed_text_folder")
#'
#'   survey_version_analysis_deck_folder_location <- purrr::map(project_power_bi_version_folders_location, ~{
#'     paste0(.x, "/Power_BI_Deck")
#'   }) %>% purrr::set_names(survey_version_analysis_deck_names)
#'
#'   survey_version_analysis_processed_data_folder_location <- purrr::map(project_power_bi_version_folders_location, ~{
#'     paste0(.x, "/processed_data")
#'   }) %>% purrr::set_names(survey_version_analysis_processed_data_names)
#'
#'   survey_version_analysis_processed_text_folder_location <- purrr::map(project_power_bi_version_folders_location, ~{
#'     paste0(.x, "/processed_text")
#'   }) %>% purrr::set_names(survey_version_analysis_processed_text_names)
#'
#'   # Maybe add TCM Folders Later
#'
#'   # Put these together as a list
#'   list_of_folders_paths <- list(
#'     "home_dir" = home_dir,
#'     "project_analysis_folder_location" = project_analysis_folder_location,
#'     "project_analysis_respondent_investigation_folder_location" = project_analysis_respondent_investigation_folder_location,
#'     "project_archive_folder_location, project_assets_folder_location" = project_archive_folder_location,
#'     "project_assets_folder_location" = project_assets_folder_location,
#'     "project_data_collection_folder_location" = project_data_collection_folder_location,
#'     "project_data_collection_survey_monkey_data_folder_location" = project_data_collection_survey_monkey_data_folder_location,
#'     "project_data_collection_survey_monkey_data_processed_data_folder_location" = project_data_collection_survey_monkey_data_processed_data_folder_location,
#'     "project_instructions_folder_location" = project_instructions_folder_location,
#'     "project_presentation_folder_location" = project_presentation_folder_location,
#'     "project_proposal_folder_location" = project_proposal_folder_location
#'   ) %>% append(project_data_collection_survey_monkey_data_version_folders_location) %>%
#'     append(project_power_bi_version_folders_location) %>%
#'     append(survey_version_analysis_deck_folder_location) %>%
#'     append(survey_version_analysis_processed_data_folder_location) %>%
#'     append(survey_version_analysis_processed_text_folder_location)
#'
#'
#'   if(should_create_survey_folders_if_they_do_not_exist == TRUE) {
#'     # If theses do not exist, create them.
#'     create_survey_folders_if_they_do_not_exist(list_of_folders_paths)
#'   }
#'
#'   return(list_of_folders_paths)
#' }
#'
#' ################################################################################
#'
#' #' create_survey_folders_if_they_do_not_exist
#' #'
#' #' @param survey_folder_structure_paths_list survey_folder_structure_paths_list
#' #'
#' #' @export
#' #'
#' create_survey_folders_if_they_do_not_exist <- function(survey_folder_structure_paths_list) {
#'   # If the folder does not exist, create it
#'   for(i in survey_folder_structure_paths_list) {
#'     if (!dir.exists(i)){
#'       dir.create(i)
#'       warning_message <- glue::glue("New Folder was created: {i}")
#'       warning(warning_message)
#'     }
#'   }
#' }
#'
#' ################################################################################
#'
#'
#' #' create_variable_specific_message
#' #'
#' #' @param df df
#' #' @param goal_n goal_n
#' #' @param variable_name variable_name
#' #' @param variable_levels variable_levels
#' #' @param required_proportions required_proportions
#' #'
#' #' @return variable_specific_message
#' #' @export
#' #'
#' create_variable_specific_message <- function(df,  goal_n, variable_name, variable_levels, required_proportions) {
#'
#'   count_table <- ftable(df[[{{variable_name}}]])
#'   prop_table <- round(prop.table(count_table) * 100, 2)
#'   levels <- levels(df[[{{variable_name}}]])
#'
#'   check_that_variable_match <- length(unique(list(variable_levels, levels))) == 1
#'
#'   if(check_that_variable_match == FALSE) {
#'     stop("There is an issue with the specified variable levels for the create_variable_specific_message function.")
#'   }
#'
#'   Variable_Name <- stringr::str_to_title(variable_name)
#'
#'   goal_sample_size <- (required_proportions[levels] * goal_n)
#'
#'   list_of_parameters <- list(count_table, levels, goal_sample_size)
#'
#'   level_sentences <- purrr::pmap(list_of_parameters, ~ {
#'     glue::glue("{..2} - We currently have {..1}. This should be close to {..3}.
#'                ") %>% paste0(collapse = "\n")
#'   })
#'
#'   variable_specific_message <- paste0(Variable_Name, ": \n", paste0(level_sentences, collapse = "\n"))
#'
#'   return(variable_specific_message)
#' }
#'
#' #' create_variable_specific_message_list
#' #'
#' #' @param df df
#' #' @param goal_n goal_n
#' #' @param list_of_variable_names list_of_variable_names
#' #' @param list_of_variable_levels list_of_variable_levels
#' #' @param list_of_required_proportions list_of_required_proportions
#' #'
#' #' @return variable_related_messages
#' #' @export
#' #'
#' create_variable_specific_message_list <- function(df, goal_n, list_of_variable_names, list_of_variable_levels, list_of_required_proportions) {
#'
#'   parameter_list <- list(list_of_variable_names, list_of_variable_levels, list_of_required_proportions)
#'
#'   variable_related_messages <- purrr::pmap(parameter_list, ~{
#'     variable_specific_message <- create_variable_specific_message(df,  goal_n, ..1, ..2, ..3)
#'     variable_specific_message
#'   })
#'
#'   return(variable_related_messages)
#' }
#'
#' #' create_update_message_for_CINT
#' #'
#' #' @param df df
#' #' @param goal_n goal_n
#' #' @param list_of_variable_names list_of_variable_names
#' #' @param list_of_variable_levels list_of_variable_levels
#' #' @param list_of_required_proportions list_of_required_proportions
#' #'
#' #' @return final_message
#' #' @export
#' #'
#' create_update_message_for_CINT <- function(df, goal_n, list_of_variable_names, list_of_variable_levels, list_of_required_proportions) {
#'
#'   # Count the number of quality participants that we have
#'   n_completes_quality <- nrow(df)
#'
#'   parameter_list <- list(list_of_variable_names, list_of_variable_levels, list_of_required_proportions)
#'
#'   variable_specific_message_list <- create_variable_specific_message_list(
#'     df = df,
#'     goal_n = goal_n,
#'     list_of_variable_names = list_of_variable_names,
#'     list_of_variable_levels = list_of_variable_levels,
#'     list_of_required_proportions = list_of_required_proportions
#'   )
#'
#'   the_specific_variables_message <- paste0(variable_specific_message_list, collapse = "\n\n")
#'
#'   final_message <- glue::glue(
#'     "DATA COLLECTION - PROGRESS SUMMARY
#'
#'     We currently have {n_completes_quality}/{goal_n} completed participants. Here are some details on the demographics and our quotas.
#'
#'     {the_specific_variables_message}
#'
#'     Note: We will do data quality checks throughout data collection, so these numbers will likely change."
#'   )
#'
#'   return(final_message)
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
