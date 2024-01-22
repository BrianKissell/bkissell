#'
#' #' obtain_project_design_information_xlsx_path
#' #'
#' #' @param project_year project_year
#' #' @param project_folder_name project_folder_name
#' #'
#' #' @return project_design_information_xlsx_path
#' #' @export
#' #'
#'
#' obtain_project_design_information_xlsx_path <- function(project_year, project_folder_name) {
#'
#'   path_part_to_adjust <- paste0(unlist(strsplit(getwd(), .Platform$file.sep))[1:3], collapse = "/")
#'
#'   project_location_part <- paste0(
#'     "/Moore DM Group/04 MDM Neuro-Fundraising Lab/00 Jobs/",
#'     as.character({{project_year}}), "/",
#'     as.character({{project_folder_name}})
#'   )
#'
#'   project_design_information_xlsx_path <- paste0(
#'     path_part_to_adjust,
#'     project_location_part,
#'     "/project_design_information.xlsx"
#'   )
#'
#'   return(project_design_information_xlsx_path)
#' }
#'
#'
#'
#'
#' #' create_file_paths_df
#' #'
#' #' @param project_year project_year
#' #' @param project_folder_name project_folder_name
#' #'
#' #' @return file_paths_df
#' #' @export
#' #'
#'
#' create_file_paths_df <- function(project_year, project_folder_name) {
#'
#'   project_dir <- paste0(
#'     "/Moore DM Group/04 MDM Neuro-Fundraising Lab/00 Jobs/",
#'     as.character({{project_year}}), "/",
#'     as.character({{project_folder_name}})
#'   )
#'
#'   analysis_dir <- paste0(project_dir, "/Analysis")
#'
#'   analysis_immersion_dir <- paste0(analysis_dir, "/Immersion")
#'
#'   immersion_animations_dir <- paste0(analysis_immersion_dir, "/Animations")
#'
#'   immersion_animations_videos_dir  <- paste0(immersion_animations_dir, "/Analysis Videos")
#'
#'   immersion_raw_data_dir <- paste0(analysis_immersion_dir, "/Raw Data")
#'
#'   immersion_processed_dir <- paste0(analysis_immersion_dir, "/Processed Immersion")
#'
#'   file_paths_df <- data.frame(
#'     file_path_name = c(
#'       "project_dir",
#'       "analysis_dir",
#'       "analysis_immersion_dir",
#'       "immersion_animations_dir",
#'       "immersion_animations_videos_dir",
#'       "immersion_raw_data_dir",
#'       "immersion_processed_dir"
#'     ),
#'     file_path = c(
#'       project_dir,
#'       analysis_dir,
#'       analysis_immersion_dir,
#'       immersion_animations_dir,
#'       immersion_animations_videos_dir,
#'       immersion_raw_data_dir,
#'       immersion_processed_dir
#'     )
#'   )
#'
#'   return(file_paths_df)
#' }
#'
#'
#' #' obtain_current_file_path_parameters_df
#' #'
#' #' @param file_paths_df file_paths_df
#' #'
#' #' @return current_file_path_parameters_df
#' #' @export
#' #'
#'
#' obtain_current_file_path_parameters_df <- function(file_paths_df) {
#'   path_part_to_adjust <- paste0(unlist(strsplit(getwd(), .Platform$file.sep))[1:3], collapse = "/")
#'
#'   current_file_paths_df <- file_paths_df
#'
#'   current_file_paths_df$file_path <- purrr::map_chr(current_file_paths_df$file_path, ~ paste0(path_part_to_adjust, .x))
#'
#'   current_file_path_parameters_df <- current_file_paths_df %>%
#'     tidyr::pivot_wider(names_from = file_path_name, values_from = file_path)
#'
#'   return(current_file_path_parameters_df)
#' }
#'
#' #' obtain_initial_project_info_parameters_df
#' #'
#' #' @param initial_project_info_df initial_project_info_df
#' #'
#' #' @return initial_project_info_parameters_df
#' #' @export
#' #'
#'
#' obtain_initial_project_info_parameters_df <- function(initial_project_info_df) {
#'   initial_project_info_parameters_df <- initial_project_info_df %>%
#'     tidyr::pivot_wider(names_from = parameter_name, values_from = parameter_value)
#'
#'   return(initial_project_info_parameters_df)
#' }
#'
#'
#'
#'
#'
#'
#' #' create_initial_project_info
#' #'
#' #' @param project_year project_year
#' #' @param project_folder_name project_folder_name
#' #' @param study_type_selection study_type_selection
#' #' @param study_location_selection study_location_selection
#' #' @param n_participants_per_session_options n_participants_per_session_options
#' #' @param total_goal_sample_size total_goal_sample_size
#' #'
#' #' @return initial_project_info_df
#' #' @export
#' #'
#'
#' create_initial_project_info <- function(
#'     project_year,
#'     project_folder_name,
#'     study_type_selection,
#'     study_location_selection,
#'     n_participants_per_session_options,
#'     total_goal_sample_size
#' ) {
#'
#'   initial_project_info_df <- data.frame(
#'     parameter_name = c(
#'       "project_year",
#'       "project_folder_name",
#'       "study_type_selection",
#'       "study_location_selection",
#'       "n_participants_per_session_options",
#'       "total_goal_sample_size"
#'     ),
#'     parameter_value = c(
#'       project_year,
#'       project_folder_name,
#'       study_type_selection,
#'       study_location_selection,
#'       n_participants_per_session_options,
#'       total_goal_sample_size
#'     )
#'   )
#'
#'   return(initial_project_info_df)
#' }
#'
#'
#' #' create_immersion_video_vars_df
#' #'
#' #' @param video_presentation_type_vec video_presentation_type_vec
#' #' @param video_name_vec video_name_vec
#' #' @param video_duration_vec video_duration_vec
#' #'
#' #' @return immersion_video_vars_df
#' #' @export
#' #'
#'
#' create_immersion_video_vars_df <- function(video_presentation_type_vec,	video_name_vec,	video_duration_vec) {
#'
#'   immersion_video_vars_df <- data.frame(
#'     video_presentation_type_vec = video_presentation_type_vec,
#'     video_name_vec = video_name_vec,
#'     video_duration_vec = video_duration_vec
#'   )
#'
#'   return(immersion_video_vars_df)
#' }
#'
#'
#'
#'
#'
#'
#' #' obtain_immersion_video_details_df
#' #'
#' #' @param immersion_video_vars_df immersion_video_vars_df
#' #' @param total_goal_sample_size total_goal_sample_size
#' #'
#' #' @return immersion_video_details_df
#' #' @export
#' #'
#'
#' obtain_immersion_video_details_df <- function(immersion_video_vars_df, total_goal_sample_size) {
#'   bs_immersion_video_vars_df <- immersion_video_vars_df %>%
#'     dplyr::filter(video_presentation_type_vec == "Between-Subjects")
#'
#'   ws_immersion_video_vars_df <- immersion_video_vars_df %>%
#'     dplyr::filter(video_presentation_type_vec == "Within-Subjects")
#'
#'   other_immersion_video_vars_df <- immersion_video_vars_df %>%
#'     dplyr::filter(video_presentation_type_vec != "Between-Subjects" & video_presentation_type_vec != "Within-Subjects")
#'
#'   if(nrow(other_immersion_video_vars_df) > 0) {
#'     stop("The only valid options are 'Between-Subjects' and 'Within-Subjects'. Please check the entered `video_presentation_type`.")
#'   }
#'
#'   n_bs_videos <- nrow(bs_immersion_video_vars_df)
#'
#'   n_ws_videos <- nrow(ws_immersion_video_vars_df)
#'
#'   goal_sample_size_per_bs_video <- ifelse(n_bs_videos == 0, 0, total_goal_sample_size / n_bs_videos)
#'
#'   goal_sample_size_per_ws_video <- ifelse(n_ws_videos == 0, 0, total_goal_sample_size)
#'
#'   immersion_video_details_df <- data.frame(
#'     n_bs_videos = n_bs_videos,
#'     n_ws_videos = n_ws_videos,
#'     goal_sample_size_per_bs_video = goal_sample_size_per_bs_video,
#'     goal_sample_size_per_ws_video = goal_sample_size_per_ws_video
#'   )
#'
#'   return(immersion_video_details_df)
#' }
#'
#'
#' #' create_user_project_info_df
#' #'
#' #' @param user_project_name_vec user_project_name_vec
#' #' @param user_project_number_vec user_project_number_vec
#' #'
#' #' @return user_project_info_df
#' #' @export
#' #'
#'
#' create_user_project_info_df <- function(user_project_name_vec, user_project_number_vec){
#'   user_project_info_df <- data.frame(
#'     user_project_name = user_project_name_vec,
#'     user_project_number = user_project_number_vec
#'   )
#'
#'   return(user_project_info_df)
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
#'
#'
#'
#'
#'
#'
#'
#' # Create Immersion Folder Structure Paths ---------------------------------------
#'
#' #' create_immersion_folder_structure_paths_list
#' #'
#' #' @param project_year project_year
#' #' @param project_folder_name project_folder_name
#' #'
#' #' @return list_of_folders_paths
#' #' @export
#' #'
#' create_immersion_folder_structure_paths_list <- function(
#'     project_year,
#'     project_folder_name
#' ) {
#'   # Get the parts of your working directory
#'   parts_of_wd <- unlist(strsplit(getwd(), .Platform$file.sep))
#'
#'   # Re-Combine the first 3 parts
#'   path_part_to_adjust <- paste0(parts_of_wd[1:3], collapse = "/")
#'
#'   # Create the home directory for the current project
#'   home_dir <- paste0(path_part_to_adjust, "/Moore DM Group/04 MDM Neuro-Fundraising Lab/00 Jobs/", as.character(project_year), "/", as.character(project_folder_name))
#'
#'   # Analysis Folder
#'   project_analysis_folder_location <- paste0(home_dir, "/Analysis")
#'
#'   # Immersion Folder
#'   project_analysis_immersion_folder_location <- paste0(home_dir, "/Analysis/Immersion")
#'
#'   # Animations Folder
#'   project_animations_folder_location <- paste0(home_dir, "/Analysis/Immersion/Animations")
#'
#'   # Analysis Videos Folder
#'   project_analysis_videos_folder_location <- paste0(home_dir, "/Analysis/Immersion/Animations/Analysis Videos")
#'
#'   # Raw Data Folder
#'   project_raw_data_folder_location <- paste0(home_dir, "/Analysis/Immersion/Raw Data")
#'
#'   # Processed Immersion
#'   project_processed_immersion_folder_location <- paste0(home_dir, "/Analysis/Immersion/Processed Immersion")
#'
#'   # Put these together as a list
#'   list_of_folders_paths <- list(
#'     "home_dir" = home_dir,
#'     "project_analysis_folder_location" = project_analysis_folder_location,
#'     "project_analysis_immersion_folder_location" = project_analysis_immersion_folder_location,
#'     "project_animations_folder_location" = project_animations_folder_location,
#'     "project_analysis_videos_folder_location" = project_analysis_videos_folder_location,
#'     "project_raw_data_folder_location" = project_raw_data_folder_location,
#'     "project_processed_immersion_folder_location" = project_processed_immersion_folder_location
#'   )
#'
#'   # Check if the files exist, and create them if they do not.
#'   create_folders_if_they_do_not_exist(list_of_folder_paths = list_of_folders_paths)
#'
#'   return(list_of_folders_paths)
#' }
#'
#' ################################################################################
#' # Obtain the session information from an immersion file name --------------------
#'
#' #' Get the elements from the raw immersion filename
#' #'
#' #' @param raw_immersion_file_path raw_immersion_file_path
#' #'
#' #' @return elements_from_raw_immersion_filename
#' #' @export
#' #'
#' get_elements_from_raw_immersion_filename <- function(raw_immersion_file_path){
#'   # Obtain the file name
#'   file_name <- basename(raw_immersion_file_path)
#'
#'   # Remove the file extension
#'   text_to_extract_from <- stringr::str_replace(file_name, ".xlsx", "")
#'
#'   # To detect copies, use the following
#'   detect_copy_number <- stringr::str_detect(text_to_extract_from, " {0,2}\\([0-9]{1,2}\\)$")
#'   if(detect_copy_number){
#'     copy_number <- stringr::str_extract(text_to_extract_from, " {0,2}\\([0-9]{1,2}\\)$")
#'     copy_number <- as.numeric(stringr::str_extract(copy_number, "[0-9]{1,2}"))
#'     copy_number <- copy_number + 1
#'     text_to_extract_from <- stringr::str_replace(text_to_extract_from, " {0,2}\\([0-9]{1,2}\\)", "")
#'   } else {
#'     copy_number <- 1
#'   }
#'
#'   # Extract the second from file name
#'   second_started <- stringr::str_extract(text_to_extract_from, "[0-9]{2}$") %>%
#'     as.numeric()
#'
#'   # Remove the second from the file name
#'   text_to_extract_from <- stringr::str_replace(text_to_extract_from, "_[0-9]{2}$", "")
#'
#'   # Extract the minute from the file name
#'   minute_started <- stringr::str_extract(text_to_extract_from, "[0-9]{2}$") %>%
#'     as.numeric()
#'
#'   # Remove the minute from the file name
#'   text_to_extract_from <- stringr::str_replace(text_to_extract_from, "_[0-9]{2}$", "")
#'
#'   # Extract the hour from the file name
#'   hour_started <- stringr::str_extract(text_to_extract_from, "[0-9]{2}$") %>%
#'     as.numeric()
#'
#'   # Remove the hour from the file name
#'   text_to_extract_from <- stringr::str_replace(text_to_extract_from, "_[0-9]{2}$", "")
#'
#'   # Extract the year from the file name
#'   year_started <- stringr::str_extract(text_to_extract_from, "[0-9]{4}$") %>%
#'     as.numeric()
#'
#'   # Remove the year from the file name
#'   text_to_extract_from <- stringr::str_replace(text_to_extract_from, "_[0-9]{4}$", "")
#'
#'   # Extract the day from the file name
#'   day_started <- stringr::str_extract(text_to_extract_from, "[0-9]{2}$") %>%
#'     as.numeric()
#'
#'   # Remove the day from the file name
#'   text_to_extract_from <- stringr::str_replace(text_to_extract_from, "_[0-9]{2}$", "")
#'   # Extract the month from the file name
#'
#'   month_started <- stringr::str_extract(text_to_extract_from, "[0-9]{2}$") %>%
#'     as.numeric()
#'
#'   # Remove the month from the file name
#'   text_to_extract_from <- stringr::str_replace(text_to_extract_from, "_[0-9]{2}$", "")
#'
#'   # Extract the timezone from the file name
#'   timezone <- stringr::str_extract(text_to_extract_from, "[a-zA-Z]{3}$") %>%
#'     as.character()
#'
#'   # Remove the timezone from the file name
#'   text_to_extract_from <- stringr::str_replace(text_to_extract_from, "_[a-zA-Z]{3}$", "")
#'
#'   # Extract the experience id from the file name
#'   experience_id <- stringr::str_extract(text_to_extract_from, "[a-zA-Z0-9]{5}$") %>%
#'     as.character()
#'
#'   # Remove the experience from the file name
#'   text_to_extract_from <- stringr::str_replace(text_to_extract_from, "_[a-zA-Z0-9]{5}$", "")
#'
#'   # Extract the given session name from the file name
#'   given_session_name <- text_to_extract_from
#'
#'   # Put all of the data into a dataframe
#'   elements_from_raw_immersion_filename <- data.frame(
#'     given_session_name, experience_id, timezone, month_started, day_started,
#'     year_started, hour_started, minute_started, second_started, copy_number)
#'
#'   # Return the dataframe
#'   return(elements_from_raw_immersion_filename)
#' }
#'
#' ################################################################################
#' # Obtain the session information df from a list of immersion file names --------------------
#'
#' #' get_session_information_from_list_of_file_names
#' #'
#' #' @param immersion_file_paths immersion_file_paths
#' #'
#' #' @return session_information_from_file_names
#' #' @export
#' #'
#' get_session_information_from_list_of_file_names <- function(immersion_file_paths) {
#'
#'   # Go through every file
#'   session_information_from_file_names <- purrr::map_df(immersion_file_paths, ~{
#'
#'     # Create a dataframe containing the path
#'     path_df <- data.frame(file_path = .x) %>%
#'
#'       # get the file information and add it to this dataframe
#'       cbind(get_elements_from_raw_immersion_filename(.x))
#'
#'     # Return the data frame for that file
#'     path_df
#'   })
#'
#'   # Return the full data frame
#'   return(session_information_from_file_names)
#' }
#'
#' ################################################################################
#' # Fill NAs that follow a label with that label ----------------------------
#'
#' # This function replaces NA values with the most recent valid label. NA's in a vector or factor are replaced with last non-NA values. If firstBack is TRUE, it will fill in leading NA's with the first non-NA value. If FALSE, it will not change leading NA's.
#' #' Replace missing data with the appropriate label.
#' #'
#' #' Replaces NA values with the most recent valid label.
#' #'
#' #' @param x A vector of categorical values or labels.
#' #'
#' #' @param firstBack A Boolean variable which should usually be false, so that it will keep the first NAs as NA. However, if I want to fill in the first NAs with the first label, I would need to make first back TRUE.
#' #'
#' #' @author Brian Kissell
#' #'
#' #' @import dplyr
#' #' @return x
#' #'
#' #' @export
#'
#' na_fill <- function(x, firstBack=FALSE) {
#'
#'   # If it's a factor, store the level labels and convert to integer
#'   lvls <- NULL
#'   if(is.factor(x)){
#'     lvls <- levels(x)
#'     x <- as.integer(x)
#'   }
#'
#'   # Save which indices are valid labels
#'   goodIdx <- !is.na(x)
#'
#'   # Create a vector that has all of the valid labels
#'
#'   if (firstBack){
#'     goodVals <- c(x[goodIdx][1], x[goodIdx])
#'   } else {
#'     goodVals <- c(NA, x[goodIdx])
#'   }
#'
#'   # Fill the indices of the output vector with the indices pulled from
#'   # these offsets of goodVals. Add 1 to avoid indexing to zero.
#'   # In other words, cumsum is what will decide which values need to be changed,
#'   # as NAs will not increase the index
#'   fillIdx <- cumsum(goodIdx) + 1
#'
#'   # Convert these numbers back into the valid labels
#'   x <- goodVals[fillIdx]
#'
#'   # If it was originally a factor, convert it back
#'   if (!is.null(lvls)) {
#'     x <- factor(x, levels=seq_along(lvls), labels=lvls)
#'   }
#'
#'   return(x)
#' }
#'
#' ################################################################################
#' # Replace Repeats over n with na ------------------------------------------
#'
#' #' Create a function to replaces any values that are repeated more than n times with an NA
#' #'
#' #' Create a function to replaces any values that are repeated more than n times with an NA
#' #'
#' #' @param var Provide the name of the variable as a string
#' #'
#' #' @param n Provide the number of repeated values to keep
#' #'
#' #' @return cleaned_values
#' #'
#' #' @import dplyr
#' #'
#' #' @export
#'
#' replace_repeats_over_n_with_na <- function(var, n = 6){
#'   # Find the values that repeat
#'   subs <- rle(var)
#'   subs$values[subs$lengths <= n] <- 0
#'   subs$values[subs$lengths > n] <- 1
#'   subs$values[subs$lengths > n] <- cumsum(subs$values[subs$lengths > n])
#'   repeated_values <- inverse.rle(subs)
#'   # Find the index for the first n repetitions
#'   indices_to_keep <- data.frame(repeated_values = repeated_values) %>%
#'     dplyr::mutate(index = row_number())  %>%
#'     dplyr::group_by(repeated_values) %>%
#'     dplyr::slice_max(order_by = desc(.data[["index"]]), n = n) %>%
#'     dplyr::pull(.data[["index"]])
#'   # Change any values that repeat more than n times to NA
#'   df <- data.frame(var = var, repeated_values = repeated_values) %>%
#'     mutate(index = row_number()) %>%
#'     mutate(
#'       new_value = ifelse(
#'         .data[["repeated_values"]] == 0,
#'         .data[["var"]],
#'         ifelse(
#'           .data[["index"]] %in% indices_to_keep,
#'           .data[["var"]],
#'           NA
#'         )
#'       )
#'     )
#'   # Get the new values as a vector
#'   cleaned_values <- df %>% pull(.data[["new_value"]])
#'   # Return the vector
#'   return(cleaned_values)
#' }
#'
#' ################################################################################
#'
#' #' Read and prepare the immersion data
#' #'
#' #' @param raw_data_path This is where the raw data files are located
#' #' @param save_file_name_session_information save_file_name_session_information
#' #' @param allow_n_repeats allow_n_repeats
#' #' @param remove_end_and_init_labels remove_end_and_init_labels
#' #' @param list_of_bs_video_names list_of_bs_video_names
#' #' @param list_of_bs_video_seconds list_of_bs_video_seconds
#' #' @param list_of_ws_video_names list_of_ws_video_names
#' #' @param list_of_ws_video_seconds list_of_ws_video_seconds
#' #' @param filter_video_lengths filter_video_lengths
#' #'
#' #' @return immersion_data
#' #' @export
#' #'
#' read_immersion_files <- function(
#'     raw_data_path,
#'     allow_n_repeats = 6,
#'     save_file_name_session_information = FALSE,
#'     remove_end_and_init_labels = TRUE,
#'     list_of_bs_video_names = NULL,
#'     list_of_bs_video_seconds = NULL,
#'     list_of_ws_video_names = NULL,
#'     list_of_ws_video_seconds = NULL,
#'     filter_video_lengths = TRUE
#' ){
#'
#'   # Obtain the file names for the immersion data
#'   immersion_file_names <- list.files(raw_data_path, pattern = ".xlsx")
#'
#'   if(identical(immersion_file_names, character(0))){
#'     rlang::abort(glue::glue("I am not seeing any xlsc files in {raw_data_path}, and thus nothing will be processed."))
#'   }
#'
#'   # Obtain the file paths for the immersion data
#'   immersion_file_paths <- paste0(raw_data_path, "/", immersion_file_names)
#'
#'   # Extract information from the file names and create a dataframe
#'   paths_df_init <- get_session_information_from_list_of_file_names(immersion_file_paths)
#'
#'   # Remove the duplicates
#'   paths_df <- paths_df_init %>%
#'     dplyr::group_by(given_session_name) %>%
#'     dplyr::slice_max(copy_number, n = 1)
#'
#'   # If we want to save this information, indicate so in the parameter
#'   if(save_file_name_session_information) {
#'
#'     # Create a path for the file
#'     path_for_session_information_from_file_names <- paste0(raw_data_path, "/session_information_from_file_names.csv")
#'
#'     # Save the file
#'     readr::write_csv(paths_df, path_for_session_information_from_file_names)
#'   }
#'
#'   # Setup progress meter
#'   pb <- progress::progress_bar$new(total = length(paths_df$file_path))
#'
#'   # Read and process the data
#'   immersion_data <- purrr::map_df(seq_along(paths_df$file_path), ~{
#'
#'     # Add one to the progress meter
#'     pb$tick()
#'
#'     # Read in the file
#'     individual_immersion_data <- suppressMessages(readxl::read_excel(paths_df$file_path[[.x]], guess_max = 10000))
#'
#'     # Put the column names in lower case
#'     colnames(individual_immersion_data) <- stringr::str_to_lower(colnames(individual_immersion_data))
#'
#'     # Clean a few labels
#'     colnames(individual_immersion_data) <- dplyr::case_when(
#'       colnames(individual_immersion_data) == "date" ~ "time",
#'       colnames(individual_immersion_data) == "datetime" ~ "time",
#'       colnames(individual_immersion_data) == "...1" ~ "time",
#'       colnames(individual_immersion_data) == "notes" ~ "label",
#'       stringr::str_detect(colnames(individual_immersion_data), "p[a-z0-9]{7,9}") ~ paste0("immersion_", colnames(individual_immersion_data)),
#'       TRUE ~ colnames(individual_immersion_data)
#'     )
#'
#'     # Immersion sometimes stores irrelavent information such as demographics and other things. It is too inconsistent to capture, so we just ignore that.
#'     individual_immersion_data <- individual_immersion_data %>%
#'       dplyr::select("time", starts_with("immersion_"), agenda, label, immersion)
#'
#'     # Put the labels in lower case to make them easier to work with
#'     individual_immersion_data$label <- stringr::str_to_lower(individual_immersion_data$label)
#'
#'     # fill in the notes
#'     individual_immersion_data$label <- na_fill(individual_immersion_data$label)
#'
#'     # Replace the initial marking that will be NAs with "init"
#'     individual_immersion_data$label <- tidyr::replace_na(individual_immersion_data$label, "init")
#'
#'     # Add the given session name to this df
#'     individual_immersion_data$given_session_name <- paths_df$given_session_name[[.x]]
#'
#'     # Add the experience id to the df
#'     individual_immersion_data$experience_id <- paths_df$experience_id[[.x]]
#'
#'     # if(keep_all_data == FALSE) {
#'     #   individual_immersion_data <- individual_immersion_data %>%
#'     #     dplyr::filter(label != "NA", label != "end")
#'     # }
#'
#'     # Adjust the df
#'     individual_immersion_data <- individual_immersion_data %>%
#'
#'       # group by the session name, the experience id, and the label to make sure each video will be labeled correctly
#'       dplyr::group_by(.data[["given_session_name"]],.data[["experience_id"]], .data[["label"]]) %>%
#'
#'       # Calculate the seconds
#'       dplyr::mutate(seconds = row_number(.data[["time"]])) %>%
#'
#'       # Ungroup
#'       dplyr::ungroup() %>%
#'
#'       # Select all of the variables needed columns
#'       dplyr::select(all_of("given_session_name"),all_of("experience_id"), all_of("seconds"), label = all_of("label"), dplyr::starts_with("immersion_"), all_of("time")) %>%
#'
#'       # Pivot so that each participant has its own row.
#'       tidyr::pivot_longer(cols = !c("given_session_name", "experience_id", "seconds", "label", "time"), names_to = "sensor_id", values_to = "immersion")
#'
#'     # Clean the labels for the sensor id data
#'     individual_immersion_data$sensor_id <- stringr::str_replace(individual_immersion_data$sensor_id, "immersion_", "")
#'
#'     # Order the data by the participant, the mark, and the time
#'     individual_immersion_data <- individual_immersion_data %>%
#'       dplyr::arrange(sensor_id, label, time, seconds)
#'
#'     # Replace values that are repeated more than 6 times with NA
#'     individual_immersion_data$immersion <- replace_repeats_over_n_with_na(individual_immersion_data$immersion, n = allow_n_repeats)
#'
#'     # Filter out invalid marks, and time after video was shown
#'
#'     # if(keep_all_data == FALSE) {
#'     #   individual_immersion_data <- dplyr::filter(individual_immersion_data, .data[["seconds"]] <= max_stimulis_second)
#'     # }
#'     # individual_immersion_data <- dplyr::filter(individual_immersion_data, .data[["seconds"]] <= max_stimulis_second)
#'
#'     # Return the dataframe
#'     individual_immersion_data
#'   })
#'
#'   # If there is a number at the end of the mark, grab it, or NA
#'   immersion_data$order <- stringr::str_extract(immersion_data$label, "[0-9]{1,4}$") %>% as.numeric()
#'
#'   # Remove the number to get the video mark
#'   immersion_data$video <- stringr::str_replace(immersion_data$label, "[0-9]{1,4}$", "")
#'
#'   # From the file name, grab the unique identifier (call it RIDS even if it is a session identifier)
#'   immersion_data$RIDS <- stringr::str_extract(immersion_data$given_session_name, "^[a-zA-Z0-9]{1,20}")
#'
#'   # Anything after the underscore
#'   immersion_data$experimental_condition <- stringr::str_replace(immersion_data$given_session_name, "^[a-zA-Z0-9]{1,20}[_]{0,1}", "")
#'
#'   # If indicated in the parameter
#'   if(remove_end_and_init_labels){
#'
#'     # Remove the data with the end and init labels
#'     immersion_data <- immersion_data %>%
#'       dplyr::filter(!(label %in% c("end", "init")))
#'
#'   }
#'
#'   # Initiate all_video_names list
#'   all_video_names <- list()
#'
#'   # Initiate all_video_seconds
#'   all_video_seconds <- list()
#'
#'   # If there are bs names
#'   if(!is.null(list_of_bs_video_names)){
#'
#'     # Add them to the list
#'     all_video_names <- append(all_video_names, list_of_bs_video_names)
#'
#'     # Check if the seconds were provided
#'     if(is.null(list_of_bs_video_seconds)) {
#'       rlang::abort("The between subjects videos do not have times specified.")
#'     }
#'
#'     # Add seconds to the list
#'     all_video_seconds <- append(all_video_seconds, list_of_bs_video_seconds)
#'
#'   }
#'
#'   # If there are ws videos
#'   if(!is.null(list_of_ws_video_names)){
#'
#'     # Add them to the list
#'     all_video_names <- append(all_video_names, list_of_ws_video_names)
#'
#'     # Check if seconds are provided
#'     if(is.null(list_of_ws_video_seconds)) {
#'       rlang::abort("The within subjects videos do not have times specified.")
#'     }
#'
#'     # Add seconds to the list
#'     all_video_seconds <- append(all_video_seconds, list_of_ws_video_seconds)
#'
#'   }
#'
#'   # As long as either bs or ws videos are provided
#'   if(!(is.null(list_of_bs_video_names) & is.null(list_of_ws_video_names))) {
#'
#'     # Should the data be filtered
#'     if(filter_video_lengths == TRUE) {
#'
#'       n_labels <- length(unique(immersion_data$video))
#'
#'       n_labels_we_should_have <- length(all_video_names)
#'
#'       if(remove_end_and_init_labels == FALSE) {
#'         n_labels_we_should_have <- n_labels_we_should_have + 2
#'       }
#'
#'       if(identical(n_labels, n_labels_we_should_have)) {
#'         # Filter the data
#'         immersion_data <- filter_video_times_with_list(
#'           df = immersion_data,
#'           list_of_video_names = all_video_names,
#'           list_of_video_seconds = all_video_seconds
#'         )
#'       } else {
#'         rlang::abort(glue::glue("There are {n_labels} unique labels, while there should be {n_labels_we_should_have}. Please check the labels in the raw data folder."))
#'       }
#'     }
#'   }
#'
#'   # Return the dataframe
#'   return(immersion_data)
#' }
#'
#' ################################################################################
#' ################################################################################
#' ################################################################################
#'
#' #' Obtain amount of missing data per rid, as well as mean and sd
#' #'
#' #' @param data dataframe
#' #'
#' #' @return immersion_individual_summary
#' #' @export
#' get_immersion_individual_summary <- function(data){
#'   amount_of_data_missing <- data %>%
#'     dplyr::group_by(.data[["sensor_id"]], .data[["given_session_name"]], .data[["label"]]) %>%
#'     dplyr::summarise(
#'       AVG_Immersion = mean(.data[["immersion"]], na.rm = TRUE),
#'       SD_Immersion = sd(.data[["immersion"]], na.rm = TRUE),
#'       missing_data_counts = sum(is.na(.data[["immersion"]])),
#'       n = n(),
#'       percent_missing = round((.data[["missing_data_counts"]] / .data[["n"]]) * 100, 2),
#'       immersion_status = ifelse(.data[["percent_missing"]] >= 30, "Exclude due to too much missing data", "Include data"),
#'       .groups = "drop")
#'
#'   immersion_individual_summary <- amount_of_data_missing %>%
#'     dplyr::select(
#'       sensor_id, label, AVG_Immersion, SD_Immersion, missing_data_counts, n,
#'       percent_missing, immersion_status, given_session_name)
#'
#'   return(immersion_individual_summary)
#' }
#'
#' ################################################################################
#' ################################################################################
#' ################################################################################
#'
#' #' Filter out excluded immersion data
#' #'
#' #' @param all_immersion_data immersion data before exclusions
#' #'
#' #' @return filtered_immersion_data
#' #' @export
#' #'
#'
#' get_filtered_immersion_data <- function(all_immersion_data){
#'
#'   # Obtain summary immersion data
#'   immersion_individual_summary <- get_immersion_individual_summary(all_immersion_data)
#'
#'   # Obtain the ids for the completed immersion participants
#'   good_immersion_ids <- immersion_individual_summary %>%
#'     dplyr::filter(immersion_status == "Include data") %>%
#'     dplyr::pull(sensor_id)
#'
#'   # Filter for good immersion data
#'   filtered_immersion_data <- all_immersion_data %>%
#'     dplyr::filter(sensor_id %in% good_immersion_ids)
#'
#'   # Return dataframe
#'   return(filtered_immersion_data)
#' }
#'
#' ################################################################################
#' ################################################################################
#' ################################################################################
#'
#' #' Get Immersion Per Second Data
#' #'
#' #' @param filtered_immersion_data data after filtering our the participants that need to be excluded.
#' #' @param grouped_by grouped_by
#' #'
#' #' @return immersion_per_second
#' #' @export
#' #'
#'
#' get_immersion_per_second <- function(filtered_immersion_data, grouped_by){
#'   # Create the averaged per second immersion scores for all labels
#'   immersion_per_second <- filtered_immersion_data %>%
#'     dplyr::group_by(.data[["seconds"]], .data[[{{grouped_by}}]]) %>%
#'     dplyr::summarise(score = mean(immersion, na.rm = TRUE), .groups = "drop") %>%
#'     dplyr::arrange(.data[[{{grouped_by}}]], .data[["seconds"]])
#'
#'   return(immersion_per_second)
#' }
#'
#' ################################################################################
#' ################################################################################
#' ################################################################################
#'
#' #' Get Immersion Per Person Data
#' #'
#' #' @param filtered_immersion_data data after filtering our the participants that need to be excluded.
#' #' @param grouped_by grouped_by
#' #'
#' #' @return immersion_person_data
#' #' @export
#' #'
#'
#' get_immersion_person_data <- function(filtered_immersion_data, grouped_by){
#'   # Create the averaged per person immersion scores for all labels
#'   immersion_person_data <- filtered_immersion_data %>%
#'     dplyr::group_by(.data[["sensor_id"]], .data[[{{grouped_by}}]]) %>%
#'     dplyr::summarize(mean_per_person = mean(immersion, na.rm = TRUE),
#'                      sd_per_person = sd(immersion, na.rm = TRUE),
#'                      n_per_person = n() - sum(is.na(immersion)),
#'                      .groups = "drop")
#'
#'   return(immersion_person_data)
#' }
#'
#'
#' #' get_immersion_person_data_2
#' #'
#' #' @param filtered_immersion_data filtered_immersion_data
#' #' @param grouped_by grouped_by
#' #' @param extra_group extra_group
#' #'
#' #' @return immersion_person_data
#' #' @export
#' #'
#' get_immersion_person_data_2 <- function(filtered_immersion_data, grouped_by, extra_group){
#'   # Create the averaged per person immersion scores for all labels
#'   immersion_person_data <- filtered_immersion_data %>%
#'     dplyr::group_by(.data[["sensor_id"]], .data[[{{grouped_by}}]], .data[[{{extra_group}}]]) %>%
#'     dplyr::summarize(mean_per_person = mean(immersion, na.rm = TRUE),
#'                      sd_per_person = sd(immersion, na.rm = TRUE),
#'                      n_per_person = n() - sum(is.na(immersion)),
#'                      .groups = "drop")
#'
#'   return(immersion_person_data)
#' }
#'
#' ################################################################################
#' ################################################################################
#' ################################################################################
#'
#' #' Get Immersion stats for the stimuli
#' #'
#' #' @param filtered_immersion_data data after filtering our the participants that need to be excluded.
#' #' @param grouped_by grouped_by
#' #'
#' #' @return immersion_stimulis_stats
#' #' @export
#' #'
#'
#' get_immersion_stimulis_stats <- function(filtered_immersion_data, grouped_by){
#'   immersion_person_data <- get_immersion_person_data(filtered_immersion_data, grouped_by = {{grouped_by}})
#'   immersion_stimulis_stats <- immersion_person_data %>%
#'     dplyr::group_by(.data[[{{grouped_by}}]]) %>%
#'     summarize(overall_mean = mean(mean_per_person, na.rm = TRUE),
#'               overall_sd = mean(sd_per_person, na.rm = TRUE),
#'               sample_size = n(),
#'               se = overall_sd / sqrt(sample_size),
#'               se_X_2 = se * 2
#'     )
#'
#'   return(immersion_stimulis_stats)
#' }
#'
#' ################################################################################
#' ################################################################################
#' ################################################################################
#'
#' #' Get Immersion per second with stats per stimulus
#' #'
#' #' @param filtered_immersion_data data after filtering our the participants that need to be excluded.
#' #' @param grouped_by grouped_by
#' #'
#' #' @return immersion_per_second_with_stats
#' #' @export
#' #'
#'
#' get_immersion_per_second_with_stats <- function(filtered_immersion_data, grouped_by){
#'   immersion_per_second <- get_immersion_per_second(filtered_immersion_data, grouped_by = {{grouped_by}})
#'   immersion_stimulis_stats <- get_immersion_stimulis_stats(filtered_immersion_data, {{grouped_by}})
#'
#'   immersion_per_second_with_stats <- immersion_per_second %>%
#'     dplyr::left_join(immersion_stimulis_stats, by = {{grouped_by}})
#'
#'   return(immersion_per_second_with_stats)
#' }
#'
#' ################################################################################
#' ################################################################################
#' ################################################################################
#'
#' # Create the function that makes the animation
#' #' make_the_immersion_animation
#' #'
#' #' @param immersion_per_second_with_stats Average Immersion score for each second
#' #' @param the_label Label of the stimulus
#' #' @param video_color What colors should the images be
#' #' @param min_y min_y
#' #' @param max_y max_y
#' #' @param min_x min_x
#' #' @param max_x max_x
#' #' @param raw_data_path raw_data_path
#' #' @param grouped_by grouped_by
#' #'
#' #' @export
#' #'
#' make_the_immersion_animation <- function(
#'     immersion_per_second_with_stats,
#'     the_label, video_color,
#'     min_y = 35, max_y = 75, min_x = 0, max_x = NULL, raw_data_path, grouped_by){
#'
#'   # Provide the plot with a title
#'   plot_title = paste0("Immersion for ", the_label)
#'
#'   max_x <- immersion_per_second_with_stats %>%
#'     dplyr::filter(.data[[{{grouped_by}}]] == the_label) %>%
#'     dplyr::pull(seconds) %>%
#'     max()
#'
#'   # Create the unprocessed animation of the plot
#'   unprocessed_animation <- immersion_per_second_with_stats %>%
#'     # Only include data for a specific video
#'     dplyr::filter(.data[[{{grouped_by}}]] == the_label) %>%
#'     # Create basic structure of the plot
#'     ggplot2::ggplot(ggplot2::aes(y = .data[["score"]], x = .data[["seconds"]])) +
#'     # Add SEX2 error bars
#'     ggplot2::geom_rect(
#'       ggplot2::aes(
#'         xmin = min(.data[["seconds"]])-1,
#'         xmax = max(.data[["seconds"]]),
#'         ymin = .data[["overall_mean"]] - .data[["se_X_2"]],
#'         ymax = .data[["overall_mean"]] + .data[["se_X_2"]]),
#'       fill = "grey", linewidth = 1, alpha = .9) +
#'     # Add Mean
#'     ggplot2::geom_rect(
#'       ggplot2::aes(
#'         xmin = min(.data[["seconds"]]),
#'         xmax = max(.data[["seconds"]]),
#'         ymin = .data[["overall_mean"]] - .01,
#'         ymax = .data[["overall_mean"]] + .01),
#'       color = "black", linewidth = 1) +
#'     # Add lines for the time series data
#'     ggplot2::geom_line(
#'       ggplot2::aes(
#'         x = .data[["seconds"]],
#'         y = .data[["score"]]),
#'       linewidth = 1, color = video_color, alpha = .25) +
#'     # Add points for the time series data
#'     ggplot2::geom_point(
#'       ggplot2::aes(
#'         group = seq_along(seconds)),
#'       size = 3, color = video_color) +
#'     # Clean up x axis
#'     ggplot2::scale_x_continuous(
#'       limits = c(min_x, max_x),
#'       breaks = seq(min_x , max_x, 5),
#'       expand = c(0,0)) +
#'     # Clean up y axis
#'     ggplot2::scale_y_continuous(
#'       limits = c(min_y, max_y),
#'       breaks = seq(min_y, max_y, 5),
#'       expand = c(0,0)) +
#'     # Adjust label
#'     ggplot2::labs(title = plot_title, x = "", y = "") +
#'     # Add general theme
#'     ggthemes::theme_gdocs() +
#'     # Animate the plot
#'     gganimate::transition_reveal(seconds, keep_last = TRUE)
#'   # Set the filename where it should be saved
#'   filename <- paste0(
#'     dirname(raw_data_path),
#'     "/Animations/animation_",
#'     snakecase::to_snake_case(the_label),
#'     ".mp4")
#'   # Process animation
#'   good_animation <- gganimate::animate(
#'     unprocessed_animation,
#'     nframes = max_x,
#'     fps = 1, duration = max_x, width = 2000, height = 500, detail = 5)
#'   # Save animation
#'   gganimate::anim_save(filename, animation = good_animation)
#'   print("Animation has been saved!")
#' }
#'
#' ################################################################################
#' ################################################################################
#' ################################################################################
#'
#' #' Create the immersion animation
#' #'
#' #' @param immersion_per_second_with_stats Immersion per second data with the stimulus stats added
#' #' @param colors_for_videos What colors should be used
#' #' @param raw_data_path  Where is the raw_data directory
#' #' @param grouped_by grouped_by
#' #'
#' #' @export
#' #'
#' create_immersion_animations <- function(immersion_per_second_with_stats, colors_for_videos = NULL, raw_data_path, grouped_by){
#'
#'   grouping <- unique(immersion_per_second_with_stats[[{{grouped_by}}]])
#'
#'   # Make assignments for the video colors
#'   if(is.null(colors_for_videos)) {
#'     video_colors <- head(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), length(grouping)) } else {
#'       video_colors <- colors_for_videos
#'     }
#'
#'   # Create and save animations for all videos
#'   for(i in seq_along(grouping)){
#'     make_the_immersion_animation(
#'       immersion_per_second_with_stats,
#'       the_label = grouping[[i]],
#'       video_color = video_colors[[i]],
#'       raw_data_path=raw_data_path,
#'       min_y = 35, max_y = 65, min_x = 0, max_x = NULL,
#'       grouped_by = "video")
#'   }
#' }
#'
#' #' read_immersion_options
#' #'
#' #' @param give_path path, explorer if null
#' #' @param filter_out_bad filter data?
#' #' @param show_inclusion_summary show_inclusion_summary
#' #'
#' #' @return immersion_data
#' #' @export
#' #'
#'
#' read_immersion_options <- function(give_path = NULL, filter_out_bad = TRUE, show_inclusion_summary = TRUE){
#'   # If no path is given, have them select one
#'   if(is.null(give_path)){
#'     path <- tcltk::tk_choose.dir()
#'   } else {
#'     path <- give_path
#'   }
#'
#'   # Read in the immersion data
#'   immersion_data <- immersiontools::read_immersion_files(raw_data_path)
#'
#'   immersion_individual_summary <- immersiontools::get_immersion_individual_summary( immersion_data )
#'
#'   include_immersion_individual_summary <- immersion_individual_summary %>%
#'     dplyr::filter(immersion_status == "Include data")
#'   include_sensor_id <- include_immersion_individual_summary$sensor_id
#'   include_sensor_id <- include_immersion_individual_summary$sensor_id
#'   include_label <- include_immersion_individual_summary$label
#'   include_percent_missing <- include_immersion_individual_summary$percent_missing
#'   include_given_session_name <- include_immersion_individual_summary$given_session_name
#'   paste0(include_sensor_id, " under ", include_given_session_name, " was shown ", include_label, ", and will be included as the % missing was only ", include_percent_missing, "%.") %>% print()
#'
#'   exclude_immersion_individual_summary <- immersion_individual_summary %>%
#'     dplyr::filter(immersion_status == "Exclude due to too much missing data")
#'   exclude_sensor_id <- exclude_immersion_individual_summary$sensor_id
#'   exclude_sensor_id <- exclude_immersion_individual_summary$sensor_id
#'   exclude_label <- exclude_immersion_individual_summary$label
#'   exclude_percent_missing <- exclude_immersion_individual_summary$percent_missing
#'   exclude_given_session_name <- exclude_immersion_individual_summary$given_session_name
#'   paste0(exclude_sensor_id, " under ", exclude_given_session_name, " was shown ", exclude_label, ", but will be excluded as the % missing was ", exclude_percent_missing, "%.") %>% print()
#'
#'   # If selected, filter out the bad data
#'   if(filter_out_bad){
#'     immersion_data <- immersiontools::get_filtered_immersion_data(immersion_data)
#'   }
#'
#'   return(immersion_data)
#' }
#' ################################################################################
#'
#' #' filter_video_times_with_list
#' #'
#' #' @param df df
#' #' @param list_of_video_names list_of_video_names
#' #' @param list_of_video_seconds list_of_video_seconds
#' #'
#' #' @return filtered_immersion_data
#' #' @export
#' #'
#' filter_video_times_with_list <- function(df, list_of_video_names, list_of_video_seconds) {
#'
#'   filtered_immersion_data <- purrr::map2_df(list_of_video_names, list_of_video_seconds, ~{
#'     df %>% dplyr::filter(video == {{.x}}, seconds <= {{.y}})
#'   })
#'
#'   return(filtered_immersion_data)
#' }
#' ################################################################################
#'
#' #' obtain_individual_immersion_summary_with_rid_video_video_condition_order
#' #'
#' #' @param data data
#' #'
#' #' @return immersion_individual_summary
#' #' @export
#' #'
#' obtain_individual_immersion_summary_with_rid_video_video_condition_order <- function(data) {
#'   amount_of_data_missing <- data %>%
#'     dplyr::group_by(.data[["RIDS"]], .data[["video"]], .data[["experimental_condition"]], .data[["order"]]) %>%
#'     dplyr::summarise(
#'       AVG_Immersion = mean(.data[["immersion"]], na.rm = TRUE),
#'       SD_Immersion = sd(.data[["immersion"]], na.rm = TRUE),
#'       missing_data_counts = sum(is.na(.data[["immersion"]])),
#'       n = n(),
#'       percent_missing = round((.data[["missing_data_counts"]] / .data[["n"]]) * 100, 2),
#'       immersion_status = ifelse(.data[["percent_missing"]] >= 30, "Exclude due to too much missing data", "Include data"),
#'       .groups = "drop")
#'
#'   # Select needed variables
#'   immersion_individual_summary <- amount_of_data_missing %>%
#'     dplyr::select(
#'       RIDS, experimental_condition, video, order, AVG_Immersion, SD_Immersion,
#'       missing_data_counts, n, percent_missing, immersion_status
#'     )
#'
#'   return(immersion_individual_summary)
#' }
#' ################################################################################
#'
#' #' filter_out_anything_with_more_than_30_perc_los
#' #'
#' #' @param df df
#' #' @param immersion_individual_summary immersion_individual_summary
#' #'
#' #' @return filtered_immersion_data
#' #' @export
#' #'
#' filter_out_anything_with_more_than_30_perc_loss <- function(df, immersion_individual_summary) {
#'
#'   missing_info <- immersion_individual_summary %>%
#'     select(RIDS, video, immersion_status)
#'
#'   filtered_immersion_data <- df %>%
#'     left_join(missing_info, by = c("RIDS", "video")) %>%
#'     dplyr::filter(immersion_status != "Exclude due to too much missing data")
#'
#'   return(filtered_immersion_data)
#' }
#' ################################################################################
#'
#'
#' #' calculate_direction_metric
#' #'
#' #' @param individual_immersion individual_immersion
#' #'
#' #' @return direction
#' #' @export
#' #'
#' calculate_direction_metric <- function(individual_immersion) {
#'   # Obtain the vector
#'   correct_immersion <- individual_immersion
#'
#'   # Delay the vector
#'   delayed_immersion <- c(individual_immersion[seq(2, length(individual_immersion))], 0)
#'
#'   # Code the direction
#'   direction <- dplyr::case_when(
#'     correct_immersion < delayed_immersion ~ 1,
#'     correct_immersion == delayed_immersion ~ 0,
#'     correct_immersion > delayed_immersion ~ -1,
#'     is.na(correct_immersion) | is.na(delayed_immersion) ~ NA
#'   )
#'
#'   # Return direction
#'   return(direction)
#' }
#'
#' #' add_direction_per_video_to_df
#' #'
#' #' @param df df
#' #'
#' #' @return df_with_direction_metric
#' #' @export
#' #'
#' add_direction_per_video_to_df <- function(df) {
#'   df_with_direction_metric <- df %>%
#'     dplyr::group_by(RIDS, video) %>%
#'     dplyr::mutate(direction_metric = calculate_direction_metric(immersion))
#'
#'   return(df_with_direction_metric)
#' }
#' ################################################################################
#'
#' #' obtain_average_direction_per_video
#' #'
#' #' @param df df
#' #'
#' #' @return average_video_direction_per_second
#' #' @export
#' #'
#' obtain_average_direction_per_video <- function(df){
#'   average_video_direction_per_second  <- df %>%
#'     dplyr::group_by(seconds, video) %>%
#'     summarize(average_direction_metric = mean(direction_metric, na.rm = TRUE))
#'
#'   return(average_video_direction_per_second)
#' }
#' ################################################################################
#'
#'
#' #' create_workbook_for_immersion_analysis
#' #'
#' #' @param df df
#' #' @param path_to_immersion path_to_immersion
#' #'
#' #' @import openxlsx
#' #' @return wb
#' #' @export
#' #'
#' create_workbook_for_immersion_analysis <- function(df, path_to_immersion) {
#'
#'   headStyle <- analysisHeadStyle()
#'
#'   unique_videos <- unique(df$video)
#'
#'   list_of_video_immersion_dfs <- purrr::map(unique_videos, ~{
#'     df %>%
#'       dplyr::group_by(seconds) %>%
#'       dplyr::filter(video == {{.x}}) %>%
#'       dplyr::select(seconds, given_session_name, immersion) %>%
#'       tidyr::pivot_wider(names_from = given_session_name, values_from = immersion)
#'   })
#'
#'   immersion_per_second_with_stats <- get_immersion_per_second_with_stats(df, grouped_by = "video")
#'
#'   immersion_data <- add_direction_per_video_to_df(df = df)
#'   average_direction_metric_per_video <- obtain_average_direction_per_video(immersion_data)
#'
#'   wb <- openxlsx::createWorkbook()
#'
#'   for(i in seq_along(unique_videos)){
#'
#'     openxlsx::addWorksheet(wb, unique_videos[[i]])
#'
#'     openxlsx::writeData(
#'       wb,
#'       sheet = unique_videos[[i]],
#'       list_of_video_immersion_dfs[[i]],
#'       startCol = 1,
#'       startRow = 1,
#'       headerStyle = headStyle,
#'       borders = "all"
#'     )
#'   }
#'
#'   for(i in unique_videos){
#'
#'     per_second_summary_label <- paste0(i, "_per_second")
#'
#'     video_immersion_per_second_with_stats <- immersion_per_second_with_stats %>%
#'       dplyr::filter(video == i)
#'
#'     openxlsx::addWorksheet(wb, per_second_summary_label)
#'
#'     openxlsx::writeData(
#'       wb,
#'       sheet = per_second_summary_label ,
#'       video_immersion_per_second_with_stats,
#'       startCol = 1,
#'       startRow = 1,
#'       headerStyle = headStyle,
#'       borders = "all"
#'     )
#'   }
#'
#'   for(i in unique_videos){
#'
#'     per_second_direction_label <- paste0(i, "_per_second_direction")
#'
#'     video_direction_per_second_with_stats <- average_direction_metric_per_video %>%
#'       dplyr::filter(video == i)
#'
#'     openxlsx::addWorksheet(wb, per_second_direction_label)
#'
#'     openxlsx::writeData(
#'       wb,
#'       sheet = per_second_direction_label,
#'       video_direction_per_second_with_stats ,
#'       startCol = 1,
#'       startRow = 1,
#'       headerStyle = headStyle,
#'       borders = "all"
#'     )
#'   }
#'
#'   openxlsx::saveWorkbook(wb, paste0(path_to_immersion, "/Analysis_Work.xlsx"), overwrite = TRUE)
#'
#'   return(wb)
#' }
#'
#' ################################################################################
#'
