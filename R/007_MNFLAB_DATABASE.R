#'
#'
#' #' create_pid_list
#' #'
#'
#' #' @export
#' #'
#' create_pid_list <- function() {
#'   paste0("pid", stringr::str_pad(seq(1,1000,1), 10, side = "left", pad = 0))
#' }
#'
#' #' create_iid_list
#' #'
#'
#' #' @export
#' #'
#' create_iid_list <- function() {
#'   paste0("iid", stringr::str_pad(seq(1,1000,1), 10, side = "left", pad = 0))
#' }
#'
#' #' create_riid_list
#' #'
#' #' @export
#' #'
#' create_riid_list <- function() {
#'   paste0("riid", stringr::str_pad(seq(1,1000,1), 10, side = "left", pad = 0))
#' }
#'
#' #' create_sid_list
#' #'
#' #' @export
#' #'
#' create_sid_list <- function() {
#'   paste0("sid", stringr::str_pad(seq(1,1000,1), 10, side = "left", pad = 0))
#' }
#'
#'
#' #' explorecode
#' #'
#' #' @export
#' #'
#' explorecode <- function() {
#'   immersion_dir_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/ALL_DATASETS/Immersion_Data"
#'   immersion_files <- list.files(immersion_dir_path)
#'   immersion_file_paths <- paste0(immersion_dir_path, "/", immersion_files)
#'   time_point_names <- c(paste0("immersion_tp_", stringr::str_pad(1:120, width = 4, side = "left", pad = 0)), "immersion_participant_mean", "immersion_participant_sd", "immersion_participant_observations")
#'
#'   immersion_data <- purrr::map_df(immersion_file_paths, ~{
#'     df_data <- readr::read_csv(.x, show_col_types = FALSE)
#'     df_data %>% dplyr::select(unique_identifier, video_name, tidyselect::all_of(time_point_names))
#'   })
#'
#'   part_dir_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/ALL_DATASETS/Participant_Data"
#'   part_files <- list.files(part_dir_path)
#'   part_file_paths <- paste0(part_dir_path, "/", part_files)
#'   part_data <- purrr::map_df(part_file_paths, ~{
#'     print(.x)
#'     df_data <- readr::read_csv(.x, show_col_types = FALSE)
#'     df_data$age <- as.numeric(df_data$age)
#'     df_data %>% dplyr::select(unique_identifier, age)
#'   })
#'
#'   immersion_data <- immersion_data %>%
#'     group_by(video_name) %>%
#'     mutate(
#'       median = median(immersion_participant_mean),
#'       high_or_low = ifelse(
#'         immersion_participant_mean >= median,
#'         "High",
#'         ifelse(
#'           immersion_participant_mean < median,
#'           "Low",
#'           "Error"
#'         )
#'       )
#'     )
#'
#'
#'   video_info <- readr::read_csv("C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/Exploratory_Dataset_Brian.csv", show_col_types = FALSE)
#'
#'   immersion_data <- immersion_data %>%
#'     left_join(video_info, "video_name") %>%
#'     left_join(part_data, "unique_identifier")
#'
#'
#'   library(ggplot2)
#'
#'   immersion_data %>%
#'     ggplot(aes(x = age, y = immersion_participant_mean, color = high_or_low)) +
#'     geom_point()
#'
#'   cor(immersion_data$age, immersion_data$immersion_participant_mean, use = "complete.obs")
#'
#' }
#'
#'
#' #' reshape_for_database
#' #'
#' #' @param file_path file_path
#' #' @param cols_keep cols_keep
#' #' @param names_to names_to
#' #' @param values_to values_to
#' #'
#' #' @return reshaped_df
#' #' @export
#' #'
#' reshape_for_database <- function(file_path = file.choose(), cols_keep = c(), names_to, values_to){
#'   # Select the file in the explorer window
#'   path <- file_path
#'   # What is the file name?
#'   file_name <- basename(path)
#'   dir_path <- dirname(path)
#'   # What should the new file name be?
#'   file_name_for_reshaped_df <- paste0(stringr::str_replace(file_name, ".csv", ""), "_LONG.csv")
#'   file_path_for_reshaped_df <- paste0(dir_path, "/", file_name_for_reshaped_df)
#'   # print(file_path_for_reshaped_df)
#'   # Read in the data, and then change the format to character strings
#'   data <- readr::read_csv(path, show_col_types = FALSE)
#'   # Reshape the data set
#'   reshaped_df <- data %>%
#'     tidyr::pivot_longer(
#'       cols = -all_of(cols_keep),
#'       names_to = {{names_to}},
#'       values_to = {{values_to}})
#'   # Write the reshaped
#'   readr::write_csv(reshaped_df, file_path_for_reshaped_df)
#'   # Return the dataframe
#'   return(reshaped_df)
#' }
#'
#'
#'
#'
#' #' read_all_files_as_list
#' #'
#' #' @param folder_to_check folder_to_check
#' #'
#' #' @return list_database_sheets
#' #' @export
#' #'
#' read_all_files_as_list <- function(folder_to_check = "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/ready_for_database/000000_SHC") {
#'   file_names <- list.files(folder_to_check, pattern = ".csv")
#'   file_paths <- paste0(folder_to_check, "/", file_names)
#'   list_database_sheets <- map(file_paths, ~read_csv(.x, show_col_types = FALSE))
#'   list_database_sheets <- list_database_sheets %>%
#'     set_names(file_names)
#'   return(list_database_sheets)
#' }
#'
#' #' check_for_issues_in_session_data_and_participant_info
#' #'
#' #' @param folder_to_check folder_to_check
#' #'
#' #' @export
#' #'
#' check_for_issues_in_session_data_and_participant_info <- function(folder_to_check){
#'   database_sheets_list <- read_all_files_as_list(folder_to_check)
#'   session_info_index <- which(stringr::str_detect(names(database_sheets_list), "session_info\\.csv"))
#'
#'   if(identical(session_info_index, integer(0))){
#'     stop("A file with the session_info.csv pattern does not exist.")
#'   }
#'
#'   session_info <- database_sheets_list[[session_info_index]]
#'
#'   if(any(stringr::str_detect(names(database_sheets_list), "identifiable_participant_info\\.csv"))) {
#'     stop("Please change the name of the file 'identifiable_participant_info.csv' to 'participant_info_identifiable.csv")
#'   }
#'
#'   participant_info_index <- which(stringr::str_detect(names(database_sheets_list), "participant_info\\.csv"))
#'
#'   if(identical(participant_info_index, integer(0))){
#'     stop("A file with the participant_info.csv pattern does not exist.")
#'   }
#'   participant_info <- tryCatch(database_sheets_list[[participant_info_index]])
#'   # participant_info$error
#'   # session_data_and_participant_info <- database_sheets_list[[session_info_index]] %>%
#'   #   full_join(
#'   #     database_sheets_list[[participant_info_index]],
#'   #     by = "participant_identifier"
#'   #   )
#'   print("Hi")
#' }
#'
#'
#'
#'
#' #' process_and_organize_participant_data
#' #'
#' #' @export
#' #'
#' process_and_organize_participant_data <- function() {
#'   library(dplyr)
#'   dir_folder <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/ALL_DATASETS/Participant_Data"
#'   file_name_ext <- "/211030_AHA_PARTICIPANT_DATASET.csv"
#'   col_file_name_ext <- "/211030_AHA_PARTICIPANT_columns.csv"
#'   participant_path <- paste0(dir_folder, file_name_ext)
#'   participant_data <- readr::read_csv(participant_path, show_col_types = FALSE)
#'   participant_colnames_path <- paste0(dir_folder, col_file_name_ext)
#'   participant_colnames <- readr::read_csv(participant_colnames_path, show_col_types = FALSE)
#'
#'   colnames(participant_data) == participant_colnames$new_name
#'
#'   columns_for_participant_info <- dplyr::filter(participant_colnames, participant_info) %>% pull(new_name)
#'   columns_for_participant_info <- c(
#'     "participant_identifier",
#'     "age",
#'     "gender",
#'     "race_ethnicity",
#'     "city_or_locality",
#'     "state_or_region",
#'     "country",
#'     "participant_timezone",
#'     "occupation",
#'     "industry",
#'     "small_business_owner",
#'     "company_size",
#'     "employment_status",
#'     "seniority",
#'     "type_of_income",
#'     "personal_income",
#'     "household_income",
#'     "level_of_education",
#'     "marital_status",
#'     "living_situation",
#'     "home_owner",
#'     "children",
#'     "browsers",
#'     "computer_with_a_webcam",
#'     "computer_operating_system",
#'     "smartphone_manufacturer",
#'     "smartphone_operating_system",
#'     "tablet_operating_system"
#'   )
#'
#'   columns_for_identifiable_participant_info <- dplyr::filter(participant_colnames, identifiable_participant_info) %>% pull(new_name)
#'   columns_for_identifiable_participant_info <- c(
#'     "participant_identifier",
#'     "first_name",
#'     "last_name",
#'     "email",
#'     "phone_number"
#'   )
#'
#'   columns_for_participant_info_other <- dplyr::filter(participant_colnames, participant_info_other) %>% pull(new_name)
#'   # columns_for_participant_info_other <- c(
#'   #   "participant_identifier",
#'   #   "internet_connection",
#'   #   "have_device_and_phone",
#'   #   "have_watch",
#'   #   "donated_within_12_months",
#'   #   "gave_to_these_organizations",
#'   #   "donation_frequency",
#'   #   "follow_directions",
#'   #   "download_app",
#'   #   "dup_state_of_residence",
#'   #   "heart_condition"
#'   # )
#'
#'   columns_for_participant_info %in% colnames(participant_data)
#'
#'
#'   participant_info <- participant_data %>% select(all_of(columns_for_participant_info))
#'   clipr::write_clip(participant_info)
#'
#'   identifiable_participant_info <- participant_data %>% select(all_of(columns_for_identifiable_participant_info))
#'   clipr::write_clip(identifiable_participant_info)
#'
#'   participant_info_other <- participant_data %>% select(all_of(columns_for_participant_info_other))
#'   clipr::write_clip(participant_info_other)
#'
#'   project_info_parts_from_part_data <- c(
#'     "rid",
#'     "project_number",
#'     "main_participant_identifier",
#'     "date_and_time_of_session"
#'   )
#'
#'   participant_data %>% select(all_of(project_info_parts_from_part_data)) %>% clipr::write_clip()
#'
#' }
