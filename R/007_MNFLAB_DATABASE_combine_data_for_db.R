#' #' combine_project_data_for_db
#' #'
#' #' @param paths_for_projects_ready_for_database paths_for_projects_ready_for_database
#' #' @param sheet_name sheet_name
#' #' @param column_types column_types
#' #'
#' #' @return data_frame
#' #' @export
#' #'
#' combine_project_data_for_db <- function(paths_for_projects_ready_for_database, sheet_name, column_types){
#'   purrr::map_df(paths_for_projects_ready_for_database, ~{
#'     # Obtain file number
#'     file_number <- list.files(.x) %>% basename() %>% stringr::str_extract("[0-9]{6}_[A-Z]{3}")
#'     # Prepare path
#'     data_paths <- paste0(.x, "/", unique(file_number), "_", sheet_name, ".csv")
#'     # Read data
#'     data_frame <- readr::read_csv(data_paths, col_types = column_types)
#'     # Return data frame
#'     return(data_frame)
#'   }
#'   )
#' }
#'
#'
#' MNFLAB_DATABASE_combine_data_for_db <- function() {
#'
#'
#'   projects_to_include_numbers <- c("220103_CVH", "220158_ABS", "000000_SHC", "220159_TNC", "220227_NKH", "220329_ACL", "220501_MAW", "220589_HDC", "220422_AHA", "210511_AHA", "211030_AHA")
#'
#'   ready_for_database_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/ready_for_database"
#'   video_names_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/ready_for_database/VIDEO_NAMES/video_names.csv"
#'   video_names_df <- readr::read_csv(video_names_path, show_col_types = FALSE)
#'
#'   paths_for_projects_ready_for_database <- paste0(ready_for_database_path, "/", projects_to_include_numbers)
#'
#'   aggregated_immersion_dataset <- combine_project_data_for_db(paths_for_projects_ready_for_database, sheet_name = "aggregated_immersion_dataset", column_types = "cccddd")
#'   identifiable_participant_info <- combine_project_data_for_db(paths_for_projects_ready_for_database, "identifiable_participant_info", "ccccc")
#'   immersion_dataset <- combine_project_data_for_db(paths_for_projects_ready_for_database, "immersion_dataset", paste0("ccc", rep("d", 120)))
#'   participant_info <- combine_project_data_for_db(paths_for_projects_ready_for_database, "participant_info", paste0("ci", rep("c", 26)))
#'   participant_info_other <- combine_project_data_for_db(paths_for_projects_ready_for_database, "participant_info_other_LONG", "ccc")
#'   project_info <- combine_project_data_for_db(paths_for_projects_ready_for_database, "project_info", "dcccccccc")
#'   ri_dataset <- combine_project_data_for_db(paths_for_projects_ready_for_database, "ri_dataset_LONG", "ccc")
#'   ri_question_bank <- combine_project_data_for_db(paths_for_projects_ready_for_database, "ri_question_bank", "dccc")
#'   session_info <- combine_project_data_for_db(paths_for_projects_ready_for_database, "session_info", "cccccccccc") %>%
#'     left_join(video_names_df, by = c("video_name" = "video_from_db"))
#'
#'   session_info %>% dplyr::pull(video_name) %>%
#'     unique() %>%
#'     clipr::write_clip()
#'
#'   participant_info %>% dplyr::glimpse()
#'
#'   participant_info <- participant_info %>%
#'     mutate(
#'       age_group = case_when(
#'         age < 30 ~ "18-29",
#'         age >= 30 & age <= 39 ~ "30-39",
#'         age >= 40 & age <= 49 ~ "40-49",
#'         age >= 50 & age <= 59 ~ "50-59",
#'         age >= 60 & age <= 69 ~ "60-69",
#'         age >= 70 ~ "70+"
#'       ),
#'       gender = case_when(
#'         gender == "Woman" ~ "Female",
#'         gender == "Man" ~ "Male",
#'         gender == "#N/A" ~ "NA",
#'         is.na(gender) ~ "NA"
#'       ),
#'       ethnicity = case_when(
#'         race_ethnicity == "#N/A" ~ "Other",
#'         is.na(race_ethnicity) ~ "Other",
#'         race_ethnicity == "African American" ~ "African American",
#'         race_ethnicity == "Black or African American" ~ "African American",
#'         race_ethnicity == "American Indian or Alaska Native" ~ "Other",
#'         race_ethnicity == "ashkenazi jewish" ~ "Other",
#'         race_ethnicity == "Asian" ~ "Asian",
#'         race_ethnicity == "bi-racial" ~ "Other",
#'         race_ethnicity == "caucasian/hispanic" ~ "Other",
#'         race_ethnicity == "Hispanic" ~ "Hispanic",
#'         race_ethnicity == "Hispanic or Latino" ~ "Hispanic",
#'         race_ethnicity == "latinx" ~ "Hispanic",
#'         race_ethnicity == "mixed ethnicity" ~ "Other",
#'         race_ethnicity == "mixed race, asian and white" ~ "Other",
#'         race_ethnicity == "mixed two or more" ~ "Other",
#'         race_ethnicity == "native american and white Native Hawaiian or Pacific Islander" ~ "Other",
#'         race_ethnicity == "new york" ~ "Other",
#'         race_ethnicity == "Other" ~ "Other",
#'         race_ethnicity == "prefer not to answer" ~ "Other",
#'         race_ethnicity == "White" ~ "White",
#'         race_ethnicity == "white and hispanic" ~ "Other",
#'         race_ethnicity == "white/asian " ~ "white/asian",
#'       )
#'     )
#'
#'   session_data <- session_info %>%
#'     select(-c(rid, sensor_id, participant_rating_user, user_project_name, start_date, end_date, researcher, project_number))
#'
#'   immersion_second_data <- immersion_dataset %>% select(-c(immersion_rid, immersion_sensor_id))
#'
#'   time_series_immersion <- session_data %>%
#'     left_join(immersion_second_data, by = "immersion_identifier") %>%
#'     left_join(participant_info, by = "participant_identifier") %>%
#'     select(-c(participant_identifier, immersion_identifier, ri_identifier)) %>%
#'     tidyr::pivot_longer(cols = starts_with("immersion_tp_"), names_to = "time_point", values_to = "immersion")
#'
#'   time_series_immersion$time_point <-stringr::str_replace(time_series_immersion$time_point, "immersion_tp_", "") %>% as.numeric()
#'
#'   time_series_immersion_data_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/EDA_Shiny_App/data/time_series_immersion_data.csv"
#'
#'   readr::write_csv(time_series_immersion, time_series_immersion_data_path)
#'
#'   session_data <- session_data %>% dplyr::select(session_identifier, participant_identifier, immersion_identifier, ri_identifier, video_name = correct_video_name)
#'   sjlabelled::set_label(session_data) <- c(
#'     "Session Identifier: Unique Identifier that signifies a unique session that occured in the lab.",
#'     "Participant Identifier: Identifier for demographic data related to the participant who took part in the study.",
#'     "Immersion Identifier: Identifier for the session's immersion data.",
#'     "RI Identifier: Identifier for the RI or survey data collected during the study.",
#'     "Video Name: What video was the participant shown?"
#'   )
#'   typeof(session_data$session_identifier)
#'   session_codebook <- tibble::enframe(sjlabelled::get_label(session_data))
#'   session_codebook$datatype <- c(
#'     "character", "character", "character", "character", "character"
#'   )
#'
#'   bktools::reshape_for_database(unique_identifier = "participant_identifier")
#'   bktools::reshape_for_database(unique_identifier = "ri_identifier")
#'
#'   identifiable_participant_info %>%
#'     dplyr::filter(duplicated(email)) %>%
#'     arrange(first_name) %>%
#'     View()
#' }
#'
#'
#'
#'
#'
#'
#' #
#' # converted_data_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/Past Immersion Data/Data to convert/MNFLAB_Immersion_Data_Converted.csv"
#' # converted_data <- readr::read_csv(converted_data_path, show_col_types = FALSE) %>%
#' #   dplyr::filter(!is.na(unique_identifier))
#' #
#' # converted_data %>%
#' #   tidyr::pivot_wider(names_from = "second", values_from = "Immersion_Index") %>%
#' #   clipr::write_clip()
