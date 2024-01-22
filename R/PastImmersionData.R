#
# explore_immersion_stuff <- function() {
#   library(dplyr)
#   library(ggplot2)
#   immersion_data <- readr::read_csv("C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Past Immersion Data/immersion_data.csv")
#   glimpse(immersion_data_2)
#
#   ggplot(data = immersion_data, aes(x = age, y = Average, color= age_group )) +
#     geom_jitter()
#
#   immersion_metrics <- immersion_data %>%
#     select(RID, Video, starts_with("second_")) %>%
#     tidyr::pivot_longer(cols = -c(RID, Video), names_to = "second", values_to = "immersion") %>%
#     group_by(RID, Video) %>%
#     mutate(
#       average_immersion = mean(immersion, na.rm = TRUE),
#       above_average = ifelse(immersion > average_immersion, 1, 0),
#       below_average = ifelse(immersion < average_immersion, 1, 0)
#     ) %>%
#     summarize(
#       average_immersion = mean(immersion, na.rm = TRUE),
#       percent_above_average = (sum(above_average, na.rm = TRUE)/120)*100,
#       percent_below_average = (sum(below_average, na.rm = TRUE)/120)*100
#     )
#
#   immersion_data_2 <- immersion_data %>%
#     left_join(immersion_metrics, by = c("RID", "Video"))
#
#
#   ggplot(data = immersion_data_2, aes(x = percent_above_average, y = average_immersion, color = Video)) +
#     geom_jitter()
#
#
#
#   immersion_data_2 %>%
#     dplyr::filter(Video %in% c("Finn", "Jeff")) %>%
#     group_by(Video) %>%
#     summarize(overall_average_immersion = mean(average_immersion), overall_percent_above_average = mean(percent_above_average))
#
#   immersion_data_2 %>%
#     dplyr::filter(Video %in% c("Finn", "Jeff")) %>%
#     ggplot(aes(x = percent_above_average, y = average_immersion, color = Video)) +
#     geom_jitter()
#
#
#
#
#   data <- immersion_data %>%
#     dplyr::filter(Video %in% c("Finn", "Jeff")) %>%
#     select(RID, Video, starts_with("second_")) %>%
#     tidyr::pivot_longer(cols = -c(RID, Video), names_to = "second", values_to = "immersion") %>%
#     mutate(second = as.numeric(gsub(".*?([0-9]+).*", "\\1", second))) %>%
#     group_by(second, Video) %>%
#     mutate(
#       average_immersion = mean(immersion, na.rm = TRUE),
#       above_average = ifelse(immersion > average_immersion, 1, 0),
#       below_average = ifelse(immersion < average_immersion, 1, 0)
#     ) %>%
#     summarize(
#       average_immersion_per_second = mean(immersion, na.rm = TRUE),
#       n = n(),
#       percent_above_average_per_second = (sum(above_average, na.rm = TRUE)/n)*100,
#       percent_below_average_per_second = (sum(below_average, na.rm = TRUE)/n)*100,
#       .groups = "drop")
#
#   data %>%
#     ggplot(aes(x = second, y = percent_above_average_per_second, color = Video)) +
#     geom_line() +
#     scale_x_continuous(breaks = seq(0, 120, 5)) +
#     theme_bw()
#
#   data %>%
#     ggplot(aes(x = second, y = average_immersion_per_second, color = Video)) +
#     geom_line() +
#     scale_x_continuous(breaks = seq(0, 120, 5)) +
#     theme_bw()
#
#
#   ####################################
#   ####################################
#
#   MNFLAB_IMMERSION_DATA_CONCERTED <- readxl::read_excel("C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/Past Immersion Data/MNFLAB_Immersion_Data_Converted.xlsx")
#   library(dplyr)
#   CVH_210505 <- MNFLAB_IMMERSION_DATA_CONCERTED %>%
#     dplyr::filter(project_number == 210505)
#
#   CVH_210505_WIDE <- CVH_210505 %>%
#     tidyr::pivot_wider(id_cols = -c(project_number, video, rid), names_from = unique_identifier, values_from = Immersion_Index)
#
#   readr::write_csv(CVH_210505_WIDE, "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/Past Immersion Data/CVH_210505_Converted_wide.csv")
#
#
#
#
# }
#
#
#
#
# immersion_video_effect_sizes <- function() {
#   folder_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/Final Data Base Sheets"
#
#   Combined_aggregated_immersion_dataset <- readr::read_csv(paste0(folder_path, "/Combined_aggregated_immersion_dataset.csv"))
#   Combined_session_info <- readr::read_csv(paste0(folder_path, "/Combined_session_info.csv"))
#   Combined_stimuli_info <- readr::read_csv(paste0(folder_path, "/Combined_stimuli_info.csv"))
#
#
#   session_information <- Combined_session_info %>%
#     dplyr::left_join(Combined_stimuli_info, by = "stimuli_info_identifier")
#
#   individual_immersion_data <- Combined_aggregated_immersion_dataset %>%
#     dplyr::left_join(session_information, by = "immersion_identifier") %>%
#     dplyr::select(immersion_identifier, stimulus_name, immersion_participant_mean, immersion_participant_sd)
#
#   video_immersion_df <- individual_immersion_data %>%
#     dplyr::group_by(stimulus_name) %>%
#     dplyr::summarize(
#       video_average = mean(immersion_participant_mean, na.rm = TRUE),
#       video_sd = sd(immersion_participant_mean, na.rm = TRUE),
#       video_n = n()
#     )
#
#   video_1 <- video_immersion_df[1,]
#   video_2 <- video_immersion_df[2,]
#
#   calculate_cohens_d <- function(video_1, video_2){
#     numerator <- video_1$video_average - video_2$video_average
#     denominator <- sqrt(((video_1$video_sd ^ 2) + (video_2$video_sd ^ 2)) / 2)
#     cohens_d <- numerator / denominator
#     return(cohens_d)
#   }
#
#   video_position_seq <- seq(1, nrow(video_immersion_df))
#
#   combinations_of_video_position_seq <- tidyr::crossing(video_position_seq, video_position_seq)
#   colnames(combinations_of_video_position_seq) <- c("video_1_id", "video_2_id")
#
#   cohens_ds <-purrr::map2_dbl(combinations_of_video_position_seq$video_1_id, combinations_of_video_position_seq$video_2_id, ~ {
#     calculate_cohens_d(video_immersion_df[.x, ], video_immersion_df[.y, ])
#   }
#   )
#
#   mean(abs(cohens_ds))
#   calculate_cohens_d(video_1, video_2)
#
#   library(pwr)
#   pwr.t.test(d = .5, sig.level = .05, n = 35)
#   pwr.t.test(d = .8, sig.level = .05, n = 35)
#
#   pwr.t.test(d = .3651, sig.level = .05, n = 35)
#
#   pwr.t.test(d = .3651, sig.level = .05, power = .8)
#   pwr.t.test(d = .3651, sig.level = .05, power = .9)
#   pwr.t.test(d = .3651, sig.level = .05, power = .95)
#
# }
#
#
#
#
# reprocess_all_immersion <- function() {
# #   read_immersion_csv_files_new <- function(paths, session_or_rid = "rid"){
# #     # Set the column names that will be used in the data frame
# #     immersion_col_names <- c("session", "seconds", "label", "sensor_id", "immersion")
# #     # Initiate data frame with supplied labels
# #     immersion_dataframe <- bktools::initiate_dataframe_with_col_names(names_for_columns = immersion_col_names)
# #     for(i in seq_along(paths)){
# #       # # print(i)
# #       # # Read in the file
# #       # individual_immersion_data <- suppressMessages(readxl::read_excel(paths[[i]], guess_max = 10000))
# #       # # Put ther column names in lower case
# #       # colnames(individual_immersion_data) <- stringr::str_to_lower(colnames(individual_immersion_data))
# #       # # Clean a few labels
# #       # colnames(individual_immersion_data) <- dplyr::case_when(
# #       #   colnames(individual_immersion_data) == "date" ~ "time",
# #       #   colnames(individual_immersion_data) == "datetime" ~ "time",
# #       #   colnames(individual_immersion_data) == "...1" ~ "time",
# #       #   colnames(individual_immersion_data) == "notes" ~ "label",
# #       #   stringr::str_detect(colnames(individual_immersion_data), "p[a-z0-9]{7,9}") ~ paste0("immersion_", colnames(individual_immersion_data)),
# #       #   TRUE ~ colnames(individual_immersion_data)
# #       # )
# #       # individual_immersion_data$label <- stringr::str_to_lower(individual_immersion_data$label)
# #       #
# #       if(session_or_rid == "session"){
# #         group_rid_or_session <- "session"
# #         # Assign the session infor to session by taking it from the path name
# #         individual_immersion_data$session <- bktools::extract_rid_from_path(path = paths[[i]], pattern = "^[0-9]{8}_[a-zA-Z]{3}[0-9]{1}")
# #       } else if (session_or_rid == "rid"){
# #         group_rid_or_session <- "rid"
# #         individual_immersion_data$rid <- bktools::extract_rid_from_path(path = paths[[i]], pattern = "^[0-9a-zA-Z]{5}")
# #       }
# #
# #       # fill in the notes
# #       individual_immersion_data$label <- bktools::na_fill(individual_immersion_data$label)
# #       # Replace NAs with "NA"
# #       individual_immersion_data$label <- tidyr::replace_na(individual_immersion_data$label, "NA")
# #       # Select the variables to include in the dataframe
# #       individual_immersion_data <- individual_immersion_data %>%
# #         dplyr::filter(label != "NA", label != "end") %>%
# #         dplyr::group_by(.data[[group_rid_or_session]], .data[["label"]]) %>%
# #         dplyr::mutate(seconds = row_number(.data[["time"]])) %>%
# #         dplyr::ungroup() %>%
# #         dplyr::select(.data[[group_rid_or_session]], .data[["seconds"]], label = .data[["label"]], dplyr::starts_with("immersion_")) %>%
# #         tidyr::pivot_longer(cols = !c(group_rid_or_session, "seconds", "label"), names_to = "sensor_id", values_to = "immersion")
# #       # Clean the labels for the sensor id data
# #       individual_immersion_data$sensor_id <- stringr::str_replace(individual_immersion_data$sensor_id, "immersion_", "")
# #       # Order the data by the participant, the mark, and the time
# #       individual_immersion_data <- individual_immersion_data %>%
# #         dplyr::arrange(sensor_id, label, seconds)
# #       # Replace values that are repeated more than 6 times with NA
# #       individual_immersion_data$immersion <- bktools::replace_repeats_over_n_with_na(individual_immersion_data$immersion, n = 6)
# #
# #       # Add data to dataframe
# #       immersion_dataframe <- rbind(immersion_dataframe, individual_immersion_data)
# #     }
# #     return(immersion_dataframe)
# #   }
# #
# #
# #   `%notin%` <- purrr::negate(`%in%`)
# #
# #   path_raw <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/00 Jobs/2023/220159_TNC_BC__Immersion Only_OID962/Analysis/Immersion/Raw Data/"
# #   file_names <- list.files(path_raw)
# #   path_for_files <- paste0(path_raw, file_names)
# #
# #   df_raw_file_info <- purrr::map_df(path_for_files, ~{
# #     file_name <- basename(.x)
# #     file_name_cleaned <- file_name %>%
# #       stringr::str_replace(" \\([0-9]{1,2}\\).xlsx", ".xlsx")
# #     pattern_base_of_filenames <- "[a-zA-Z0-9]{5}_UTC_[0-9]{2}_[0-9]{2}_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}.xlsx"
# #     base_of_filenames <- file_name_cleaned %>% stringr::str_extract(pattern_base_of_filenames)
# #     file_name_typed_into_immersion <- stringr::str_replace(file_name_cleaned, paste0("_", base_of_filenames), "")
# #     immersion_experience_id <- stringr::str_extract(base_of_filenames, "^[a-zA-Z0-9]{5}")
# #     date_portion_of_file_name <- stringr::str_replace(base_of_filenames, "^[a-zA-Z0-9]{5}_UTC_", "")
# #     date_from_file_name <- date_portion_of_file_name %>%
# #       stringr::str_extract("^[0-9]{2}_[0-9]{2}_[0-9]{4}") %>%
# #       stringr::str_replace_all("_", "/") %>%
# #       lubridate::mdy()
# #     time_from_file_name <- date_portion_of_file_name %>%
# #       stringr::str_replace("^[0-9]{2}_[0-9]{2}_[0-9]{4}_", "") %>%
# #       stringr::str_replace(".xlsx$", "") %>%
# #       stringr::str_replace_all("_", ":") %>%
# #       lubridate::hms()
# #     df_raw_file_info <- tibble::tibble(
# #       file_name,
# #       file_name_typed_into_immersion,
# #       immersion_experience_id,
# #       date_from_file_name,
# #       time_from_file_name)
# #     df_raw_file_info
# #   })
# #
# #   df_raw_file_info$numbers_from_file_name <- stringr::str_extract_all(df_raw_file_info$file_name, "[0-9]") %>%
# #     purrr::map_dbl(~{as.numeric(paste0(.x, collapse = ""))})
# #
# #   df_raw_file_info <- df_raw_file_info %>%
# #     group_by(file_name_typed_into_immersion) %>%
# #     slice_max(numbers_from_file_name, n = 1)
# #
# #   path_for_files_to_run <- paste0(path_raw, "/", df_raw_file_info[["file_name"]])
# #
# #   immersion_df <- purrr::pmap_df(
# #     list(path_for_files_to_run,
# #          df_raw_file_info$file_name_typed_into_immersion,
# #          df_raw_file_info$immersion_experience_id,
# #          df_raw_file_info$date_from_file_name,
# #          df_raw_file_info$time_from_file_name), ~{
# #
# #            immersion_data <- suppressMessages(readxl::read_excel(..1, sheet = "Immersion", guess_max = 10000))
# #            immersion_data$file_name_typed_into_immersion <- ..2
# #            immersion_data$experience_id <- ..3
# #            immersion_data$date_from_file_name <- ..4
# #            immersion_data$time_from_file_name <- ..5
# #
# #            colnames(immersion_data) <- stringr::str_to_lower(colnames(immersion_data))
# #
# #            colnames(immersion_data) <- dplyr::case_when(
# #              colnames(immersion_data) == "date" ~ "time",
# #              colnames(immersion_data) == "datetime" ~ "time",
# #              colnames(immersion_data) == "...1" ~ "time",
# #              colnames(immersion_data) == "notes" ~ "label",
# #              stringr::str_detect(colnames(immersion_data), "p[a-z0-9]{7,9}") ~ paste0("immersion_", colnames(immersion_data)),
# #              TRUE ~ colnames(immersion_data))
# #
# #            immersion_data$label <- immersion_data$label %>%
# #              stringr::str_to_lower() %>%
# #              bktools::na_fill() %>%
# #              tidyr::replace_na("NA")
# #
# #            # Select the variables to include in the dataframe
# #            immersion_data <- immersion_data %>%
# #              dplyr::filter(label != "NA", label != "end") %>%
# #              dplyr::group_by(.data[["label"]]) %>%
# #              dplyr::mutate(seconds = row_number(.data[["time"]])) %>%
# #              dplyr::ungroup() %>%
# #              dplyr::select(.data[["file_name_typed_into_immersion"]], .data[["immersion_experience_id"]], .data[["date_from_file_name"]], .data[["time_from_file_name"]], .data[["time"]], .data[["seconds"]], label = .data[["label"]], dplyr::starts_with("immersion_")) %>%
# #              tidyr::pivot_longer(cols = !c("file_name_typed_into_immersion", "immersion_experience_id", "date_from_file_name", "time_from_file_name", "time", "seconds", "label"), names_to = "sensor_id", values_to = "immersion")
# #            # Clean the labels for the sensor id data
# #            immersion_data$sensor_id <- stringr::str_replace(immersion_data$sensor_id, "immersion_", "")
# #            # Order the data by the participant, the mark, and the time
# #            immersion_data <- immersion_data %>%
# #              dplyr::arrange(sensor_id, label, seconds)
# #            # Replace values that are repeated more than 6 times with NA
# #            immersion_data <- immersion_data %>%
# #              group_by(sensor_id, label) %>%
# #              mutate(immersion = bktools::replace_repeats_over_n_with_na(immersion, n = 6)) %>%
# #              ungroup()
# #
# #            immersion_data
# #          })
# #
# #   #
# #   #
# #   # immersion_df <- immersion_df %>%
# #   #   group_by(file_name_typed_into_immersion, immersion_experience_id, date_from_file_name, time_from_file_name, sensor_id, label) %>%
# #   #   mutate(dt_start = min(time), dt_end = max(time)) %>%
# #   #   ungroup() %>%
# #   #   select(sensor_id, file_name_typed_into_immersion, immersion_experience_id, dt_start, dt_end, seconds, immersion)
# #   #
# #   #
# #   #
# #
# #
# #
# #   safety_data <- readxl::read_excel(.x, sheet= "Safety", skip = 1) %>%
# #     dplyr::filter(identifier != "DateTime")
# #   safety_data$identifier <- openxlsx::convertToDateTime(safety_data$identifier)
# #   safety_data <- safety_data %>%
# #     tidyr::pivot_longer(
# #       cols = -identifier,
# #       names_to = "sensor_id",
# #       values_to = "psychological_safety")
# # })
# }
