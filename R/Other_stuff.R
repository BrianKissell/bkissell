# tcm_video_Web_Scraper <- function() {
#   library(rvest)
#   library(dplyr)
#
#   scraplinks <- function(url){
#     # Create an html document from the url
#     webpage <- xml2::read_html(url)
#     # Extract the URLs
#     url_ <- webpage %>%
#       rvest::html_nodes("a") %>%
#       rvest::html_attr("href")
#     # Extract the link text
#     link_ <- webpage %>%
#       rvest::html_nodes("a") %>%
#       rvest::html_text()
#     return(tibble(link = link_, url = url_))
#   }
#
#   first_page <- scraplinks("http://targetedcontentmarketing.com/projects/")
#
#
#   pages <- first_page %>% filter(link %in% as.character(1:17)) %>% pull(url)
#
#   data <- data.frame(link = c(), url = c())
#
#   for(i in pages){
#     data <- rbind(data, scraplinks(i))
#   }
#
#   data <- data %>%
#     filter(stringr::str_detect(data$url, "portfolio"))
#
#   data$link <- stringr::str_replace(data$link, "DRTV", "") %>% stringr::str_replace(" / Featured / Video", "") %>% stringr::str_replace(" / Video", "")
#
#   video_links <- c()
#   name <- c()
#   url <- c()
#   for(i in seq(1, length(data$url))){
#     x <- xml2::read_html(data$url[[i]]) %>%
#       rvest::html_nodes(".wistia_embed") %>%
#       rvest::html_attr("src")
#
#     if(length(x) == 0){
#       x <- "NA"
#     }
#
#     video_links <- c(video_links, x)
#     print(x)
#     name <- c(name, data$link[[i]])
#     print(data$link[[i]])
#     url <- c(url, data$url[[i]])
#     print(data$url[[i]])
#   }
#
#   tcm_video_links <- data.frame(name = name, url = url, video_links = video_links)
#   tcm_video_links$unique_identifier <- as.character(unique_identifier[[1]])
#
#   tcm_video_links$text_for_formr <- paste0(
#     "You have been selected to watch [",
#     tcm_video_links$name,
#     "](",
#     tcm_video_links$video_links,
#     "). Please use '",
#     tcm_video_links$unique_identifier,
#     "_' followed by your initials in the unique identifier section."
#   )
#
#   tcm_video_links$name_for_formr <- paste0("video_", 1:146)
#   tcm_video_links$condition_for_formr <- paste0("random_for_video == ", 1:146)
#
#
#   readr::write_csv(tcm_video_links, "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Content Evaluation Measure/TCM Videos/tcm_video_links.csv")
#
# }







################ Story Corps ###################


#
# ################################## Story Corps - Alchemer ######################
# story_corps_alchemer_path <- file.path(
#   "C:",
#   "Users",
#   "Brian",
#   "TCM Dropbox",
#   "Brian Kissell",
#   "04 MDM Neuro-Fundraising Lab",
#   "00 Jobs",
#   "2024",
#   "StoryCorps PSA Testing",
#   "Data Collection",
#   "alchemer_data",
#   "All Versions"
# )
#
# story_corps_survey_monkey_path <- file.path(
#   "C:",
#   "Users",
#   "Brian",
#   "TCM Dropbox",
#   "Brian Kissell",
#   "04 MDM Neuro-Fundraising Lab",
#   "00 Jobs",
#   "2024",
#   "StoryCorps PSA Testing",
#   "Data Collection",
#   "survey_monkey_data"
# )
# #
# # story_corps_column_names_path <- file.path(
# #   "C:",
# #   "Users",
# #   "Brian",
# #   "TCM Dropbox",
# #   "Brian Kissell",
# #   "04 MDM Neuro-Fundraising Lab",
# #   "00 Jobs",
# #   "2024",
# #   "StoryCorps PSA Testing",
# #   "Data Collection",
# #   "alchemer_data",
# #   "All Versions",
# #   "All Versions_column_names.xlsx"
# # )
# path_to_rids_to_remove <- paste0(dirname(story_corps_survey_monkey_path), "/", "rids_to_remove.xlsx")
# rids_to_remove <- readxl::read_excel(path_to_rids_to_remove)
#
# story_corps_alchemer_path_newest <- bkissell::FILES_find_newest_file(directory = story_corps_alchemer_path, file_type = ".csv", format_pattern = "_[0-9]{8}_[0-9]{4}", based_on_modiication_date = TRUE)
# story_corps_alchemer_path_column_names <- file.path(story_corps_alchemer_path, "All Versions_column_names.xlsx")
# story_corps_alchemer_column_names_details <- readxl::read_excel(story_corps_alchemer_path_column_names)
# story_corps_alchemer_column_names <- story_corps_alchemer_column_names_details$column_names
# story_corps_alchemer_initial_data_types <- story_corps_alchemer_column_names_details$data_type_initial_read
#
# story_corps_alchemer_data <- readr::read_csv(
#   story_corps_alchemer_path_newest,
#   col_names = story_corps_alchemer_column_names,
#   skip = 1,
#   col_types = story_corps_alchemer_initial_data_types)
#
#
# story_corps_survey_monkey_condition_names <- list.files(story_corps_survey_monkey_path)
#
# story_corps_survey_monkey_condition_paths <- file.path(story_corps_survey_monkey_path, story_corps_survey_monkey_condition_names)
#
# story_corps_survey_monkey_cond_path_newest_list <- purrr::map(story_corps_survey_monkey_condition_paths, ~{
#   bkissell::FILES_find_newest_file(
#     directory = .x,
#     file_type = ".zip",
#     format_pattern = "_[0-9]{8}_[0-9]{4}",
#     based_on_modiication_date = TRUE)
# })
#
# story_corps_survey_monkey_cond_path_col_names_list <- purrr::map2(
#   story_corps_survey_monkey_condition_paths,
#   story_corps_survey_monkey_condition_names,
#   ~{
#     story_corps_alchemer_path_column_names <- file.path(.x, paste0(.y, "_column_names.xlsx"))
#   }
# )
#
# story_corps_survey_monkey_combined <- purrr::map_df(
#   seq_along(story_corps_survey_monkey_cond_path_col_names_list),
#   ~{
#     iteration <- .x
#     story_corps_survey_monkey_column_names_details <- readxl::read_excel(story_corps_survey_monkey_cond_path_col_names_list[[iteration]])
#     story_corps_survey_monkey_column_names <- story_corps_survey_monkey_column_names_details$column_names
#     story_corps_survey_monkey_initial_data_types <- story_corps_survey_monkey_column_names_details$data_type_initial_read
#
#     # This needs to be zip
#     # story_corps_survey_monkey <- readr::read_csv(
#     #   story_corps_survey_monkey_cond_path_newest_list[[iteration]],
#     #   col_names = story_corps_survey_monkey_column_names,
#     #   skip = 1,
#     #   col_types = story_corps_survey_monkey_initial_data_types)
#     #
#     # story_corps_survey_monkey
#
#     # Create Connection to zip files
#     connection_to_zip_file <- bkissell::obtain_the_connection_to_zip_file(story_corps_survey_monkey_cond_path_newest_list[[iteration]])
#
#     survey_data_read <- readr::read_csv(
#       connection_to_zip_file,
#       col_names = story_corps_survey_monkey_column_names,
#       col_types = paste0(story_corps_survey_monkey_initial_data_types, collapse = ""),
#       show_col_types = FALSE,
#       skip = 2)
#
#     survey_data_read
#   })
# #
# # story_corps_survey_monkey_combined %>% glimpse(
# # )
# #
# # story_corps_alchemer_data %>%
# #   dplyr::filter(RID == duplicated(RID))
# #
# # story_corps_survey_monkey_combined %>%
# #   dplyr::filter(sm_RID == duplicated(sm_RID))
# # # story_corps_survey_monkey_combined$sm_RID %>% unique()
# # story_corps_survey_monkey_combined <- story_corps_survey_monkey_combined %>% distinct()
# # story_corps_alchemer_data <- story_corps_alchemer_data %>% distinct()
# # story_corps_survey_monkey_combined$RID <- story_corps_survey_monkey_combined$sm_RID
#
# story_corps_alchemer_data <- story_corps_alchemer_data %>%
#   dplyr::filter(stringr::str_length(RID) > 30) %>%
#   dplyr::filter(!is.na(RID))
#
# story_corps_alchemer_data <- story_corps_alchemer_data %>%
#   dplyr::select(-over_quota_condition_EE)
#
#
# story_corps_survey_monkey_combined <- story_corps_survey_monkey_combined %>%
#   dplyr::filter(stringr::str_length(sm_RID) > 30) %>%
#   dplyr::filter(!is.na(sm_RID))
#
# story_corps_alchemer_data$alc_date_time_start <- story_corps_alchemer_data$alc_date_time_start %>% lubridate::mdy_hms()
# story_corps_alchemer_data$alc_date_time_end <- story_corps_alchemer_data$alc_date_time_end %>% lubridate::mdy_hms()
#
# story_corps_survey_monkey_combined$sm_start_date_time <- story_corps_survey_monkey_combined$sm_start_date_time %>% lubridate::mdy_hms()
# story_corps_survey_monkey_combined$sm_end_date_time <- story_corps_survey_monkey_combined$sm_end_date_time %>% lubridate::mdy_hms()
#
# #
# # index_1 <- colnames(story_corps_alchemer_data) %in% colnames(story_corps_survey_monkey_combined)
# # colnames(story_corps_alchemer_data)[index_1]
# #
# # index_2 <- colnames(story_corps_survey_monkey_combined) %in% colnames(story_corps_alchemer_data)
# # colnames(story_corps_survey_monkey_combined)[index_2]
#
#
# story_corps_alchemer_data <- story_corps_alchemer_data[!duplicated(story_corps_alchemer_data$RID), ]
# story_corps_survey_monkey_combined <- story_corps_survey_monkey_combined[!duplicated(story_corps_survey_monkey_combined$sm_RID), ]
#
# story_corps_data <- story_corps_alchemer_data %>%
#   dplyr::inner_join(story_corps_survey_monkey_combined, by = c("RID" = "sm_RID"))
#
# story_corps_data$study_duration_minutes <- lubridate::ymd_hms(story_corps_data$sm_end_date_time) - lubridate::ymd_hms(story_corps_data$alc_date_time_start)
#
# story_corps_data <- story_corps_data %>%
#   dplyr::filter(!is.na(answered_honestly))
#
#
# story_corps_data$check_on_age <- dplyr::case_when(
#   story_corps_data$age_group == "18 - 29" & (story_corps_data$confirm_age_number >= 18 & story_corps_data$confirm_age_number <= 29) ~ "Correct Age",
#   story_corps_data$age_group == "30 - 39" & (story_corps_data$confirm_age_number >= 30 & story_corps_data$confirm_age_number <= 39) ~ "Correct Age",
#   story_corps_data$age_group == "40 - 49" & (story_corps_data$confirm_age_number >= 40 & story_corps_data$confirm_age_number <= 49) ~ "Correct Age",
#   story_corps_data$age_group == "50 - 59" & (story_corps_data$confirm_age_number >= 50 & story_corps_data$confirm_age_number <= 59) ~ "Correct Age",
#   story_corps_data$age_group == "60 - 69" & (story_corps_data$confirm_age_number >= 60 & story_corps_data$confirm_age_number <= 69) ~ "Correct Age",
#   story_corps_data$age_group == "70 - 74" & (story_corps_data$confirm_age_number >= 70 & story_corps_data$confirm_age_number <= 74) ~ "Correct Age",
#   story_corps_data$age_group == "75+" & (story_corps_data$confirm_age_number >= 75) ~ "Correct Age",
#   .default = "Wrong Age"
#
# )
#
# story_corps_data <- story_corps_data %>%
#   dplyr::filter(check_on_age != "Wrong Age")
#
# # story_corps_data %>% clipr::write_clip()
# #
# # story_corps_data <- story_corps_data %>%
# #   dplyr::filter(!is.na(hope_in_political_system),
# #                 !is.na(faith_in_political_system),
# #                 !is.na(political_views),
# #                 !is.na(stronger_disagreement_with_which_party)
# #   )
#
# story_corps_data$political_views_categorized <- dplyr::case_when(
#   story_corps_data$political_views > 90 ~ "High Conservative Political View",
#   story_corps_data$political_views <= 90 & story_corps_data$political_views >= 50 ~ "Conservative Political View",
#   story_corps_data$political_views < 50 & story_corps_data$political_views >= 10 ~ "Liberal Political View",
#   story_corps_data$political_views < 10 ~ "High Liberal Political View"
# )
#
# # is.na(story_corps_data$political_views_categorized) %>% sum()
#
# story_corps_data$stronger_disagreement_with_which_party <- factor(
#   story_corps_data$stronger_disagreement_with_which_party,
#   levels = c("The Democratic Party", "I disagree with the Republican Party and the Democratic Party equally", "The Republican Party"),
#   labels = c("Democrats", "Both", "Republicans")
# )
#
# # is.na(story_corps_data$stronger_disagreement_with_which_party) %>% sum()
#
# story_corps_data$coded_political_groups <- dplyr::case_when(
#   story_corps_data$political_views_categorized == "High Conservative Political View" & story_corps_data$stronger_disagreement_with_which_party == "Democrats" ~ "High Conservative",
#   story_corps_data$political_views_categorized == "High Conservative Political View" & story_corps_data$stronger_disagreement_with_which_party == "Both" ~ "Conservative",
#   story_corps_data$political_views_categorized == "High Conservative Political View" & story_corps_data$stronger_disagreement_with_which_party == "Republicans" ~ "Other",
#
#   story_corps_data$political_views_categorized == "Conservative Political View" & story_corps_data$stronger_disagreement_with_which_party == "Democrats" ~ "Conservative",
#   story_corps_data$political_views_categorized == "Conservative Political View" & story_corps_data$stronger_disagreement_with_which_party == "Both" ~ "Conservative",
#   story_corps_data$political_views_categorized == "Conservative Political View" & story_corps_data$stronger_disagreement_with_which_party == "Republicans" ~ "Other",
#
#   story_corps_data$political_views_categorized == "Liberal Political View" & story_corps_data$stronger_disagreement_with_which_party == "Democrats" ~ "Other",
#   story_corps_data$political_views_categorized == "Liberal Political View" & story_corps_data$stronger_disagreement_with_which_party == "Both" ~ "Liberal",
#   story_corps_data$political_views_categorized == "Liberal Political View" & story_corps_data$stronger_disagreement_with_which_party == "Republicans" ~ "Liberal",
#
#   story_corps_data$political_views_categorized == "High Liberal Political View" & story_corps_data$stronger_disagreement_with_which_party == "Democrats" ~ "Other",
#   story_corps_data$political_views_categorized == "High Liberal Political View" & story_corps_data$stronger_disagreement_with_which_party == "Both" ~ "Liberal",
#   story_corps_data$political_views_categorized == "High Liberal Political View" & story_corps_data$stronger_disagreement_with_which_party == "Republicans" ~ "High Liberal"
# )
#
# # is.na(story_corps_data$coded_political_groups) %>% sum()
#
# story_corps_data$political_activity_categorized <- ifelse(!is.na(story_corps_data$political_activity__none_of_the_above), "Not Politically Activity", "Politically Active")
#
# story_corps_data$faith_in_political_system_categorized <- ifelse(story_corps_data$faith_in_political_system >= 50, "High Faith", "Low Faith")
#
# story_corps_data$hope_in_political_system_categorized <- ifelse(story_corps_data$hope_in_political_system >= 25, "Not Very Low Hope", "Very Low Hope")
#
# IS_HIGH_CONSERVATIVE <- story_corps_data$coded_political_groups == "High Conservative"
# IS_CONSERVATIVE <- story_corps_data$coded_political_groups == "Conservative"
# IS_OTHER <- story_corps_data$coded_political_groups == "Other"
# IS_LIBERAL <- story_corps_data$coded_political_groups == "Liberal"
# IS_HIGH_LIBERAL <- story_corps_data$coded_political_groups == "High Liberal"
#
# IS_POLITICALLY_ACTIVE <- story_corps_data$political_activity_categorized == "Politically Active"
# IS_NOT_POLITICALLY_ACTIVE <- story_corps_data$political_activity_categorized == "Not Politically Activity"
#
# IS_HIGH_FAITH <- story_corps_data$faith_in_political_system_categorized == "High Faith"
# IS_LOW_FAITH <- story_corps_data$faith_in_political_system_categorized == "Low Faith"
#
# IS_VERY_LOW_HOPE <- story_corps_data$hope_in_political_system_categorized == "Very Low Hope"
# IS_NOT_VERY_LOW_HOPE <- story_corps_data$hope_in_political_system_categorized == "Not Very Low Hope"
#
# political_tribes_ideology_groups_1 <- dplyr::case_when(
#   IS_HIGH_CONSERVATIVE ~ "Traditional Conservative",
#   IS_CONSERVATIVE ~ "Traditional Conservative",
#   IS_OTHER ~ "Everyone Else",
#   IS_LIBERAL ~ "Everyone Else",
#   IS_HIGH_LIBERAL ~ "Everyone Else"
# )
#
# # is.na(political_tribes_ideology_groups_1) %>% sum()
#
# political_tribes_ideology_groups_2 <- dplyr::case_when(
#   IS_NOT_POLITICALLY_ACTIVE & IS_LOW_FAITH ~ "Disengaged",
#   .default = "Not Disengaged"
# )
#
# political_tribes_ideology_groups_3 <- dplyr::case_when(
#   political_tribes_ideology_groups_2 == "Disengaged" & IS_VERY_LOW_HOPE ~ "Exclude",
#   IS_HIGH_LIBERAL & IS_POLITICALLY_ACTIVE ~ "Exclude",
#   IS_HIGH_CONSERVATIVE ~ "Exclude",
#   .default = "Do Not Exclude"
# )
#
# political_tribes_ideology_groups_4 <- dplyr::case_when(
#   political_tribes_ideology_groups_3 == "Exclude" ~ "Exclude",
#   political_tribes_ideology_groups_2 == "Disengaged" ~ "Politically Disengaged",
#   .default = political_tribes_ideology_groups_1
# )
#
# story_corps_data$political_tribes_ideology_groups <- political_tribes_ideology_groups_4
#
#
# # sum(is.na(story_corps_data$political_tribes_ideology_groups))
#
# story_corps_data$experimental_condition_group <- dplyr::case_when(
#   !is.na(story_corps_data$traditional_conservatives_experimental_conditions) ~ "Traditional_Conservative_Condition",
#   !is.na(story_corps_data$politically_disengaged_experimental_conditions) ~ "Politically_Disengaged_Condition",
#   !is.na(story_corps_data$everyone_else_experimental_conditions) ~ "Everyone_Else_Condition"
# )
#
# story_corps_data$experimental_condition <- dplyr::case_when(
#   !is.na(story_corps_data$traditional_conservatives_experimental_conditions) ~ story_corps_data$traditional_conservatives_experimental_conditions,
#   !is.na(story_corps_data$politically_disengaged_experimental_conditions) ~ story_corps_data$politically_disengaged_experimental_conditions,
#   !is.na(story_corps_data$everyone_else_experimental_conditions) ~ story_corps_data$everyone_else_experimental_conditions
# )
#
#
# # sum(!is.na(story_corps_data$traditional_conservatives_experimental_conditions) & !is.na(story_corps_data$politically_disengaged_experimental_conditions))
# # sum(!is.na(story_corps_data$everyone_else_experimental_conditions) & !is.na(story_corps_data$politically_disengaged_experimental_conditions))
# # sum(!is.na(story_corps_data$everyone_else_experimental_conditions) & !is.na(story_corps_data$traditional_conservatives_experimental_conditions))
# # story_corps_data$political_tribes_ideology_groups %>% table()
#
#
#
#
# story_corps_data <- story_corps_data %>%
#   dplyr::filter(!is.na(hope_in_political_system),
#                 !is.na(faith_in_political_system),
#                 !is.na(political_views),
#                 !is.na(stronger_disagreement_with_which_party),
#                 experimental_condition != "Exclude"
#   )
#
# story_corps_data <- story_corps_data %>%
#   dplyr::filter(answered_honestly != "I did not answer questions honestly, and should be disqualified from participation")
# story_corps_data %>% clipr::write_clip()
# story_corps_data <- story_corps_data %>%
#   dplyr::filter(!(RID %in% rids_to_remove$RID))
#
# story_corps_data %>% clipr::write_clip()
#
#
# #
# # story_corps_data %>%
# #   dplyr::select(
# #     political_views_categorized,
# #     stronger_disagreement_with_which_party,
# #     coded_political_groups,
# #     political_activity__none_of_the_above,
# #     faith_in_political_system,
# #     hope_in_political_system,
# #
# #     political_tribes_ideology_groups,
# #     experimental_condition_group,
# #     experimental_condition
# #   ) %>% clipr::write_clip()
# #
# #   dplyr::filter(!is.na(experimental_condition)) %>% View()
#
#
#
#
#
#
# # Count the number of quality participants that we have
# n_completes_quality <- nrow(story_corps_data)
#
# # Labels for
# story_corps_data$experimental_condition_group <- factor(story_corps_data$experimental_condition_group, levels = c("Everyone_Else_Condition","Politically_Disengaged_Condition", "Traditional_Conservative_Condition"))
# experimental_condition_group_count_table <- ftable(story_corps_data$experimental_condition_group)
# experimental_condition_group_prop_table <- round(prop.table(experimental_condition_group_count_table) * 100, 2)
# experimental_condition_group_levels <- levels(story_corps_data$experimental_condition_group)
# # story_corps_data$experimental_condition %>% unique()
# story_corps_data$experimental_condition <- factor(story_corps_data$experimental_condition, levels = c(
#   "Condition 1 - 1a - 2a - 3a",
#   "Condition 2 - 1a - 2a - 3b",
#   "Condition 3 - 1a - 2b - 3a",
#   "Condition 4 - 1a - 2b - 3b",
#   "Condition 5 - 1b - 2a - 3a",
#   "Condition 6 - 1b - 2a - 3b",
#   "Condition 7 - 1b - 2b - 3a",
#   "Condition 8 - 1b - 2b - 3b"))
# experimental_condition_count_table <- ftable(story_corps_data$experimental_condition)
# experimental_condition_prop_table <- round(prop.table(experimental_condition_count_table) * 100, 2)
# experimental_condition_levels <- levels(story_corps_data$experimental_condition)
#
#
#
#
#
# # Labels for generation/age group
# story_corps_data$age_group <- factor(story_corps_data$age_group, levels = c(
#   "18 - 29",
#   "30 - 39",
#   "40 - 49",
#   "50 - 59",
#   "60 - 69",
#   "70 - 74",
#   "75+"))
# age_count_table <- ftable(story_corps_data$age_group)
# age_prop_table <- round(prop.table(age_count_table) * 100, 2)
# age_levels <- levels(story_corps_data$age_group)
#
# # Labels for gender
# story_corps_data$gender <- factor(story_corps_data$gender, levels = c(
#   "Female",
#   "Male",
#   "Other (Please Specify)"))
# gender_count_table <- ftable(story_corps_data$gender)
# gender_prop_table <- round(prop.table(gender_count_table) * 100, 2)
# gender_levels <- levels(story_corps_data$gender)
#
# # Labels for ethnicity
# ethnicity_count_table <- ftable(story_corps_data$ethnicity)
# ethnicity_prop_table <- round(prop.table(ethnicity_count_table) * 100, 2)
# ethnicity_levels <- levels(story_corps_data$ethnicity)
#
# group_by_condition_count_table <- ftable(story_corps_data$experimental_condition, story_corps_data$experimental_condition_group)
# group_by_condition_prop_table <- round(prop.table(group_by_condition_count_table) * 100, 2)
#
# age_by_condition_count_table <- ftable(story_corps_data$experimental_condition, story_corps_data$age_group)
# age_by_condition_prop_table <- round(prop.table(age_by_condition_count_table) * 100, 2)
#
# gender_by_condition_count_table <- ftable(story_corps_data$experimental_condition, story_corps_data$gender)
# gender_by_condition_prop_table <- round(prop.table(gender_by_condition_count_table) * 100, 2)
# ##--------------------------- Create Message ---------------------------------##
#
# # This could be another part of the excel doc, where each requirement could be its own sheet, where all of this information can be provided.
#
# cat(paste0(
#   "  We currently have ",
#   n_completes_quality,
#   "/3600 completed participants. Here are some details on the political groups we are recruiting.
#
#   Political Groups:
#     ", experimental_condition_group_levels[[1]], " - We currently have ", experimental_condition_group_count_table[[1]], ". This will need to be 1200.
#     ", experimental_condition_group_levels[[3]], " - We currently have ", experimental_condition_group_count_table[[3]], ". This will need to be 1200.
#     ", experimental_condition_group_levels[[2]], " - We currently have ", experimental_condition_group_count_table[[2]], ". This will need to be 1200.
#
#   Note: We will do data quality checks throughout data collection, so these numbers will likely change.
# "))
#
#
# story_corps_data %>%
#   dplyr::group_by(experimental_condition_group, gender, age_group) %>%
#   dplyr::summarize(n = n(), .groups = "drop") %>%
#   dplyr::group_by(experimental_condition_group) %>%
#   dplyr::mutate(political_group_total = sum(n), percent = (n / political_group_total) * 100) %>%
#   clipr::write_clip()
#
# # Gender
# story_corps_data %>%
#   dplyr::group_by(experimental_condition_group, gender) %>%
#   dplyr::summarize(n = n(), .groups = "drop") %>%
#   dplyr::group_by(experimental_condition_group) %>%
#   dplyr::mutate(political_group_total = sum(n), percent = (n / political_group_total) * 100)  %>%
#   clipr::write_clip()
#
# story_corps_data %>%
#   dplyr::group_by(experimental_condition_group, age_group) %>%
#   dplyr::summarize(n = n(), .groups = "drop") %>%
#   dplyr::group_by(experimental_condition_group) %>%
#   dplyr::mutate(political_group_total = sum(n), percent = (n / political_group_total) * 100)  %>%
#   clipr::write_clip()
#
# story_corps_data %>%
#   dplyr::group_by(experimental_condition_group, experimental_condition) %>%
#   dplyr::summarize(n = n(), .groups = "drop") %>%
#   dplyr::group_by(experimental_condition_group) %>%
#   dplyr::mutate(political_group_total = sum(n), percent = (n / political_group_total) * 100)  %>%
#   clipr::write_clip()
#
# # Create counts that can be entered into our check tool
# demographic_counts <- story_corps_data %>%
#   dplyr::group_by(experimental_condition, experimental_condition_group) %>%
#   summarize(n = n())
#
#
# demographic_counts %>% clipr::write_clip()
