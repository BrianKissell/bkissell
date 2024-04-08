# Ok, so for this function, I think the files should be processed entirely and
# then combined at the end. Thus at this level, we will include all of the purrr
# functions and file paths that will be used, but everything it will be calling
# will be done on one file at a time. Thus I just need to re-write the functions
# so that all of the actual processing will be done one at a time. Any joining
# of the data frames should then be done in this function. This will leave us
# with a nested function, but it should make more theoretical sense.

# survey_version_name = "Member"
survey_version_name = "Donor"
should_create_nonexistant_dirs = TRUE
survey_monkey_used = TRUE
wave_names = c("w01_11_2023", "w02_02_2024")
storage_platform = "dropbox"
storage_platform_name = "TCM Dropbox"
group_dir_name = "04 MDM Neuro-Fundraising Lab"
jobs_folder_name = "00 Jobs"
project_year = 2024
project_folder_name = "SDZ_BC__Quarterly Survey_Wave_2"
convert_numeric_age_to_age_group = TRUE
survey_file_ext = ".zip"
survey_datetime_format_pattern = "_[0-9]{8}_[0-9]{4}"
name_of_column_details = "column_details"
write_data = TRUE
variables_to_include_with_text = c(
      "start_date", "end_date", "wave_info", "version_name",  "gender", "age",
      "age_group", "ethnicity", "marital_status", "h_c__no_children",	"h_c__under_12",
      "h_c__12_to_17",	"h_c__18_to_65",	"h_c__65_and_up",
      "annual_household_income",	"zip_code", "audience_type", "net_promoter",
      "length_of_membership",	"number_of_visits_last_year",
      "number_of_planned_visits",	"likelihood_of_renewing",
      "donated_to_sdzwa_in_past", "confident_donation_will_help",
      "monthly_donor",	"support_sdzwa_again")
grouping_vars = c("wave_info", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income", "audience_type")

DASHBOARD_CREATION_PLACEHOLDER <- function(
    survey_version_name = "Member",
    should_create_nonexistant_dirs = TRUE,
    survey_monkey_used = TRUE,
    wave_names = c("w01_11_2023", "w02_02_2024"),
    storage_platform = "dropbox",
    storage_platform_name = "TCM Dropbox",
    group_dir_name = "04 MDM Neuro-Fundraising Lab",
    jobs_folder_name = "00 Jobs",
    project_year = 2024,
    project_folder_name = "SDZ_BC__Quarterly Survey_Wave_2",
    convert_numeric_age_to_age_group = TRUE,
    survey_file_ext = ".zip",
    survey_datetime_format_pattern = "_[0-9]{8}_[0-9]{4}",
    name_of_column_details = "column_details",
    write_data = TRUE,
    variables_to_include_with_text = c(
      "start_date", "end_date", "wave_info", "version_name",  "gender", "age",
      "age_group", "ethnicity", "marital_status", "h_c__no_children",	"h_c__under_12",
      "h_c__12_to_17",	"h_c__18_to_65",	"h_c__65_and_up",
      "annual_household_income",	"zip_code", "audience_type", "net_promoter",
      "length_of_membership",	"number_of_visits_last_year",
      "number_of_planned_visits",	"likelihood_of_renewing",
      "donated_to_sdzwa_in_past", "confident_donation_will_help",
      "monthly_donor",	"support_sdzwa_again"),
    grouping_vars = c("wave_info", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income", "audience_type")
) {

  # Create all of the needed file paths, and assign them to the my_current_env env.
  # Set the initial environment
  my_current_env <- rlang::env()

  my_current_env <- bkissell::set_up_project_environment(
    my_env = my_current_env,
    storage_platform = storage_platform,
    storage_platform_name = storage_platform_name,
    group_dir_name = group_dir_name,
    jobs_folder_name = jobs_folder_name,
    project_year = project_year,
    project_folder_name = project_folder_name,
    should_create_nonexistant_dirs = should_create_nonexistant_dirs,
    survey_version_name = survey_version_name,
    survey_monkey_used = survey_monkey_used,
    wave_names = wave_names
  )

  # # This is a list of things that were created. I do want this to be more specific
  # # to the specific project we are running, but right now it is not doing that.
  # rlang::env_print(my_current_env)

  # Section 1b - Set-up Column Workbook Lists --------------------------------------------
  column_names_paths <- paste0(my_current_env$working_directory_path, "/", my_current_env$column_names_paths)
  # Create the column details list
  my_current_env <- bkissell::create_multiple_column_workbook_lists(my_env = my_current_env, column_names_paths = column_names_paths, name_of_column_details)

  # Section 2 - Read and process survey data --------------------------------
  my_current_env <- bkissell::create_survey_data_for_power_bi_df(
    my_env = my_current_env,
    write_data = write_data,
    survey_file_ext = survey_file_ext,
    survey_datetime_format_pattern = "_[0-9]{8}_[0-9]{4}",
    data_collection_survey_directory_paths = my_current_env$survey_directory_paths,
    working_directory_path = my_current_env$working_directory_path,
    column_workbook_lists = my_current_env$column_workbook_lists,
    survey_version_name = survey_version_name,
    convert_numeric_age_to_age_group = TRUE,
    spellcheck_column_paths = my_current_env$spellcheck_column_paths,
    name_of_column_details = name_of_column_details)

  # Process the text data
  my_current_env <- bkissell::process_all_of_the_text_stuff_for_power_bi(
    my_env = my_current_env,
    vars_to_include_with_text = variables_to_include_with_text,
    survey_data_for_power_bi_df = my_current_env$survey_data_for_power_bi_df,
    survey_dir_paths_list = my_current_env$survey_directory_paths,
    text_survey_data_path = dirname(my_current_env$text_survey_data_path),
    working_directory_path = my_current_env$working_directory_path)

  # There is a chance that I need to make a column workbook list specifically for the combined files. At this point, I will just take the first one.
  rlang::env_poke(env = my_current_env, "column_workbook_lists_single", my_current_env$column_workbook_lists[[1]])

  # Multiple Choice
  my_current_env <- bkissell::create_power_bi_multiple_choice(my_env = my_current_env)

  # Select All
  my_current_env <- bkissell::create_power_bi_select_all(my_env = my_current_env)

  # Numeric
  my_current_env <- bkissell::create_power_bi_descr_table_num(my_env = my_current_env)

  # Net Promoter
  my_current_env <- bkissell::create_power_bi_net_promoter(my_env = my_current_env)

  # Qualitative Coding Overall
  my_current_env <- bkissell::create_power_bi_overall_qualitative(my_env = my_current_env)

  # Qualitative Coding Break Down
  my_current_env <- bkissell::create_power_bi_break_down_qualitative_combined(my_env = my_current_env)

  # Return the environment
  return(invisible(my_current_env))
}






#
#
# DASHBOARD_CREATION_PLACEHOLDER(
#   survey_version_name = "Member",
#   should_create_nonexistant_dirs = TRUE,
#   survey_monkey_used = TRUE,
#   wave_names = c("w01_11_2023", "w02_02_2024"),
#   storage_platform = "dropbox",
#   storage_platform_name = "TCM Dropbox",
#   group_dir_name = "04 MDM Neuro-Fundraising Lab",
#   jobs_folder_name = "00 Jobs",
#   project_year = 2024,
#   project_folder_name = "SDZ_BC__Quarterly Survey_Wave_2",
#   convert_numeric_age_to_age_group = TRUE,
#   survey_file_ext = ".zip",
#   survey_datetime_format_pattern = "_[0-9]{8}_[0-9]{4}",
#   name_of_column_details = "column_details",
#   write_data = TRUE,
#   variables_to_include_with_text = c(
#     "start_date", "end_date", "wave_info", "version_name",  "gender", "age",
#     "age_group", "ethnicity", "marital_status", "h_c__no_children",	"h_c__under_12",
#     "h_c__12_to_17",	"h_c__18_to_65",	"h_c__65_and_up",
#     "annual_household_income",	"zip_code", "audience_type", "net_promoter",
#     "length_of_membership",	"number_of_visits_last_year",
#     "number_of_planned_visits",	"likelihood_of_renewing",
#     "donated_to_sdzwa_in_past", "confident_donation_will_help",
#     "monthly_donor",	"support_sdzwa_again"),
#   grouping_vars = c("wave_info", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income", "audience_type")
# )
#
# DASHBOARD_CREATION_PLACEHOLDER(
#   survey_version_name = "Donor",
#   should_create_nonexistant_dirs = TRUE,
#   survey_monkey_used = TRUE,
#   wave_names = c("w01_11_2023", "w02_02_2024"),
#   storage_platform = "dropbox",
#   storage_platform_name = "TCM Dropbox",
#   group_dir_name = "04 MDM Neuro-Fundraising Lab",
#   jobs_folder_name = "00 Jobs",
#   project_year = 2024,
#   project_folder_name = "SDZ_BC__Quarterly Survey_Wave_2",
#   convert_numeric_age_to_age_group = TRUE,
#   survey_file_ext = ".zip",
#   survey_datetime_format_pattern = "_[0-9]{8}_[0-9]{4}",
#   name_of_column_details = "column_details",
#   write_data = TRUE,
#   variables_to_include_with_text = c(
#     "start_date", "end_date", "wave_info", "version_name",  "gender", "age",
#     "age_group", "ethnicity", "marital_status", "h_c__no_children",	"h_c__under_12",
#     "h_c__12_to_17",	"h_c__18_to_65",	"h_c__65_and_up",
#     "annual_household_income",	"zip_code", "audience_type", "net_promoter",
#     "length_of_membership",	"number_of_visits_last_year",
#     "number_of_planned_visits",	"likelihood_of_renewing",
#     "donated_to_sdzwa_in_past", "confident_donation_will_help",
#     "monthly_donor",	"support_sdzwa_again"),
#   grouping_vars = c("wave_info", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income", "audience_type")
# )



RUN_Qualitative_Coding <- function(
    storage_platform,
    storage_platform_name,
    group_dir_name,
    jobs_folder_name,
    project_year,
    project_folder_name,
    should_create_nonexistant_dirs,
    survey_version_name,
    survey_monkey_used,
    wave_names,
    write_data,
    name_of_column_details = "column_details"
) {

  # 1) Initiate the current environment as "my_current_env". This allows us to create a bucket that contains copies of the things we do throughout the function.
  bkissell::initiate_my_current_env(
    storage_platform, storage_platform_name, group_dir_name, jobs_folder_name,
    project_year, project_folder_name
  )

  # 2) Set up the needed folders and needed file paths

  # Add traditional project dirs
  bkissell::add_traditional_project_dir_list_to_env(my_current_env, should_create_nonexistant_dirs)

  # Add dirs related to running surveys
  bkissell::add_survey_related_dir_list_to_env(my_current_env, survey_version_name, survey_monkey_used, wave_names, should_create_nonexistant_dirs)

  # Use vector to specify additional paths to create.
  bkissell::create_specified_file_paths(
    my_env = my_current_env,
    survey_directory_paths = my_current_env$survey_directory_paths,
    vector_of_path_names_or_objects_to_create = c("qualitative_coding_data_path_list", "column_names_paths", "column_workbook_lists", "spellcheck_column_paths", "power_bi_overall_qualitative_path", "power_bi_break_down_qualitative_path", "power_bi_break_down_qualitative_path_list"),
    column_names_paths = my_current_env$column_names_paths,
    name_of_column_details = name_of_column_details
  )

  message("1) Job specific paths have been created")

  # 3) Process the survey data that will be used to create the calculations. This will be used to join other important information to the qual data.
  suppressWarnings(bkissell::create_survey_data_for_power_bi_df(my_env = my_current_env, write_data = FALSE))


  message("2) Survey data has been read in an processed")

  # 4) Do all of the overall qualitative coding stuff (needs more work)
  power_bi_qualitative_both <- bkissell::qualitative_coding_system(
    qual_path_list = my_current_env$qualitative_coding_data_path_list,
    processed_survey_data = my_current_env$survey_data_for_power_bi_df,
    column_workbook_lists_single = my_current_env$column_workbook_lists_single,
    grouping_vars,
    name_of_column_details,
    path_to_qual_coding_data_list = my_current_env$qualitative_coding_data_path_list,
    write_data,
    power_bi_overall_qualitative_path = my_current_env$power_bi_overall_qualitative_path
  )

  rlang::env_poke(env = my_current_env, "power_bi_overall_qualitative_combined", power_bi_qualitative_both$power_bi_overall_qualitative_combined)
  rlang::env_poke(env = my_current_env, "power_bi_break_down_qualitative_combined", power_bi_qualitative_both$power_bi_break_down_qualitative_combined)

message("3) Qualitative data has been processed.")
  return(invisible(my_current_env))

}







#
#
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
#
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
#     }
#   )
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
# })
# #
# # story_corps_survey_monkey_combined %>% glimpse(
# # )
# #
# # story_corps_alchemer_data %>%
# #   dplyr::filter(RID == duplicated(RID))
# #
# # story_corps_survey_monkey_combined %>%
# #   dplyr::filter(sm_RID == duplicated(sm_RID))
# # story_corps_survey_monkey_combined$sm_RID %>% unique()
#
# story_corps_data <- story_corps_alchemer_data %>%
#   dplyr::inner_join(story_corps_survey_monkey_combined, by = c("RID" = "sm_RID"))
#
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
#   )
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
#                 )
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
# story_corps_data$experimental_condition_group <- factor(story_corps_data$experimental_condition_group, levels = c("Traditional_Conservative_Condition", "Everyone_Else_Condition","Politically_Disengaged_Condition"))
# experimental_condition_group_count_table <- ftable(story_corps_data$experimental_condition_group)
# experimental_condition_group_prop_table <- round(prop.table(experimental_condition_group_count_table) * 100, 2)
# experimental_condition_group_levels <- levels(story_corps_data$experimental_condition_group)
#
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
#   During our updates after the full launch, we will also include the demographic breakdowns for the differing political groups.
# "))
#
#
# #sm_df$gender <- factor(sm_df$gender, c("Female", "Male"))
#
# # Create counts that can be entered into our check tool
# demographic_counts <- sm_df %>%
#   dplyr::group_by( version_name, age_group_recalc, gender, ethnicity) %>%
#   summarize(n = n())
#
#
#
#
#
#
#
#
#
#
#
#
#
#








