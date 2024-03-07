# Ok, so for this function, I think the files should be processed entirely and
# then combined at the end. Thus at this level, we will include all of the purrr
# functions and file paths that will be used, but everything it will be calling
# will be done on one file at a time. Thus I just need to re-write the functions
# so that all of the actual processing will be done one at a time. Any joining
# of the data frames should then be done in this function. This will leave us
# with a nested function, but it should make more theoretical sense.

survey_version_name = "Member"
# survey_version_name = "Donor"
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
variables_to_include_with_text = c("start_date", "end_date", "wave_info", "version_name",  "gender", "age", "age_group", "ethnicity", "marital_status", "h_c__no_children",	"h_c__under_12",	"h_c__12_to_17",	"h_c__18_to_65",	"h_c__65_and_up", "annual_household_income",	"zip_code", "audience_type")
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
  set_up_project_environment(
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

  # Section 1b - Set-up Column Workbook Lists --------------------------------------------

  # Create the column details list
  bkissell::create_multiple_column_workbook_lists(my_env = my_current_env)

  # Section 2 - Read and process survey data --------------------------------
  bkissell::create_survey_data_for_power_bi_df(my_env = my_current_env)

  # Process the text data
  bkissell::process_all_of_the_text_stuff_for_power_bi(my_env = my_current_env)

  # There is a chance that I need to make a column workbook list specifically for the combined files. At this point, I will just take the first one.
  rlang::env_poke(env = my_current_env, "column_workbook_lists_single", my_current_env$column_workbook_lists[[1]])

  # Multiple Choice
  bkissell::create_power_bi_multiple_choice(my_env = my_current_env)

  # Select All
  bkissell::create_power_bi_select_all(my_env = my_current_env)

  # Numeric
  bkissell::create_power_bi_descr_table_num(my_env = my_current_env)

  # Net Promoter
  bkissell::create_power_bi_net_promoter(my_env = my_current_env)

  # Qualitative Coding Overall
  bkissell::create_power_bi_overall_qualitative(my_env = my_current_env)

  # Qualitative Coding Break Down
  bkissell::create_power_bi_break_down_qualitative_combined(my_env = my_current_env)

  # Return the environment
  return(invisible(my_current_env))

}







#
#
#
#
#
#
# DASHBOARD_CREATION_PLACEHOLDER(
#   survey_version_name = "Member",
#   should_create_nonexistant_dirs = TRUE,
#   survey_monkey_used = TRUE,
#   wave_names = c("w01_11_2023", "w02_02_2024", "w03_04_2024"),
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

# #
# #
# #
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
