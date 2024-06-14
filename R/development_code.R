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













