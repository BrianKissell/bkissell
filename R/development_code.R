# Ok, so for this function, I think the files should be processed entirely and
# then combined at the end. Thus at this level, we will include all of the purrr
# functions and file paths that will be used, but everything it will be calling
# will be done on one file at a time. Thus I just need to re-write the functions
# so that all of the actual processing will be done one at a time. Any joining
# of the data frames should then be done in this function. This will leave us
# with a nested function, but it should make more theoretical sense.
DASHBOARD_CREATION_PLACEHOLDER <- function(
  survey_version_name = "Member"
  ,
  should_create_nonexistant_dirs = TRUE
  ,
  survey_monkey_used = TRUE
  ,
  wave_names = c("w01_11_2023", "w02_02_2024")
  ,
  storage_platform = "dropbox"
  ,
  storage_platform_name = "TCM Dropbox"
  ,
  group_dir_name = "04 MDM Neuro-Fundraising Lab"
  ,
  jobs_folder_name = "00 Jobs"
  ,
  project_year = 2024
  ,
  project_folder_name = "SDZ_BC__Quarterly Survey_Wave_2"
  ,
  convert_numeric_age_to_age_group = TRUE,
  survey_file_ext = ".zip",
  survey_datetime_format_pattern = "_[0-9]{8}_[0-9]{4}",
  name_of_column_details = "column_details"
  ) {


# Section 1a - Set-up dirs and paths ---------------------------------------

  working_directory_path <- bkissell::set_project_working_directory(
    storage_platform = storage_platform,
    storage_platform_name = storage_platform_name,
    group_dir_name = group_dir_name,
    jobs_folder_name = jobs_folder_name,
    project_year = project_year,
    project_folder_name = project_folder_name
  )

  bkissell::create_traditional_project_dir_list(should_create_nonexistant_dirs = should_create_nonexistant_dirs)

  survey_related_dir_list <- create_survey_related_dir_list(
    survey_version_name = survey_version_name,
    survey_monkey_used = survey_monkey_used,
    wave_names = wave_names,
    should_create_nonexistant_dirs = should_create_nonexistant_dirs
  )

  survey_directory_path_names <- names(survey_related_dir_list)[stringr::str_detect(names(survey_related_dir_list), "^p_path_dc_sm_svn_wave_names_")]

  survey_directory_paths <- survey_related_dir_list[survey_directory_path_names]

  column_names_paths <- purrr::map(survey_directory_paths, ~{
    file.path(.x, paste0(basename(.x), "_column_names.xlsx"))
  })

  spellcheck_column_paths <- purrr::map(survey_directory_paths, ~paste0(.x, "/spellchecked_text_columns.xlsx"))

# Section 1b - Set-up Column Workbook Lists --------------------------------------------

  # Create the column details list
  column_workbook_lists <- purrr::map(column_names_paths, ~{
    column_workbook_list_attempt <- try({bkissell::create_column_details_and_named_vectors_list(
      path_to_column_workbook = .x,
      name_of_column_details = name_of_column_details)}, , silent = TRUE)

    if(class(column_workbook_list_attempt) != "try-error") return(column_workbook_list_attempt)
    })


# Section 2 - Read and process survey data --------------------------------


  survey_data_for_power_bi_list <- purrr::map(seq_along(survey_directory_paths), ~{

    survey_data_for_power_bi_single <- try({
      process_survey_data_for_single_power_bi(
        survey_directory_path_pb = survey_directory_paths[[.x]],
        column_workbook_list_pb = column_workbook_lists[[.x]],
        survey_file_ext = survey_file_ext,
        survey_datetime_format_pattern = survey_datetime_format_pattern,
        survey_version_name = survey_version_name,
        convert_numeric_age_to_age_group = convert_numeric_age_to_age_group,
        spellcheck_column_path_pb = spellcheck_column_paths[[.x]],
        name_of_column_details = name_of_column_details
      )
    }, silent = TRUE)

    if(any(class(survey_data_for_power_bi_single) == "try-error") ){
      warning("data could not be processed")
    } else {

      return(survey_data_for_power_bi_single)
    }

  }) %>% suppressWarnings()

  survey_data_for_power_bi_df <- purrr::reduce(survey_data_for_power_bi_list, dplyr::bind_rows)

  return(survey_data_for_power_bi_df)
}



setdiff(
  colnames(survey_data_for_power_bi_list[[2]]),
  colnames(survey_data_for_power_bi_list[[1]])

)
survey_data_for_power_bi_list[[2]] %>% glimpse()
survey_data_for_power_bi_list[[2]]$X113


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
#   convert_numeric_age_to_age_group = TRUE
# )
#
# DASHBOARD_CREATION_PLACEHOLDER(
#   survey_version_name = "Donor",
#   should_create_nonexistant_dirs = TRUE,
#   survey_monkey_used = TRUE,
#   wave_names = c("w01_11_2023", "w02_02_2024")
# )
