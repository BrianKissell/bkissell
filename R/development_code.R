# Ok, so for this function, I think the files should be processed entirely and
# then combined at the end. Thus at this level, we will include all of the purrr
# functions and file paths that will be used, but everything it will be calling
# will be done on one file at a time. Thus I just need to re-write the functions
# so that all of the actual processing will be done one at a time. Any joining
# of the data frames should then be done in this function. This will leave us
# with a nested function, but it should make more theoretical sense.

survey_version_name = "Member"
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
variables_to_include_with_text = c("start_date", "end_date", "wave_info", "version_name",  "gender", "age", "age_group", "ethnicity", "marital_status", "h_c__no_children",	"h_c__under_12",	"h_c__12_to_17",	"h_c__18_to_65",	"h_c__65_and_up", "annual_household_income",	"zip_code", "audience_type")
grouping_vars = c("wave_info", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income", "audience_type")

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
  convert_numeric_age_to_age_group = TRUE
  ,
  survey_file_ext = ".zip"
  ,
  survey_datetime_format_pattern = "_[0-9]{8}_[0-9]{4}"
  ,
  name_of_column_details = "column_details"
  ,
  write_data = TRUE
  ,
  variables_to_include_with_text = c("start_date", "end_date", "wave_info", "version_name",  "gender", "age", "age_group", "ethnicity", "marital_status", "h_c__no_children",	"h_c__under_12",	"h_c__12_to_17",	"h_c__18_to_65",	"h_c__65_and_up", "annual_household_income",	"zip_code", "audience_type")
  ,
  grouping_vars = c("wave_info", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income", "audience_type")
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

  traditional_project_dir_list <- bkissell::create_traditional_project_dir_list(should_create_nonexistant_dirs = should_create_nonexistant_dirs)

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

  power_bi_clean_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/power_bi_deck/clean_data.csv")

  processed_data_clean_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_data/PROCESSED_", snakecase::to_snake_case(survey_version_name), "_data_", bkissell::create_time_chr_string_for_file_names("%Y%m%d_%H%M"), ".csv")


  text_survey_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_text/TEXT_PROCESSED_", snakecase::to_snake_case(survey_version_name), "_", bkissell::create_time_chr_string_for_file_names("%Y%m%d_%H%M"), ".csv")


  text_selected_example_text_survey_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_selected_example_text.csv")

  power_bi_text_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_text_survey_data.csv")

  text_selected_example_text_survey_data_path <- paste0("Data Collection/survey_monkey_data/", survey_version_name, "/selected_example_text.xlsx")

  text_selected_example_response_vars <- readxl::excel_sheets(text_selected_example_text_survey_data_path)

  parameters_for_example_read <- data.frame(Var1 = text_selected_example_response_vars)
  parameters_for_example_read$Var2 <- text_selected_example_text_survey_data_path

  power_bi_mc_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_multiple_choice.csv")

  power_bi_sa_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_select_all.csv")

  power_bi_descr_table_num_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_descr_table_num.csv")

  power_bi_net_promoter_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_net_promoter.csv")



  qualitative_coding_data_path_list <- paste0(survey_directory_paths, "/qualitative_coding_data.xlsx") %>% as.list()

  # column_workbook_list <- purrr::map(path_to_column_workbook_list, ~{
  #   create_column_details_and_named_vectors_list(path_to_column_workbook = .x, name_of_column_details = "column_details")
  # })
  #
  # column_details_list <- purrr::map(column_workbook_list, ~{
  #   obtain_column_details_table(column_workbook_list = .x, name_of_column_details = "column_details")
  # })

  power_bi_overall_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_overall_qualitative.csv")

  power_bi_break_down_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")

  power_bi_break_down_qualitative_path_list <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")




#   # Create the paths for the folders that need to exist for the survey project
#   survey_folder_structure_paths_list <- append(traditional_project_dir_list, survey_related_dir_list)
#     # bkissell::create_survey_folder_structure_paths_list_power_bi
#
#   # Point to where the survey data is located
#   survey_directory_path <<- survey_folder_structure_paths_list$project_data_collection_survey_monkey_data_folder_location
#
#   # Create path for home dir
#   home_dir <<- survey_folder_structure_paths_list$home_dir
#
#   # Read the survey data as a list
#   survey_data_list <<- read_survey_data_for_power_bi(survey_directory_path = survey_directory_path)
#
#   # Obtain the version names
#   version_names_from_list_data_collection <<- names(survey_data_list)
#
#   # Create Version Directories
#   version_directories_data_collection_path <<- prepare_version_directory_paths(survey_directory_path)
#
#   spellcheck_column_paths <<- paste0(version_directories_data_collection_path, "/spellchecked_text_columns.xlsx")
#
#   # Create the path where the column information workbook will be located
#   path_to_column_workbook <<- obtain_column_name_paths_for_all_survey_versions(version_directories_data_collection_path)
#
#   path_to_column_workbook_list <<- path_to_column_workbook
#
#   version_names_from_list_analysis <<- names(survey_data_list)
#
#   rids_to_remove_path <<- paste0(survey_directory_path, "/rids_to_remove.csv")
#
#   rids_to_keep_path <<- paste0(survey_directory_path, "/rids_to_keep.csv")
#
#
#
#   processed_data_clean_data_path_list <<- purrr::map(version_names_from_list_analysis, ~{
#     paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/processed_data/PROCESSED_", snakecase::to_snake_case(.x), "_data_", create_time_chr_string_for_file_names(), ".csv")
#   }, home_dir)
#
#   # Combine other and group variable names
#   variables_to_include_in_all_tables <<- c(other_vars_to_include, grouping_vars)
#
#
#   survey_data_list <<- process_and_clean_power_bi_survey_data(
#     survey_directory_path,
#     spellcheck_replace_columns,
#     spellcheck_column_paths,
#     all_versions_in_same_analysis_folder,
#     path_to_column_workbook,
#     version_name_style,
#     likert_indicator = "ec__",
#     item_everyone_must_complete,
#     remove_rids,
#     keep_rids
#   )
#
#
#   # Filter list to match the variables contained in the df
#   variables_to_include_list <<- purrr::map(survey_data_list, ~{
#     data <- .x
#     #
#     #     if(!("age_group" %in% colnames(data))) {
#     #       data$age_group <- NA
#     #     }
#     #
#     #     if(!("study_version" %in% colnames(data))) {
#     #       data$study_version <- NA
#     #     }
#
#
#     data %>% dplyr::select(any_of(variables_to_include_in_all_tables)) %>% colnames()
#   })
#
#   # Filter to list only the grouping vars contained in each data frame
#   grouping_vars_list <<- purrr::map(survey_data_list, ~{
#     .x %>% dplyr::select(any_of(grouping_vars)) %>% colnames()
#   })
#
#   text_survey_data_path_list <<- purrr::map(version_names_from_list_analysis, ~{
#     paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/processed_text/TEXT_PROCESSED_", snakecase::to_snake_case(.x), "_", create_time_chr_string_for_file_names(), ".csv")
#   }, home_dir)
#
#   # Save it to the powerbi folder
#   power_bi_text_path_list <<- purrr::map(version_names_from_list_analysis, ~{
#     paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_text_survey_data.csv")
#   }, home_dir)
#
#
#   if(spellcheck_replace_columns == TRUE) {
#     text_selected_example_text_survey_data_path_list <<- purrr::map_chr(version_names_from_list_analysis, ~{
#       paste0(home_dir, "/Data Collection/survey_monkey_data/", .x, "/selected_example_text.xlsx")
#     }, home_dir)
#
#     text_selected_example_response_vars_list <<- purrr::map(text_selected_example_text_survey_data_path_list, ~{
#       readxl::excel_sheets(.x)
#     })
#
#     create_VAR_df_with_single_row <- purrr::as_mapper(
#       function(path, response_var_to_use) {
#         data.frame(Var1 = response_var_to_use, Var2 = path)
#       })
#
#     create_VAR_df_with_multiple_rows <- purrr::as_mapper(
#       function(path, response_vars_to_use) {
#         path <- path
#         response_vars_to_use <- response_vars_to_use
#         purrr::map_df(response_vars_to_use,  ~ {
#           create_VAR_df_with_single_row(path, .x)
#         }, path)
#       })
#
#     parameters_for_example_read_list <<- purrr::map2(text_selected_example_text_survey_data_path_list, text_selected_example_response_vars_list, ~ {
#       create_VAR_df_with_multiple_rows(.x, .y)
#     })
#
#     text_selected_example_text_survey_data_path_list <<- purrr::map_chr(version_names_from_list_analysis, ~{
#       paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_selected_example_text.csv")
#     }, home_dir)
#   }
#
#   power_bi_mc_path_list <<- purrr::map(version_names_from_list_analysis, ~{
#     paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_member_multiple_choice.csv")
#   }, home_dir)
#
#   power_bi_sa_path_list <<- purrr::map(version_names_from_list_analysis, ~{
#     paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_member_select_all.csv")
#   }, home_dir)
#
#   power_bi_descr_table_num_path_list <<- purrr::map(version_names_from_list_analysis, ~{
#     paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_member_descr_table_num.csv")
#   }, home_dir)
#
#   power_bi_net_promoter_path_list <<- purrr::map(version_names_from_list_analysis, ~{
#     paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_net_promoter.csv")
#   }, home_dir)
#
#   column_workbook_list <<- purrr::map(path_to_column_workbook_list, ~{
#     create_column_details_and_named_vectors_list(path_to_column_workbook = .x, name_of_column_details = "column_details")
#   })
#
#   column_details_list <<- purrr::map(column_workbook_list, ~{
#     obtain_column_details_table(column_workbook_list = .x, name_of_column_details = "column_details")
#   })
#
#   power_bi_overall_qualitative_path_list <<- purrr::map(version_names_from_list_analysis, ~{
#     paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_overall_qualitative.csv")
#   }, home_dir)
#
#   power_bi_break_down_qualitative_path_list <<- purrr::map(version_names_from_list_analysis, ~{
#     paste0(home_dir, "/Analysis/Respondent Investigation/", .x, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")
#   }, home_dir)


# Section 1b - Set-up Column Workbook Lists --------------------------------------------

  # Create the column details list
  column_workbook_lists <- purrr::map(column_names_paths, ~{
    column_workbook_list_attempt <- try({bkissell::create_column_details_and_named_vectors_list(
      path_to_column_workbook = .x,
      name_of_column_details = name_of_column_details)}, , silent = TRUE)

    if(class(column_workbook_list_attempt) != "try-error") return(column_workbook_list_attempt)
    })


# Section 2 - Read and process survey data --------------------------------

  survey_data_for_power_bi_df <- purrr::map(seq_along(survey_directory_paths), ~{
    iteration <- .x

    survey_data_for_power_bi_single <- try({
      bkissell::process_survey_data_for_single_power_bi(
        survey_directory_path_pb = survey_directory_paths[[iteration]],
        column_workbook_list_pb = column_workbook_lists[[iteration]],
        survey_file_ext = survey_file_ext,
        survey_datetime_format_pattern = survey_datetime_format_pattern,
        survey_version_name = survey_version_name,
        convert_numeric_age_to_age_group = convert_numeric_age_to_age_group,
        spellcheck_column_path_pb = spellcheck_column_paths[[iteration]],
        name_of_column_details = name_of_column_details
      )
    }, silent = TRUE)

    if(any(class(survey_data_for_power_bi_single) == "try-error") ){
      warning("data could not be processed")
      return(data.frame())
    } else {
      return(survey_data_for_power_bi_single)
    }
  }) %>%
    suppressWarnings() %>%
    purrr::reduce(dplyr::bind_rows)


  # Save the cleaned data to the power bi folder
  if(write_data){
    readr::write_csv(survey_data_for_power_bi_df, power_bi_clean_data_path)

    Sys.sleep(1)

    # Save another copy to the backup folder
    readr::write_csv(survey_data_for_power_bi_df, processed_data_clean_data_path)
  }


  # # run this to just read in an old version of the cleaned data
  # survey_data_for_power_bi_df <- readr::read_csv("Analysis/Respondent Investigation/Member/processed_data/PROCESSED_member_data_20240212_0910.csv")

## Text
  # Filter for needed variables
  text_survey_data <-  survey_data_for_power_bi_df %>%
    dplyr::select(
      "RID",
      tidyselect::all_of(variables_to_include_with_text),
      starts_with("text_")
    )

  if(write_data){
    # Save a copy of the text data as a backup
    readr::write_csv(text_survey_data, text_survey_data_path)
  }


  # Shape and filter the textual data
  power_bi_text <- text_survey_data %>%
    tidyr::pivot_longer(
      cols = -c("RID", tidyselect::all_of(variables_to_include_with_text)),
      names_to = "response_var_used",
      names_prefix = "text_",
      values_to = "textual_responses"
    ) %>%
    dplyr::filter(!is.na(.data[["textual_responses"]]))

  # Remove the "'" as the symbol is entered incorrectly in excel
  power_bi_text$textual_responses <- stringr::str_replace_all(power_bi_text$textual_responses, "'", "")

  if(write_data){
    # Save it to the powerbi folder
    readr::write_csv(power_bi_text, power_bi_text_path)
  }


  if(file.exists(text_selected_example_text_survey_data_path)){

    read_and_add_param_to_column <- purrr::as_mapper(
      function(param_df) {
        purrr::map_df(seq_along(param_df$Var1), ~{
          selected_examples_df <- readxl::read_excel(param_df$Var2[[.x]], param_df$Var1[[.x]])
          selected_examples_df$response_var_used <- param_df$Var1[[.x]]
          selected_examples_df
        }, param_df)
      })

    selected_examples_text_df <- read_and_add_param_to_column(parameters_for_example_read)
    # selected_examples_text_df$RID <- selected_examples_text_df$respondent_id
    selected_examples_text_df$RID <- as.numeric(selected_examples_text_df$RID)
    text_survey_data$RID <- as.numeric(text_survey_data$RID)

    combined_df_selected_examples_text_df <- selected_examples_text_df %>%
        dplyr::left_join(text_survey_data, by = "RID") %>%
        dplyr::select(!tidyselect::starts_with("text_"))

    selected_examples_text_df$RID <- as.character(selected_examples_text_df$RID)
    text_survey_data$RID <- as.character(text_survey_data$RID)
    combined_df_selected_examples_text_df$RID <- as.character(combined_df_selected_examples_text_df$RID)

    if(write_data){
      writexl::write_xlsx(combined_df_selected_examples_text_df, text_selected_example_text_survey_data_path)
    }
  }

## There is a chance that I need to make a column workbook list specifically for the combined files. At this point, I will just take the first one.
  column_workbook_lists_single <- column_workbook_lists[[1]]

  ## Multiple Choice
  if(any(column_workbook_lists_single[[name_of_column_details]]$type == "mc")) {

    power_bi_multiple_choice <- create_power_bi_data_mc_CALCULATED_TABLES(
      df = survey_data_for_power_bi_df,
      column_workbook_list = column_workbook_lists_single,
      grouping_vars = grouping_vars,
      name_of_column_details = name_of_column_details)

    if(write_data){
      readr::write_csv(power_bi_multiple_choice, power_bi_mc_path)
    }
    power_bi_multiple_choice

  }


  ### Select All ###############################################################
  check_for_select_all_vars <- stringr::str_detect(column_workbook_lists_single[[name_of_column_details]]$type, "^sa")

  if(any(check_for_select_all_vars)){
    power_bi_select_all <- create_power_bi_data_sa_CALCULATED_TABLES(
      df = survey_data_for_power_bi_df,
      column_workbook_list = column_workbook_lists_single,
      grouping_vars = grouping_vars,
      name_of_column_details = name_of_column_details)
    if(write_data){
      readr::write_csv(power_bi_select_all, power_bi_sa_path)
    }
    power_bi_select_all
  }

  ### Numeric ##################################################################
  check_for_numeric_vars <- column_workbook_lists_single[[name_of_column_details]]$type == "numeric"

  check_for_scale_vars <- !is.na(column_workbook_lists_single[[name_of_column_details]]$scale_names)

  if(any(check_for_numeric_vars) | any(check_for_scale_vars)){
    power_bi_descr_table_num <- create_power_bi_data_num_CALCULATED_TABLES(
      df = survey_data_for_power_bi_df,
      column_workbook_list = column_workbook_lists_single,
      grouping_vars = grouping_vars,
      name_of_column_details = name_of_column_details)
    if(write_data){
      readr::write_csv(power_bi_descr_table_num, power_bi_descr_table_num_path)
    }
    power_bi_descr_table_num
  }

  ### Net Promoter #############################################################
  check_for_net_promoter_vars <- column_workbook_lists_single[[name_of_column_details]]$type == "numeric"

  if(any(check_for_net_promoter_vars)){
    power_bi_net_promoter <- create_power_bi_data_nps_CALCULATED_TABLES(
      df = survey_data_for_power_bi_df,
      column_workbook_list = column_workbook_lists_single,
      grouping_vars = grouping_vars,
      name_of_column_details = name_of_column_details)
    if(write_data){
      readr::write_csv(power_bi_net_promoter, power_bi_net_promoter_path)
    }
    power_bi_net_promoter
  }



  ### Qualitative Coding #######################################################
# If I end up needing a combined set, I will need to add an optional piece that does those calculations
  power_bi_overall_qualitative_combined <- purrr::map(qualitative_coding_data_path_list, ~ {
    if(file.exists(.x)){
      power_bi_overall_qualitative <- create_power_bi_data_qualitative_CALCULATED_TABLES(
        df = survey_data_for_power_bi_df,
        column_workbook_list,
        grouping_vars,
        name_of_column_details,
        path_to_qual_coding_data = .x,
        qualitative_type = "overall_coding",
        identifier = "RID")

      power_bi_overall_qualitative <- power_bi_overall_qualitative[[1]]

      order_information_lookup_table <- create_order_information_lookup_table(column_workbook_list, name_of_column_details)

      power_bi_overall_qualitative$grouping_var_used <- snakecase::to_snake_case(power_bi_overall_qualitative$grouping_var_used)

      power_bi_overall_qualitative <- adjust_order_information_lookup_table_per_type(table = power_bi_overall_qualitative, order_information_lookup_table, type = "grouping_var_")
      power_bi_overall_qualitative <- power_bi_overall_qualitative %>%
        dplyr::filter(!is.na(grouping_var_levels))


      power_bi_overall_qualitative$Wave <- basename(dirname(.x))

      power_bi_overall_qualitative
    } else {
      return(data.frame())
    }
  }) %>% purrr::reduce(dplyr::bind_rows)


  if(write_data){
    readr::write_csv(power_bi_overall_qualitative_combined, power_bi_overall_qualitative_path)
  }


  power_bi_break_down_qualitative_combined <- purrr::map(qualitative_coding_data_path_list, ~ {
    if(file.exists(.x)){

      power_bi_break_down_qualitative <- create_power_bi_data_qualitative_CALCULATED_TABLES(
        df = survey_data_for_power_bi_df,
        column_workbook_list,
        grouping_vars,
        name_of_column_details,
        path_to_qual_coding_data = .x,
        qualitative_type = "break_down_coding",
        identifier = "RID")

      power_bi_break_down_qualitative <- power_bi_break_down_qualitative[[1]]

      order_information_lookup_table <- create_order_information_lookup_table(column_workbook_list, name_of_column_details)

      power_bi_break_down_qualitative$grouping_var_used <- snakecase::to_snake_case(power_bi_break_down_qualitative$grouping_var_used)

      power_bi_break_down_qualitative <- adjust_order_information_lookup_table_per_type(table = power_bi_break_down_qualitative, order_information_lookup_table, type = "grouping_var_")
      power_bi_break_down_qualitative <- power_bi_break_down_qualitative %>%
        dplyr::filter(!is.na(grouping_var_levels))

      power_bi_break_down_qualitative$Wave <- basename(dirname(.x))

      power_bi_break_down_qualitative
    } else {
      return(data.frame())
    }

  }) %>% purrr::reduce(dplyr::bind_rows)

  if(write_data){
    readr::write_csv(power_bi_break_down_qualitative_combined, power_bi_break_down_qualitative_path)
  }




}


#
#
#
#
#
# DASHBOARD_CREATION_PLACEHOLDER(
#     survey_version_name = "Member",
#     should_create_nonexistant_dirs = TRUE,
#     survey_monkey_used = TRUE,
#     wave_names = c("w01_11_2023", "w02_02_2024"),
#     storage_platform = "dropbox",
#     storage_platform_name = "TCM Dropbox",
#     group_dir_name = "04 MDM Neuro-Fundraising Lab",
#     jobs_folder_name = "00 Jobs",
#     project_year = 2024,
#     project_folder_name = "SDZ_BC__Quarterly Survey_Wave_2",
#     convert_numeric_age_to_age_group = TRUE,
#     survey_file_ext = ".zip",
#     survey_datetime_format_pattern = "_[0-9]{8}_[0-9]{4}",
#     name_of_column_details = "column_details",
#     write_data = TRUE,
#     variables_to_include_with_text = c("start_date", "end_date", "wave_info", "version_name",  "gender", "age", "age_group", "ethnicity", "marital_status", "h_c__no_children",	"h_c__under_12",	"h_c__12_to_17",	"h_c__18_to_65",	"h_c__65_and_up", "annual_household_income",	"zip_code", "audience_type"),
#     grouping_vars = c("wave_info", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income", "audience_type")
# )
#
#
#
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
#   variables_to_include_with_text = c("start_date", "end_date", "wave_info", "version_name",  "gender", "age", "age_group", "ethnicity", "marital_status", "h_c__no_children",	"h_c__under_12",	"h_c__12_to_17",	"h_c__18_to_65",	"h_c__65_and_up", "annual_household_income",	"zip_code", "audience_type"),
#   grouping_vars = c("wave_info", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income", "audience_type")
# )
