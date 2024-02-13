# Ok, so for this function, I think the files should be processed entirely and
# then combined at the end. Thus at this level, we will include all of the purrr
# functions and file paths that will be used, but everything it will be calling
# will be done on one file at a time. Thus I just need to re-write the functions
# so that all of the actual processing will be done one at a time. Any joining
# of the data frames should then be done in this function. This will leave us
# with a nested function, but it should make more theoretical sense.

survey_version_name = "Member"
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

  # column_workbook_list <- purrr::map(path_to_column_workbook_list, ~{
  #   create_column_details_and_named_vectors_list(path_to_column_workbook = .x, name_of_column_details = "column_details")
  # })
  #
  # column_details_list <- purrr::map(column_workbook_list, ~{
  #   obtain_column_details_table(column_workbook_list = .x, name_of_column_details = "column_details")
  # })

  power_bi_overall_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_overall_qualitative.csv")

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


  survey_data_for_power_bi_list <- purrr::map(seq_along(survey_directory_paths), ~{

    survey_data_for_power_bi_single <- try({
      bkissell::process_survey_data_for_single_power_bi(
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



#
#   library(microbenchmark)
#   results <- microbenchmark(
#     create_table_single_mc_DEV(
#       df = df,
#       response_var_name = parameter_df$response_var_names[[1]],
#       grouping_var_name = parameter_df$grouping_vars[[1]]),
#     times = 50)

# results <- microbenchmark(
#   create_grouped_percentages_table(df, grouping_var_name, response_var_name),
#   times = 50)


#   # # Save the cleaned data to the power bi folder
#   # if(write_data){
#   #   purrr::walk2(survey_data_list, power_bi_clean_data_path_list, ~{
#   #     readr::write_csv(.x, .y)
#   #   })
#   #
#   #   Sys.sleep(1)
#   #
#   #   # Save another copy to the backup folder
#   #   purrr::walk2(survey_data_list, processed_data_clean_data_path_list, ~{
#   #     readr::write_csv(.x, .y)
#   #   })
#   # }
# #
# #   # Select the text variables in the df.
# #   text_survey_data <- purrr::map2(survey_data_list, variables_to_include_list, ~{
# #     select_text_survey_data(data = .x, variables_to_include = .y)
# #   })
# #
# #   if(write_data){
# #     # Save a copy of the text data as a backup
# #     purrr::walk2(text_survey_data, text_survey_data_path_list, ~{
# #       readr::write_csv(.x, .y)
# #     })
# #   }
# #
# #   # Shape the text data for power bi
# #   power_bi_text <- purrr::map2(text_survey_data, variables_to_include_list, ~{
# #     create_power_bi_text_data(text_survey_data =.x, variables_to_include = .y)
# #   })
# #
# #   if(write_data){
# #     # Save it to the powerbi folder
# #     purrr::walk2(power_bi_text, power_bi_text_path_list, ~{
# #       readr::write_csv(.x, .y)
# #     })
# #   }
# #
# #   if(include_selected_example == TRUE){
# #     read_and_add_param_to_column <- purrr::as_mapper(
# #       function(param_df) {
# #         purrr::map_df(seq_along(param_df$Var1), ~{
# #           selected_examples_df <- readxl::read_excel(param_df$Var2[[.x]], param_df$Var1[[.x]])
# #           selected_examples_df$response_var_used <- param_df$Var2[[.x]]
# #           selected_examples_df
# #         }, param_df)
# #       })
# #
# #     selected_examples_text_df_list <- purrr::map(parameters_for_example_read_list, ~{
# #       read_and_add_param_to_column(.x)
# #     })
# #   }
# #
# #   combined_df_selected_examples_text_df_list <- purrr::map2(text_survey_data, selected_examples_text_df_list, ~{
# #     .y %>%
# #       dplyr::left_join(.x, by = "respondent_id") %>%
# #       dplyr::select(!tidyselect::starts_with("text_"))
# #   })
# #
# #   if(write_data){
# #     purrr::walk2(combined_df_selected_examples_text_df_list, text_selected_example_text_survey_data_path_list, ~{
# #       data <- .x
# #       data$wave <- "Wave 1"
# #       data <- data %>% dplyr::select("wave", everything())
# #       readr::write_csv(data, .y)
# #     })
# #   }
#   ### Multiple Choice ##########################################################
#   power_bi_multiple_choice_parameters <- list(survey_data_list, path_to_column_workbook_list, grouping_vars_list, column_details_list, power_bi_mc_path_list)
#
#   power_bi_multiple_choice_list <- purrr::pmap(power_bi_multiple_choice_parameters, ~{
#     if(any(..4$type == "mc")){
#       power_bi_multiple_choice <- create_power_bi_data_mc(
#         df = ..1,
#         single_path_to_column_workbook = ..2,
#         grouping_vars = ..3)
#       if(write_data){
#         readr::write_csv(power_bi_multiple_choice, ..5)
#       }
#       power_bi_multiple_choice
#     }
#   })
#   ### Select All ###############################################################
#   power_bi_select_all_parameters <- list(survey_data_list, path_to_column_workbook_list, grouping_vars_list, column_details_list, power_bi_sa_path_list)
#
#   power_bi_select_all_list <- purrr::pmap(power_bi_select_all_parameters, ~{
#     if(any(..4$type == "sa")){
#       power_bi_select_all <- create_power_bi_data_sa(
#         df = ..1,
#         single_path_to_column_workbook = ..2,
#         grouping_vars = ..3)
#       if(write_data){
#         readr::write_csv(power_bi_select_all, ..5)
#       }
#       power_bi_select_all
#     }
#   })
#   ### Numeric ##################################################################
#   power_bi_num_parameters <- list(survey_data_list, path_to_column_workbook_list, grouping_vars_list, column_details_list, power_bi_descr_table_num_path_list)
#
#   power_bi_descr_table_num_list <- purrr::pmap(power_bi_num_parameters, ~{
#     if(any(..4$type == "sa")){
#       power_bi_descr_table_num <- create_power_bi_data_num(
#         df = ..1,
#         single_path_to_column_workbook = ..2,
#         grouping_vars = ..3)
#       if(write_data){
#         readr::write_csv(power_bi_descr_table_num, ..5)
#       }
#       power_bi_descr_table_num
#     }
#   })
#   ### Net Promoter #############################################################
#   power_bi_net_promoter_parameters <- list(survey_data_list, path_to_column_workbook_list, grouping_vars_list, column_details_list, power_bi_net_promoter_path_list)
#
#   power_bi_net_promoter_list <- purrr::pmap(power_bi_net_promoter_parameters, ~{
#     if(any(..4$column_names == "net_promoter")){
#       power_bi_net_promoter <- create_power_bi_data_nps(
#         df = ..1,
#         single_path_to_column_workbook = ..2,
#         grouping_vars = ..3)
#       if(write_data){
#         readr::write_csv(power_bi_net_promoter, ..5)
#       }
#       power_bi_net_promoter
#     }
#   })
#
#   ### Qualitative Coding #######################################################
#   power_bi_overall_qualitative_parameters <- list(survey_data_list, path_to_column_workbook_list, grouping_vars_list)
#
#   power_bi_overall_qualitative <- purrr::pmap( power_bi_overall_qualitative_parameters, ~{
#     create_power_bi_data_qualitative(
#       df = ..1,
#       single_path_to_column_workbook = ..2,
#       grouping_vars = ..3,
#       qualitative_type = "overall_coding",
#       identifier = "respondent_id"
#     )
#   })
#
#   column_workbook_list <- purrr::map(path_to_column_workbook_list, ~{
#     create_column_details_and_named_vectors_list(path_to_column_workbook = .x, name_of_column_details = "column_details")
#   })
#
#   column_details_list <- purrr::map(column_workbook_list, ~{
#     obtain_column_details_table(column_workbook_list = .x, name_of_column_details = "column_details")
#   })
#
#   power_bi_overall_qualitative <- purrr::pmap(list(power_bi_overall_qualitative, column_workbook_list, column_details_list), ~{
#     revised_table <- ..1[[1]]
#     revised_table <- fix_levels_and_order_in_table(
#       table = revised_table,
#       var_type = "grouping_var",
#       column_details = ..3,
#       column_workbook_list = ..2,
#       title_form = TRUE)
#
#     revised_table
#   })
#
#   power_bi_overall_qualitative <- purrr::map(power_bi_overall_qualitative, ~{
#     data_to_revise <- .x
#     data_to_revise <- data_to_revise %>%
#       dplyr::filter(!is.na(grouping_var_levels))
#     data_to_revise
#   })
#
#   if(write_data){
#     purrr::walk2(power_bi_overall_qualitative, version_names_from_list_analysis, ~{
#       power_bi_overall_qualitative_path <- paste0(home_dir, "/Analysis/Respondent Investigation/", .y, "/Power_BI_Deck/power_bi_overall_qualitative.csv")
#       readr::write_csv(.x, power_bi_overall_qualitative_path)
#     })
#   }
#
#   power_bi_break_down_qualitative <- purrr::pmap( power_bi_overall_qualitative_parameters, ~{
#     create_power_bi_data_qualitative(
#       df = ..1,
#       single_path_to_column_workbook = ..2,
#       grouping_vars = ..3,
#       qualitative_type = "break_down_coding",
#       identifier = "respondent_id"
#     )
#   })
#
#   power_bi_break_down_qualitative <- purrr::pmap(list(power_bi_break_down_qualitative, column_workbook_list, column_details_list), ~{
#     revised_table <- ..1[[1]]
#     revised_table <- fix_levels_and_order_in_table(
#       table = revised_table,
#       var_type = "grouping_var",
#       column_details = ..3,
#       column_workbook_list = ..2,
#       title_form = TRUE)
#
#     revised_table
#   })
#
#   power_bi_break_down_qualitative <- purrr::map(power_bi_break_down_qualitative, ~{
#     data_to_revise <- .x
#     data_to_revise <- data_to_revise %>%
#       dplyr::filter(!is.na(grouping_var_levels))
#     data_to_revise
#   })
#
#   if(write_data){
#     purrr::walk2(power_bi_break_down_qualitative, version_names_from_list_analysis, ~{
#       power_bi_break_down_qualitative_path <- paste0(home_dir, "/Analysis/Respondent Investigation/", .y, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")
#       readr::write_csv(.x, power_bi_break_down_qualitative_path)
#     })
#   }
}
