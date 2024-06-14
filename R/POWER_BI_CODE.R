
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
grouping_vars = c("wave_info", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income", "audience_type",
                  "net_promoter",
                  "length_of_membership",	"number_of_visits_last_year",
                  "number_of_planned_visits",	"likelihood_of_renewing",
                  "donated_to_sdzwa_in_past", "confident_donation_will_help",
                  "monthly_donor",	"support_sdzwa_again")

use_the_question_text_for_variable_name = TRUE

FULL_Processing_Code <- function(
  survey_version_name,
  should_create_nonexistant_dirs,
  survey_monkey_used,
  wave_names,
  storage_platform,
  storage_platform_name,
  group_dir_name,
  jobs_folder_name,
  project_year,
  project_folder_name,
  convert_numeric_age_to_age_group,
  survey_file_ext,
  survey_datetime_format_pattern,
  name_of_column_details,
  write_data,
  variables_to_include_with_text,
  grouping_vars,
  use_the_question_text_for_variable_name
) {

  # Use the computer information to create the working directory according to
  # the information included as a parameter. This will be used instead of using
  # projects.
  working_directory_path <- bkissell::set_project_working_directory(
    storage_platform = storage_platform,
    storage_platform_name = storage_platform_name,
    group_dir_name = group_dir_name,
    jobs_folder_name = jobs_folder_name,
    project_year = project_year,
    project_folder_name = project_folder_name
  )

  # Check that the traditional project directories exist.
  # There is an option to create them if they do not exist
  traditional_project_dir_list <- bkissell::create_traditional_project_dir_list(should_create_nonexistant_dirs = should_create_nonexistant_dirs)

  # Check that the directories related to surveys exist according to the
  # included parameters. There is an option to create them if they do not exist
  survey_related_dir_list <- bkissell::create_survey_related_dir_list(
    survey_version_name = survey_version_name,
    survey_monkey_used = survey_monkey_used,
    wave_names = wave_names,
    should_create_nonexistant_dirs = should_create_nonexistant_dirs
  )

  # Obtain the names where the directories for the different survey versions
  survey_directory_path_names <- names(survey_related_dir_list)[stringr::str_detect(names(survey_related_dir_list), "^p_path_dc_sm_svn_wave_names_")]

  # Obtain the actual paths for the survey version directories
  survey_directory_paths <- survey_related_dir_list[survey_directory_path_names]

  # Obtain the paths where the column names documents should be
  column_names_paths <- purrr::map(survey_directory_paths, ~{
    file.path(.x, paste0(basename(.x), "_column_names.xlsx"))
  })

  names(column_names_paths) <- NULL

  # Obtain the paths that contain the data where the text has gone through custom spell check process
  spellcheck_column_paths <- purrr::map(survey_directory_paths, ~paste0(.x, "/spellchecked_text_columns.xlsx"))

  # Create the path for the cleaned survey data that will be stored in the power bi directory
  power_bi_clean_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/power_bi_deck/clean_data.csv")

  # Create the path for the cleaned survey data that will be stored in the processed clean data folder
  processed_data_clean_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_data/PROCESSED_", snakecase::to_snake_case(survey_version_name), "_data_", bkissell::create_time_chr_string_for_file_names("%Y%m%d_%H%M"), ".csv")

  # Create path for the text data from the survey
  text_survey_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_text/TEXT_PROCESSED_", snakecase::to_snake_case(survey_version_name), "_", bkissell::create_time_chr_string_for_file_names("%Y%m%d_%H%M"), ".csv")

  # Create the path for the text of the selected responses that we are wanting to highlight, which will be stored in the power bi folder
  power_bi_text_selected_example_text_survey_data_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_selected_example_text.csv")

  # Create the path for the text data that will be stored in the power bi folder
  power_bi_text_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_text_survey_data.csv")

  # Create the path for the selected example text survey data
  text_selected_example_text_survey_data_path <- paste0("Data Collection/survey_monkey_data/", survey_version_name, "/", wave_names, "/selected_example_text.xlsx")

  # Create the path for the mc power bi
  power_bi_mc_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_multiple_choice.csv")

  # Create the path for the power bi sa
  power_bi_sa_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_select_all.csv")

  # Create the path for the power bi desc table
  power_bi_descr_table_num_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_descr_table_num.csv")

  # Create the path for the net promoter
  power_bi_net_promoter_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_net_promoter.csv")

  # Create the path for the quality coding
  qualitative_coding_data_path_list <- paste0(survey_directory_paths, "/qualitative_coding_data.xlsx") %>% as.list()

  # Create the path for the power bi overall qual
  power_bi_overall_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_overall_qualitative.csv")

  # Create the path for the power bi breakdown qual
  power_bi_break_down_qualitative_path <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")

  # Create the path for the power bi breakdown list qual
  power_bi_break_down_qualitative_path_list <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/Power_BI_Deck/power_bi_break_down_qualitative.csv")

  # Get the example text repsonse sheets and put them together as a df in the env
  text_selected_example_response_vars <- purrr::map(text_selected_example_text_survey_data_path, ~ readxl::excel_sheets(.x))

  # Create the parameters df
  parameters_for_example_read_list <- purrr::map2(text_selected_example_response_vars, text_selected_example_text_survey_data_path, ~{
    parameters_for_example_read <- data.frame(Var1 = .x)
    parameters_for_example_read$Var2 <- .y
    parameters_for_example_read
  })


  # Section 1b - Set-up Column Workbook Lists --------------------------------------------
  # Create the column details list

  # Loop through all versions of the column information folder
  column_workbook_lists <- purrr::map(column_names_paths, ~{
    # For an individual path, read in and process the file
    column_workbook_list_attempt <-
      try({
        bkissell::create_column_details_and_named_vectors_list(
          path_to_column_workbook = .x,
          name_of_column_details = name_of_column_details)},
        silent = TRUE
      )

    # Return the read in data only if there was not an error
    if(class(column_workbook_list_attempt) != "try-error") {
      return(column_workbook_list_attempt)
    } else {
      # If there is an error, return an error
      stop(paste0("'create_column_details_and_named_vectors_list' had an error
           when run with ", .x, ". Look at function
           'create_multiple_column_workbook_lists'"))
    }
  })

  survey_datetime_format_pattern = "_[0-9]{8}_[0-9]{4}"
  data_collection_survey_directory_paths = survey_directory_paths
  convert_numeric_age_to_age_group = TRUE

  setwd(working_directory_path)
  #
  survey_data_for_power_bi_df_list <- purrr::map(seq_along(data_collection_survey_directory_paths), ~{
    iteration <- .x

    survey_directory_path_pb = data_collection_survey_directory_paths[[iteration]]
    column_workbook_list = column_workbook_lists[[iteration]]
    spellcheck_column_path_pb = spellcheck_column_paths[[iteration]]

    # Obtain the column details
    column_details <- column_workbook_list[[name_of_column_details]]

    survey_data_for_power_bi_single <- try({

      # Read in the file
      survey_data_for_power_bi_single <- bkissell::read_survey_data_for_power_bi_single(
        survey_directory_path = survey_directory_path_pb,
        file_ext = survey_file_ext, format_pattern = survey_datetime_format_pattern,
        survey_version_name = survey_version_name,
        column_workbook_list = column_workbook_list
      ) %>% suppressWarnings()
    })
  })

  survey_data_for_power_bi_df_list <- purrr::map(seq_along(survey_data_for_power_bi_df_list), ~{
    iteration <- .x
    survey_data_for_power_bi_single <- survey_data_for_power_bi_df_list[[iteration]]

    # Convert Age
    if(convert_numeric_age_to_age_group == TRUE) {
      survey_data_for_power_bi_single$age_group <- bkissell::convert_age_to_generation(age_numeric = survey_data_for_power_bi_single$age, year_of_data_collection = project_year)
    }

    return(survey_data_for_power_bi_single)
  })


  survey_data_for_power_bi_df_list <- purrr::map(seq_along(survey_data_for_power_bi_df_list), ~{
    iteration <- .x

    survey_data_for_power_bi_single <- survey_data_for_power_bi_df_list[[iteration]]
    spellcheck_column_path_pb = spellcheck_column_paths[[iteration]]

    # Fix Spell checked columns
    spellcheck_it <- try(bkissell::replace_spelling_with_excel_workbook(df = survey_data_for_power_bi_single, spellchecked_path = spellcheck_column_path_pb))

    if(all(class(spellcheck_it) != "try-error")) {
      survey_data_for_power_bi_single <- spellcheck_it
    }

    survey_data_for_power_bi_single
  })


  survey_data_for_power_bi_df_list <- purrr::map(seq_along(survey_data_for_power_bi_df_list), ~{
    iteration <- .x
    survey_data_for_power_bi_single <- survey_data_for_power_bi_df_list[[iteration]]
    column_workbook_list = column_workbook_lists[[iteration]]
    column_details <- column_workbook_list[[name_of_column_details]]

    # Process the net promotor score
    survey_data_for_power_bi_single <- bkissell::capwcw_process_net_promoter(data = survey_data_for_power_bi_single, column_details)

    return(survey_data_for_power_bi_single)
  })

      # column_workbook_list = column_workbook_list_pb

      # # Process the Multiple Choice
      # survey_data_for_power_bi_single <- bkissell::convert_vars_to_factors_with_workbook(
      #   data = survey_data_for_power_bi_single,
      #   column_workbook_list = column_workbook_list_pb,
      #   name_of_column_details = name_of_column_details)

  survey_data_for_power_bi_df_list <- purrr::map(seq_along(survey_data_for_power_bi_df_list), ~{
    iteration <- .x
    survey_data_for_power_bi_single <- survey_data_for_power_bi_df_list[[iteration]]
    column_workbook_list = column_workbook_lists[[iteration]]
    column_details <- column_workbook_list[[name_of_column_details]]

    df_to_use <- survey_data_for_power_bi_single

    # Get the names of the mc variable
    multiple_choice_included <- "mc" %in% column_details$type

    # If mc is included
    if(multiple_choice_included == TRUE) {
      # Get the column details for multiple choice
      column_details_question_type_df <- dplyr::filter(column_details, .data[["type"]] == "mc")

      multiple_choice_names <- column_details_question_type_df$column_names
      multiple_choice_label_names <- column_details_question_type_df$label_info

      df_for_mc_processing <- df_to_use

      for(item_name_index in seq_along(multiple_choice_names)) {
        iteration_1 <- item_name_index
        mc_choice_name <- multiple_choice_names[[iteration_1]]

        mc_cd_levels_name <- multiple_choice_label_names[[iteration_1]]

        if(mc_choice_name %in% colnames(df_for_mc_processing)){
          # print(mc_choice_name %in% colnames(df_for_mc_processing))
          var_from_df_for_mc_processing <- factor(
            x =  df_for_mc_processing[[mc_choice_name]],
            levels = column_workbook_list[[mc_cd_levels_name]],
            labels = names(column_workbook_list[[mc_cd_levels_name]]),
            ordered = TRUE
          )

          df_for_mc_processing <- df_for_mc_processing %>%
            dplyr::mutate({{mc_choice_name}} := var_from_df_for_mc_processing)
        }
      }

      df_to_use <- df_for_mc_processing
    }

    return(df_to_use)
  })



  survey_data_for_power_bi_df_list <- purrr::map(seq_along(survey_data_for_power_bi_df_list), ~{
    iteration <- .x
    survey_data_for_power_bi_single <- survey_data_for_power_bi_df_list[[iteration]]
    column_workbook_list = column_workbook_lists[[iteration]]
    column_details <- column_workbook_list[[name_of_column_details]]

    df_to_use <- survey_data_for_power_bi_single


    # Detect sa questions with text responses
    select_all_text_vars <- column_details %>%
      dplyr::filter(type == "sa_other") %>%
      dplyr::pull(column_names)

    # If there are sa other variables
    if(length(select_all_text_vars) > 0) {
      # Copy them under a new name
      for(text_var in select_all_text_vars) {
        df_to_use[[paste0("text_", text_var)]] <- df_to_use[[text_var]]
      }
    }

    # Extract the names of the select all questions
    SELECT_ALL_var_names <- column_details %>%
      dplyr::filter(stringr::str_detect(.data[["type"]], "^sa")) %>%
      dplyr::pull(column_names)

    SELECT_ALL_var_labels <- column_details %>%
      dplyr::filter(stringr::str_detect(.data[["type"]], "^sa")) %>%
      dplyr::pull(label_info)

    unique_SELECT_ALL_var_labels <- unique(SELECT_ALL_var_labels)

    # Loop through all select all questions and process them
    for(sa_var_label in unique_SELECT_ALL_var_labels) {

      # Obtain the variable names for this question
      list_of_variable_names <- df_to_use %>%
        dplyr::select(tidyselect::starts_with(sa_var_label)) %>% colnames()

      # Check if all columns are missing data
      is_all_column_data_missing_for_row <- bkissell::check_if_all_are_missing_from_row(df_to_use, list_of_variable_names)

      for(variable_name_1 in list_of_variable_names) {
        # Get the index for the var
        index_for_var_to_change <- which(colnames(df_to_use) == variable_name_1)

        # Select the variable
        vector_to_change <- df_to_use[,index_for_var_to_change][[1]]

        # Convert those that are missing everything to NA, but change all other NAs to zero
        df_to_use[,index_for_var_to_change] <- bkissell::process_a_select_all_response_as_ones_and_zeros(
          is_all_column_data_missing_for_row,
          df_to_use[[variable_name_1]]
        )
      }
    }

    return(df_to_use)
  })


  survey_data_for_power_bi_df_list <- purrr::map(seq_along(survey_data_for_power_bi_df_list), ~{
    iteration <- .x
    survey_data_for_power_bi_single <- survey_data_for_power_bi_df_list[[iteration]]
    column_workbook_list = column_workbook_lists[[iteration]]
    column_details <- column_workbook_list[[name_of_column_details]]
    data_used <- survey_data_for_power_bi_single

    numeric_column_details <- column_details %>%
      dplyr::filter(!is.na(.data[["scale_names"]]) | .data$type == "numeric")

    numeric_column_details_names <- numeric_column_details %>% dplyr::pull(column_names)

    # See if numeric type should be run
    numeric_included <- "numeric" %in% column_details$type | any(!is.na(column_details$scale_names))

    if(numeric_included == TRUE) {

      purely_numeric_vars <- numeric_column_details %>%
        dplyr::filter(is.na(.data[["label_info"]])) %>%
        dplyr::pull(column_names)

      data_used <- data_used %>%
        dplyr::mutate(
          dplyr::across(
            tidyselect::any_of(numeric_column_details_names),
            as.numeric,
            # This provides the naming convention to use
            .names = "NUM__{.col}")
        )

      variables_needs_to_be_reverse_coded <- numeric_column_details %>%
        dplyr::filter(stringr::str_detect(.data[["scale_names"]], "_r$")) %>%
        dplyr::pull(column_names)

      variables_needs_to_be_reverse_coded_labels <- numeric_column_details %>%
        dplyr::filter(stringr::str_detect(.data[["scale_names"]], "_r$")) %>%
        dplyr::pull(label_info)

      for(i in variables_needs_to_be_reverse_coded) {
        label_for_revers_coded_var <- paste0("NUM__REVERSE_CODED__", i)
        label_for_original_numeric <- paste0("NUM__", i)
        fix_label_for_orig <- paste0 <- paste0("ORIG_NUM__", i)
        length_levels_for_factor <- length(levels(data_used[[i]]))

        numeric_variable <- as.numeric(data_used[[i]])

        reordered_numeric_variable <- (length_levels_for_factor + 1) - numeric_variable

        data_used[fix_label_for_orig] <- data_used[label_for_original_numeric]
        data_used <- data_used %>% dplyr::select(-c(all_of(label_for_original_numeric)))
        data_used[label_for_revers_coded_var] <- reordered_numeric_variable
      }

      scales_details <- numeric_column_details %>%
        dplyr::filter(!is.na(.data[["scale_names"]]))

      scales_details$var_names_for_scaling <- dplyr::case_when(
        stringr::str_detect(scales_details$scale_names, "_r$") ~ paste0("NUM__REVERSE_CODED__", scales_details$column_names),
        !is.na(scales_details$scale_names) ~ paste0("NUM__", scales_details$column_names)
      )

      scales_details$final_scale_names <- stringr::str_replace(scales_details$scale_names, "_r$","")
      unique_scale_names_to_use <- unique(scales_details$final_scale_names)

      for(scale_name_to_use in unique_scale_names_to_use) {
        label_for_the_score <- paste0("AVERAGE_SCORE__", snakecase::to_snake_case(scale_name_to_use))
        final_scale_variable_names <- scales_details %>%
          dplyr::filter(.data[["final_scale_names"]] == scale_name_to_use) %>%
          dplyr::pull(var_names_for_scaling)

        data_used[label_for_the_score] <- data_used %>%
          dplyr::select(starts_with(final_scale_variable_names)) %>%
          rowMeans()
      }

      orig_num_var_names <- data_used %>%
        dplyr::select(
          starts_with("ORIG_NUM")
        ) %>% colnames()

      if(length(orig_num_var_names) > 0) {
        for(n_orig_num_name in seq_along(orig_num_var_names)){
          iteration <- n_orig_num_name
          converted_label <- orig_num_var_names[[iteration]]
          add_label <- stringr::str_replace(converted_label, "ORIG_", "")
          data_used[[add_label]] <- data_used[[converted_label]]
          data_used <- data_used %>% dplyr::select(-all_of(converted_label))
        }
      }
    }

    return(data_used)
  })


  survey_data_for_power_bi_df_list <- purrr::map(seq_along(survey_data_for_power_bi_df_list), ~{
    iteration <- .x
    survey_data_for_power_bi_single <- survey_data_for_power_bi_df_list[[iteration]]

    survey_data_for_power_bi_single <- survey_data_for_power_bi_single %>%
      dplyr::filter(!is.na(net_promoter))

    return(survey_data_for_power_bi_single)
  })



  survey_data_for_power_bi_df <- survey_data_for_power_bi_df_list %>%
    suppressWarnings() %>%
    purrr::reduce(dplyr::bind_rows)

  survey_data_for_power_bi_df <- survey_data_for_power_bi_df %>%
    dplyr::filter(!is.na(as.numeric(zip_code)))

  ##############################################################################
  # Save the cleaned data to the power bi folder
  if(write_data){
    readr::write_csv(survey_data_for_power_bi_df, power_bi_clean_data_path)
    Sys.sleep(1)
    readr::write_csv(survey_data_for_power_bi_df, processed_data_clean_data_path)
  }

  vars_to_include_with_text = c(variables_to_include_with_text, "NUM_net_promoter")

  # Process the text data

  # Filter for needed variables
  text_survey_data <- survey_data_for_power_bi_df %>%
    dplyr::select(
      "RID",
      tidyselect::any_of(vars_to_include_with_text),
      starts_with("text_")
    )

  #THIS NEEDS TO BE THE ANALYSIS FOLDER
  survey_dir_paths_main <- purrr::map_chr(survey_dir_paths_list, dirname) %>% unique()
  survey_dir_names_list <- purrr::map(survey_dir_paths_list, basename)

  for(wave_name in survey_dir_names_list){
    text_survey_data_per_wave <- text_survey_data %>%
      dplyr::filter(.data[["wave_info"]] == wave_name)

    path_for_text_survey_data_per_wave <- paste0("Analysis/Respondent Investigation/", survey_version_name, "/processed_text/", wave_name, "/TEXT_PROCESSED_", bkissell::combine_file_string_with_time(wave_name), ".csv")

    if(write_data){
      readr::write_csv(text_survey_data_per_wave, path_for_text_survey_data_per_wave)
    }
  }

  # Shape and filter the textual data
  power_bi_text <- text_survey_data %>%
    tidyr::pivot_longer(
      cols = -c("RID", tidyselect::any_of(vars_to_include_with_text)),
      names_to = "response_var_used",
      names_prefix = "text_",
      values_to = "textual_responses"
    ) %>%
    dplyr::filter(!is.na(.data[["textual_responses"]]))

  # Remove the "'" as the symbol is entered incorrectly in excel
  power_bi_text$textual_responses <- stringr::str_replace_all(power_bi_text$textual_responses, "'", "")

  # power_bi_text <- power_bi_text %>%
  #   dplyr::select(
  #     "wave" = "wave_info",
  #     "respondent_id" = "RID",
  #     "start_date",
  #     "end_date",
  #     "h_c__no_children",
  #     "h_c__under_12",
  #     "h_c__12_to_17",
  #     "h_c__18_to_65",
  #     "h_c__65_and_up",
  #     "zip_code",
  #     "gender",
  #     "age_group",
  #     "ethnicity",
  #     "marital_status",
  #     "annual_household_income",
  #     "net_promoter",
  #     "length_of_membership",
  #     "number_of_visits_last_year",
  #     "number_of_planned_visits",
  #     "likelihood_of_renewing",
  #     "donated_to_sdzwa_in_past",
  #     "study_version" = "version_name",
  #     "response_var_used",
  #     "textual_responses"
  #   )

  power_bi_text <- power_bi_text %>%
    dplyr::select(
      "RID",
      "start_date",
      "end_date",
      "wave_info",
      "version_name",
      "gender",
      "age",
      "age_group",
      "ethnicity",
      "marital_status",
      "h_c__no_children",
      "h_c__under_12",
      "h_c__12_to_17",
      "h_c__18_to_65",
      "h_c__65_and_up",
      "annual_household_income",
      "zip_code",
      "audience_type",
      "response_var_used",
      "textual_responses"

    )

  power_bi_text$zip_code <- as.numeric(power_bi_text$zip_code)


  if(write_data){
    # Save it to the powerbi folder
    readr::write_csv(power_bi_text, power_bi_text_path)
  }

  combined_df_selected_examples_text_df_combined <- purrr::map2_df(text_selected_example_text_survey_data_path, parameters_for_example_read_list, ~{

    if(file.exists(.x)){

      read_and_add_param_to_column <- purrr::as_mapper(
        function(param_df) {
          purrr::map_df(seq_along(param_df$Var1), ~{
            iteration <- .x
            selected_examples_df <- readxl::read_excel(param_df$Var2[[iteration]], param_df$Var1[[iteration]])
            selected_examples_df$response_var_used <- param_df$Var1[[iteration]]
            selected_examples_df
          }, param_df)
        })

      selected_examples_text_df <- read_and_add_param_to_column(param_df = .y)
      selected_examples_text_df$RID <- as.numeric(selected_examples_text_df$RID)

      text_survey_data <- text_survey_data
      text_survey_data$RID <- as.numeric(text_survey_data$RID)

      combined_df_selected_examples_text_df <- selected_examples_text_df %>%
        dplyr::left_join(text_survey_data, by = "RID") %>%
        dplyr::select(!tidyselect::starts_with("text_"))

      combined_df_selected_examples_text_df$wave <- combined_df_selected_examples_text_df$wave_info

      selected_examples_text_df$RID <- as.character(selected_examples_text_df$RID)
      text_survey_data$RID <- as.character(text_survey_data$RID)
      combined_df_selected_examples_text_df$RID <- as.character(combined_df_selected_examples_text_df$RID)

      combined_df_selected_examples_text_df <- combined_df_selected_examples_text_df %>%
        dplyr::select(
          dplyr::any_of(
            c("wave", "RID",	"Text",	"Category",	"response_var_used",	"start_date",
              "end_date",	"h_c__no_children",	"h_c__under_12",	"h_c__12_to_17",
              "h_c__18_to_65",	"h_c__65_and_up",	"zip_code",	"gender",	"age_group",
              "ethnicity",	"marital_status",	"annual_household_income",
              "net_promoter",	"length_of_membership",	"number_of_visits_last_year",
              "number_of_planned_visits",	"likelihood_of_renewing",
              "donated_to_sdzwa_in_past", "confident_donation_will_help",
              "monthly_donor",	"support_sdzwa_again","study_version"
            )
          )
        )

      combined_df_selected_examples_text_df
    }


  })

  combined_df_selected_examples_text_df_combined <- combined_df_selected_examples_text_df_combined %>%
    dplyr::filter(!is.na(wave))

  if(write_data){
    readr::write_csv(combined_df_selected_examples_text_df_combined, power_bi_text_selected_example_text_survey_data_path)
  }

  grouping_vars <- grouping_vars[grouping_vars %in% colnames(survey_data_for_power_bi_df)]

  column_workbook_lists_single <- column_workbook_lists[[1]]

  ##############################################################################
  ###################################### Multiple Choice #######################
  ##############################################################################

  # my_current_env <- bkissell::create_power_bi_multiple_choice(my_env = my_current_env)


  if(any(column_workbook_lists_single[[name_of_column_details]]$type == "mc")) {

    # Bring in the column details
    column_details <-  column_workbook_lists_single[[name_of_column_details]]



    # Create a list of variables that includes those not in the details sheet
    grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
      x_var <- .x
      if(!(x_var %in% column_details$column_names)){
        var <- x_var
      } else {
        var <- column_details %>% dplyr::filter(column_names == x_var) %>% dplyr::pull(label_info)
      }
      var
    })


    # Obtain the grouping var order
    grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

    # Get the multiple choice variables df
    multiple_choice_df <- bkissell::extract_column_details_question_type_df(column_details_table = column_details, question_type_indicator = "mc")

    # get the mc column names
    response_var_names <- multiple_choice_df$column_names

    # Get the mc vector names
    response_var_used_vector_names <- multiple_choice_df$label_info

    # Get the response var order
    response_var_used_order <- factor(response_var_used_vector_names, unique(response_var_used_vector_names)) %>% as.numeric()

    # Create df with response var data
    rv_names_and_order_df <- data.frame(
      response_var_names = response_var_names,
      response_var_used_order = response_var_used_order
    )

    # Create df with groupiong var data
    gv_names_and_order_df <- data.frame(
      grouping_vars = grouping_vars,
      grouping_var_used_order = grouping_var_used_order
    )

    # Put together all combinations
    parameter_df <- expand.grid(response_var_names, grouping_vars)

    # Rename the columns
    colnames(parameter_df) <- c("response_var_names", "grouping_vars")

    # Join the tables creating the parameters df
    parameter_df <- parameter_df %>%
      dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
      dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

    # Make a function to obtain the mc calcualtions
    perform_calc_create_table_single_mc <- function(df, WaveName = "All Waves", parameter_df){
      # Obtain the number of iterations
      n_iterations <- length(parameter_df$response_var_names)
      # Create progress bar
      pb <- progress::progress_bar$new(total = n_iterations, format = glue::glue("Performing Calculations for Multiple Choice PBT for Wave: {WaveName}  [:bar] :percent eta: :eta"))
      # Loop through every possibility from the parameter df
      purrr::map_df(seq_along(parameter_df$response_var_names), ~{
        iteration <- .x
        print(iteration)
        # Obtain Grouped percentages
        t <- bkissell::create_grouped_percentages_table(
          df = df,
          grouping_var_name = parameter_df$grouping_vars[[iteration]],
          response_var_name = parameter_df$response_var_names[[iteration]],
          type = NULL,
          group_n_table = NULL)

        # Add additional needed info to df
        t$response_var_used <- parameter_df$response_var_names[[iteration]]
        t$response_var_used_order <- parameter_df$response_var_used_order[[iteration]]
        t$grouping_var_used <- parameter_df$grouping_vars[[iteration]]
        t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[iteration]]
        t$wave <- WaveName

        # Indicate progress
        pb$tick()
        # Return the table that was made
        t
      })
    }

    # Make a df for every wave
    split_wave_dfs <- split(survey_data_for_power_bi_df, as.factor(survey_data_for_power_bi_df$wave_info))

    # Add the df with all data to that list
    all_dfs <- append(list("All Waves" = survey_data_for_power_bi_df), split_wave_dfs)

    # Obtain the calculations for every df in the list
    wave_tables_df <- purrr::map2_df(all_dfs, as.list(names(all_dfs)), ~{
      perform_calc_create_table_single_mc(df = .x, WaveName = .y, parameter_df = parameter_df)
    }, parameter_df)

    # Temp create these
    wave_tables_df$grouping_var_order <- NA
    wave_tables_df$response_var_order <- NA

    # Get rid of any nas
    wave_tables_df <- wave_tables_df %>%
      dplyr::filter(!is.na(response_var_levels)) %>%
      dplyr::filter(!is.na(grouping_var_levels))

    #Obtain the order information lookup table
    order_information_lookup_table <- bkissell::create_order_information_lookup_table(column_workbook_lists_single, name_of_column_details)

    # Fix the order numbers for response vars
    wave_tables_df <- bkissell::adjust_order_information_lookup_table_per_type(
      table = wave_tables_df,
      order_information_lookup_table = order_information_lookup_table,
      type = "response_var_")

    # Fix the order numbers for grouping vars
    wave_tables_df <- bkissell::adjust_order_information_lookup_table_per_type(wave_tables_df, order_information_lookup_table, type = "grouping_var_")
    # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "response_var", column_details = column_details, column_workbook_list = column_workbook_list)
    # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

    wave_tables_df$grouping_var_used  <- stringr::str_replace_all(wave_tables_df$grouping_var_used, "_", " ") %>% stringr::str_to_title()

    wave_tables_df$response_var_used  <- stringr::str_replace_all(wave_tables_df$response_var_used, "_", " ") %>% stringr::str_to_title()

    wave_tables_df$percentage[is.nan(wave_tables_df$percentage)] <- 0

    wave_tables_df <- wave_tables_df %>% dplyr::select(
      "wave", "grouping_var_levels", "response_var_levels", "grouping_var_order",
      "response_var_order", "counts", "group_n", "percentage",
      "grouping_var_used", "grouping_var_used_order", "response_var_used", "response_var_used_order", "overall_sample_size_for_response_var")

    power_bi_multiple_choice <- wave_tables_df

    if(write_data){
      readr::write_csv(power_bi_multiple_choice, power_bi_mc_path)
    }
  }

  ##############################################################################
  ###################################### Select All ############################
  ##############################################################################

  # Select All

  check_for_select_all_vars <- stringr::str_detect(column_workbook_lists_single[[name_of_column_details]]$type, "^sa")

  if(any(check_for_select_all_vars)){

    # Bring in the column details
    column_details <- column_workbook_lists_single[[name_of_column_details]]

    # Create a list of variables that includes those not in the details sheet
    grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
      if(!(.x %in% column_details$column_names)){
        var <- .x
      } else {
        var <- column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info)
      }
      var
      })

    # grouping_var_used_vector_names <- grouping_var_used_vector_names[grouping_var_used_vector_names %in% colnames(survey_data_for_power_bi_df)]

    grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

    select_all_df <- bkissell::extract_column_details_question_type_df(column_details_table = column_details, question_type_indicator = "sa")

    response_var_names <- select_all_df$column_names %>% stringr::str_extract( "^.+__") %>% unique() %>% stringr::str_replace("__$", "")

    response_var_used_order <- factor(response_var_names, unique(response_var_names)) %>% as.numeric()

    rv_names_and_order_df <- data.frame(
      response_var_names = response_var_names,
      response_var_used_order = response_var_used_order
    )

    gv_names_and_order_df <- data.frame(
      grouping_vars = grouping_vars,
      grouping_var_used_order = grouping_var_used_order
    )

    parameter_df <- expand.grid(response_var_names, grouping_vars)

    colnames(parameter_df) <- c("response_var_names", "grouping_vars")

    parameter_df <- parameter_df %>%
      dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
      dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")


      perform_calc_create_table_single_sa <- function(df, WaveName = "All Waves", parameter_df, column_workbook){
        # # Obtain the number of iterations
        # n_iterations <- length(parameter_df$response_var_names)
        # # Create progress bar
        # pb <- progress::progress_bar$new(total = n_iterations, format = glue::glue("Performing Calculations for Select All PBT for Wave: {WaveName}  [:bar] :percent eta: :eta"))
        # # Loop through every possibility from the parameter df
        # table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{
        #
        #   iteration <- .x
        #
        #   list_of_variable_names <- df %>% dplyr::select(
        #     tidyselect::starts_with(parameter_df$response_var_names[[iteration]])) %>%
        #     colnames()
        #
        #   df_filtered <- df %>%
        #     dplyr::select(parameter_df$grouping_vars[[iteration]], all_of(list_of_variable_names)) %>%
        #     na.omit()
        #
        #   df_filtered[[parameter_df$grouping_vars[[iteration]]]] <- as.character(df_filtered[[parameter_df$grouping_vars[[iteration]]]])
        #
        #   df_long <- df_filtered %>%
        #     tidyr::pivot_longer(cols = - parameter_df$grouping_vars[[iteration]], names_to = parameter_df$response_var_names[[iteration]], values_to = "values") %>%
        #     dplyr::filter(values == 1) %>% dplyr::select(-values)
        #
        #   new_response_var_labels <- column_workbook_list[[parameter_df$response_var_names[[iteration]]]]
        #
        #   df_long[[parameter_df$response_var_names[[iteration]]]] <- factor(df_long[[parameter_df$response_var_names[[iteration]]]], new_response_var_labels, names(new_response_var_labels)) %>% as.character()
        #
        #   # group_n_table <- df_filtered |>
        #   #   dplyr::group_by(.data[[parameter_df$grouping_vars[[iteration]]]]) |>
        #   #   dplyr::summarise(group_n = n())
        #
        #   if(nrow(df_long) > 0 ) {
        #     # Obtain Grouped percentages
        #     t <- bkissell::create_grouped_percentages_table(
        #       df = df_long,
        #       grouping_var_name = parameter_df$grouping_vars[[iteration]],
        #       response_var_name = parameter_df$response_var_names[[iteration]],
        #       type = "sa",
        #       group_n_table = NULL)
        #
        #     # Add additional needed info to df
        #     t$response_var_used <- parameter_df$response_var_names[[iteration]]
        #     t$response_var_used_order <- parameter_df$response_var_used_order[[iteration]]
        #     t$grouping_var_used <- parameter_df$grouping_vars[[iteration]]
        #     t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[iteration]]
        #     t$wave <- WaveName
        #
        #     # Indicate progress
        #     pb$tick()
        #     # Return the table that was made
        #     t
        #   } else {
        #     pb$tick()
        #     return(data.frame())
        #   }
        #
        # })
        # return(table)
      }

    # Make a df for every wave
    split_wave_dfs <- split(survey_data_for_power_bi_df, as.factor(survey_data_for_power_bi_df$wave_info))

    # Add the df with all data to that list
    all_dfs <- append(list("All Waves" = survey_data_for_power_bi_df), split_wave_dfs)

    # Obtain the calculations for every df in the list
    wave_tables_df <- purrr::map2_df(all_dfs, as.list(names(all_dfs)), ~{
      df <- .x
      WaveName <- .y

      # Obtain the number of iterations
      n_iterations <- length(parameter_df$response_var_names)
      # Create progress bar
      pb <- progress::progress_bar$new(total = n_iterations, format = glue::glue("Performing Calculations for Select All PBT for Wave: {WaveName}  [:bar] :percent eta: :eta"))
      # Loop through every possibility from the parameter df
      table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{

        iteration <- .x

        list_of_variable_names <- df %>% dplyr::select(
          tidyselect::starts_with(parameter_df$response_var_names[[iteration]])) %>%
          colnames()

        df_filtered <- df %>%
          dplyr::select(parameter_df$grouping_vars[[iteration]], all_of(list_of_variable_names)) %>%
          na.omit()

        df_filtered[[parameter_df$grouping_vars[[iteration]]]] <- as.character(df_filtered[[parameter_df$grouping_vars[[iteration]]]])

        group_ns <- df_filtered %>%
          dplyr::group_by(.data[[parameter_df$grouping_vars[[iteration]]]]) %>%
          dplyr::summarize(n = n())

        all_group_ns <- data.frame(
          n = sum(group_ns$n)
        )

        all_group_ns[[parameter_df$grouping_vars[[iteration]]]] <- "All"
        all_group_ns <- all_group_ns %>%
          select(parameter_df$grouping_vars[[iteration]], n)

        group_ns <- group_ns %>%
          rbind(all_group_ns)


        df_long <- df_filtered %>%
          tidyr::pivot_longer(cols = - parameter_df$grouping_vars[[iteration]], names_to = parameter_df$response_var_names[[iteration]], values_to = "values") %>%
          dplyr::filter(values == 1) %>% dplyr::select(-values)

        new_response_var_labels <- column_workbook_lists_single[[parameter_df$response_var_names[[iteration]]]]

        df_long[[parameter_df$response_var_names[[iteration]]]] <- factor(df_long[[parameter_df$response_var_names[[iteration]]]], new_response_var_labels, names(new_response_var_labels)) %>% as.character()

        # group_n_table <- df_filtered |>
        #   dplyr::group_by(.data[[parameter_df$grouping_vars[[iteration]]]]) |>
        #   dplyr::summarise(group_n = n())

        if(nrow(df_long) > 0 ) {
          # Obtain Grouped percentages
          t <- bkissell::create_grouped_percentages_table(
            df = df_long,
            grouping_var_name = parameter_df$grouping_vars[[iteration]],
            response_var_name = parameter_df$response_var_names[[iteration]],
            type = "sa",
            group_n_table = group_ns)

          # Add additional needed info to df
          t$response_var_used <- parameter_df$response_var_names[[iteration]]
          t$response_var_used_order <- parameter_df$response_var_used_order[[iteration]]
          t$grouping_var_used <- parameter_df$grouping_vars[[iteration]]
          t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[iteration]]
          t$wave <- WaveName

          # Indicate progress
          pb$tick()
          # Return the table that was made
          t
        } else {
          pb$tick()
          return(data.frame())
        }

      })
      return(table)

    }, parameter_df, column_workbook_lists_single)

    # Temp create these
    wave_tables_df$grouping_var_order <- NA
    wave_tables_df$response_var_order <- NA

    # Get rid of any nas
    wave_tables_df <- wave_tables_df %>%
      dplyr::filter(!is.na(response_var_levels)) %>%
      dplyr::filter(!is.na(grouping_var_levels))

    # Obtain the order information lookup table
    order_information_lookup_table <- bkissell::create_order_information_lookup_table(column_workbook_lists_single, name_of_column_details)

    # Fix the order numbers for response vars
    wave_tables_df <- bkissell::adjust_order_information_lookup_table_per_type(
      table = wave_tables_df,
      order_information_lookup_table = order_information_lookup_table,
      type = "response_var_")

    # Fix the order numbers for grouping vars
    wave_tables_df <- bkissell::adjust_order_information_lookup_table_per_type(wave_tables_df, order_information_lookup_table, type = "grouping_var_")
    # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "response_var", column_details = column_details, column_workbook_list = column_workbook_list)
    # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

    unique_sa_variable_names <- unique(wave_tables_df$response_var_used)

    question_text_df <- column_details %>%
      dplyr::select(all_of(c("label_info", "question"))) %>%
      na.omit() %>%
      unique(by = "label_info") %>%
      dplyr::filter(.data[["label_info"]] %in% unique_sa_variable_names)

    if(use_the_question_text_for_variable_name == TRUE) {
      wave_tables_df$response_var_used <- factor(wave_tables_df$response_var_used, levels = question_text_df$label_info, labels = question_text_df$question)
    }

    wave_tables_df$grouping_var_used  <- stringr::str_replace_all(wave_tables_df$grouping_var_used, "_", " ") %>% stringr::str_to_title()

    wave_tables_df$response_var_used  <- stringr::str_replace_all(wave_tables_df$response_var_used, "_", " ") %>% stringr::str_to_title()

    wave_tables_df$percentage[is.nan(wave_tables_df$percentage)] <- 0

    wave_tables_df <- wave_tables_df %>% dplyr::select(
      "wave", "grouping_var_levels", "response_var_levels", "grouping_var_order",
      "response_var_order", "counts", "group_n", "percentage",
      "grouping_var_used", "grouping_var_used_order", "response_var_used", "response_var_used_order", "overall_sample_size_for_response_var")

    power_bi_select_all <- wave_tables_df

    if(write_data){
      readr::write_csv(power_bi_select_all, power_bi_sa_path)
    }
  }


  ##############################################################################
  ###################################### Numeric ###############################
  ##############################################################################

  check_for_numeric_vars <- column_workbook_lists_single[[name_of_column_details]]$type == "numeric"

  check_for_scale_vars <- !is.na(column_workbook_lists_single[[name_of_column_details]]$scale_names)

  if(any(check_for_numeric_vars) | any(check_for_scale_vars)){

    # Bring in the column details
    column_details <- column_workbook_lists_single[[name_of_column_details]]

    # Create a list of variables that includes those not in the details sheet
    grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
      if(!(.x %in% column_details$column_names)){
        var <- .x
      } else {
        var <- column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info)
      }
      var
    })

    # grouping_var_used_vector_names <- grouping_var_used_vector_names[grouping_var_used_vector_names %in% colnames(survey_data_for_power_bi_df)]

    grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

    additional_num_vars <- column_details$column_names[column_details$type == "numeric" & !is.na(column_details$type)]

    likert_variables <- survey_data_for_power_bi_df %>%
      dplyr::select( tidyselect::starts_with("NUM__")) %>%
      colnames()

    response_var_names <- c(
      # additional_num_vars,
      likert_variables)

    response_var_used_order <- factor(response_var_names, unique(response_var_names)) %>% as.numeric()

    rv_names_and_order_df <- data.frame(
      response_var_names = response_var_names,
      response_var_used_order = response_var_used_order
    )

    gv_names_and_order_df <- data.frame(
      grouping_vars = grouping_vars,
      grouping_var_used_order = grouping_var_used_order
    )

    parameter_df <- expand.grid(response_var_names, grouping_vars)

    colnames(parameter_df) <- c("response_var_names", "grouping_vars")

    parameter_df <- parameter_df %>%
      dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
      dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

    perform_calc_create_table_single_numeric <- function(df, WaveName, parameter_df, column_workbook){
      cgdst_df <- df
      # Obtain the number of iterations
      n_iterations <- length(parameter_df$response_var_names)
      # Create progress bar
      pb <- progress::progress_bar$new(total = n_iterations, format = glue::glue("Performing Calculations for Numeric Stats Table for Wave: {WaveName}  [:bar] :percent eta: :eta"))
      # Loop through every possibility from the parameter df
      table_C <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{

        iteration <- .x

        # Obtain Grouped stats
        # t <- bkissell::create_grouped_des_stats_table(
        #   df = df,
        #   grouping_var_name = parameter_df$grouping_vars[[iteration]],
        #   response_var_name = parameter_df$response_var_names[[iteration]]
        # )

        cgdst_grouping_var_name = parameter_df$grouping_vars[[iteration]]
        cgdst_response_var_name = parameter_df$response_var_names[[iteration]]

        cgdst_variable_used <- cgdst_response_var_name

        if(!(is.numeric(cgdst_df[[cgdst_variable_used]])) & !stringr::str_detect(cgdst_variable_used, "^NUM__")){
          cgdst_variable_used <- paste0("NUM__", cgdst_variable_used)
        }

        if(!all(is.na(cgdst_df[[{{cgdst_variable_used}}]]))) {

          # Rename the variable as all
          cgdst_df$all <- cgdst_df[[{{cgdst_variable_used}}]]

          table <- cgdst_df %>%
            # Rename these variables so that we can combine everything into the same column
            dplyr::mutate(
              grouping_var_levels = as.character(.data[[{{cgdst_grouping_var_name}}]])) %>%
            # Group by these variables
            dplyr::group_by(
              grouping_var_levels) %>%

            dplyr::filter(!is.na(.data[[{{cgdst_variable_used}}]])) %>%
            # Get the counts for the grouping variable by the response variables
            dplyr::summarize(
              mean = mean(.data[[{{cgdst_variable_used}}]], na.rm = TRUE),
              sd = sd(.data[[{{cgdst_variable_used}}]], na.rm = TRUE),
              n = dplyr::n(),
              se = sd/sqrt(n),
              ci_limit = se * 1.96,
              ci_upper = mean + ci_limit,
              ci_lower = mean - ci_limit, .groups = "drop")

          # Get the order for the grouping variables
          table$grouping_var_used <- {{cgdst_grouping_var_name}}


          table <- table %>%
            dplyr::select(
              grouping_var_levels,
              mean, sd, n, se, ci_limit, ci_upper, ci_lower,
              grouping_var_used
            )

          table_all <- df %>%
            dplyr::mutate(
              grouping_var_levels = "All") %>%
            # Group by these variables
            dplyr::group_by(
              grouping_var_levels) %>%
            dplyr::filter(!is.na(.data[[{{cgdst_variable_used}}]])) %>%
            # Calculate the counts
            dplyr::summarize(
              mean = mean(.data[[{{cgdst_variable_used}}]], na.rm = TRUE),
              sd = sd(.data[[{{cgdst_variable_used}}]], na.rm = TRUE),
              n = dplyr::n(),
              se = sd/sqrt(n),
              ci_limit = se * 1.96,
              ci_upper = mean + ci_limit,
              ci_lower = mean - ci_limit, .groups = "drop")


          table_all$grouping_var_used <- {{cgdst_grouping_var_name}}
          table_all$grouping_var_levels <- "All"


          # Order the variables
          table_all <- table_all %>%
            dplyr::select(
              grouping_var_levels,
              mean, sd, n, se, ci_limit, ci_upper, ci_lower,
              grouping_var_used
            )

          # Combine the two data frames
          table <- table %>% rbind(table_all)
          table$grouping_var_order <- NA
          table$response_var_order <- NA
          table$response_var_used <- cgdst_variable_used
          table$response_var_levels <- "All"
          table$overall_sample_size_for_response_var <- table_all %>% dplyr::pull(n) %>% max()

          # Add additional needed info to df
          table$response_var_used <- parameter_df$response_var_names[[iteration]]
          table$response_var_used_order <- parameter_df$response_var_used_order[[iteration]]
          table$grouping_var_used <- parameter_df$grouping_vars[[iteration]]
          table$grouping_var_used_order <- parameter_df$grouping_var_used_order[[iteration]]
          table$wave <- WaveName

          # Indicate progress
          pb$tick()
          # Return the table that was made
          return(table)

        } else {
          pb$tick()
          return(NULL)
        }
      })

      table_C <- table_C %>%
        dplyr::filter(!is.na(.data[["grouping_var_levels"]]))
      table_C$ci_limit <- tidyr::replace_na(table_C$ci_limit, 0)
      table_C$ci_upper <- tidyr::replace_na(table_C$ci_upper, 0)
      table_C$ci_lower <- tidyr::replace_na(table_C$ci_lower, 0)
      table_C$sd <- tidyr::replace_na(table_C$sd, 0)
      table_C$se <- tidyr::replace_na(table_C$se, 0)

      return(table_C)
    }

    # Make a df for every wave
    split_wave_dfs <- split(survey_data_for_power_bi_df, as.factor(survey_data_for_power_bi_df$wave_info))

    # Add the df with all data to that list
    all_dfs <- append(list("All Waves" = survey_data_for_power_bi_df), split_wave_dfs)

    # Obtain the calculations for every df in the list
    wave_tables_df <- purrr::map_df(seq_along(all_dfs), ~{
      iteration <- .x

      perform_calc_create_table_single_numeric(
        df = all_dfs[[iteration]],
        WaveName = as.list(names(all_dfs))[[iteration]],
        parameter_df = parameter_df,
        column_workbook = column_workbook_lists_single
        )

      # perform_calc_create_table_single_sa(df = all_dfs[[3]], WaveName = as.list(names(all_dfs))[[3]], parameter_df, column_workbook = column_workbook_list)
    }, parameter_df, column_workbook_lists_single)

    # Temp create these
    wave_tables_df$grouping_var_order <- NA
    wave_tables_df$response_var_order <- 0

    # Get rid of any nas
    wave_tables_df <- wave_tables_df %>%
      dplyr::filter(!is.na(response_var_levels)) %>%
      dplyr::filter(!is.na(grouping_var_levels))

    # Obtain the order information lookup table
    order_information_lookup_table <- bkissell::create_order_information_lookup_table(column_workbook_lists_single, name_of_column_details)


    # Fix the order numbers for grouping vars
    wave_tables_df <- bkissell::adjust_order_information_lookup_table_per_type(wave_tables_df, order_information_lookup_table, type = "grouping_var_")
    # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "response_var", column_details = column_details, column_workbook_list = column_workbook_list)
    # wave_tables_df <- fix_levels_and_order_in_table(table = wave_tables_df, var_type = "grouping_var", column_details = column_details, column_workbook_list = column_workbook_list)

    wave_tables_df$grouping_var_used  <- stringr::str_replace_all(wave_tables_df$grouping_var_used, "_", " ") %>% stringr::str_to_title()

    wave_tables_df$response_var_used  <- stringr::str_replace_all(wave_tables_df$response_var_used, "NUM__ec__", "") %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_to_title()

    wave_tables_df <- wave_tables_df %>%
      dplyr::select(
        "wave",
        "grouping_var_levels",
        "grouping_var_order",
        "mean",
        "sd",
        "n",
        "se",
        "ci_limit",
        "ci_upper",
        "ci_lower",
        "grouping_var_used",
        "grouping_var_used_order",
        "response_var_used",
        "response_var_used_order",
        "overall_sample_size_for_response_var",
        "response_var_order",
        "response_var_levels"

      )




      # dplyr::select("wave", "grouping_var_levels",	"grouping_var_order",	"mean",	"sd",	"n",	"se",
      #                                                  "ci_limit",	"ci_upper",	"ci_lower",	"grouping_var_used",
      #                                                  "grouping_var_used_order",	"response_var_used",
      #                                                  "response_var_used_order", "overall_sample_size_for_response_var", "response_var_order", "response_var_levels")

    power_bi_descr_table_num <- wave_tables_df

    if(write_data){
      readr::write_csv(power_bi_descr_table_num, power_bi_descr_table_num_path)
    }
  }

  ##############################################################################
  ################################# Net Promoter ###############################
  ##############################################################################

  # Net Promoter
  # my_current_env <- bkissell::create_power_bi_net_promoter(my_env = my_current_env)

  check_for_net_promoter_vars <- column_workbook_lists_single[[name_of_column_details]]$type == "numeric"

  if(any(check_for_net_promoter_vars)){
    # power_bi_net_promoter <- bkissell::create_power_bi_data_nps_CALCULATED_TABLES(
    #   df = my_env$survey_data_for_power_bi_df,
    #   column_workbook_list = my_env$column_workbook_lists_single,
    #   grouping_vars = grouping_vars,
    #   name_of_column_details = name_of_column_details)

    # Bring in the column details
    column_details <- column_workbook_lists_single[[name_of_column_details]]

    # Create a list of variables that includes those not in the details sheet
    grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
      if(!(.x %in% column_details$column_names)){
        var <- .x
      } else {
        var <- column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info)
      }
      var
    })

    # grouping_var_used_vector_names <- grouping_var_used_vector_names[grouping_var_used_vector_names %in% colnames(survey_data_for_power_bi_df)]

    grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()

    response_var_names <- "net_promoter"

    response_var_used_order <- 1

    rv_names_and_order_df <- data.frame(
      response_var_names = response_var_names,
      response_var_used_order = response_var_used_order
    )

    gv_names_and_order_df <- data.frame(
      grouping_vars = grouping_vars,
      grouping_var_used_order = grouping_var_used_order
    )

    parameter_df <- expand.grid(response_var_names, grouping_vars)

    colnames(parameter_df) <- c("response_var_names", "grouping_vars")

    parameter_df <- parameter_df %>%
      dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
      dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")

    perform_calc_create_table_single_nps<- function(df, WaveName = "All Waves", parameter_df, column_workbook){
      # Obtain the number of iterations
      n_iterations <- length(parameter_df$response_var_names)
      # Create progress bar
      pb <- progress::progress_bar$new(total = n_iterations, format = glue::glue("Performing Calculations for Numeric Stats Table for Wave: {WaveName}  [:bar] :percent eta: :eta"))
      # Loop through every possibility from the parameter df
      table <- purrr::map_df(seq_along(parameter_df$response_var_names), ~{

        iteration <- .x

        # Obtain Grouped stats
        t <- bkissell::create_grouped_nps_stats_table(
          df = df,
          grouping_var_name = parameter_df$grouping_vars[[iteration]],
          response_var_name = parameter_df$response_var_names[[iteration]]
        )

        # Add additional needed info to df
        t$response_var_used <- parameter_df$response_var_names[[iteration]]
        t$response_var_used_order <- parameter_df$response_var_used_order[[iteration]]
        t$grouping_var_used <- parameter_df$grouping_vars[[iteration]]
        t$grouping_var_used_order <- parameter_df$grouping_var_used_order[[iteration]]
        t$wave <- WaveName

        # Indicate progress
        pb$tick()
        # Return the table that was made
        t

      })

      table <- table %>%
        dplyr::filter(!is.na(grouping_var_levels))

      return(table)
    }

    # Make a df for every wave
    split_wave_dfs <- split(survey_data_for_power_bi_df, as.factor(survey_data_for_power_bi_df$wave_info))

    # Add the df with all data to that list
    all_dfs <- append(list("All Waves" = survey_data_for_power_bi_df), split_wave_dfs)

    # Obtain the calculations for every df in the list
    wave_tables_df <- purrr::map2_df(all_dfs, as.list(names(all_dfs)), ~{
      perform_calc_create_table_single_nps(df = .x, WaveName = .y, parameter_df, column_workbook = column_workbook_lists_single)
      # perform_calc_create_table_single_sa(df = all_dfs[[3]], WaveName = as.list(names(all_dfs))[[3]], parameter_df, column_workbook = column_workbook_list)
    }, parameter_df, column_workbook_lists_single)

    # Temp create these
    wave_tables_df$grouping_var_order <- NA
    wave_tables_df$response_var_order <- 0

    # Get rid of any nas
    wave_tables_df <- wave_tables_df %>%
      dplyr::filter(!is.na(response_var_levels)) %>%
      dplyr::filter(!is.na(grouping_var_levels))

    # Obtain the order information lookup table
    order_information_lookup_table <- bkissell::create_order_information_lookup_table(column_workbook_lists_single, name_of_column_details)


    # Fix the order numbers for grouping vars
    wave_tables_df <- bkissell::adjust_order_information_lookup_table_per_type(wave_tables_df, order_information_lookup_table, type = "grouping_var_")


    wave_tables_df$grouping_var_used  <- stringr::str_replace_all(wave_tables_df$grouping_var_used, "_", " ") %>% stringr::str_to_title()

    wave_tables_df$response_var_used  <- stringr::str_replace_all(wave_tables_df$response_var_used, "_", " ") %>%
      stringr::str_to_title()

    wave_tables_df <- wave_tables_df %>%
      dplyr::select("wave", "grouping_var_levels",	"grouping_var_order",
                    "sum",	"n",	"average",	"NPS",	"grouping_var_used",
                    "grouping_var_used_order", "overall_sample_size_for_response_var", "response_var_used", "response_var_levels")

    power_bi_net_promoter <- wave_tables_df

    if(write_data){
      readr::write_csv(power_bi_net_promoter, power_bi_net_promoter_path)
    }
  }


  ##############################################################################
  ################################# Qualitative Coding Overall ###############################
  ##############################################################################

  # Qualitative Coding Overall
  # my_current_env <- bkissell::create_power_bi_overall_qualitative(my_env = my_current_env)

  # If I end up needing a combined set, I will need to add an optional piece that does those calculations
  power_bi_overall_qualitative_combined <- purrr::map_df(seq_along(qualitative_coding_data_path_list), ~ {

    # Set the number for the iteration
    iteration <- .x

    qualitative_type = "overall_coding"
    identifier = "RID"

    # qualitative_coding_data_path_list[[iteration]]
    processed_survey_data <- survey_data_for_power_bi_df

    path_to_qual_coding_data_list_o_c <- qualitative_coding_data_path_list[[iteration]]

    # Check to see if this file exists
    if(file.exists(path_to_qual_coding_data_list_o_c)){

      # Filter the processed data for the specific wave
      ind_processed_survey_data <- processed_survey_data %>%
        dplyr::filter(wave_info == basename(dirname(path_to_qual_coding_data_list_o_c)))

      # Create the calculated Overall Power BI table
      power_bi_overall_qualitative <- bkissell::create_power_bi_data_qualitative_CALCULATED_TABLES(
        df = ind_processed_survey_data,
        column_workbook_list = column_workbook_lists_single,
        grouping_vars = grouping_vars,
        name_of_column_details,
        path_to_qual_coding_data = path_to_qual_coding_data_list_o_c,
        qualitative_type = qualitative_type,
        identifier = identifier)[[1]]

      order_information_lookup_table <- bkissell::create_order_information_lookup_table(column_workbook_lists_single, name_of_column_details)

      power_bi_overall_qualitative$grouping_var_used <- snakecase::to_snake_case(power_bi_overall_qualitative$grouping_var_used)

      power_bi_overall_qualitative <- bkissell::adjust_order_information_lookup_table_per_type(
        table = power_bi_overall_qualitative,
        order_information_lookup_table,
        type = "grouping_var_"
      )

      power_bi_overall_qualitative <- power_bi_overall_qualitative %>%
        dplyr::filter(!is.na(grouping_var_levels))

      power_bi_overall_qualitative$wave <- basename(dirname(path_to_qual_coding_data_list_o_c))

        return(power_bi_overall_qualitative)
      } else {
        return(data.frame())
      }

  })

  if(write_data){
    readr::write_csv(power_bi_overall_qualitative_combined, power_bi_overall_qualitative_path)
  }




  power_bi_break_down_qualitative_combined <- purrr::map_df(seq_along(qualitative_coding_data_path_list), ~ {

    # Set the number for the iteration
    iteration <- .x
    qualitative_type = "break_down_coding"
    identifier = "RID"

    # qualitative_coding_data_path_list[[iteration]]
    processed_survey_data <- survey_data_for_power_bi_df

    path_to_qual_coding_data_list_o_c <- qualitative_coding_data_path_list[[iteration]]

    # Check to see if this file exists
    if(file.exists(path_to_qual_coding_data_list_o_c)){

      # Filter the processed data for the specific wave
      ind_processed_survey_data <- processed_survey_data %>%
        dplyr::filter(wave_info == basename(dirname(path_to_qual_coding_data_list_o_c)))


      # Create the calculated Break Down Power BI table
      power_bi_break_down_qualitative <- bkissell::create_power_bi_data_qualitative_CALCULATED_TABLES(
        df = ind_processed_survey_data,
        column_workbook_list = column_workbook_lists_single,
        grouping_vars = grouping_vars,
        name_of_column_details,
        path_to_qual_coding_data = path_to_qual_coding_data_list_o_c,
        qualitative_type = qualitative_type,
        identifier = identifier)[[1]]


      order_information_lookup_table <- bkissell::create_order_information_lookup_table(column_workbook_lists_single, name_of_column_details)

      power_bi_break_down_qualitative$grouping_var_used <- snakecase::to_snake_case(power_bi_break_down_qualitative$grouping_var_used)

      power_bi_break_down_qualitative <- bkissell::adjust_order_information_lookup_table_per_type(
        table = power_bi_break_down_qualitative,
        order_information_lookup_table,
        type = "grouping_var_"
      )

      power_bi_break_down_qualitative <- power_bi_break_down_qualitative %>%
        dplyr::filter(!is.na(grouping_var_levels))

      power_bi_break_down_qualitative$wave <- basename(dirname(path_to_qual_coding_data_list_o_c))

      return(power_bi_break_down_qualitative)
    } else {
      return(data.frame())
    }
  })





  if(write_data){
    readr::write_csv(power_bi_break_down_qualitative_combined, power_bi_break_down_qualitative_path)
  }






#
#
#     ############################################################################
#     ############################################################################
#
#     iteration <- .x
#
#     if(file.exists(qualitative_coding_data_path_list[[iteration]])){
#       # power_bi_overall_qualitative <- bkissell::create_power_bi_data_qualitative_CALCULATED_TABLES(
#       #   df = my_env$survey_data_for_power_bi_df,
#       #   column_workbook_list = my_env$column_workbook_lists_single,
#       #   grouping_vars = grouping_vars,
#       #   name_of_column_details,
#       #   path_to_qual_coding_data = my_env$qualitative_coding_data_path_list[[iteration]],
#       #   qualitative_type = "overall_coding",
#       #   identifier = "RID")
#
#       qualitative_type = "overall_coding"
#       identifier = "RID"
#       path_to_qual_coding_data = qualitative_coding_data_path_list[[iteration]]
#
#
#       # Bring in the column details
#       column_details <- column_workbook_lists_single[[name_of_column_details]]
#
#       # Create a list of variables that includes those not in the details sheet
#       grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
#         if(!(.x %in% column_details$column_names)){
#           var <- .x
#         } else {
#           var <- column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info)
#         }
#         var
#       })
#
#       grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()
#
#       response_var_names <- column_details$column_names[column_details$type == "qualitative" & !is.na(column_details$type)]
#       ##### old
#
#       check_if_qual_data_path_exists <- file.exists(path_to_qual_coding_data)
#
#       if(!check_if_qual_data_path_exists | is.null(qualitative_type)) {
#         return(data.frame())
#       } else {
#         path_to_qualitative_coding_data <- path_to_qual_coding_data
#
#         workbook_sheets_qualitative_names_all <- readxl::excel_sheets(path_to_qualitative_coding_data)
#
#         qualitative_coding_details <- readxl::read_excel(path_to_qualitative_coding_data, "qual_column_details")
#
#         workbook_sheets_qualitative_names <- workbook_sheets_qualitative_names_all[workbook_sheets_qualitative_names_all != "qual_column_details"]
#
#         # Loop through each sheet
#         workbook_sheets_qualitative <- purrr::map(workbook_sheets_qualitative_names, ~{
#
#           current_sheet_name <- .x
#           # Read in the data for it
#           coding_data <- readxl::read_excel(path_to_qualitative_coding_data, sheet = current_sheet_name)
#
#           survey_data_for_power_bi_df$RID <- as.numeric(survey_data_for_power_bi_df$RID)
#           coding_data$RID <- as.numeric(coding_data$RID)
#
#           qual_coding_data <- survey_data_for_power_bi_df %>%
#             dplyr::left_join(coding_data, by = {{identifier}})
#
#           qual_coding_data$RID <- as.character(qual_coding_data$RID)
#           return(qual_coding_data)
#
#           # Name all sheets so they can be easily accessed
#         }) %>% purrr::set_names(workbook_sheets_qualitative_names)
#
#         qualitative_coding_details_df_list <- purrr::map(workbook_sheets_qualitative_names,  ~{
#           single_sheet_qualitative_coding_details <- qualitative_coding_details %>%
#             dplyr::filter(question == .x)
#
#           single_sheet_qualitative_coding_details
#         })
#
#         overall_qualitative_coding_details_df_list <- purrr::map(qualitative_coding_details_df_list, ~{
#           overall_qualitative_coding_details <- .x %>%
#             dplyr::filter(section_type == "overall_coding_labels")
#
#           overall_qualitative_coding_details
#         })
#
#         overall_code_labels_list <- purrr::map(overall_qualitative_coding_details_df_list, ~{
#           .x$column_labels
#         })
#
#         overall_code_indicator_list <- purrr::map(overall_qualitative_coding_details_df_list, ~{
#           response_var_names_overall <- .x$column_names %>% stringr::str_extract("^.+__") %>% unique()
#           response_var_names_overall
#         })
#
#         overall_code_indicator_order_list <- purrr::map(seq_along(overall_code_indicator_list), ~{
#           response_var_used_order_overall <- .x
#           response_var_used_order_overall
#         })
#
#         grouping_vars_selected_list <- purrr::map(workbook_sheets_qualitative, ~{
#           .x %>% dplyr::select(tidyselect::any_of(grouping_vars)) %>% colnames()
#         })
#
#         grouping_vars_selected_order_list <- purrr::map(grouping_vars_selected_list, ~{
#           .x %>% as.factor() %>% as.numeric()
#         })
#
#         obtain_the_parameters_for_overall <- function(overall_code_indicator, overall_code_indicator_order, grouping_vars_selected, grouping_vars_selected_order) {
#
#           rv_names_and_order_df <- data.frame(
#             response_var_names = overall_code_indicator,
#             response_var_used_order = overall_code_indicator_order
#           )
#
#           gv_names_and_order_df <- data.frame(
#             grouping_vars = grouping_vars_selected,
#             grouping_var_used_order = grouping_vars_selected_order
#           )
#
#           parameter_df <- expand.grid(rv_names_and_order_df$response_var_names, gv_names_and_order_df$grouping_vars)
#
#           colnames(parameter_df) <- c("response_var_names", "grouping_vars")
#
#           parameter_df <- parameter_df %>%
#             dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
#             dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")
#
#           parameter_df
#
#         }
#
#         overall_parameter_list <- list(overall_code_indicator_list, overall_code_indicator_order_list, grouping_vars_selected_list, grouping_vars_selected_order_list)
#
#         parameter_df_list <- purrr::pmap(overall_parameter_list, obtain_the_parameters_for_overall)
#
#         table_list <- purrr::map2(parameter_df_list, workbook_sheets_qualitative, ~{
#           overall_code_data <- .y
#
#           single_parameter_df <- .x
#
#           n_iterations <- length(single_parameter_df$response_var_names)
#
#           pb <- progress::progress_bar$new(total = n_iterations)
#
#           table <- purrr::map_df(seq_along(single_parameter_df$response_var_names), ~{
#             number_for_row <- .x
#             t <- bkissell::create_table_single(
#               df = overall_code_data,
#               response_var_name = single_parameter_df$response_var_names[[number_for_row]],
#               grouping_var_name = single_parameter_df$grouping_vars[[number_for_row]],
#               response_var_type = "sa")
#
#             pb$tick()
#
#             t$response_var_used <- single_parameter_df$response_var_names[[number_for_row]]
#             t$response_var_used_order <- single_parameter_df$response_var_used_order[[number_for_row]]
#             t$grouping_var_used <- single_parameter_df$grouping_vars[[number_for_row]]
#             t$grouping_var_used_order <- single_parameter_df$grouping_var_used_order[[number_for_row]]
#
#             t
#           })
#
#           table
#         })
#
#         overall_table_list <- purrr::map2(table_list, overall_qualitative_coding_details_df_list, ~{
#           table <- .x
#           table$response_var_levels <- factor(table$response_var_levels, .y$column_names, .y$column_labels)
#           table$response_var_order <- as.numeric(table$response_var_levels)
#           table
#         })
#
#         overall_table_list <- purrr::map(overall_table_list, ~{
#           table <- .x
#           table$grouping_var_used <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>% stringr::str_to_title()
#           table
#         })
#
#         break_down_qualitative_coding_details <- purrr::map2(qualitative_coding_details_df_list, overall_code_labels_list, ~{
#           .x %>%
#             dplyr::filter(section_type %in% .y)
#         })
#
#         break_down_categories_list <- purrr::map(break_down_qualitative_coding_details, ~{
#           break_down_categories <- .x$section_type %>% unique()
#           break_down_categories
#         })
#
#         break_down_categories_details_list <- purrr::map2(break_down_qualitative_coding_details, break_down_categories_list, ~{
#           break_down_qualitative_coding_details_1 <- .x
#           break_down_categories <- .y
#           purrr::pmap(list(seq_along(break_down_categories)), ~{
#             break_down_qualitative_coding_details_1 %>% dplyr::filter(section_type == break_down_categories[[..1]])
#           })
#
#         })
#
#         break_down_code_labels_list <- purrr::map(break_down_categories_details_list, ~{
#           break_down_categories_details <- .x
#
#           purrr::pmap(list(seq_along(break_down_categories_details)), ~{
#             break_down_categories_details[[.x]]$column_labels %>% as.character()
#           })
#         })
#
#         break_down_code_indicator_list <- purrr::map(break_down_categories_details_list, ~{
#           break_down_categories_details <- .x
#
#           purrr::pmap(list(seq_along(break_down_categories_details)), ~{
#             break_down_categories_details[[.x]]$column_names %>% stringr::str_extract("^.+__.+__") %>% unique()
#           })
#         })
#
#         break_down_code_indicator_order_list <- purrr::map(break_down_categories_details_list, ~{
#           break_down_code_indicator_order <- .x
#
#           purrr::pmap(list(seq_along(break_down_code_indicator_order)), ~{
#             ..1
#           })
#         })
#
#         break_down_parameter_df_list <- purrr::map(seq_along(break_down_code_indicator_list), ~{
#           first_level <- .x
#
#           purrr::map(seq_along(break_down_code_indicator_list[[first_level]]), ~{
#             second_level <- .x
#
#             break_down_parameter_list <- list(
#               break_down_code_indicator_list[[first_level]][[second_level]],
#               break_down_code_indicator_order_list[[first_level]][[second_level]],
#               grouping_vars_selected_list[[first_level]],
#               grouping_vars_selected_order_list[[first_level]])
#
#             purrr::pmap_df(break_down_parameter_list, obtain_the_parameters_for_overall)
#
#           })})
#
#         break_down_table_list <- purrr::map2(break_down_parameter_df_list, workbook_sheets_qualitative, ~{
#           overall_code_data <- .y
#
#           single_parameter_df_list <- .x
#
#           purrr::map_df(single_parameter_df_list, ~{
#             single_parameter_df <- .x
#
#             table <- purrr::map_df(seq_along(single_parameter_df$response_var_names), ~{
#               number_for_row <- .x
#               t <- bkissell::create_table_single(
#                 df = overall_code_data,
#                 response_var_name = single_parameter_df$response_var_names[[number_for_row]],
#                 grouping_var_name = single_parameter_df$grouping_vars[[number_for_row]],
#                 response_var_type = "sa")
#
#
#               t$response_var_used <- single_parameter_df$response_var_names[[number_for_row]]
#               t$response_var_used_order <- single_parameter_df$response_var_used_order[[number_for_row]]
#               t$grouping_var_used <- single_parameter_df$grouping_vars[[number_for_row]]
#               t$grouping_var_used_order <- single_parameter_df$grouping_var_used_order[[number_for_row]]
#
#               t
#             })
#
#             table
#           })
#         })
#
#         break_down_table_list <- purrr::map2(break_down_table_list, break_down_qualitative_coding_details, ~{
#           table <- .x
#           table$response_var_levels <- factor(table$response_var_levels, .y$column_names, .y$column_labels)
#           table$response_var_order <- as.numeric(table$response_var_levels)
#           table
#         })
#
#         break_down_table_list <- purrr::map(break_down_table_list, ~{
#           table <- .x
#           table$grouping_var_used <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>% stringr::str_to_title()
#           table
#         })
#
#         if(qualitative_type == "overall_coding") {
#           combined_final_table <- overall_table_list
#         } else if(qualitative_type == "break_down_coding"){
#           combined_final_table <- break_down_table_list
#         }
#
#         return(combined_final_table)
#       }
#
#       power_bi_overall_qualitative <- power_bi_overall_qualitative[[1]]
#
#       order_information_lookup_table <- bkissell::create_order_information_lookup_table(column_workbook_lists_single, name_of_column_details)
#
#       power_bi_overall_qualitative$grouping_var_used <- snakecase::to_snake_case(power_bi_overall_qualitative$grouping_var_used)
#
#       power_bi_overall_qualitative <- adjust_order_information_lookup_table_per_type(table = power_bi_overall_qualitative, order_information_lookup_table, type = "grouping_var_")
#       power_bi_overall_qualitative <- power_bi_overall_qualitative %>%
#         dplyr::filter(!is.na(grouping_var_levels))
#
#       power_bi_overall_qualitative$wave <- basename(dirname(qualitative_coding_data_path_list[[iteration]]))
#
#       return(power_bi_overall_qualitative)
#     } else {
#       return(data.frame())
#     }
#   }) %>% purrr::reduce(dplyr::bind_rows)
#
#   # rlang::env_poke(env = my_env, "power_bi_overall_qualitative_combined", power_bi_overall_qualitative_combined)
#
#   if(write_data){
#     readr::write_csv(power_bi_overall_qualitative_combined, power_bi_overall_qualitative_path)
#   }
#
#   # if(write_data){
#   #   readr::write_csv(power_bi_break_down_qualitative_combined, power_bi_break_down_qualitative_path)
#   # }
#
#   ##############################################################################
#   ################################# Qualitative Coding Overall ###############################
#   ##############################################################################
#
#   # Qualitative Coding Break Down
#   # my_current_env <- bkissell::create_power_bi_break_down_qualitative_combined(my_env = my_current_env)
#
#
#   power_bi_break_down_qualitative_combined <- purrr::map(qualitative_coding_data_path_list, ~ {
#     if(file.exists(.x)){
#
#       # power_bi_break_down_qualitative <- create_power_bi_data_qualitative_CALCULATED_TABLES(
#       #   df = my_env$survey_data_for_power_bi_df,
#       #   column_workbook_list = my_env$column_workbook_lists_single,
#       #   grouping_vars = grouping_vars,
#       #   name_of_column_details,
#       #   path_to_qual_coding_data = .x,
#       #   qualitative_type = "break_down_coding",
#       #   identifier = "RID")
#
#
#       survey_data_for_power_bi_df <- survey_data_for_power_bi_df
#
#       # Bring in the column details
#       column_details <- column_workbook_lists_single[[name_of_column_details]]
#
#       # Create a list of variables that includes those not in the details sheet
#       grouping_var_used_vector_names <- purrr::map_chr(grouping_vars, ~{
#         if(!(.x %in% column_details$column_names)){
#           var <- .x
#         } else {
#           var <- column_details %>% dplyr::filter(column_names == .x) %>% dplyr::pull(label_info)
#         }
#         var
#       })
#
#       grouping_var_used_order <- factor(grouping_var_used_vector_names, unique(grouping_var_used_vector_names)) %>% as.numeric()
#
#       response_var_names <- column_details$column_names[column_details$type == "qualitative" & !is.na(column_details$type)]
#       ##### old
#
#       check_if_qual_data_path_exists <- file.exists(path_to_qual_coding_data)
#
#       if(!check_if_qual_data_path_exists | is.null(qualitative_type)) {
#         return(data.frame())
#       } else {
#         path_to_qualitative_coding_data <- path_to_qual_coding_data
#
#         workbook_sheets_qualitative_names_all <- readxl::excel_sheets(path_to_qualitative_coding_data)
#
#         qualitative_coding_details <- readxl::read_excel(path_to_qualitative_coding_data, "qual_column_details")
#
#         workbook_sheets_qualitative_names <- workbook_sheets_qualitative_names_all[workbook_sheets_qualitative_names_all != "qual_column_details"]
#
#         # Loop through each sheet
#         workbook_sheets_qualitative <- purrr::map(workbook_sheets_qualitative_names, ~{
#
#           current_sheet_name <- .x
#           # Read in the data for it
#           coding_data <- readxl::read_excel(path_to_qualitative_coding_data, sheet = current_sheet_name)
#
#           survey_data_for_power_bi_df$RID <- as.numeric(survey_data_for_power_bi_df$RID)
#           coding_data$RID <- as.numeric(coding_data$RID)
#
#           qual_coding_data <- survey_data_for_power_bi_df %>%
#             dplyr::left_join(coding_data, by = {{identifier}})
#
#           qual_coding_data$RID <- as.character(qual_coding_data$RID)
#           return(qual_coding_data)
#
#           # Name all sheets so they can be easily accessed
#         }) %>% purrr::set_names(workbook_sheets_qualitative_names)
#
#         qualitative_coding_details_df_list <- purrr::map(workbook_sheets_qualitative_names,  ~{
#           single_sheet_qualitative_coding_details <- qualitative_coding_details %>%
#             dplyr::filter(question == .x)
#
#           single_sheet_qualitative_coding_details
#         })
#
#         overall_qualitative_coding_details_df_list <- purrr::map(qualitative_coding_details_df_list, ~{
#           overall_qualitative_coding_details <- .x %>%
#             dplyr::filter(section_type == "overall_coding_labels")
#
#           overall_qualitative_coding_details
#         })
#
#         overall_code_labels_list <- purrr::map(overall_qualitative_coding_details_df_list, ~{
#           .x$column_labels
#         })
#
#         overall_code_indicator_list <- purrr::map(overall_qualitative_coding_details_df_list, ~{
#           response_var_names_overall <- .x$column_names %>% stringr::str_extract("^.+__") %>% unique()
#           response_var_names_overall
#         })
#
#         overall_code_indicator_order_list <- purrr::map(seq_along(overall_code_indicator_list), ~{
#           response_var_used_order_overall <- .x
#           response_var_used_order_overall
#         })
#
#         grouping_vars_selected_list <- purrr::map(workbook_sheets_qualitative, ~{
#           .x %>% dplyr::select(tidyselect::any_of(grouping_vars)) %>% colnames()
#         })
#
#         grouping_vars_selected_order_list <- purrr::map(grouping_vars_selected_list, ~{
#           .x %>% as.factor() %>% as.numeric()
#         })
#
#         obtain_the_parameters_for_overall <- function(overall_code_indicator, overall_code_indicator_order, grouping_vars_selected, grouping_vars_selected_order) {
#
#           rv_names_and_order_df <- data.frame(
#             response_var_names = overall_code_indicator,
#             response_var_used_order = overall_code_indicator_order
#           )
#
#           gv_names_and_order_df <- data.frame(
#             grouping_vars = grouping_vars_selected,
#             grouping_var_used_order = grouping_vars_selected_order
#           )
#
#           parameter_df <- expand.grid(rv_names_and_order_df$response_var_names, gv_names_and_order_df$grouping_vars)
#
#           colnames(parameter_df) <- c("response_var_names", "grouping_vars")
#
#           parameter_df <- parameter_df %>%
#             dplyr::left_join(rv_names_and_order_df, by = "response_var_names") %>%
#             dplyr::left_join(gv_names_and_order_df, by = "grouping_vars")
#
#           parameter_df
#
#         }
#
#         overall_parameter_list <- list(overall_code_indicator_list, overall_code_indicator_order_list, grouping_vars_selected_list, grouping_vars_selected_order_list)
#
#         parameter_df_list <- purrr::pmap(overall_parameter_list, obtain_the_parameters_for_overall)
#
#         table_list <- purrr::map2(parameter_df_list, workbook_sheets_qualitative, ~{
#           overall_code_data <- .y
#
#           single_parameter_df <- .x
#
#           n_iterations <- length(single_parameter_df$response_var_names)
#
#           pb <- progress::progress_bar$new(total = n_iterations)
#
#           table <- purrr::map_df(seq_along(single_parameter_df$response_var_names), ~{
#             number_for_row <- .x
#             t <- bkissell::create_table_single(
#               df = overall_code_data,
#               response_var_name = single_parameter_df$response_var_names[[number_for_row]],
#               grouping_var_name = single_parameter_df$grouping_vars[[number_for_row]],
#               response_var_type = "sa")
#
#             pb$tick()
#
#             t$response_var_used <- single_parameter_df$response_var_names[[number_for_row]]
#             t$response_var_used_order <- single_parameter_df$response_var_used_order[[number_for_row]]
#             t$grouping_var_used <- single_parameter_df$grouping_vars[[number_for_row]]
#             t$grouping_var_used_order <- single_parameter_df$grouping_var_used_order[[number_for_row]]
#
#             t
#           })
#
#           table
#         })
#
#         overall_table_list <- purrr::map2(table_list, overall_qualitative_coding_details_df_list, ~{
#           table <- .x
#           table$response_var_levels <- factor(table$response_var_levels, .y$column_names, .y$column_labels)
#           table$response_var_order <- as.numeric(table$response_var_levels)
#           table
#         })
#
#         overall_table_list <- purrr::map(overall_table_list, ~{
#           table <- .x
#           table$grouping_var_used <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>% stringr::str_to_title()
#           table
#         })
#
#         break_down_qualitative_coding_details <- purrr::map2(qualitative_coding_details_df_list, overall_code_labels_list, ~{
#           .x %>%
#             dplyr::filter(section_type %in% .y)
#         })
#
#         break_down_categories_list <- purrr::map(break_down_qualitative_coding_details, ~{
#           break_down_categories <- .x$section_type %>% unique()
#           break_down_categories
#         })
#
#         break_down_categories_details_list <- purrr::map2(break_down_qualitative_coding_details, break_down_categories_list, ~{
#           break_down_qualitative_coding_details_1 <- .x
#           break_down_categories <- .y
#           purrr::pmap(list(seq_along(break_down_categories)), ~{
#             break_down_qualitative_coding_details_1 %>% dplyr::filter(section_type == break_down_categories[[..1]])
#           })
#
#         })
#
#         break_down_code_labels_list <- purrr::map(break_down_categories_details_list, ~{
#           break_down_categories_details <- .x
#
#           purrr::pmap(list(seq_along(break_down_categories_details)), ~{
#             break_down_categories_details[[.x]]$column_labels %>% as.character()
#           })
#         })
#
#         break_down_code_indicator_list <- purrr::map(break_down_categories_details_list, ~{
#           break_down_categories_details <- .x
#
#           purrr::pmap(list(seq_along(break_down_categories_details)), ~{
#             break_down_categories_details[[.x]]$column_names %>% stringr::str_extract("^.+__.+__") %>% unique()
#           })
#         })
#
#         break_down_code_indicator_order_list <- purrr::map(break_down_categories_details_list, ~{
#           break_down_code_indicator_order <- .x
#
#           purrr::pmap(list(seq_along(break_down_code_indicator_order)), ~{
#             ..1
#           })
#         })
#
#         break_down_parameter_df_list <- purrr::map(seq_along(break_down_code_indicator_list), ~{
#           first_level <- .x
#
#           purrr::map(seq_along(break_down_code_indicator_list[[first_level]]), ~{
#             second_level <- .x
#
#             break_down_parameter_list <- list(
#               break_down_code_indicator_list[[first_level]][[second_level]],
#               break_down_code_indicator_order_list[[first_level]][[second_level]],
#               grouping_vars_selected_list[[first_level]],
#               grouping_vars_selected_order_list[[first_level]])
#
#             purrr::pmap_df(break_down_parameter_list, obtain_the_parameters_for_overall)
#
#           })})
#
#         break_down_table_list <- purrr::map2(break_down_parameter_df_list, workbook_sheets_qualitative, ~{
#           overall_code_data <- .y
#
#           single_parameter_df_list <- .x
#
#           purrr::map_df(single_parameter_df_list, ~{
#             single_parameter_df <- .x
#
#             table <- purrr::map_df(seq_along(single_parameter_df$response_var_names), ~{
#               number_for_row <- .x
#               t <- bkissell::create_table_single(
#                 df = overall_code_data,
#                 response_var_name = single_parameter_df$response_var_names[[number_for_row]],
#                 grouping_var_name = single_parameter_df$grouping_vars[[number_for_row]],
#                 response_var_type = "sa")
#
#
#               t$response_var_used <- single_parameter_df$response_var_names[[number_for_row]]
#               t$response_var_used_order <- single_parameter_df$response_var_used_order[[number_for_row]]
#               t$grouping_var_used <- single_parameter_df$grouping_vars[[number_for_row]]
#               t$grouping_var_used_order <- single_parameter_df$grouping_var_used_order[[number_for_row]]
#
#               t
#             })
#
#             table
#           })
#         })
#
#         break_down_table_list <- purrr::map2(break_down_table_list, break_down_qualitative_coding_details, ~{
#           table <- .x
#           table$response_var_levels <- factor(table$response_var_levels, .y$column_names, .y$column_labels)
#           table$response_var_order <- as.numeric(table$response_var_levels)
#           table
#         })
#
#         break_down_table_list <- purrr::map(break_down_table_list, ~{
#           table <- .x
#           table$grouping_var_used <- stringr::str_replace_all(table$grouping_var_used, "_", " ") %>% stringr::str_to_title()
#           table
#         })
#
#         if(qualitative_type == "overall_coding") {
#           combined_final_table <- overall_table_list
#         } else if(qualitative_type == "break_down_coding"){
#           combined_final_table <- break_down_table_list
#         }
#
#         return(combined_final_table)
#       }
#
#       power_bi_break_down_qualitative <- power_bi_break_down_qualitative[[1]]
#
#       order_information_lookup_table <- create_order_information_lookup_table(column_workbook_lists_single, name_of_column_details)
#
#       power_bi_break_down_qualitative$grouping_var_used <- snakecase::to_snake_case(power_bi_break_down_qualitative$grouping_var_used)
#
#       power_bi_break_down_qualitative <- adjust_order_information_lookup_table_per_type(table = power_bi_break_down_qualitative, order_information_lookup_table, type = "grouping_var_")
#       power_bi_break_down_qualitative <- power_bi_break_down_qualitative %>%
#         dplyr::filter(!is.na(grouping_var_levels))
#
#       power_bi_break_down_qualitative$wave <- basename(dirname(.x))
#
#       return(power_bi_break_down_qualitative)
#     } else {
#       return(data.frame())
#     }
#
#   }) %>% purrr::reduce(dplyr::bind_rows)
#
#   # rlang::env_poke(env = my_env, "power_bi_break_down_qualitative_combined", power_bi_break_down_qualitative_combined)
#
#   if(write_data){
#     readr::write_csv(power_bi_break_down_qualitative_combined, power_bi_break_down_qualitative_path)
#   }

}

#
# FULL_Processing_Code(
#   survey_version_name,
#   should_create_nonexistant_dirs,
#   survey_monkey_used,
#   wave_names,
#   storage_platform,
#   storage_platform_name,
#   group_dir_name,
#   jobs_folder_name,
#   project_year,
#   project_folder_name,
#   convert_numeric_age_to_age_group,
#   survey_file_ext,
#   survey_datetime_format_pattern,
#   name_of_column_details,
#   write_data,
#   variables_to_include_with_text,
#   grouping_vars,
#   use_the_question_text_for_variable_name
# )
