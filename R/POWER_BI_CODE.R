
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
  grouping_vars
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
  text_selected_example_text_survey_data_path <- paste0("Data Collection/survey_monkey_data/", survey_version_name, "/selected_example_text.xlsx")

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
  text_selected_example_response_vars <- readxl::excel_sheets(text_selected_example_text_survey_data_path)

  # Create the parameters df
  parameters_for_example_read <- data.frame(Var1 = text_selected_example_response_vars)
  parameters_for_example_read$Var2 <- text_selected_example_text_survey_data_path

  # # Assign everything that was created and save it to the environment. This
  # # will be able to used and referred to across other functions.
  #
  # rlang::env_poke(env = my_env, "working_directory_path", working_directory_path)
  # rlang::env_poke(env = my_env, "traditional_project_dir_list", traditional_project_dir_list)
  #
  # for(iteration in seq_along(traditional_project_dir_list)){
  #   value_to_add <- traditional_project_dir_list[[iteration]]
  #   name_to_add <- names(traditional_project_dir_list)[[iteration]]
  #   rlang::env_poke(env = my_env, name_to_add, value_to_add)
  # }
  #
  # rlang::env_poke(env = my_env, "survey_related_dir_list", survey_related_dir_list)
  #
  # for(iteration in seq_along(survey_related_dir_list)){
  #   value_to_add <- survey_related_dir_list[[iteration]]
  #   name_to_add <- names(survey_related_dir_list)[[iteration]]
  #   rlang::env_poke(env = my_env, name_to_add, value_to_add)
  # }
  #
  # rlang::env_poke(env = my_env, "spellcheck_column_paths", spellcheck_column_paths)
  #
  # for(iteration in seq_along(spellcheck_column_paths)){
  #   value_to_add <- spellcheck_column_paths[[iteration]]
  #   name_to_add <- names(spellcheck_column_paths)[[iteration]]
  #   rlang::env_poke(env = my_env, name_to_add, value_to_add)
  # }
  #
  # rlang::env_poke(env = my_env, "power_bi_clean_data_path", power_bi_clean_data_path)
  # rlang::env_poke(env = my_env, "processed_data_clean_data_path", processed_data_clean_data_path)
  # rlang::env_poke(env = my_env, "text_survey_data_path", text_survey_data_path)
  # rlang::env_poke(env = my_env, "power_bi_text_selected_example_text_survey_data_path", power_bi_text_selected_example_text_survey_data_path)
  # rlang::env_poke(env = my_env, "power_bi_text_path", power_bi_text_path)
  # rlang::env_poke(env = my_env, "text_selected_example_text_survey_data_path", text_selected_example_text_survey_data_path)
  # rlang::env_poke(env = my_env, "power_bi_mc_path", power_bi_mc_path)
  # rlang::env_poke(env = my_env, "survey_directory_path_names", survey_directory_path_names)
  # rlang::env_poke(env = my_env, "survey_directory_paths", survey_directory_paths)
  # rlang::env_poke(env = my_env, "column_names_paths", column_names_paths)
  #
  # for(iteration in seq_along(column_names_paths)){
  #   value_to_add <- column_names_paths[[iteration]]
  #   name_to_add <- column_names_paths[[iteration]]
  #   rlang::env_poke(env = my_env, name_to_add, value_to_add)
  # }
  #
  # rlang::env_poke(env = my_env, "power_bi_sa_path", power_bi_sa_path)
  # rlang::env_poke(env = my_env, "power_bi_descr_table_num_path", power_bi_descr_table_num_path)
  # rlang::env_poke(env = my_env, "power_bi_net_promoter_path", power_bi_net_promoter_path)
  # rlang::env_poke(env = my_env, "qualitative_coding_data_path_list", qualitative_coding_data_path_list)
  # rlang::env_poke(env = my_env, "power_bi_overall_qualitative_path", power_bi_overall_qualitative_path)
  # rlang::env_poke(env = my_env, "power_bi_break_down_qualitative_path", power_bi_break_down_qualitative_path)
  # rlang::env_poke(env = my_env, "power_bi_break_down_qualitative_path_list", power_bi_break_down_qualitative_path_list)
  # rlang::env_poke(env = my_env, "text_selected_example_response_vars", text_selected_example_response_vars)
  # rlang::env_poke(env = my_env, "parameters_for_example_read", parameters_for_example_read)


  # Section 1b - Set-up Column Workbook Lists --------------------------------------------
  # column_names_paths <- paste0(my_current_env$working_directory_path, "/", my_current_env$column_names_paths)
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

  survey_data_for_power_bi_df <- purrr::map(seq_along(data_collection_survey_directory_paths), ~{
    iteration <- .x

    survey_directory_path_pb = data_collection_survey_directory_paths[[iteration]]
    column_workbook_list = column_workbook_lists[[iteration]]
    spellcheck_column_path_pb = spellcheck_column_paths[[iteration]]

    survey_data_for_power_bi_single <- try({

      # Obtain the column details
      column_details <- column_workbook_list[[name_of_column_details]]

      # Read in the file
      survey_data_for_power_bi_single <- bkissell::read_survey_data_for_power_bi_single(
        survey_directory_path = survey_directory_path_pb,
        file_ext = survey_file_ext, format_pattern = survey_datetime_format_pattern,
        survey_version_name = survey_version_name,
        column_workbook_list = column_workbook_list
      ) %>% suppressWarnings()

      # Convert Age
      if(convert_numeric_age_to_age_group == TRUE) {
        survey_data_for_power_bi_single$age_group <- bkissell::convert_age_to_generation(age_numeric = survey_data_for_power_bi_single$age, year_of_data_collection = project_year)
      }
#
#       # Fix Spell checked columns
#       spellcheck_it <- try(bkissell::replace_spelling_with_excel_workbook(survey_data_for_power_bi_single, spellcheck_column_path_pb))
#
#       if(all(class(spellcheck_it) != "try-error")) {
#         survey_data_for_power_bi_single <- spellcheck_it
#       }

      # Process the net promotor score
      survey_data_for_power_bi_single <- bkissell::capwcw_process_net_promoter(data = survey_data_for_power_bi_single, column_details)

      column_workbook_list = column_workbook_list_pb

      # # Process the Multiple Choice
      # survey_data_for_power_bi_single <- bkissell::convert_vars_to_factors_with_workbook(
      #   data = survey_data_for_power_bi_single,
      #   column_workbook_list = column_workbook_list_pb,
      #   name_of_column_details = name_of_column_details)

      df_to_use <- survey_data_for_power_bi_single
      column_details <- column_workbook_list[[name_of_column_details]]

      # Get the names of the mc variable
      multiple_choice_included <- "mc" %in% column_details$type

      # If mc is included
      if(multiple_choice_included == TRUE) {
        # Get the column details for multiple choice
        column_details_question_type_df <- dplyr::filter(column_details, .data[["type"]] == "mc")

        multiple_choice_names <- column_details_question_type_df$column_names
        multiple_choice_label_names <- column_details_question_type_df$label_info



        for(i in seq_along(multiple_choice_names)) ~{
          iteration <- 3i

          if(multiple_choice_names[[iteration]] %in% colnames(df_to_use)){
            df_to_use[[multiple_choice_names[[iteration]]]] <- factor(
              x =  df_to_use[[multiple_choice_names[[iteration]]]],
              levels = column_workbook_list[[multiple_choice_label_names[[iteration]]]],
              labels = names(column_workbook_list[[multiple_choice_label_names[[iteration]]]]),
              ordered = TRUE
            )
          }
        }
      }

    survey_data_for_power_bi_single <- df_to_use
    column_workbook_list = column_workbook_list_pb

    # Process the Select All Variables
    df_to_use <- survey_data_for_power_bi_single
    column_details <- column_workbook_list[[name_of_column_details]]

    # Detect sa questions with text responses
    select_all_text_vars <- column_details %>%
      dplyr::filter(type == "sa_other") %>%
      dplyr::pull(column_names)

    # If there are sa other variables
    if(length(select_all_text_vars) > 0) {
      # Copy them under a new name
      for(i in select_all_text_vars){
        df_to_use[[paste0("text_", i)]] <- df_to_use[[i]]
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
    for(i in unique_SELECT_ALL_var_labels) {

      # Obtain the variable names for this question
      list_of_variable_names <- df_to_use %>%
        dplyr::select(tidyselect::starts_with(i)) %>% colnames()

      # Check if all columns are missing data
      is_all_column_data_missing_for_row <- bkissell::check_if_all_are_missing_from_row(df_to_use, list_of_variable_names)

      # Go though each variable
      purrr::walk(list_of_variable_names, ~ {

        # Get the index for the var
        index_for_var_to_change <- which(colnames(df_to_use) == .x)

        # Select the variable
        vector_to_change <- df_to_use[,index_for_var_to_change][[1]]

        # Convert those that are missing everything to NA, but change all other NAs to zero
        df_to_use[,index_for_var_to_change] <<- bkissell::process_a_select_all_response_as_ones_and_zeros(
          is_all_column_data_missing_for_row,
          df_to_use[[.x]]
        )
      })
    }

    survey_data_for_power_bi_single <- df_to_use
    column_workbook_list = column_workbook_list_pb




    # Process the Numeric Variables
    # Prep the data
    data_used <- survey_data_for_power_bi_single

    # Create the df that just contains the column details
    column_details <- column_workbook_list[[name_of_column_details]]

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

      for(i in unique_scale_names_to_use) {
        label_for_the_score <- paste0("AVERAGE_SCORE__", snakecase::to_snake_case(i))
        final_scale_variable_names <- scales_details %>%
          dplyr::filter(.data[["final_scale_names"]] == i) %>%
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
        for(i in seq_along(orig_num_var_names)){
          iteration <- i
          converted_label <- orig_num_var_names[[iteration]]
          add_label <- stringr::str_replace(converted_label, "ORIG_", "")
          data_used[[add_label]] <- data_used[[converted_label]]
          data_used <- data_used %>% dplyr::select(-all_of(converted_label))
        }
      }
    }

    survey_data_for_power_bi_single <- data_used
    }, silent = TRUE)

    if(any(class(survey_data_for_power_bi_single) == "try-error") ){
      warning("data could not be processed")
      return(data.frame())
    } else {
      # survey_data_for_power_bi_single %>% glimpse()
      survey_data_for_power_bi_single <- survey_data_for_power_bi_single %>%
        dplyr::filter(!is.na(net_promoter))

      return(survey_data_for_power_bi_single)
    }
  }) %>%
    suppressWarnings() %>%
    purrr::reduce(dplyr::bind_rows)

  ##############################################################################

  # rlang::env_poke(env = my_env, "survey_data_for_power_bi_df", survey_data_for_power_bi_df)

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

  # rlang::env_poke(env = my_env, "text_survey_data", text_survey_data)

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
  #
  #   if(write_data){
  #     # Save a copy of the text data as a backup
  #     readr::write_csv(my_env$text_survey_data, my_env$text_survey_data_path)
  #   }

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

  # rlang::env_poke(env = my_env, "power_bi_text", power_bi_text)

  if(write_data){
    # Save it to the powerbi folder
    readr::write_csv(power_bi_text, power_bi_text_path)
  }

  if(file.exists(text_selected_example_text_survey_data_path)){

    read_and_add_param_to_column <- purrr::as_mapper(
      function(param_df) {
        purrr::map_df(seq_along(param_df$Var1), ~{
          iteration <- .x
          selected_examples_df <- readxl::read_excel(param_df$Var2[[iteration]], param_df$Var1[[iteration]])
          selected_examples_df$response_var_used <- param_df$Var1[[iteration]]
          selected_examples_df
        }, param_df)
      })

    selected_examples_text_df <- read_and_add_param_to_column(param_df = parameters_for_example_read)
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

    # rlang::env_poke(env = my_env, "combined_df_selected_examples_text_df", combined_df_selected_examples_text_df)

    if(write_data){
      readr::write_csv(combined_df_selected_examples_text_df, power_bi_text_selected_example_text_survey_data_path)
    }
  }




}
