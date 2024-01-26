#' FULL_qualitative_coding_data
#'
#' @param man_wd man_wd
#' @param coder_names coder_names
#' @param ext__initial_name_qual_coding ext__initial_name_qual_coding
#' @param text_names text_names
#' @param numeric_names numeric_names
#' @param multiple_choice_variables multiple_choice_variables
#' @param category_variables category_variables
#' @param remove_empty_sheets remove_empty_sheets
#' @param other_vars_that_should_not_be_counted other_vars_that_should_not_be_counted
#' @param data_folder_location data_folder_location
#' @param file_name_part file_name_part
#' @param sheets_to_exclude sheets_to_exclude
#' @param column_names_details_name column_names_details_name
#'
#' @return prepared_video_data
#' @export
#'
FULL_qualitative_coding_data <- function(
    man_wd = NULL,
    data_folder_location,
    file_name_part,
    coder_names,
    ext__initial_name_qual_coding,
    text_names,
    numeric_names,
    multiple_choice_variables,
    category_variables,
    remove_empty_sheets = TRUE,
    other_vars_that_should_not_be_counted,
    sheets_to_exclude = c("Template","Data Validation"),
    column_names_details_name = "Version 2_column_names.xlsx"
) {

  # If a manual working directory is provided,
  if(!is.null(man_wd)) {
    # Save current working directory
    current_wd <- getwd()
    # Change the working directory
    setwd(man_wd)
  }

  # Create path for the column name details
  column_names_details_path <- file.path(
    data_folder_location,
    column_names_details_name
  )

  # Create the column details list
  column_workbook_list <- bkissell::create_column_details_and_named_vectors_list(column_names_details_path)

  # Create a df that contains all of the paths and sheet names
  file_paths_df_all_paths <- bkissell::create_excel_file_paths_df(
    data_folder_location,
    file_name_part,
    unique_name_part = coder_names,
    file_ext = ext__initial_name_qual_coding,
    sheets_to_exclude
  )

  # Setup progress meter
  pb <- progress::progress_bar$new(
    format = "Reading and Processing Individual Sheet Data [:bar] :current/:total(:percent) in :elapsed",
    total = length(file_paths_df_all_paths[[1]]),
    clear = FALSE)

  # Process the video coding data
  sheet_coding_data <- bkissell::process_video_data(
    file_paths_df_all_paths,
    column_workbook_list
  )

  # Convert the data to the wide format
  data_for_calcs_wide <- prepare_data_for_calcs(
    sheet_coding_data,
    multiple_choice_variables
  )

  # Remove the Section 00 videos as it means that it was not coded
  if(remove_empty_sheets == TRUE) {
    data_for_calcs_wide <- data_for_calcs_wide %>%
      dplyr::filter(.data[["section_label"]] != "Section 00")
  }

  # Obtain the names of the columns in the wide format
  wide_column_names <- data_for_calcs_wide %>% names()

  # Create a filter for which columns not to count
  filter_out <- !(wide_column_names %in% {other_vars_that_should_not_be_counted})

  # Filter out those variables
  all_vars_to_count_duration <- wide_column_names[filter_out]

  # Calculate the first occurrence data
  prepared_first_occurence_data <- bkissell::prepare_first_occurrence_data(
    data_for_calcs_wide, all_vars_to_count_duration
  )

  # Calculate the video duration data
  prepared_video_duration_data <- bkissell::prepare_video_duration_data(
    data_for_calcs_wide, all_vars_to_count_duration
  )

  # Combine the the two video data types
  prepared_video_data <- prepared_video_duration_data  %>%
    dplyr::left_join(prepared_first_occurence_data, by = "video_name")

  # Round out the seconds
  prepared_video_data$total_seconds_duration <-
    prepared_video_data$total_seconds_duration %>%
    round(0)

  # If a manual working directory is provided, reset to original
  if(!is.null(man_wd)) {
    setwd(current_wd)
  }

  return(prepared_video_data)
}

#' process_video_data
#'
#' @param file_paths_df_all_paths file_paths_df_all_paths
#' @param column_workbook_list column_workbook_list
#'
#' @return sheet_coding_data_df
#' @export
#'
process_video_data <- function(
    file_paths_df_all_paths,
    column_workbook_list
){
  # Read in the data
  sheet_coding_data_df_list <-
    purrr::map(seq_along(file_paths_df_all_paths[[1]]), ~{
      sheet_coding_data <- bkissell::pvd__read_and_process_sheet_data(
        individual_file_information_df = file_paths_df_all_paths[.x,],
        column_workbook_list)

      sheet_coding_data$order_read_in <- .x
      # Add one to the progress meter
      pb$tick()
      return(sheet_coding_data)
    }, file_paths_df_all_paths, column_workbook_list)

  # Combine all of the data frames
  sheet_coding_data_df <- bind_rows(sheet_coding_data_df_list)

  # Return the variable
  return(sheet_coding_data_df)
}

#' pvd__read_and_process_sheet_data
#'
#' @param individual_file_information_df individual_file_information_df
#' @param column_workbook_list column_workbook_list
#'
#' @return sheet_coding_data
#' @export
#'
pvd__read_and_process_sheet_data <- function(
    individual_file_information_df,
    column_workbook_list
    ) {

  # Extract the column details from the workbook
  column_details_table <- bkissell::obtain_column_details_table(column_workbook_list)

  # Prepare to read data
  the_col_names_original <- column_details_table$original_name
  the_col_names_correct <- column_details_table$column_names
  the_col_types <- column_details_table$data_type
  path_to_excel_sheet <- individual_file_information_df[["file_path"]]
  sheet_name_to_read <- individual_file_information_df[["sheet_name"]]

  # Read in data
  sheet_coding_data <- readxl::read_excel(
    path_to_excel_sheet,
    sheet = sheet_name_to_read,
    col_types = the_col_types
    )

  # If there is an issue with the names, throw an error.
  if(!all(colnames(sheet_coding_data) == the_col_names_original)) {
    rlang::abort(glue::glue("Column names from {path_to_excel_sheet} - {sheet_name_to_read} do not match {paste0(the_col_names_original, collapse = ', ')}"))
  }

  # Correct the names
  colnames(sheet_coding_data) <- the_col_names_correct

  # Get all of the single text variables
  single_text_variables <- column_details_table %>%
    dplyr::filter(.data[["type"]] == "single_text") %>%
    dplyr::pull(.data[["column_names"]])

  # Add that data to the entire dataset
  if(length(single_text_variables) > 0) {
    sheet_coding_data <- sheet_coding_data %>%
      mutate(across(all_of(single_text_variables), ~{.x[[1]]}))
  }

  # Convert starts into 1 and 0s
  section_number <-
    ifelse(sheet_coding_data$section == "Start of Section", 1, 0) |>
    tidyr::replace_na(0) |>
    cumsum() |>
    stringr::str_pad(width = 3, side = "left", pad = "0")

  # Create the section label
  sheet_coding_data$section_label <- paste0("Section ", section_number)

  # Get the name of the coder
  coded_by <- stringr::str_extract(
    individual_file_information_df["file_path"],
    "__.+xlsx$"
  ) |>
    stringr::str_replace("__", "") |>
    stringr::str_replace(".xlsx$", "") |>
    snakecase::to_snake_case()

  # Add coded data to df
  sheet_coding_data$coded_by <- coded_by

  # Get the multiple choice variables
  multiple_choice_variables <- column_details_table %>%
    dplyr::filter(.data[["type"]] == "multiple_choice") %>%
    dplyr::pull(.data[["column_names"]])

  # Convert data to characters and snakecase
  if(length(multiple_choice_variables > 0)) {
    sheet_coding_data_df <- sheet_coding_data |>
      dplyr::mutate(
        dplyr::across(
          starts_with(multiple_choice_variables),
          snakecase::to_snake_case
        )
      )
  }

  # Get the logical variables
  logical_variables <- column_details_table %>%
    dplyr::filter(.data[["type"]] == "logical") %>%
    dplyr::pull(.data[["column_names"]])

  # Convert NAs to 0
  if(length(logical_variables > 0)) {
    sheet_coding_data_df <- sheet_coding_data_df |>
      dplyr::mutate(
        dplyr::across(
          logical_variables,
          ~{tidyr::replace_na(.x, 0)}
        )
      )
  }

  # Re-order the data
  sheet_coding_data_df <- sheet_coding_data_df |>
    select("section_label", everything())

  # Get the duration of the video
  sheet_coding_data_df <- sheet_coding_data_df |>
    dplyr::group_by(.data[["video_name"]]) |>
    dplyr::mutate(
      duration_of_video = .data[["time_point"]] |> max() |> round()
    )

  # Return the object
  return(sheet_coding_data_df)
}




#' prepare_data_for_calcs
#'
#' @param sheet_coding_data sheet_coding_data created with `process_video_data`
#' @param multiple_choice_vars multiple_choice_vars
#'
#' @return wide_data_for_calcs
#' @import dplyr
#' @export
#'
prepare_data_for_calcs <- function(sheet_coding_data, multiple_choice_vars) {

  # multiple_choice_vars <- create_multiple_choice_variables_for_coding_data()

  # Prep the data
  data_for_calcs <- sheet_coding_data

  data_for_calcs$video_name <- paste0(data_for_calcs$video_name, "_", data_for_calcs$coded_by, "_", data_for_calcs$order_read_in)


  # data = data_for_calcs
  # mc_var_name = "visual_type"
  # Create a helper function that creates the multiple choice tables
  pavd_create_mc_df_table <- function(data, mc_var_name, video_name_var = "video_name", time_point_var = "time_point") {


    # Remove missing data
    data <- data %>% dplyr::filter(!is.na(.data[[{{mc_var_name}}]]))

    # Create data to help with the pivot
    data$element_present <- 1

    # Pivot the table
    mc_data_for_calcs <- data %>%
      dplyr::select({{video_name_var}}, {{time_point_var}}, {{mc_var_name}}, "element_present") %>%
      tidyr::pivot_wider(
        id_cols = c(everything()),
        names_from = .data[[{{mc_var_name}}]],
        names_glue = paste0({{mc_var_name}}, "__{.name}"),
        values_from = .data[["element_present"]]
      ) %>%
      dplyr::mutate(dplyr::across(tidyselect::starts_with(paste0({{mc_var_name}}, "__")), ~tidyr::replace_na(.x, 0)))

    # Return the table
    return(mc_data_for_calcs)
  }

  # add to the multiple choice vector
  the_multiple_choice_vars <- c(multiple_choice_vars, "coded_by", "duration_of_video")

  # Create the tables for the multiple choice variable
  list_of_data_for_calcs <- purrr::map(the_multiple_choice_vars, ~{
    pavd_create_mc_df_table(
      data = data_for_calcs,
      mc_var_name = {{.x}},
      video_name_var = "video_name",
      time_point_var = "time_point"
    )
  }, data_for_calcs)

  # Join all of the dfs together
  new_data_for_calcs <- list_of_data_for_calcs %>%
    purrr::reduce(dplyr::left_join, by = c("video_name", "time_point"))

  # Replace the na
  wide_multiple_choice_df <- new_data_for_calcs %>%
    dplyr::mutate(across(everything(), ~tidyr::replace_na(.x, 0)))

  # Join the mc data
  wide_data_for_calcs <- data_for_calcs %>%
    dplyr::left_join(wide_multiple_choice_df, by = c("video_name", "time_point"))

  # Return the data
  return(wide_data_for_calcs)
}

#' prepare_first_occurrence_data
#'
#' @param data_for_calcs_wide data_for_calcs_wide
#' @param all_vars_to_count_duration ll_vars_to_count_duration
#'
#' @return prepared_first_occurence_data
#' @export
#'
prepare_first_occurrence_data <- function(data_for_calcs_wide, all_vars_to_count_duration) {

  video_names_df <- data.frame(video_name = data_for_calcs_wide$video_name %>% unique())

  df_for_new_vars <- purrr::map_dfc(all_vars_to_count_duration, ~{

    new_var_name <- paste0("first_occurence__", {{.x}})

    first_occur_df_prep <- data_for_calcs_wide %>%
      dplyr::filter(.data[[{{.x}}]] == 1)

    if(nrow(first_occur_df_prep) == 0) {
      new_var_for_df <- data.frame(video_name = unique(data_for_calcs_wide$video_name))

      new_var_for_df <- new_var_for_df %>%
        group_by(video_name) %>%
        dplyr::summarize({{new_var_name}} := NA)

    } else {
      new_var_for_df <- first_occur_df_prep %>%
        dplyr::filter(.data[[{{.x}}]] == 1) %>%
        dplyr::group_by(video_name) %>%
        dplyr::summarize({{new_var_name}} := min(time_point, na.rm = TRUE))
    }

    video_names_df %>%
      dplyr::left_join(new_var_for_df, by = "video_name") %>%
      dplyr::select(-"video_name")
  }, data_for_calcs_wide, video_names_df)

  prepared_first_occurence_data <- cbind(video_names_df, df_for_new_vars)

  return(prepared_first_occurence_data)
}


#' prepare_video_duration_data
#'
#' @param data_for_calcs_wide data_for_calcs_wide
#' @param all_vars_to_count_duration all_vars_to_count_duration
#'
#' @return prepared_video_duration_data
#' @import dplyr
#' @export
#'
prepare_video_duration_data <- function(data_for_calcs_wide, all_vars_to_count_duration) {

  section_duration_df <- data_for_calcs_wide %>%
    dplyr::group_by(.data[["section_label"]], .data[["video_name"]], .data[["duration_of_video"]]) %>%
    dplyr::mutate(across(all_of(all_vars_to_count_duration), ~as.numeric(.x))) %>%
    dplyr::summarize(
      ms_duration = n() * 3,
      across(all_of(all_vars_to_count_duration), ~sum(.x) * 3, .names = "duration_of_{.col}"), .groups = "drop")

  prepared_video_duration_data <- section_duration_df %>%
    dplyr::group_by(.data[["video_name"]]) %>%
    dplyr::summarize(
      n_sections = n(),
      mean_section_seconds_duration = base::mean(.data[["ms_duration"]], na.rm = TRUE) / 90 %>% round(2),
      sd_section_seconds_duration = stats::sd(.data[["ms_duration"]], na.rm = TRUE) / 90 %>% round(2),
      total_seconds_duration = sum(.data[["ms_duration"]], na.rm = TRUE) / 90  %>% round(2),
      across(starts_with("duration_of_"), ~ sum(.x, na.rm = TRUE) / 90 %>% round(2), .names = "sum_of_seconds_{.col}")
    )

  return(prepared_video_duration_data)
}
