#' ################################################################################
#' # Create the URLs for the participant data pages ---------------------------
#'
#' #' create_user_participant_urls
#' #'
#' #' @param project_numbers Project numbers
#' #'
#' #' @return user_participant_urls
#' #' @export
#' #'
#' create_user_participant_urls <- function(project_numbers) {
#'   # Create a url for every project number
#'   user_participant_urls <- purrr::map_chr(project_numbers, ~{
#'     paste0("https://www.userinterviews.com/projects/", .x, "/participants")
#'   })
#'
#'   # return the vector
#'   return(user_participant_urls)
#' }
#'
#' ################################################################################
#' # Prepare User Participant Data File Paths for Processing -----------------
#'
#'
#' #' prepare_UPD_file_paths_for_processing
#' #' Prepare User Participant Data File Paths for Processing
#' #'
#' #' @param home_dir Home directory for the study
#' #' @param download_location Where the download folder is located.
#' #' @param project_numbers project numbers
#' #'
#' #' @return path_for_UPD_files
#' #' @export
#' #'
#' prepare_UPD_file_paths_for_processing <- function(
#'     home_dir,
#'     download_location,
#'     project_numbers
#' ) {
#'   # Create the path where the UPD files will be temporarily held
#'   path_for_UPD_folder <- paste0(home_dir, "/Data Collection/Project Files/Recruitment/user_participant_data")
#'
#'   # Obtain the names of all of csv files in the download folder
#'   file_names_located_in_download_folder <- list.files(download_location, pattern = ".csv$")
#'
#'   # Detect which files should be moved
#'   file_should_be_moved <- file_names_located_in_download_folder %>%
#'     stringr::str_detect("participants.+csv")
#'
#'   # Select files that should be moved
#'   file_names_located_in_download_folder_to_move <- file_names_located_in_download_folder[file_should_be_moved]
#'
#'   # If nothing is in it
#'   if(identical(file_names_located_in_download_folder, character(0))){
#'     path_to_file_in_download_folder <- character(0)
#'     path_df <- data.frame()
#'   } else {
#'     # Create path to file in download location
#'     path_to_file_in_download_folder <- paste0(download_location, "/", file_names_located_in_download_folder)
#'     # Put file information into a dataframe
#'     path_df <- data.frame(path_for_UPD_folder, file_names_located_in_download_folder, path_to_file_in_download_folder)
#'   }
#'
#'   # Extract info about whether it is a copy or not
#'   path_df$copy_number <- file_names_located_in_download_folder %>%
#'     stringr::str_extract("\\([0-9]\\).csv$") %>%
#'     stringr::str_extract("[0-9]{1,2}") %>%
#'     as.numeric() %>%
#'     tidyr::replace_na(0)
#'
#'   # Clean names
#'   path_df$project_file_name <- file_names_located_in_download_folder %>%
#'     stringr::str_replace("-participant.+$", "") %>%
#'     snakecase::to_snake_case()
#'
#'   # Cause an error if the download folder is empty
#'   if(identical(file_names_located_in_download_folder, character(0))) {
#'     rlang::abort("No files are in the downloads folder. No files will be moved.")
#'   } else if(length(path_to_file_in_download_folder) < length(project_numbers)) {
#'     rlang::abort("There is not a file for every project in the download folder. No files will be moved.")
#'   } else {
#'
#'     # Only obtain the most recent copies
#'     path_df_to_use <- path_df %>%
#'       group_by(project_file_name) %>%
#'       dplyr::slice_max(order_by = copy_number, n = 1)
#'
#'     # Prepare path where you are getting it from
#'     from_files <- path_df_to_use %>%
#'       pull(path_to_file_in_download_folder)
#'
#'     # Prepare file names
#'     project_file_name <- path_df_to_use %>%
#'       pull(project_file_name)
#'
#'     # Prepare paths for where it should be sent
#'     path_df_to_use$path_for_UPD_files_to <- paste0(path_df_to_use$path_for_UPD_folder, "/", project_file_name, ".csv")
#'
#'     # Extract the paths
#'     path_for_UPD_files <- path_df_to_use %>%
#'       pull(path_for_UPD_files_to)
#'
#'     # Grab the paths for the files that do not need to be read, and can just be removed
#'     paths_to_remove <- path_df$path_to_file_in_download_folder[which(!(path_df$path_to_file_in_download_folder %in% from_files))]
#'
#'     # Send files to the correct location, depending on whether files were downloaded or not
#'     purrr::walk2(from_files, path_for_UPD_files, ~{bktools::my_file_rename(.x, .y)})
#'
#'     # Remove the unnecessary files from download folder
#'     purrr::walk(paths_to_remove, ~ file.remove(.x))
#'   }
#'
#'   # Return Paths
#'   return(path_for_UPD_files)
#' }
#'
#' ################################################################################
#' # Clean up the upd --------------------------------------------------------
#'
#'
#' #' Clean the upd df
#' #'
#' #' @param user_data_df dataframe containing the user participant data
#' #' @param age_group_levels what age groups should be included
#' #'
#' #' @return user_data
#' #' @export
#' #'
#' clean_UPD_df <- function(user_data_df, age_group_levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")){
#'   # Remove participants who have been labeled "Deleted"
#'   user_data <- dplyr::filter(user_data_df, first_name != "Deleted")
#'
#'   # Take out duplicate rows
#'   user_data <- dplyr::distinct(user_data)
#'
#'   # Extract the numeric value for the age group
#'   min_level_age <-  as.numeric(stringr::str_extract(age_group_levels, "^[0-9]{2}"))
#'
#'   # Set-up Age Groups
#'   user_data$age_group <- AMR::age_groups(as.numeric(user_data$age), split_at = min_level_age, na.rm = FALSE)
#'
#'   # Set-up Ethnicity
#'   user_data$race_ethnicity <- dplyr::case_when(
#'     user_data$race_ethnicity == "Hispanic or Latino" ~ "Hispanic",
#'     user_data$race_ethnicity == "White" ~ "White",
#'     user_data$race_ethnicity == "Black or African American" ~ "African American",
#'     user_data$race_ethnicity == "Asian" ~ "Asian",
#'     user_data$race_ethnicity == "native american" ~ "American Indian",
#'     TRUE ~ "Other"
#'   )
#'
#'   # Set-up state
#'   user_data$state <- user_data$region
#'
#'   # Create state vectors
#'   Northeast_states <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont", "Northeast")
#'   Midwest_states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin", "Midwest")
#'   West_states <- c("Alaska", "Arizona", "California", "Colorado", "Hawaii", "Idaho", "Montana", "Nevada", "New Mexico", "Oregon", "Utah", "Washington", "Wyoming", "West")
#'   South_states <- c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia", "South")
#'
#'   # Set-up Region
#'   user_data$region <- dplyr::case_when(
#'     user_data$region %in% Northeast_states ~ "Northeast",
#'     user_data$region %in% Midwest_states ~ "Midwest",
#'     user_data$region %in% West_states ~ "West",
#'     user_data$region %in% South_states ~ "South",
#'     TRUE ~ "ERROR"
#'   )
#'
#'   # # Set up income levels vectors
#'   # income_150 <- c("$150,000 - $199,999", "> $200,000")
#'   # income_100 <- c("$100,000 - $149,999")
#'   # income_50 <- c("$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $89,999", "$90,000 - $99,999")
#'   # income_20 <- c("<$20,000", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999")
#'
#'   # # Set up income labels
#'   # user_data$household_income <- dplyr::case_when(
#'   #   user_data$household_income %in% income_150 ~ "150+",
#'   #   user_data$household_income %in% income_100 ~ "100-150k",
#'   #   user_data$household_income %in% income_50 ~ "50-100k",
#'   #   user_data$household_income %in% income_20 ~ "Under 50k",
#'   #   TRUE ~ "Other"
#'   # )
#'
#'   # Set up income labels
#'   user_data$household_income  <- factor(user_data$household_income, c("< $30,000", "$30,000 - $39,999", "$40,000 - $59,999", "$60,000 - $79,999", "$80,000 - $99,999", "$100,000 - $149,999", "$150,000 - $199,999", "> $200,000"), ordered = TRUE)
#'
#'   # Set-up Children status
#'   user_data$children <- dplyr::case_when(
#'     stringr::str_detect(as.character(user_data$children), "Adult") ~ "Adult Children",
#'     stringr::str_detect(as.character(user_data$children), "No children") ~ "No Children",
#'     TRUE ~ "Under 18 Children"
#'   )
#'
#'   # Set up gender (labeling gender2, as that is what it has been for a while)
#'   user_data$gender2 <- dplyr::case_when(
#'     user_data$gender == "Woman" ~ "Female",
#'     user_data$gender == "Man" ~ "Male",
#'     TRUE ~ "Other"
#'   )
#'
#'   # Set up ethnicity
#'   user_data$ethnicity <- dplyr::case_when(
#'     user_data$race_ethnicity == "African American" ~ "African American",
#'     TRUE ~ "Not African American"
#'   )
#'
#'   # Create the names of participants variable
#'   user_data$names <- paste0(user_data$first_name, " ", user_data$last_name)
#'
#'   # Organize Columns
#'   user_data <- dplyr::select(user_data, names, everything(), state)
#'
#'   # Remove those who are not qualified
#'   user_data <- dplyr::filter(user_data, participant_status != "Unqualified")
#'
#'   # # Rename real duplicates
#'   # user_data <- user_data %>%
#'   #   dplyr::mutate(names = ifelse(names %in% c("Michael Allen", "Patrick Brown"), paste0(names, " ", as.character(age)), names))
#'   # # Remove specific ids (for duplicate issues)
#'   # user_data <- dplyr::filter(user_data, !(id %in% c("pp_k0gho7xj4j", "pp_k0gho7xg5g", "pp_rlkhyrv090", "pp_yy3hded137", "pp_9exh510356")))
#'   return(user_data)
#' }
#'
#' ################################################################################
#' # Read and process user data ----------------------------------------------
#'
#'
#' #' read_and_process_UPD
#' #' Read and process user data
#' #'
#' #' @param upd_paths paths for the data to read
#' #' @param Clip_data should it put the files in the clipboard
#' #'
#' #' @return user_participant_data
#' #' @export
#' #'
#'
#' read_and_process_UPD <- function(upd_paths, Clip_data = FALSE) {
#'   # Read each of the files
#'   user_data_df <- purrr::map_df(upd_paths, ~ readr::read_csv(.x, show_col_types = FALSE))
#'
#'   initial_col_names <- colnames(user_data_df)
#'
#'   column_names_folder <- paste0(dirname(upd_paths)[[1]], "/", "col_names")
#'
#'   column_names_file <- paste0(column_names_folder, "/column_names.csv")
#'
#'   if(!file.exists(column_names_file)) {
#'     characteristics_vars <- c(
#'       "id", "participant_status", "screener_version", "response_time", "review_tier",
#'       "date_added", "other_id", "first_name", "last_name", "email", "phone_number",
#'       "timezone", "occupation", "age", "locality", "region", "country", "browsers",
#'       "children", "company_size", "computer_with_a_webcam", "computer_operating_system",
#'       "employment_status", "gender", "home_owner", "household_income", "industry",
#'       "level_of_education", "living_situation", "marital_status", "race_ethnicity",
#'       "seniority", "small_business_owner", "smartphone_manufacturer", "smartphone_operating_system",
#'       "tablet_operating_system", "type_of_income", "session", "internet_connection",
#'       "available_devices", "cord_cutter", "available_devices_2", "fitness_watch",
#'       "smart_watch", "state_of_residence", "donated_within_12_months", "gave_to_these_organizations",
#'       "frequency_of_charitable_giving", "read_and_follow_instructions", "download_app"
#'     )
#'
#'     if(length(characteristics_vars) > length(initial_col_names)) {
#'       characteristics_vars <- characteristics_vars[seq(1, length(initial_col_names))]
#'     } else if(length(characteristics_vars) < length(initial_col_names)) {
#'       n_to_add <- length(initial_col_names) - length(characteristics_vars)
#'       characteristics_vars <- c(characteristics_vars, rep("NA", n_to_add))
#'     }
#'
#'     col_names_df <- data.frame(
#'       old_name = initial_col_names,
#'       new_name = initial_col_names,
#'       possible = characteristics_vars
#'     )
#'
#'     readr::write_csv(col_names_df, column_names_file)
#'
#'     stop("Please fix the column names in dropbox")
#'
#'   } else {
#'     col_names_df <- readr::read_csv(column_names_file, show_col_types = FALSE)
#'   }
#'
#'   colnames(user_data_df) <- col_names_df$new_name
#'
#'   ## ------------------ run code for all upd_paths ----------------- ##
#'   # # Consolidate and read files
#'   # user_data_df <- purrr::map_df(upd_paths, consolidate_repeated_error_columns_for_one)
#'   # Clean and process files
#'   user_data <- clean_UPD_df(user_data_df)
#'
#'   user_data <- user_data %>%
#'     dplyr::select(-other_id)
#'
#'   #If indicated, add df to the clipboard, otherwise just return the dataframe
#'   if(Clip_data == TRUE){
#'     # Save the data to your clipboard so you can paste it into the workbook
#'     clipr::write_clip(user_data)
#'   }
#'   return(user_data)
#' }
#'
#' ################################################################################
#' # Process the scraped data ------------------------------------------------
#'
#' #' Process scraped schedules
#' #'
#' #' @param home_dir home_dir
#' #'
#' #' @return scrape_df
#' #' @export
#' #'
#' process_scraped_schedules <- function(home_dir){
#'   # Obtain location of the schedule folder
#'   path_for_scraped_schedule_folder <- paste0(home_dir, "/Data Collection/Project Files/Recruitment/Schedule")
#'
#'   # Obtain file names
#'   html_schedule_file_names <- list.files(path_for_scraped_schedule_folder, pattern = ".html")
#'
#'   # Obtain file paths
#'   paths_for_html_schedule <- paste0(path_for_scraped_schedule_folder, "/", html_schedule_file_names)
#'
#'   # Obtain location of the combined folder
#'   dir_for_html_combined <- paste0(path_for_scraped_schedule_folder, "/combined")
#'
#'   # Combined timeslots paths
#'   html_combined_file_location <- paste0(dir_for_html_combined, "/", bktools::combine_file_name_with_time("combined_timeslots_"))
#'
#'   # Cleaned file names
#'   html_schedule_file_names_2 <- stringr::str_replace(html_schedule_file_names, ".html", "")
#'
#'   # Read the html files
#'   html_list <- purrr::map(paths_for_html_schedule, ~{rvest::read_html(.x)})
#'
#'   # Obtain the ui-cards
#'   ui_cards_list <- purrr::map(html_list, ~{rvest::html_nodes(.x, ".ui-card")})
#'
#'   # Detect the number of sessions
#'   number_of_sessions <- purrr::map_dbl(ui_cards_list, ~{length(.x %>% rvest::html_children())})
#'
#'   if(sum(number_of_sessions) != 0) {
#'
#'     # Create a vector that has the project name for every time slot
#'     session_project_names <- purrr::map2(html_schedule_file_names_2, number_of_sessions, ~{rep(.x, .y)}) %>% purrr::flatten() %>% unlist()
#'
#'     # Get a list of all of the sections
#'     ui_card_sections_list <- purrr::map(ui_cards_list, ~{.x %>% rvest::html_children()}) %>% purrr::flatten()
#'
#'     # Get the form list for the date
#'     date_form_list <- purrr::map(ui_card_sections_list, ~{(.x %>% rvest::html_children())[[2]]})
#'
#'     # Extract it
#'     date_form_list_2 <- purrr::map( date_form_list, ~{(.x %>% rvest::html_children())})
#'
#'     # Find where the date is located
#'     which_has_date <- purrr::map(date_form_list_2, ~{which(.x %>% rvest::html_attr("class") == "SessionView__header")})
#'
#'     # Clean this variable
#'     has_date <- purrr::map_dbl(which_has_date, ~{ifelse(identical(.x, integer(0)), 0, .x)})
#'
#'     # Obtain the Date for all of the sections
#'     date_vector <- purrr::map2_chr(date_form_list_2, has_date, ~{ifelse(.y == 1, (.x[[.y]] %>% rvest::html_text()), NA)}) %>% bktools::na_fill()
#'
#'     # Find where the time is located
#'     which_has_time <- purrr::map(date_form_list_2, ~{which(.x %>% rvest::html_attr("class") == "SessionView__details")})
#'
#'     # Clean this variable
#'     has_time <- purrr::map_dbl(which_has_time, ~{ifelse(identical(.x, integer(0)), 0, .x)})
#'
#'     # Obtain the Time Slot Form for all sections
#'     time_slot_form <- purrr::map(date_form_list_2, ~{.x %>% rvest::html_children()})
#'
#'     # Obtain the Time for all of the sections
#'     time_vector <- purrr::map2_chr(date_form_list_2, which_has_time, ~{
#'       ifelse(
#'         .y > 0,
#'         .x[[.y]]  %>% rvest::html_text() %>% stringr::str_extract("[0-9].+CDT"),
#'         NA)
#'     })
#'     # rvest::html_children())[[1]] %>%
#'     # rvest::html_text()), NA)
#'
#'
#'     # Find where the Ntimeslots are located
#'     which_has_Ntimeslots <- purrr::map(time_slot_form, ~{.x %>% rvest::html_attr("class") == "session-num-slots"})
#'
#'     # Clean the missing data
#'     which_has_Ntimeslots <- purrr::map(which_has_Ntimeslots, ~{tidyr::replace_na(.x, replace = 0)})
#'
#'     # Clean this variable
#'     has_Ntimeslots <- purrr::map_dbl(which_has_Ntimeslots, ~{ifelse(identical(.x, integer(0)), 0, which(.x))})
#'
#'     # Obtain the Time for all of the sections
#'     Ntimeslots_vector <- purrr::map2_chr(time_slot_form, has_Ntimeslots, ~{ifelse(.y > 0, .x[[.y]] %>% rvest::html_text(), NA)})
#'
#'     # How many people are actually signed up?
#'     number_of_real_signups <- stringr::str_extract(Ntimeslots_vector, "[0-9]{1,2}/") %>% stringr::str_replace("/", "") %>% as.numeric()
#'
#'     # How many potential slots are there?
#'     number_of_potential_signups <- stringr::str_extract(Ntimeslots_vector, "/[0-9]{1,2}") %>% stringr::str_replace("/", "") %>% as.numeric()
#'
#'     # How many empty timeslots
#'     number_of_empty_slots <- number_of_potential_signups - number_of_real_signups
#'
#'     # Create a time_slot vector
#'     date_time_vector_chr <- paste0(date_vector, " ", time_vector)
#'
#'
#'     obtain_the_signups_1_file <- function(a_time_slot_form, a_number) {
#'       names_form <- a_time_slot_form %>% rvest::html_children() %>% rvest::html_children() %>% rvest::html_children() %>% rvest::html_children() %>% rvest::html_children()
#'       where_names <- which(names_form %>% rvest::html_attr("class") == "ProfileCell__content__name")
#'       names_list <- names_form[where_names] %>% rvest::html_text()
#'       names_list_w_empty <- c(names_list, rep("", number_of_empty_slots[[a_number]]))
#'       timeslot_w_empty <-  rep(date_time_vector_chr[[a_number]], length(names_list_w_empty))
#'       data.frame(names = names_list_w_empty, timeslot = timeslot_w_empty, project_file_name = session_project_names[a_number])
#'     }
#'
#'     # Obtain the sign-ups
#'     sign_ups <- purrr::map2_df(time_slot_form, seq_along(date_time_vector_chr), ~{
#'       obtain_the_signups_1_file(.x, .y)
#'       # names_form <- .x %>% rvest::html_children() %>% rvest::html_children() %>% rvest::html_children() %>% rvest::html_children() %>% rvest::html_children()
#'       # where_names <- which(names_form %>% rvest::html_attr("class") == "ProfileCell__content__name")
#'       # names_list <- names_form[where_names] %>% rvest::html_text()
#'       # names_list_w_empty <- c(names_list, rep("", number_of_empty_slots[[.y]]))
#'       # timeslot_w_empty <-  rep(date_time_vector_chr[[.y]], length(names_list_w_empty))
#'       # data.frame(names = names_list_w_empty, timeslot = timeslot_w_empty, project_file_name = session_project_names[.y])
#'     })
#'
#'     # Clean up day, date, and time
#'     sign_ups$day <- stringr::str_extract(sign_ups$timeslot, ".+day")
#'     use_to_get_date <- stringr::str_replace(sign_ups$timeslot, ".+day - ", "")
#'
#'     sign_ups$date <- stringr::str_extract(use_to_get_date, "^[A-z]+ [0-9]{1,2}th, [0-9]{4}")
#'     use_to_get_time <- stringr::str_replace(use_to_get_date, "^[A-z]+ [0-9]{1,2}th, [0-9]{4} ", "")
#'
#'     sign_ups$time <- stringr::str_extract(use_to_get_time, "([0-9]{1,2}:[0-9]{2} [AP]M) -") %>% stringr::str_replace_all(" -", "")
#'
#'     # Prepare to clean
#'     scrape_df <- sign_ups
#'
#'     # Get the weekday from the date
#'     scrape_df$weekday <- stringr::str_extract(scrape_df$timeslot, pattern = "Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday")
#'
#'     # Get the date from the date
#'     scrape_df$day <- stringr::str_extract( use_to_get_date, pattern = "1st|2nd|3rd|4th|5th|6th|7th|8th|9th|10th|11th|12th|13th|14th|15th|16th|17th|18th|19th|20th|21st|22nd|23rd|24th|25th|26th|27th|28th|29th|30th|31st")
#'
#'     # Get the month from the date
#'     scrape_df$month <- stringr::str_extract(use_to_get_date, pattern = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec")
#'
#'     # Create a list of the months
#'     possible_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#'
#'     # Create a list of the numeric values for the months
#'     numeric_months <- as.character(1:12)
#'
#'     # Prepare the column that will become numeric month
#'     scrape_df$month_numeric <- scrape_df$month
#'
#'     # Loop through every month
#'     for(i in seq_along(possible_months)){
#'       # Replace the month with the proper number
#'       scrape_df$month_numeric <- stringr::str_replace(scrape_df$month_numeric, possible_months[[i]], numeric_months[[i]])
#'     }
#'
#'     # Change it to numeric
#'     scrape_df$month_numeric <- as.numeric(scrape_df$month_numeric)
#'
#'     # Extract the day
#'     scrape_df$day <- stringr::str_extract(scrape_df$day, "[0-9]{1,2}")
#'
#'     # Make it numeric
#'     scrape_df$day <- as.numeric(scrape_df$day)
#'     # scrape_df$time <- stringr::str_extract(scrape_df$timeslot, "[0-9]{2}:[0-9]{2} [AP]M - ")
#'
#'     # Extract the hour
#'     scrape_df$hour <- as.numeric(stringr::str_extract(scrape_df$time, "^[0-9]{1,2}"))
#'
#'     # Remove the hour
#'     time_to_clean <- stringr::str_replace(use_to_get_time, "^[0-9]{1,2}:", "")
#'
#'     # Extract the minutes
#'     scrape_df$minute <- stringr::str_extract(time_to_clean, "^[0-9]{1,2}")
#'
#'     # Remove the minutes
#'     time_to_clean <- stringr::str_replace(time_to_clean , "^[0-9]{2}\\s", "")
#'
#'     # Extract the period
#'     scrape_df$period <- stringr::str_extract(time_to_clean, "^[a-zA-Z]{2}")
#'
#'     # Extract the year
#'     scrape_df$year <- stringr::str_extract(scrape_df$timeslot, "[0-9]{4}")
#'
#'     # Convert the hour to military time
#'     scrape_df$military_hour <- ifelse(scrape_df$period == "AM", scrape_df$hour, ifelse(scrape_df$hour == 12, scrape_df$hour, scrape_df$hour + 12))
#'
#'     # Put the data frame columns in the desired order
#'     scrape_df <- scrape_df %>% dplyr::select(tidyselect::all_of("weekday"), tidyselect::all_of("month"), tidyselect::all_of("day"), tidyselect::all_of("hour"), tidyselect::all_of("minute"), dplyr::everything())
#'
#'     # Order the rows by date
#'     scrape_df <- scrape_df %>% dplyr::arrange(.data[["month_numeric"]], .data[["day"]], .data[["military_hour"]], .data[["minute"]])
#'
#'     # Create session date string
#'     scrape_df$Session_Date <- glue::glue("{scrape_df$month_numeric}/{scrape_df$day}/{scrape_df$year}")
#'
#'     # Create session time string
#'     scrape_df$Session_Time <- glue::glue("{scrape_df$military_hour}:{scrape_df$minute}")
#'     scrape_df$Data_Cleaning_Notes = ""
#'
#'     # Put the df in the desired order
#'     scrape_df <- scrape_df %>% dplyr::select(tidyselect::all_of("Session_Date"), tidyselect::all_of("Session_Time"), tidyselect::all_of("project_file_name"), tidyselect::all_of("Data_Cleaning_Notes"), tidyselect::all_of("names"), tidyselect::all_of("weekday"))
#'
#'     # Write the file to dropbox
#'     readr::write_csv(scrape_df, html_combined_file_location)
#'
#'   } else {
#'
#'     # Create an empty df
#'     scrape_df <- data.frame(
#'       Session_Date = NA,
#'       Session_Time = NA,
#'       project_file_name = NA,
#'       Data_Cleaning_Notes = NA,
#'       names = NA,
#'       weekday = NA
#'     )
#'
#'     # Write the file to dropbox
#'     readr::write_csv(scrape_df, html_combined_file_location)
#'   }
#'
#'   # Return the df
#'   return(scrape_df)
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' #
#' #
#' # scrape_schedule_from_html_file_new <- function(html_file_paths){
#' #
#' #   for(i in html_file_paths){
#' #     html <- rvest::read_html(i)
#' #
#' #     # Grab all of the ui cards
#' #     ui_cards <- rvest::html_nodes(html, ".ui-card")
#' #
#' #     ui_card_sections <- purrr::map(ui_cards, ~{.x %>% rvest::html_children()})
#' #
#' #     # Get the date form
#' #     date_form <- purrr::map(ui_card_sections, ~{(.x %>% rvest::html_children())[[2]]})
#' #
#' #     # and extract the date
#' #     date <- purrr::map_chr(date_form, ~{(.x %>% rvest::html_children())[1] %>% rvest::html_text()})
#' #
#' #     # Get the time_slot_form
#' #     time_slot_form <- purrr::map(date_form, ~{
#' #       .x %>% rvest::html_children()
#' #     })
#' #
#' #     # Extract the time slot data
#' #     time_slot <- purrr::map(time_slot_form, ~{
#' #       (.x[[2]] %>% rvest::html_children())[[1]] %>% rvest::html_text()
#' #     }) %>% unlist()
#' #
#' #
#' #     # Initiate vectors for the variables we want to collect
#' #     time_slots <- c()
#' #     names <- c()
#' #     date_vector <- c()
#' #     personal_links <- c()
#' #     moderators <- c()
#' #
#' #     # Go through every card
#' #     for(i in seq_along(ui_cards)){
#' #
#' #       # # Begin going through the section
#' #       # ui_card_sections <- ui_cards[[i]] %>% rvest::html_children()
#' #
#' #       for(j in seq_along(ui_card_sections)){
#' #
#' #         # # Get the date form
#' #         # date_form <- (ui_card_sections[[j]] %>% rvest::html_children())[[2]]
#' #
#' #         # # and extract the date
#' #         # date <- ((date_form %>% rvest::html_children())[[1]] %>% rvest::html_children())[[1]] %>% rvest::html_text()
#' #
#' #         # # Get the time_slot_form
#' #         # time_slot_form <- date_form %>% rvest::html_children()
#' #
#' #         # # Extract the time slot data
#' #         # time_slot <- (time_slot_form[[2]] %>% rvest::html_children())[[1]] %>% rvest::html_text()
#' #       }
#' #
#' #
#' #       # time_slot_form[[2]]  %>% rvest::html_text()
#' #       # purrr::map(time_slot_form, rvest::html_text)
#' #
#' #       # moderator_line <- (date_form %>% rvest::html_children())[[6]] %>% rvest::html_children()
#' #       # moderator <- ((moderator_line[[1]] %>% rvest::html_children())[[1]] %>% rvest::html_children())[[2]] %>% rvest::html_text()
#' #       #
#' #
#' #
#' #       # If the time slot form is structured in a certain way, you have to extract the name and personal_link this way
#' #       if(length(time_slot_form) > 2) {
#' #         # Name
#' #         name <- (((time_slot_form[[4]] %>% rvest::html_children())[[1]] %>% rvest::html_children() %>% rvest::html_children() %>% rvest::html_children())[[2]] %>% rvest::html_children())[[1]] %>% rvest::html_text()
#' #
#' #         # Personal Link
#' #         personal_link <- ((time_slot_form[[4]]) %>% rvest::html_children())[[1]] %>% rvest::html_nodes("a") %>% rvest::html_attr('href')
#' #
#' #       } else {
#' #         # Otherwise label them as unavailable
#' #         name <- "Timeslot Available"
#' #         personal_link <- "Timeslot Available"
#' #       }
#' #
#' #       # Add the names, dates, times, and personal links to the growing vector
#' #       names <- c(names, name)
#' #       date_vector <- c(date_vector, date)
#' #       time_slots <- c(time_slots, time_slot)
#' #       personal_links <- c(personal_links, personal_link)
#' #       # moderators <- c(moderators, moderator)
#' #
#' #       # If the ui_card_section is structured in a certain way, you have to extract everything this way
#' #       if(length(ui_card_sections) == 1){
#' #         # i.e. do nothing else
#' #       } else {
#' #
#' #         for(j in seq(2, length(ui_card_sections))) {
#' #
#' #           # Go to time form
#' #           time_slot_form <- (ui_card_sections[[j]] %>% rvest::html_children())[[2]] %>% rvest::html_children()
#' #
#' #           # Get the time slot
#' #           time_slot <- (time_slot_form[[1]] %>% rvest::html_children())[[1]] %>% rvest::html_text()
#' #
#' #           if(length(time_slot_form) > 1) {
#' #             # Get the name
#' #             name <- (((time_slot_form[[3]] %>% rvest::html_children())[[1]] %>% rvest::html_children() %>% rvest::html_children() %>% rvest::html_children())[[2]] %>% rvest::html_children())[[1]] %>% rvest::html_text()
#' #
#' #             # Get the personal link
#' #             personal_link <- ((time_slot_form[[3]]) %>% rvest::html_children())[[1]] %>% rvest::html_nodes("a") %>% rvest::html_attr('href')
#' #           } else {
#' #
#' #             # Otherwise lable them both as unavailable
#' #             name <- "Timeslot Available"
#' #             personal_link <- "Timeslot Available"
#' #           }
#' #
#' #           # Add the names, dates, times, and personal links to the growing vector
#' #           names <- c(names, name)
#' #           date_vector <- c(date_vector, date)
#' #           time_slots <- c(time_slots, time_slot)
#' #           personal_links <- c(personal_links, personal_link)
#' #         }
#' #       }}
#' #
#' #     # Return a data frame
#' #     schedule_data <- data.frame(date = date_vector, time_slots = time_slots, names = names, personal_links = personal_links)
#' #
#' #     # } else {
#' #     #   # Return empty list
#' #     #   schedule_data <- data.frame(date = c(), time_slots = c(), names = c(), personal_links = c())
#' #     # }
#' #
#' #     # Return the schedule
#' #     schedule_data
#' #  }
#' #
#' #
#' #
#' #
#' #
#' #
#' #
#' # }
#'
#' # read_scraped_schedule_files <- function(paths_schedule_data_from_user) {
#' #   paths_schedule_data_from_user rvest::read_html()
#' # }
#'
#' ################################################################################
#' # Process the scraped schedule data ---------------------------------------
#'
#'
#'
#' #' process_scraped_schedule
#' #' Process the scraped data
#' #'
#' #' @param scraped_schedule df with the scraped schedul
#' #'
#' #' @return scrape_df
#' #' @export
#' #'
#'
#' process_scraped_schedule <- function(scraped_schedule){
#'   scrape_df <- scraped_schedule
#'
#'   # Get the weekday from the date
#'   scrape_df$weekday <- stringr::str_extract(scrape_df$timeslot, pattern = "Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday")
#'
#'   # Get the date from the date
#'   scrape_df$day <- stringr::str_extract(scrape_df$timeslot, pattern = "1st|2nd|3rd|4th|5th|6th|7th|8th|9th|10th|11th|12th|13th|14th|15th|16th|17th|18th|19th|20th|21st|22nd|23rd|24th|25th|26th|27th|28th|29th|30th|31st")
#'
#'   # Get the month from the date
#'   scrape_df$month <- stringr::str_extract(scrape_df$timeslot, pattern = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec")
#'
#'   # Create a list of the months
#'   possible_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#'
#'   #Create a list of the numeric values for the months
#'   numeric_months <- as.character(1:12)
#'
#'   # Prepare the column that will become numeric month
#'   scrape_df$month_numeric <- scrape_df$month
#'
#'   # Loop through every month
#'   for(i in seq_along(possible_months)){
#'
#'     # Replace the month with the proper number
#'     scrape_df$month_numeric <- stringr::str_replace(scrape_df$month_numeric, possible_months[[i]], numeric_months[[i]])
#'
#'   }
#'
#'   # Change it to numeric
#'   scrape_df$month_numeric <- as.numeric(scrape_df$month_numeric)
#'
#'   # Extract the day
#'   scrape_df$day <- stringr::str_extract(scrape_df$day, "[0-9]{1,2}")
#'
#'   # Make it numeric
#'   scrape_df$day <- as.numeric(scrape_df$day)
#'
#'   scrape_df$time <- stringr::str_extract(scrape_df$timeslot, "[0-9]{2}:[0-9]{2} [AP]M - ")
#'
#'   # Extract the hour
#'   scrape_df$hour <- as.numeric(stringr::str_extract(scrape_df$time, "^[0-9]{2}"))
#'
#'   # Remove the hour
#'   time_to_clean <- stringr::str_replace(scrape_df$time, "^[0-9]{2}:", "")
#'
#'   # Extract the minutes
#'   scrape_df$minute <- stringr::str_extract(time_to_clean, "^[0-9]{2}")
#'
#'   # Remove the minutes
#'   time_to_clean <- stringr::str_replace(time_to_clean , "^[0-9]{2}\\s", "")
#'
#'   # Extract the period
#'   scrape_df$period <- stringr::str_extract(time_to_clean, "^[a-zA-Z]{2}")
#'
#'   scrape_df$year <- stringr::str_extract(scrape_df$timeslot, "[0-9]{4}")
#'
#'   # Convert the hour to military time
#'   scrape_df$military_hour <- ifelse(scrape_df$period == "AM", scrape_df$hour, ifelse(scrape_df$hour == 12, scrape_df$hour, scrape_df$hour + 12))
#'
#'   # Put the data frame columns in the desired order
#'   scrape_df <- scrape_df %>% dplyr::select(tidyselect::all_of("weekday"), tidyselect::all_of("month"), tidyselect::all_of("day"), tidyselect::all_of("hour"), tidyselect::all_of("minute"), dplyr::everything())
#'
#'   scrape_df <- scrape_df %>% dplyr::arrange(.data[["month_numeric"]], .data[["day"]], .data[["military_hour"]], .data[["minute"]])
#'
#'   scrape_df$Session_Date <- glue::glue("{scrape_df$month_numeric}/{scrape_df$day}/{scrape_df$year}")
#'   scrape_df$Session_Time <- glue::glue("{scrape_df$military_hour}:{scrape_df$minute}")
#'
#'   scrape_df <- scrape_df %>%
#'     dplyr::select(tidyselect::all_of("names"), tidyselect::all_of("Session_Date"), tidyselect::all_of("Session_Time"), tidyselect::all_of("weekday"))
#'
#'   return(scrape_df)
#' }
#'
#'
#' ################################################################################
#' # Create the global paths for the project ---------------------------------
#'
#' #' create_global_project_pathways
#' #'
#' #' @param project_year project_year
#' #' @param project_folder_name project_folder_name
#' #'
#' #' @export
#' #'
#' create_global_project_pathways <- function(
#'     project_year,
#'     project_folder_name
#' ){
#'   # Get the parts of your working directory
#'   parts_of_wd <- unlist(strsplit(getwd(), .Platform$file.sep))
#'
#'   # Re-Combine the first 3 parts
#'   path_part_to_adjust <<- paste0(parts_of_wd[1:3], collapse = "/")
#'
#'   # Create the home directory for the current project
#'   home_dir <<- paste0(path_part_to_adjust, "/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/00 Jobs/", as.character(project_year), "/", as.character(project_folder_name))
#'
#'   # Create the downloads folder
#'   download_location <<- paste0(path_part_to_adjust, "/Downloads")
#'
#'   ## Use this to obtain the User Participant Data
#'   user_participant_urls <<- usertools::create_user_participant_urls(project_numbers)
#'
#'   # Location of the completed user data
#'   project_data_collection_folder_location <<- paste0(home_dir, "/Data Collection")
#'   project_data_collection_project_files_folder_location <<- paste0(home_dir, "/Data Collection/Project Files")
#'   project_data_collection_project_files_recruitment_folder_location <<- paste0(home_dir, "/Data Collection/Project Files/Recruitment")
#'   project_data_collection_project_files_recruitment_user_participant_data_folder_location <<- paste0(home_dir, "/Data Collection/Project Files/Recruitment/user_participant_data")
#'   UPD_file_location <<- paste0(home_dir, "/Data Collection/Project Files/Recruitment/user_participant_data")
#'   UPD_folder_complete_location <<- paste0(home_dir, "/Data Collection/Project Files/Recruitment/user_participant_data/combined")
#'   UPD_folder_col_names_location <<- paste0(home_dir, "/Data Collection/Project Files/Recruitment/user_participant_data/col_names")
#'   UPD_folder_schedule <<- paste0(home_dir, "/Data Collection/Project Files/Recruitment/Schedule")
#'   UPD_folder_schedule_combined <<- paste0(home_dir, "/Data Collection/Project Files/Recruitment/Schedule/combined")
#'   project_analysis_folder_path <<- paste0(home_dir, "/Analysis")
#'   project_analysis_immersion_folder_path <<- paste0(home_dir, "/Analysis/Immersion")
#'   raw_immersion_folder_path <<- paste0(home_dir, "/Analysis/Immersion/Raw Data")
#'
#'   # Put these together as a list
#'   list_of_folders_for_UPD_folder_complete <- list(
#'     project_data_collection_folder_location,
#'     project_data_collection_project_files_folder_location,
#'     project_data_collection_project_files_recruitment_folder_location,
#'     project_data_collection_project_files_recruitment_user_participant_data_folder_location,
#'     UPD_folder_complete_location,
#'     UPD_folder_col_names_location,
#'     UPD_folder_schedule,
#'     UPD_folder_schedule_combined,
#'     project_analysis_folder_path,
#'     project_analysis_immersion_folder_path,
#'     raw_immersion_folder_path
#'   )
#'
#'   # If the folder does not exist, create it
#'   for(i in list_of_folders_for_UPD_folder_complete) {
#'     if (!dir.exists(i)){
#'       dir.create(i)
#'     }
#'   }
#'
#'   # Create the file path
#'   UPD_file_complete_location <<- paste0(UPD_folder_complete_location, "/most_recent_combined.csv")
#' }
#'
#' ################################################################################
