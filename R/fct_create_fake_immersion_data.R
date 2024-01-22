#'
#' ################################################################################
#'
#' #' create_fake_sensor_id
#' #'
#' #' @param n number of desired fake sensor ids
#' #'
#' #' @return fake_sensor_ids
#' #' @export
#' #'
#' create_fake_sensor_id <- function(n) {
#'   if(n > 0) {
#'     # Obtain the lowercase letters
#'     lower_case_letters <- stringr::str_to_lower(LETTERS)
#'
#'     # Obtain the possible numbers
#'     numbers <- 0:9
#'
#'     # Add these together of the character options
#'     character_options <- c(lower_case_letters, numbers)
#'
#'     # For all requestion
#'     random_8_characters <- purrr::map_chr(seq(1, n), ~ {
#'       # Randomly select 8 characters, and combine them into a string
#'       sample(character_options, 8) %>% paste0(collapse = "")
#'     })
#'
#'     # Create the sensor ids
#'     fake_sensor_ids <- paste0("p", random_8_characters)
#'
#'     # Return the ids
#'     return(fake_sensor_ids)
#'   }
#' }
#'
#' ################################################################################
#'
#' #' create_fake_date_time_series
#' #'
#' #' @param n_seconds How many seconds of data do you need
#' #'
#' #' @return date
#' #' @export
#' #'
#' create_fake_date_time_series <- function(n_seconds) {
#'   # Get the current time
#'   current_time <- Sys.time()
#'
#'   # Make sure the number does not occur in the future
#'   latest_possible_start <- current_time - n_seconds
#'
#'   # Randomly select a number
#'   random_number <- sample(1:9999999, 1)
#'
#'   # Obtain the random starting point
#'   starting_time <- latest_possible_start - random_number
#'
#'   # Create a time point for every required second
#'   number_vector_for_seconds <- seq(1, n_seconds)
#'
#'   # Add this to the starting time
#'   date <- starting_time + number_vector_for_seconds
#'
#'   # Return the vector of datetime values
#'   return(date)
#' }
#'
#' ################################################################################
#'
#' #' create_fake_agenda
#' #'
#' #' @param n_seconds n_seconds
#' #'
#' #' @return agenda
#' #' @export
#' #'
#' create_fake_agenda <- function(n_seconds) {
#'
#'   # Create a time point for every required second
#'   number_vector_for_seconds <- seq(1, n_seconds)
#'
#'   # Get the minimum n per section
#'   section_n_init <- floor(max(number_vector_for_seconds) / 3)
#'
#'   # Create the initial sections
#'   sections_init <- rep(section_n_init, 3)
#'
#'   # If the n_seconds is not the same as the added values
#'   if(sum(sections_init) != n_seconds) {
#'
#'     # Find out how many need to be added
#'     difference <- n_seconds - sum(sections_init)
#'
#'     # Create a vector to add
#'     n_to_add <- c(rep(1, difference), rep(0, 3 - difference))
#'
#'     # Create a vector with the correct number of needed values
#'     number_for_sections <- sections_init + n_to_add
#'
#'     # Otherwise
#'   } else {
#'
#'     # Leave it as it is
#'     number_for_sections <- sections_init
#'   }
#'
#'   # Create the beginning middle and end values
#'   Beginning_values <- rep("Beginning", number_for_sections[[1]])
#'   Middle_values <- rep("Middle", number_for_sections[[2]])
#'   End_values <- rep("End", number_for_sections[[3]])
#'
#'   # Add them together
#'   agenda <- c(Beginning_values, Middle_values, End_values)
#'
#'   # Return the vector
#'   return(agenda)
#'
#' }
#'
#' ################################################################################
#'
#' # I think it is easiest to enter the videos we want, and then that will determine the size of the dataframe
#' #' add_marks_for_videos
#' #'
#' #' @param list_of_marks list_of_marks
#' #' @param add_order add_order
#' #' @param list_of_seconds_per_video list_of_seconds_per_video
#' #'
#' #' @return all_marks
#' #' @export
#' #'
#' add_marks_for_videos <- function(list_of_marks, list_of_seconds_per_video, add_order = TRUE) {
#'
#'   # If add_order is true, create an order number and add it to the mark
#'   if(add_order == TRUE) {
#'     order_version <- seq(1, length(list_of_marks))
#'     list_of_marks <- paste0(list_of_marks, order_version)
#'   }
#'
#'   # Create the sections for the videos
#'   video_NAs <- purrr::map2(list_of_marks, list_of_seconds_per_video, ~{
#'     video_NAs <- rep(NA, .y)
#'     video_NAs[[1]] <- .x
#'     video_NAs
#'   })
#'
#'   # Create the section for the end marks between them
#'   end_NAs <- purrr::map2(list_of_marks, list_of_seconds_per_video, ~{
#'     random_probability <- runif(1, .1, .75)
#'     size_of_end_section <- round(random_probability * .y)
#'     end_NAs <- rep(NA, size_of_end_section)
#'     end_NAs[[1]] <- "end"
#'     end_NAs
#'   })
#'
#'   # combine them together into one vector
#'   both_sections_NAs <- purrr::map2(video_NAs, end_NAs, ~{
#'     both_sections_NAs <- c(.x, .y)
#'     both_sections_NAs
#'   }) %>% unlist()
#'
#'   # Add a start and a finish section
#'   length_of_both <- length(both_sections_NAs)
#'   random_start <- runif(1, .2, .5)
#'   random_finish <- runif(1, .2, .5)
#'   length_of_start <- round(random_start * length_of_both)
#'   length_of_finish <- round(random_finish * length_of_both)
#'   start_NAs <- rep(NA, length_of_start)
#'   finish_NAs <- rep(NA, length_of_finish)
#'   all_marks <- c(start_NAs, both_sections_NAs, finish_NAs)
#'
#'
#'
#'
#'   # Return the vector
#'   return(all_marks)
#' }
#'
#' ################################################################################
#'
#' #' create_simulated_immersion_data_for_sensor_ids
#' #'
#' #' @param n_participants n_participants
#' #' @param n_row_for_df n_row_for_df
#' #' @param desired_mean desired_mean
#' #'
#' #' @return immersion_columns
#' #' @export
#' #'
#' create_simulated_immersion_data_for_sensor_ids <- function(n_participants, n_row_for_df, desired_mean = 50) {
#'   # Create the sensor ids
#'   random_sensor_ids <- create_fake_sensor_id(n_participants)
#'
#'   individual_means <- rnorm(n_participants, desired_mean, 20)
#'
#'   # Create the immersion columns
#'   immersion_columns <- purrr::map2(random_sensor_ids, individual_means, ~ {
#'     create_immersion_data_for_participant(n_row_for_df = n_row_for_df, length_of_moments = 20, desired_mean = .y)
#'   }) %>%
#'     purrr::set_names(random_sensor_ids) %>%
#'     as.data.frame()
#'
#'   return(immersion_columns)
#' }
#'
#' ################################################################################
#'
#' #' create_immersion_aggregate_column
#' #' Create the aggregated immersion_column
#' #'
#' #' @param immersion_columns immersion_columns
#' #'
#' #' @return Immersion
#' #' @export
#' #'
#'
#' create_immersion_aggregate_column <- function(immersion_columns) {
#'
#'   # Calculate the average immersion score
#'   Immersion <- rowSums(immersion_columns) / ncol(immersion_columns)
#'
#'   # Return the immersion file
#'   return(Immersion)
#' }
#'
#' ################################################################################
#'
#' #' create_simulated_immersion_sheet_data
#' #'
#' #' @param list_of_marks list_of_marks
#' #' @param list_of_seconds_per_video list_of_seconds_per_video
#' #' @param n_participants n_participants
#' #' @param add_order add_order
#' #'
#' #' @return immersion_sheet
#' #' @export
#' #'
#' create_simulated_immersion_sheet_data <- function(list_of_marks, list_of_seconds_per_video, n_participants, add_order = TRUE) {
#'   # Start with the video marks, as this will determine the size of the data frame.
#'   Notes <- add_marks_for_videos(list_of_marks, list_of_seconds_per_video, add_order = add_order)
#'
#'   # How many rows should be in the dataframe?
#'   n_row_for_df <- length(Notes)
#'
#'   # Create the date
#'   date <- create_fake_date_time_series(n_row_for_df)
#'
#'   # Create the dataframe
#'   immersion_sheet <- data.frame(date = date)
#'
#'   # Create the blank column
#'   immersion_sheet$` ` <- rep(NA, n_row_for_df)
#'
#'   # Create the immersion and sensor id data
#'   immersion_data_for_sensor_ids <- create_simulated_immersion_data_for_sensor_ids(n_participants, n_row_for_df)
#'
#'   # Add immersion data to immersion sheet
#'   immersion_sheet <- cbind(immersion_sheet, immersion_data_for_sensor_ids)
#'
#'   # Create the agenda column
#'   immersion_sheet$Agenda <- create_fake_agenda(n_seconds = n_row_for_df)
#'
#'   # Add the Notes Column
#'   immersion_sheet$Notes <- Notes
#'
#'   # Add the Immersion aggregate column
#'   immersion_sheet$Immersion <- create_immersion_aggregate_column(immersion_data_for_sensor_ids)
#'
#'   # Return the dataframe
#'   return(immersion_sheet)
#' }
#'
#'
#' ################################################################################
#'
#' #' create_pattern
#' #'
#' #' @param length_of_section length_of_section
#' #' @param direction direction
#' #' @param amount_of_change amount_of_change
#' #' @param perc_for_peak perc_for_peak
#' #' @param perc_for_return perc_for_return
#' #'
#' #' @return values
#' #' @export
#' #'
#' create_pattern <- function(
#'     length_of_section = 30,
#'     direction = "Incline",
#'     amount_of_change = 8,
#'     perc_for_peak = .25,
#'     perc_for_return = .75
#' ) {
#'
#'   direction_constant <- ifelse(direction == "Incline", 1, -1)
#'   peak_point <- floor(length_of_section * perc_for_peak)
#'   return_point <- floor(length_of_section * perc_for_return)
#'   middle_point <- round(((return_point - peak_point) / 2) + peak_point)
#'   half_of_change <- round(amount_of_change / 2)
#'   high_point_y <- amount_of_change + half_of_change
#'
#'   section_1 <- seq(1, peak_point)
#'   section_2 <- seq(peak_point + 1, middle_point)
#'   # if(peak_point == middle_point) {}
#'   section_3 <- seq(middle_point + 1, return_point)
#'   # if(middle_point == peak_point) {}
#'   section_4 <- seq(return_point + 1, length_of_section)
#'
#'   section_1_x_min <- min(section_1)
#'   section_1_x_max <- max(section_1)
#'   section_1_x_length <- length(section_1)
#'   section_1_values <- round((amount_of_change / section_1_x_length) * seq_along(section_1))
#'
#'   section_2_x_min <- min(section_2)
#'   section_2_x_max <- max(section_2)
#'   section_2_x_length <- length(section_2)
#'   section_2_values <- round(amount_of_change + ((half_of_change / section_2_x_length) * seq_along(section_2)))
#'
#'   section_3_x_min <- min(section_3)
#'   section_3_x_max <- max(section_3)
#'   section_3_x_length <- length(section_3)
#'   section_3_values <- round(high_point_y - ((half_of_change / section_3_x_length) * seq_along(section_3)))
#'
#'   section_4_x_min <- min(section_4)
#'   section_4_x_max <- max(section_4)
#'   section_4_x_length <- length(section_4)
#'   section_4_values <- rev(round((amount_of_change / section_4_x_length) * seq_along(section_4)))
#'
#'   values <- c(section_1_values, section_2_values, section_3_values, section_4_values)
#'
#'   values <- direction_constant * values
#'
#'   return(values)
#' }
#'
#' ################################################################################
#'
#' #' create_immersion_data_for_participant
#' #'
#' #' @param n_row_for_df n_row_for_df
#' #' @param length_of_moments length_of_moments
#' #' @param desired_mean desired_mean
#' #'
#' #' @return immersion_scores_for_participant
#' #' @export
#' #'
#' create_immersion_data_for_participant <- function(n_row_for_df, length_of_moments = 50, desired_mean = 50) {
#'   n_sections <- floor(n_row_for_df / length_of_moments)
#'   left_over <- n_row_for_df - (n_sections * length_of_moments)
#'   sections_and_lengths <- rep(n_sections, length_of_moments)
#'   sections_and_lengths[[1]] <- sections_and_lengths[[1]] + left_over
#'
#'   immersion_scores_for_participant <- purrr::map(sections_and_lengths, ~{
#'     random_direction <- sample(c("Incline", "Decline"), 1)
#'     random_amount_of_change <- rnorm(1, 20, 5)
#'     random_perc_for_peak <- sample(seq(5, 74) / 100, 1)
#'     start_for_random_perc_for_return <- random_perc_for_peak * 100
#'     random_perc_for_return <- sample(seq(start_for_random_perc_for_return + 10, 95) / 100, 1)
#'
#'     create_pattern(
#'       length_of_section = .x,
#'       direction = random_direction,
#'       amount_of_change = random_amount_of_change,
#'       perc_for_peak = random_perc_for_peak,
#'       perc_for_return = random_perc_for_return
#'     )
#'   }) %>% unlist()
#'
#'   current_mean_immersion_scores_for_participant <- mean(immersion_scores_for_participant)
#'
#'   mean_difference <- desired_mean - current_mean_immersion_scores_for_participant
#'
#'   immersion_scores_for_participant <- round(immersion_scores_for_participant + mean_difference)
#'
#'   immersion_scores_for_participant <- ifelse(immersion_scores_for_participant > 100, 100, immersion_scores_for_participant)
#'
#'   immersion_scores_for_participant <- ifelse(immersion_scores_for_participant < 1, 1, immersion_scores_for_participant)
#'
#'   immersion_scores_for_participant <- immersion_scores_for_participant[seq(1, n_row_for_df)]
#'
#'   return(immersion_scores_for_participant)
#' }
#'
#' ################################################################################
#'
#' #' create_unique_ids
#' #'
#' #' @param n Number of strings to create
#' #' @param seed_no Random seed
#' #' @param char_len Character Length
#' #'
#' #' @return res
#' #' @export
#' #'
#' create_unique_ids <- function(n, seed_no = 1, char_len = 5){
#'   # Set random seed
#'   set.seed(seed_no)
#'
#'   # Characters
#'   pool <- c(letters, LETTERS, 0:9)
#'
#'   #pre-allocating vector is much faster than growing it
#'   res <- character(n)
#'
#'   # For every id that needs to be created
#'   for(i in seq(n)){
#'
#'     # Select the characters
#'     this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
#'
#'     # Check if there are any repeats, and if so, repeat it
#'     while(this_res %in% res){
#'       this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
#'     }
#'
#'     # Add the string to the vector
#'     res[i] <- this_res
#'   }
#'
#'   # Return the ids
#'   return(res)
#' }
#'
#' ################################################################################
#'
#' #' create_fake_immersion_file_name
#' #'
#' #' @param datetime datetime
#' #' @param video_marks_list video_marks_list
#' #' @param method_type method_type
#' #' @param version version
#' #'
#' #' @return new_file_name
#' #' @export
#' #'
#' create_fake_immersion_file_name <- function(
#'     datetime,
#'     video_marks_list = NULL,
#'     method_type = "Within",
#'     version = 1
#' ) {
#'
#'   # Process the datetime variable
#'   datetime <- lubridate::ymd_hms(datetime)
#'
#'   # Obtain the year
#'   year <- lubridate::year(datetime)
#'
#'   # Obtain month
#'   month <- lubridate::month(datetime)
#'   month <- ifelse(stringr::str_length(month) == 1, paste0("0", month), month)
#'
#'   # Obtain day
#'   day <- lubridate::day(datetime)
#'   day <- ifelse(stringr::str_length(day) == 1, paste0("0", day), day)
#'
#'   # Obtain hour
#'   hour <- lubridate::hour(datetime)
#'   hour <- ifelse(stringr::str_length(hour) == 1, paste0("0", hour), hour)
#'
#'   # Obtain the minute
#'   minute <- lubridate::minute(datetime)
#'   minute <- ifelse(stringr::str_length(minute) == 1, paste0("0", minute), minute)
#'
#'   # Obtain the second
#'   second <- lubridate::second(datetime)
#'   second <- ifelse(stringr::str_length(minute) == 1, paste0("0", second), second)
#'
#'   # Obtain the timezone
#'   timezone <- lubridate::tz(datetime)
#'
#'   # Prepare possible characters
#'   characters <- c(letters, 0:9)
#'
#'   # Create fake experience id
#'   fake_experience_id <- sample(characters, 5) %>% paste0(collapse = "")
#'
#'   # If within subjects
#'   if(method_type == "Within") {
#'
#'     # Use date, seat #, version #
#'     name_entered_in_immersion <- paste0(
#'       month, "_",
#'       day, "_",
#'       year, "_",
#'       hour, "_",
#'       minute, " - Seat ",
#'       sample(1:3, 1), " - ",
#'       "V", version
#'     )
#'
#'     # If between subjects
#'   } else if (method_type == "Between") {
#'
#'     # Create fake RID
#'     RID <- create_unique_ids(1)
#'
#'     # Select Video
#'     video_name_part <- sample(unlist(video_marks_list), 1)
#'
#'     # Create name with seat #
#'     name_entered_in_immersion <- paste0(
#'       RID, "_", video_name_part," - Seat ",  sample(1:3, 1)
#'     )
#'   }
#'
#'   # Put the names together
#'   new_file_name <- paste0(
#'     name_entered_in_immersion, "_",
#'     fake_experience_id, "_",
#'     timezone, "_",
#'     month, "_",
#'     day, "_",
#'     year, "_",
#'     hour, "_",
#'     minute, "_",
#'     second,
#'     ".xlsx"
#'   )
#'
#'   # Return the file name
#'   return(new_file_name)
#' }
#'
#' ################################################################################
#'
#' #' save_excel_workbook
#' #'
#' #' @param simulated_sheet_data simulated_sheet_data
#' #' @param path_location path_location
#' #' @param method_type method_type
#' #' @param video_marks_list video_marks_list
#' #' @param version version
#' #'
#' #' @return wb
#' #' @export
#' #'
#' save_excel_workbook <- function(
#'     simulated_sheet_data,
#'     path_location = "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research Tools/datacollectiontools/data/",
#'     method_type = "Within",
#'     video_marks_list = NULL,
#'     version = 1
#' ) {
#'
#'   # Create the file name
#'   file_name <- create_fake_immersion_file_name(
#'     datetime = simulated_sheet_data$date[[1]],
#'     video_marks_list =video_marks_list,
#'     method_type = method_type,
#'     version = version
#'   )
#'
#'   # Put the path together
#'   workbook_location <- paste0(path_location, file_name)
#'
#'   # Create the workbook
#'   wb <- createWorkbook()
#'
#'   # Add sheet
#'   addWorksheet(wb, sheetName = "Immersion")
#'
#'   # Add sheet
#'   addWorksheet(wb, sheetName = "Individual-Level-Data")
#'
#'   # Add sheet
#'   addWorksheet(wb, sheetName = "Safety")
#'
#'   # Write Data to the workbook
#'   writeData(wb, "Immersion", x = simulated_sheet_data)
#'
#'   # Save the file
#'   saveWorkbook(wb, workbook_location, overwrite = TRUE)
#'
#'   # Return the workbook
#'   return(wb)
#' }
#'
#' ################################################################################
#'
#'
#'
#'
#'
#'
#'
#'
#' #' create_simulated_immersion_workbook
#' #'
#' #' @param list_of_marks list_of_marks
#' #' @param list_of_seconds_per_video list_of_seconds_per_video
#' #' @param n_participants n_participants
#' #' @param path_location path_location
#' #' @param method_type method_type
#' #' @param versions versions
#' #'
#' #' @return wb
#' #' @export
#' #'
#' create_simulated_immersion_workbook <- function(
#'     list_of_marks = list("SAM", "CAN", "FAN"),
#'     list_of_seconds_per_video = list(120, 60, 55),
#'     n_participants = 5,
#'     path_location = "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research Tools/datacollectiontools/data/",
#'     method_type = "Within",
#'     versions = 1:5
#' ) {
#'
#'   # Set the marks
#'   list_of_marks = list_of_marks
#'
#'   # How long should these marks be played
#'   list_of_seconds_per_video = list_of_seconds_per_video
#'
#'   version <- sample(versions, 1)
#'
#'   # # Create the marks column
#'   # marks_for_videos <- add_marks_for_videos(list_of_marks, list_of_seconds_per_video)
#'
#'   # Create a simulated df for sheet data
#'   simulated_sheet_data <- create_simulated_immersion_sheet_data(
#'     list_of_marks = list_of_marks,
#'     list_of_seconds_per_video = list_of_seconds_per_video,
#'     n_participants = n_participants,
#'     add_order = TRUE
#'   )
#'
#'   # Save the excel file
#'   wb <- save_excel_workbook(
#'     simulated_sheet_data = simulated_sheet_data,
#'     path_location = path_location,
#'     method_type = method_type,
#'     video_marks_list = list_of_marks,
#'     version = version
#'   )
#'
#'   # Return the workbook
#'   return(wb)
#' }
#'
#' ################################################################################
#' #
#' # n_participants <- 29
#' # n_participants_per_session <- "1-on-1-Style"
#' # n_bs_videos <- 3
#' # n_ws_videos <- NULL
#' #
#' # list_of_ws_video_names <- list("chr", "kev", "mur", "jac")
#' # list_of_ws_video_seconds <- list(179, 136, 138, 87)
#' #
#' # # What are the names of the videos of which participants will see one? list() or NULL
#' # list_of_bs_video_names <- NULL
#' # list_of_bs_video_seconds <- NULL
#' #
#' #
#' #
#' # read_fake_data(n_participants = 29, n_participants_per_session = "1-on-1-Style", n_bs_videos = 3, n_ws_videos = NULL)
#' #
#'
#'
#'
#' #' read_fake_data
#' #'
#' #' @param n_participants n_participants
#' #' @param n_participants_per_session n_participants_per_session
#' #' @param n_bs_videos n_bs_videos
#' #' @param n_ws_videos n_ws_videos
#' #'
#' #' @return df_immersion_sheets
#' #' @export
#' #'
#' read_fake_data <- function(n_participants, n_participants_per_session = "1-on-1-Style", n_bs_videos = NULL, n_ws_videos = NULL){
#'
#'   if(n_participants_per_session != "1-on-1-Style") {
#'     n_participants <- ceiling(n_participants / 5)
#'   }
#'
#'   big_list_of_marks <- NULL
#'   big_list_of_seconds_per_video <- NULL
#'
#'   if(!is.null( n_bs_videos)){
#'     bs_video_names <- purrr::map_chr(seq(1, n_bs_videos), ~{
#'       paste0(sample(c(LETTERS, letters), 3, TRUE), collapse = "")
#'     })
#'
#'     bs_video_seconds <- purrr::map_dbl(seq(1, n_bs_videos), ~{
#'       paste0(sample(0:9, 3, TRUE), collapse = "") %>% as.numeric()
#'     })
#'
#'     named_seconds <- bs_video_seconds
#'
#'     names(named_seconds) <- bs_video_names
#'
#'     list_of_bs_marks <- sample(bs_video_names, n_participants, replace = TRUE)
#'
#'     list_of_bs_seconds <- named_seconds[list_of_bs_marks]
#'
#'     big_list_of_marks <- purrr::map(list_of_bs_marks, ~{.x})
#'     big_list_of_seconds_per_video <- purrr::map(list_of_bs_seconds, ~{.x})
#'   }
#'
#'   if(!is.null( n_ws_videos)){
#'     ws_video_names <- purrr::map_chr(seq(1, n_ws_videos), ~{
#'       paste0(sample(c(LETTERS, letters), 3, TRUE), collapse = "")
#'     })
#'
#'     ws_video_seconds <- purrr::map_dbl(seq(1, n_ws_videos), ~{
#'       paste0(sample(0:9, 3, TRUE), collapse = "") %>% as.numeric()
#'     })
#'
#'     list_of_ws_marks <- purrr::map(seq(1, n_participants), ~ {
#'       ws_video_names
#'     })
#'
#'     list_of_seconds_per_ws_video <- purrr::map(seq(1, n_participants), ~ {
#'       ws_video_seconds
#'     })
#'
#'     if(!is.null(big_list_of_marks)){
#'       big_list_of_marks <- purrr::map2(list_of_ws_marks, big_list_of_marks, ~{
#'         c(.x, .y)
#'       })
#'
#'       big_list_of_seconds_per_video <- purrr::map2(list_of_seconds_per_ws_video, big_list_of_seconds_per_video, ~{
#'         c(.x, .y)
#'       })
#'     } else {
#'       big_list_of_marks <- purrr::map(list_of_ws_marks, ~{.x})
#'       big_list_of_seconds_per_video <- purrr::map(list_of_seconds_per_ws_video, ~{.x})
#'     }
#'   }
#'
#'   if(is.null(big_list_of_marks)){
#'     df_immersion_sheets <- create_simulated_immersion_sheet_data(list("RANDOM"), list(120), 1, add_order = TRUE)
#'   } else if(n_participants_per_session != "1-on-1-Style") {
#'     df_immersion_sheets <- purrr::map2(big_list_of_marks, big_list_of_seconds_per_video, ~{
#'       create_simulated_immersion_sheet_data(.x, .y, 5, add_order = TRUE)
#'     })
#'   } else {
#'     df_immersion_sheets <- purrr::map2(big_list_of_marks, big_list_of_seconds_per_video, ~{
#'       create_simulated_immersion_sheet_data(.x, .y, 1, add_order = TRUE)
#'     })
#'   }
#'
#'   return(df_immersion_sheets)
#'
#' }
#'
#'
