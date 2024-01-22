#'
#' # Client Name DF ----------------------------------------------------------
#' #' create_input_client_name
#' #' Create the df that contains the client name
#' #'
#' #' @param client_name character string
#' #'
#' #' @return client_name_df
#' #' @export
#'
#' create_input_client_name <- function(client_name = NULL) {
#'   # If client name is null, make it an 2-space string
#'   if(is.null(client_name)) client_name <- "NA"
#'
#'   # If client name is NA, make it an 2-space string
#'   if(is.na(client_name)) client_name <- "NA"
#'
#'   # If client name is empty or one space, make it an 2-space string
#'   if(client_name == "" | client_name == " ") client_name <- "NA"
#'
#'   # If client name is not a character string, turn it into one
#'   if(!is.character(client_name)) client_name <- as.character(client_name)
#'
#'   # Put the dataframe together
#'   client_name_df <- data.frame(
#'     "Client_Moore" = client_name,
#'     "X_NA_1" = rep("", 1),
#'     "X_NA_2" = rep("", 1),
#'     "X_NA_3" = rep("", 1),
#'     "X_NA_4" = rep("", 1),
#'     "X_NA_5" = rep("", 1),
#'     "X_NA_6" = rep("", 1),
#'     "X_NA_7" = rep("", 1),
#'     "X_NA_8" = rep("", 1),
#'     "X_NA_9" = rep("", 1),
#'     "X_NA_10" = rep("", 1),
#'     "X_NA_11" = rep("", 1),
#'     "X_NA_12" = rep("", 1),
#'     "X_NA_13" = rep("", 1),
#'     "Client_Name" = client_name,
#'     "Total_Costs" = 0,
#'     "Actual_Costs" = 0
#'   )
#'
#'   # Return the dataframe
#'   return(client_name_df)
#' }
#'
#'
#'
#' # Moore Production Staff DF --------------------------------------------------
#' #' create_input_moore_production_staff
#' #' Create the df for the Moore production staff
#' #'
#' #' @param moore_production_staff vector of strings that contain the selected moore positions
#' #' @param estimated_shoot_days_needed integer for number of shoot days
#' #' @param location_of_shoot character string showing whether the shoot is in Tulsa, Domestic Travel, or International Travel. I need to add the actual text options here.
#' #'
#' #' @return moore_production_staff_df
#' #' @export
#'
#' create_input_moore_production_staff <- function(
#'     moore_production_staff = NULL,
#'     estimated_shoot_days_needed = NULL,
#'     location_of_shoot = NULL
#' ) {
#'
#'   # If the Moore Staff variable is null, leave it that way as an empty vector
#'   if(is.null(moore_production_staff)) {
#'     moore_production_staff <- c()
#'   }
#'
#'   # If the location shoot variable is missing, change it to "None"
#'   if(is.null(location_of_shoot)){
#'     location_of_shoot <- "None"
#'     should_travel_days_be_added <- 0
#'   } else {
#'     # If location shoot is Tulsa, Ok., also change travel days to 0
#'     should_travel_days_be_added <- ifelse(
#'       location_of_shoot == "Tulsa, Ok." | location_of_shoot == "Please Select" | location_of_shoot == "None",
#'       0,
#'       # Otherwise the travel days should always be 2
#'       2
#'     )
#'   }
#'   # If the estimated shoot days variable is missing, just make it zero
#'   if(is.null(estimated_shoot_days_needed)) estimated_shoot_days_needed <- 0
#'
#'   # If multiple numbers are somehow entered
#'   if(length(estimated_shoot_days_needed) > 1) {
#'     warning("estimated_shoot_days_needed should only have 1 number. Since this vector has more than one, we will use the first number in the vector")
#'     estimated_shoot_days_needed <- estimated_shoot_days_needed[[1]]
#'   }
#'
#'   # Convert shoot days to numeric
#'   estimated_shoot_days_needed <- as.numeric(estimated_shoot_days_needed)
#'
#'   # If shoot days is NA, make it zero
#'   if(is.na(estimated_shoot_days_needed)) estimated_shoot_days_needed <- 0
#'
#'   # Use the data saved into the packages to obtain the options provided in shiny
#'   moore_production_position_to_use <- moore_production_position_options
#'
#'   # Remove the Others variable, as this just gives the option to enter custom text
#'   moore_production_position_to_use <- moore_production_position_to_use[moore_production_position_to_use != "Others (Select to Provide Custom Positions)"]
#'
#'   # If there is custom selections, combine them with the others
#'   moore_production_position_to_use <- union(moore_production_position_to_use, moore_production_staff)
#'
#'   # Count the number of options, as this will be the n rows in the df
#'   n_moore_production_staff <- as.numeric(length(moore_production_position_to_use))
#'
#'   # Since we do not know the line items, we mark it as ?
#'   Line_Item <- rep("?", n_moore_production_staff)
#'
#'   # What happens for empty columns
#'   missing_columns <- rep("", n_moore_production_staff)
#'
#'   # The following shows what happens when the position was selected by the user
#'
#'   # "Please Enter Name" is entered into name
#'   name <- ifelse(moore_production_position_to_use %in% moore_production_staff, "Please Enter Name", "NA")
#'
#'   # Otherwise everything is given a zero no matter what. This will likely change
#'   day_rate <- ifelse(moore_production_position_to_use %in% moore_production_staff, 0, 0)
#'   shoot_days <- ifelse(moore_production_position_to_use %in% moore_production_staff, estimated_shoot_days_needed, 0)
#'   total_day_rate <- ifelse(moore_production_position_to_use %in% moore_production_staff, 0, 0)
#'   travel_days <- ifelse(moore_production_position_to_use %in% moore_production_staff, should_travel_days_be_added, 0)
#'   travel_pay <- ifelse(moore_production_position_to_use %in% moore_production_staff, 0, 0)
#'   per_diem_pay <- ifelse(moore_production_position_to_use %in% moore_production_staff, 0, 0)
#'   personal_kit_day_rate <- ifelse(moore_production_position_to_use %in% moore_production_staff, 0, 0)
#'   kit_total_rate <- ifelse(moore_production_position_to_use %in% moore_production_staff, 0, 0)
#'   OT_hours <- ifelse(moore_production_position_to_use %in% moore_production_staff, 0, 0)
#'   OT_pay <- ifelse(moore_production_position_to_use %in% moore_production_staff, 0, 0)
#'   Total_Costs <- ifelse(moore_production_position_to_use %in% moore_production_staff, 0, 0)
#'   Actual_Costs <- ifelse(moore_production_position_to_use %in% moore_production_staff, 0, 0)
#'
#'   # Put it all together into the dataframe
#'   moore_production_staff_df <- data.frame(
#'     "Moore_Production_Staff" = moore_production_position_to_use,
#'     "Line_Item" = Line_Item,
#'     "Name" = name,
#'     "day_rate" = day_rate,
#'     "shoot_days" = shoot_days,
#'     "total_day_rate" = total_day_rate,
#'     "travel_days" = travel_days,
#'     "travel_pay" = travel_pay,
#'     "per_diem_pay" = per_diem_pay,
#'     "personal_kit_day_rate" = personal_kit_day_rate,
#'     "kit_total_rate" = kit_total_rate,
#'     "OT_hours" = OT_hours,
#'     "OT_pay" = OT_pay,
#'     "X_NA_1" = missing_columns,
#'     "Moore_Production_Staff_Names " = moore_production_position_to_use,
#'     "Total_Costs" = Total_Costs,
#'     "Actual_Costs" = Actual_Costs
#'   )
#'
#'   # Return the dataframe
#'   return(moore_production_staff_df)
#' }
#'
#'
#' # Freelance Positions DF -----------------------------------------------------
#' #' create_input_production_freelance
#' #' Create the df for the Freelance staff
#' #'
#' #' @param selected_crew_table a df that contains the selected crew_position, travel_or_local, and needed_prep_days
#' #' @param estimated_shoot_days_needed integer for number of shoot days
#' #' @param location_of_shoot character string showing whether the shoot is in Tulsa, Domestic Travel, or International Travel. I need to add the actual text options here.
#' #'
#' #' @return crew_table_final
#' #' @export
#' #'
#'
#' create_input_production_freelance <- function(
#'     selected_crew_table = NULL,
#'     estimated_shoot_days_needed = NULL,
#'     location_of_shoot = NULL) {
#'
#'   # If the selected crew variable is missing, create an empty dataframe
#'   if(is.null(selected_crew_table)) {
#'     selected_crew_table_df <- data.frame(
#'       crew_position = character(0),
#'       travel_or_local = character(0),
#'       needed_prep_days = numeric(0)
#'     )
#'   } else {
#'     # Otherwise, rename the column names
#'     selected_crew_table_df <- selected_crew_table %>%
#'       dplyr::select(
#'         crew_position = freelance_position,
#'         travel_or_local = is_travel_needed,
#'         needed_prep_days = are_prep_days_needed
#'       )
#'   }
#'
#'   # If location is empty or marked as please select, mark it none.
#'   if(is.null(location_of_shoot) | identical(location_of_shoot, "Please Select")) {
#'     location_of_shoot <- "None"
#'   }
#'
#'   # It the estimated shoot days is missing, make it zero days.
#'   if(is.null(estimated_shoot_days_needed)) {
#'     estimated_shoot_days_needed <- 0
#'   }
#'
#'   # Save the data from the package on the crew rates as crew_table
#'   crew_table <- pricing_crew_rates
#'
#'   # If there are rows in the selected crew table
#'   if(nrow(selected_crew_table_df) > 0) {
#'
#'     # Check if any of the selected vars are duplicates
#'     selected_crew_table_df$duplicated <- duplicated(selected_crew_table_df$crew_position)
#'
#'     # Create a vector that contains the shoot days
#'     selected_crew_table_df$estimated_shoot_days <- rep(estimated_shoot_days_needed, nrow(selected_crew_table_df))
#'
#'     # Create a vector with the location of shoot variable for each crew member
#'     selected_crew_table_df$location_of_shoot <- rep(location_of_shoot, nrow(selected_crew_table_df))
#'
#'     # I decided to rely only on whether the user says travel is needed or not. I can change this.
#'     selected_crew_table_df$travel_needed <- dplyr::case_when(
#'       selected_crew_table_df$travel_or_local == "Travel is needed" ~ "Yes",
#'       selected_crew_table_df$travel_or_local == "Travel is not needed" ~ "No"
#'       # selected_crew_table_df$location_of_shoot == "Tulsa, Ok." ~ "No",
#'       # selected_crew_table_df$location_of_shoot == "None" ~ "No",
#'       # selected_crew_table_df$location_of_shoot != "Tulsa, Ok." & ,
#'       # selected_crew_table_df$location_of_shoot != "Tulsa, Ok." & selected_crew_table_df$travel_or_local == "Travel is not needed" ~ "No"
#'     )
#'
#'     # Make sure shoot days is numeric
#'     selected_crew_table_df$estimated_shoot_days <- as.numeric(selected_crew_table_df$estimated_shoot_days)
#'
#'     # Make sure prep days is numeric
#'     selected_crew_table_df$needed_prep_days <- as.numeric(selected_crew_table_df$needed_prep_days)
#'
#'     # Add prep days to shoot days
#'     selected_crew_table_df$shoot_days <- as.numeric(selected_crew_table_df$estimated_shoot_days) + as.numeric(selected_crew_table_df$needed_prep_days)
#'
#'     # Provide 2 travel days for those who will be traveling
#'     selected_crew_table_df$travel_days <- ifelse(selected_crew_table_df$travel_needed == "Yes", 2, 0)
#'
#'     # Filter for unique crew positions
#'     not_duplicated_selected_crew_table_df <- selected_crew_table_df %>%
#'       dplyr::filter(.data[["duplicated"]] == FALSE)
#'
#'     # Filter for duplicated crew positions
#'     duplicated_selected_crew_table_df <- selected_crew_table_df %>%
#'       dplyr::filter(.data[["duplicated"]] == TRUE)
#'
#'     # Join the unique data with the financial freelance data
#'     if(nrow(not_duplicated_selected_crew_table_df) > 0){
#'       not_duplicated_crew_table <- crew_table %>%
#'         dplyr::left_join(not_duplicated_selected_crew_table_df, by = "crew_position")
#'     } else {
#'       # Stop if this does not have anything to join
#'       stop("There has to be data for this table")
#'     }
#'
#'     # If there are some duplicated crew positions
#'     if(nrow(duplicated_selected_crew_table_df) > 0){
#'
#'       # Create a new table that joins the duplicates with the financial crew data
#'       duplicated_crew_table <- crew_table %>%
#'         dplyr::left_join(duplicated_selected_crew_table_df, by = "crew_position") %>%
#'         dplyr::filter(!is.na(.data[["travel_or_local"]]))
#'
#'       # Combine the duplicate table with the unique table
#'       combined_crew_table <- not_duplicated_crew_table %>%
#'         rbind(duplicated_crew_table)
#'
#'     } else {
#'
#'       # Otherwise, just include the unique data
#'       combined_crew_table <- not_duplicated_crew_table
#'     }
#'
#'     # Now, if the selected table does not contain any rows create an empty version of the crew table
#'   } else if(nrow(selected_crew_table_df) == 0){
#'
#'     # Copy the crew table data
#'     empty_crew_table <- crew_table
#'
#'     # Indicate the number of rows in the crew table
#'     n_for_crew_table <- nrow(empty_crew_table) %>% as.numeric()
#'
#'     # Create needed columns, and mark everything as NA
#'     empty_crew_table$travel_or_local <- rep(NA, n_for_crew_table) %>% as.character()
#'     empty_crew_table$needed_prep_days <- rep(NA, n_for_crew_table) %>% as.numeric()
#'     empty_crew_table$duplicated <- rep(NA, n_for_crew_table) %>% as.logical()
#'     empty_crew_table$estimated_shoot_days <- rep(NA, n_for_crew_table) %>% as.numeric()
#'     empty_crew_table$location_of_shoot <- rep(NA, n_for_crew_table) %>% as.character()
#'     empty_crew_table$travel_needed <-  rep(NA, n_for_crew_table) %>% as.logical()
#'     empty_crew_table$shoot_days <- rep(NA, n_for_crew_table) %>% as.numeric()
#'     empty_crew_table$travel_days <- rep(NA, n_for_crew_table) %>% as.numeric()
#'
#'     # Rename this table combined crew table
#'     combined_crew_table <- empty_crew_table
#'   }
#'
#'   ## Now that the tables are set up appropriately, put together the final df
#'
#'   # Get the number of rows
#'   n_crew_positions <- nrow(combined_crew_table)
#'
#'   # Replace NAs with 0 for shoot days
#'   combined_crew_table$shoot_days <- tidyr::replace_na(combined_crew_table$shoot_days, 0)
#'
#'   # If the position was selected, enter "Please Enter Name"
#'   combined_crew_table$name <- ifelse(combined_crew_table$shoot_days > 0, "Please Enter Name", "NA")
#'
#'   # Replace NAs with 0 for shoot days
#'   combined_crew_table$travel_days <- tidyr::replace_na(combined_crew_table$travel_days, 0)
#'
#'   # Multiply shoot rate with the shoot days
#'   combined_crew_table$total_day_rate <- as.numeric(combined_crew_table$crew_shoot_day_rate) *
#'     as.numeric(combined_crew_table$shoot_days)
#'
#'   # Multiply travel days with travel rate
#'   combined_crew_table$travel_pay <- as.numeric(combined_crew_table$travel_days) *
#'     as.numeric(combined_crew_table$crew_travel_day_rate)
#'
#'   # Calculate per_diem_pay
#'   combined_crew_table$per_diem_pay <- ifelse(
#'     # If the freelance position is not traveling
#'     as.numeric(combined_crew_table$travel_days) == 0,
#'     # they do not get per_diem_pay
#'     0,
#'     # Otherwise multiply the number of all days with 75
#'     (as.numeric(combined_crew_table$shoot_days) +
#'        as.numeric(combined_crew_table$travel_days)) * 75
#'   )
#'
#'   # Mark these as 0 no matter what
#'   combined_crew_table$personal_kit_day_rate <- rep(0, n_crew_positions)
#'   combined_crew_table$kit_total_rate <- rep(0, n_crew_positions)
#'   combined_crew_table$OT_hours <- rep(0, n_crew_positions)
#'   combined_crew_table$OT_pay <- rep(0, n_crew_positions)
#'   combined_crew_table$Actual_Costs <- rep(0, n_crew_positions)
#'
#'   # Add all of the costs for each position
#'   combined_crew_table$Total_Costs <- as.numeric(combined_crew_table$total_day_rate) +
#'     as.numeric(combined_crew_table$travel_pay) +
#'     as.numeric(combined_crew_table$per_diem_pay) +
#'     as.numeric(combined_crew_table$kit_total_rate) +
#'     as.numeric(combined_crew_table$OT_pay)
#'
#'   # Create the column for the missing column
#'   combined_crew_table["X_NA_1"] <- rep("", n_crew_positions)
#'
#'   # Make a few name adjustments for the final freelance crew table
#'   crew_table_final <- combined_crew_table %>%
#'     dplyr::select(
#'       "Production_Freelance" = crew_position,
#'       "Line_Item" = accounting_code,
#'       name,
#'       "day_rate" = crew_shoot_day_rate,
#'       shoot_days,
#'       total_day_rate,
#'       travel_days,
#'       travel_pay,
#'       per_diem_pay,
#'       personal_kit_day_rate,
#'       kit_total_rate,
#'       OT_hours,
#'       OT_pay,
#'       X_NA_1,
#'       Freelance_Names = crew_position,
#'       Total_Costs,
#'       Actual_Costs
#'     )
#'
#'   # Return the table
#'   return(crew_table_final)
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
#' # Calculations ------------------------------------------------------------
#'
#' #' create_moore_staff_numbers_list
#' #'
#' #' @param moore_production_staff vector of strings that contain the selected moore positions
#' #' @param location_of_shoot character string showing whether the shoot is in Tulsa, Domestic Travel, or International Travel. I need to add the actual text options here.
#' #'
#' #' @return  moore_staff_numbers_list
#' #' @export
#' #'
#' create_moore_staff_numbers_list <- function(
#'     moore_production_staff = NULL,
#'     location_of_shoot = NULL
#' ) {
#'
#'   # If Moore staff is null, leave it as an empty vector
#'   if(is.null(moore_production_staff)){
#'     moore_production_staff <- c()
#'   }
#'
#'   # If location is Null, change it to "None"
#'   if(is.null(location_of_shoot) | identical(location_of_shoot, "Please Select")) {
#'     location_of_shoot <- "None"
#'   }
#'
#'   # Count number of positions in the vector
#'   n_moore_staff <- length(moore_production_staff)
#'
#'   # If the location is not in Tulsa, add the count to travel
#'   n_moore_travel <- ifelse(!(location_of_shoot %in% c("Tulsa, Ok.", "None")), n_moore_staff, 0)
#'
#'   # If the location is in Tulsa, add it to local
#'   n_moore_local <- ifelse((location_of_shoot %in% c("Tulsa, Ok.", "None")), n_moore_staff, 0)
#'
#'   # Create the list
#'   moore_staff_numbers_list <- list(
#'     n_moore_staff = n_moore_staff,
#'     n_moore_travel = n_moore_travel,
#'     n_moore_local = n_moore_local
#'   )
#'
#'   # Return the list
#'   return( moore_staff_numbers_list)
#' }
#'
#'
#' #' create_freelance_staff_numbers_list
#' #'
#' #' @param freelance_staff a df that contains the selected crew_position, travel_or_local, and needed_prep_days
#' #'
#' #' @return freelance_staff_numbers_list
#' #' @export
#' #'
#' create_freelance_staff_numbers_list <- function(freelance_staff = NULL) {
#'
#'   # If Null, create an empty dataframe
#'   if(is.null(freelance_staff)) {
#'     freelance_staff <- data.frame(
#'       crew_position = character(0),
#'       travel_or_local = character(0),
#'       needed_prep_days = numeric(0)
#'     )
#'   } else {
#'     # Otherwise adjust the names
#'     freelance_staff <- freelance_staff %>%
#'       dplyr::select(
#'         crew_position = freelance_position,
#'         travel_or_local = is_travel_needed,
#'         needed_prep_days = are_prep_days_needed
#'       )
#'   }
#'
#'   # If there are rows in the freelance df
#'   if(nrow(freelance_staff) > 0){
#'
#'     # Count the rows
#'     n_freelance_staff <- nrow(freelance_staff)
#'
#'     # Count the number that need travel
#'     n_freelance_travel <- freelance_staff %>%
#'       dplyr::filter(.data[["travel_or_local"]] == "Travel is needed")  %>%
#'       nrow()
#'
#'     # Count the number that do not need travel
#'     n_freelance_local <- freelance_staff %>%
#'       dplyr::filter(.data[["travel_or_local"]] == "Travel is not needed")  %>%
#'       nrow()
#'
#'   } else {
#'     # Otherwise, just make them all zero
#'     n_freelance_staff <- 0
#'     n_freelance_travel <- 0
#'     n_freelance_local <- 0
#'   }
#'
#'   # Put it together as a list
#'   freelance_staff_numbers_list <- list(
#'     n_freelance_staff = n_freelance_staff,
#'     n_freelance_travel = n_freelance_travel,
#'     n_freelance_local = n_freelance_local
#'   )
#'
#'   # Return the list
#'   return(freelance_staff_numbers_list)
#' }
#'
#'
#' #' convert_to_10_from_plus
#' #'
#' #' @param var numeric or character variable
#' #'
#' #' @return converted_number
#' #' @export
#' #'
#' convert_to_10_from_plus <- function(var) {
#'
#'   # If the variable is "10+", just make it 10
#'   converted_number <- as.numeric(ifelse(var == "10+", 10, var))
#'
#'   # Return that number
#'   return(converted_number)
#' }
#'
#'
#' #' warn_of_10_plus
#' #'
#' #' @param var numeric or character variable
#' #'
#' #' @return warning_for_number
#' #' @export
#' #'
#' warn_of_10_plus <- function(var) {
#'
#'   # If the variable is "10+", mark as TRUE
#'   warning_for_number <- ifelse(var == "10+", TRUE, FALSE)
#'
#'   # Return the boolean
#'   return(warning_for_number)
#' }
#'
#'
#' #' create_talent_numbers_list
#' #'
#' #' @param n_adult_leads Integer Number of adult leads
#' #' @param n_children_leads Integer Number of children leads
#' #' @param n_adult_speaking_extras Integer Number of adult speaking extras
#' #' @param n_children_speaking_extras Integer Number of children speaking extras
#' #' @param n_adult_non_speaking_extras Integer Number of adult non-speaking extras
#' #' @param n_children_non_speaking_extras Integer Number of children non-speaking extras
#' #'
#' #' @return talent_numbers_list
#' #' @export
#' #'
#' create_talent_numbers_list <- function(
#'     n_adult_leads = NULL,
#'     n_children_leads = NULL,
#'     n_adult_speaking_extras = NULL,
#'     n_children_speaking_extras = NULL,
#'     n_adult_non_speaking_extras = NULL,
#'     n_children_non_speaking_extras = NULL
#' ) {
#'
#'   # If variable is Null, make it zero
#'   if(is.null(n_adult_leads)) n_adult_leads <- 0
#'   if(is.null(n_children_leads)) n_children_leads <- 0
#'   if(is.null(n_adult_speaking_extras)) n_adult_speaking_extras <- 0
#'   if(is.null(n_children_speaking_extras)) n_children_speaking_extras <- 0
#'   if(is.null(n_adult_non_speaking_extras)) n_adult_non_speaking_extras <- 0
#'   if(is.null(n_children_non_speaking_extras)) n_children_non_speaking_extras <- 0
#'
#'   # Set lead variables to numeric
#'   n_adult_leads <- as.numeric(n_adult_leads)
#'   n_children_leads <- as.numeric(n_children_leads)
#'
#'   ## The other 4 variables allow one to enter "10+" as an answer.
#'   # Check if warning is needed for each of them
#'   warn_10_plus_adult_speaking_extras <- warn_of_10_plus(n_adult_speaking_extras)
#'   warn_10_plus_children_speaking_extras <- warn_of_10_plus(n_children_speaking_extras)
#'   warn_10_plus_adult_non_speaking_extras <- warn_of_10_plus(n_adult_non_speaking_extras)
#'   warn_10_plus_children_non_speaking_extras <- warn_of_10_plus(n_children_non_speaking_extras)
#'
#'   # Convert any "10+" to 10, otherwise leave it alone
#'   n_adult_speaking_extras <- convert_to_10_from_plus(n_adult_speaking_extras)
#'   n_children_speaking_extras <- convert_to_10_from_plus(n_children_speaking_extras)
#'   n_adult_non_speaking_extras <- convert_to_10_from_plus(n_adult_non_speaking_extras)
#'   n_children_non_speaking_extras <- convert_to_10_from_plus(n_children_non_speaking_extras)
#'
#'   # Set the variables to numeric
#'   n_adult_speaking_extras <- as.numeric(n_adult_speaking_extras)
#'   n_children_speaking_extras <- as.numeric(n_children_speaking_extras)
#'   n_adult_non_speaking_extras <- as.numeric(n_adult_non_speaking_extras)
#'   n_children_non_speaking_extras <- as.numeric(n_children_non_speaking_extras)
#'
#'   # At this point, talent will always be local
#'   n_talent_travel <- 0
#'
#'   # Calculate the number of local talent
#'   n_talent_local <- n_adult_leads + n_children_leads +
#'     n_adult_speaking_extras + n_children_speaking_extras +
#'     n_adult_non_speaking_extras + n_children_non_speaking_extras
#'
#'   # Calculate the total number of talent
#'   n_talent_total <- n_talent_travel + n_talent_local
#'
#'   # Create the list, along with checks if warnings are needed
#'   talent_numbers_list <- list(
#'     n_talent_total = n_talent_total,
#'     n_talent_travel = n_talent_travel,
#'     n_talent_local = n_talent_local,
#'     warn_10_plus_adult_speaking_extras = warn_10_plus_adult_speaking_extras,
#'     warn_10_plus_children_speaking_extras = warn_10_plus_children_speaking_extras,
#'     warn_10_plus_adult_non_speaking_extras = warn_10_plus_adult_non_speaking_extras,
#'     warn_10_plus_children_non_speaking_extras = warn_10_plus_children_non_speaking_extras
#'   )
#'
#'   # Return the list
#'   return(talent_numbers_list)
#' }
#'
#'
#' #' create_overall_people_list
#' #'
#' #' @param moore_staff_numbers_list list of moore staff numbers
#' #' @param freelance_staff_numbers_list list of freelance numbers
#' #' @param talent_numbers_list list of talent numbers
#' #' @param people_need_catering_n Integer for how many extra people will attend
#' #'
#' #' @return overall_people_list
#' #' @export
#' #'
#' create_overall_people_list <- function(
#'     moore_staff_numbers_list = NULL,
#'     freelance_staff_numbers_list = NULL,
#'     talent_numbers_list = NULL,
#'     people_need_catering_n = NULL
#' ) {
#'
#'   # If Null, make everything zero
#'   if(is.null(moore_staff_numbers_list)){
#'     moore_staff_numbers_list <- list(
#'       n_moore_staff = 0,
#'       n_moore_travel = 0,
#'       n_moore_local = 0
#'     )
#'   }
#'
#'   # If Null, make everything zero
#'   if(is.null(freelance_staff_numbers_list)){
#'     freelance_staff_numbers_list <-  list(
#'       n_freelance_staff = 0,
#'       n_freelance_travel = 0,
#'       n_freelance_local = 0
#'     )
#'   }
#'
#'   # If Null, make everything zero
#'   if(is.null(talent_numbers_list)){
#'     talent_numbers_list <-  list(
#'       n_talent_total = 0,
#'       n_talent_travel = 0,
#'       n_talent_local = 0,
#'       warn_10_plus_adult_speaking_extras = FALSE,
#'       warn_10_plus_children_speaking_extras = FALSE,
#'       warn_10_plus_adult_non_speaking_extras = FALSE,
#'       warn_10_plus_children_non_speaking_extras = FALSE
#'     )
#'   }
#'
#'   # If Null, make everything zero
#'   if(is.null(people_need_catering_n)){
#'     people_need_catering_n <- 0
#'   }
#'
#'   # Calculate the number of travelers
#'   all_people_travelers <- as.numeric(moore_staff_numbers_list$n_moore_travel) +
#'     as.numeric(freelance_staff_numbers_list$n_freelance_travel) +
#'     as.numeric(talent_numbers_list$n_talent_travel)
#'
#'   # Calculate the number of local people
#'   all_people_local <- as.numeric(moore_staff_numbers_list$n_moore_local) +
#'     as.numeric(freelance_staff_numbers_list$n_freelance_local) +
#'     as.numeric(talent_numbers_list$n_talent_local) +
#'     as.numeric(people_need_catering_n)
#'
#'   # Calculate the total number of people
#'   all_people_total <- as.numeric(all_people_travelers) + as.numeric(all_people_local)
#'
#'   # Create list
#'   overall_people_list <- list(
#'     all_people_total = all_people_total,
#'     all_people_travelers = all_people_travelers,
#'     all_people_local = all_people_local
#'   )
#'
#'   # Return list
#'   return(overall_people_list)
#' }
#'
#'
#' #' create_travel_information_list
#' #'
#' #' @param location_of_shoot Character where location will take place
#' #' @param needed_equipment Equipment package name
#' #' @param estimated_shoot_days_needed Number of days the shoot will take place
#' #' @param overall_people_list The list with the people data
#' #'
#' #' @return travel_information_list
#' #' @export
#' #'
#' create_travel_information_list <- function(
#'     location_of_shoot = NULL,
#'     needed_equipment = NULL,
#'     estimated_shoot_days_needed = NULL,
#'     overall_people_list = NULL
#' ) {
#'
#'   # If Null, make None
#'   if(is.null(location_of_shoot)) {
#'     location_of_shoot <- "None"
#'   }
#'
#'   # If Null, make No Package
#'   if(is.null(needed_equipment)) {
#'     needed_equipment <- "No Package"
#'   }
#'
#'   # If Null, make everything zero
#'   if(is.null(estimated_shoot_days_needed)) {
#'     estimated_shoot_days_needed <- 0
#'   }
#'
#'   # If Null, make everything zero
#'   if(is.null(overall_people_list)) {
#'     overall_people_list <- list(
#'       all_people_total = 0,
#'       all_people_travelers = 0,
#'       all_people_local = 0
#'     )
#'   }
#'
#'   # Label the type of travel
#'   travel_type <- dplyr::case_when(
#'     location_of_shoot %in% c("Tulsa, Ok.", "Please Select", "None") ~ "None",
#'     location_of_shoot == "Outside of Tulsa, OK. (United States)" ~ "Domestic",
#'     location_of_shoot == "Outside of U.S. (International)" ~ "International"
#'   )
#'
#'   # Calculate the number of bags per equipment selection
#'   n_equipment_bags_per_package <- dplyr::case_when(
#'     needed_equipment == "Small Package - Single Cam" ~ 5,
#'     needed_equipment == "Medium Package - Multi-Cam" ~ 8,
#'     needed_equipment == "Large Package - Multi-Cam" ~ 10,
#'     needed_equipment == "No Package" ~ 0
#'   )
#'
#'   # Do not add equipment bags if no travel is required
#'   n_equipment_bags_per_package <- ifelse(travel_type == "Domestic" | travel_type == "International", n_equipment_bags_per_package, 0)
#'
#'   # If travel is needed, make 1
#'   if_travel_1 <- ifelse(travel_type == "Domestic" | travel_type == "International", 1, 0)
#'
#'   # If international travel is needed, make 1
#'   if_travel_international_1 <- ifelse(travel_type == "International", 1, 0)
#'
#'   # If domestic travel is needed, make 1
#'   if_travel_domestic_1 <- ifelse(travel_type == "Domestic", 1, 0)
#'
#'   # If domestic travel, number of equipment bags
#'   n_equipment_bags_per_package_domestic <- ifelse(travel_type == "Domestic", n_equipment_bags_per_package, 0)
#'
#'   # If international travel, number of equipment bags
#'   n_equipment_bags_per_package_international <- ifelse(travel_type == "International", n_equipment_bags_per_package, 0)
#'
#'   # N Travel Days
#'   n_travel_days <- ifelse(travel_type == "None", 0, 2)
#'
#'   # N domestic Travel Days
#'   n_travel_days_domestic <- ifelse(travel_type == "Domestic", 2, 0)
#'
#'   # N international Travel Days
#'   n_travel_days_international <- ifelse(travel_type == "International", 2, 0)
#'
#'   # Constant Var
#'   constant_var <- 1
#'
#'   # Shoot Days
#'   n_shoot_days <- as.numeric(estimated_shoot_days_needed)
#'
#'   # All Days
#'   n_travel_and_shoot_days <- ifelse(
#'     as.numeric(n_travel_days) == 0,
#'     as.numeric(n_shoot_days),
#'     as.numeric(n_shoot_days) + as.numeric(n_travel_days)
#'   )
#'
#'   # Hotel Days
#'   n_hotel_days <- ifelse(
#'     as.numeric(n_travel_days) == 0,
#'     0,
#'     as.numeric(n_shoot_days) + as.numeric(n_travel_days) - 1
#'   )
#'
#'   # Employee Meals per person
#'   n_employee_meals_per_person <- ifelse(
#'     as.numeric(n_travel_days) == 0,
#'     0,
#'     (as.numeric(n_travel_days) * 3) + as.numeric(n_shoot_days)
#'   )
#'
#'   # N Client Meals
#'   n_client_meals <- ifelse(
#'     as.numeric(n_travel_days) == 0,
#'     0,
#'     as.numeric(n_travel_and_shoot_days) / 2
#'   )
#'
#'   # Days for parking
#'   n_uber_airport_parking <- ifelse(
#'     as.numeric(n_travel_days) == 0,
#'     0,
#'     as.numeric(n_travel_and_shoot_days)
#'   )
#'
#'   # Do not add to list
#'   all_travelers <- as.numeric(overall_people_list$all_people_travelers)
#'
#'   #  All Domestic Travelers
#'   all_travelers_domestic <- ifelse(travel_type == "Domestic", all_travelers, 0)
#'
#'   # All International Travelers
#'   all_travelers_international <- ifelse(travel_type == "International", all_travelers, 0)
#'
#'   # N Vans
#'   n_vehicle_rental_van <- dplyr::case_when(
#'     as.numeric(all_travelers) <= 3 ~ 0,
#'     (as.numeric(all_travelers) >= 4 & as.numeric(all_travelers) < 6) & needed_equipment %in% c("No Package", "Small Package - Single Cam") ~ 1,
#'     (as.numeric(all_travelers) >= 4 & as.numeric(all_travelers) < 6) & needed_equipment %in% c("Medium Package - Multi-Cam", "Large Package - Multi-Cam") ~ 2,
#'     as.numeric(all_travelers) >= 6 & needed_equipment %in% c("No Package", "Small Package - Single Cam") ~ 2,
#'     as.numeric(all_travelers) >= 6 & needed_equipment %in% c("Medium Package - Multi-Cam", "Large Package - Multi-Cam") ~ 3
#'   )
#'
#'   # Price for the van
#'   day_price_for_van <- as.numeric(pricing_travel_and_meals_df$cost_per_person_per_day[pricing_travel_and_meals_df$expense_type == "Vehicle Rental - Van(s)"])
#'
#'   # Total prive for the van
#'   total_price_for_van <- as.numeric(day_price_for_van) * as.numeric(n_vehicle_rental_van)
#'
#'   # Ten percent fee for van
#'   ten_percent_price_for_van <- (as.numeric(total_price_for_van) * as.numeric(n_travel_and_shoot_days)) * .1
#'
#'   # N cars
#'   n_vehicle_rental_car <- ifelse(
#'     as.numeric(all_travelers) == 0,
#'     0,
#'     ifelse(
#'       as.numeric(all_travelers) <= 3 & needed_equipment %in% c("No Package"),
#'       1,
#'       0
#'     )
#'   )
#'
#'   # Price for the car
#'   day_price_for_car <- as.numeric(pricing_travel_and_meals_df$cost_per_person_per_day[pricing_travel_and_meals_df$expense_type == "Vehicle Rental - Standard Car"])
#'
#'   # Total price for car
#'   total_price_for_car <- as.numeric(day_price_for_car) * as.numeric(n_vehicle_rental_car)
#'
#'   # Ten percent for car
#'   ten_percent_price_for_car <- (as.numeric(total_price_for_car) * as.numeric(n_travel_and_shoot_days)) * .1
#'
#'   # Parking, Tolls, and Gas
#'   parking_tolls_gas_cost <- as.numeric(ten_percent_price_for_van) + as.numeric(ten_percent_price_for_car)
#'
#'   # Create the list
#'   travel_information_list <- list(
#'     travel_type = travel_type,
#'     n_equipment_bags_per_package = n_equipment_bags_per_package,
#'     if_travel_1 = if_travel_1,
#'     n_travel_days = n_travel_days,
#'     constant_var = constant_var,
#'     n_shoot_days = n_shoot_days,
#'     n_travel_and_shoot_days = n_travel_and_shoot_days,
#'     n_hotel_days = n_hotel_days,
#'     n_employee_meals_per_person = n_employee_meals_per_person,
#'     n_client_meals = n_client_meals,
#'     n_uber_airport_parking = n_uber_airport_parking,
#'     n_vehicle_rental_van = n_vehicle_rental_van,
#'     day_price_for_van = day_price_for_van,
#'     total_price_for_van = total_price_for_van,
#'     ten_percent_price_for_van = ten_percent_price_for_van,
#'     n_vehicle_rental_car = n_vehicle_rental_car,
#'     day_price_for_car = day_price_for_car,
#'     total_price_for_car = total_price_for_car,
#'     ten_percent_price_for_car = ten_percent_price_for_car,
#'     parking_tolls_gas_cost = parking_tolls_gas_cost,
#'     all_travelers_domestic = all_travelers_domestic,
#'     all_travelers_international = all_travelers_international,
#'     n_equipment_bags_per_package_domestic = n_equipment_bags_per_package_domestic,
#'     n_equipment_bags_per_package_international = n_equipment_bags_per_package_international,
#'     if_travel_international_1 = if_travel_international_1,
#'     if_travel_domestic_1 = if_travel_domestic_1,
#'     n_travel_days_domestic = n_travel_days_domestic,
#'     n_travel_days_international = n_travel_days_international
#'   )
#'
#'   # Return the list
#'   return(travel_information_list)
#' }
#'
#'
#' # Travel Food Lodge DF ----------------------------------------------------
#'
#' #' create_input_travel_lodge_food
#' #' Create the df for the travel lodge food
#' #'
#' #' @param overall_people_list overall_people_list
#' #' @param travel_information_list travel_information_list
#' #' @param freelance_staff_numbers_list freelance_staff_numbers_list
#' #' @param moore_staff_numbers_list moore_staff_numbers_list
#' #'
#' #' @return input_travel_lodge_food
#' #' @export
#' #'
#' create_input_travel_lodge_food <- function(
#'     overall_people_list = NULL,
#'     travel_information_list = NULL,
#'     freelance_staff_numbers_list = NULL,
#'     moore_staff_numbers_list = NULL
#' ) {
#'
#'   # If Null, make everything zero
#'   if(is.null(overall_people_list)) {
#'     overall_people_list <- list(
#'       all_people_total = 0,
#'       all_people_travelers = 0,
#'       all_people_local = 0
#'     )
#'   }
#'
#'   # If Null, make everything zero
#'   if(is.null(travel_information_list)) {
#'     travel_information_list <- list(
#'       travel_type = "None",
#'       n_equipment_bags_per_package = 0,
#'       if_travel_1 = 0,
#'       n_travel_days = 0,
#'       constant_var = 1,
#'       n_shoot_days = 0,
#'       n_travel_and_shoot_days = 0,
#'       n_hotel_days = 0,
#'       n_employee_meals_per_person = 0,
#'       n_client_meals = 0,
#'       n_uber_airport_parking = 0,
#'       n_vehicle_rental_van = 0,
#'       day_price_for_van = 115,
#'       total_price_for_van = 0,
#'       ten_percent_price_for_van = 0,
#'       n_vehicle_rental_car = 0,
#'       day_price_for_car = 67,
#'       total_price_for_car = 0,
#'       ten_percent_price_for_car = 0,
#'       parking_tolls_gas_cost = 0,
#'       all_travelers_domestic = 0,
#'       all_travelers_international = 0,
#'       n_equipment_bags_per_package_domestic = 0,
#'       n_equipment_bags_per_package_international = 0,
#'       if_travel_international_1 = 0,
#'       if_travel_domestic_1 = 0,
#'       n_travel_days_domestic = 0,
#'       n_travel_days_international = 0
#'     )
#'   }
#'
#'   # If Null, make everything zero
#'   if(is.null(freelance_staff_numbers_list)) {
#'     freelance_staff_numbers_list <- list(
#'       n_freelance_staff = 0,
#'       n_freelance_travel = 0,
#'       n_freelance_local = 0
#'     )
#'   }
#'
#'   # If Null, make everything zero
#'   if(is.null(moore_staff_numbers_list)) {
#'     moore_staff_numbers_list <- list(
#'       n_moore_staff = 0,
#'       n_moore_travel = 0,
#'       n_moore_local = 0
#'     )
#'   }
#'
#'   # Extract the variables that will be utilized
#'   all_travelers <- overall_people_list$all_people_travelers
#'   n_equipment_bags_per_package <- travel_information_list$n_equipment_bags_per_package
#'   if_travel_1 <- travel_information_list$if_travel_1
#'   n_vehicle_rental_van <- travel_information_list$n_vehicle_rental_van
#'   n_vehicle_rental_car <- travel_information_list$n_vehicle_rental_car
#'   moore_travel <- moore_staff_numbers_list$n_moore_travel
#'   all_total <- overall_people_list$all_people_total
#'   all_travelers_domestic <- travel_information_list$all_travelers_domestic
#'   all_travelers_international <- travel_information_list$all_travelers_international
#'   n_equipment_bags_per_package_domestic <- travel_information_list$n_equipment_bags_per_package_domestic
#'   n_equipment_bags_per_package_international <- travel_information_list$n_equipment_bags_per_package_international
#'   if_travel_international_1 <- travel_information_list$if_travel_international_1
#'   if_travel_domestic_1 <- travel_information_list$if_travel_domestic_1
#'   n_travel_days_domestic <- travel_information_list$n_travel_days_domestic
#'   n_travel_days_international <- travel_information_list$n_travel_days_international
#'   n_travel_days <- travel_information_list$n_travel_days
#'   n_travel_and_shoot_days <- travel_information_list$n_travel_and_shoot_days
#'   constant_var <- travel_information_list$constant_var
#'   n_hotel_days <- travel_information_list$n_hotel_days
#'   n_employee_meals_per_person <- travel_information_list$n_employee_meals_per_person
#'   n_shoot_days <- travel_information_list$n_shoot_days
#'   n_client_meals <- travel_information_list$n_client_meals
#'   n_uber_airport_parking <- travel_information_list$n_uber_airport_parking
#'
#'   # Create a number list
#'   number_information_list <- list(
#'     all_travelers = all_travelers,
#'     n_equipment_bags_per_package = n_equipment_bags_per_package,
#'     if_travel_1 = if_travel_1,
#'     n_vehicle_rental_van = n_vehicle_rental_van,
#'     n_vehicle_rental_car = n_vehicle_rental_car,
#'     moore_travel = moore_travel,
#'     all_total = all_total,
#'     all_travelers_domestic = all_travelers_domestic,
#'     all_travelers_international = all_travelers_international,
#'     n_equipment_bags_per_package_domestic = n_equipment_bags_per_package_domestic,
#'     n_equipment_bags_per_package_international = n_equipment_bags_per_package_international,
#'     if_travel_international_1 = if_travel_international_1,
#'     if_travel_domestic_1 = if_travel_domestic_1,
#'     n_travel_days_domestic = n_travel_days_domestic,
#'     n_travel_days_international = n_travel_days_international,
#'     n_travel_days = n_travel_days,
#'     n_travel_and_shoot_days = n_travel_and_shoot_days,
#'     constant_var = constant_var,
#'     n_hotel_days = n_hotel_days,
#'     n_employee_meals_per_person = n_employee_meals_per_person,
#'     n_shoot_days = n_shoot_days,
#'     n_client_meals = n_client_meals,
#'     n_uber_airport_parking = n_uber_airport_parking
#'   )
#'
#'   # Create a day list
#'   day_information_list <- list(
#'     n_travel_days = n_travel_days,
#'     n_travel_and_shoot_days = n_travel_and_shoot_days,
#'     constant_var = constant_var,
#'     n_hotel_days = n_hotel_days,
#'     n_employee_meals_per_person = n_employee_meals_per_person,
#'     n_shoot_days = n_shoot_days,
#'     n_client_meals = n_client_meals,
#'     n_uber_airport_parking = n_uber_airport_parking,
#'     all_travelers_domestic = all_travelers_domestic,
#'     all_travelers_international = all_travelers_international,
#'     n_equipment_bags_per_package_domestic = n_equipment_bags_per_package_domestic,
#'     n_equipment_bags_per_package_international = n_equipment_bags_per_package_international,
#'     if_travel_international_1 = if_travel_international_1,
#'     if_travel_domestic_1 = if_travel_domestic_1,
#'     n_travel_days_domestic = n_travel_days_domestic,
#'     n_travel_days_international = n_travel_days_international,
#'     n_travel_days = n_travel_days,
#'     n_travel_and_shoot_days = n_travel_and_shoot_days,
#'     constant_var = constant_var,
#'     n_hotel_days = n_hotel_days,
#'     n_employee_meals_per_person = n_employee_meals_per_person,
#'     n_shoot_days = n_shoot_days,
#'     n_client_meals = n_client_meals,
#'     n_uber_airport_parking = n_uber_airport_parking
#'   )
#'
#'   # Use the data stored in the package to extract the travel information
#'   travel_lodge_food <- pricing_travel_and_meals_df
#'
#'   # Get the numbers per day
#'   number_per_day <- purrr::map_dbl(travel_lodge_food$number_per_day, ~{
#'     ifelse(is.na(.x), 0, as.numeric(number_information_list[[.x]]))
#'   })
#'
#'   # Get the applicable days
#'   applicable_days <- purrr::map_dbl(travel_lodge_food$applicable_days, ~{
#'     ifelse(is.na(.x), 0, as.numeric(day_information_list[[.x]]))
#'   })
#'
#'   # Prep the variables for the df
#'   expense_type <- travel_lodge_food$expense_type
#'   line_item <- travel_lodge_food$line_item
#'   notes <- ifelse(number_per_day > 0, "Add Notes Here", "NA")
#'   cost_per_person_per_day <- travel_lodge_food$cost_per_person_per_day
#'   total_Travel_Lodge_Food <- as.numeric(cost_per_person_per_day) * as.numeric(number_per_day) * as.numeric(applicable_days)
#'   n_Prod_Travel_Lodge_Food <- length(expense_type)
#'
#'   # Create the df
#'   input_travel_lodge_food <- data.frame(
#'     "Prod_Travel_Lodge_Food" = expense_type,
#'     "line_item" = line_item,
#'     "notes" = notes,
#'     "cost_per_person_per_day" = cost_per_person_per_day,
#'     "number_per_day" = number_per_day,
#'     "applicable_days" = applicable_days,
#'     "total" = total_Travel_Lodge_Food,
#'     "X_NA_1" = rep("",  n_Prod_Travel_Lodge_Food),
#'     "X_NA_2" = rep("",  n_Prod_Travel_Lodge_Food),
#'     "X_NA_3" = rep("",  n_Prod_Travel_Lodge_Food),
#'     "X_NA_4" = rep("",  n_Prod_Travel_Lodge_Food),
#'     "X_NA_5" = rep("",  n_Prod_Travel_Lodge_Food),
#'     "X_NA_6" = rep("",  n_Prod_Travel_Lodge_Food),
#'     "X_NA_7" = rep("",  n_Prod_Travel_Lodge_Food),
#'     "Items" = expense_type,
#'     "Total_Costs" = total_Travel_Lodge_Food,
#'     "Actual_Costs" = rep(0,  n_Prod_Travel_Lodge_Food)
#'   )
#'
#'   # Return the df
#'   return(input_travel_lodge_food)
#' }
#'
#' # Location Rental DF ------------------------------------------------------
#'
#' #' create_input_location_rental
#' #' Create the df for the location rental
#' #'
#' #' @param type_of_shooting_location a vector with the potential locations
#' #' @param estimated_shoot_days_needed numeric number of days the shoot should take
#' #'
#' #' @return input_location_rental
#' #' @export
#' #'
#' create_input_location_rental <- function(
#'     type_of_shooting_location = NULL,
#'     estimated_shoot_days_needed = NULL
#' ){
#'   # If the type of shooting location is NULL, keep it that way
#'   if(is.null(type_of_shooting_location)) {
#'     type_of_shooting_location <- c()
#'   }
#'
#'   # If the estimated shoot days is NULL, change it to zero
#'   if(is.null(estimated_shoot_days_needed)) {
#'     estimated_shoot_days_needed <- 0
#'   }
#'
#'   # Set the options for the locations
#'   location_options <- c("Studio", "Airbnb/VRBO", "Other")
#'
#'   # Set the line items
#'   line_item <- c("D122", "D122", "D122")
#'
#'   # If the location were selected, add "Please Enter Name
#'   name <- ifelse(location_options %in% type_of_shooting_location, "Please Enter Name", "NA")
#'
#'   # Also mark the amount as 1
#'   amount <- ifelse(location_options %in% type_of_shooting_location, 1, 0)
#'
#'   # Also enter the shoot days for each selected
#'   day_quantity <- ifelse(
#'     location_options %in% type_of_shooting_location,
#'     estimated_shoot_days_needed,
#'     0
#'   )
#'
#'   # How many rows will there be
#'   N_location_options <- length(location_options)
#'
#'   # Enter Zeros for the cost
#'   avg_cost_day_ea <- rep(0, N_location_options)
#'
#'   # Calculate the total
#'   total <- as.numeric(day_quantity) * as.numeric(amount) * as.numeric(avg_cost_day_ea)
#'
#'   # Create the df
#'   input_location_rental <- data.frame(
#'     "Location" = location_options,
#'     "line_item" = line_item,
#'     "Name" = name,
#'     "avg_cost_day_ea" = avg_cost_day_ea,
#'     "amount" = amount,
#'     "day_quantity" = day_quantity,
#'     "total" = total,
#'     "X_NA_1" = rep("", N_location_options),
#'     "X_NA_2" = rep("", N_location_options),
#'     "X_NA_3" = rep("", N_location_options),
#'     "X_NA_4" = rep("", N_location_options),
#'     "X_NA_5" = rep("", N_location_options),
#'     "X_NA_6" = rep("", N_location_options),
#'     "X_NA_7" = rep("", N_location_options),
#'     "Locations" = location_options,
#'     "Total_Costs" = amount,
#'     "Actual_Costs" = rep(0, N_location_options)
#'   )
#'
#'   # Return the df
#'   return(input_location_rental)
#' }
#'
#' # Talent / Cast DF --------------------------------------------------------
#'
#' #' create_input_cast
#' #' Create the df for the cast/talent
#' #'
#' #' @param need_hired_leads  need_hired_leads
#' #' @param need_hired_speaking_extras  need_hired_speaking_extras
#' #' @param need_hired_non_speaking_extras need_hired_non_speaking_extras
#' #' @param n_adult_leads n_adult_leads
#' #' @param n_children_leads n_children_leads
#' #' @param n_adult_speaking_extras n_adult_speaking_extras
#' #' @param n_children_speaking_extras n_children_speaking_extras
#' #' @param n_adult_non_speaking_extras n_adult_non_speaking_extras
#' #' @param n_children_non_speaking_extras n_children_non_speaking_extras
#' #' @param estimated_shoot_days_needed estimated_shoot_days_needed
#' #'
#' #' @return input_cast
#' #' @export
#' #'
#' create_input_cast <- function(
#'     need_hired_leads = NULL,
#'     need_hired_speaking_extras = NULL,
#'     need_hired_non_speaking_extras = NULL,
#'     n_adult_leads = NULL,
#'     n_children_leads = NULL,
#'     n_adult_speaking_extras = NULL,
#'     n_children_speaking_extras = NULL,
#'     n_adult_non_speaking_extras = NULL,
#'     n_children_non_speaking_extras = NULL,
#'     estimated_shoot_days_needed = NULL
#' ){
#'
#'   # If Null, make it false
#'   if(is.null(need_hired_leads)) need_hired_leads <- FALSE
#'
#'   # If Null, make it false
#'   if(is.null(need_hired_speaking_extras)) need_hired_speaking_extras <- FALSE
#'
#'   # If Null, make it false
#'   if(is.null(need_hired_non_speaking_extras)) need_hired_non_speaking_extras <- FALSE
#'
#'   # If Null, make it zero
#'   if(is.null(n_adult_leads)) n_adult_leads <- 0
#'
#'   # Make sure it is numeric
#'   n_adult_leads <- as.numeric(n_adult_leads)
#'
#'   # If Null, make it zero
#'   if(is.null(n_children_leads)) n_children_leads <- 0
#'
#'   # Make sure it is numeric
#'   n_children_leads <- as.numeric(n_children_leads)
#'
#'   # If Null, make it zero
#'   if(is.null(n_adult_speaking_extras)) n_adult_speaking_extras <- 0
#'
#'   # Clean adult_speaking_extras
#'   warn_10_plus_adult_speaking_extras <- warn_of_10_plus(n_adult_speaking_extras)
#'   n_adult_speaking_extras <- convert_to_10_from_plus(n_adult_speaking_extras)
#'   n_adult_speaking_extras <- as.numeric(n_adult_speaking_extras)
#'
#'   # If Null, make it zero
#'   if(is.null(n_children_speaking_extras)) n_children_speaking_extras <- 0
#'
#'   # Clean n_children_speaking_extras
#'   warn_10_plus_children_speaking_extras <- warn_of_10_plus(n_children_speaking_extras)
#'   n_children_speaking_extras <- convert_to_10_from_plus(n_children_speaking_extras)
#'   n_children_speaking_extras <- as.numeric(n_children_speaking_extras)
#'
#'   # If Null, make it zero
#'   if(is.null(n_adult_non_speaking_extras)) n_adult_non_speaking_extras <- 0
#'
#'   # Clean n_adult_non_speaking_extras
#'   warn_10_plus_adult_non_speaking_extras <- warn_of_10_plus(n_adult_non_speaking_extras)
#'   n_adult_non_speaking_extras <- convert_to_10_from_plus(n_adult_non_speaking_extras)
#'   n_adult_non_speaking_extras <- as.numeric(n_adult_non_speaking_extras)
#'
#'   # If Null, make it zero
#'   if(is.null(n_children_non_speaking_extras)) n_children_non_speaking_extras <- 0
#'
#'   # Clean n_children_non_speaking_extras
#'   warn_10_plus_children_non_speaking_extras <- warn_of_10_plus(n_children_non_speaking_extras)
#'   n_children_non_speaking_extras <- convert_to_10_from_plus(n_children_non_speaking_extras)
#'   n_children_non_speaking_extras <- as.numeric(n_children_non_speaking_extras)
#'
#'   # If Null, make it zero
#'   if(is.null(estimated_shoot_days_needed)) estimated_shoot_days_needed <- 0
#'   estimated_shoot_days_needed <- as.numeric(estimated_shoot_days_needed)
#'
#'   # If false, make vars zero
#'   if(need_hired_leads == FALSE){
#'     n_adult_leads <- 0
#'     n_children_leads <- 0
#'   }
#'
#'   # If false, make vars zero
#'   if(need_hired_speaking_extras == FALSE){
#'     n_adult_speaking_extras <- 0
#'     n_children_speaking_extras <- 0
#'   }
#'
#'   # If false, make vars zero
#'   if(need_hired_non_speaking_extras == FALSE){
#'     n_adult_non_speaking_extras <- 0
#'     n_children_non_speaking_extras <- 0
#'   }
#'
#'   # Set up row names
#'   cast <- c("Adult Lead", "Child Lead", "Adult Speaking Extra", "Child Speaking Extra", "Adult Non-Speaking Extra", "Child Non-Speaking Extra", "VO Labor", "Agency Fees")
#'
#'   # How many rows
#'   n_cast <- length(cast)
#'
#'   # Set the rate for the actors
#'   day_rate <- c(400, 300, 150, 150, 150, 150, 0, 0)
#'
#'   # How many cast members
#'   n_cast_to_hire <- c(n_adult_leads, n_children_leads, n_adult_speaking_extras, n_children_speaking_extras, n_adult_non_speaking_extras, n_children_non_speaking_extras, 0, 0)
#'
#'   # How many days will the shoot be
#'   shoot_days <- rep(estimated_shoot_days_needed, n_cast)
#'
#'   # How much will each type cost
#'   total_cast_rate <- as.numeric(day_rate) * as.numeric(shoot_days) * as.numeric(n_cast_to_hire)
#'
#'   # Enter "Please enter name" if anyone is selected
#'   name <- ifelse(n_cast_to_hire > 0, "Please Enter Name", "NA")
#'
#'   # Add calcualtion for agency fees
#'   agency_index <- which(cast == "Agency Fees")
#'
#'   # Add this to thetotal cost rate
#'   total_cast_rate[agency_index] <- sum(total_cast_rate) * .15
#'
#'   # Set up DF
#'   input_cast <- data.frame(
#'     "Cast" = cast,
#'     "line_item" = c(rep("?", 6), "M259", "?"),
#'     "Name" = name,
#'     n_cast_to_hire,
#'     day_rate,
#'     shoot_days,
#'     total_cast_rate,
#'     "X_NA_1" = rep("", 8),
#'     "X_NA_2" = rep("", 8),
#'     "X_NA_3" = rep("", 8),
#'     "X_NA_4" = rep("", 8),
#'     "X_NA_5" = rep("", 8),
#'     "X_NA_6" = rep("", 8),
#'     "X_NA_7" = rep("", 8),
#'     "Talent" = cast,
#'     "Total_Costs" = total_cast_rate,
#'     "Actual_Costs" = rep(0, 8)
#'   )
#'
#'   # Return DF
#'   return(input_cast)
#' }
#'
#'
#' # Equipment DF ------------------------------------------------------------
#'
#' #' create_input_equipment
#' #' Create the df for the equipment/gear
#' #'
#' #' @param input_equipment_options input_equipment_options
#' #' @param estimated_shoot_days_needed estimated_shoot_days_needed
#' #'
#' #' @return input_equipment
#' #' @export
#' #'
#' create_input_equipment <- function(
#'     input_equipment_options = NULL,
#'     estimated_shoot_days_needed = NULL
#' ) {
#'
#'   # IF NULL, leave it as an empty vector
#'   if(is.null(input_equipment_options)){
#'     input_equipment_options <- c()
#'   }
#'
#'   # IF NULL, change it to zero
#'   if(is.null(estimated_shoot_days_needed)){
#'     estimated_shoot_days_needed <- 0
#'   }
#'
#'   # Use the equipment pricing data from the package to start the dataframe
#'   pricing_for_selected_equipment <- data.frame(
#'     Equipment = pricing_for_equipment_packages$component,
#'     line_item = pricing_for_equipment_packages$line_item,
#'     name = rep("NA", nrow(pricing_for_equipment_packages)),
#'     avg_cost_day_ea = pricing_for_equipment_packages$cost
#'   )
#'
#'   # If the piece of equipment was selected, mark the amount as 1
#'   pricing_for_selected_equipment$amount <- ifelse(pricing_for_selected_equipment$Equipment %in% input_equipment_options, 1, 0) %>% as.numeric()
#'
#'   # If the piece of equipment was selected, mark the name as "Please Enter Name"
#'   pricing_for_selected_equipment$name <- ifelse(pricing_for_selected_equipment$Equipment %in% input_equipment_options, "Please Enter Name", "NA")
#'
#'   # For certain selections, do custom changes to the day quantities
#'   pricing_for_selected_equipment$day_quantity <- dplyr::case_when(
#'     pricing_for_equipment_packages$component %in% input_equipment_options & pricing_for_equipment_packages$cost_type %in% "Cost Per Day" ~ as.numeric(estimated_shoot_days_needed),
#'     pricing_for_equipment_packages$component %in% input_equipment_options & pricing_for_equipment_packages$cost_type %in% "Flat Rate" ~ 1,
#'     pricing_for_equipment_packages$component %in% input_equipment_options & pricing_for_equipment_packages$cost_type %in% "Percentage of Total" ~ 1,
#'     !(pricing_for_equipment_packages$component %in% input_equipment_options) ~ 0
#'   ) %>% as.numeric()
#'
#'   # Filter for expendables
#'   filter_for_expendables <- pricing_for_equipment_packages$component == "Expendables/Buffer"
#'
#'   # If Expendables was selected
#'   if(pricing_for_selected_equipment$amount[filter_for_expendables] > 0){
#'     percentage_for_expendables <- pricing_for_selected_equipment$avg_cost_day_ea[filter_for_expendables]
#'     total_equipment_cost <- sum(pricing_for_selected_equipment$avg_cost_day_ea[pricing_for_selected_equipment$amount > 0])
#'     pricing_for_selected_equipment$avg_cost_day_ea[filter_for_expendables] <- percentage_for_expendables * total_equipment_cost
#'   } else {
#'     pricing_for_selected_equipment$avg_cost_day_ea[filter_for_expendables] <- 0
#'   }
#'
#'   # Calculate the total amount
#'   pricing_for_selected_equipment$total <- as.numeric(pricing_for_selected_equipment$amount) * as.numeric(pricing_for_selected_equipment$day_quantity) * as.numeric(pricing_for_selected_equipment$avg_cost_day_ea)
#'
#'   # pricing_for_equipment_packages %>%
#'   #   dplyr::filter(component %in% input_equipment_options)
#'
#'   # Calculate the number of rows in the df
#'   number_of_equipment_rows <- nrow(pricing_for_selected_equipment)
#'
#'   # Put the df together
#'   input_equipment <- data.frame(
#'     "Equipment" = pricing_for_selected_equipment$Equipment,
#'     "line_item" = pricing_for_selected_equipment$line_item,
#'     "name" = pricing_for_selected_equipment$name,
#'     "avg_cost_day_ea" = pricing_for_selected_equipment$avg_cost_day_ea,
#'     "amount" = pricing_for_selected_equipment$amount,
#'     "day_quantity" = pricing_for_selected_equipment$day_quantity,
#'     "total" = pricing_for_selected_equipment$total,
#'     "X_NA_1" = rep("", number_of_equipment_rows),
#'     "X_NA_2" = rep("", number_of_equipment_rows),
#'     "X_NA_3" = rep("", number_of_equipment_rows),
#'     "X_NA_4" = rep("", number_of_equipment_rows),
#'     "X_NA_5" = rep("", number_of_equipment_rows),
#'     "X_NA_6" = rep("", number_of_equipment_rows),
#'     "X_NA_7" = rep("", number_of_equipment_rows),
#'     "Gear" = pricing_for_selected_equipment$Equipment,
#'     "Total_Costs" = pricing_for_selected_equipment$total,
#'     "Actual_Costs" = rep(0, number_of_equipment_rows)
#'   )
#'
#'   # Return the datafram
#'   return(input_equipment)
#' }
#'
#'
#' # Post Production DF ------------------------------------------------------
#'
#' #' create_input_post_production
#' #' Create the df for the post production information
#' #'
#' #' @param selected_post_production selected_post_production
#' #'
#' #' @return input_post_production
#' #' @export
#' #'
#' create_input_post_production <- function(
#'     selected_post_production = NULL
#' ){
#'
#'   # If NULL, leave it as an empty vector
#'   if(is.null(selected_post_production)) {
#'     selected_post_production <- c()
#'   }
#'
#'   # Obtain needed information from the post pricing data stored in the package
#'   post_production_options <- post_production_rates$Post_Production
#'   post_production_line_items <- post_production_rates$Line_Item
#'   number_of_post_production <- length(post_production_options)
#'   post_production_rate <- post_production_rates$Rate %>% as.numeric()
#'
#'   # If selected, enter "Please Enter Name" to the name
#'   post_production_name <- ifelse(
#'     post_production_options %in% selected_post_production,
#'     "Please Enter Name",
#'     "NA")
#'
#'   # If Selected, give it the quantity of 1
#'   post_production_qty <- ifelse(
#'     post_production_options %in% selected_post_production,
#'     1,
#'     0
#'   ) %>% as.numeric()
#'
#'   # Calculate the total cost
#'   post_production_total <- as.numeric(post_production_rate) * as.numeric(post_production_qty)
#'
#'   # Put the dataframe together
#'   input_post_production <- data.frame(
#'     "Post Production" = post_production_options,
#'     "line_item" = post_production_line_items,
#'     "name" = post_production_name,
#'     "rate" = post_production_rate,
#'     "qty" = post_production_qty,
#'     "total" = post_production_total,
#'     "X_NA_1" = rep("", number_of_post_production ),
#'     "X_NA_2" = rep("", number_of_post_production ),
#'     "X_NA_3" = rep("", number_of_post_production ),
#'     "X_NA_4" = rep("", number_of_post_production ),
#'     "X_NA_5" = rep("", number_of_post_production ),
#'     "X_NA_6" = rep("", number_of_post_production ),
#'     "X_NA_7" = rep("", number_of_post_production ),
#'     "X_NA_8" = rep("", number_of_post_production ),
#'     "Post Production Items" = post_production_options,
#'     "Total_Costs" = post_production_total,
#'     "Actual_Costs" = rep(0, number_of_post_production )
#'   )
#'
#'   # Print the dataframe
#'   return(input_post_production)
#' }
#'
#'
#' # Add Summary Row -------------------------------------------------------
#'
#' #' add_summary_row
#' #' Adjustments and Enhancements of Data Frames
#' #'
#' #' @param df dataframe to add
#' #'
#' #' @return df
#' #' @export
#' #'
#' add_summary_row <- function(df){
#'   # Calculate the sums for everything
#'   total_row_from_df <- df %>% summarize(across(everything(), special_sum))
#'
#'   # Indicate where the total should go
#'   where_total_should_go <- length(df) - 2
#'   total_row_from_df[ , where_total_should_go] <- as.character(total_row_from_df[ , where_total_should_go])
#'   total_row_from_df[1, where_total_should_go] <- "Total"
#'   total_row_from_df[1, seq(1, length(df) - 3)] <- NA
#'   df <- df %>% rbind(total_row_from_df)
#'
#'   return(df)
#' }
#'
#'
#' # Get Total from DF  -------------------------------------------------------
#'
#' #' get_total_from_df
#' #'
#' #' @param df  df
#' #'
#' #' @return total_from_df
#' #' @export
#' #'
#'
#' get_total_from_df <- function(df) {
#'   # Number of rows
#'   df_last_row <- nrow(df)
#'
#'   # Number of the columns
#'   df_last_col <- ncol(df)
#'
#'   # Total from DF
#'   total_from_df <- df[df_last_row, df_last_col - 1]
#'
#'   # Make sure it is numeric
#'   total_from_df <- as.numeric(total_from_df)
#'
#'   # Return it
#'   return(total_from_df)
#' }
#'
#' # Add Summary Table -------------------------------------------------------
#' #' Title
#' #'
#' #' @param input_client_name  input_client_name
#' #' @param input_moore_production_staff  input_moore_production_staff
#' #' @param input_production_freelance input_production_freelance
#' #' @param input_travel_lodge_food input_travel_lodge_food
#' #' @param input_location_rental input_location_rental
#' #' @param input_cast input_cast
#' #' @param input_equipment input_equipment
#' #' @param input_post_production input_post_production
#' #' @param Total_Fee_projected Total_Fee_projected
#' #' @param COG_percent_Goal COG_percent_Goal
#' #'
#' #' @return input_total_project_costs
#' #' @export
#' #'
#' create_DF_summary_table <- function(
#'     input_client_name,
#'     input_moore_production_staff,
#'     input_production_freelance,
#'     input_travel_lodge_food,
#'     input_location_rental,
#'     input_cast,
#'     input_equipment,
#'     input_post_production,
#'     Total_Fee_projected = 85000,
#'     COG_percent_Goal = .35
#' ) {
#'
#'   # Calculate the total
#'   COG_dollar_Est <- get_total_from_df(input_moore_production_staff) +
#'     get_total_from_df(input_production_freelance) +
#'     get_total_from_df(input_travel_lodge_food) +
#'     get_total_from_df(input_location_rental) +
#'     get_total_from_df(input_cast) +
#'     get_total_from_df(input_equipment) +
#'     get_total_from_df(input_post_production)
#'
#'   # What was the projected total
#'   Total_Fee_projected <- Total_Fee_projected
#'
#'   # Divide the total cost by the projected cost
#'   COG_percent_Est <- as.numeric(COG_dollar_Est) / as.numeric(Total_Fee_projected)
#'
#'   # Set the percentage goal
#'   COG_percent_Goal <- COG_percent_Goal
#'
#'   # Calculate the dollar amount for the goal
#'   COG_dollar_Goal <- Total_Fee_projected * COG_percent_Goal
#'
#'   # Subtract the cost from the projected cost
#'   Margin <- Total_Fee_projected - COG_dollar_Est
#'
#'   # Subtract the cost from the goal
#'   Remaining <- COG_dollar_Goal - COG_dollar_Est
#'
#'   # Provide the names for the metrics
#'   Overall_Metrics <- c("COG_dollar_Est", "COG_percent_Est", "Total_Fee_projected",
#'                        "COG_percent_Goal", "COG_dollar_Goal", "Margin", "Remaining")
#'
#'   # Provide the metrics
#'   Overall_Estimates <- c(COG_dollar_Est, COG_percent_Est, Total_Fee_projected, COG_percent_Goal,
#'                          COG_dollar_Goal, Margin, Remaining)
#'
#'   # How many metrics are there
#'   n_metrics <- length(Overall_Metrics)
#'
#'   # Add zeros for each metric
#'   Total_Actual <- rep(0, n_metrics)
#'
#'   # Put the df together
#'   input_total_project_costs <- data.frame(
#'     "X_NA_1" = rep("", n_metrics),
#'     "X_NA_2" = rep("", n_metrics),
#'     "X_NA_3" = rep("", n_metrics),
#'     "X_NA_4" = rep("", n_metrics),
#'     "X_NA_5" = rep("", n_metrics),
#'     "X_NA_6" = rep("", n_metrics),
#'     "X_NA_7" = rep("", n_metrics),
#'     "X_NA_8" = rep("", n_metrics),
#'     "X_NA_9" = rep("", n_metrics),
#'     "X_NA_10" = rep("", n_metrics),
#'     "X_NA_11" = rep("", n_metrics),
#'     "X_NA_12" = rep("", n_metrics),
#'     "X_NA_13" = rep("", n_metrics),
#'     "X_NA_14" = rep("", n_metrics),
#'     Overall_Metrics,
#'     Overall_Estimates,
#'     Total_Actual
#'   )
#'
#'   # Make the number round
#'   input_total_project_costs$Overall_Estimates <- round(input_total_project_costs$Overall_Estimates)
#'
#'   # Return the df
#'   return(input_total_project_costs)
#' }
#'
#'
#'
#'
#'
#'
