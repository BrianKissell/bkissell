#' #' server
#' #'
#' #' @description A fct function
#' #'
#' #' @return The return value, if any, from executing the function.
#' #'
#' #' @noRd
#'
#' ################################################################################
#' special_sum <- function(x){
#'   if(is.numeric(x)) sum(x, na.rm = TRUE) else ""
#' }
#'
#' ################################################################################
#'
#'
#'
#' ################################################################################
#'
#' # convert_to_10_from_plus <- function(var) {
#' #   as.numeric(ifelse(var == "10+", 10, var))
#' # }
#' #
#' # warn_of_10_plus <- function(var) {
#' #   ifelse(var == "10+", TRUE, FALSE)
#' # }
#' ################################################################################
#'
#' insert_cost_report_tab <- function(tab_name, tab_content, trigger, session) {
#'
#'   if(as.numeric(trigger) == 1) {
#'     insertTab(
#'       "tabs",
#'       tabPanel(tab_name, tab_content),
#'       select = TRUE
#'     )
#'   } else if(as.numeric(trigger) > 1) {
#'     updateTabsetPanel(session, "tabs", selected = tab_name)
#'   }
#' }
#'
#' ################################################################################
#'
#' create_file_name <- function(
#'     client_name_var = NULL,
#'     report_creator_name_var = NULL,
#'     report_creation_date_var = NULL){
#'   if(is.null(client_name_var)) {
#'     client_name_var <- "Client Name"
#'   }
#'   if(is.null(report_creator_name_var)) {
#'     report_creator_name_var <- "Reporter Created Name"
#'   }
#'   if(is.null(report_creation_date_var)) {
#'     report_creator_name_var <- "No Date Provided"
#'   }
#'   snake_client_name <- snakecase::to_snake_case(as.character(client_name_var))
#'   snake_client_report_creator_name <- snakecase::to_snake_case(as.character(report_creator_name_var))
#'   snake_client_report_creation_date <- snakecase::to_snake_case(as.character(report_creation_date_var))
#'   file_name <- paste0(snake_client_name, "_", snake_client_report_creator_name, "_", snake_client_report_creation_date, "_production_cost_report", ".xlsx")
#'   return(file_name)
#' }
#'
#'
#' DEV_save_app_data_versions_df <- function(df, file_path = "dev/data/df.csv") {
#'   readr::write_csv(df, file_path)
#' }
#'
#'
#'
#'
#' check_function_inputs <- function(
#'     reactive_to_check,
#'     vec_of_check_types,
#'     print_checks = FALSE
#' ){
#'
#'   checks_for_each_var <- c()
#'
#'   for(i in seq_along(reactive_to_check)){
#'
#'     if(vec_of_check_types[[i]] == "Not Null") {
#'
#'       check_each_var <- !is.null(reactive_to_check[[i]])
#'
#'     } else if(vec_of_check_types[[i]] == "Not Please Select") {
#'
#'       check_each_var <- reactive_to_check[[i]] != "Please Select"
#'
#'     } else if(vec_of_check_types[[i]] == "Not Empty DF") {
#'
#'       check_each_var <- !(nrow(reactive_to_check[[i]]) < 1)
#'
#'     }
#'
#'     checks_for_each_var <- c(checks_for_each_var, check_each_var)
#'
#'   }
#'
#'   check_all <- all(checks_for_each_var)
#'
#'   if(print_checks == TRUE) {
#'
#'     message("Reactive Vars")
#'     message(reactive_to_check)
#'
#'     message("Individual Check Results")
#'     message(checks_for_each_var)
#'
#'     message("All Checks Passed")
#'     message(check_all)
#'
#'   }
#'
#'   return(check_all)
#' }
#'
#' #
#' # create_people_data_list <- function(
#'     #     moore_production_staff,
#' #     production_freelance,
#' #     location_of_shoot,
#' #     estimated_shoot_days_needed,
#' #     n_adult_leads,
#' #     n_children_leads,
#' #     n_adult_speaking_extras,
#' #     n_children_speaking_extras,
#' #     n_adult_non_speaking_extras,
#' #     n_children_non_speaking_extras,
#' #     people_need_catering_n
#' # ) {
#' #
#' #   if(is.null(moore_production_staff)) {
#' #     moore_production_staff <- c()
#' #   }
#' #
#' #   if(is.null(production_freelance)) {
#' #     production_freelance <- data.frame(
#' #       crew_position = character(0),
#' #       travel_or_local = character(0),
#' #       needed_prep_days = numeric(0)
#' #     )
#' #   }
#' #
#' #   if(is.null(location_of_shoot)) {
#' #     location_of_shoot <- "None"
#' #   }
#' #
#' #
#' #   if(is.null(estimated_shoot_days_needed)) {
#' #     estimated_shoot_days_needed <- 0
#' #   }
#' #
#' #   estimated_shoot_days_needed <- as.numeric(estimated_shoot_days_needed)
#' #
#' #   if(is.null(n_adult_leads)) {
#' #     n_adult_leads <- 0
#' #   }
#' #
#' #   n_adult_leads <- as.numeric(n_adult_leads)
#' #
#' #   if(is.null(n_children_leads)) {
#' #     n_children_leads <- 0
#' #   }
#' #
#' #   n_children_leads <- as.numeric(n_children_leads)
#' #
#' #   if(is.null(n_adult_speaking_extras)) {
#' #     n_adult_speaking_extras <- 0
#' #   }
#' #
#' #   n_adult_speaking_extras <- as.numeric(n_adult_speaking_extras)
#' #
#' #   if(is.null(n_children_speaking_extras)) {
#' #     n_children_speaking_extras <- 0
#' #   }
#' #
#' #   n_children_speaking_extras <- as.numeric(n_children_speaking_extras)
#' #
#' #   if(is.null(n_adult_non_speaking_extras)) {
#' #     n_adult_non_speaking_extras <- 0
#' #   }
#' #
#' #   n_adult_non_speaking_extras<- as.numeric(n_adult_non_speaking_extras)
#' #
#' #   if(is.null(n_children_non_speaking_extras)) {
#' #     n_children_non_speaking_extras <- 0
#' #   }
#' #
#' #   n_children_non_speaking_extras <- as.numeric(n_children_non_speaking_extras)
#' #
#' #   if(is.null(people_need_catering_n)) {
#' #     people_need_catering_n <- 0
#' #   }
#' #
#' #   people_need_catering_n <- as.numeric(people_need_catering_n)
#' #
#' #   n_moore_production_staff <- length(moore_production_staff)
#' #   n_moore_production_staff_travel <- ifelse(!(location_of_shoot %in% c("Tulsa, Ok.", "None")), length(moore_production_staff), 0)
#' #
#' #
#' #   production_freelance$location_of_shoot <- rep(location_of_shoot, nrow(production_freelance))
#' #
#' #   production_freelance$travel_needed <- dplyr::case_when(
#' #     production_freelance$location_of_shoot == "Tulsa, Ok." ~ "No",
#' #     production_freelance$location_of_shoot == "None" ~ "No",
#' #     production_freelance$location_of_shoot != "Tulsa, Ok." & production_freelance$travel_or_local == "Travel is needed" ~ "Yes",
#' #     production_freelance$location_of_shoot != "Tulsa, Ok." & production_freelance$travel_or_local == "Travel is not needed" ~ "No"
#' #   )
#' #
#' #   n_production_freelance <- production_freelance %>%
#' #     nrow()
#' #
#' #   n_production_freelance_travel <- production_freelance %>%
#' #     dplyr::filter(.data[["travel_needed"]] == "Yes") %>%
#' #     nrow()
#' #
#' #   n_travel <- as.numeric(n_moore_production_staff_travel) + as.numeric(n_production_freelance_travel)
#' #
#' #   n_talent <- as.numeric(n_adult_leads) + as.numeric(n_children_leads) + as.numeric(n_adult_speaking_extras) + as.numeric(n_children_speaking_extras) + as.numeric(n_adult_speaking_extras) + as.numeric(n_children_speaking_extras)
#' #
#' #   n_additional_people_at_shoot <- as.numeric(people_need_catering_n)
#' #
#' #   n_total_people_attending_shoot <- as.numeric(n_moore_production_staff) + as.numeric(n_production_freelance) + as.numeric(n_talent) + as.numeric(n_additional_people_at_shoot)
#' #
#' # }
#'
#'
#' #
#' # create_moore_staff_numbers_list <- function(moore_production_staff = NULL, location_of_shoot = NULL) {
#' #
#' #   if(is.null(moore_production_staff)){
#' #     moore_production_staff <- c()
#' #   }
#' #   if(is.null(location_of_shoot)) {
#' #     location_of_shoot <- "None"
#' #   }
#' #
#' #   n_moore_staff <- length(moore_production_staff)
#' #   n_moore_travel <- ifelse(!(location_of_shoot %in% c("Tulsa, Ok.", "None")), n_moore_staff, 0)
#' #   n_moore_local <- ifelse((location_of_shoot %in% c("Tulsa, Ok.", "None")), n_moore_staff, 0)
#' #
#' #   list(
#' #     n_moore_staff = n_moore_staff,
#' #     n_moore_travel = n_moore_travel,
#' #     n_moore_local = n_moore_local
#' #   )
#' # }
#' #
#' # # freelance_staff <- data.frame(
#' # #   crew_position = c("Producer", "Producer"),
#' # #   travel_or_local = c("Travel is not needed", "Travel is needed"),
#' # #   needed_prep_days = c(0, 0)
#' # # )
#' #
#' #
#' # create_freelance_staff_numbers_list <- function(freelance_staff = NULL) {
#' #
#' #   if(is.null(freelance_staff)) {
#' #     freelance_staff <- data.frame(
#' #       crew_position = character(0),
#' #       travel_or_local = character(0),
#' #       needed_prep_days = numeric(0)
#' #     )
#' #   } else {
#' #     freelance_staff <- freelance_staff %>%
#' #       dplyr::select(
#' #         crew_position = freelance_position,
#' #         travel_or_local = is_travel_needed,
#' #         needed_prep_days = are_prep_days_needed
#' #                     )
#' #   }
#' #
#' #   # freelance_staff <- data.frame(
#' #   #   crew_position = c("Producer", "Producer"),
#' #   #   travel_or_local = c("Travel is not needed", "Travel is needed"),
#' #   #   needed_prep_days = c(0, 0)
#' #   # )
#' #
#' #   if(nrow(freelance_staff) > 0){
#' #
#' #     n_freelance_staff <- nrow(freelance_staff)
#' #     n_freelance_travel <- freelance_staff %>%
#' #       dplyr::filter(.data[["travel_or_local"]] == "Travel is needed")  %>%
#' #       nrow()
#' #     n_freelance_local <- freelance_staff %>%
#' #       dplyr::filter(.data[["travel_or_local"]] == "Travel is not needed")  %>%
#' #       nrow()
#' #
#' #   } else {
#' #     n_freelance_staff <- 0
#' #     n_freelance_travel <- 0
#' #     n_freelance_local <- 0
#' #   }
#' #
#' #   list(
#' #     n_freelance_staff = n_freelance_staff,
#' #     n_freelance_travel = n_freelance_travel,
#' #     n_freelance_local = n_freelance_local
#' #   )
#' # }
#' #
#' #
#' # ################################################################################
#' #
#' # create_talent_numbers_list <- function(
#'     #     n_adult_leads= NULL,
#' #     n_children_leads= NULL,
#' #     n_adult_speaking_extras= NULL,
#' #     n_children_speaking_extras= NULL,
#' #     n_adult_non_speaking_extras= NULL,
#' #     n_children_non_speaking_extras= NULL
#' #     ) {
#' #
#' #   if(is.null(n_adult_leads)) {
#' #     n_adult_leads <- 0
#' #   }
#' #
#' #   n_adult_leads <- as.numeric(n_adult_leads)
#' #
#' #   if(is.null(n_children_leads)) {
#' #     n_children_leads <- 0
#' #   }
#' #
#' #   n_children_leads <- as.numeric(n_children_leads)
#' #
#' #   if(is.null(n_adult_speaking_extras)) {
#' #     n_adult_speaking_extras <- 0
#' #   }
#' #
#' #   warn_10_plus_adult_speaking_extras <- warn_of_10_plus(n_adult_speaking_extras)
#' #   n_adult_speaking_extras <- convert_to_10_from_plus(n_adult_speaking_extras)
#' #
#' #   if(is.null(n_children_speaking_extras)) {
#' #     n_children_speaking_extras <- 0
#' #   }
#' #
#' #   warn_10_plus_children_speaking_extras <- warn_of_10_plus(n_children_speaking_extras)
#' #   n_children_speaking_extras <- convert_to_10_from_plus(n_children_speaking_extras)
#' #
#' #   if(is.null(n_adult_non_speaking_extras)) {
#' #     n_adult_non_speaking_extras <- 0
#' #   }
#' #
#' #   warn_10_plus_adult_non_speaking_extras <- warn_of_10_plus(n_adult_non_speaking_extras)
#' #   n_adult_non_speaking_extras <- convert_to_10_from_plus(n_adult_non_speaking_extras)
#' #
#' #   if(is.null(n_children_non_speaking_extras)) {
#' #     n_children_non_speaking_extras <- 0
#' #   }
#' #
#' #   warn_10_plus_children_non_speaking_extras <- warn_of_10_plus(n_children_non_speaking_extras)
#' #   n_children_non_speaking_extras <- convert_to_10_from_plus(n_children_non_speaking_extras)
#' #
#' #   n_talent_travel <- 0
#' #
#' #   n_talent_local <- n_adult_leads + n_children_leads + n_adult_speaking_extras + n_children_speaking_extras + n_adult_non_speaking_extras + n_children_non_speaking_extras
#' #
#' #   n_talent_total <- n_talent_travel + n_talent_local
#' #
#' #   list(
#' #     n_talent_total = n_talent_total,
#' #     n_talent_travel = n_talent_travel,
#' #     n_talent_local = n_talent_local,
#' #     warn_10_plus_adult_speaking_extras = warn_10_plus_adult_speaking_extras,
#' #     warn_10_plus_children_speaking_extras = warn_10_plus_children_speaking_extras,
#' #     warn_10_plus_adult_non_speaking_extras = warn_10_plus_adult_non_speaking_extras,
#' #     warn_10_plus_children_non_speaking_extras = warn_10_plus_children_non_speaking_extras
#' #   )
#' # }
#' #
#' #
#' # # create_talent_numbers_list(
#' # #     n_adult_leads = 1,
#' # #     n_children_leads = 1,
#' # #     n_adult_speaking_extras = 1,
#' # #     n_children_speaking_extras = 1,
#' # #     n_adult_non_speaking_extras = "10+",
#' # #     n_children_non_speaking_extras = 1
#' # # )
#' #
#' # create_overall_people_list <- function(
#'     #     moore_staff_numbers_list = NULL,
#' #     freelance_staff_numbers_list = NULL,
#' #     talent_numbers_list = NULL,
#' #     people_need_catering_n = NULL
#' # ) {
#' #
#' #   if(is.null(moore_staff_numbers_list)){
#' #     moore_staff_numbers_list <- list(
#' #       n_moore_staff = 0,
#' #       n_moore_travel = 0,
#' #       n_moore_local = 0
#' #     )
#' #   }
#' #
#' #   if(is.null(freelance_staff_numbers_list)){
#' #     freelance_staff_numbers_list <-  list(
#' #       n_freelance_staff = 0,
#' #       n_freelance_travel = 0,
#' #       n_freelance_local = 0
#' #     )
#' #   }
#' #
#' #   if(is.null(talent_numbers_list)){
#' #     talent_numbers_list <-  list(
#' #       n_talent_total = 0,
#' #       n_talent_travel = 0,
#' #       n_talent_local = 0,
#' #       warn_10_plus_adult_speaking_extras = FALSE,
#' #       warn_10_plus_children_speaking_extras = FALSE,
#' #       warn_10_plus_adult_non_speaking_extras = FALSE,
#' #       warn_10_plus_children_non_speaking_extras = FALSE
#' #     )
#' #   }
#' #
#' #   if(is.null(people_need_catering_n)){
#' #     people_need_catering_n <- 0
#' #   }
#' #
#' #   all_people_travelers <- as.numeric(moore_staff_numbers_list$n_moore_travel) +
#' #     as.numeric(freelance_staff_numbers_list$n_freelance_travel) +
#' #     as.numeric(talent_numbers_list$n_talent_travel)
#' #
#' #   all_people_local <- as.numeric(moore_staff_numbers_list$n_moore_local) +
#' #     as.numeric(freelance_staff_numbers_list$n_freelance_local) +
#' #     as.numeric(talent_numbers_list$n_talent_local) +
#' #     as.numeric(people_need_catering_n)
#' #
#' #   all_people_total <- as.numeric(all_people_travelers) + as.numeric(all_people_local)
#' #
#' #   list(
#' #     all_people_total = all_people_total,
#' #     all_people_travelers = all_people_travelers,
#' #     all_people_local = all_people_local
#' #     )
#' # }
#' #
#' #
#' # create_travel_information_list <- function(
#'     #     location_of_shoot=NULL,
#' #     needed_equipment=NULL,
#' #     estimated_shoot_days_needed=NULL,
#' #     overall_people_list=NULL
#' #     ) {
#' #
#' #   if(is.null(location_of_shoot)) {
#' #     location_of_shoot <- "None"
#' #   }
#' #
#' #   if(is.null(needed_equipment)) {
#' #     needed_equipment <- "No Package"
#' #   }
#' #
#' #   if(is.null(estimated_shoot_days_needed)) {
#' #     estimated_shoot_days_needed <- 0
#' #   }
#' #
#' #   if(is.null(overall_people_list)) {
#' #     overall_people_list <- list(
#' #       all_people_travelers = 0
#' #     )
#' #   }
#' #
#' #   # Label the type of travel
#' #   travel_type <- dplyr::case_when(
#' #     location_of_shoot %in% c("Tulsa, Ok.", "Please Select", "None") ~ "None",
#' #     location_of_shoot == "Outside of Tulsa, OK. (United States)" ~ "Domestic",
#' #     location_of_shoot == "Outside of U.S. (International)" ~ "International"
#' #   )
#' #
#' #   # Which packages
#' #   n_equipment_bags_per_package <- dplyr::case_when(
#' #     needed_equipment == "Small Package - Single Cam" ~ 5,
#' #     needed_equipment == "Medium Package - Multi-Cam" ~ 8,
#' #     needed_equipment == "Large Package - Multi-Cam" ~ 10,
#' #     needed_equipment == "No Package" ~ 0
#' #   )
#' #
#' #   n_equipment_bags_per_package <- ifelse(travel_type == "Domestic" | travel_type == "International", n_equipment_bags_per_package, 0)
#' #
#' #   if_travel_1 <- ifelse(travel_type == "Domestic" | travel_type == "International", 1, 0)
#' #
#' #   if_travel_international_1 <- ifelse(travel_type == "International", 1, 0)
#' #
#' #   if_travel_domestic_1 <- ifelse(travel_type == "Domestic", 1, 0)
#' #
#' #   n_equipment_bags_per_package_domestic <- ifelse(travel_type == "Domestic", n_equipment_bags_per_package, 0)
#' #
#' #   n_equipment_bags_per_package_international <- ifelse(travel_type == "International", n_equipment_bags_per_package, 0)
#' #
#' #
#' #   # N Travel Days
#' #   n_travel_days <- ifelse(travel_type == "None", 0, 2)
#' #
#' #   n_travel_days_domestic <- ifelse(travel_type == "Domestic", 2, 0)
#' #
#' #   n_travel_days_international <- ifelse(travel_type == "International", 2, 0)
#' #
#' #   # Constant Var
#' #   constant_var <- 1
#' #
#' #   # Shoot Days
#' #   n_shoot_days <- as.numeric(estimated_shoot_days_needed)
#' #
#' #   # All Days
#' #   n_travel_and_shoot_days <- ifelse(
#' #     as.numeric(n_travel_days) == 0,
#' #     as.numeric(n_shoot_days),
#' #     as.numeric(n_shoot_days) + as.numeric(n_travel_days)
#' #     )
#' #
#' #   # Hotel Days
#' #   n_hotel_days <- ifelse(
#' #     as.numeric(n_travel_days) == 0,
#' #     0,
#' #     as.numeric(n_shoot_days) + as.numeric(n_travel_days) - 1
#' #     )
#' #
#' #   n_employee_meals_per_person <- ifelse(
#' #     as.numeric(n_travel_days) == 0,
#' #     0,
#' #     (as.numeric(n_travel_days) * 3) + as.numeric(n_shoot_days)
#' #     )
#' #
#' #   n_client_meals <- ifelse(
#' #     as.numeric(n_travel_days) == 0,
#' #     0,
#' #     as.numeric(n_travel_and_shoot_days) / 2
#' #     )
#' #
#' #   n_uber_airport_parking <- ifelse(
#' #     as.numeric(n_travel_days) == 0,
#' #     0,
#' #     as.numeric(n_travel_and_shoot_days)
#' #     )
#' #
#' #   # Do not add to list
#' #   all_travelers <- as.numeric(overall_people_list$all_people_travelers)
#' #
#' #   all_travelers_domestic <- ifelse(travel_type == "Domestic", all_travelers, 0)
#' #
#' #   all_travelers_international <- ifelse(travel_type == "International", all_travelers, 0)
#' #
#' #
#' #   n_vehicle_rental_van <- dplyr::case_when(
#' #     as.numeric(all_travelers) <= 3 ~ 0,
#' #     (as.numeric(all_travelers) >= 4 & as.numeric(all_travelers) < 6) & needed_equipment %in% c("No Package", "Small Package - Single Cam") ~ 1,
#' #     (as.numeric(all_travelers) >= 4 & as.numeric(all_travelers) < 6) & needed_equipment %in% c("Medium Package - Multi-Cam", "Large Package - Multi-Cam") ~ 2,
#' #     as.numeric(all_travelers) >= 6 & needed_equipment %in% c("No Package", "Small Package - Single Cam") ~ 2,
#' #     as.numeric(all_travelers) >= 6 & needed_equipment %in% c("Medium Package - Multi-Cam", "Large Package - Multi-Cam") ~ 3
#' #   )
#' #
#' #   day_price_for_van <- as.numeric(pricing_travel_and_meals_df$cost_per_person_per_day[pricing_travel_and_meals_df$expense_type == "Vehicle Rental - Van(s)"])
#' #
#' #   total_price_for_van <- as.numeric(day_price_for_van) * as.numeric(n_vehicle_rental_van)
#' #
#' #   ten_percent_price_for_van <- (as.numeric(total_price_for_van) * as.numeric(n_travel_and_shoot_days)) * .1
#' #
#' #   n_vehicle_rental_car <- ifelse(
#' #     as.numeric(all_travelers) == 0,
#' #     0,
#' #     ifelse(
#' #       as.numeric(all_travelers) <= 3 & needed_equipment %in% c("No Package"),
#' #       1,
#' #       0
#' #     )
#' #   )
#' #
#' #   day_price_for_car <- as.numeric(pricing_travel_and_meals_df$cost_per_person_per_day[pricing_travel_and_meals_df$expense_type == "Vehicle Rental - Standard Car"])
#' #
#' #   total_price_for_car <- as.numeric(day_price_for_car) * as.numeric(n_vehicle_rental_car)
#' #
#' #   ten_percent_price_for_car <- (as.numeric(total_price_for_car) * as.numeric(n_travel_and_shoot_days)) * .1
#' #
#' #   parking_tolls_gas_cost <- as.numeric(ten_percent_price_for_van) + as.numeric(ten_percent_price_for_car)
#' #
#' #   list(
#' #     travel_type = travel_type,
#' #     n_equipment_bags_per_package = n_equipment_bags_per_package,
#' #     if_travel_1 = if_travel_1,
#' #     n_travel_days = n_travel_days,
#' #     constant_var = constant_var,
#' #     n_shoot_days = n_shoot_days,
#' #     n_travel_and_shoot_days = n_travel_and_shoot_days,
#' #     n_hotel_days = n_hotel_days,
#' #     n_employee_meals_per_person = n_employee_meals_per_person,
#' #     n_client_meals = n_client_meals,
#' #     n_uber_airport_parking = n_uber_airport_parking,
#' #     n_vehicle_rental_van = n_vehicle_rental_van,
#' #     day_price_for_van = day_price_for_van,
#' #     total_price_for_van = total_price_for_van,
#' #     ten_percent_price_for_van = ten_percent_price_for_van,
#' #     n_vehicle_rental_car = n_vehicle_rental_car,
#' #     day_price_for_car = day_price_for_car,
#' #     total_price_for_car = total_price_for_car,
#' #     ten_percent_price_for_car = ten_percent_price_for_car,
#' #     parking_tolls_gas_cost = parking_tolls_gas_cost,
#' #     all_travelers_domestic = all_travelers_domestic,
#' #     all_travelers_international = all_travelers_international,
#' #     n_equipment_bags_per_package_domestic = n_equipment_bags_per_package_domestic,
#' #     n_equipment_bags_per_package_international = n_equipment_bags_per_package_international,
#' #     if_travel_international_1 = if_travel_international_1,
#' #     if_travel_domestic_1 = if_travel_domestic_1,
#' #     n_travel_days_domestic = n_travel_days_domestic,
#' #     n_travel_days_international = n_travel_days_international
#' #     )
#' # }
