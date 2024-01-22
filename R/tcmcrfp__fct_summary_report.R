#' #' summary_report
#' #'
#' #' @description A fct function
#' #'
#' #' @return The return value, if any, from executing the function.
#' #'
#' #' @noRd
#'
#'
#'
#' # Create Project Creation Sentence ----------------------------------------
#'
#' create_project_creation_sentence <- function(
#'     client_name = "[Client Name]",
#'     report_creator_name = "[Report Creator Name]",
#'     report_creation_date = "[Report Creation Date]"
#' ) {
#'
#'   project_creation_sentence <- glue::glue(
#'     "{report_creator_name} created this project for {client_name}
#'     on {report_creation_date}."
#'   )
#'
#'   return(project_creation_sentence)
#' }
#'
#' # Create New Footage Sentence ---------------------------------------------
#'
#' create_new_footage_sentence <- function(requires_new_footage) {
#'
#'   if(requires_new_footage == "No") {
#'
#'     new_footage_sentence <- "No new footage is needed."
#'
#'   } else if(requires_new_footage == "Yes") {
#'
#'     new_footage_sentence <- "New footage is needed for this project."
#'
#'   }
#'
#'   return(new_footage_sentence)
#' }
#'
#' # Check New Footage Sentence ----------------------------------------------
#'
#' check_new_footage_sentence <- function(requires_new_footage) {
#'
#'   if(requires_new_footage == "No") {
#'
#'     cond <- FALSE
#'
#'   } else if(requires_new_footage == "Yes") {
#'
#'     cond <- TRUE
#'
#'   }
#'
#'   return(cond)
#' }
#'
#' # Create Shoot Information Sentence ---------------------------------------
#'
#' create_shoot_information_sentence <- function(
#'     type_of_shoot,
#'     location_type_for_shoot,
#'     estimated_shoot_days_needed
#' ) {
#'
#'   shoot_information_sentence <- glue::glue(
#'     "The shoot will be a {type_of_shoot},
#'     will take place {location_type_for_shoot},
#'     and will shoot for {estimated_shoot_days_needed} days."
#'   )
#'
#'   return(shoot_information_sentence)
#' }
#'
#' # Create Freelance Staff Sentence -----------------------------------------
#'
#' create_freelance_staff_sentence <- function(selected_crew_data_frame) {
#'
#'   freelance_person_travel <- selected_crew_data_frame$is_travel_needed
#'
#'   freelance_person_prep_days <- dplyr::case_when(
#'     selected_crew_data_frame$are_prep_days_needed > 0 ~ "Prep Days Needed",
#'     selected_crew_data_frame$are_prep_days_needed == 0 ~ "Prep Days Not Needed"
#'   )
#'
#'   freelance_position_strings <- glue::glue("{selected_crew_data_frame$freelance_position}({freelance_person_travel} | {freelance_person_prep_days})")
#'
#'   paste_freelance_position_strings <- paste0(freelance_position_strings, collapse = ", ")
#'
#'   freelance_staff_sentence <- glue::glue("The following freelance positions will also attend the shoot: {paste_freelance_position_strings}")
#'
#'   return(freelance_staff_sentence)
#' }
#'
#' # Create Equipment Sentence -----------------------------------------------
#'
#' create_equipment_sentence <- function(needed_equipment, paste_equipment_options) {
#'
#'   paste_equipment_options <-  paste0(equipment_options, collapse = ", ")
#'
#'   equipment_sentence <- glue::glue("{needed_equipment} and the following gear was selected: {paste_equipment_options}")
#'
#'   return(equipment_sentence)
#' }
#'
#' # Create Cast Sentence ----------------------------------------------------
#'
#' create_cast_sentence <- function(
#'     n_adult_leads = 0, n_children_leads = 0, n_adult_speaking_extras = 0,
#'     n_children_speaking_extras = 0, n_adult_non_speaking_extras = 0,
#'     n_children_non_speaking_extras = 0
#' ) {
#'
#'   cast_sentence <- glue::glue(
#'     "For cast members: we will need
#'   {n_adult_leads} adult leads,
#'   {n_children_leads} children leads,
#'   {n_adult_speaking_extras} adult speaking extras,
#'   {n_children_speaking_extras} children speaking extras,
#'   {n_adult_non_speaking_extras} adult non-speaking extras,
#'   {n_children_non_speaking_extras} children non-speaking extras")
#'
#'   return(cast_sentence)
#' }
#'
#' # Create notification of shoot sentence -----------------------------------
#'
#' create_prior_notification_of_shoot_sentence <- function(prior_notification_of_shoot) {
#'
#'   prior_notification_of_shoot_sentence <- case_when(
#'     prior_notification_of_shoot == "Yes" ~ "The creator of this report has notified the production team of this project.",
#'     prior_notification_of_shoot == "No" ~ "The creator of this report has not notified the production team of this project."
#'   )
#'
#'   return(prior_notification_of_shoot_sentence)
#' }
#'
#' # Create Production Sentence ----------------------------------------------
#'
#' create_post_production_sentence <- function(post_production_selection){
#'
#'   paste_post_production_selection <-  paste0(post_production_selection, collapse = ", ")
#'
#'   post_production_sentence <- glue::glue("The following post-production services will also be utilized: {paste_post_production_selection}.")
#'
#'   return(post_production_sentence)
#' }
#'
#' # Create the html for the summary page ------------------------------------
#'
#' get_project_details <- function(
#'     client_name = "Client Name",
#'     report_creator_name = "Report Creator Name",
#'     report_creation_date = "Report Creation Date",
#'     needs_new_footage ="New footage",
#'     type_of_shoot = "Type of Shoot",
#'     location_of_shoot = "Location of Shoot",
#'     estimated_shoot_days_needed = "Shoot Days",
#'     moore_production_staff_to_attend = "Moore Staff",
#'     selected_crew_data_frame = NULL,
#'     needed_equipment = "Equipment Package",
#'     equipment_options = "Gear",
#'     need_hired_leads = "No",
#'     n_adult_leads = 0,
#'     n_children_leads = 0,
#'     need_hired_speaking_extras = "No",
#'     n_adult_speaking_extras = 0,
#'     n_children_speaking_extras = 0,
#'     need_hired_non_speaking_extras = "No",
#'     n_adult_non_speaking_extras = 0,
#'     n_children_non_speaking_extras = 0,
#'     people_need_catering_n = 0,
#'     shooting_location_requirement = "NA",
#'     type_of_shooting_locations = "NA",
#'     need_production_books_printed = "NA",
#'     prior_notification_of_shoot = "NA",
#'     post_production_selection = "NA"
#' ) {
#'
#'   requires_new_footage <- ifelse(
#'     is.null(needs_new_footage),
#'     "Response Not Provided",
#'     ifelse(
#'       needs_new_footage == "New footage",
#'       "Yes",
#'       "No"))
#'
#'   paste_moore_production_staff_to_attend <- paste0(moore_production_staff_to_attend, collapse = ", ")
#'
#'   if(is.null(selected_crew_data_frame)){
#'     selected_crew_data_frame <- data.frame(
#'       freelance_position = c("None", "None"),
#'       is_travel_needed = c("No", "No"),
#'       are_prep_days_needed = c(0, 0)
#'     )
#'   }
#'
#'   paste_type_of_shooting_locations <-  paste0(type_of_shooting_locations, collapse = ", ")
#'
#'   project_creation_sentence <- create_project_creation_sentence(client_name, report_creator_name, report_creation_date)
#'
#'   new_footage_sentence <- create_new_footage_sentence(requires_new_footage)
#'
#'   cond_new_footage_sentence <- check_new_footage_sentence(requires_new_footage)
#'
#'   location_type_for_shoot <- dplyr::case_when(
#'     location_of_shoot == "Tulsa, Ok." ~ "locally in Tulsa",
#'     location_of_shoot == "Outside of Tulsa, OK. (United States)" ~ "domestically",
#'     location_of_shoot == "Outside of U.S. (International)" ~ "internationally"
#'   )
#'
#'   shoot_information_sentence <- create_shoot_information_sentence(type_of_shoot, location_type_for_shoot, estimated_shoot_days_needed)
#'
#'   moore_staff_sentence <- glue::glue("The following Moore staff positions will attend the shoot: {paste_moore_production_staff_to_attend}")
#'
#'   freelance_staff_sentence <- create_freelance_staff_sentence(selected_crew_data_frame)
#'
#'   equipment_sentence <- create_equipment_sentence(needed_equipment, paste_equipment_options)
#'
#'   cast_sentence <- create_cast_sentence(
#'     n_adult_leads, n_children_leads, n_adult_speaking_extras,
#'     n_children_speaking_extras, n_adult_non_speaking_extras,
#'     n_children_non_speaking_extras
#'   )
#'
#'   people_need_catering_sentence <- glue::glue("{people_need_catering_n} additional people will be attending the shoot.")
#'
#'   prior_notification_of_shoot_sentence <- create_prior_notification_of_shoot_sentence(prior_notification_of_shoot)
#'
#'   post_production_sentence <- create_post_production_sentence(post_production_selection)
#'
#'   if(cond_new_footage_sentence == FALSE) {
#'     taglist_to_return <- shiny::tagList({
#'       shiny::fluidRow(
#'         shiny::column(
#'           width = 12,
#'
#'           shiny::p(""),
#'           shiny::h3("Project Summary"),
#'           shiny::p(project_creation_sentence),
#'           shiny::p(new_footage_sentence),
#'           shiny::p(post_production_sentence)
#'
#'         )
#'       )
#'     })
#'   } else if(cond_new_footage_sentence == TRUE) {
#'     taglist_to_return <- shiny::tagList({
#'       shiny::fluidRow(
#'         shiny::column(
#'           width = 12,
#'
#'           shiny::p(""),
#'           shiny::h3("Project Summary"),
#'           shiny::p(project_creation_sentence),
#'           shiny::p(new_footage_sentence),
#'           shiny::p(shoot_information_sentence),
#'           shiny::p(moore_staff_sentence),
#'           shiny::p(freelance_staff_sentence),
#'           shiny::p(equipment_sentence),
#'           shiny::p(cast_sentence),
#'           shiny::p(people_need_catering_sentence),
#'           shiny::p(prior_notification_of_shoot_sentence),
#'           shiny::p(post_production_sentence)
#'
#'         )
#'       )
#'     })
#'   }
#'
#'   return(taglist_to_return)
#' }
#'
#' # tags$li(tags$strong("Do you need to rent a shoot location?: "), glue::glue("{shooting_location_requirement}")),
#' # tags$li(tags$strong("What shoot location is needed?: "), glue::glue("{paste_type_of_shooting_locations}")),
#' # tags$li(tags$strong("Printing the Production Books?: "), glue::glue("{need_production_books_printed}")),
