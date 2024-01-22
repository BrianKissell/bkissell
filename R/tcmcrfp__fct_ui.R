#' #' ui
#' #'
#' #' @description A fct function
#' #'
#' #' @return The return value, if any, from executing the function.
#' #'
#' #' @import bslib
#' #'
#' #' @noRd
#'
#' ################################################################################
#'
#' condSelectInput <- function(cond, header = NULL, inputId, label, choices) {
#'   # Create conditional panel
#'   conditionalPanel(
#'     # If new footage is to be created, create this panel
#'     condition = cond,
#'     style = "display: none;",
#'     # Header
#'     h4(header),
#'     # Allow user to select the type of shoot
#'     selectInput(
#'       inputId = inputId,
#'       label = label,
#'       width = '100%',
#'       choices = choices
#'     )
#'   )
#' }
#'
#' ################# Initial Project Information #####################
#'
#' Create_Project_Information_UI <- function(){
#'   tagList(
#'
#'     h4("Initial Project information"),
#'
#'     fluidRow(
#'       column(
#'         width = 4,
#'
#'         textInput(
#'           inputId = "client_name",
#'           label = "What is the name of the client?",
#'           width = '100%',
#'           value = ""
#'         )
#'       ),
#'
#'       column(
#'         width = 4,
#'         textInput(
#'           inputId = "report_creator_name",
#'           label = "What is your name?",
#'           width = '100%',
#'           value = ""
#'         )
#'       ),
#'
#'       column(
#'         width = 4,
#'         dateInput(
#'           "report_creation_date",
#'           "What is today's date?",
#'           width = '100%'
#'         )
#'       )
#'     ),
#'
#'     conditionalPanel(
#'       condition = "(input.client_name != '' & input.report_creator_name != '')",
#'       style = "display: none;",
#'       # Header
#'       h4("Job Type"),
#'       # Allow users to select whether new footage is needed
#'       selectInput(
#'         inputId = "needs_new_footage",
#'         label = "Will the project include shooting new footage or is this a post production only job? ",
#'         width = '100%',
#'         choices = c("Please Select", "New footage", "Post only")
#'       ),
#'
#'       conditionalPanel(
#'         condition = 'input.needs_new_footage == "Post only"',
#'         style = "display: none;",
#'         fluidPage(actionButton("click_to_continue_initial_2", "Please Click to Continue"))
#'       ),
#'
#'       conditionalPanel(
#'         condition = "(input.needs_new_footage == 'New footage')",
#'         style = "display: none;",
#'         h4("Type of Footage"),
#'         selectInput(
#'           "type_of_shoot",
#'           "Which best describes your type of shoot?",
#'           c("Please Select", "Concept shoot", "First Person Story", "Celebrity Wrap", "Celebrity First person story", "Broll only", "Media day"),
#'           width = '100%',
#'           selected = NULL
#'         ),
#'
#'         conditionalPanel(
#'           condition = "input.type_of_shoot != 'Please Select'",
#'           style = "display: none;",
#'           h4("Location of Shoot"),
#'           selectInput(
#'             "location_of_shoot",
#'             "Where will the shoot take place?",
#'             c("Please Select", "Tulsa, Ok.", "Outside of Tulsa, OK. (United States)", "Outside of U.S. (International)"),
#'             width = '100%'
#'           ),
#'
#'           conditionalPanel(
#'             condition = "input.location_of_shoot != 'Please Select'",
#'             style = "display: none;",
#'
#'             h4("Duration of Shoot"),
#'
#'             shinyWidgets::prettyRadioButtons(
#'               inputId = "estimated_shoot_days_needed",
#'               label = "How many days on set will it take to complete the shoot? (This excludes travel time)",
#'               choices = c(1:14),
#'               selected = character(0),
#'               inline = TRUE,
#'               width = '100%',
#'               outline = FALSE
#'             ),
#'
#'             conditionalPanel(
#'               condition = paste0(paste0("input.estimated_shoot_days_needed == ", 1:14), collapse = " | "),
#'               style = "display: none;",
#'               fluidPage(actionButton("click_to_continue_initial", "Please Click to Continue"))
#'             )
#'           )
#'         )
#'       )
#'     )
#'   )
#' }
#'
#' ################################################################################
#'
#' Create_Moore_Staff_Only_UI <- function() {
#'   additional_moore_staff_other_text_ids <- purrr::map_chr(
#'     seq(1, 5), ~{
#'       paste0("additional_moore_staff_other_text_", .x)
#'     })
#'
#'   number_v <- c("First", "Second", "Third", "Fourth", "Fifth")
#'
#'   numbers_for_label <- number_v[seq(1, 5)]
#'
#'   additional_moore_staff_other_text_label <- purrr::map_chr(
#'     numbers_for_label, ~{
#'       paste0("Please Enter ", .x, " Custom Staff Position")
#'     })
#'
#'   tagList(
#'     h4("Moore Staff"),
#'     fluidRow(
#'       column(
#'         width = 12,
#'
#'         checkboxGroupInput(
#'           "moore_production_staff_to_attend",
#'           label = "Which of the following Moore staff positions will be attending the shoot? (Please select all that apply)",
#'           choices = moore_production_position_options,
#'           selected = NULL,
#'           inline = FALSE,
#'           width = "100%"
#'         ),
#'
#'         conditionalPanel(
#'           condition = 'input.moore_production_staff_to_attend.includes("Others (Select to Provide Custom Positions)")',
#'           style = "display: none;",
#'           selectInput(
#'             "n_additional_moore_staff",
#'             "How many additional Moore Staffers will be attending the shoot?",
#'             1:5,
#'             width = "100%"
#'           ),
#'
#'           conditionalPanel(
#'             condition = 'input.n_additional_moore_staff > 0',
#'             style = "display: none;",
#'             shinyWidgets::textInputIcon(
#'               inputId = "additional_moore_staff_other_text_1",
#'               label = "Please Enter First Custom Staff Position",
#'               icon = icon("plus"),
#'               width = "100%",
#'               size = "sm"
#'             )
#'           ),
#'
#'           conditionalPanel(
#'             condition = 'input.n_additional_moore_staff > 1',
#'             style = "display: none;",
#'             shinyWidgets::textInputIcon(
#'               inputId = "additional_moore_staff_other_text_2",
#'               label = "Please Enter Second Custom Staff Position",
#'               icon = icon("plus"),
#'               width = "100%",
#'               size = "sm"
#'             )
#'           ),
#'
#'           conditionalPanel(
#'             condition = 'input.n_additional_moore_staff > 2',
#'             style = "display: none;",
#'             shinyWidgets::textInputIcon(
#'               inputId = "additional_moore_staff_other_text_3",
#'               label = "Please Enter Third Custom Staff Position",
#'               icon = icon("plus"),
#'               width = "100%",
#'               size = "sm"
#'             )
#'           ),
#'
#'           conditionalPanel(
#'             condition = 'input.n_additional_moore_staff > 3',
#'             style = "display: none;",
#'             shinyWidgets::textInputIcon(
#'               inputId = "additional_moore_staff_other_text_4",
#'               label = "Please Enter Fourth Custom Staff Position",
#'               icon = icon("plus"),
#'               width = "100%",
#'               size = "sm"
#'             )
#'           ),
#'
#'           conditionalPanel(
#'             condition = 'input.n_additional_moore_staff > 4',
#'             style = "display: none;",
#'             shinyWidgets::textInputIcon(
#'               inputId = "additional_moore_staff_other_text_5",
#'               label = "Please Enter Fifth Custom Staff Position",
#'               icon = icon("plus"),
#'               width = "100%",
#'               size = "sm"
#'             )
#'           )
#'         )
#'       )
#'     ),
#'     actionButton("click_to_continue_moore_staff", "Click to Continue")
#'   )
#' }
#'
#'
#' #
#' # moore_production_staff_to_attend <- moore_production_position_options
#' # additional_moore_staff_other_text_1 = ""
#' # additional_moore_staff_other_text_2 = ""
#' # additional_moore_staff_other_text_3 = ""
#' # additional_moore_staff_other_text_4 = ""
#' # additional_moore_staff_other_text_5 = ""
#'
#' create_full_moore_production_staff_to_attend <- function(
#'     moore_production_staff_to_attend = NULL,
#'     n_additional_moore_staff = NULL,
#'     additional_moore_staff_other_text_1 = NULL,
#'     additional_moore_staff_other_text_2 = NULL,
#'     additional_moore_staff_other_text_3 = NULL,
#'     additional_moore_staff_other_text_4 = NULL,
#'     additional_moore_staff_other_text_5 = NULL) {
#'
#'   other_string <- "Others (Select to Provide Custom Positions)"
#'
#'   other_was_clicked <- any(other_string %in% moore_production_staff_to_attend)
#'
#'   moore_production_staff_to_attend_strings <- moore_production_staff_to_attend[moore_production_staff_to_attend != other_string]
#'
#'   additional_moore_staff_others <- c(
#'     additional_moore_staff_other_text_1, additional_moore_staff_other_text_2,
#'     additional_moore_staff_other_text_3, additional_moore_staff_other_text_4,
#'     additional_moore_staff_other_text_5
#'   )
#'
#'   if(is.null(n_additional_moore_staff)) {
#'     n_additional_moore_staff <- 0
#'   }
#'
#'   if(n_additional_moore_staff > 0){
#'     additional_moore_staff_others_combined <- additional_moore_staff_others[seq(1, n_additional_moore_staff)]
#'   } else {
#'     additional_moore_staff_others_combined <- NULL
#'   }
#'
#'   if(other_was_clicked) {
#'     selected_moore_staff <- c(moore_production_staff_to_attend_strings, additional_moore_staff_others_combined)
#'   } else {
#'     selected_moore_staff <- moore_production_staff_to_attend_strings
#'   }
#'
#'   return(selected_moore_staff)
#'
#' }
#'
#' # create_full_moore_production_staff_to_attend(
#' #     moore_production_staff_to_attend = moore_production_position_options,
#' #     n_additional_moore_staff = 3,
#' #     additional_moore_staff_other_text_1 = NULL,
#' #     additional_moore_staff_other_text_2 = NULL,
#' #     additional_moore_staff_other_text_3 = NULL,
#' #     additional_moore_staff_other_text_4 = NULL,
#' #     additional_moore_staff_other_text_5 = NULL)
#'
#'
#'
#'
#' # create_ui_moore_staff_only_other_information <- function(moore_staff_selection) {
#' #
#' #   # if(is.null(moore_staff_selection)) {
#' #   #
#' #   # } else if("Others (Select to Provide Custom Positions)" %in% moore_staff_selection) {
#' #     tagList(
#' #         selectInput(
#' #         "n_additional_moore_staff",
#' #         "How many additional Moore Staffers will be attending the shoot?",
#' #         1:5,
#' #         width = "100%"
#' #         ),
#' #         shinyWidgets::textInputIcon(
#' #           inputId = additional_moore_staff_other_text_1,
#' #           label = "Please Enter First Custom Staff Position",
#' #           icon = icon("plus"),
#' #           width = "100%",
#' #           size = "sm"
#' #         ),
#' #         shinyWidgets::textInputIcon(
#' #           inputId = additional_moore_staff_other_text_2,
#' #           label = "Please Enter Second Custom Staff Position",
#' #           icon = icon("plus"),
#' #           width = "100%",
#' #           size = "sm"
#' #         ),
#' #         shinyWidgets::textInputIcon(
#' #           inputId = additional_moore_staff_other_text_3,
#' #           label = "Please Enter Third Custom Staff Position",
#' #           icon = icon("plus"),
#' #           width = "100%",
#' #           size = "sm"
#' #         ),
#' #         shinyWidgets::textInputIcon(
#' #           inputId = additional_moore_staff_other_text_4,
#' #           label = "Please Enter Fourth Custom Staff Position",
#' #           icon = icon("plus"),
#' #           width = "100%",
#' #           size = "sm"
#' #         ),
#' #         shinyWidgets::textInputIcon(
#' #           inputId = additional_moore_staff_other_text_5,
#' #           label = "Please Enter Fifth Custom Staff Position",
#' #           icon = icon("plus"),
#' #           width = "100%",
#' #           size = "sm"
#' #         )
#' #       )
#' #   # }
#' # }
#'
#' # create_ui_moore_staff_text_options <- function() {
#' #
#' #   # if(is.null(n_additional_moore_staff)) {} else {
#' #   additional_moore_staff_other_text_ids <- purrr::map_chr(
#' #     seq(1, 5), ~{
#' #       paste0("additional_moore_staff_other_text_", .x)
#' #     })
#' #
#' #   number_v <- c("First", "Second", "Third", "Fourth", "Fifth")
#' #
#' #   numbers_for_label <- number_v[seq(1, 5)]
#' #
#' #   additional_moore_staff_other_text_label <- purrr::map_chr(
#' #     numbers_for_label, ~{
#' #       paste0("Please Enter ", .x, " Custom Staff Position")
#' #     })
#' #
#' #
#' #
#' #   purrr::map2(
#' #     additional_moore_staff_other_text_ids,
#' #     additional_moore_staff_other_text_label, ~{
#' #     tagList(
#' #       shinyWidgets::textInputIcon(
#' #         inputId = .x,
#' #         label = .y,
#' #         icon = icon("plus"),
#' #         width = "100%",
#' #         size = "sm"
#' #       )
#' #     )
#' #   })
#' # }
#'
#' # create_ui_moore_staff_text_options <- function(n_additional_moore_staff) {
#' #
#' #   if(is.null(n_additional_moore_staff)) {} else {
#' #     additional_moore_staff_other_text_ids <- purrr::map_chr(
#' #       seq(1, n_additional_moore_staff), ~{
#' #         paste0("additional_moore_staff_other_text_", .x)
#' #       })
#' #
#' #     number_v <- c("First", "Second", "Third", "Fourth", "Fifth")
#' #
#' #     numbers_for_label <- number_v[seq(1, n_additional_moore_staff)]
#' #
#' #     additional_moore_staff_other_text_label <- purrr::map_chr(
#' #       numbers_for_label, ~{
#' #         paste0("Please Enter ", .x, " Custom Staff Position")
#' #       })
#' #
#' #
#' #
#' #
#' #     purrr::map2(
#' #       additional_moore_staff_other_text_ids,
#' #       additional_moore_staff_other_text_label, ~{
#' #         tagList(
#' #           shinyWidgets::updateTextInputIcon(
#' #             inputId = .x,
#' #             label = .y,
#' #             icon = icon("plus"),
#' #             width = "100%",
#' #             size = "sm",
#' #             value = input[[.x]]
#' #           )
#' #         )
#' #       })
#' #   }
#' # }
#'
#'
#'
#'
#'
#' # # Set the functions for the Crew Selection UI
#' # create_ui_moore_production_staff_to_attend <- function() {
#' #   names <- moore_production_position_options
#' #   values <- names
#' #   tagList(
#' #     checkboxGroupInput(
#' #       "moore_production_staff_to_attend",
#' #       label = "Which of the following Moore staff positions will be attending the shoot? (Please select all that apply)",
#' #       choices = moore_production_position_options,
#' #       selected = NULL,
#' #       inline = FALSE,
#' #       width = "100%"
#' #     )
#' #   )
#' # }
#'
#' create_ui_travel_needed_for_crew_member_ui <- function(travel_needed_id, location_of_shoot){
#'   if(!is.null(location_of_shoot)){
#'     if(location_of_shoot == "Tulsa, Ok."){
#'       default_selection <- "Travel is not needed"
#'     } else if(location_of_shoot %in% c("Outside of Tulsa, OK. (United States)", "Outside of U.S. (International)")) {
#'       default_selection <- "Travel is not needed"
#'     } else {
#'       default_selection <- "Please Select"
#'     }
#'     tagList(
#'       selectInput(
#'         inputId = travel_needed_id,
#'         # inputId = "travel_needed_for_crew_member",
#'         label = NULL,
#'         # label = "Will this person need to travel or are they from the local area?",
#'
#'         choices = c("Please Select", "Travel is needed", "Travel is not needed"),
#'         selected = default_selection
#'       )
#'     )
#'   }
#' }
#'
#' create_ui_how_many_prep_days_ui <- function(select_crew_position){
#'   if(!is.null(select_crew_position)){
#'     if(select_crew_position %in% c("Craft Services", "Caterer", "Art Coordinator")){
#'       default_selection <- 1
#'     } else {
#'       default_selection <- 0
#'     }
#'     tagList(
#'       selectInput(
#'         inputId = "how_many_prep_days",
#'         label = "How many prep days?",
#'         choices = c(0:5),
#'         selected = default_selection
#'       )
#'     )
#'   }
#' }
#'
#' create_ui_other_shooting_location_option_ui <- function(type_of_shooting_location){
#'   if(is.null(type_of_shooting_location)){
#'
#'   } else if(type_of_shooting_location %in% 'Other'){
#'     textInput(
#'       "shooting_location_other",
#'       "Please enter the type of location",
#'       value = ""
#'     )
#'   }
#' }
#'
#' add_crew_members <- function(current_selected_crew_table, crew_member, travel_needed_for_crew_member, how_many_prep_days) {
#'   if(crew_member == "Please Select" | travel_needed_for_crew_member == "Please Select" | how_many_prep_days == "Please Select") {
#'     message("Please make sure all options are selected.")
#'   } else {
#'
#'     add_to_df_selected_crew_table <- data.frame(
#'       crew_position = crew_member,
#'       travel_or_local = travel_needed_for_crew_member,
#'       needed_prep_days = how_many_prep_days
#'     )
#'
#'     selected_crew_table_df <- rbind(
#'       current_selected_crew_table,
#'       add_to_df_selected_crew_table
#'     )
#'
#'     selected_crew_table_df
#'   }
#' }
#'
#' remove_crew_members <- function(current_selected_crew_table, crew_member, travel_needed_for_crew_member, how_many_prep_days) {
#'
#'   if(length(current_selected_crew_table) > 0){
#'     matches_position <- stringr::str_detect(current_selected_crew_table$crew_position, crew_member)
#'     matches_location <- stringr::str_detect(current_selected_crew_table$travel_or_local, travel_needed_for_crew_member)
#'     matches_prep_days <- current_selected_crew_table$needed_prep_days == how_many_prep_days
#'     matches <- matches_position & matches_location & matches_prep_days
#'
#'     if(sum(matches) > 0){
#'       index_for_matches <- which(matches)
#'       index_to_remove <- index_for_matches[1]
#'       selected_crew_table_df <- current_selected_crew_table[-index_to_remove,]
#'     } else {
#'       message("This var is not yet in the list")
#'       selected_crew_table_df <- current_selected_crew_table
#'     }
#'
#'     selected_crew_table_df
#'   }
#' }
#'
#' change_column_names_for_output_selected_crew_table <- function(selected_crew_table){
#'   if(length(colnames(selected_crew_table)) == 3) {
#'     colnames(selected_crew_table) <- c("Crew Position", "Travel or Local", "Needed Prep Days")
#'   }
#'   selected_crew_table
#' }
#'
#' create_ui_select_crew_position <- function() {
#'   tagList(
#'     selectInput(
#'       inputId = "select_crew_position",
#'       label = "Please select crew position to hire.",
#'       choices = c("Please Select", crew_positions_to_select_from)
#'     )
#'   )
#' }
#'
#' Create_Crew_Selection_UI <- function(){
#'   tagList(
#'     fluidRow(
#'       column(
#'         width = 12,
#'         h4("Freelance Staff"),
#'
#'         # fluidRow(
#'         #   column(
#'         #     width = 6,
#'         #     actionButton("add", "Click to Add a Freelance Crew Member", class = "btn-primary", width = "100%")
#'         #   ),
#'         #   column(
#'         #     width = 6,
#'         #     conditionalPanel(
#'         #       condition = "input.add == 0",
#'         #       style = "display: none;",
#'         #       actionButton("click_to_continue_crew_no_freelance", "Click if No Freelance Crew Members Will Be Hired", class = "btn-warning", width = "100%")
#'         #     )
#'         #   )
#'         # ),
#'         conditionalPanel(
#'           condition = "input.add > 0",
#'           style = "display: none;",
#'           fluidRow(
#'             column(
#'               width = 3,
#'               p("Please Select Position")
#'             ),
#'             column(
#'               width = 3,
#'               p("Will this person need to travel or are they from the local area?")
#'             ),
#'             column(
#'               width = 3,
#'               p("Are Prep Days Needed?")
#'             ),
#'             column(
#'               width = 3,
#'             )
#'           ),
#'
#'           fluidRow(id = "continue"),
#'           # fluidRow(
#'           #   column(
#'           #     width = 12,
#'           #     h5(""),
#'           #     actionButton("click_to_continue_crew", "Click to Continue")
#'           #   )
#'           # )
#'         ),
#'         fluidRow(
#'           column(
#'             width = 6,
#'             actionButton("add", "Click to Add a Freelance Crew Member", class = "btn-primary", width = "100%")
#'           ),
#'           column(
#'             width = 6,
#'             conditionalPanel(
#'               condition = "input.add == 0",
#'               style = "display: none;",
#'               actionButton("click_to_continue_crew_no_freelance", "Click if No Freelance Crew Members Will Be Hired", class = "btn-warning", width = "100%")
#'             ),
#'             conditionalPanel(
#'               condition = "input.add > 0",
#'               style = "display: none;",
#'               actionButton("click_to_continue_crew", "Click to Continue")
#'             )
#'           )
#'         )
#'       )
#'     )
#'   )
#' }
#'
#'
#' ################################################################################
#' create_selected_equipment_ui <- function(needed_equipment){
#'   default_equipment <- dplyr::filter(pricing_for_equipment_packages, .data[["package_type"]] == needed_equipment)
#'   default_equipment <- dplyr::pull(default_equipment, .data[["component"]])
#'   equipment_choices <- c(default_equipment, equipment_options)
#'   tagList(
#'     checkboxGroupInput(
#'       "equipment_options",
#'       "The gear included in your selected package are listed below. Please make any needed adjustments.",
#'       choices = equipment_choices,
#'       selected = default_equipment,
#'       width = '100%'
#'     )
#'   )
#' }
#'
#'
#' Create_Select_Equipment_UI <- function(){
#'   tagList(
#'
#'     h4("Select Equipment"),
#'
#'     fluidRow(
#'       column(
#'         width = 6,
#'         selectInput(
#'           inputId = "needed_equipment",
#'           label = "What equipment package is needed?",
#'           choices = c(
#'             "Please Select",
#'             "Small Package - Single Cam",
#'             "Medium Package - Multi-Cam",
#'             "Large Package - Multi-Cam",
#'             "No Package"
#'           ),
#'           width = '100%'
#'         ),
#'
#'         conditionalPanel(
#'
#'           condition = "input.needed_equipment != 'Please Select'",
#'           style = "display: none;",
#'
#'           actionButton("click_to_continue_equipment", "Click to Continue")
#'         )
#'
#'       ),
#'
#'       column(
#'         width = 6,
#'
#'         conditionalPanel(
#'
#'           condition = "input.needed_equipment != 'Please Select'",
#'           style = "display: none;",
#'
#'           uiOutput("equipment_options_ui")
#'         )
#'       )
#'     )
#'   )
#' }
#'
#' ################################################################################
#'
#' Create_Select_Talent_UI <- function(){
#'   tagList(
#'
#'     h4("Talent"),
#'
#'     shinyWidgets::prettyRadioButtons(
#'       inputId = "will_any_talent_be_hired",
#'       label = "Will any actors/talent be hired?",
#'       choices = c("No", "Yes"),
#'       selected = character(0),
#'       inline = TRUE,
#'       width = '100%',
#'       outline = FALSE
#'     ),
#'
#'     conditionalPanel(
#'       condition = "input.will_any_talent_be_hired == 'Yes'",
#'       style = "display: none;",
#'
#'       fluidRow(
#'         column(
#'           width = 4,
#'           checkboxInput(
#'             inputId = "need_hired_leads",
#'             label = "Please select if you need to hire any leads",
#'             width = '100%',
#'           ),
#'
#'           conditionalPanel(
#'             condition = "input.need_hired_leads",
#'             style = "display: none;",
#'
#'             selectInput(
#'               inputId = "n_adult_leads",
#'               label = "How many adult leads will you need?",
#'               choices = 0:10
#'             ),
#'             selectInput(
#'               inputId = "n_children_leads",
#'               label = "How many children leads will you need?",
#'               choices = 0:10
#'             )
#'           )
#'         ),
#'
#'         column(
#'           width = 4,
#'           checkboxInput(
#'             inputId = "need_hired_speaking_extras",
#'             label = "Please select if you need to hire any speaking extras",
#'             width = '100%',
#'           ),
#'
#'           conditionalPanel(
#'             condition = "input.need_hired_speaking_extras",
#'             style = "display: none;",
#'
#'             selectInput(
#'               inputId = "n_adult_speaking_extras",
#'               label = "How many adult speaking extras will you need?",
#'               choices = c(0:10, "10+")
#'             ),
#'
#'             selectInput(
#'               inputId = "n_children_speaking_extras",
#'               label = "How many children speaking extras will you need?",
#'               choices = c(0:10, "10+")
#'             )
#'           )
#'         ),
#'
#'         column(
#'           width = 4,
#'           checkboxInput(
#'             inputId = "need_hired_non_speaking_extras",
#'             label = "Please select if you need to hire any non-speaking extras",
#'             width = '100%'
#'           ),
#'
#'           conditionalPanel(
#'             condition = "input.need_hired_non_speaking_extras",
#'             style = "display: none;",
#'
#'             selectInput(
#'               inputId = "n_adult_non_speaking_extras",
#'               label = "How many adult non-speaking extras will you need?",
#'               choices = c(0:10, "10+")
#'             ),
#'
#'             selectInput(
#'               inputId = "n_children_non_speaking_extras",
#'               label = "How many children non-speaking extras will you need?",
#'               choices = c(0:10, "10+")
#'             )
#'           )
#'         )
#'       )
#'     ),
#'
#'     conditionalPanel(
#'       condition = "input.will_any_talent_be_hired == 'Yes' | input.will_any_talent_be_hired == 'No'",
#'       style = "display: none;",
#'       fluidRow(
#'         column(
#'           width = 12,
#'           actionButton("click_to_continue_actors", "Click to Continue")
#'         )
#'       )
#'     )
#'   )
#' }
#'
#' ################################################################################
#'
#' Create_Select_Other_UI <- function(){
#'   tagList(
#'     h4("Catering"),
#'
#'     # radioButtons(
#'     #   inputId = "people_need_catering_n",
#'     #   label = "For catering purposes, how many additional people ((i.e. parents/guardians, client reps, talent managers) will be attending the shoot?",
#'     #   choices = 0:30,
#'     #   selected = character(0),
#'     #   inline = TRUE,
#'     #   width = '100%'
#'     # ),
#'
#'
#'
#'
#'
#'
#'     fluidRow(
#'       column(
#'         width = 12,
#'
#'         shinyWidgets::prettyRadioButtons(
#'           inputId = "additional_people_attending_shoot",
#'           label = "For catering purposes, will additional people (i.e. parents/guardians, client reps, talent managers) be attending the shoot?",
#'           choices = c("No", "Yes"),
#'           selected = character(0),
#'           inline = TRUE,
#'           width = '100%',
#'           outline = FALSE
#'         ),
#'
#'         conditionalPanel(
#'           condition = 'input.additional_people_attending_shoot == "Yes"',
#'           style = "display: none;",
#'           sliderInput(
#'             "people_need_catering_n",
#'             "How many additional people will be attending the shoot?",
#'             min = 1,
#'             max = 30,
#'             value = 1,
#'             step = 1,
#'             width = "100%"
#'           )
#'         )
#'       )
#'     ),
#'
#'
#'
#'     conditionalPanel(
#'       condition = "input.additional_people_attending_shoot == 'No' | input.additional_people_attending_shoot == 'Yes'",
#'       fluidRow(
#'         column(
#'           width = 4,
#'           h4("Shooting Location"),
#'
#'           shinyWidgets::prettyRadioButtons(
#'             inputId = "shooting_location_requirement",
#'             label = "Please select if a shooting location need to be rented?",
#'             choices = c("No", "Yes"),
#'             selected = character(0),
#'             inline = TRUE,
#'             width = '100%',
#'             outline = FALSE
#'           ),
#'
#'           conditionalPanel(
#'             condition = 'input.shooting_location_requirement == "Yes"',
#'             style = "display: none;",
#'             checkboxGroupInput(
#'               inputId = "type_of_shooting_location",
#'               label = "What type of location is needed?",
#'               choices = c(
#'                 "Studio",
#'                 "Airbnb/VRBO",
#'                 "Other"
#'               ),
#'               width = '100%'
#'             ),
#'
#'             conditionalPanel(
#'               condition = "input.type_of_shooting_location.includes('Other')",
#'               style = "display: none;",
#'
#'               textInput(
#'                 "shooting_location_other",
#'                 "Please enter the type of location",
#'                 value = ""
#'               )
#'             )
#'           )
#'         )
#'       )
#'     ),
#'
#'
#'
#'
#'     conditionalPanel(
#'       condition = "input.shooting_location_requirement == 'Yes' | input.shooting_location_requirement == 'No'",
#'       fluidRow(
#'         column(
#'           width = 12,
#'           h4("Production Books Needed?"),
#'
#'           shinyWidgets::prettyRadioButtons(
#'             inputId = "need_production_books_printed",
#'             label = "Please select if production books need to be printed?",
#'             choices = c("No", "Yes"),
#'             selected = character(0),
#'             inline = TRUE,
#'             width = '100%',
#'             outline = FALSE
#'           )
#'         )
#'       )
#'     ),
#'
#'     conditionalPanel(
#'       condition = "input.need_production_books_printed == 'Yes' | input.need_production_books_printed == 'No'",
#'       fluidRow(
#'         column(
#'           width = 12,
#'           actionButton("click_to_continue_other", "Click to Continue")
#'         )
#'       )
#'     )
#'   )
#' }
#'
#' ################################################################################
#'
#' Create_Select_Post_Production_UI <- function(){
#'
#'   tagList(
#'
#'     h4("Post Production"),
#'
#'     checkboxGroupInput(
#'       "post_production_selection",
#'       "Please select all that apply",
#'       post_production_rates[["Post_Production"]],
#'       width = '100%'
#'     ),
#'
#'     actionButton("click_to_continue_post_production", "Click to Continue")
#'   )
#' }
#'
#' ################################################################################
#'
#' Create_Download_Cost_Report_UI <- function(){
#'
#'   tagList(
#'     h4("Download Cost Report"),
#'
#'     downloadButton("download_workbook", "Download Workbook"),
#'
#'     htmlOutput("project_details"),
#'
#'     actionButton("exit_program_button", "You have completed the cost report. Please select to close the program.")
#'
#'   )
#' }
#'
#' ################################################################################
#'
#'
#' add_initial_project_info_tab <- function() {
#'   # Right when the app is initiated, I want the following tab to be inserted into the ai.
#'   insertTab(
#'     # Add the tab to the tabs tabset
#'     inputId = "tabs",
#'     # We do not need to set a target other tabs should not be there yet.
#'     target = NULL,
#'     # Send the user to this tab when it is created.
#'     select = TRUE,
#'     # Set up the tab
#'     tabPanel(
#'       # This is the name of the new (first tab)
#'       "Project Information",
#'       # Add a header
#'       h4("Initial Project information"),
#'       # Allow users to enter the name of the client
#'       textInput(
#'         inputId = "client_name",
#'         label = "Please enter the name of the client",
#'         width = '100%',
#'         value = " "
#'       ),
#'       # Header
#'       h4("New Shoot or Post Only"),
#'       # Allow users to select whether new footage is needed
#'       selectInput(
#'         inputId = "needs_new_footage",
#'         label = "Will the project include shooting new footage or is this a post only job? ",
#'         width = '100%',
#'         choices = c(
#'           "Please provide your response",
#'           "New footage",
#'           "Post only")
#'       )
#'     )
#'   )
#' }
#'
#' ################################################################################
#'
#' create_input_id <- function(add_button, id_prefix){
#'   if(is.null(add_button)|is.null(id_prefix)){
#'     stop("input from create_input_id is null")
#'   } else {
#'     input_id <- paste0(id_prefix, add_button)
#'     return(input_id)
#'   }
#' }
#'
#' add_input_id <- function(input_ids, input_id){
#'   if(is.null(input_id)){
#'     stop("input from create_input_id is null")
#'   } else {
#'     input_ids <- c(input_ids, input_id)
#'     return(input_ids)
#'   }
#' }
#'
#' extracting_values_from_input_int <- function(current_id_strings, input) {
#'   current_id_strings_values <- purrr::map_int(current_id_strings, ~{
#'     tryCatch(as.integer(input[[.x]]), error = function(cnd) integer(0))
#'   })
#'   return(current_id_strings_values)
#' }
#'
#' extracting_values_from_input_chr <- function(current_id_strings, input) {
#'   current_id_strings_values <- purrr::map_chr(current_id_strings, ~{
#'     tryCatch(as.character(input[[.x]]), error = function(cnd) character(0))
#'   })
#'   return(current_id_strings_values)
#' }
#'
#' extracting_values_from_input_dbl <- function(current_id_strings, input) {
#'   current_id_strings_values <- purrr::map_dbl(current_id_strings, ~{
#'     tryCatch(as.numeric(input[[.x]]), error = function(cnd) double(0))
#'   })
#'   return(current_id_strings_values)
#' }
#'
#' update_prep_days_input <- function(prior_df, current_df) {
#'   print(prior_df)
#'   print(current_df)
#'
#'
#'   if(!is.null(prior_df) && !is.null(current_df)) {
#'     nrow_current_df <- nrow(current_df)
#'     nrow_prior_df <- nrow(prior_df)
#'     # && (nrow_current_df == nrow_prior_df)
#'     # Both dfs need to have at least one row
#'     if((nrow_current_df != 0 && nrow_prior_df != 0)){
#'
#'
#'       # Check if anything has been added.
#'       if((nrow_current_df == nrow_prior_df)){
#'
#'         print("Nothing Added")
#'         # If not, check if anything has changed
#'         input_changed_from_prior <- prior_df$freelance_position != current_df$freelance_position
#'
#'         if(any(input_changed_from_prior)){
#'           print("Something has changed")
#'
#'           # Check for the id of what changed
#'           prep_days_input_id_to_potentially_update <- current_df$are_prep_days_needed_id[input_changed_from_prior]
#'
#'           # Check for the position of what changed
#'           position_for_potential_to_update_var <- current_df$freelance_position[input_changed_from_prior]
#'
#'           # If position is one of the following, update it.
#'           if(position_for_potential_to_update_var %in% c("Craft Services", "Caterer", "Art Coordinator")){
#'             updateSelectInput(
#'               inputId = prep_days_input_id_to_potentially_update,
#'               choices = 0:5,
#'               selected = 1
#'             )
#'           } else {
#'             updateSelectInput(
#'               inputId = prep_days_input_id_to_potentially_update,
#'               choices = 0:5,
#'               selected = 0
#'             )
#'           }
#'         }
#'
#'       } else {
#'         print("Something Added")
#'       }
#'
#'     }
#'   }
#' }
#'
#' # create_input_id <- function(add_button, id_prefix){
#' #   if(is.null(add_button)|is.null(id_prefix)){
#' #     stop("input from create_input_id is null")
#' #   } else {
#' #     input_id <- paste0(id_prefix, add_button)
#' #     return(input_id)
#' #   }
#' # }
#' #
#' # add_input_id <- function(input_ids, input_id){
#' #   if(is.null(input_id)){
#' #     stop("input from create_input_id is null")
#' #   } else {
#' #     input_ids <- c(input_ids, input_id)
#' #     return(input_ids)
#' #   }
#' # }
#'
#'
