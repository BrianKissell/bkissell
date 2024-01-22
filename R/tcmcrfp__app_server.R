#' #' The application server-side
#' #'
#' #' @param input,output,session Internal parameters for {shiny}.
#' #'     DO NOT REMOVE.
#' #' @import shiny
#' #' @import openxlsx
#' #' @import writexl
#' #' @import tidyverse
#' #' @import dplyr
#' #' @import bslib
#' #' @noRd
#' app_server <- function(input, output, session) {
#'
#'   # Client Name -------------------------------------------------------------
#'
#'   # Create and update the 'Client Name' data frame, which will be added to the workbook
#'   input_client_name_reactive_df <- reactiveVal(c())
#'
#'   # Create listener for client names
#'   LISTEN_input_client_name_reactive_df <- reactive({list(input$client_name)})
#'
#'   # Create name df
#'   observeEvent(LISTEN_input_client_name_reactive_df(), ignoreInit = FALSE, {
#'     create_input_client_name(input$client_name) %>% input_client_name_reactive_df()
#'   })
#'
#'   # Moore Staff Page --------------------------------------------------------
#'
#'   # When the continue button is pressed on the initial page insert the ui shell
#'   observeEvent(input$click_to_continue_initial, {
#'     insert_cost_report_tab(
#'       "Moore Staff Selection", Create_Moore_Staff_Only_UI(),
#'       input$click_to_continue_initial, session
#'     )
#'   })
#'
#'   # Create listener for moore staff related inputs
#'   LISTEN_moore_staff_inputs <- reactive({
#'     list(
#'       input$moore_production_staff_to_attend, input$n_additional_moore_staff, input$additional_moore_staff_other_text_1, input$additional_moore_staff_other_text_2, input$additional_moore_staff_other_text_3, input$additional_moore_staff_other_text_4, input$additional_moore_staff_other_text_5
#'     )
#'   })
#'
#'   # Create reactive variable for the moore staff vector
#'   moore_production_staff_to_attend_reactive <- reactiveVal(c())
#'
#'   # When listener is triggered
#'   observeEvent(LISTEN_moore_staff_inputs(), {
#'
#'     # Create the vector
#'     full_moore_production_staff_to_attend <- create_full_moore_production_staff_to_attend(
#'       moore_production_staff_to_attend = input$moore_production_staff_to_attend,
#'       n_additional_moore_staff = input$n_additional_moore_staff,
#'       additional_moore_staff_other_text_1 = input$additional_moore_staff_other_text_1,
#'       additional_moore_staff_other_text_2 = input$additional_moore_staff_other_text_2,
#'       additional_moore_staff_other_text_3 = input$additional_moore_staff_other_text_3,
#'       additional_moore_staff_other_text_4 = input$additional_moore_staff_other_text_4,
#'       additional_moore_staff_other_text_5 = input$additional_moore_staff_other_text_5)
#'
#'     # Update the vector
#'     moore_production_staff_to_attend_reactive(full_moore_production_staff_to_attend)
#'   })
#'
#'   # Create and update the 'Moore Staff' data frame, which will be added to the workbook
#'   input_moore_production_staff_reactive_df <- reactiveVal(data.frame())
#'
#'   # Create listener for moore vars
#'   LISTEN_input_moore_production_staff_reactive_df <- reactive({
#'     list(
#'       moore_production_staff_to_attend_reactive(), input$estimated_shoot_days_needed, input$location_of_shoot
#'     )
#'   })
#'
#'   # If listener is triggered
#'   observeEvent(LISTEN_input_moore_production_staff_reactive_df(), ignoreInit = FALSE, {
#'
#'     # Create Production Staff Table for Excel was updated
#'     create_input_moore_production_staff(
#'       moore_production_staff_to_attend_reactive(), input$estimated_shoot_days_needed,
#'       input$location_of_shoot
#'     ) %>% input_moore_production_staff_reactive_df()
#'   })
#'
#'   # Create and update the moore_staff_numbers_list
#'   moore_staff_numbers_list_reactive <- reactiveVal()
#'   LISTEN_moore_staff_numbers_list_reactive <- reactive({list(moore_production_staff_to_attend_reactive(), input$location_of_shoot)})
#'   observeEvent(LISTEN_moore_staff_numbers_list_reactive(), ignoreInit = FALSE, {
#'     create_moore_staff_numbers_list(
#'       moore_production_staff = moore_production_staff_to_attend_reactive(),
#'       location_of_shoot = input$location_of_shoot
#'     ) %>% moore_staff_numbers_list_reactive()
#'   })
#'
#'   # Freelance Staff Code ----------------------------------------------------
#'
#'   # When the continue button located on the staff page is clicked
#'   observeEvent(input$click_to_continue_moore_staff, {
#'     # Add the Freelance Tab
#'     insert_cost_report_tab(
#'       "Crew Selection", Create_Crew_Selection_UI(),
#'       input$click_to_continue_moore_staff, session
#'     )
#'   })
#'
#'   # Create reactive that will contain a vector for each variable for freelance
#'   select_freelance_ids_reactive <- reactiveVal(c())
#'   select_travel_needed_ids_reactive <- reactiveVal(c())
#'   select_prep_days_needed_ids_reactive <- reactiveVal(c())
#'   rmv_ids_reactive <- reactiveVal(c())
#'
#'   # When the add button is selected
#'   observeEvent(input$add, ignoreInit = TRUE, {
#'     # create a numeric label that contains the number of times the button has been clicked
#'     input_add_number <- input$add
#'
#'     # Create freelance id
#'     select_freelance_id <- create_input_id(input_add_number, "select_freelance_")
#'
#'     # Add new freelance id to the vector
#'     add_input_id(select_freelance_ids_reactive(), select_freelance_id) %>%
#'       select_freelance_ids_reactive()
#'
#'     # Create the travel neeeded button id
#'     select_travel_needed_id <- create_input_id(input_add_number, "select_travel_needed_")
#'
#'     # Add the id to the vector
#'     add_input_id(select_travel_needed_ids_reactive(), select_travel_needed_id) %>% select_travel_needed_ids_reactive()
#'
#'     # Create the prep days needed id
#'     select_prep_days_needed_id <- create_input_id(input_add_number, "select_prep_days_needed_")
#'
#'     # Add the id to the vector
#'     add_input_id(select_prep_days_needed_ids_reactive(), select_prep_days_needed_id) %>%
#'       select_prep_days_needed_ids_reactive()
#'
#'     # Create the remove button id
#'     rmv_id <- create_input_id(input_add_number, "freelance_row_rmv_")
#'
#'     # Add the id to the vector
#'     add_input_id(rmv_ids_reactive(), rmv_id) %>%
#'       rmv_ids_reactive()
#'
#'     # Put the ui elements together
#'     ui_object <- tagList(
#'
#'       fluidRow(
#'         # Create and id
#'         id = rmv_id,
#'
#'         # Create a title
#'         title = rmv_id,
#'
#'         # Add the position selector
#'         column(
#'           width = 3,
#'           selectInput(
#'             select_freelance_id,
#'             NULL,
#'             c("Please Select", crew_positions_to_select_from)
#'           )
#'         ),
#'
#'         # Add the travel needed selector
#'         column(
#'           width = 3,
#'           create_ui_travel_needed_for_crew_member_ui(
#'             select_travel_needed_id, input$location_of_shoot
#'           )
#'         ),
#'
#'         # Add the travel needed selector
#'         column(
#'           width = 3,
#'           selectInput(
#'             inputId = select_prep_days_needed_id,
#'             label = NULL,
#'             choices = c(0:5)
#'           )
#'         ),
#'
#'         # Add the remove button
#'         column(
#'           width = 3,
#'           actionButton(rmv_id, "Delete Row")
#'         )
#'       )
#'     )
#'
#'     # Insert the UI element into the ui
#'     insertUI(
#'       selector = "#continue",
#'       where = "beforeBegin",
#'       ui = ui_object
#'     )
#'   })
#'
#'   LISTEN_ui_removal_reactive <- reactive({
#'     # Create the list of inputs that will trigger removal
#'     list(
#'       "input$freelance_row_rmv_1" = input$freelance_row_rmv_1, "input$freelance_row_rmv_2" = input$freelance_row_rmv_2, "input$freelance_row_rmv_3" = input$freelance_row_rmv_3, "input$freelance_row_rmv_4" = input$freelance_row_rmv_4, "input$freelance_row_rmv_5" = input$freelance_row_rmv_5, "input$freelance_row_rmv_6" = input$freelance_row_rmv_6, "input$freelance_row_rmv_7" = input$freelance_row_rmv_7, "input$freelance_row_rmv_8" = input$freelance_row_rmv_8, "input$freelance_row_rmv_9" = input$freelance_row_rmv_9, "input$freelance_row_rmv_10" = input$freelance_row_rmv_10,
#'       "input$freelance_row_rmv_11" = input$freelance_row_rmv_11, "input$freelance_row_rmv_12" = input$freelance_row_rmv_12, "input$freelance_row_rmv_13" = input$freelance_row_rmv_13, "input$freelance_row_rmv_14" = input$freelance_row_rmv_14, "input$freelance_row_rmv_15" = input$freelance_row_rmv_15, "input$freelance_row_rmv_16" = input$freelance_row_rmv_16, "input$freelance_row_rmv_17" = input$freelance_row_rmv_17, "input$freelance_row_rmv_18" = input$freelance_row_rmv_18, "input$freelance_row_rmv_19" = input$freelance_row_rmv_19, "input$freelance_row_rmv_20" = input$freelance_row_rmv_20,
#'       "input$freelance_row_rmv_21" = input$freelance_row_rmv_21, "input$freelance_row_rmv_22" = input$freelance_row_rmv_22, "input$freelance_row_rmv_23" = input$freelance_row_rmv_23, "input$freelance_row_rmv_24" = input$freelance_row_rmv_24, "input$freelance_row_rmv_25" = input$freelance_row_rmv_25, "input$freelance_row_rmv_26" = input$freelance_row_rmv_26, "input$freelance_row_rmv_27" = input$freelance_row_rmv_27, "input$freelance_row_rmv_28" = input$freelance_row_rmv_28, "input$freelance_row_rmv_29" = input$freelance_row_rmv_29, "input$freelance_row_rmv_30" = input$freelance_row_rmv_30,
#'       "input$freelance_row_rmv_31" = input$freelance_row_rmv_31, "input$freelance_row_rmv_32" = input$freelance_row_rmv_32, "input$freelance_row_rmv_33" = input$freelance_row_rmv_33, "input$freelance_row_rmv_34" = input$freelance_row_rmv_34, "input$freelance_row_rmv_35" = input$freelance_row_rmv_35, "input$freelance_row_rmv_36" = input$freelance_row_rmv_36,  "input$freelance_row_rmv_37" = input$freelance_row_rmv_37, "input$freelance_row_rmv_38" = input$freelance_row_rmv_38, "input$freelance_row_rmv_39" = input$freelance_row_rmv_39, "input$freelance_row_rmv_40" = input$freelance_row_rmv_40
#'     )
#'   })
#'
#'   observeEvent(LISTEN_ui_removal_reactive(), ignoreInit = TRUE, ignoreNULL = TRUE, {
#'     # Pull the vectors for the freelance data
#'     existing_rmv_vars <- rmv_ids_reactive()
#'     existing_select_freelance_ids <- select_freelance_ids_reactive()
#'     existing_select_travel_needed_ids <- select_travel_needed_ids_reactive()
#'     existing_select_prep_days_needed_ids <- select_prep_days_needed_ids_reactive()
#'
#'     # Obtain the values from the inputs
#'     existing_rmv_values <- extracting_values_from_input_int(existing_rmv_vars, input)
#'     existing_select_freelance_values <- extracting_values_from_input_chr(existing_select_freelance_ids, input)
#'     existing_select_travel_needed_values <- extracting_values_from_input_chr(existing_select_travel_needed_ids, input)
#'     existing_select_prep_days_needed_values <- extracting_values_from_input_dbl(existing_select_prep_days_needed_ids, input)
#'
#'     # Which index needs to be removed
#'     index_to_remove <- which(existing_rmv_values > 0)
#'
#'     # Only if there is an index that needs to be removed
#'     if(!identical(index_to_remove, integer(0))) {
#'       # Select the vars to remove
#'       rmv_ui_to_remove <- existing_rmv_vars[index_to_remove]
#'       freelance_ui_to_remove <- existing_select_freelance_ids[index_to_remove]
#'       travel_ui_to_remove <- existing_select_travel_needed_ids[index_to_remove]
#'       prep_days_ui_to_remove <- existing_select_prep_days_needed_ids[index_to_remove]
#'
#'       # Remove the ui
#'       removeUI(paste0("#", rmv_ui_to_remove))
#'
#'       # Select only the index to keep
#'       existing_rmv_vars <- existing_rmv_vars[-index_to_remove]
#'       existing_select_freelance_ids <- existing_select_freelance_ids[-index_to_remove]
#'       existing_select_travel_needed_ids <- existing_select_travel_needed_ids[-index_to_remove]
#'       existing_select_prep_days_needed_ids <- existing_select_prep_days_needed_ids[-index_to_remove]
#'
#'       # Update these to the vectors
#'       rmv_ids_reactive(existing_rmv_vars)
#'       select_freelance_ids_reactive(existing_select_freelance_ids)
#'       select_travel_needed_ids_reactive(existing_select_travel_needed_ids)
#'       select_prep_days_needed_ids_reactive(existing_select_prep_days_needed_ids)
#'     }
#'   })
#'
#'   LISTEN_freelance_inputs <- reactive({
#'     list(
#'       # Set up inputs for freelance ids
#'       input$select_freelance_1, input$select_freelance_2, input$select_freelance_3, input$select_freelance_4, input$select_freelance_5, input$select_freelance_6, input$select_freelance_7, input$select_freelance_8, input$select_freelance_9, input$select_freelance_10, input$select_freelance_11, input$select_freelance_12, input$select_freelance_13, input$select_freelance_14, input$select_freelance_15, input$select_freelance_16, input$select_freelance_17, input$select_freelance_18, input$select_freelance_19, input$select_freelance_20, input$select_freelance_21, input$select_freelance_22, input$select_freelance_23, input$select_freelance_24, input$select_freelance_25, input$select_freelance_26, input$select_freelance_27, input$select_freelance_28, input$select_freelance_29, input$select_freelance_30, input$select_freelance_31, input$select_freelance_32, input$select_freelance_33, input$select_freelance_34, input$select_freelance_35, input$select_freelance_36, input$select_freelance_37, input$select_freelance_38, input$select_freelance_39, input$select_freelance_40,
#'       # Set up inputs for travel needs ids
#'       input$select_travel_needed_1, input$select_travel_needed_2, input$select_travel_needed_3, input$select_travel_needed_4, input$select_travel_needed_5, input$select_travel_needed_6, input$select_travel_needed_7, input$select_travel_needed_8, input$select_travel_needed_9, input$select_travel_needed_10, input$select_travel_needed_11, input$select_travel_needed_12, input$select_travel_needed_13, input$select_travel_needed_14, input$select_travel_needed_15, input$select_travel_needed_16, input$select_travel_needed_17, input$select_travel_needed_18, input$select_travel_needed_19, input$select_travel_needed_20, input$select_travel_needed_21, input$select_travel_needed_22, input$select_travel_needed_23, input$select_travel_needed_24, input$select_travel_needed_25, input$select_travel_needed_26, input$select_travel_needed_27, input$select_travel_needed_28, input$select_travel_needed_29, input$select_travel_needed_30, input$select_travel_needed_31, input$select_travel_needed_32, input$select_travel_needed_33, input$select_travel_needed_34, input$select_travel_needed_35, input$select_travel_needed_36, input$select_travel_needed_37, input$select_travel_needed_38, input$select_travel_needed_39, input$select_travel_needed_40,
#'       # Set up inputs for prep days ids
#'       input$select_prep_days_needed_1, input$select_prep_days_needed_2, input$select_prep_days_needed_3, input$select_prep_days_needed_4, input$select_prep_days_needed_5, input$select_prep_days_needed_6, input$select_prep_days_needed_7, input$select_prep_days_needed_8, input$select_prep_days_needed_9, input$select_prep_days_needed_10, input$select_prep_days_needed_11, input$select_prep_days_needed_12, input$select_prep_days_needed_13, input$select_prep_days_needed_14, input$select_prep_days_needed_15, input$select_prep_days_needed_16, input$select_prep_days_needed_17, input$select_prep_days_needed_18, input$select_prep_days_needed_19, input$select_prep_days_needed_20, input$select_prep_days_needed_21, input$select_prep_days_needed_22, input$select_prep_days_needed_23, input$select_prep_days_needed_24, input$select_prep_days_needed_25, input$select_prep_days_needed_26, input$select_prep_days_needed_27, input$select_prep_days_needed_28, input$select_prep_days_needed_29, input$select_prep_days_needed_30, input$select_prep_days_needed_31, input$select_prep_days_needed_32, input$select_prep_days_needed_33, input$select_prep_days_needed_34, input$select_prep_days_needed_35, input$select_prep_days_needed_36, input$select_prep_days_needed_37, input$select_prep_days_needed_38, input$select_prep_days_needed_39, input$select_prep_days_needed_40
#'     )
#'   })
#'
#'   # Create reactives to test the old and new dfs
#'   selected_crew_data_frame_current_reactive <- reactiveVal(data.frame())
#'   reactive_crew_before_change <- reactiveVal(data.frame())
#'
#'   # When one of these inputs are selected
#'   observeEvent(LISTEN_freelance_inputs(), ignoreInit = FALSE, priority = 1, {
#'
#'     # Pull the vectors for the freelance data
#'     existing_rmv_vars <- rmv_ids_reactive()
#'     existing_select_freelance_ids <- select_freelance_ids_reactive()
#'     existing_select_travel_needed_ids <- select_travel_needed_ids_reactive()
#'     existing_select_prep_days_needed_ids <- select_prep_days_needed_ids_reactive()
#'
#'     # Obtain the values from the inputs
#'     existing_rmv_values <- extracting_values_from_input_int(existing_rmv_vars, input)
#'     existing_select_freelance_values <- extracting_values_from_input_chr(existing_select_freelance_ids, input)
#'     existing_select_travel_needed_values <- extracting_values_from_input_chr(existing_select_travel_needed_ids, input)
#'     existing_select_prep_days_needed_values <- extracting_values_from_input_dbl(existing_select_prep_days_needed_ids, input)
#'
#'     # set the df before changes
#'     reactive_crew_before_change(selected_crew_data_frame_current_reactive())
#'
#'     # Put the df together
#'     df_for_crew_table_freelance <- data.frame(
#'       freelance_position = existing_select_freelance_values,
#'       is_travel_needed = existing_select_travel_needed_values,
#'       are_prep_days_needed = existing_select_prep_days_needed_values,
#'       freelance_position_id = existing_select_freelance_ids,
#'       is_travel_needed_id = existing_select_travel_needed_ids,
#'       are_prep_days_needed_id = existing_select_prep_days_needed_ids
#'     )
#'
#'     # Update the df
#'     selected_crew_data_frame_current_reactive(df_for_crew_table_freelance)
#'   })
#'
#'   # When the freelance df is updated,
#'   observeEvent(selected_crew_data_frame_current_reactive(), {
#'     # Obtain the freelance dfs
#'     prior_df <- reactive_crew_before_change()
#'     current_df <- selected_crew_data_frame_current_reactive()
#'
#'     # Update the df
#'     update_prep_days_input(prior_df, current_df)
#'   })
#'
#'   # Create and update the freelance_staff_numbers_list
#'   freelance_staff_numbers_list_reactive <- reactiveVal()
#'   LISTEN_freelance_staff_numbers_list_reactive <- reactive({
#'     list(selected_crew_data_frame_current_reactive(), input$click_to_continue_crew)
#'   })
#'   observeEvent(LISTEN_freelance_staff_numbers_list_reactive(), ignoreInit = FALSE, {
#'     create_freelance_staff_numbers_list(
#'       freelance_staff = selected_crew_data_frame_current_reactive()
#'     ) %>% freelance_staff_numbers_list_reactive()
#'   })
#'
#'   # Create and update the freelance crew table
#'   input_production_freelance_reactive_df <- reactiveVal(data.frame())
#'   LISTEN_input_production_freelance_reactive_df <- reactive({
#'     list(
#'       selected_crew_data_frame_current_reactive(), input$estimated_shoot_days_needed,
#'       input$location_of_shoot
#'     )
#'   })
#'   # When the add crew member button is pushed, add that member to the reactive crew table
#'   observeEvent(LISTEN_input_production_freelance_reactive_df(), ignoreInit = FALSE, {
#'     create_input_production_freelance(
#'       selected_crew_data_frame_current_reactive(), input$estimated_shoot_days_needed,
#'       input$location_of_shoot
#'     ) %>% input_production_freelance_reactive_df()
#'   })
#'
#'   # Equipment Page Code ----------------------------------------------------
#'
#'   # Create reactive variable
#'   validate_freelance_page <- reactiveVal(0)
#'   counter_freelance_button <- reactiveVal(0)
#'   counter_no_freelance_button <- reactiveVal(0)
#'
#'   # Measure when continue buttons are selected
#'   observeEvent(input$click_to_continue_crew, {
#'     (counter_freelance_button() + 1) %>% counter_freelance_button()
#'   })
#'
#'   # Measure when continue buttons are selected
#'   observeEvent(input$click_to_continue_crew_no_freelance, {
#'     (counter_no_freelance_button() + 1) %>% counter_no_freelance_button()
#'   })
#'
#'   # Listen for when these buttons are clicked
#'   LISTEN_add_freelance_UI <- reactive({
#'     list(counter_freelance_button(), counter_no_freelance_button())
#'   })
#'
#'   # When listener is triggered
#'   observeEvent(LISTEN_add_freelance_UI(), ignoreInit = TRUE,{
#'
#'     # Grab the freelance table
#'     crew_table_freelance <- selected_crew_data_frame_current_reactive()
#'
#'     # Create freelance counter
#'     if(input$click_to_continue_crew_no_freelance > 0){
#'       (validate_freelance_page() + 1) %>% validate_freelance_page()
#'     }
#'
#'     # Account for issues in counter
#'     if(input$click_to_continue_crew > 0 ){
#'       if(any(crew_table_freelance$freelance_position %in% "Please Select")){
#'         print("You must select the position")
#'
#'         validate(
#'           need(any(crew_table_freelance$freelance_position %in% "Please Select"), label = "You must select the position")
#'         )
#'
#'       } else {
#'         (validate_freelance_page() + 1) %>% validate_freelance_page()
#'       }
#'     }
#'
#'     # Create the Select equipment package
#'     if(validate_freelance_page() > 0){
#'       insert_cost_report_tab(
#'         "Select Equipment", Create_Select_Equipment_UI(),
#'         validate_freelance_page(), session
#'       )
#'     }
#'   })
#'
#'   # Create the equipment ui
#'   output$equipment_options_ui <- renderUI({
#'     create_selected_equipment_ui(input$needed_equipment)
#'   })
#'
#'   # Create and update the equipment_df table
#'   input_equipment_reactive_df <- reactiveVal(data.frame())
#'   LISTEN_input_equipment_reactive_df <- reactive({list(input$equipment_options, input$estimated_shoot_days_needed)})
#'   observeEvent(LISTEN_input_equipment_reactive_df(), ignoreInit = FALSE, {
#'     create_input_equipment(
#'       input_equipment_options = input$equipment_options,
#'       estimated_shoot_days_needed = input$estimated_shoot_days_needed
#'     ) %>% input_equipment_reactive_df()
#'   })
#'
#'   # Talent Page -------------------------------------------------------------
#'
#'   # Insert talent code
#'   observeEvent(input$click_to_continue_equipment, {
#'     insert_cost_report_tab(
#'       "Talent", Create_Select_Talent_UI(),
#'       input$click_to_continue_equipment, session
#'     )
#'   })
#'
#'   # Create and update the talent_numbers_list
#'   talent_numbers_list_reactive <- reactiveVal()
#'   LISTEN_talent_numbers_list_reactive <- reactive({
#'     list(
#'       input$n_adult_leads, input$n_children_leads,
#'       input$n_adult_speaking_extras, input$n_children_speaking_extras,
#'       input$n_adult_non_speaking_extras, input$n_children_non_speaking_extras
#'     )
#'   })
#'   observeEvent(LISTEN_talent_numbers_list_reactive(), ignoreInit = FALSE, {
#'     create_talent_numbers_list(
#'       n_adult_leads = input$n_adult_leads,
#'       n_children_leads = input$n_children_leads,
#'       n_adult_speaking_extras = input$n_adult_speaking_extras,
#'       n_children_speaking_extras = input$n_children_speaking_extras,
#'       n_adult_non_speaking_extras = input$n_adult_non_speaking_extras,
#'       n_children_non_speaking_extras = input$n_children_non_speaking_extras
#'     ) %>% talent_numbers_list_reactive()
#'   })
#'
#'   # Create and update the input_cast_df_reactive
#'   input_cast_df_reactive <- reactiveVal(data.frame())
#'   LISTEN_input_cast_df_reactive <- reactive({
#'     list(
#'       input$need_hired_leads, input$need_hired_speaking_extras,
#'       input$need_hired_non_speaking_extras, input$n_adult_leads,
#'       input$n_children_leads, input$n_adult_speaking_extras,
#'       input$n_children_speaking_extras, input$n_adult_non_speaking_extras,
#'       input$n_children_non_speaking_extras, input$estimated_shoot_days_needed
#'     )
#'   })
#'   observeEvent(LISTEN_input_cast_df_reactive(), {
#'     create_input_cast(
#'       need_hired_leads = input$need_hired_leads,
#'       need_hired_speaking_extras = input$need_hired_speaking_extras,
#'       need_hired_non_speaking_extras = input$need_hired_non_speaking_extras,
#'       n_adult_leads = input$n_adult_leads,
#'       n_children_leads = input$n_children_leads,
#'       n_adult_speaking_extras = input$n_adult_speaking_extras,
#'       n_children_speaking_extras = input$n_children_speaking_extras,
#'       n_adult_non_speaking_extras = input$n_adult_non_speaking_extras,
#'       n_children_non_speaking_extras = input$n_children_non_speaking_extras,
#'       estimated_shoot_days_needed = input$estimated_shoot_days_needed
#'     ) %>% input_cast_df_reactive()
#'   })
#'
#'   # Other Page --------------------------------------------------------------
#'
#'   # Create reactive info
#'   validate_talent_page <- reactiveVal(0)
#'
#'   # Insert talent tab
#'   observeEvent(input$click_to_continue_actors, {
#'     if(input$will_any_talent_be_hired == "No") {
#'       (validate_talent_page() + 1) %>% validate_talent_page()
#'       insert_cost_report_tab(
#'         "Other", Create_Select_Other_UI(),
#'         input$click_to_continue_actors, session
#'       )
#'     } else if(input$will_any_talent_be_hired == "Yes") {
#'
#'       # Create custom vars
#'       do_not_need_hired_leads <- input$need_hired_leads == FALSE
#'       no_adult_leads <- as.character(input$n_adult_leads) == "0"
#'       no_children_leads <- as.character(input$n_children_leads) == "0"
#'       do_not_need_hired_speaking_extras <- input$need_hired_speaking_extras == FALSE
#'       no_adult_speaking_extras <- as.character(input$n_adult_speaking_extras) == "0"
#'       no_children_speaking_extras <- as.character(input$n_children_speaking_extras) == "0"
#'       do_not_need_hired_non_speaking_extras <- input$need_hired_non_speaking_extras == FALSE
#'       no_adult_non_speaking_extras <- as.character(input$n_adult_non_speaking_extras) == "0"
#'       no_children_non_speaking_extras <- as.character(input$n_children_non_speaking_extras) == "0"
#'
#'       # Conditions
#'       no_actors_selected <- do_not_need_hired_leads & do_not_need_hired_speaking_extras & do_not_need_hired_non_speaking_extras
#'       lead_needed_but_zero <- input$need_hired_leads & no_adult_leads & no_children_leads
#'       hired_speaking_extras_needed_but_zero <- input$need_hired_speaking_extras & no_adult_speaking_extras & no_children_speaking_extras
#'       hired_non_speaking_extras_needed_but_zero <- input$need_hired_non_speaking_extras & no_adult_non_speaking_extras & no_children_non_speaking_extras
#'
#'       # Depending on the conditions, add tabs
#'       if(no_actors_selected | lead_needed_but_zero | hired_speaking_extras_needed_but_zero | hired_non_speaking_extras_needed_but_zero) {
#'         message("Selections are inconsistent")
#'       } else {
#'         (validate_talent_page() + 1) %>% validate_talent_page()
#'         insert_cost_report_tab("Other", Create_Select_Other_UI(), validate_talent_page(), session)
#'       }
#'     }
#'   })
#'
#'   # Post Production Page ----------------------------------------------------
#'
#'   observeEvent(input$click_to_continue_other, {
#'     if(input$click_to_continue_initial_2 == 0) {
#'       insert_cost_report_tab(
#'         "Post Production", Create_Select_Post_Production_UI(),
#'         input$click_to_continue_other, session
#'       )
#'     } else if(input$click_to_continue_initial_2 > 0) {
#'       insert_cost_report_tab(
#'         "Post Production", Create_Select_Post_Production_UI(),
#'         input$click_to_continue_initial_2, session
#'       )
#'     }
#'   })
#'
#'   observeEvent(input$click_to_continue_initial_2, {
#'     if(input$click_to_continue_initial == 0) {
#'       insert_cost_report_tab(
#'         "Post Production",
#'         Create_Select_Post_Production_UI(),
#'         input$click_to_continue_initial_2, session
#'       )
#'     } else if(input$click_to_continue_initial > 0) {
#'       insert_cost_report_tab(
#'         "Post Production", Create_Select_Post_Production_UI(),
#'         input$click_to_continue_other, session
#'       )
#'     }
#'   })
#'
#'
#'   # Download Cost Report Page -----------------------------------------------
#'
#'   observeEvent(input$click_to_continue_post_production, {
#'     insert_cost_report_tab(
#'       "Download Cost Report", Create_Download_Cost_Report_UI(),
#'       input$click_to_continue_post_production, session
#'     )
#'   })
#'
#'   output$project_details <- renderUI({
#'     get_project_details(
#'       client_name = input$client_name,
#'       report_creator_name = input$report_creator_name,
#'       report_creation_date = input$report_creation_date,
#'       needs_new_footage = input$needs_new_footage,
#'       type_of_shoot = input$type_of_shoot,
#'       location_of_shoot = input$location_of_shoot,
#'       estimated_shoot_days_needed = input$estimated_shoot_days_needed,
#'       moore_production_staff_to_attend = moore_production_staff_to_attend_reactive(),
#'       selected_crew_data_frame = selected_crew_data_frame_current_reactive(),
#'       needed_equipment = input$needed_equipment,
#'       equipment_options = input$equipment_options,
#'       need_hired_leads = input$need_hired_leads,
#'       n_adult_leads = input$n_adult_leads,
#'       n_children_leads = input$n_children_leads,
#'       need_hired_speaking_extras = input$need_hired_speaking_extras,
#'       n_adult_speaking_extras = input$n_adult_speaking_extras,
#'       n_children_speaking_extras = input$n_children_speaking_extras,
#'       need_hired_non_speaking_extras = input$need_hired_non_speaking_extras,
#'       n_adult_non_speaking_extras = input$n_adult_non_speaking_extras,
#'       n_children_non_speaking_extras = input$n_children_non_speaking_extras,
#'       people_need_catering_n = input$people_need_catering_n,
#'       shooting_location_requirement = input$shooting_location_requirement,
#'       type_of_shooting_locations = input$type_of_shooting_location,
#'       need_production_books_printed = input$need_production_books_printed,
#'       prior_notification_of_shoot = input$prior_notification_of_shoot,
#'       post_production_selection = input$post_production_selection
#'     )
#'   })
#'
#'   # Calculations -----------------------------------------------------------------
#'
#'   # Create and update the overall_people_list
#'   overall_people_list_reactive <- reactiveVal()
#'   LISTEN_overall_people_list_reactive <- reactive({
#'     list(
#'       moore_staff_numbers_list_reactive(),
#'       freelance_staff_numbers_list_reactive(),
#'       talent_numbers_list_reactive(), input$people_need_catering_n
#'     )
#'   })
#'   observeEvent(LISTEN_overall_people_list_reactive(), ignoreInit = FALSE, {
#'     create_overall_people_list(
#'       moore_staff_numbers_list = moore_staff_numbers_list_reactive(),
#'       freelance_staff_numbers_list = freelance_staff_numbers_list_reactive(),
#'       talent_numbers_list = talent_numbers_list_reactive(),
#'       people_need_catering_n = input$people_need_catering_n
#'     ) %>% overall_people_list_reactive()
#'   })
#'
#'   # Create and update the travel_information_list
#'   travel_information_list_reactive <- reactiveVal()
#'   LISTEN_travel_information_list_reactive <- reactive({
#'     list(
#'       input$location_of_shoot, input$needed_equipment,
#'       input$estimated_shoot_days_needed, overall_people_list_reactive()
#'     )
#'   })
#'   observeEvent(LISTEN_travel_information_list_reactive(), ignoreInit = FALSE, {
#'     create_travel_information_list(
#'       location_of_shoot = input$location_of_shoot,
#'       needed_equipment = input$needed_equipment,
#'       estimated_shoot_days_needed = input$estimated_shoot_days_needed,
#'       overall_people_list = overall_people_list_reactive()
#'     ) %>% travel_information_list_reactive()
#'   })
#'
#'
#'   # Travel Lodge ------------------------------------------------------------
#'
#'   # Create and update the travel_lodge_food_reactive
#'   input_travel_lodge_food_reactive_df <- reactiveVal(data.frame())
#'   LISTEN_input_travel_lodge_food_reactive_df <- reactive({
#'     list(
#'       overall_people_list_reactive(), travel_information_list_reactive(),
#'       freelance_staff_numbers_list_reactive(), moore_staff_numbers_list_reactive()
#'     )
#'   })
#'   observeEvent(LISTEN_input_travel_lodge_food_reactive_df(), {
#'     create_input_travel_lodge_food(
#'       overall_people_list = overall_people_list_reactive(),
#'       travel_information_list = travel_information_list_reactive(),
#'       freelance_staff_numbers_list = freelance_staff_numbers_list_reactive(),
#'       moore_staff_numbers_list = moore_staff_numbers_list_reactive()
#'     ) %>% input_travel_lodge_food_reactive_df()
#'   }, ignoreInit = FALSE)
#'
#'   # Create and update the input_location_rental_reactive
#'   input_location_rental_df_reactive <- reactiveVal(data.frame())
#'   LISTEN_input_location_rental_df_reactive <- reactive({
#'     list(
#'       input$shooting_location_requirement, input$type_of_shooting_location
#'     )
#'   })
#'   observeEvent(LISTEN_input_location_rental_df_reactive(), {
#'     create_input_location_rental(
#'       type_of_shooting_location = input$type_of_shooting_location,
#'       estimated_shoot_days_needed = input$estimated_shoot_days_needed
#'     ) %>% input_location_rental_df_reactive()
#'   })
#'
#'   # Post Production ------------------------------------------------------------
#'
#'   # Create and update the input_post_production
#'   input_post_production_df_reactive <- reactiveVal(data.frame())
#'   LISTEN_input_post_production_df_reactive <- reactive({
#'     list(input$post_production_selection)
#'   })
#'   observeEvent(LISTEN_input_post_production_df_reactive(), {
#'     create_input_post_production(
#'       input$post_production_selection
#'     ) %>% input_post_production_df_reactive()
#'   })
#'
#'
#'
#'   # Create Workbook --------------------------------------------------------
#'
#'   # Create and update the file_name_reactive
#'   file_name_reactive <- reactiveVal(c())
#'   LISTEN_file_name_reactive <- reactive({
#'     list(
#'       input$client_name, input$report_creator_name, input$report_creation_date
#'     )
#'   })
#'   observeEvent(LISTEN_file_name_reactive(), {
#'     create_file_name(
#'       client_name_var = input$client_name,
#'       report_creator_name_var = input$report_creator_name,
#'       report_creation_date_var = input$report_creation_date
#'     ) %>% file_name_reactive()
#'   })
#'
#'   # Create and download the workbook
#'   output$download_workbook <- downloadHandler(
#'     filename = function() {
#'       file_name_reactive()
#'     },
#'     content = function(file) {
#'
#'       # # # Save all of the dfs for testing purposes
#'       # DEV_save_app_data_versions_df(input_client_name_reactive_df(), "dev/data/input_client_name_reactive_df.csv")
#'       # DEV_save_app_data_versions_df(input_moore_production_staff_reactive_df(), "dev/data/input_moore_production_staff_reactive_df.csv")
#'       # DEV_save_app_data_versions_df(input_production_freelance_reactive_df(), "dev/data/input_production_freelance_reactive_df.csv")
#'       # DEV_save_app_data_versions_df(input_travel_lodge_food_reactive_df(), "dev/data/input_travel_lodge_food_reactive_df.csv")
#'       # DEV_save_app_data_versions_df(input_location_rental_df_reactive(), "dev/data/input_location_rental_df_reactive.csv")
#'       # DEV_save_app_data_versions_df(input_cast_df_reactive(), "dev/data/input_cast_df_reactive.csv")
#'       # DEV_save_app_data_versions_df(input_equipment_reactive_df(), "dev/data/input_equipment_reactive_df.csv")
#'       # DEV_save_app_data_versions_df(input_post_production_df_reactive(), "dev/data/input_post_production_df_reactive.csv")
#'
#'       wb <- create_excel_workbook(
#'         input_client_name = input_client_name_reactive_df(),
#'         input_moore_production_staff = input_moore_production_staff_reactive_df(),
#'         input_production_freelance = input_production_freelance_reactive_df(),
#'         input_travel_lodge_food = input_travel_lodge_food_reactive_df(),
#'         input_location_rental = input_location_rental_df_reactive(),
#'         input_cast = input_cast_df_reactive(),
#'         input_equipment = input_equipment_reactive_df(),
#'         input_post_production = input_post_production_df_reactive(),
#'         save_location = file
#'       )
#'
#'       saveWorkbook(wb, file = file, overwrite = TRUE)
#'     }
#'   )
#'
#'   # Stop the program
#'   observeEvent(input$exit_program_button, {stopApp()})
#'
#'   # I should adjust code that makes the plots based on this
#'   observeEvent(input$click_to_continue_initial_2, {
#'     if(input$needs_new_footage == "Post only") {
#'       print("Df with Name of Client has been created")
#'       print(input_client_name_reactive_df())
#'       print("Moore Staff")
#'       print(input_moore_production_staff_reactive_df())
#'       print("Freelance Staff")
#'       print(input_production_freelance_reactive_df())
#'       print(input_travel_lodge_food_reactive_df())
#'       print(input_location_rental_df_reactive())
#'       print(input_cast_df_reactive())
#'       print(input_equipment_reactive_df())
#'       print(input_post_production_df_reactive())
#'       # create_excel_workbook(
#'       #   input_client_name = input_client_name_reactive_df(),
#'       #   input_moore_production_staff = input_moore_production_staff_reactive_df(),
#'       #   input_production_freelance = input_production_freelance_reactive_df(),
#'       #   input_travel_lodge_food = input_travel_lodge_food_reactive_df(),
#'       #   input_location_rental = input_location_rental_df_reactive(),
#'       #   input_cast = input_cast_df_reactive(),
#'       #   input_equipment = input_equipment_reactive_df(),
#'       #   input_post_production = input_post_production_df_reactive(),
#'       #   input_total_project_costs = input_total_project_costs,
#'       #   save_location = file
#'       # )
#'     }
#'   })
#'
#'
#' }
