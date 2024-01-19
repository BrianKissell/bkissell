#' code_for_creating_shiny_eda_app
#'
#' @export
#'
# code_for_creating_shiny_eda_app <- function() {
#   path_for_videos_to_code_information <- paste0(qualitative_coding_path, "/Videos_to_Code_Information.xlsx")
#
#   df_donation_information <- readxl::read_excel(path_for_videos_to_code_information, sheet = "Videos to Code")
#
#   df_donation_information$join_name <- stringr::str_replace(df_donation_information$file_name, ".mp4$", "")
#   df_donation_information <- df_donation_information %>% dplyr::filter(!is.na(file_name)) %>%
#     dplyr::select(Client, Video_Name = join_name, total_donations = sum, average_donation_size = mean, n_donations = n)
#
#   video_full_df <- video_duration_df %>%
#     dplyr::left_join(df_donation_information, by = c("video_name" = "Video_Name"))
#
#   names(video_full_df) <- stringr::str_replace(names(video_full_df), "sum_of_seconds_duration_of_", "")
#
#   correlation_vars <- c(
#     "n_sections", "animal", "main_character_s", "supporting_character_s",
#     "background_character_s", "direct_eye_contact_with_camera_for_any_animals_or_people",
#     "lower_third", "logo", "fundraising_information",
#     "visual_type__interview_testimonial",
#     "visual_type__b_roll_generic",
#     "visual_type__b_roll_story_characters",
#     "visual_type__b_roll_still_photo",
#     "visual_type__full_slate",
#     "type_of_slate__call_out_slate_text_on_full_slate_of_screen",
#     "type_of_slate__ask_usual_phone_email",
#     "type_of_slate__premium_gift",
#     "type_of_slate__logo_type",
#     "spoken_audio",
#     "spoken_audio_type__main_character",
#     "spoken_audio_type__supporting_character",
#     "spoken_audio_type__voice_over_artist",
#     "text_on_screen",
#     "type_of_text_on_string__name_of_person_super_graphic",
#     "type_of_text_on_string__extra_part_of_lower_third",
#     "type_of_text_on_string__statistics",
#     "type_of_text_on_string__other",
#     "type_of_text_on_string__multiple",
#     "story_chapter__introduction",
#     "story_chapter__the_problem",
#     "story_chapter__resolution",
#     "fundraising_information"
#   )
#
#   library(ggplot2)
#   library(shiny)
#
#   # Define UI for application that draws a histogram
#   ui <- fluidPage(
#
#     # Application title
#     titlePanel("EDA Duration Data"),
#
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#       sidebarPanel(
#         selectInput(
#           "eda_x_var",
#           "Select X Variable",
#           all_of(correlation_vars)
#         ),
#         selectInput(
#           "eda_y_var",
#           "Select Y Variable",
#           c("n_donations", "total_donations", "average_donation_size")
#         ),
#         selectInput(
#           "eda_size_var",
#           "Select Size Variable",
#           c("average_donation_size", "n_donations", "total_donations")
#         ),
#         selectInput(
#           "eda_color_var",
#           "Select Color Variable",
#           c("Client", "video_name")
#         )
#       ),
#
#       # Show a plot of the generated distribution
#       mainPanel(
#         plotOutput("edaPlot")
#       )
#     )
#   )
#
#   # Define server logic required to draw a histogram
#   server <- function(input, output) {
#
#     output$edaPlot <- renderPlot({
#
#       video_full_df %>%
#         ggplot(
#           aes(
#             x = .data[[{{input$eda_x_var}}]],
#             y = .data[[{{input$eda_y_var}}]],
#             size = .data[[{{input$eda_size_var}}]],
#             color = .data[[{{input$eda_color_var}}]]
#           )
#         ) +
#         geom_point() +
#         scale_x_continuous() +
#         scale_y_continuous() +
#         ggthemes::theme_gdocs()
#
#     })
#   }
#
#   # Run the application
#   shinyApp(ui = ui, server = server)
# }





#' Fully read and process the global coding data.
#'
#' @param man_wd man_wd
#' @param file_part__sm_raw_folder file_part__sm_raw_folder
#' @param global_coding_file_path global_coding_file_path
#' @param backup_for_combined_data_folder_path backup_for_combined_data_folder_path
#' @param backup_for_editable_data_folder_path backup_for_editable_data_folder_path
#' @param backup_for_raw_data_folder_path backup_for_raw_data_folder_path
#' @param backup_for_data_to_add_folder_path backup_for_data_to_add_folder_path
#'
#' @return global_coding_to_save
#' @export
#'
# FULL_global_coding_with_editiable_worksheet <- function(
#     man_wd = NULL,
#     file_part__sm_raw_folder = "Qualitative Coding/Version 2/global_variables/raw_data",
#     global_coding_file_path = "Qualitative Coding/Version 2/global_variables/Global_Coding_REFERENCE.xlsx"
#     # ,
#     # backup_for_combined_data_folder_path = "Qualitative Coding/Version 2/global_variables/backup_files/backup_for_combined_data",
#     # backup_for_editable_data_folder_path = "Qualitative Coding/Version 2/global_variables/backup_files/backup_for_editable_data",
#     # backup_for_raw_data_folder_path = "Qualitative Coding/Version 2/global_variables/backup_files/backup_for_raw_data",
#     # backup_for_data_to_add_folder_path = "Qualitative Coding/Version 2/global_variables/backup_files/backup_for_data_to_add"
# ) {
#
#   # Read in the data from survey monkey
#   survey_monkey_data <- bkissell::read_survey_monkey_data(
#     man_wd = man_wd,
#     file_part__sm_raw_folder = file_part__sm_raw_folder
#   )[[1]]
#
#   # Convert particular NAs to 0s, and other responses to 1s
#   survey_monkey_data <- survey_monkey_data %>%
#     mutate(
#       across(
#         c(starts_with("animals__"),
#           starts_with("mc_"),
#           starts_with("sc_"),
#           starts_with("bc_"),
#           starts_with("cel_"),
#           starts_with("premium__")
#         ),
#         ~{ifelse(is.na(.x), 0, 1)})
#     )
#
#   # Adjust organization of the data
#   survey_monkey_data <- survey_monkey_data %>%
#     dplyr::select(-c("collector_id", "email_address", "first_name", "last_name", "custom_data_1"))
#
#   # Create id so it with a letter so it is read correctly in excel
#   survey_monkey_data$respondent_id <- paste0("id_", survey_monkey_data$respondent_id)
#
#   #### Update Global Variables with Editable - Survey Monkey
#
#   global_coding_file_path <- {{global_coding_file_path}}
#
#   # Read in the editable file
#   global_coding_for_editing <- readxl::read_excel(global_coding_file_path)
#
#   # Remove NAs from the respondent id column
#   global_coding_for_editing <- global_coding_for_editing %>% dplyr::filter(!is.na(respondent_id))
#
#   # For the add file, remove the columns from the survey monkey data that already exist in the editable file
#   global_coding_to_add <- survey_monkey_data %>%
#     dplyr::filter(!(.data[["respondent_id"]] %in% global_coding_for_editing$respondent_id))
#
#   # Decide which files to use in differing circumstances
#   if(nrow(global_coding_to_add) == 0 & nrow(global_coding_for_editing) == 0) {
#     # If nothing exists, default to the editable file
#     global_coding_to_save <- global_coding_for_editing
#   } else if(nrow(global_coding_for_editing) == 0 & nrow(global_coding_to_add) > 0) {
#     # If only data exists in the data to add, use that
#     global_coding_to_save <- global_coding_to_add
#   } else if (nrow(global_coding_for_editing) > 0 & nrow(global_coding_to_add) == 0) {
#     # If data only exists in the editable data, use that
#     global_coding_to_save <- global_coding_for_editing
#   } else if (nrow(global_coding_for_editing) > 0 & nrow(global_coding_to_add) > 0) {
#     # Otherwise, add the two data sets together
#     global_coding_to_save <- global_coding_for_editing %>%
#       rbind(global_coding_to_add)
#   }
#
#   # Organize the data by end date
#   global_coding_to_save <- global_coding_to_save %>% arrange(end_date)
#
#   # Write all of the files
#   backup_for_combined_data_file_path <- paste0(backup_for_combined_data_folder_path, "/combined.csv") %>% bkissell::combine_file_string_with_time()
#   backup_for_editable_data_file_path <- paste0(backup_for_editable_data_folder_path, "/editable.csv") %>% bkissell::combine_file_string_with_time()
#   backup_for_raw_data_file_path <- paste0(backup_for_raw_data_folder_path, "/raw.csv") %>% bkissell::combine_file_string_with_time()
#   backup_for_data_to_add_file_path <- paste0(backup_for_data_to_add_folder_path, "/to_add.csv") %>% bkissell::combine_file_string_with_time()
#   readr::write_csv(global_coding_for_editing, backup_for_editable_data_file_path)
#   readr::write_csv(survey_monkey_data, backup_for_raw_data_file_path)
#   readr::write_csv(global_coding_to_add, backup_for_data_to_add_file_path)
#   readr::write_csv(global_coding_to_save, backup_for_combined_data_file_path)
#   readr::write_csv(global_coding_to_save, global_coding_file_path)
#   writexl::write_xlsx(global_coding_to_save, global_coding_file_path)
#
#   # Return the object
#   return(global_coding_to_save)
# }

