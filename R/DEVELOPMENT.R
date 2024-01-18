#' code_for_creating_shiny_eda_app
#'
#' @export
#'
code_for_creating_shiny_eda_app <- function() {
  path_for_videos_to_code_information <- paste0(qualitative_coding_path, "/Videos_to_Code_Information.xlsx")

  df_donation_information <- readxl::read_excel(path_for_videos_to_code_information, sheet = "Videos to Code")

  df_donation_information$join_name <- stringr::str_replace(df_donation_information$file_name, ".mp4$", "")
  df_donation_information <- df_donation_information %>% dplyr::filter(!is.na(file_name)) %>%
    dplyr::select(Client, Video_Name = join_name, total_donations = sum, average_donation_size = mean, n_donations = n)

  video_full_df <- video_duration_df %>%
    dplyr::left_join(df_donation_information, by = c("video_name" = "Video_Name"))

  names(video_full_df) <- stringr::str_replace(names(video_full_df), "sum_of_seconds_duration_of_", "")

  correlation_vars <- c(
    "n_sections", "animal", "main_character_s", "supporting_character_s",
    "background_character_s", "direct_eye_contact_with_camera_for_any_animals_or_people",
    "lower_third", "logo", "fundraising_information",
    "visual_type__interview_testimonial",
    "visual_type__b_roll_generic",
    "visual_type__b_roll_story_characters",
    "visual_type__b_roll_still_photo",
    "visual_type__full_slate",
    "type_of_slate__call_out_slate_text_on_full_slate_of_screen",
    "type_of_slate__ask_usual_phone_email",
    "type_of_slate__premium_gift",
    "type_of_slate__logo_type",
    "spoken_audio",
    "spoken_audio_type__main_character",
    "spoken_audio_type__supporting_character",
    "spoken_audio_type__voice_over_artist",
    "text_on_screen",
    "type_of_text_on_string__name_of_person_super_graphic",
    "type_of_text_on_string__extra_part_of_lower_third",
    "type_of_text_on_string__statistics",
    "type_of_text_on_string__other",
    "type_of_text_on_string__multiple",
    "story_chapter__introduction",
    "story_chapter__the_problem",
    "story_chapter__resolution",
    "fundraising_information"
  )

  library(ggplot2)
  library(shiny)

  # Define UI for application that draws a histogram
  ui <- fluidPage(

    # Application title
    titlePanel("EDA Duration Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "eda_x_var",
          "Select X Variable",
          all_of(correlation_vars)
        ),
        selectInput(
          "eda_y_var",
          "Select Y Variable",
          c("n_donations", "total_donations", "average_donation_size")
        ),
        selectInput(
          "eda_size_var",
          "Select Size Variable",
          c("average_donation_size", "n_donations", "total_donations")
        ),
        selectInput(
          "eda_color_var",
          "Select Color Variable",
          c("Client", "video_name")
        )
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("edaPlot")
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    output$edaPlot <- renderPlot({

      video_full_df %>%
        ggplot(
          aes(
            x = .data[[{{input$eda_x_var}}]],
            y = .data[[{{input$eda_y_var}}]],
            size = .data[[{{input$eda_size_var}}]],
            color = .data[[{{input$eda_color_var}}]]
          )
        ) +
        geom_point() +
        scale_x_continuous() +
        scale_y_continuous() +
        ggthemes::theme_gdocs()

    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}
