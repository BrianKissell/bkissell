# process_coded_data <- function() {
#   # Indicate the location of the workbook
#   path_to_excel_file <- "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Initial Descriptive Video Coding/Descriptive_Coding_120_seconds.xlsx"
#   path_to_category_time_dataframe_file <- "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Initial Descriptive Video Coding/category_time_dataframe.csv"
#   path_to_video_sections_df_file <- "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Initial Descriptive Video Coding/video_sections_df.csv"
#   path_to_summary_category_time_df_file <- "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Initial Descriptive Video Coding/summary_category_time_df.csv"
#   # Read in the workbook
#   descriptive_video_coding <- readxl::read_excel(path_to_excel_file)
#
#   # Obtain the list of sheets
#   list_of_sheets <- readxl::excel_sheets(path_to_excel_file)
#
#   # Filter out irrelevant sheets
#   list_of_sheets <- list_of_sheets[!(list_of_sheets %in% c("Template", "Data Validation", "category_time_dataframe", "video_sections_df", "summary_category_time_df"))]
#
#   # Provide list of categories
#   categories <- c("Person", "Slate", "Lower Third", "Text on Screen", "Logo", "Talking", "Other")
#
#   # Create function to process the data for a specific sheet / Video
#   process_video_data <- function(
#     path_to_excel_file,
#     sheet_name = "Silent Night Rework CVH",
#     categories
#   ){
#     # Read in the data
#     sheet_coding_data <- readxl::read_excel(path_to_excel_file, sheet = sheet_name)
#
#     # Convert starts into 1 and 0s
#     section_number <- ifelse(sheet_coding_data$Section == "Start", 1, 0) %>%
#       tidyr::replace_na(0) %>%
#       cumsum() %>%
#       stringr::str_pad(width = 2, side = "left", pad = "0")
#
#     # Create the section label
#     sheet_coding_data$section_label <- paste0("Section ", section_number)
#
#     # clean data
#     sheet_coding_data <- sheet_coding_data %>%
#       mutate(across(categories, ~{ifelse(is.na(.x), 0, .x)}))
#
#     # Add Video Name
#     sheet_coding_data$`Video Name` <- sheet_name
#
#     # Re-order the data
#     sheet_coding_data <- sheet_coding_data %>% select(section_label, everything())
#
#     return(sheet_coding_data)
#   }
#
#   sheet_data <- purrr::map_df(list_of_sheets, ~{
#     sheet_data <- process_video_data(path_to_excel_file, sheet_name = .x, categories)
#     sheet_data
#   })
#
#   library(dplyr)
#   library(ggplot2)
#   sheet_data %>%
#     select(video_name = `Video Name`, visual_type = `Visual Type`) %>%
#     group_by(video_name, visual_type) %>%
#     summarize(percentage_of_type = n() / 3606) %>%
#     # tidyr::pivot_wider(names_from = visual_type, values_from = percentage_of_type) %>%
#     clipr::write_clip()
#
#
#
#
#
#   info_on_section_with_category <- function(sheet_data, category){
#     # # Read in the data
#     # sheet_coding_data <- readxl::read_excel(path_to_excel_file, sheet = sheet_name)
#     #
#     sheet_data %>%
#       mutate(duration_of_video = max(.data[["Time Point"]])) %>%
#       dplyr::filter(.data[[category]] == 1) %>%
#       dplyr::group_by(.data[["section_label"]]) %>%
#       summarise(
#         `Video Name`,
#         time_on_screen = max(.data[["Time Point"]]) - min(.data[["Time Point"]]),
#         time_it_appeared = min(.data[["Time Point"]]),
#         category = category,
#         duration_of_video = max(duration_of_video))
#   }
#
#   category_time_df <- purrr::map_df(categories, ~{
#     info_on_section_with_category(sheet_data, category = .x) %>%
#       mutate(categories = .x)
#   })
#
#   category_time_df
#
#   readr::write_csv(category_time_df, path_to_category_time_dataframe_file)
#
#
#   video_sections_df <- purrr::map_df(list_of_sheets, ~{
#     sheet_data <- process_video_data(path_to_excel_file, sheet_name = .x, categories)
#     video_sections_data <- sheet_data %>%
#       dplyr::group_by(.data[["section_label"]]) %>%
#       summarise(
#         time_on_screen = max(.data[["Time Point"]]) - min(.data[["Time Point"]]),
#         time_it_appeared = min(.data[["Time Point"]])
#       ) %>%
#       summarise(average_section_length = mean(time_on_screen), sd_section_length = sd(time_on_screen), number_of_sections = n(), se = sd_section_length/sqrt(number_of_sections), ci = se * 1.96)
#     video_sections_data$video_name <- .x
#     video_sections_data
#   })
#
#   readr::write_csv(video_sections_df, path_to_video_sections_df_file)
#
#   summary_category_time_df <- category_time_df %>%
#     group_by(`Video Name`, category) %>%
#     summarize(
#       total_time = sum(time_on_screen),
#       video_length = max(duration_of_video),
#       percentage = (total_time / video_length) * 100,
#       number_of_sections = n(),
#       time_it_first_appeared = min(time_it_appeared)
#     )
#
#   readr::write_csv(summary_category_time_df, path_to_summary_category_time_df_file)
#
#
#   video_names_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/ready_for_database/VIDEO_NAMES/video_names.csv"
#   video_names_df <- readr::read_csv(video_names_path, show_col_types = FALSE)
#
#
#
#   percentage_on_screen_per_video <- summary_category_time_df %>%
#     select(`Video Name`, category, percentage_on_screen = percentage) %>%
#     tidyr::pivot_wider(names_from = category, values_from = percentage_on_screen)
#
#   colnames(percentage_on_screen_per_video) <- paste0(
#     "percentage_on_screen_",
#     colnames(percentage_on_screen_per_video) %>%
#       snakecase::to_snake_case()
#   ) %>%
#     stringr::str_replace("percentage_on_screen_video_name", "video_name")
#
#   first_appeared_on_screen_per_video <- summary_category_time_df %>%
#     select(video_name, category, time_it_first_appeared) %>%
#     tidyr::pivot_wider(names_from = category, values_from = time_it_first_appeared)
#
#   colnames(first_appeared_on_screen_per_video) <- paste0(
#     "time_it_first_appeared_",
#     colnames(first_appeared_on_screen_per_video) %>%
#       snakecase::to_snake_case()
#   ) %>%
#     stringr::str_replace("time_it_first_appeared_video_name", "video_name")
#
#   number_of_sections_per_video <- video_sections_df %>% select(video_name, number_of_sections)
#
#   video_description_data <- number_of_sections_per_video %>%
#     left_join(percentage_on_screen_per_video, by = "video_name") %>%
#     left_join(first_appeared_on_screen_per_video, by = "video_name") %>%
#     left_join(video_names_df, by = c("video_name" = "video_from_descriptive_data"))
#
#   video_description_data_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/EDA_Shiny_App/data/video_description_data.csv"
#   readr::write_csv(video_description_data, video_description_data_path)
#
#   # video_description_data %>% pull(video_name) %>%
#   #   unique() %>%
#   #   clipr::write_clip()
#
# }
#
# R_Code_descriptive_video_codig <- function() {
#
#   path_to_excel_file <- "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Initial Descriptive Video Coding/Template_Descriptive_Coding_MC.xlsx"
#   descriptive_video_coding <- readxl::read_excel(path_to_excel_file)
#   list_of_sheets <- readxl::excel_sheets(path_to_excel_file)
#   list_of_sheets <- list_of_sheets[!(list_of_sheets %in% c("Template", "Data Validation", "category_time_dataframe", "video_sections_df", "summary_category_time_df"))]
#   categories <- c("Person", "Slate", "Lower Third", "Text on Screen", "Logo", "Talking", "Other")
#
#   # process_video_data <- function(
#     #     path_to_excel_file,
#   #     sheet_name = "Silent Night Rework LAB CVH",
#   #     categories
#   #     ){
#   #   # Read in the data
#   #   sheet_coding_data <- readxl::read_excel(path_to_excel_file, sheet = sheet_name)
#   #   # Convert starts into 1 and 0s
#   #   section_number <- ifelse(sheet_coding_data$Section == "Start", 1, 0) %>%
#   #     tidyr::replace_na(0) %>%
#   #     cumsum() %>%
#   #     stringr::str_pad(width = 2, side = "left", pad = "0")
#   #   # Create the section label
#   #   sheet_coding_data$section_label <- paste0("Section ", section_number)
#   #   # clean data
#   #   sheet_coding_data <- sheet_coding_data %>%
#   #     mutate(across(categories, ~{ifelse(is.na(.x), 0, .x)}))
#   #   # Re-order the data
#   #   sheet_coding_data %>% select(section_label, everything())
#   # }
#
#   info_on_section_with_category <- function(sheet_data, category){
#     sheet_data %>%
#       mutate(duration_of_video = max(.data[["Time Point"]])) %>%
#       dplyr::filter(.data[[category]] == 1) %>%
#       dplyr::group_by(.data[["section_label"]]) %>%
#       summarise(
#         time_on_screen = max(.data[["Time Point"]]) - min(.data[["Time Point"]]),
#         time_it_appeared = min(.data[["Time Point"]]),
#         category = category,
#         duration_of_video = max(duration_of_video))
#   }
#
#   category_time_dataframe <- purrr::map_df(list_of_sheets, ~{
#     sheet_data <- process_video_data(path_to_excel_file, sheet_name = .x, categories)
#     category_time_df <- purrr::map_df(categories, ~{info_on_section_with_category(sheet_data, category = .x)})
#     category_time_df$video_name <- .x
#     category_time_df
#   })
#
#   path_to_category_time_dataframe_file <- "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Initial Descriptive Video Coding/category_time_dataframe.csv"
#   path_to_video_sections_df_file <- "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Initial Descriptive Video Coding/video_sections_df.csv"
#   path_to_summary_category_time_df_file <- "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Initial Descriptive Video Coding/summary_category_time_df.csv"
#
#   readr::write_csv(category_time_dataframe, path_to_category_time_dataframe_file)
#
#   video_sections_df <- purrr::map_df(list_of_sheets, ~{
#     sheet_data <- process_video_data(path_to_excel_file, sheet_name = .x, categories)
#     video_sections_data <- sheet_data %>%
#       dplyr::group_by(.data[["section_label"]]) %>%
#       summarise(
#         time_on_screen = max(.data[["Time Point"]]) - min(.data[["Time Point"]]),
#         time_it_appeared = min(.data[["Time Point"]])
#       ) %>%
#       summarise(average_section_length = mean(time_on_screen), sd_section_length = sd(time_on_screen), number_of_sections = n(), se = sd_section_length/sqrt(number_of_sections), ci = se * 1.96)
#     video_sections_data$video_name <- .x
#     video_sections_data
#   })
#
#   readr::write_csv(video_sections_df, path_to_video_sections_df_file)
#
#   summary_category_time_df <- category_time_dataframe %>%
#     group_by(video_name, category) %>%
#     summarize(
#       total_time = sum(time_on_screen),
#       video_length = max(duration_of_video),
#       percentage = (total_time / video_length) * 100,
#       number_of_sections = n(),
#       time_it_first_appeared = min(time_it_appeared)
#     )
#
#   readr::write_csv(summary_category_time_df, path_to_summary_category_time_df_file)
#
#
#   video_names_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/ready_for_database/VIDEO_NAMES/video_names.csv"
#   video_names_df <- readr::read_csv(video_names_path, show_col_types = FALSE)
#
#
#
#   percentage_on_screen_per_video <- summary_category_time_df %>%
#     select(video_name, category, percentage_on_screen = percentage) %>%
#     tidyr::pivot_wider(names_from = category, values_from = percentage_on_screen)
#
#   colnames(percentage_on_screen_per_video) <- paste0(
#     "percentage_on_screen_",
#     colnames(percentage_on_screen_per_video) %>%
#       snakecase::to_snake_case()
#   ) %>%
#     stringr::str_replace("percentage_on_screen_video_name", "video_name")
#
#   first_appeared_on_screen_per_video <- summary_category_time_df %>%
#     select(video_name, category, time_it_first_appeared) %>%
#     tidyr::pivot_wider(names_from = category, values_from = time_it_first_appeared)
#
#   colnames(first_appeared_on_screen_per_video) <- paste0(
#     "time_it_first_appeared_",
#     colnames(first_appeared_on_screen_per_video) %>%
#       snakecase::to_snake_case()
#   ) %>%
#     stringr::str_replace("time_it_first_appeared_video_name", "video_name")
#
#   number_of_sections_per_video <- video_sections_df %>% select(video_name, number_of_sections)
#
#   video_description_data <- number_of_sections_per_video %>%
#     left_join(percentage_on_screen_per_video, by = "video_name") %>%
#     left_join(first_appeared_on_screen_per_video, by = "video_name") %>%
#     left_join(video_names_df, by = c("video_name" = "video_from_descriptive_data"))
#
#   video_description_data_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/MNFLAB_DATABASE/EDA_Shiny_App/data/video_description_data.csv"
#   readr::write_csv(video_description_data, video_description_data_path)
#
#   # video_description_data %>% pull(video_name) %>%
#   #   unique() %>%
#   #   clipr::write_clip()
#
# }
#
