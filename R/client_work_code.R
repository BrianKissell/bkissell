# TNC_Immersion_Analysis <- function() {
#   ##--------Parameters-------##
#   location_for_the_raw_immersion_folder <- "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/00 Jobs/2023/220159_TNC_BC__Immersion Only_OID962/Analysis/Immersion/Raw Data"
#   video_marks <- c("^bwd$", "^itw$", "^tsg$")
#   video_levels <- c("Beautiful World", "In the Wild", "The Seedling")
#   length_of_video_sec <- 120
#   #############################
#
#   ##--------
#   # Obtain the file names for the immersion data
#   immersion_file_names <- list.files(location_for_the_raw_immersion_folder, pattern = ".xlsx")
#
#   # Obtain the file paths for the immersion data
#   immersion_file_paths <- paste0(location_for_the_raw_immersion_folder, "/", immersion_file_names)
#
#   # Read and combine the raw immersion data files
#   immersion_data <- bktools::read_immersion_csv_files_new(paths = immersion_file_paths)
#
#   # Check for weird issues with the videos marks.
#   for(i in unique(immersion_data$label)) {
#     good_mark <- any(stringr::str_detect(i, video_marks))
#     if(!good_mark){
#       warning(i, "is not an appropriate mark and needs to be adjusted")
#     }
#   }
#   # message(
#   #   paste0(
#   #     "These are the labels that were included in this dataset: ",
#   #     paste0(unique(immersion_data$label), collapse = ", ")))
#
#   # # # DEV Purposes
#   # immersion_data$label <- immersion_data$label[immersion_data$label == "fgn"] <- "bwd"
#   # immersion_data$label <- immersion_data$label[immersion_data$label == "myh"] <- "itw"
#   # immersion_data$label <- immersion_data$label[immersion_data$label == "bpd"] <- "tsg"
#
#   # Loop through and change the marks to the correct names
#   for(i in seq_along(video_marks)){
#     immersion_data$label <- stringr::str_replace(immersion_data$label, video_marks[[i]], video_levels[[i]])
#   }
#
#   # Filter out invalid marks, and time after video was shown
#   immersion_data <- dplyr::filter(immersion_data, .data[["seconds"]] <= length_of_video_sec)
#   immersion_data <- dplyr::filter(immersion_data, .data[["label"]] %in% video_levels)
#
#   # Obtain summary immersion data
#   immersion_individual_summary <- bktools::immersion_individual_summary(immersion_data, identifier = "sensor_id")
#
#   # Prepare variable to allow us to sort the data
#   immersion_individual_summary$time_for_sorting <- as.numeric(stringr::str_extract(immersion_individual_summary$session, pattern = "[0-9]{8}"))
#
#   # Sort and select desired data
#   immersion_individual_summary <- dplyr::arrange(immersion_individual_summary, time_for_sorting)
#   immersion_individual_summary <- dplyr::select(immersion_individual_summary, sensor_id, label, AVG_Immersion, SD_Immersion, missing_data_counts, n, percent_missing, immersion_status, session)
#
#   # Save the data to your clipboard so you can paste it into the workbook
#   clipr::write_clip(immersion_individual_summary)
#
#   # obtain the acceptable ids
#   good_immersion_ids <- immersion_individual_summary %>%
#     dplyr::filter(immersion_status == "Include data") %>%
#     dplyr::pull(sensor_id)
#
#   # Filter for acceptable participants
#   immersion_data_filtered <- immersion_data %>%
#     dplyr::filter(sensor_id %in% good_immersion_ids)
#
#   ##-------Additional Processing for Excel Workbook------##
#   # Process data into multiple lists that contain differing aggregations
#   summarized_immersion_list <- bktools::create_summarized_immersion_list(data = immersion_data_filtered, identifier = "sensor_id")
#
#   # Obtain df that has the per second data
#   immersion_per_second <- summarized_immersion_list$immersion_per_second
#
#   # Obtain df that has the per person data
#   immersion_person_data <- summarized_immersion_list$immersion_person_data
#
#   # Obtain the summary data for each video
#   overall_stats <- summarized_immersion_list$overall_stats
#
#
#   labels <- unique(immersion_per_second$label)
#
#   video_colors <- c("blue", "yellow", "green")
#
#   # animated_plots <- purrr::map2(labels, video_colors, ~{
#   #   immersion_per_second %>%
#   #     dplyr::filter(label == .x) %>%
#   #     ggplot2::ggplot(
#   #       ggplot2::aes(
#   #         y = .data[["score"]],
#   #         x = .data[["seconds"]])) +
#   #     ggplot2::geom_rect(
#   #       ggplot2::aes(
#   #         xmin = min(.data[["seconds"]])-1,
#   #         xmax = max(.data[["seconds"]]),
#   #         ymin = overall_mean - se_times_2,
#   #         ymax = overall_mean + se_times_2), fill = "grey", size = 1, alpha = .01) +
#   #     ggplot2::geom_rect(
#   #       ggplot2::aes(
#   #         xmin = min(.data[["seconds"]]),
#   #         xmax = max(.data[["seconds"]]),
#   #         ymin = overall_mean - .01,
#   #         ymax = overall_mean + .01), color = "black", size = 1) +
#   #     ggplot2::geom_line(
#   #       ggplot2::aes(x = .data[["seconds"]], y = .data[["score"]]), size = 1, color = .y, alpha = .9) +
#   #     ggplot2::geom_point(
#   #       ggplot2::aes(
#   #         group = seq_along(seconds)), size = 3, color = .y) +
#   #     ggplot2::scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 5), expand = c(0,0)) +
#   #     ggplot2::scale_y_continuous(limits = c(min_y, max_y), breaks = seq(min_y, max_y, 5), expand = c(0,0)) +
#   #     ggplot2::labs(title = plot_title, x = "", y = "") +
#   #     ggthemes::theme_gdocs() +
#   #     gganimate::transition_reveal(seconds, keep_last = TRUE)
#   #
#   # })
#   #
#   # gganimate::animate(animated_plots[[1]], nframes = max_x, fps = 1, duration = max_x, width = 2000, height = 500, detail = 5)
#
#
#   # Set the variables to factors
#   immersion_person_data$label <- as.factor(immersion_person_data$label)
#
#   options(contrasts = c("contr.helmert", "contr.poly"))
#
#   immersion_person_lm <- lm(mean_per_person ~ label, data = immersion_person_data)
#
#   car::Anova(immersion_person_lm, type = 3)
#
#   beautiful_world_immersion <- immersion_person_data %>%
#     dplyr::filter(label == "Beautiful World") %>%
#     dplyr::pull(mean_per_person)
#
#   the_seedling_immersion <- immersion_person_data %>%
#     dplyr::filter(label == "The Seedling") %>%
#     dplyr::pull(mean_per_person)
#
#   in_the_wild_immersion <- immersion_person_data %>%
#     dplyr::filter(label == "In the Wild") %>%
#     dplyr::pull(mean_per_person)
#
#
#   effsize::cohen.d(beautiful_world_immersion, the_seedling_immersion)
#   effsize::cohen.d(beautiful_world_immersion, in_the_wild_immersion)
#   effsize::cohen.d(the_seedling_immersion, in_the_wild_immersion)
#
#
#
#   bktools::create_immersion_workbook(project_dir = home_dir, immersion_per_second, file_name = "immersion_workbook")
#
# }
#
#
#
# DEV_create_table_of_formulas_excel <- function() {
#   library(readr)
#   library(openxlsx)
#
#   # Set up body and head styles
#   bodyStyle <- openxlsx::createStyle(
#     border = "TopBottomLeftRight",
#     borderColour = "#3c3f43",
#     fontName = "Cambria",
#     halign = "center",
#     valign = "center")
#
#   headStyleInput <- openxlsx::createStyle(
#     border = "TopBottomLeftRight",
#     borderColour = "#3c3f43",
#     fontName = "Cambria",
#     halign = "center",
#     valign = "center",
#     bgFill = "#ed7d31",
#     fgFill = "#ed7d31")
#
#   headStyle <- openxlsx::createStyle(
#     border = "TopBottomLeftRight",
#     borderColour = "#3c3f43",
#     fontName = "Cambria",
#     halign = "center",
#     valign = "center",
#     bgFill = "#0070C0",
#     fgFill = "#0070C0",
#     fontColour = "#FFFFFF")
#
#   wb <- createWorkbook()
#
#   # data <- read_csv("tnc_data.csv", show_col_types = FALSE)
#   data <- readRDS(file = "tnc_data.rds")
#
#   #
#   # create_table_of_formulas <- function(variable_1, variable_2, row_start = 1, col_start = 1){
#   #   row_n_to_change <- row_start - 1
#   #   col_n_to_change <- col_start - 1
#   #   table <- table(variable_1, variable_2)
#   #   value_n_col <- ncol(table)
#   #   value_n_row <- nrow(table)
#   #   full_table_col_names <- c("Table Name", dimnames(table)[[2]], "Total")
#   #   full_table_row_names <- c("Table Name", dimnames(table)[[1]], "Total")
#   #   full_table_n_col <- length(full_table_col_names)
#   #   full_table_n_row <- length(full_table_row_names)
#   #   #
#   #   # print(value_n_col, value_n_row, full_table_col_names, full_table_row_names)
#   #   excel_location_full_table_columns <- seq(col_n_to_change + 1, col_n_to_change + full_table_n_col)
#   #   excel_location_full_table_start_row <- seq(row_n_to_change + 1, row_n_to_change + full_table_n_row)
#   #
#   #   location_data <- tidyr::crossing(excel_location_full_table_start_row, excel_location_full_table_columns)
#   #   location_data
#   #   # c(
#   #   #   rep("column_name", length(full_table_col_names)),
#   #   #   rep(length(full_table_col_names)-2)
#   #   #   )
#   #   }
#   #
#   # created_table <- create_table_of_formulas(data$video, data$call_phone_or_go_online)
#
#
#   create_frequency_table <- function(data, var1, var2_string) {
#     table <- data %>%
#       count(.data[[var1]], .data[[var2_string]], .drop = FALSE) %>%
#       tidyr::pivot_wider(names_from = .data[[var2_string]], values_from = n) %>%
#       dplyr::mutate(Total = rowSums(across(where(is.numeric)), na.rm=TRUE)) %>%
#       ungroup()
#     total <- table %>%
#       dplyr::summarize(across(where(is.numeric),  sum), {{var1}} := "Total") %>%
#       dplyr::select({{var1}}, everything())
#     table <- rbind(table, total)
#     return(table)
#   }
#
#   create_percentage_table <- function(data, var1, var2_string) {
#     table <- data %>%
#       count(.data[[var1]], .data[[var2_string]], .drop = FALSE) %>%
#       group_by(.data[[var1]]) %>%
#       mutate('%' = round(n / sum(n), 2)) %>%
#       select(-n) %>%
#       tidyr::pivot_wider(names_from = .data[[var2_string]], values_from = '%') %>%
#       ungroup()
#     total <- table %>%
#       dplyr::summarize(across(where(is.numeric),  mean), {{var1}} := "Total") %>%
#       dplyr::select({{var1}}, everything())
#     table <- rbind(table, total)
#     return(table)
#   }
#
#   variables_to_create_tables_for <- c(
#     "call_phone_or_go_online",
#     "which_device",
#     "remember_url_in_advertisement",
#     "frequency_asked_for",
#     "use_blanket",
#     "blanket_would_influence_donations",
#     "use_calendar",
#     "calendar_would_influence_donations",
#     "use_magazine",
#     "magazine_would_influence_donations",
#     "agree__enjoyed_viewing_ad",
#     "agree__enjoyed_learning_about_org",
#     "agree__organization_appears_trustworthy",
#     "agree__these_issues_are_important",
#     "agree__ad_was_well_made",
#     "agree__ad_was_clearly_presented",
#     "agree__ad_presented_most_important_information",
#     "agree__appreciated_tone_and_feel_of_ad",
#     "agree__music_in_ad_was_a_good_choice",
#     "agree__ad_felt_inauthentic_or_fake__numeric_been_reversed",
#     "agree__ad_felt_boring__numeric_been_reversed",
#     "agree__ad_too_repetitive__numeric_been_reversed",
#     "agree__voice_over_gave_useful_information",
#     "agree__voice_over_clear",
#     "agree__there_were_people_I_enjoyed_seeing",
#     "agree__specific_things_motivated_me_to_donate",
#     "agree__specific_things_made_me_hesitant_to_help",
#     "agree__I_would_donate",
#     "agree__ad_would_raise_money",
#     "agree__my_priorities_match_the_organization",
#     "agree__issues_related_to_me",
#     "agree__issues_related_to_friends_and_family",
#     "agree__this_org_deserves_support",
#     "agree__org_makes_the_world_a_better_place",
#     "agree__org_has_made_substantial_progress",
#     "agree__org_uses_funds_responsibly",
#     "agree__good_name_recognition_of_org",
#     "agree__org_doing_exciting_work",
#     "agree__it_is_important_that_this_org_exists",
#     "agree__org_can_make_lives_better",
#     "agree__org_asking_for_appropriate_amount",
#     "agree__clearly_showed_how_donations_spent",
#     "agree__convinced_importance_of_cause",
#     "impact_donation_would_have_on_cause",
#     "one_time_or_monthly_donation"
#   )
#
#   other_questions <- c(
#     "what_was_the_free_gift_tote_bag",
#     "what_was_the_free_gift_t-shirt",
#     "what_was_the_free_gift_blanket",
#     "what_was_the_free_gift_trunk_organizer",
#     "what_was_the_free_gift_calendar",
#     "what_was_the_free_gift_gift_card",
#     "what_was_the_free_gift_magnet",
#     "what_was_the_free_gift_sticker",
#     "what_was_the_free_gift_magazine",
#     "what_was_the_free_gift_other",
#     "percentage_that_goes_to_cause",
#     "percentage_that_should_go_to_cause")
#
#
#   where_the_tables_should_start_in_excel_row <- 2
#   where_the_tables_should_start_in_excel_col <- 2
#
#   for(i in variables_to_create_tables_for){
#     the_sheet_name <- {{i}}
#     the_sheet_name <- substr(the_sheet_name,1,30)
#     addWorksheet(wb, the_sheet_name)
#
#     created_table <- create_frequency_table(data, "video", i)
#     created_percentage <- create_percentage_table(data, "video", i)
#
#     # Write data to each sheet
#     writeData(
#       wb,
#       sheet = the_sheet_name,
#       created_table,
#       startCol = where_the_tables_should_start_in_excel_col,
#       startRow = where_the_tables_should_start_in_excel_row,
#       headerStyle = headStyle,
#       borders = "all"
#     )
#
#     writeData(
#       wb,
#       sheet = the_sheet_name,
#       created_percentage,
#       startCol = where_the_tables_should_start_in_excel_col,
#       startRow = nrow(created_table) + 4,
#       headerStyle = headStyle,
#       borders = "all")
#
#   }
#
#   # openxlsx::addStyle(wb, "Test sheet", bodyStyle, rows = balanced_list_rows, cols = balanced_list_columns, gridExpand = TRUE)
#   # openxlsx::addStyle(wb, "Test sheet", headStyle, rows = 1, cols = balanced_list_columns, gridExpand = TRUE)
#
#
#   saveWorkbook(wb, "test.xlsx", overwrite = TRUE)
#
# }
#
# data_cleaning_1 <- function() {
#   # Load packages
#   library(dplyr)
#   library(readr)
#
#   # Read in the raw data
#   tnc_raw_data <- read_csv("tnc_raw_combined.csv", show_col_types = FALSE)
#
#   # Create vectors that contain the response options for all question types
#   call_phone_or_go_online_labels <- c("I would call the phone number provided to donate", "I would go online to donate")
#   which_device_labels <- c("Personal computer", "Mobile device", "Tablet")
#   remember_url_in_advertisement_labels <- c("Yes", "No")
#   gift_response_options <- c("Absolutely not", "Probably not", "Neutral", "Probably", "Absolutely")
#   frequency_asked_for_labels <- c("Monthly donation", "Other (please specify)", "One-time donation")
#   agreement_labels <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neutral", "Somewhat agree", "Agree", "Strongly agree")
#   impact_donation_would_have_on_cause_labels <- c("It would not make any difference", "It would only make a slight difference", "It would make a moderate difference", "It would make a large difference", "It would make a very large difference")
#   one_time_or_monthly_donation_labels <- c("One-time donation", "Monthly donation", "Other (please specify)")
#
#   # Since this will converted to numeric, save the text as a separate file.
#   tnc_raw_data$text_what_was_the_free_gift_other <- tnc_raw_data$what_was_the_free_gift_other
#
#   # Create a helper function to reverse code items
#   reverse_code <- function(x) {
#     if(is.numeric(x)){
#       (x * -1) + (length(unique(x)) + 1)
#     } else {
#       x
#     }
#   }
#
#   # Clean the data
#   tnc_data <- tnc_raw_data %>%
#     mutate(
#       # Create factors and assign appropriate labels
#       call_phone_or_go_online = factor(call_phone_or_go_online, levels = call_phone_or_go_online_labels, ordered = TRUE),
#       which_device = factor(which_device, levels = which_device_labels, ordered = TRUE),
#       remember_url_in_advertisement = factor(remember_url_in_advertisement, levels = remember_url_in_advertisement_labels, ordered = TRUE),
#       frequency_asked_for = factor(frequency_asked_for, levels = frequency_asked_for_labels, ordered = TRUE),
#       across(c(starts_with("use_"), ends_with("_would_influence_donations")), ~{factor(.x, levels = gift_response_options, ordered = TRUE)}),
#       across(starts_with("agree__"), ~ {factor(.x, levels = agreement_labels, ordered = TRUE)}),
#       impact_donation_would_have_on_cause = factor(impact_donation_would_have_on_cause, levels = impact_donation_would_have_on_cause_labels, ordered = TRUE),
#       one_time_or_monthly_donation = factor(one_time_or_monthly_donation, levels = one_time_or_monthly_donation_labels, ordered = TRUE),
#       # Convert variables to numeric
#       across(starts_with("what_was_the_free_gift_"), ~{ifelse(!is.na(.x), 1, 0)}),
#       across(c("percentage_that_goes_to_cause", "percentage_that_should_go_to_cause"), as.numeric),
#       # Convert likert factors to numeric scores
#       across(starts_with("agree__"), as.numeric, .names = "numeric_{.col}"),
#       across(contains("__reverse_code_this_item"), reverse_code, .names = "{stringr::str_replace(.col, '_reverse_code_this_item', '')}_numeric_been_reversed")
#     ) %>%
#     select(!ends_with("_reverse_code_this_item")) %>%
#     select(everything(), "Region",	"Ethnicity", 	"Age_Group",	"Gender")
#
#
#   # Save cleaned data
#   write_csv(tnc_data, "tnc_data.csv")
#   saveRDS(tnc_data, file = "tnc_data.rds")
#
# }
#
#
#
#
# # Allow the author to enter a local path.
# # This code converts the path for the user.
# FILES_obtain_home_dir_for_current_machine <- function(example_home_dir){
#   # Obtain the working directory from the user's computer.
#   local_working_directory <- getwd()
#
#   # Split the working directory into multiple parts to make it easier to work with.
#   parts_of_wd <- local_working_directory %>%
#     strsplit(.Platform$file.sep) %>%
#     unlist()
#
#   # Check if the first element of the working directory is a drive
#   if(stringr::str_detect(parts_of_wd[[1]], "[:]$")){
#     local_computer_drive <- parts_of_wd[[1]]
#   } else { stop("Error as wd does not have an associated drive.")}
#
#   # Check if the second element is Users to make sure we are not in a odd location
#   if(stringr::str_detect(parts_of_wd[[2]], "Users")){
#     users_folder_name <- "Users"
#   } else {stop("There is an issue with the working directory")}
#
#   # Assign other elements
#   users_computer_name <- parts_of_wd[[3]]
#   dropbox_folder <- "Dropbox (TCM Creative)"
#   lab_folder_name <- "04 MDM Neuro-Fundraising Lab"
#
#   # Obtain a string that contains the location part of the project
#   home_dir_path_part <- example_home_dir %>%
#     stringr::str_replace(paste0(".+", lab_folder_name, "/"), "")
#
#   # create a path for the download location and export it as a global variable
#   download_location <<- c(local_computer_drive, users_folder_name, users_computer_name, "Downloads") %>%
#     paste0(collapse = "/")
#
#   # Create the home_directory, and export it as a global variable
#   home_dir <<- c(local_computer_drive, users_folder_name, users_computer_name, dropbox_folder, lab_folder_name, home_dir_path_part) %>%
#     paste0(collapse = "/")
#
#   # Return the home_directory
#   return(home_dir)
# }
#
#
#
#
#
# create_category_anova <- function(data, question) {
#   jmv::anovaRM(
#     data = data,
#     rm = list(
#       list(
#         label="RM Factor 1",
#         levels=c(
#           paste0("Before_After_", question),
#           paste0("Families_", question),
#           paste0("Medical_", question),
#           paste0("Mild_", question),
#           paste0("Older_", question),
#           paste0("Severe_", question),
#           paste0("Younger_", question)))),
#     rmCells = list(
#       list(
#         measure=paste0("average_before_after_individuals_", question),
#         cell=paste0("Before_After_", question)),
#       list(
#         measure=paste0("average_family_individuals_", question),
#         cell=paste0("Families_", question)),
#       list(
#         measure=paste0("average_medical_individuals_", question),
#         cell=paste0("Medical_", question)),
#       list(
#         measure=paste0("average_mild_individuals_", question),
#         cell=paste0("Mild_", question)),
#       list(
#         measure=paste0("average_older_individuals_", question),
#         cell=paste0("Older_", question)),
#       list(
#         measure=paste0("average_severe_individuals_", question),
#         cell=paste0("Severe_", question)),
#       list(
#         measure=paste0("average_younger_individuals_", question),
#         cell=paste0("Younger_", question))),
#     bs = country,
#     effectSize = "eta",
#     rmTerms = ~ `RM Factor 1`,
#     bsTerms = ~ country,
#     postHoc = list(
#       "RM Factor 1",
#       "country",
#       c("RM Factor 1", "country")),
#     emMeans = ~ `RM Factor 1`:country,
#     emmTables = TRUE)
# }
#
# run_2_way_rm_anova <- function(data, category, question) {
#   jmv::anovaRM(
#     data = data,
#     rm = list(
#       list(
#         label="RM Factor 1",
#         levels=c(
#           paste0(category, "_Image_1_", question),
#           paste0(category, "_Image_2_", question),
#           paste0(category, "_Image_3_", question),
#           paste0(category, "_Image_4_", question),
#           paste0(category, "_Image_5_", question),
#           paste0(category, "_Image_6_", question),
#           paste0(category, "_Image_7_", question),
#           paste0(category, "_Image_8_", question),
#           paste0(category, "_Image_9_", question)))),
#     rmCells = list(
#       list(
#         measure= paste0(category, "_1_", question),
#         cell=paste0(category, "_Image_1_", question)),
#       list(
#         measure= paste0(category, "_2_", question),
#         cell=paste0(category, "_Image_2_", question)),
#       list(
#         measure= paste0(category, "_3_", question),
#         cell=paste0(category, "_Image_3_", question)),
#       list(
#         measure= paste0(category, "_4_", question),
#         cell=paste0(category, "_Image_4_", question)),
#       list(
#         measure= paste0(category, "_5_", question),
#         cell=paste0(category, "_Image_5_", question)),
#       list(
#         measure= paste0(category, "_6_", question),
#         cell=paste0(category, "_Image_6_", question)),
#       list(
#         measure= paste0(category, "_7_", question),
#         cell=paste0(category, "_Image_7_", question)),
#       list(
#         measure= paste0(category, "_8_", question),
#         cell=paste0(category, "_Image_8_", question)),
#       list(
#         measure= paste0(category, "_9_", question),
#         cell=paste0(category, "_Image_9_", question))),
#     bs = country,
#     effectSize = "eta",
#     rmTerms = ~ `RM Factor 1`,
#     bsTerms = ~ country,
#     spherTests = TRUE,
#     leveneTest = TRUE,
#     qq = TRUE,
#     postHoc = list(
#       "RM Factor 1",
#       "country",
#       c("RM Factor 1", "country")),
#     emMeans = ~ `RM Factor 1` + country + `RM Factor 1`:country,
#     emmTables = TRUE,
#     groupSumm = TRUE)
# }
#
#
# OPS_data_processing <- function() {
#
#   # Set the path to where the survey data will be stored
#   survey_directory_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/00 Jobs/2023/230180_OPS___Image Testing Online Study_OID1536/Data Collection/OPS_Survey_Data"
#
#   # Adjust path to be correct no matter what computer is being used
#   survey_data_raw_directory <- survey_directory_path %>%
#     FILES_obtain_home_dir_for_current_machine()
#
#   # Get the file names that are provided in this directory
#   survey_data_raw_directory_filenames <- survey_data_raw_directory %>%
#     list.files()
#
#   # Test whether each file is a directory or not (This folder should only have
#   # directories that represent the different versions of the surveys)
#   # One exception is the processed_data folder
#   is_project_names <- survey_data_raw_directory_filenames %>%
#     tools::file_ext() %>%
#     stringr::str_detect("^$")
#
#   # Create a vector with only the directory names
#   version_names <- survey_data_raw_directory_filenames[is_project_names]
#
#   # Remove the processed_data folder from the list of versions
#   version_names <- version_names[version_names != "processed_data"]
#
#   # Create paths for each version directory
#   version_directories <- survey_data_raw_directory %>%
#     paste0("/", version_names)
#
#   # These directories will have all downloaded copies of the data
#   # They will have the date and time in the file name, and should be zip files.
#   # Naming structure is "{Org Initials}_{mmddyyyy}_{hhmm}.zip"
#   # The following will obtain the most recent copy of the data
#   newest_files_for_project <- version_directories %>%
#     purrr::map_chr(~ FILES_find_newest_file(.x, file_type = ".zip"))
#
#   # Create path for the column names workbook
#   version_column_names_path <- survey_data_raw_directory %>%
#     paste0("/column_names.xlsx")
#
#   # Check if the doc that contains the column names exists in the file system
#   does_the_column_names_file_exist <- version_column_names_path %>%
#     file.exists()
#
#   # If column names file exists
#   if(does_the_column_names_file_exist) {
#
#     # Obtain the sheet names from the column names doc
#     real_sheet_names <- version_column_names_path %>%
#       readxl::excel_sheets()
#
#     # Check if there is more than one sheet
#     more_than_one_sheet <-  length(real_sheet_names) > 1
#
#     # If there is more than one sheet
#     if(more_than_one_sheet) {
#
#       # Check to make sure it has a final column names sheet
#       if(!("final_column_names" %in% real_sheet_names)) {
#
#         # If it does not, Throw an error
#         stop("When more than one sheet is needed, you must have a final_column_names sheet")
#       }
#
#       # Create a list that contains the column names from all sheets
#       column_names <- real_sheet_names %>%
#         purrr::map(~{
#           version_column_names_path %>%
#             readxl::read_excel(sheet = .x) %>%
#             pull(column_names)
#         })
#
#       # Set the names for the list
#       column_names <- purrr::set_names(column_names, if_multi_sheet_names)
#
#       # If there is only one sheet
#     } else {
#
#       #  Create a vector with the column names
#       column_names <- version_column_names_path %>%
#         readxl::read_excel() %>%
#         dplyr::pull(column_names) %>%
#         list()
#     }
#
#     # If the column names file does not exist,
#   } else {
#
#     # make the column names NULL
#     column_names <- NULL
#   }
#
#   # Add title of the project
#   title_of_the_survey_monkey_project <- "Operation Smile Norming Survey"
#
#   # Indicate the file structure that is used in the zip file
#   path_inside_zip <- paste0("CSV/", title_of_the_survey_monkey_project, ".csv")
#
#   # Create the connection to the zip files
#   connection_to_zip_files <- newest_files_for_project %>%
#     purrr::map(~unz(.x, path_inside_zip))
#
#   # Read in the data
#   survey_data_list <- purrr::map2(connection_to_zip_files, column_names, ~ {
#     readr::read_csv(.x, show_col_types = FALSE, col_names = .y)
#   })
#
#   # Remove the first 2 rows
#   survey_data <- purrr::map_df(survey_data_list, ~dplyr::slice(.x, -1:-2))
#
#   # Create vector with the category names
#   image_category <- c(
#     "before_after_individuals",
#     "family_individuals",
#     "medical_individuals",
#     "mild_individuals",
#     "older_individuals",
#     "severe_individuals",
#     "younger_individuals"
#   )
#
#   # Create a vector with the questions
#   image_question <- c(
#     "sad_happy",
#     "discouraged_hopeful",
#     "forgettable_eyecatching",
#     "uncomfortable_heartwarming",
#     "unimportant_urgent"
#   )
#
#   #  Get the names of the items from the image task
#   image_item_names <- purrr::map(image_category, ~{
#     survey_data %>%
#       dplyr::select(tidyselect::starts_with(.x)) %>%
#       colnames()}) %>%
#     purrr::flatten_chr()
#
#   #  Get the items from the image task
#   items <- survey_data %>%
#     dplyr::select(tidyselect::all_of(image_item_names))
#
#   # Get the number of complete items that they have
#   number_of_complete_responses <- items %>%
#     dplyr::mutate(dplyr::across(everything(), purrr::negate(is.na))) %>%
#     rowSums()
#
#   # Keep the completed data
#   completes_survey_data <- survey_data %>%
#     dplyr::filter(number_of_complete_responses == 315)
#
#   # Filter out excluded participants
#   completes_survey_data <- completes_survey_data %>%
#     dplyr::filter(
#       country %in% c("United Kingdom", "United States of America"),
#       household_income != "$0 - $29,999",
#       charitable_giving %in% c("$50 or More", "$40 - $49", "$30 - $39", "$20 - $29")
#     )
#
#   # Format start_date as time variable
#   completes_survey_data$start_date <- lubridate::mdy_hms(completes_survey_data$start_date)
#
#   # Format end_date as time variable
#   completes_survey_data$end_date <- lubridate::mdy_hms(completes_survey_data$end_date)
#
#   # Create duration_of_study
#   completes_survey_data$duration_of_study <- completes_survey_data$end_date - completes_survey_data$start_date
#
#   ## Clean items, gender, and country
#   completes_survey_data <- completes_survey_data %>%
#     dplyr::mutate(
#       dplyr::across(tidyselect::all_of(image_item_names), as.integer),
#       gender = factor(gender, levels = c("Female", "Male")),
#       country = factor(country, levels = c("United Kingdom", "United States of America"))
#     )
#
#   # Clean age_group
#   completes_survey_data$age_group_recalc <- dplyr::case_when(
#     completes_survey_data$age_group %in% c("18 - 24", "25 - 34", "35 - 44", "45 - 54") ~ "Under 54",
#     completes_survey_data$age_group == "55 - 64" ~ "55 - 64",
#     completes_survey_data$age_group %in% c("65 - 74", "75+") ~ "Over 65") %>%
#     factor(levels = c("Under 54", "55 - 64", "Over 65"), ordered = TRUE)
#
#   # Clean ethnicity
#   completes_survey_data$ethnicity <- dplyr::case_when(
#     completes_survey_data$ethnicity == "White" ~ "White",
#     completes_survey_data$ethnicity == "Black or African American" ~ "African American",
#     completes_survey_data$ethnicity == "Hispanic, Latino, or Spanish origin" ~ "Hispanic",
#     completes_survey_data$ethnicity == "American Indian or Alaska Native" ~ "Native American",
#     completes_survey_data$ethnicity == "Asian" ~ "Asian",
#     completes_survey_data$ethnicity == "Middle Eastern or North African" ~ "Middle Eastern",
#     completes_survey_data$ethnicity == "Native Hawaiian or Other pacific Islander" ~ "Native Hawaiian",
#     completes_survey_data$ethnicity == "Other (please specify)" ~ "Other") %>%
#     factor(levels = c("Asian", "African American", "Hispanic", "Middle Eastern", "Native American", "Other", "White"),
#            ordered = FALSE)
#
#   # Clean household income
#   completes_survey_data$household_income <- factor(completes_survey_data$household_income, levels = c("$0 - $29,999", "$30,000 - $59,999", "$60,000 - $89,999", "$90,000 - $119,999", "$120,000 - $149,999", "$150,000+"), ordered = TRUE)
#
#   # Clean highest level of education
#   completes_survey_data$highest_level_of_education  <- factor(completes_survey_data$highest_level_of_education, levels = c("Some High School", "High School/GED", "Some College", "Associate???s Degree or Trade School", "Bachelor's Degree", "Graduate Degree"))
#
#   # Clean country
#   completes_survey_data$country  <- factor(completes_survey_data$country, levels = c("United Kingdom", "United States of America"))
#
#   # Clean charitable giving
#   completes_survey_data$charitable_giving <- factor(completes_survey_data$charitable_giving, levels = c("Nothing", "$1 - $9", "$10 - $19", "$20 - $29", "$30 - $39", "$40 - $49", "$50 or More"))
#
#   # Clean cable subscriber
#   completes_survey_data$cable_subscriber <- factor(completes_survey_data$cable_subscriber, levels = c("I have a cable or satellite subscription.", "I do not have a cable or satellite subscription but I do watch cable networks through streaming services like YouTubeTV.", "I do not have a cable or satellite subscription and do not watch cable networks through streaming services like YouTubeTV."))
#
#   # Use a new name for the cleaned survey monkey data frame
#   sm_df <- completes_survey_data
#
#
#   remove_rid_data_quality <- c(
#     "6462cbe0-f3ed-9158-cb09-2a3839d3c9bf",
#     "6463b3f9-572c-6cab-ac17-9f35c5b2f0cf",
#     "64627d8c-0995-2eef-c912-07bee87edeea",
#     "64627f88-a6cd-861d-ec60-282fcd1a511b",
#     "64627d02-d962-4d4d-347a-76e7d49dab9d",
#     "64627d1e-af2e-71b1-2115-430100aeaec4",
#     "64627d26-3189-0fef-37d5-6a76cb874fdb",
#     "64627d12-53e3-a586-bb20-df0332a5a98f",
#     "6463a70b-f76a-1243-b16e-247be516b4c7"
#   )
#
#   remove_rid_angry_participant <- c(
#     "64643a1e-009f-c7a8-c38c-d361e347c71c",
#     "646418c9-619b-ad37-cbf3-a4aca7c7068e",
#     "6462b44b-6346-05c1-dfb0-28a5eb1b23df",
#     "6465197b-4308-2cc4-80b2-632a156b4da9",
#     "64627cfd-a83e-9f90-7950-4fc6fe237e07",
#     "64627d0b-dea5-5b1d-223d-39ae24535497",
#     "64627e23-5065-a552-cfae-bc3d5e338a75",
#     "64627cf6-ed78-cab7-e9ec-44fd02e6b885",
#     "646281b8-2364-3fdc-ad05-cd8782e841cc",
#     "64627d20-a15b-7ebd-07a7-1b46683bce0f",
#     "64691e33-8a1a-b3a4-6a54-2aa0bdd8c02c",
#     "64674a51-4808-986a-7171-472e62eeaa11",
#     "64671a6a-f6ae-a437-fc13-08d848dc53a4",
#     "6467365e-82df-e29b-c7a3-5a67b016d09e",
#     "646e24a1-34a6-6011-5f6a-0674482e3b3b",
#     "646728a6-9280-4b67-b7ae-38b979988344",
#     "6467314d-1d11-a707-70c6-8da0628ea263",
#     "646cb64a-2e08-61e6-ced7-d1a2763c21a6"
#   )
#
#
#   sm_df <- sm_df %>%
#     dplyr::filter(!(rid %in% remove_rid_data_quality)) %>%
#     dplyr::filter(!(rid %in% remove_rid_angry_participant))
#
#
#   # Count the number of quality participants that we have
#   n_completes_quality <- nrow(sm_df)
#
#   # Labels for version
#   country_count_table <- ftable(sm_df$country)
#   country_prop_table <- round(prop.table(country_count_table) * 100, 2)
#   country_levels <- levels(sm_df$country)
#
#   # Labels for age
#   age_count_table <- ftable(sm_df$age_group_recalc)
#   age_prop_table <- round(prop.table(age_count_table) * 100, 2)
#   age_levels <- levels(sm_df$age_group_recalc)
#
#   # Labels for region
#   gender_count_table <- ftable(sm_df$gender)
#   gender_prop_table <- round(prop.table(gender_count_table) * 100, 2)
#   gender_levels <- levels(sm_df$gender)
#
#   ##--------------------------- Create Message ---------------------------------##
#   cat(paste0(
#     "We currently have ",
#     n_completes_quality,
#     "/1000 completed participants. Here are some details on the demographics and our quotas.\n
#   Country:
#   ??? ", country_levels[[1]], " - We currently have ", country_count_table[[1]], " of the 500 we will need.
#   ??? ", country_levels[[2]], " - We currently have ", country_count_table[[2]], " of the 500 we will need.
#
#   Age Group:
#   ??? ", age_levels[[1]], " - We currently have ", age_count_table[[1]], ". This will need to be 100.
#   ??? ", age_levels[[2]], " - We currently have ", age_count_table[[2]], ". This will need to be 300.
#   ??? ", age_levels[[3]], " - We currently have ", age_count_table[[3]], ". This will need to be 600.
#
#   Gender:
#   ??? ", gender_levels[[1]], " - We currently have ", gender_count_table[[1]], ". This will need to be 500.
#   ??? ", gender_levels[[2]], " - We currently have ", gender_count_table[[2]], ". This will need to be 500.
# "))
#
#   # Create counts that can be entered into our check tool
#   demographic_counts <- sm_df %>%
#     dplyr::group_by(country, gender, age_group_recalc) %>%
#     count()
#
#   # Write it to the clip board
#   clipr::write_clip(demographic_counts)
#
#   ################################################################################
#   ################################################################################
#   ################################################################################
#
#   # Function to obtain names that contain specific conditions
#   select_variable_names <- function(image_item_names, var1, var2) {
#     # Detect elements and occur for both variables
#     selected_column_indexs <- stringr::str_detect(image_item_names, var1) & stringr::str_detect(image_item_names, var2)
#     # Extract those columns
#     selected_columns <- image_item_names[selected_column_indexs]
#     # Return them
#     return(selected_columns)
#   }
#
#   rep_question <- rep(image_question, each = length(image_category))
#   rep_category <- rep(image_category, length(image_question))
#
#
#   for(i in seq_along(rep_question)){
#
#     var1 <- rep_category[[i]]
#     var2 <- rep_question[[i]]
#
#     selected_variable_names <- image_item_names %>%
#       select_variable_names(var1, var2)
#
#     name_for_average <- paste0("average_", var1, "_", var2)
#
#     sm_df <- sm_df %>%
#       mutate({{name_for_average}} := rowMeans(across(selected_variable_names)))
#   }
#
#   # Create path to where data should be saved
#   processed_data_path <- survey_data_raw_directory %>%
#     paste0(combine_file_string_with_time("/processed_data/processed_data"), ".csv")
#
#   # Write the new csv file
#   readr::write_csv(sm_df, processed_data_path)
#
#   ################################################################################
#   ################################################################################
#   ################################################################################
#   ################################################################################
#
#
#   image_folder_path <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/00 Jobs/2023/230180_OPS___Image Testing Online Study_OID1536/Assets/FINAL_STIMULI"
#   image_file_names <- list.files(image_folder_path)
#   image_file_paths <- paste0(image_folder_path, "/", image_file_names)
#
#   image_df <- data.frame(
#     image_file_names = image_file_names,
#     image_file_paths = image_file_paths)
#
#   image_df$category_name <- case_when(
#     stringr::str_detect(image_df$image_file_names, "BEFORE_AFTER") ~ "before_after_individuals",
#     stringr::str_detect(image_df$image_file_names, "FAMILY") ~ "family_individuals",
#     stringr::str_detect(image_df$image_file_names, "MEDICAL") ~ "medical_individuals",
#     stringr::str_detect(image_df$image_file_names, "MILD") ~ "mild_individuals",
#     stringr::str_detect(image_df$image_file_names, "OLDER") ~ "older_individuals",
#     stringr::str_detect(image_df$image_file_names, "SEVERE") ~ "severe_individuals",
#     stringr::str_detect(image_df$image_file_names, "YOUNGER") ~ "younger_individuals"
#   )
#
#   image_df$image_number <- case_when(
#     stringr::str_detect(image_df$image_file_names, "1") ~ "Image_1",
#     stringr::str_detect(image_df$image_file_names, "2") ~ "Image_2",
#     stringr::str_detect(image_df$image_file_names, "3") ~ "Image_3",
#     stringr::str_detect(image_df$image_file_names, "4") ~ "Image_4",
#     stringr::str_detect(image_df$image_file_names, "5") ~ "Image_5",
#     stringr::str_detect(image_df$image_file_names, "6") ~ "Image_6",
#     stringr::str_detect(image_df$image_file_names, "7") ~ "Image_7",
#     stringr::str_detect(image_df$image_file_names, "8") ~ "Image_8",
#     stringr::str_detect(image_df$image_file_names, "9") ~ "Image_9"
#   )
#
#   image_df$cat_image_name <- paste0(image_df$category_name, "_", image_df$image_number)
#
#   image_df$image_paths_inst <- paste0("<img src='", image_df$image_file_paths, "'
#     width='100' /><br>*", image_df$image_number, "*")
#
#   ################################################################################
#   get_data_for_image_analysis <- function(data, category){
#
#     image_question <- c("sad_happy", "discouraged_hopeful", "forgettable_eyecatching", "uncomfortable_heartwarming", "unimportant_urgent")
#
#     compiled_data <- purrr::map_df(image_question, ~ {
#       anova_results <- run_2_way_rm_anova(data = data, category = category, question = .x)
#       marginal_means_data <- anova_results$emm[[1]]$emmTable %>% as.data.frame()
#       marginal_means_data$image_number <- stringr::str_extract(marginal_means_data$`RM Factor 1`, "Image_[0-9]{1}")
#       marginal_means_data$category <- category
#       marginal_means_data$question <- .x
#       return(marginal_means_data)
#     })
#
#     compiled_data$question_type <- factor(compiled_data$question)
#
#     compiled_data <- as.data.frame(compiled_data)
#
#     return(compiled_data)
#   }
#
#   before_after_individuals <- get_data_for_image_analysis(data = sm_df, category = "before_after_individuals")
#   # clipr::write_clip(before_after_individuals)
#
#   family_individuals <- get_data_for_image_analysis(data = sm_df, category = "family_individuals")
#   # clipr::write_clip(family_individuals)
#
#   medical_individuals <- get_data_for_image_analysis(data = sm_df, category = "medical_individuals")
#   # clipr::write_clip(medical_individuals)
#
#   mild_individuals <- get_data_for_image_analysis(data = sm_df, category = "mild_individuals")
#   # clipr::write_clip(mild_individuals)
#
#   older_individuals <- get_data_for_image_analysis(data = sm_df, category = "older_individuals")
#   # clipr::write_clip(older_individuals)
#
#   severe_individuals <- get_data_for_image_analysis(data = sm_df, category = "severe_individuals")
#   # clipr::write_clip(severe_individuals)
#
#   younger_individuals <- get_data_for_image_analysis(data = sm_df, category = "younger_individuals")
#   # clipr::write_clip(younger_individuals)
#
#   library(openxlsx)
#   ## Create a new workbook
#   wb <- createWorkbook("OPS_Image_Study")
#
#   rep_question <- rep(image_question, each = length(image_category))
#   rep_category <- rep(image_category, length(image_question))
#
#   categorical_anova_results <- purrr::map2(rep_category, rep_question, ~ run_2_way_rm_anova(data = sm_df, category = .x, question = .y))
#
#   categorical_anova_model_results <- purrr::map_df(seq_along(categorical_anova_results), ~ {
#     categorical_anova_results_tibble_rm <- (categorical_anova_results[[.x]])[["rmTable"]] %>% tibble::as_tibble()
#     names(categorical_anova_results_tibble_rm) <- c("name", "sum_of_squares", "df", "mean_squares", "f", "p", "eta")
#
#     categorical_anova_results_tibble_rm$category <- rep_question[[.x]]
#     categorical_anova_results_tibble_rm$question <- rep_category[[.x]]
#     categorical_anova_results_tibble_rm$analysis_type <- "repeated_measures"
#
#     categorical_anova_results_tibble_bs <- (categorical_anova_results[[.x]])[["bsTable"]] %>% tibble::as_tibble()
#     names(categorical_anova_results_tibble_bs) <- c("name", "sum_of_squares", "df", "mean_squares", "f", "p", "eta")
#
#     categorical_anova_results_tibble_bs$category <- rep_question[[.x]]
#     categorical_anova_results_tibble_bs$question <- rep_category[[.x]]
#     categorical_anova_results_tibble_bs$analysis_type <- "between_subjects"
#
#     categorical_anova_model_results <- categorical_anova_results_tibble_rm %>%
#       rbind(categorical_anova_results_tibble_bs)
#
#     return(categorical_anova_model_results)
#   })
#
#   categorical_anova_model_results <- categorical_anova_model_results %>%
#     dplyr::select(c("category", "question", "analysis_type", "name", "sum_of_squares",
#                     "df", "mean_squares", "f", "p", "eta"))
#
#   path_categorical_anova_model_results_save <- paste0(dirname(dirname(survey_directory_path)), "/Analysis/survey/categorical_anova_model_results.csv")
#
#   readr::write_csv(categorical_anova_model_results, path_categorical_anova_model_results_save)
#
#   ####################################################
#   individual_image_and_country_results <- purrr::map_df(seq_along(categorical_anova_results), ~ {
#     individual_image_and_country_results_rm <- (((categorical_anova_results[[.x]])[["emm"]])[[3]])$emmTable %>% tibble::as_tibble()
#     names(individual_image_and_country_results_rm) <- c("country", "name", "mean", "se", "lower", "upper")
#
#     individual_image_and_country_results_rm$question<- rep_question[[.x]]
#     individual_image_and_country_results_rm$category  <- rep_category[[.x]]
#     individual_image_and_country_results_rm$image_number <- stringr::str_extract(individual_image_and_country_results_rm$name, "Image_[0-9]")
#     individual_image_and_country_results_rm$ci <- individual_image_and_country_results_rm$se * 1.96
#     individual_image_and_country_results_rm$ci_upper <- individual_image_and_country_results_rm$mean + individual_image_and_country_results_rm$ci
#     individual_image_and_country_results_rm$ci_lower <- individual_image_and_country_results_rm$mean - individual_image_and_country_results_rm$ci
#     individual_image_and_country_results_rm <- individual_image_and_country_results_rm %>%
#       dplyr::select(image_type =category, question_type = question, image_number, country, ave_ratings = mean, se, ci, ci_upper, ci_lower)
#
#     return(individual_image_and_country_results_rm)
#   })
#
#   individual_image_and_country_results$order_for_image_type <- dplyr::case_when(
#     individual_image_and_country_results$image_type == "before_after_individuals" ~ 1,
#     individual_image_and_country_results$image_type == "family_individuals" ~ 2,
#     individual_image_and_country_results$image_type == "medical_individuals" ~ 3,
#     individual_image_and_country_results$image_type == "mild_individuals" ~ 4,
#     individual_image_and_country_results$image_type == "older_individuals" ~ 7,
#     individual_image_and_country_results$image_type == "severe_individuals" ~ 5,
#     individual_image_and_country_results$image_type == "younger_individuals" ~ 6,
#   )
#
#   individual_image_and_country_results$image_type_type_label <- dplyr::case_when(
#     individual_image_and_country_results$image_type == "before_after_individuals" ~ "Before and After",
#     individual_image_and_country_results$image_type == "family_individuals" ~ "Family",
#     individual_image_and_country_results$image_type == "medical_individuals" ~ "Medical",
#     individual_image_and_country_results$image_type == "mild_individuals" ~ "Mild",
#     individual_image_and_country_results$image_type == "older_individuals" ~ "Older",
#     individual_image_and_country_results$image_type == "severe_individuals" ~ "Severe",
#     individual_image_and_country_results$image_type == "younger_individuals" ~ "Younger",
#   )
#
#   individual_image_and_country_results$order_for_question_type <- dplyr::case_when(
#     individual_image_and_country_results$question_type == "sad_happy" ~ 5,
#     individual_image_and_country_results$question_type == "discouraged_hopeful" ~ 4,
#     individual_image_and_country_results$question_type == "forgettable_eyecatching" ~ 3,
#     individual_image_and_country_results$question_type == "uncomfortable_heartwarming" ~ 2,
#     individual_image_and_country_results$question_type == "unimportant_urgent" ~ 1
#   )
#
#   individual_image_and_country_results$question_type_label <- dplyr::case_when(
#     individual_image_and_country_results$question_type == "sad_happy" ~ "Sad to Happy",
#     individual_image_and_country_results$question_type == "discouraged_hopeful" ~ "Discouraged to Hopeful",
#     individual_image_and_country_results$question_type == "forgettable_eyecatching" ~ "Forgettable to Eyecatching",
#     individual_image_and_country_results$question_type == "uncomfortable_heartwarming" ~ "Uncomfortable to Heartwarming",
#     individual_image_and_country_results$question_type == "unimportant_urgent" ~ "Unimportant to Urgent"
#   )
#
#   overall_question_type_rankings <- individual_image_and_country_results %>%
#     dplyr::group_by(image_type, image_number, question_type) %>%
#     dplyr::summarize(all_country_average = mean(ave_ratings, na.rm = TRUE)) %>%
#     dplyr::arrange(question_type, desc(all_country_average)) %>%
#     ungroup() %>%
#     group_by(question_type) %>%
#     mutate(overall_question_type_rankings = row_number()) %>%
#     select(-all_country_average)
#
#   individual_image_and_country_results <- individual_image_and_country_results %>%
#     left_join(overall_question_type_rankings, by = c("image_type", "image_number", "question_type"))
#
#   individual_image_and_country_results %>%
#     dplyr::group_by(country, image_type, image_number, question_type) %>%
#     dplyr::arrange(country, question_type, desc(ave_ratings)) %>%
#     ungroup() %>%
#     group_by(country, question_type) %>%
#     mutate(by_country_question_type_rankings = row_number())
#
#   # ## I do not want this to change in dropbox right now
#   # path_individual_image_and_country_results_save <- paste0(dirname(dirname(survey_directory_path)), "/Analysis/survey/individual_image_and_country_means.xlsx")
#   # writexl::write_xlsx(individual_image_and_country_results, path_individual_image_and_country_results_save)
#   # individual_image_and_country_results <- readxl::read_excel(path_individual_image_and_country_results_save)
#   #
#
#
#
#   mean_per_image_wide <- individual_image_and_country_results %>%
#     tidyr::pivot_wider(c("image_type", "image_number", "country"), names_from = question_type, values_from = ave_ratings, names_prefix = "mean_")
#
#   ci_lower_per_image_wide <- individual_image_and_country_results %>%
#     tidyr::pivot_wider(c("image_type", "image_number", "country"), names_from = question_type, values_from = ci_lower, names_prefix = "ci_lower_")
#
#   ci_upper_per_image_wide <- individual_image_and_country_results %>%
#     tidyr::pivot_wider(c("image_type", "image_number", "country"), names_from = question_type, values_from = ci_upper, names_prefix = "ci_upper_")
#
#
#   individual_image_and_country_results_wide <- mean_per_image_wide %>%
#     left_join(ci_lower_per_image_wide, by = c("image_type", "image_number", "country")) %>%
#     left_join(ci_upper_per_image_wide, by = c("image_type", "image_number", "country"))
#
#   path_individual_image_and_country_wide_save <- paste0(dirname(dirname(survey_directory_path)), "/Analysis/survey/individual_image_and_country_means_wide.xlsx")
#
#   writexl::write_xlsx(individual_image_and_country_results_wide, path_individual_image_and_country_wide_save)
#
#
#
#
#
#
#
#
#
#   library(dplyr)
#   library(tidyr)
#   library(stringr)
#
#   sm_df_long <- sm_df %>%
#     select(rid, country, ethnicity, gender, age_group_recalc, starts_with("average_")) %>%
#     pivot_longer(cols = starts_with("average_"), names_to = "names", values_to = "values")
#
#   sm_df_long$image_category <- stringr::str_extract(sm_df_long$names, ".+individuals")
#   sm_df_long$image_question_type <- stringr::str_replace(sm_df_long$names, paste0(sm_df_long$image_category, "_"), "")
#   sm_df_long$image_category <- sm_df_long$image_category %>%
#     str_replace("average_", "") %>%
#     str_replace_all("_", " ") %>%
#     str_replace(" individuals", "") %>%
#     str_to_title()
#
#
#   sm_df_long
#
#
#   sm_df_long$image_question_type <- sm_df_long$image_question_type %>% str_replace("_", " to ") %>% str_to_title()
#   participant_long_save <- paste0(dirname(dirname(survey_directory_path)), "/Analysis/survey/participant_long.xlsx")
#   writexl::write_xlsx(sm_df_long, participant_long_save)
#
#   participant_long_with_wide_q_type <- sm_df_long %>%
#     select(-names) %>%
#     pivot_wider(names_from = image_question_type, values_from = values)
#
#   participant_long_with_wide_q_type_save <- paste0(dirname(dirname(survey_directory_path)), "/Analysis/survey/participant_long_with_wide_q_type.xlsx")
#
#   writexl::write_xlsx(participant_long_with_wide_q_type, participant_long_with_wide_q_type_save)
#
#
#
#   participant_long_with_wide_i_type <- sm_df_long %>%
#     select(-names) %>%
#     pivot_wider(names_from = image_category, values_from = values)
#
#   participant_long_with_wide_i_type_save <- paste0(dirname(dirname(survey_directory_path)), "/Analysis/survey/participant_long_with_wide_i_type.xlsx")
#
#   writexl::write_xlsx(participant_long_with_wide_i_type, participant_long_with_wide_i_type_save)
#
#
#   participant_image_data <- sm_df_long %>%
#     select(-names) %>%
#     pivot_wider(names_from = image_category, values_from = values)
#
#   participant_long_with_wide_i_type_save <- paste0(dirname(dirname(survey_directory_path)), "/Analysis/survey/participant_long_with_wide_i_type.xlsx")
#
#   writexl::write_xlsx(participant_long_with_wide_i_type, participant_long_with_wide_i_type_save)
#
#
#
#
#
#   # cor_plot_data <- participant_long_with_wide_q_type %>% select(c(`Sad To Happy`, `Discouraged To Hopeful`, `Forgettable To Eyecatching`, `Uncomfortable To Heartwarming`, `Unimportant To Urgent`))
#   # install.packages("corrplot", repos = "http://cran.us.r-project.org")
#   # library(corrplot)
#   #
#   # M <- cor(cor_plot_data)
#   #
#   # #corrplot(M, method = "number", tl.cex = 0.6, tl.srt = 45, tl.col = "black", type = "upper", order = "hclust", diag = FALSE, sig.level = 0.05)
#   #
#   # testRes = cor.mtest(cor_plot_data, conf.level = 0.95)
#   # corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
#   #          order = 'AOE', diag = FALSE)$corrPos -> p1
#   # text(p1$x, p1$y, round(p1$corr, 2))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#   ################################################################################
#   for(i in seq_along(image_category)){
#     cat_name <- image_category[[i]]
#     sheet_name <- paste0("cat_", cat_name)
#
#     results <- categorical_anova_model_results %>%
#       dplyr::filter(category == cat_name)
#
#     addWorksheet(wb, {{sheet_name}})
#
#     writeData(wb, {{sheet_name}}, results, startCol = 20, startRow = 2)
#   }
#
#   all_image_means_df <- before_after_individuals %>%
#     rbind(family_individuals) %>%
#     rbind(medical_individuals) %>%
#     rbind(mild_individuals) %>%
#     rbind(older_individuals) %>%
#     rbind(severe_individuals) %>%
#     rbind(younger_individuals)
#
#   all_image_means_df$ci_limit <- all_image_means_df$se * 1.96
#
#   all_image_means_df <- all_image_means_df %>%
#     dplyr::select(category, question, image_number, mean, se, ci_limit)
#
#   path_for_all_save <- paste0(dirname(dirname(survey_directory_path)), "/Analysis/survey/all_image_means_df.csv")
#
#   readr::write_csv(all_image_means_df, path_for_all_save)
#
#   get_individual_image_mean_table <- function(data, category, image_number){
#     image_mean_table <- data %>%
#       dplyr::filter(category == {{category}}, image_number == {{image_number}}) %>%
#       dplyr::select(-c(image_number, se))
#
#     image_mean_table$question <- dplyr::case_when(
#       image_mean_table$question == "unimportant_urgent" ~ "Unimportant (Left) to Urgent (Right)",
#       image_mean_table$question == "uncomfortable_heartwarming" ~ "Uncomfortable (Left) to Heartwarming (Right)",
#       image_mean_table$question == "sad_happy" ~ "Sad (Left) to Happy (Right)",
#       image_mean_table$question == "forgettable_eyecatching" ~ "Forgettable (Left) to Eyecatching (Right)",
#       image_mean_table$question == "discouraged_hopeful" ~ "Discouraged (Left) to Hopeful (Right)"
#     )
#     return(image_mean_table)
#   }
#
#   ################################################################################
#
#
#
#   for(i in seq_along(image_df$image_file_paths)){
#     path_to_the_image <- image_df$image_file_paths[[i]]
#     sheet_name <- basename(image_df$image_file_paths[[i]]) %>% stringr::str_replace(".jpg", "")
#     category_name <- image_df$category_name[[i]]
#     image_number <- image_df$image_number[[i]]
#
#     addWorksheet(wb, {{sheet_name}})
#
#     insertImage(wb, {{sheet_name}}, path_to_the_image, startRow = 2, startCol = 2)
#
#     individual_image_mean_table <- get_individual_image_mean_table(all_image_means_df, {{category_name}}, {{image_number}}) %>% tibble::tibble()
#
#     writeData(wb, {{sheet_name}}, individual_image_mean_table, startCol = 2, startRow = 18)
#
#     p1 <- individual_image_mean_table %>%
#       ggplot2::ggplot(ggplot2::aes(x = question, y = mean, fill = question)) +
#       ggplot2::geom_col() +
#       ggplot2::geom_point() +
#       ggplot2::geom_errorbar(ggplot2::aes(ymin = (mean - ci_limit), ymax = (mean + ci_limit))) +
#       ggplot2::coord_flip() +
#       ggplot2::labs(x = "", y = "") +
#       ggthemes::theme_excel_new() +
#       ggplot2::theme(legend.position = "none") +
#       ggplot2::scale_y_continuous(limits = c(0,100), breaks = c(0, 50, 100), labels = c("Left", "Neutral", "Right"))
#
#     print(p1) # plot needs to be showing
#     insertPlot(wb, sheet_name, startCol = 2, startRow = 25, fileType = "png", units = "in")
#   }
#
#   save_path_for_workbook <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/00 Jobs/2023/230180_OPS___Image Testing Online Study_OID1536/Analysis/survey/test_workbook.xlsx"
#   saveWorkbook(wb, save_path_for_workbook, overwrite = TRUE)
#
#   ################################################################################
#
#   # library(mschart)
#   #
#   # save_path_for_pptx <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/00 Jobs/2023/230180_OPS___Image Testing Online Study_OID1536/Analysis/survey/test_workbook.pptx"
#   #
#   # doc <- read_pptx()
#   #
#   # for(i in seq_along(image_df$image_file_paths)){
#   #   path_to_the_image <- image_df$image_file_paths[[i]]
#   #   sheet_name <- basename(image_df$image_file_paths[[i]]) %>% stringr::str_replace(".jpg", "")
#   #   category_name <- image_df$category_name[[i]]
#   #   image_number <- image_df$image_number[[i]]
#   #
#   #   individual_image_mean_table <- get_individual_image_mean_table(all_image_means_df, {{category_name}}, {{image_number}}) %>% tibble::tibble()
#   #
#   #   my_barchart <- ms_barchart(data = individual_image_mean_table,
#   #                              x = "question", y = "mean")
#   #   chart_labels(x = my_barchart, title = sheet_name)
#   #   mschart_theme()
#   #   doc <- ?add_slide(doc, layout = "Title and Content", master = "Office Theme")
#   #   doc <- ph_with(doc, value = my_barchart, location = ph_location_fullsize())
#   #
#   #
#   #   doc <- ph_with(doc, value = external_img(path_to_the_image), location = ph_location_fullsize())
#   #
#   #
#   #   print(doc, target = save_path_for_pptx)
#   ################################################################################
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#   get_data_for_image_analysis_plot <- function(data, category, question){
#     anova_results <- run_2_way_rm_anova(data = data, category = category, question = question)
#
#     anova_results %>% names()
#
#     anova_results$rmTable
#
#
#
#
#
#     selected_image_df <- image_df %>%
#       dplyr::filter(category_name == category)
#
#     selected_image_df %>% dplyr::filter(image_number == "Image_1") %>% dplyr::pull(image_paths_inst)
#
#     labels <- c(
#       Image_1 = selected_image_df %>% dplyr::filter(image_number == "Image_1") %>% dplyr::pull(image_paths_inst),
#       Image_2 = selected_image_df %>% dplyr::filter(image_number == "Image_2") %>% dplyr::pull(image_paths_inst),
#       Image_3 = selected_image_df %>% dplyr::filter(image_number == "Image_3") %>% dplyr::pull(image_paths_inst),
#       Image_4 = selected_image_df %>% dplyr::filter(image_number == "Image_4") %>% dplyr::pull(image_paths_inst),
#       Image_5 = selected_image_df %>% dplyr::filter(image_number == "Image_5") %>% dplyr::pull(image_paths_inst),
#       Image_6 = selected_image_df %>% dplyr::filter(image_number == "Image_6") %>% dplyr::pull(image_paths_inst),
#       Image_7 = selected_image_df %>% dplyr::filter(image_number == "Image_7") %>% dplyr::pull(image_paths_inst),
#       Image_8 = selected_image_df %>% dplyr::filter(image_number == "Image_8") %>% dplyr::pull(image_paths_inst),
#       Image_9 = selected_image_df %>% dplyr::filter(image_number == "Image_9") %>% dplyr::pull(image_paths_inst)
#     )
#
#     low_q_label <- stringr::str_replace(question, "_.+$", "") %>% stringr::str_to_title()
#     high_q_label <- stringr::str_replace(question, "^.+_", "") %>% stringr::str_to_title()
#
#     marginal_means_data <- anova_results$emm[[1]]$emmTable %>% as.data.frame()
#
#     marginal_means_data$image_number <- stringr::str_extract(marginal_means_data$`RM Factor 1`, "Image_[0-9]{1}")
#
#     final_plot <- marginal_means_data %>%
#       ggplot2::ggplot(ggplot2::aes(x = image_number, y = mean)) +
#       ggplot2::geom_point() +
#       ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper)) +
#       ggplot2::geom_hline(yintercept = 50) +
#       ggplot2::labs(x = "", y = "") +
#       ggplot2::theme_minimal() +
#       ggplot2::scale_y_continuous(limits = c(0,100), breaks = c(0, 50, 100), labels = c(low_q_label, "Neutral", high_q_label)) +
#       scale_x_discrete(name = NULL, labels = labels) +
#       theme(
#         axis.text.x = element_markdown(color = "black", size = 11)
#       )
#     print(final_plot)
#   }
#
#
#
#   purrr::map(image_question, ~get_data_for_image_analysis_plot(data = sm_df, category = "before_after_individuals", question = .x))
#   purrr::map(image_question, ~get_data_for_image_analysis_plot(data = sm_df, category = "family_individuals", question = .x))
#   purrr::map(image_question, ~get_data_for_image_analysis_plot(data = sm_df, category = "medical_individuals", question = .x))
#   purrr::map(image_question, ~get_data_for_image_analysis_plot(data = sm_df, category = "mild_individuals", question = .x))
#   purrr::map(image_question, ~get_data_for_image_analysis_plot(data = sm_df, category = "older_individuals", question = .x))
#   purrr::map(image_question, ~get_data_for_image_analysis_plot(data = sm_df, category = "severe_individuals", question = .x))
#   purrr::map(image_question, ~get_data_for_image_analysis_plot(data = sm_df, category = "younger_individuals", question = .x))
#
#
#
#
#
#   purrr::map(image_question, ~get_data_for_image_analysis_plot(data = sm_df, category = "before_after_individuals", question = "sad_happy"))
#
#
#
#
#
#   # get_data_for_image_analysis <- function(data, category){
#   #
#   #   image_question <- c("sad_happy", "discouraged_hopeful", "forgettable_eyecatching", "uncomfortable_heartwarming", "unimportant_urgent")
#   #
#   #   compiled_data <- purrr::map_df(image_question, ~ {
#   #     anova_results <- run_2_way_rm_anova(data = data, category = category, question = .x)
#   #     marginal_means_data <- anova_results$emm[[1]]$emmTable %>% as.data.frame()
#   #     marginal_means_data$image_number <- stringr::str_extract(marginal_means_data$`RM Factor 1`, "Image_[0-9]{1}")
#   #     marginal_means_data$category <- category
#   #     marginal_means_data$question <- .x
#   #     return(marginal_means_data)
#   #   })
#   #
#   #   compiled_data$question_type <- factor(compiled_data$question)
#   #
#   #   compiled_data <- as.data.frame(compiled_data)
#   #
#   #   return(compiled_data)
#   # }
#   #
#   # before_after_individuals <- get_data_for_image_analysis(data = sm_df, category = "before_after_individuals")
#   # # clipr::write_clip(before_after_individuals)
#   #
#   # family_individuals <- get_data_for_image_analysis(data = sm_df, category = "family_individuals")
#   # # clipr::write_clip(family_individuals)
#   #
#   # medical_individuals <- get_data_for_image_analysis(data = sm_df, category = "medical_individuals")
#   # # clipr::write_clip(medical_individuals)
#   #
#   # mild_individuals <- get_data_for_image_analysis(data = sm_df, category = "mild_individuals")
#   # # clipr::write_clip(mild_individuals)
#   #
#   # older_individuals <- get_data_for_image_analysis(data = sm_df, category = "older_individuals")
#   # # clipr::write_clip(older_individuals)
#   #
#   # severe_individuals <- get_data_for_image_analysis(data = sm_df, category = "severe_individuals")
#   # # clipr::write_clip(severe_individuals)
#   #
#   # younger_individuals <- get_data_for_image_analysis(data = sm_df, category = "younger_individuals")
#   # # clipr::write_clip(younger_individuals)
#   #
#   # all_image_means_df <- before_after_individuals %>%
#   #   rbind(family_individuals) %>%
#   #   rbind(medical_individuals) %>%
#   #   rbind(mild_individuals) %>%
#   #   rbind(older_individuals) %>%
#   #   rbind(severe_individuals) %>%
#   #   rbind(younger_individuals)
#   #
#   # all_image_means_df$ci_limit <- all_image_means_df$se * 1.96
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#   library(jpeg)
#
#
#
#
#   source <- image_file_paths[[1]]
#
#   img <- readJPEG(source, native = FALSE)
#   labels <- c(virginica = "<img src='test.png' width='100' /><br>*virginica*")
#
#   #
#   # library(ggtext)
#
#   # lincoln <- readPNG("test.png") # replace with whatever
#   # labels <- c(virginica = "<img src='test.png' width='100' /><br>*virginica*") # replace with whatever
#   #
#   #
#   # df <- iris %>%
#   #   filter(Species == "virginica")
#   #
#   # ggplot(df, aes(Species, Sepal.Length)) +
#   #   geom_col() +
#   #   scale_x_discrete(labels = labels) +
#   #   theme(axis.text.x = ggtext::element_markdown())
#   image_path <- paste0("<img src='", source, "'
#     width='100' /><br>*I. setosa*")
#
#   labels <- c(
#     setosa = labels[[1]],
#     virginica = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Iris_virginica_-_NRCS.jpg/320px-Iris_virginica_-_NRCS.jpg'
#     width='100' /><br>*I. virginica*",
#     versicolor = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/20140427Iris_versicolor1.jpg/320px-20140427Iris_versicolor1.jpg'
#     width='100' /><br>*I. versicolor*"
#   )
#
#   ggplot(iris, aes(Species, Sepal.Width)) +
#     geom_boxplot() +
#     scale_x_discrete(
#       name = NULL,
#       labels = labels
#     ) +
#     theme(
#       axis.text.x = element_markdown(color = "black", size = 11)
#     )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#   library(dplyr)
#   library(ggplot2)
#   library(tidyr)
#
#   # item_names <- sm_df %>%
#   #   select(starts_with("before_after_individuals_"),
#   #          starts_with("family_individuals_"),
#   #          starts_with("medical_individuals_"),
#   #          starts_with("mild_individuals_"),
#   #          starts_with("older_individuals_"),
#   #          starts_with("severe_individuals_"),
#   #          starts_with("younger_individuals_")
#   #          ) %>%
#   #   colnames()
#
#   # sm_df_long <- sm_df %>%
#   #   pivot_longer(
#   #     cols = item_names,
#   #     names_to = "image_type",
#   #     values_to = "rating"
#   #     )
#   #
#   # sm_df_long$image_category <- stringr::str_extract(sm_df_long$image_type, ".+individuals")
#   #
#   # sm_df_long$image_question_type <- stringr::str_replace(sm_df_long$image_type, paste0(sm_df_long$image_category, "_[0-9]_"), "")
#   #
#   # sm_df_long$image_name <- stringr::str_extract(sm_df_long$image_type, paste0(sm_df_long$image_category, "_[0-9]"))
#   #
#   # sm_df_long <- sm_df_long %>%
#   #   select(rid, image_category, image_question_type, image_name, rating)
#
#
#   sm_df_long <- sm_df %>%
#     tidyr::pivot_longer(
#       cols = tidyselect::all_of(image_item_names),
#       names_to = "image_type",
#       values_to = "rating"
#     )
#
#   sm_df_long$image_category <- stringr::str_extract(sm_df_long$image_type, ".+individuals")
#
#   sm_df_long$image_question_type <- stringr::str_replace(sm_df_long$image_type, paste0(sm_df_long$image_category, "_[0-9]_"), "")
#
#   sm_df_long$image_name <- stringr::str_extract(sm_df_long$image_type, paste0(sm_df_long$image_category, "_[0-9]"))
#
#
#
#
#   summarized_image_ratings <- sm_df_long %>%
#     group_by(image_category, image_question_type) %>%
#     summarize(mean = mean(rating, na.rm = TRUE), sd = sd(rating, na.rm = TRUE), n = n(), se = sd / sqrt(n), ci = se * 1.96)
#
#   summarized_image_ratings %>%
#     ggplot(aes(x=image_category, y = mean, fill = image_question_type)) +
#     geom_col(position = "dodge") +
#     coord_flip() +
#     geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci)) +
#     scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,5)) +
#     facet_wrap(~image_question_type) +
#     ggthemes::theme_gdocs() +
#     theme(legend.position = "none")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#   ## Use this code to create the variable names
#   # emotion_items <- c("sad_happy", "discouraged_hopeful", "forgettable_eyecatching","uncomfortable_heartwarming", "unimportant_urgent")
#   # condition_groupings <- c("younger_individuals", "severe_individuals", "older_individuals", "mild_individuals", "medical_individuals", "family_individuals", "before_after_individuals")
#   # image_name <- paste0(rep(condition_groupings, each = 9), "_", rep(1:9, 7))
#   # image_variable_names <- paste0(rep(image_name, each = 5), "_", emotion_items)
#   # clipr::write_clip(image_variable_names)
#
#
#
#   create_category_anova <- function(data, question) {
#     jmv::anovaRM(
#       data = data,
#       rm = list(
#         list(
#           label="RM Factor 1",
#           levels=c(
#             paste0("Before_After_", question),
#             paste0("Families_", question),
#             paste0("Medical_", question),
#             paste0("Mild_", question),
#             paste0("Older_", question),
#             paste0("Severe_", question),
#             paste0("Younger_", question)))),
#       rmCells = list(
#         list(
#           measure=paste0("average_before_after_individuals_", question),
#           cell=paste0("Before_After_", question)),
#         list(
#           measure=paste0("average_family_individuals_", question),
#           cell=paste0("Families_", question)),
#         list(
#           measure=paste0("average_medical_individuals_", question),
#           cell=paste0("Medical_", question)),
#         list(
#           measure=paste0("average_mild_individuals_", question),
#           cell=paste0("Mild_", question)),
#         list(
#           measure=paste0("average_older_individuals_", question),
#           cell=paste0("Older_", question)),
#         list(
#           measure=paste0("average_severe_individuals_", question),
#           cell=paste0("Severe_", question)),
#         list(
#           measure=paste0("average_younger_individuals_", question),
#           cell=paste0("Younger_", question))),
#       bs = country,
#       effectSize = "eta",
#       rmTerms = ~ `RM Factor 1`,
#       bsTerms = ~ country,
#       postHoc = list(
#         "RM Factor 1",
#         "country",
#         c("RM Factor 1", "country")),
#       emMeans = ~ `RM Factor 1`:country,
#       emmTables = TRUE)
#   }
#
#
#   create_category_anova(data = sm_df, question = "sad_happy")
#
#
#
#
#   ## sad_happy
#   jmv::anovaRM(
#     data = sm_df,
#     rm = list(
#       list(
#         label="RM Factor 1",
#         levels=c(
#           "Before_After",
#           "Families",
#           "Medical",
#           "Mild",
#           "Older",
#           "Severe",
#           "Younger"))),
#     rmCells = list(
#       list(
#         measure="average_before_after_individuals_sad_happy",
#         cell="Before_After"),
#       list(
#         measure="average_family_individuals_sad_happy",
#         cell="Families"),
#       list(
#         measure="average_medical_individuals_sad_happy",
#         cell="Medical"),
#       list(
#         measure="average_mild_individuals_sad_happy",
#         cell="Mild"),
#       list(
#         measure="average_older_individuals_sad_happy",
#         cell="Older"),
#       list(
#         measure="average_severe_individuals_sad_happy",
#         cell="Severe"),
#       list(
#         measure="average_younger_individuals_sad_happy",
#         cell="Younger")),
#     bs = country,
#     effectSize = "eta",
#     rmTerms = ~ `RM Factor 1`,
#     bsTerms = ~ country,
#     postHoc = list(
#       "RM Factor 1",
#       "country",
#       c("RM Factor 1", "country")),
#     emMeans = ~ `RM Factor 1`:country,
#     emmTables = TRUE)
#
#
#
#   ## discouraged_hopeful
#   jmv::anovaRM(
#     data = sm_df,
#     rm = list(
#       list(
#         label="RM Factor 1",
#         levels=c(
#           "Before_After",
#           "Families",
#           "Medical",
#           "Mild",
#           "Older",
#           "Severe",
#           "Younger"))),
#     rmCells = list(
#       list(
#         measure="average_before_after_individuals_discouraged_hopeful",
#         cell="Before_After"),
#       list(
#         measure="average_family_individuals_discouraged_hopeful",
#         cell="Families"),
#       list(
#         measure="average_medical_individuals_discouraged_hopeful",
#         cell="Medical"),
#       list(
#         measure="average_mild_individuals_discouraged_hopeful",
#         cell="Mild"),
#       list(
#         measure="average_older_individuals_discouraged_hopeful",
#         cell="Older"),
#       list(
#         measure="average_severe_individuals_discouraged_hopeful",
#         cell="Severe"),
#       list(
#         measure="average_younger_individuals_discouraged_hopeful",
#         cell="Younger")),
#     bs = country,
#     effectSize = "eta",
#     rmTerms = ~ `RM Factor 1`,
#     bsTerms = ~ country,
#     postHoc = list(
#       "RM Factor 1",
#       "country",
#       c("RM Factor 1", "country")),
#     emMeans = ~ `RM Factor 1`:country,
#     emmTables = TRUE)
#
#   ## Forgettable_eyecatching
#   jmv::anovaRM(
#     data = sm_df,
#     rm = list(
#       list(
#         label="RM Factor 1",
#         levels=c(
#           "Before_After",
#           "Families",
#           "Medical",
#           "Mild",
#           "Older",
#           "Severe",
#           "Younger"))),
#     rmCells = list(
#       list(
#         measure="average_before_after_individuals_forgettable_eyecatching",
#         cell="Before_After"),
#       list(
#         measure="average_family_individuals_forgettable_eyecatching",
#         cell="Families"),
#       list(
#         measure="average_medical_individuals_forgettable_eyecatching",
#         cell="Medical"),
#       list(
#         measure="average_mild_individuals_forgettable_eyecatching",
#         cell="Mild"),
#       list(
#         measure="average_older_individuals_forgettable_eyecatching",
#         cell="Older"),
#       list(
#         measure="average_severe_individuals_forgettable_eyecatching",
#         cell="Severe"),
#       list(
#         measure="average_younger_individuals_forgettable_eyecatching",
#         cell="Younger")),
#     bs = country,
#     effectSize = "eta",
#     rmTerms = ~ `RM Factor 1`,
#     bsTerms = ~ country,
#     postHoc = list(
#       "RM Factor 1",
#       "country",
#       c("RM Factor 1", "country")),
#     emMeans = ~ `RM Factor 1`:country,
#     emmTables = TRUE)
#
#
#
#   ## uncomfortable_heartwarming
#   jmv::anovaRM(
#     data = sm_df,
#     rm = list(
#       list(
#         label="RM Factor 1",
#         levels=c(
#           "Before_After",
#           "Families",
#           "Medical",
#           "Mild",
#           "Older",
#           "Severe",
#           "Younger"))),
#     rmCells = list(
#       list(
#         measure="average_before_after_individuals_uncomfortable_heartwarming",
#         cell="Before_After"),
#       list(
#         measure="average_family_individuals_uncomfortable_heartwarming",
#         cell="Families"),
#       list(
#         measure="average_medical_individuals_uncomfortable_heartwarming",
#         cell="Medical"),
#       list(
#         measure="average_mild_individuals_uncomfortable_heartwarming",
#         cell="Mild"),
#       list(
#         measure="average_older_individuals_uncomfortable_heartwarming",
#         cell="Older"),
#       list(
#         measure="average_severe_individuals_uncomfortable_heartwarming",
#         cell="Severe"),
#       list(
#         measure="average_younger_individuals_uncomfortable_heartwarming",
#         cell="Younger")),
#     bs = country,
#     effectSize = "eta",
#     rmTerms = ~ `RM Factor 1`,
#     bsTerms = ~ country,
#     postHoc = list(
#       "RM Factor 1",
#       "country",
#       c("RM Factor 1", "country")),
#     emMeans = ~ `RM Factor 1`:country,
#     emmTables = TRUE)
#
#
#
#   ## unimportant_urgent
#   jmv::anovaRM(
#     data = sm_df,
#     rm = list(
#       list(
#         label="RM Factor 1",
#         levels=c(
#           "Before_After",
#           "Families",
#           "Medical",
#           "Mild",
#           "Older",
#           "Severe",
#           "Younger"))),
#     rmCells = list(
#       list(
#         measure="average_before_after_individuals_unimportant_urgent",
#         cell="Before_After"),
#       list(
#         measure="average_family_individuals_unimportant_urgent",
#         cell="Families"),
#       list(
#         measure="average_medical_individuals_unimportant_urgent",
#         cell="Medical"),
#       list(
#         measure="average_mild_individuals_unimportant_urgent",
#         cell="Mild"),
#       list(
#         measure="average_older_individuals_unimportant_urgent",
#         cell="Older"),
#       list(
#         measure="average_severe_individuals_unimportant_urgent",
#         cell="Severe"),
#       list(
#         measure="average_younger_individuals_unimportant_urgent",
#         cell="Younger")),
#     bs = country,
#     effectSize = "eta",
#     rmTerms = ~ `RM Factor 1`,
#     bsTerms = ~ country,
#     postHoc = list(
#       "RM Factor 1",
#       "country",
#       c("RM Factor 1", "country")),
#     emMeans = ~ `RM Factor 1`:country,
#     emmTables = TRUE)
#
#
#
#
#
#
#
#
#
#
#
#
#   run_2_way_rm_anova <- function(data, category, question) {
#     jmv::anovaRM(
#       data = data,
#       rm = list(
#         list(
#           label="RM Factor 1",
#           levels=c(
#             paste0("Image_1_", question),
#             paste0("Image_2_", question),
#             paste0("Image_3_", question),
#             paste0("Image_4_", question),
#             paste0("Image_5_", question),
#             paste0("Image_6_", question),
#             paste0("Image_7_", question),
#             paste0("Image_8_", question),
#             paste0("Image_9_", question)))),
#       rmCells = list(
#         list(
#           measure= paste0(category, "_1_", question),
#           cell=paste0("Image_1_", question)),
#         list(
#           measure= paste0(category, "_2_", question),
#           cell=paste0("Image_2_", question)),
#         list(
#           measure= paste0(category, "_3_", question),
#           cell=paste0("Image_3_", question)),
#         list(
#           measure= paste0(category, "_4_", question),
#           cell=paste0("Image_4_", question)),
#         list(
#           measure= paste0(category, "_5_", question),
#           cell=paste0("Image_5_", question)),
#         list(
#           measure= paste0(category, "_6_", question),
#           cell=paste0("Image_6_", question)),
#         list(
#           measure= paste0(category, "_7_", question),
#           cell=paste0("Image_7_", question)),
#         list(
#           measure= paste0(category, "_8_", question),
#           cell=paste0("Image_8_", question)),
#         list(
#           measure= paste0(category, "_9_", question),
#           cell=paste0("Image_9_", question))),
#       bs = country,
#       effectSize = "eta",
#       rmTerms = ~ `RM Factor 1`,
#       bsTerms = ~ country,
#       spherTests = TRUE,
#       leveneTest = TRUE,
#       qq = TRUE,
#       postHoc = list(
#         "RM Factor 1",
#         "country",
#         c("RM Factor 1", "country")),
#       emMeans = ~ `RM Factor 1` + country + `RM Factor 1`:country,
#       emmTables = TRUE,
#       groupSumm = TRUE)
#   }
#
#
#   purrr::map(image_question, ~run_2_way_rm_anova(data = sm_df, category = "before_after_individuals", question = .x))
#   purrr::map(image_question, ~run_2_way_rm_anova(data = sm_df, category = "family_individuals", question = .x))
#   purrr::map(image_question, ~run_2_way_rm_anova(data = sm_df, category = "medical_individuals", question = .x))
#   purrr::map(image_question, ~run_2_way_rm_anova(data = sm_df, category = "mild_individuals", question = .x))
#   purrr::map(image_question, ~run_2_way_rm_anova(data = sm_df, category = "older_individuals", question = .x))
#   purrr::map(image_question, ~run_2_way_rm_anova(data = sm_df, category = "severe_individuals", question = .x))
#   purrr::map(image_question, ~run_2_way_rm_anova(data = sm_df, category = "younger_individuals", question = .x))
#
#
#
#
#
#
#
#
#   # sm_df_long <- sm_df_long %>%
#   #   select(rid, image_category, image_question_type, image_name, rating)
#   # image_item_names
#   # sm_df_long$image_and_q_name <- paste0(sm_df_long$image_category, "_", sm_df_long$image_question_type)
#   # sm_df_long %>%
#   #   group_by(rid, image_and_q_name, image_category) %>%
#   #   summarize(average_cat_score = mean(rating, na.rm = TRUE), groups = "drop") %>%
#   #   pivot_wider(names_from = "image_and_q_name", values_from = "average_cat_score") %>%
#   #   pull(average_cat_score)
#   # image_question
#
#
#
#
#
#   #
#   # get_data_for_image_analysis_group <- function(data, category = "younger_individuals", grouping_var){
#   #
#   #   image_question <- c("sad_happy", "discouraged_hopeful", "forgettable_eyecatching", "uncomfortable_heartwarming", "unimportant_urgent")
#   #
#   #   compiled_data <- purrr::map_df(image_question, ~ {
#   #     anova_results <- run_2_way_rm_anova(data = data, category = category, question = .x)
#   #     marginal_means_data <- anova_results$emm[[1]]$emmTable %>% as.data.frame()
#   #     marginal_means_data$image_number <- stringr::str_extract(marginal_means_data$`RM Factor 1`, "Image_[0-9]{1}")
#   #     marginal_means_data$category <- category
#   #     marginal_means_data$question <- .x
#   #     return(marginal_means_data)
#   #   })
#   #
#   #   compiled_data$question_type <- factor(compiled_data$question)
#   #
#   #   compiled_data <- as.data.frame(compiled_data)
#   #
#   #   return(compiled_data)
#   # }
#   #
#   # get_data_for_image_analysis_group(data = sm_df, category = "younger_individuals", grouping_var = "country")
#
#
#
#
#
#   #
#   # for(i in seq_along(image_df$image_file_paths)){
#   #   path_to_the_image <- image_df$image_file_paths[[i]]
#   #   sheet_name <- basename(image_df$image_file_paths[[i]]) %>% stringr::str_replace(".jpg", "")
#   #   category_name <- image_df$category_name[[i]]
#   #   image_number <- image_df$image_number[[i]]
#   #
#   #   individual_image_mean_table <- get_individual_image_mean_table(all_image_means_df, {{category_name}}, {{image_number}}) %>% tibble::tibble()
#   #
#   #
#   #
#   #   p1 <- individual_image_mean_table %>%
#   #     ggplot2::ggplot(ggplot2::aes(x = question, y = mean, fill = question)) +
#   #     ggplot2::geom_col() +
#   #     ggplot2::geom_point() +
#   #     ggplot2::geom_errorbar(ggplot2::aes(ymin = (mean - ci_limit), ymax = (mean + ci_limit))) +
#   #     ggplot2::coord_flip() +
#   #     ggplot2::labs(x = "", y = "") +
#   #     ggthemes::theme_excel_new() +
#   #     ggplot2::theme(legend.position = "none") +
#   #     ggplot2::scale_y_continuous(limits = c(0,100), breaks = c(0, 50, 100), labels = c("Left", "Neutral", "Right"))
#   #
#   #   print(p1) # plot needs to be showing
#   #   # insertPlot(wb, sheet_name, startCol = 2, startRow = 25, fileType = "png", units = "in")
#   #   library(rvg)
#   #   library(ggplot2)
#   #   library(officer)
#   #   p1 <- iris %>%
#   #     ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
#   #     geom_point()
#   #   doc <- read_xlsx()
#   #   doc <- xl_add_vg(doc, sheet = "BEFORE_AFTER_1", code =  p1,
#   #                         width = 6, height = 6, left = 1, top = 2 )
#   #   print(doc, target = save_path_for_workbook)
#   #   library(officer)
#   #   my_ws <- read_xlsx(save_path_for_workbook)
#   #   my_ws <- xl_add_vg(my_ws,
#   #                      sheet = "Feuil1",
#   #                      code = print(p1), width = 6, height = 6, left = 2, top = 2
#   #   )
#   #   fileout <- save_path_for_workbook
#   #   print(my_ws, target = fileout)
#   # }
#
#
#
#
#   # barplot(individual_image_mean_table$mean ~ individual_image_mean_table$question)
#
#
#   #
#   # p1 <- individual_image_mean_table %>%
#   #   ggplot2::ggplot(ggplot2::aes(x = question, y = mean, fill = question)) +
#   #   ggplot2::geom_col() +
#   #   ggplot2::geom_point() +
#   #   ggplot2::geom_errorbar(ggplot2::aes(ymin = (mean - ci_limit), ymax = (mean + ci_limit))) +
#   #   ggplot2::coord_flip() +
#   #   ggplot2::labs(x = "", y = "") +
#   #   ggthemes::theme_excel_new() +
#   #   ggplot2::theme(legend.position = "none") +
#   #   ggplot2::scale_y_continuous(limits = c(0,100), breaks = c(0, 50, 100), labels = c("Left", "Neutral", "Right"))
#   #
#   # print(p1) # plot needs to be showing
#   # insertPlot(wb, sheet_name, startCol = 2, startRow = 25, fileType = "png", units = "in")
#
# }
