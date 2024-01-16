currentwd <- getwd()
setwd("C:/Users/Brian/TCM Dropbox/Brian Kissell/04 MDM Neuro-Fundraising Lab/Research and Development/00 Jobs/2024/003_RD_CodingVideoContent")
qualitative_coding_path <- "Qualitative Coding/Version 2"
survey_monkey_raw_data_path <- paste0(qualitative_coding_path, "/global_variables/raw_data")
# bkissell::read_survey_data(survey_monkey_raw_data_path)

# Provide all of the names to the coding worksheets
# names_of_all_video_coding_docs <- c("Video_Coding__Ben.xlsx","Video_Coding__Brian.xlsx", "Video_Coding__Jill.xlsx", "Video_Coding__Sarah.xlsx", "Video_Coding__Talia.xlsx")
names_of_all_video_coding_docs <- paste0("Video_Coding_V2__", c("Ben","Brian", "Jill", "Sarah", "Talia"), ".xlsx")

# Create the paths for the copding documents
video_coding_docs_file_paths <- paste0(qualitative_coding_path, "/", names_of_all_video_coding_docs)

text_names = c("video_name", "section", "visual_type",
               "phone_and_url_present", "type_of_text_on_screen",
               "story_chapter", "global_variables_have_been_entered",
               "notes")

numeric_names = c("time_point", "direct_eye_contact_with_camera_for_any_animals_or_people",
                  "visual_type",
                  "qr_code_present", "logo_present",
                  "lower_third_present", "credit_card_symbols_present",
                  "trust_indicator_present", "donor_directed_language")

multiple_choice_variables <- c("visual_type", "phone_and_url_present", "type_of_text_on_screen",
  "story_chapter", "global_variables_have_been_entered")

category_variables <- c("direct_eye_contact_with_camera_for_any_animals_or_people",
  "qr_code_present", "logo_present", "lower_third_present",
  "credit_card_symbols_present", "trust_indicator_present",
  "donor_directed_language")

sheet_coding_data <- bkissell::process_video_data(
  video_coding_docs_file_paths, text_names, numeric_names, multiple_choice_variables, category_variables)

setwd(currentwd)


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
