# BRIAN_CODE_CLEAN_TRANSCRIPTS <- function() {
#   # Import packages
#   library(officer)
#   library(purrr)
#   library(dplyr)
#   library(stringr)
#
#   # Create the file paths for each transcript
#   transcript_dir_path <- bktools::FILES_obtain_home_dir_for_current_machine("C:/Users/benja/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Transcript Analysis/Transcriptions")
#   transcription_files <- list.files(transcript_dir_path, ".docx")
#   transcript_file_path <- paste0(transcript_dir_path, "/", transcription_files)
#
#   # Map applies the code I provided in the brackets to each file
#   transcripts_df <- purrr::map_df(transcript_file_path, ~{
#     # Read the word document
#     df_transcript_raw <- officer::read_docx(.x)
#     # Put the data into a dataframe
#     df_transcript <- docx_summary(df_transcript_raw)
#     # Get the file name
#     file_name <- basename(as.character(.x)) %>%
#       # Remove the.docx
#       stringr::str_replace_all(".docx", "") %>%
#       # Remove "_"
#       stringr::str_replace_all("_", ":")
#     # Add the file name to the data frame so we know what video it belong to.
#     df_transcript$file_name <- file_name
#     # Return the data frame
#     return(df_transcript)
#   })
#
#   # The data was in an odd format that needed a bunch of cleaning.
#   # Depending on differing characteristics of the text, create a label
#   transcripts_df$label <- case_when(
#     str_detect(transcripts_df$text, "[0-9]{2};[0-9]{2};[0-9]{2};[0-9]{2}") ~ "TIMESTAMP",
#     str_detect(transcripts_df$text, "TCM PROJECT #") ~ "TCM_PROJECT_NUMBER",
#     str_detect(transcripts_df$text, "CLIENT:") ~ "CLIENT_NAME",
#     str_detect(transcripts_df$text, "PROJECT TITLE:") ~ "PROJECT_TITLE",
#     str_detect(transcripts_df$text, "TRANSCRIPT DATE:") ~ "TRANSCRIPT_DATE",
#     is.na(transcripts_df$text) | transcripts_df$text == "" | transcripts_df$text == "Unknown" | str_detect(transcripts_df$text, "___") | str_detect(transcripts_df$text, "# END OF SCRIPT #") ~ "EMPTY",
#     !is.na(transcripts_df$text) ~ "TEXT"
#   )
#
#   # Adjust the shape of the dataframe (putting it into a tidy format)
#   transcripts_df <- transcripts_df %>%
#     select(file_name, doc_index, label, text) %>%
#     dplyr::filter(label != "EMPTY") %>%
#     tidyr::pivot_wider(names_from = label, values_from = text) %>%
#     tidyr::fill(
#       c(TCM_PROJECT_NUMBER,
#         CLIENT_NAME,
#         PROJECT_TITLE,
#         TRANSCRIPT_DATE,
#         TIMESTAMP),
#       .direction = "downup") %>%
#     dplyr::filter(!is.na(TEXT))
#
#   # Clean up the values
#   transcripts_df$TCM_PROJECT_NUMBER <- stringr::str_replace(transcripts_df$TCM_PROJECT_NUMBER, "TCM PROJECT #:", "")
#   transcripts_df$CLIENT_NAME <- stringr::str_replace(transcripts_df$CLIENT_NAME, "CLIENT:", "")
#   transcripts_df$PROJECT_TITLE <- stringr::str_replace(transcripts_df$PROJECT_TITLE, "PROJECT TITLE:", "")
#   transcripts_df$TRANSCRIPT_DATE <- stringr::str_replace(transcripts_df$TRANSCRIPT_DATE, "TRANSCRIPT DATE:", "")
#
#   # Excel was reading the text in a weird way, due to a couple elements,
#   # and thus this code removes the thing that was causing the problem
#   transcripts_df$TEXT <- stringr::str_replace(transcripts_df$TEXT, "^-", "")
#   transcripts_df$TEXT <- stringr::str_replace(transcripts_df$TEXT, "^[ ]{1,4}", "")
#
#   # Decide where the file should go
#   path_to_cleaned_transcripts <- paste0(dirname(transcript_dir_path), "/CLEANED_TRANSCRIPTS.csv")
#
#   # Write the file to dropbox.
#   readr::write_csv(transcripts_df, path_to_cleaned_transcripts)
#
#
# }
#
# Transcipt_Analysis_Part2_BK <- function() {
#   library(dplyr)
#   library(stringr)
#   library(tidyr)
#   library(readr)
#   library(tidytext)
#   library(tidyverse)
#
#   # Read in cleaned transcript data
#   data <- readr::read_csv("C:/users/benja/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/Transcript Analysis/Transcript_Analysis_Export.csv", show_col_types = FALSE)
#   # data <- readr::read_csv("C:/users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/Transcript Analysis/Transcript_Analysis_Export.csv", show_col_types = FALSE)
#
#   # Please provide median, IQR, sd, count, se
#   #word counts
#   #max word count
#   max_words <- max(data$word_counts)
#
#   #min word count
#   min_words <- min(data$word_counts)
#
#   #average word count
#   avg_words <- mean(data$word_counts)
#
#   #readability
#   #max_readability
#   max_readability <- max(data$Flesch)
#
#   #min_readability
#   min_readability <- min(data$Flesch)
#
#   #average readability
#   avg_readability <- mean(data$Flesch)
#
#   #positive words
#   #max positive words
#   max_pos_words <- max(data$nrc_positive_words)
#
#   #min positive words
#   min_pos_words <- min(data$nrc_positive_words)
#
#   #average positive words
#   avg_pos_words <- mean(data$nrc_positive_words)
#
#   #negative words
#   #max negative words
#   max_neg_words <- max(data$nrc_negative_words)
#
#   #min negative words
#   min_neg_words <- min(data$nrc_negative_words)
#
#   #average negative words
#   avg_neg_words <- mean(data$nrc_negative_words)
#
#   #sentence counts
#   #max sentence counts
#   max_sentence_counts <- max(data$sentence_count)
#
#   #min sentence counts
#   min_sentence_counts <- min(data$sentence_count)
#
#   #average sentence counts
#   avg_sentence_counts <- mean(data$sentence_count)
#
#   #question counts
#   #max question counts
#   max_question_counts <- max(data$question_count)
#
#   #min question counts
#   min_question_counts <- min(data$question_count)
#
#   #average question counts
#   avg_question_counts <- mean(data$question_count)
#
#   information_df <- data.frame(
#     Measure = c("Max Word Count", "Min Word Count", "Average Word Count",
#                 "Max Readability", "Min Readability", "Average Readability",
#                 "Max Positive Words", "Min Positive Words", "Average Positive Words",
#                 "Max Negative Words", "Min Negative Words", "Average Negative Words",
#                 "Max Sentence Counts", "Min Sentence Counts", "Average Sentence Counts",
#                 "Max Question Counts", "Min Question Counts", "Average Question Counts"),
#     Value = c(max_words, min_words, avg_words,
#               max_readability, min_readability, avg_readability,
#               max_pos_words, min_pos_words, avg_pos_words,
#               max_neg_words, min_neg_words, avg_neg_words,
#               max_sentence_counts, min_sentence_counts, avg_sentence_counts,
#               max_question_counts, min_question_counts, avg_question_counts)
#   )
#
#   information_df
#
#
#
#
#
#
#
#   ## Ben -I am adding this here, so feel free to put it somewhere else (BK)
#
#   library(NLP)
#   library(openNLP)
#
#   # This function the strings, and provides the POS tags for each word
#   tagPOS <-  function(x, ...) {
#     # This takes x (a vector of strings), and turns it into one long string
#     s <- as.String(x)
#     # This initiates the tool you will use to extract the information
#     word_token_annotator <- Maxent_Word_Token_Annotator()
#     # Create an annotation object
#     a2 <- Annotation(1L, "sentence", 1L, nchar(s))
#     # Iterate over string and annotate where the words start and stop
#     a2 <- annotate(s, word_token_annotator, a2)
#     # Go through each word and label their POS
#     a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
#     # Filter for the word type
#     a3w <- a3[a3$type == "word"]
#     # Pull out the POS
#     POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
#     # combine the POS and the word
#     POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
#     # Return it as a list
#     list(POStagged = POStagged, POStags = POStags)
#   }
#
#   ## This seems a little complicated, but this is how I would do this.
#
#   # Get the names of the videos
#   unique_names_of_videos <- unique(data$PROJECT_TITLE)
#
#   # Get all of the possible tags
#   unique_tagPOS <- unique(tagPOS(data$Description)$POStags)
#
#   # Use purr to iterate through each video
#   df_POS_N_for_each_video <- purrr::map_df(unique_names_of_videos, ~{
#
#     # Filter for the correct video
#     filtered_data <- dplyr::filter(data, PROJECT_TITLE == .x)
#
#     # Obtain the POS tags
#     filtered_listtagPOS <- tagPOS(filtered_data$Description)$POStags
#
#     # Iterate through each tag, count it, and then combine it as a list
#     list_n_POS_per_video <- purrr::map(unique_tagPOS, ~{sum(stringr::str_detect(filtered_listtagPOS, paste0("^", .x, "$")))})
#
#     # Name the list
#     list_n_POS_per_video <- rlang::set_names(list_n_POS_per_video, unique_tagPOS)
#
#     # Turn it into a df
#     df_n_POS_per_video <- as.data.frame(list_n_POS_per_video)
#
#     # Add the name of the video to the df
#     df_n_POS_per_video$PROJECT_TITLE <- .x
#
#     # Make the video name the first variable
#     df_n_POS_per_video <- df_n_POS_per_video %>%
#       dplyr::select(PROJECT_TITLE, everything())
#
#     # Return the data frame
#     df_n_POS_per_video
#   })
#
#
#
# }
#
#
# Transcipt_Analysis_Version2 <- function() {
#
#   library(dplyr)
#   library(gapminder)
#   library(stringr)
#   library(tidyr)
#   library(ggplot2)
#   library(readr)
#   library(epubr)
#   library(tidytext)
#   library(tidyverse)
#   library(topicmodels)
#   library(quanteda.textstats)
#   library(tm)
#   library(koRpus)
#
#   # Create path
#   path_for_cleaned_transcripts <- "C:/users/benja/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/Transcript Analysis/CLEANED_TRANSCRIPTS.CSV"
#   # path_for_cleaned_transcripts <- bktools::FILES_obtain_home_dir_for_current_machine(path_for_cleaned_transcripts)
#
#   # Read in cleaned transcript data
#   data <- readr::read_csv(path_for_cleaned_transcripts, show_col_types = FALSE)
#
#   #set up table with names and text
#   datatext <- data %>%
#     group_by(PROJECT_TITLE, CLIENT_NAME, NP_CATEGORY) %>%
#     reframe (Description = paste(TEXT, collapse = ","))
#
#
#   #combine word count info into existing data table - and I think it worked?!
#   requested_project_data <- data %>%
#     group_by(PROJECT_TITLE, CLIENT_NAME, NP_CATEGORY) %>%
#     reframe (Description = paste(TEXT, collapse = ","))
#
#   # Collapse all of the text for each video
#   video_text_data <- requested_project_data %>%
#     group_by(PROJECT_TITLE, CLIENT_NAME, NP_CATEGORY) %>%
#     summarize(script = paste0(Description, collapse = " "))
#
#   #word counts
#   requested_project_data$word_counts <- count_words(requested_project_data$Description, word_pattern = "[A-Za-z0-9&]", break_pattern = " |\n")
#
#
#   # Calculate readability and add to the dataframe
#   requested_project_data$Flesch <-  textstat_readability(
#     video_text_data$script,
#     measure = "Flesch",
#     remove_hyphens = TRUE,
#     min_sentence_length = 1,
#     max_sentence_length = 10000,
#     intermediate = FALSE) %>%
#     pull(Flesch)
#
#   requested_project_data$FleschKincaid <-  textstat_readability(
#     video_text_data$script,
#     measure = "Flesch.Kincaid",
#     remove_hyphens = TRUE,
#     min_sentence_length = 1,
#     max_sentence_length = 10000,
#     intermediate = FALSE) %>%
#     pull(Flesch.Kincaid)
#
#   requested_project_data$FOG <-  textstat_readability(
#     video_text_data$script,
#     measure = "FOG",
#     remove_hyphens = TRUE,
#     min_sentence_length = 1,
#     max_sentence_length = 10000,
#     intermediate = FALSE) %>%
#     pull(FOG)
#
#   requested_project_data$Coleman <-  textstat_readability(
#     video_text_data$script,
#     measure = "Coleman",
#     remove_hyphens = TRUE,
#     min_sentence_length = 1,
#     max_sentence_length = 10000,
#     intermediate = FALSE) %>%
#     pull(Coleman)
#
#   requested_project_data$ARI <-  textstat_readability(
#     video_text_data$script,
#     measure = "ARI",
#     remove_hyphens = TRUE,
#     min_sentence_length = 1,
#     max_sentence_length = 10000,
#     intermediate = FALSE) %>%
#     pull(ARI)
#
#   requested_project_data$SMOG <-  textstat_readability(
#     video_text_data$script,
#     measure = "SMOG",
#     remove_hyphens = TRUE,
#     min_sentence_length = 1,
#     max_sentence_length = 10000,
#     intermediate = FALSE) %>%
#     pull(SMOG)
#
#   requested_project_data$FORCAST <-  textstat_readability(
#     video_text_data$script,
#     measure = "FORCAST",
#     remove_hyphens = TRUE,
#     min_sentence_length = 1,
#     max_sentence_length = 10000,
#     intermediate = FALSE) %>%
#     pull(FORCAST)
#
#
#
#
#   data_tokenized_no_stop <- requested_project_data %>%
#     unnest_tokens(word, Description) %>%
#     anti_join(stop_words, by = "word")
#
#   non_stop_words_df <- data_tokenized_no_stop %>%
#     group_by(PROJECT_TITLE) %>%
#     summarize(non_stop_words = n())
#
#   requested_project_data <- requested_project_data %>%
#     left_join(non_stop_words_df, by = "PROJECT_TITLE")
#
#   requested_project_data
#
#   nrc_pos <- get_sentiments("nrc") %>%
#     dplyr::filter(sentiment == "positive")
#
#   positive_words_df <- data_tokenized_no_stop  %>%
#     inner_join(nrc_pos, by = "word") %>%
#     group_by(PROJECT_TITLE) %>%
#     summarize(nrc_positive_words = n())
#
#   requested_project_data <- requested_project_data %>%
#     left_join(positive_words_df, by = "PROJECT_TITLE")
#
#   nrc_neg <- get_sentiments("nrc") %>%
#     dplyr::filter(sentiment == "negative")
#
#   negative_words_df <- data_tokenized_no_stop %>%
#     inner_join(nrc_neg, by = "word") %>%
#     group_by(PROJECT_TITLE) %>%
#     summarize(nrc_negative_words = n())
#
#   requested_project_data <- requested_project_data %>%
#     left_join(negative_words_df, by = "PROJECT_TITLE")
#
#   tidyr::replace_na(requested_project_data$nrc_positive_words, 0)
#
#   requested_project_data <- requested_project_data %>%
#     mutate(
#       nrc_positive_words = tidyr::replace_na(nrc_positive_words, 0),
#       nrc_negative_words = tidyr::replace_na(nrc_negative_words, 0),
#       overall_sentiment = nrc_positive_words - nrc_negative_words,
#       ratio = nrc_positive_words / nrc_negative_words)
#
#   #most frequent positive words in each ad
#   data_tokenized <- requested_project_data %>%
#     unnest_tokens(word, Description) %>%
#     count(PROJECT_TITLE, word, sort = TRUE)
#
#   nrc_pos <- get_sentiments("nrc") %>%
#     dplyr::filter(sentiment == "positive")
#
#   data_tokenized_positive <- data_tokenized %>%
#     inner_join(nrc_pos, by = "word")
#
#   top_positive_words <- data_tokenized_positive %>%
#     group_by(PROJECT_TITLE) %>%
#     summarize(
#       top_positive_word_1 = nth(word, 1),
#       top_positive_word_2 = nth(word, 2),
#       top_positive_word_3 = nth(word, 3)
#     )
#
#   requested_project_data <- requested_project_data %>%
#     left_join(top_positive_words, by = "PROJECT_TITLE")
#
#   requested_project_data <- requested_project_data %>%
#     mutate(
#       top_positive_word_1 = ifelse(is.na(top_positive_word_1), "NA", top_positive_word_1),
#       top_positive_word_2 = ifelse(is.na(top_positive_word_2), "NA", top_positive_word_2),
#       top_positive_word_3 = ifelse(is.na(top_positive_word_3), "NA", top_positive_word_3),
#       nrc_positive_words = ifelse(is.na(nrc_positive_words), 0, nrc_positive_words),
#       nrc_negative_words = ifelse(is.na(nrc_negative_words), 0, nrc_negative_words)
#     )
#
#   requested_project_data
#
#   #most frequent negative word(words?) in each ad
#   data_tokenized <- requested_project_data %>%
#     unnest_tokens(word, Description) %>%
#     count(PROJECT_TITLE, word, sort = TRUE)
#
#   nrc_neg <- get_sentiments("nrc") %>%
#     dplyr::filter(sentiment == "negative")
#
#   data_tokenized_negative <- data_tokenized %>%
#     inner_join(nrc_neg, by = "word")
#
#   top_negative_words <- data_tokenized_negative %>%
#     group_by(PROJECT_TITLE) %>%
#     summarize(
#       top_negative_word_1 = nth(word, 1),
#       top_negative_word_2 = nth(word, 2),
#       top_negative_word_3 = nth(word, 3)
#     )
#   requested_project_data <- requested_project_data %>%
#     left_join(top_negative_words, by = "PROJECT_TITLE")
#
#   requested_project_data <- requested_project_data %>%
#     mutate(
#       top_negative_word_1 = ifelse(is.na(top_negative_word_1), "NA", top_negative_word_1),
#       top_negative_word_2 = ifelse(is.na(top_negative_word_2), "NA", top_negative_word_2),
#       top_negative_word_3 = ifelse(is.na(top_negative_word_3), "NA", top_negative_word_3),
#       nrc_negative_words = ifelse(is.na(nrc_negative_words), 0, nrc_negative_words),
#     )
#
#   #sentence count in each ad
#   requested_project_data$sentence_count <- str_count(requested_project_data$Description, "\\.|\\?|\\!")
#
#   #question count in each ad
#   requested_project_data$question_count <- str_count(requested_project_data$Description, "\\?")
#
#   # Create a dataframe
#   df <- data.frame(requested_project_data)
#
#   # Export the dataframe
#
#   write.csv(requested_project_data, "C:/users/benja/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/Transcript Analysis\\Transcript_Analysis_Export.csv", row.names=FALSE)
# }
#
