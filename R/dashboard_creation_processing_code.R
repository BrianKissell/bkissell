# # Import the package that contains the functions this doc will be explaining.
# library(datacollectiontools)
#
# sum2 <- function(x) {
#   x <- as.numeric(x)
#   sum(x, na.rm = TRUE)
# }
#
# # Create a function that processes the data
# create_perc_table_mc <- function(data, variable, grouping_vars){
#   # Create the name of the reponse_variable's order var
#   order_variable <- paste0("VAR_ORDER__", {{variable}})
#
#   # Rename the variable as all
#   data$all <- data[[{{variable}}]]
#
#   # Obtain the order for all
#   data$VAR_ORDER__all <- data[[{{order_variable}}]]
#
#   # grouping_vars <- c(grouping_vars)
#
#   # Map through the grouping variables
#   df <- purrr::map_df(grouping_vars, ~ {
#     # Create the name of the grouping_variable's order var
#     order_grouping_var <- paste0("VAR_ORDER__", {{.x}})
#
#     data1 <- data %>%
#       # Rename these variables so that we can combine everything into the same column
#       dplyr::mutate(
#         grouping_var_levels = as.character(.data[[{{.x}}]]),
#         response_var_levels = as.character(.data[[{{variable}}]]),
#         grouping_var_order = .data[[order_grouping_var]],
#         response_var_order = .data[[{{order_variable}}]],
#       ) %>%
#       dplyr::filter(!is.na(grouping_var_levels), !is.na(response_var_levels)) %>%
#       # Group by these variables
#       dplyr::group_by(
#         wave,
#         grouping_var_levels,
#         response_var_levels,
#         grouping_var_order,
#         response_var_order) %>%
#       # Get the counts for the grouping variable by the response variables
#       dplyr::summarize(counts = n(), .groups = "drop") %>%
#       # group just by the grouping var
#       dplyr::group_by(grouping_var_levels) %>%
#       dplyr::mutate(
#         # Get the number you will use to calculate the percentage
#         group_n = sum(counts),
#         # Calculate the percentages
#         percentage = (counts / group_n),
#         # Label the grouping variable
#         grouping_var_used = {{.x}}) %>%
#       ungroup()
#
#     # Get the order for the grouping variables
#     data1$grouping_var_used_order <- which(grouping_vars == {{.x}})
#
#     data1
#   })
#
#   df_all <- data %>%
#     # Rename the response variable and the order of those vars
#     dplyr::mutate(
#       response_var_levels = as.character(.data[[{{variable}}]]),
#       response_var_order = .data[[{{order_variable}}]]
#     ) %>%
#     dplyr::filter(!is.na(response_var_levels)) %>%
#     # Group by these variables
#     dplyr::group_by(
#       wave,
#       response_var_levels,
#       response_var_order) %>%
#     # Calculate the counts
#     dplyr::summarize(counts = n(), .groups = "drop") %>%
#     dplyr::mutate(
#       # Get the group count
#       group_n = sum(counts),
#       # Calculate the percentage
#       percentage = (counts / group_n),
#       # Name the variable all
#       grouping_var_used = "All")
#
#   df_all$grouping_var_levels <- "All"
#   df_all$grouping_var_order <- 1
#   df_all$grouping_var_used_order <- 0
#
#   # Order the variables
#   df_all <- df_all%>%
#     dplyr::select(
#       wave, grouping_var_levels, response_var_levels,
#       grouping_var_order, response_var_order,
#       counts, group_n, percentage, grouping_var_used,
#       grouping_var_used_order
#     )
#
#   # Combine the two data frames
#   df <- df %>% rbind(df_all)
#
#   # # Recalculate the grouping var order
#   # df$grouping_var_order <- (df$grouping_var_used_order * 10) + df$grouping_var_order
#
#   return(df)
# }
#
# ################################################################################
#
# create_perc_table_sa <- function(data, item_set_selector, grouping_vars) {
#
#   # Obtain the variable names for this question
#   list_of_variable_names <- data %>%
#     dplyr::select(tidyselect::starts_with({{item_set_selector}})) %>%
#     colnames()
#
#   prefix <- paste0({{item_set_selector}})
#   # # Check if all columns are missing data
#   # is_all_column_data_missing_for_row <- check_if_all_are_missing_from_row(data, list_of_variable_names)
#
#   #loop through the grouping variables
#   processed_df <- purrr::map_df(grouping_vars, ~{
#
#     order_grouping_var <- paste0("VAR_ORDER__", {{..1}})
#
#     count_df <- data %>%
#       dplyr::mutate(
#         grouping_var_levels = .data[[{{..1}}]],
#         grouping_var_order = .data[[order_grouping_var]]
#       ) %>%
#       dplyr::select("wave", grouping_var_levels, grouping_var_order, all_of(list_of_variable_names))%>%
#
#       tidyr::pivot_longer(
#         cols = -c("wave", "grouping_var_levels", "grouping_var_order"),
#         names_to = "response_var_levels",
#         names_prefix = prefix,
#         values_to = "counts"
#       )  %>%
#       dplyr::filter(!is.na(counts)) %>%
#       dplyr::group_by(
#         .data[["wave"]],
#         .data[["grouping_var_levels"]],
#         .data[["grouping_var_order"]],
#         .data[["response_var_levels"]]
#       ) %>%
#       dplyr::summarize(counts = sum(counts))
#
#     count_df$grouping_var_used = {{..1}}
#     count_df$grouping_var_used_order <- which(grouping_vars == {{..1}})
#     count_df$grouping_var_levels <- as.character(count_df$grouping_var_levels)
#
#     count_df
#
#     n_df <- data %>%
#       dplyr::mutate(
#         grouping_var_levels = .data[[{{..1}}]],
#         grouping_var_order = .data[[order_grouping_var]]) %>%
#       dplyr::select("wave", grouping_var_levels, grouping_var_order, all_of(list_of_variable_names)) %>%
#       tidyr::pivot_longer(
#         cols = -c("wave", "grouping_var_levels", "grouping_var_order"),
#         names_to = "response_var_levels",
#         names_prefix = prefix,
#         values_to = "group_n"
#       ) %>%
#       dplyr::filter(!is.na(group_n)) %>%
#       dplyr::group_by(
#         .data[["wave"]],
#         .data[["grouping_var_levels"]],
#         .data[["grouping_var_order"]],
#         .data[["response_var_levels"]]
#       ) %>%
#       dplyr::summarize(group_n = n())
#
#     percentage_df <- count_df %>%
#       dplyr::left_join(n_df, by = c("wave", "grouping_var_levels", "response_var_levels", "grouping_var_order")) %>%
#       dplyr::mutate(percentage = counts / group_n)
#
#     percentage_df$percentage <- ifelse(percentage_df$counts == 0 & percentage_df$group_n == 0, 0, percentage_df$percentage)
#
#     percentage_df
#   })
#
#   all_count_df <- data %>%
#
#       dplyr::select("wave", all_of(list_of_variable_names)) %>%
#
#       tidyr::pivot_longer(
#         cols = -c("wave"),
#         names_to = "response_var_levels",
#         names_prefix = prefix,
#         values_to = "counts"
#       )  %>%
#       dplyr::filter(!is.na(counts)) %>%
#       dplyr::group_by(
#         .data[["wave"]],
#         .data[["response_var_levels"]]
#       ) %>%
#       dplyr::summarize(counts = sum(counts)) %>%
#     dplyr::mutate(grouping_var_levels = "All",
#                   grouping_var_order = 0,
#                   grouping_var_used = "All",
#                   grouping_var_used_order = 0)
#
#
#
#     all_n_df <- data %>%
#       dplyr::select("wave", all_of(list_of_variable_names)) %>%
#       tidyr::pivot_longer(
#         cols = -c("wave"),
#         names_to = "response_var_levels",
#         names_prefix = prefix,
#         values_to = "group_n"
#       ) %>%
#       dplyr::filter(!is.na(group_n)) %>%
#       dplyr::group_by(
#         .data[["wave"]],
#         .data[["response_var_levels"]]
#       ) %>%
#       dplyr::summarize(group_n = n())
#
#     all_percentage_df <- all_count_df %>%
#       dplyr::left_join(all_n_df, by = c("wave", "response_var_levels")) %>%
#       dplyr::mutate(percentage = counts / group_n)
#
#     all_percentage_df$percentage <- ifelse(all_percentage_df$counts == 0 & all_percentage_df$group_n == 0, 0, all_percentage_df$percentage)
#
#
#
#
#   all_percentage_df$response_var_used = {{item_set_selector}}
#   all_percentage_df$response_var_order <- NA
#
#   all_percentage_df <- all_percentage_df %>%
#     dplyr::select(
#       wave, grouping_var_levels, response_var_levels, grouping_var_order,
#       response_var_order, counts, group_n, percentage,
#       grouping_var_used, grouping_var_used_order, response_var_used
#     )
#
#   processed_df$response_var_used = {{item_set_selector}}
#   processed_df$response_var_order <- NA
#
#   processed_df <- processed_df %>%
#     dplyr::select(
#       wave, grouping_var_levels, response_var_levels, grouping_var_order,
#       response_var_order, counts, group_n, percentage,
#       grouping_var_used, grouping_var_used_order, response_var_used
#     )
#
#   processed_df <- processed_df %>% rbind(all_percentage_df)
#
#   processed_df$response_var_levels  <- stringr::str_replace_all(processed_df$response_var_levels, "_", " ") %>%
#     stringr::str_to_title()
#
#   processed_df$response_var_order <- as.numeric(factor(processed_df$response_var_levels))
#
#   processed_df$grouping_var_order <- (processed_df$grouping_var_used_order * 10) + processed_df$grouping_var_order
#
#   return(processed_df)
# }
#
#
# create_descr_table_num <- function(data, variable, grouping_vars) {
#
#   # Create the name of the reponse_variable's order var
#   order_variable <- paste0("VAR_ORDER__", {{variable}})
#
#   # Rename the variable as all
#   data$all <- data[[{{variable}}]]
#
#   # # Obtain the order for all
#   # data$VAR_ORDER__all <- data[[{{order_variable}}]]
#
#   # grouping_vars <- c(grouping_vars)
#
#   # Map through the grouping variables
#   df <- purrr::map_df(grouping_vars, ~ {
#     # Create the name of the grouping_variable's order var
#     order_grouping_var <- paste0("VAR_ORDER__", {{.x}})
#
#     data1 <- data %>%
#       # Rename these variables so that we can combine everything into the same column
#       dplyr::mutate(
#         grouping_var_levels = as.character(.data[[{{.x}}]]),
#         grouping_var_order = .data[[order_grouping_var]]) %>%
#       # Group by these variables
#       dplyr::group_by(
#         wave,
#         grouping_var_levels,
#         grouping_var_order) %>%
#
#       dplyr::filter(!is.na(.data[[{{variable}}]])) %>%
#       # Get the counts for the grouping variable by the response variables
#       dplyr::summarize(
#         mean = mean(.data[[{{variable}}]], na.rm = TRUE),
#         sd = sd(.data[[{{variable}}]], na.rm = TRUE),
#         n = n(),
#         se = sd/sqrt(n),
#         ci_limit = se * 1.96,
#         ci_upper = mean + ci_limit,
#         ci_lower = mean - ci_limit)
#     # Get the order for the grouping variables
#     data1$grouping_var_used <- {{.x}}
#
#     data1$grouping_var_used_order <- which(grouping_vars == {{.x}})
#
#     data1
#   })
#
#   df_all <- data %>%
#     dplyr::mutate(
#       grouping_var_levels = "All",
#       grouping_var_order = 0) %>%
#     # Group by these variables
#     dplyr::group_by(
#       wave,
#       grouping_var_levels,
#       grouping_var_order) %>%
#     dplyr::filter(!is.na(.data[[{{variable}}]])) %>%
#     # Calculate the counts
#     dplyr::summarize(
#       mean = mean(.data[[{{variable}}]], na.rm = TRUE),
#       sd = sd(.data[[{{variable}}]], na.rm = TRUE),
#       n = n(),
#       se = sd/sqrt(n),
#       ci_limit = se * 1.96,
#       ci_upper = mean + ci_limit,
#       ci_lower = mean - ci_limit)
#
#   df_all$grouping_var_used <- "All"
#   df_all$grouping_var_levels <- "All"
#   df_all$grouping_var_order <- 0
#   df_all$grouping_var_used_order <- 0
#
#   # Order the variables
#   df_all <- df_all %>%
#     dplyr::select(
#       wave, grouping_var_levels, grouping_var_order,
#       mean, sd, n, se, ci_limit, ci_upper, ci_lower,
#       grouping_var_used,
#       grouping_var_used_order
#     )
#
#   # Combine the two data frames
#   df <- df %>% rbind(df_all)
#
#   # Recalculate the grouping var order
#   df$grouping_var_order <- (df$grouping_var_used_order * 10) + df$grouping_var_order
#
#   return(df)
# }
#
#
# # Folder Set-Up -----------------------------------------------------------
#
# # Create the paths for the folders that need to exist for the survey project
# survey_folder_structure_paths_list <- create_survey_folder_structure_paths_list(
#   project_year = 2023,
#   project_folder_name = "230389_SDZ_BC__Quarterly Survey_OID1771",
#   list_of_survey_version_names = list("Wave 1 - Member", "Wave 1 - Donor")
# )
#
# # # Print the paths so you can see which are created
# # print(survey_folder_structure_paths_list)
#
# # If theses do not exist, create them.
# create_survey_folders_if_they_do_not_exist(survey_folder_structure_paths_list)
#
# # Point to where the survey data is located
# survey_directory_path <- survey_folder_structure_paths_list$project_data_collection_survey_monkey_data_folder_location
# home_dir <- survey_folder_structure_paths_list$home_dir
#
# # Read and Process Data ---------------------------------------------------
#
# # Read the survey data as a list
# survey_data <- read_survey_data(survey_directory_path = survey_directory_path)
#
# # # Filter out bad data
# # survey_data <- purrr::map(survey_data, ~{
# #   df <- remove_rids_from_df(survey_directory_path, df = .x)
# #   df
# # })
#
# # Obtain the version names
# version_name <- names(survey_data)
#
# # Label the different version types
# MEMBER_VERSION <- stringr::str_detect(version_name, "Member")
# DONOR_VERSION <- stringr::str_detect(version_name, "Donor")
#
# # Filter it into two different datasets
# survey_data_member <- survey_data[MEMBER_VERSION] %>% purrr::map_df(~.x)
# survey_data_donor <- survey_data[DONOR_VERSION] %>% purrr::map_df(~.x)
#
# # # Filter only for good RIDS: usually used at the end
# # # good_rids <- readr::read_csv(paste0(survey_directory_path, "/good_rids.csv"))
# # # survey_data <- survey_data %>%
# # #   dplyr::filter(RID %in% good_rids$rids)
#
# # Adjust Data Types -------------------------------------------------------
#
# survey_data_member$wave <- stringr::str_extract(survey_data_member$version_name, "Wave [0-9]{1,2}")
# survey_data_member$study_version <- stringr::str_extract(survey_data_member$version_name, "(Member)|(Donor)")
#
# survey_data_member$ethnicity <- ifelse(is.na(survey_data_member$ethnicity), "Other (please specify)", as.character(survey_data_member$ethnicity))
#
# survey_data_member$age <- as.numeric(survey_data_member$age)
#
# current_year <- lubridate::today() %>% lubridate::year()
#
# survey_data_member$age_group <- dplyr::case_when(
#   survey_data_member$age >= current_year - 1945 ~ "Seniors_Traditional",
#   survey_data_member$age >= current_year - 1964 & survey_data_member$age <= current_year - 1946 ~ "Baby Boomers",
#   survey_data_member$age >= current_year - 1980 & survey_data_member$age <= current_year - 1965 ~ "Gen X",
#   survey_data_member$age >= current_year - 1996 & survey_data_member$age <= current_year - 1981 ~ "Millennials",
#   survey_data_member$age <= current_year - 1995 ~ "Gen Z"
# ) %>% factor(levels = c("Gen Z", "Millennials", "Gen X", "Baby Boomers", "Seniors_Traditional"), ordered = TRUE)
#
# # Get named vectors
# path_to_named_vectors_workbook <- paste0(survey_directory_path, "/named_vectors.xlsx")
#
# named_vectors_workbook_sheets <- readxl::excel_sheets(path_to_named_vectors_workbook)
#
# named_vectors_list <- purrr::map(named_vectors_workbook_sheets, ~{
#   df <- readxl::read_excel(path_to_named_vectors_workbook, sheet = .x)
#
#   named_vector <- df$vector_of_factor_levels
#
#   names(named_vector) <- df$names_of_factor_levels
#
#   return(named_vector)
# }) %>% purrr::set_names(named_vectors_workbook_sheets)
#
# # create a list that contains all variables that you want to turn into factors
# member_variables_to_reorder <- list(
#   "length_of_membership",
#   "number_of_visits_last_year",
#   "number_of_planned_visits",
#   "why_originally_became_member",
#   "fuob__tour_or_tram",
#   "fuob__support_conservation",
#   "fuob__wild_perks_discounts",
#   "fuob__admission_discount",
#   "fuob__journal_subscription",
#   "vob__tour_or_tram",
#   "vob__support_conservation",
#   "vob__wild_perks_discounts",
#   "vob__admission_discount",
#   "vob__journal_subscription",
#   "ad_most_influenced",
#   "likelihood_of_renewing",
#   "method_of_renewal",
#   "familiarity_of_efforts",
#   "confidence_in_saving_species",
#   "preferred_habitats",
#   "donated_to_sdzwa_in_past",
#   "considering_upgrading",
#   "net_promoter",
#   "gender",
#   "age_group",
#   "ethnicity",
#   "marital_status",
#   "annual_household_income",
#   "ec__relates_to_me_personally",
#   "ec__relates_to_f_and_f",
#   "ec__deserves_my_support",
#   "ec__made_substantial_progress",
#   "ec__is_trustworthy",
#   "ec__uses_donations_responsibly",
#   "ec__is_compassionate_and_caring",
#   "ec__is_innovative_and_a_leader",
#   "ec__is_doing_enough",
#   "ec__improve_lives_like_mine",
#   "ec__does_unique_important_work",
#   "ec__heard_pos_from_media",
#   "ec__heard_pos_from_friends",
#   "ec__heard_neg_from_media",
#   "ec__heard_neg_from_friends",
#   "ec__is_a_good_organization",
#   "ec__strong_positive_feelings",
#   "ec__is_transparent_and_clear",
#   "ec__one_of_the_best",
#   "ec__addresses_problems_i_t_m",
#   "wave")
#
# # Create a second list that contains the named vector that specifies the appropriate levels
# member_named_vectors <- list(
#   named_vectors_list$length_of_membership,
#   named_vectors_list$number_of_visits_last_year,
#   named_vectors_list$number_of_planned_visits,
#   named_vectors_list$why_originally_became_member,
#   named_vectors_list$frequency_of_use,
#   named_vectors_list$frequency_of_use,
#   named_vectors_list$frequency_of_use,
#   named_vectors_list$frequency_of_use,
#   named_vectors_list$frequency_of_use,
#   named_vectors_list$level_of_value,
#   named_vectors_list$level_of_value,
#   named_vectors_list$level_of_value,
#   named_vectors_list$level_of_value,
#   named_vectors_list$level_of_value,
#   named_vectors_list$ad_most_influenced_you_to_join,
#   named_vectors_list$likelihood_of_renewing,
#   named_vectors_list$preferred_method_of_renewal,
#   named_vectors_list$familiarity,
#   named_vectors_list$confidence,
#   named_vectors_list$preferred_habitats,
#   named_vectors_list$donated_to_sdzwa_in_past,
#   named_vectors_list$considering_upgrading,
#   named_vectors_list$net_promoter,
#   named_vectors_list$gender,
#   named_vectors_list$age_group,
#   named_vectors_list$ethnicity,
#   named_vectors_list$marital_status,
#   named_vectors_list$annual_household_income,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$wave
# )
#
# # Convert variables to factors - Note that'ord' is an ordered factor
# survey_data_member <- factor_vars_with_named_vectors(survey_data_member, member_variables_to_reorder, member_named_vectors)
#
# # Save the text vars to the correct location
# survey_data_member$text_whtrm__other <- survey_data_member$whtrm__other
# survey_data_member$text_wcctr__other <- survey_data_member$wcctr__other
# survey_data_member$text_wyitr__other <- survey_data_member$wyitr__other
# survey_data_member$text_fidtu__other <- survey_data_member$fidtu__other
# survey_data_member$text_fmhtu__other <- survey_data_member$fmhtu__other
# survey_data_member$text_wwctu__other <- survey_data_member$wwctu__other
#
# SELECT_ALL_VARS <- c(
#   "whtrm__",
#   "wcctr__",
#   "wyitr__",
#   "pmtrcn__",
#   "fidtu__",
#   "fmhtu__",
#   "wwctu__",
#   "h_c__"
# )
#
# for(i in SELECT_ALL_VARS) {
#   survey_data_member <- process_select_all_question(survey_data_member, i)
# }
#
# survey_data_member <- survey_data_member %>%
#   mutate(dplyr::across(where(is.factor), as.numeric, .names = "VAR_ORDER__{.col}"))
#
# survey_data_member <- survey_data_member %>%
#   dplyr::filter(!is.na(annual_household_income))
#
# survey_data_member <- survey_data_member %>%
#   mutate(
#     across(
#       c("net_promoter", starts_with("ec__")),
#       as.numeric,
#       .names = "NUM__{.col}")
#   )
#
# survey_data_member$NUM__ec_total <- survey_data_member %>%
#   select(starts_with("NUM__ec__")) %>%
#   rowMeans()
# ###############################################################################
#
# clean_member_data_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/clean_member_data.csv")
# readr::write_csv(survey_data_member, clean_member_data_path)
#
# ################################################################################
#
# text_survey_data_member <- survey_data_member %>%
#   dplyr::select(
#     respondent_id, start_date, end_date, wave,	study_version,
#     gender,	age, age_group,	ethnicity,	text_ethnicity_other,
#     marital_status, h_c__no_children,
#     h_c__under_12,	h_c__12_to_17,
#     h_c__18_to_65,	h_c__65_and_up,
#     annual_household_income,	zip_code,
#     starts_with("text_")
#   )
#
#
# # text_originally_became_member,
# # text_most_enjoyed,
# # text_ad_most_influenced,
# # text_whtrm__other,
# # text_wcctr__other,
# # text_what_other_organization,
# # text_wyitr__other,
# # text_method_of_renewal,
# # text_fidtu__other,
# # text_fmhtu__other,
# # text_wwctu__other,
# # text_why_net_promoter,
# # text_any_other_feedback,
# # text_ethnicity_other
# ################################################################################
#
# text_survey_data_member_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/text_survey_data_member.csv")
# readr::write_csv(text_survey_data_member, text_survey_data_member_path)
#
# ################################################################################
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
# # library(openxlsx)
# #
# # wb <- createWorkbook()
# #
# # addWorksheet(wb, "Textual Responses")
#
# power_bi_member_text <- text_survey_data_member %>%
#   tidyr::pivot_longer(
#     cols = -c(respondent_id, start_date, wave, study_version, gender, age, age_group,
#               ethnicity, marital_status, h_c__no_children, h_c__under_12,
#               h_c__12_to_17, h_c__18_to_65, h_c__65_and_up,
#               annual_household_income, zip_code),
#     names_to = "response_var_used",
#     names_prefix = "text_",
#     values_to = "textual_responses"
#   ) %>%
#   dplyr::filter(!is.na(textual_responses))
#
# power_bi_member_text_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/power_bi_member_text.csv")
# readr::write_csv(power_bi_member_text, power_bi_member_text_path)
#
# writeData(wb, "Textual Responses", textual_responses_df)
#
# item_names <- c(
#   "length_of_membership",
#   "number_of_visits_last_year",
#   "number_of_planned_visits",
#   "why_originally_became_member",
#   "fuob__tour_or_tram",
#   "fuob__support_conservation",
#   "fuob__wild_perks_discounts",
#   "fuob__admission_discount",
#   "fuob__journal_subscription",
#   "vob__tour_or_tram",
#   "vob__support_conservation",
#   "vob__wild_perks_discounts",
#   "vob__admission_discount",
#   "vob__journal_subscription",
#   "ad_most_influenced",
#   "likelihood_of_renewing",
#   "method_of_renewal",
#   "familiarity_of_efforts",
#   "confidence_in_saving_species",
#   "preferred_habitats",
#   "donated_to_sdzwa_in_past",
#   "considering_upgrading",
#   "net_promoter",
#   "gender",
#   "age_group",
#   "ethnicity",
#   "marital_status",
#   "annual_household_income",
#   "ec__relates_to_me_personally",
#   "ec__relates_to_f_and_f",
#   "ec__deserves_my_support",
#   "ec__made_substantial_progress",
#   "ec__is_trustworthy",
#   "ec__uses_donations_responsibly",
#   "ec__is_compassionate_and_caring",
#   "ec__is_innovative_and_a_leader",
#   "ec__is_doing_enough",
#   "ec__improve_lives_like_mine",
#   "ec__does_unique_important_work",
#   "ec__heard_pos_from_media",
#   "ec__heard_pos_from_friends",
#   "ec__heard_neg_from_media",
#   "ec__heard_neg_from_friends",
#   "ec__is_a_good_organization",
#   "ec__strong_positive_feelings",
#   "ec__is_transparent_and_clear",
#   "ec__one_of_the_best",
#   "ec__addresses_problems_i_t_m",
#   "wave"
#   )
#
# # addWorksheet(wb, "Multiple Choice")
#
# power_bi_member_multiple_choice <- purrr::map_df(item_names, ~{
#   table_mc_df <- create_perc_table_mc(
#     data = survey_data_member,
#     variable = .x,
#     grouping_vars = c(
#       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
#     )
#   )
#
#   table_mc_df$response_var_used <- {{.x}}
#   table_mc_df$response_var_used_order <- which(item_names == {{.x}})
#
#   table_mc_df
# })
#
# power_bi_member_multiple_choice <- power_bi_member_multiple_choice %>%
#   dplyr::filter(!is.na(response_var_levels)) %>%
#   dplyr::filter(!is.na(grouping_var_levels))
#
# response_var_multiplier <- dplyr::case_when(
#   power_bi_member_multiple_choice$response_var_used_order %in% c(1) ~ 1,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(2, 3) ~ 2,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(4) ~ 3,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(5,6,7,8,9) ~ 4,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(10, 11, 12, 13, 14) ~ 5,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(15) ~ 6,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(16) ~ 7,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(17) ~ 8,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(18) ~ 9,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(19) ~ 10,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(20) ~ 11,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(21) ~ 12,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(22) ~ 13,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(23) ~ 14,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(24) ~ 15,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(25) ~ 16,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(26) ~ 17,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(27) ~ 18,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(28) ~ 19,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(29:48) ~ 20,
#   power_bi_member_multiple_choice$response_var_used_order %in% c(49) ~ 21)
#
# power_bi_member_multiple_choice$response_var_order <- power_bi_member_multiple_choice$response_var_order + (response_var_multiplier * 10)
# power_bi_member_multiple_choice$grouping_var_order <- power_bi_member_multiple_choice$grouping_var_order + (power_bi_member_multiple_choice$grouping_var_used_order * 10)
#
# power_bi_member_multiple_choice$response_var_order <- ifelse(
#   power_bi_member_multiple_choice$response_var_levels == "Other",
#   max(power_bi_member_multiple_choice$response_var_order) + 1,
#   power_bi_member_multiple_choice$response_var_order
#   )
#
# power_bi_member_multiple_choice$grouping_var_order <- ifelse(
#   power_bi_member_multiple_choice$grouping_var_levels == "Other",
#   max(power_bi_member_multiple_choice$grouping_var_order) + 1,
#   power_bi_member_multiple_choice$grouping_var_order
# )
#
# power_bi_member_multiple_choice$grouping_var_used  <- stringr::str_replace_all(power_bi_member_multiple_choice$grouping_var_used, "_", " ") %>%
#   stringr::str_to_title()
#
# power_bi_member_multiple_choice$response_var_used  <- stringr::str_replace_all(power_bi_member_multiple_choice$response_var_used, "_", "") %>%
#   stringr::str_to_lower()
#
#
# power_bi_member_multiple_choice_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/power_bi_member_multiple_choice.csv")
# readr::write_csv(power_bi_member_multiple_choice, power_bi_member_multiple_choice_path)
#
#
#
# # power_bi_member_multiple_choice %>%
# #   group_by(response_var_order, response_var_levels) %>%
# #   count() %>% View()
# #
# # unique(power_bi_member_multiple_choice$response_var_order) %>% length()
# # unique(power_bi_member_multiple_choice$response_var_levels) %>% length()
# #
# # power_bi_member_multiple_choice %>%
# #   group_by(grouping_var_order, grouping_var_levels) %>%
# #   count() %>% View()
# #
# # unique(power_bi_member_multiple_choice$grouping_var_order) %>% length()
# # unique(power_bi_member_multiple_choice$grouping_var_levels) %>% length()
# writeData(wb, "Multiple Choice", table_mc)
#
# SELECT_ALL_VARS <- c(
#   "whtrm__",
#   "wcctr__",
#   "wyitr__",
#   "pmtrcn__",
#   "fidtu__",
#   "fmhtu__",
#   "wwctu__",
#   "h_c__"
# )
#
# power_bi_member_select_all <- purrr::map_df(SELECT_ALL_VARS, ~{
#   table_sa_df <- create_perc_table_sa(
#     data = survey_data_member,
#     item_set_selector = .x,
#     grouping_vars = c(
#       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
#     )
#   )
#
#   table_sa_df$response_var_used <- {{.x}}
#   table_sa_df$response_var_used_order <- which(SELECT_ALL_VARS == {{.x}})
#
#
#   table_sa_df
# })
#
# power_bi_member_select_all <- power_bi_member_select_all %>%
#   dplyr::filter(!is.na(response_var_levels))%>%
#   dplyr::filter(!is.na(grouping_var_levels))
#
# power_bi_member_select_all$response_var_order <- ifelse(
#   power_bi_member_select_all$response_var_levels == "Other",
#   max(power_bi_member_select_all$response_var_order) + 1,
#   power_bi_member_select_all$response_var_order
# )
#
# power_bi_member_select_all$grouping_var_order <- ifelse(
#   power_bi_member_select_all$grouping_var_levels == "Other",
#   max(power_bi_member_select_all$grouping_var_order) + 1,
#   power_bi_member_select_all$grouping_var_order
# )
#
# power_bi_member_select_all$response_var_order <- power_bi_member_select_all$response_var_order + (power_bi_member_select_all$response_var_used_order * 10)
#
# response_levels_check <- power_bi_member_select_all %>%
#   group_by(response_var_order, response_var_levels) %>%
#   count() %>%
#   pull(response_var_levels)
#
# duplicate_indicator <- response_levels_check %>%
#   duplicated()
#
# response_level_duplicates <- response_levels_check[duplicate_indicator]
#
# for(i in seq_along(response_level_duplicates)) {
#   power_bi_member_select_all$response_var_order <- ifelse(
#     power_bi_member_select_all$response_var_levels == response_level_duplicates[[i]],
#     max(power_bi_member_select_all$response_var_order) + i,
#     power_bi_member_select_all$response_var_order)
# }
#
#
# power_bi_member_select_all$grouping_var_used  <- stringr::str_replace_all(power_bi_member_select_all$grouping_var_used, "_", " ") %>%
#   stringr::str_to_title()
#
# power_bi_member_select_all$response_var_used  <- stringr::str_replace_all(power_bi_member_select_all$response_var_used, "_", "") %>%
#   stringr::str_to_lower()
#
#
# power_bi_member_select_all$response_var_used <- dplyr::case_when(
#   power_bi_member_select_all$response_var_used == "whtrm" ~ "Which of the following options best describes why you are hesitant to renew membership?",
#   power_bi_member_select_all$response_var_used == "wcctr" ~ "Which of the following options best describes what would convince you to renew your San Diego Zoo and San Diego Zoo Safari Park membership?",
#   power_bi_member_select_all$response_var_used == "wyitr" ~ "Which of the following best describes why you intend to renew your San Diego Zoo and San Diego Zoo Safari Park membership for the next year?",
#   power_bi_member_select_all$response_var_used == "pmtrcn" ~ "How would you prefer to receive information on San Diego Zoo Wildlife Alliance’s conservation work?",
#   power_bi_member_select_all$response_var_used == "fidtu" ~ "Which of these factors influenced your decision to upgrade to a higher membership level?",
#   power_bi_member_select_all$response_var_used == "fmhtu" ~ "Which of these factors make you hesitant to upgrade to a higher membership level?",
#   power_bi_member_select_all$response_var_used == "wwctu" ~ "What would convince you to upgrade to a higher membership level?",
#   power_bi_member_select_all$response_var_used == "hc" ~ "Which describes your household composition?",
# )
#
# power_bi_member_select_all_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/power_bi_member_select_all.csv")
# readr::write_csv(power_bi_member_select_all, power_bi_member_select_all_path)
#
#
# numeric_variables <- survey_data_member %>%
#   select( starts_with("NUM__ec_")) %>%
#   colnames()
#
# power_bi_member_descr_table_num <- purrr::map_df(numeric_variables, ~{
#   table_num_df <- create_descr_table_num(
#     data = survey_data_member,
#     variable = .x,
#     grouping_vars = c(
#       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
#     )
#   )
#
#   table_num_df$response_var_used <- {{.x}}
#   table_num_df$response_var_used_order <- which(numeric_variables == {{.x}})
#
#
#   table_num_df
# })
#
#
# power_bi_member_descr_table_num$grouping_var_order <- ifelse(
#   power_bi_member_descr_table_num$grouping_var_levels == "Other",
#   max(power_bi_member_descr_table_num$grouping_var_order) + 1,
#   power_bi_member_descr_table_num$grouping_var_order
# )
#
#
# grouping_levels_check <- power_bi_member_descr_table_num %>%
#   group_by(grouping_var_order, grouping_var_levels) %>%
#   count() %>%
#   pull(grouping_var_levels)
#
# duplicate_indicator <- grouping_levels_check %>%
#   duplicated()
#
# grouping_level_duplicates <- grouping_levels_check[duplicate_indicator]
#
# for(i in seq_along(grouping_level_duplicates)) {
#   power_bi_member_descr_table_num$grouping_var_order <- ifelse(
#     power_bi_member_descr_table_num$grouping_var_levels == grouping_level_duplicates[[i]],
#     max(power_bi_member_descr_table_num$grouping_var_order) + i,
#     power_bi_member_descr_table_num$grouping_var_order)
# }
#
# #
# #
# # power_bi_member_multiple_choice$response_var_order <- power_bi_member_multiple_choice$response_var_order + (response_var_multiplier * 10)
# # power_bi_member_multiple_choice$grouping_var_order <- power_bi_member_multiple_choice$grouping_var_order + (power_bi_member_multiple_choice$grouping_var_used_order * 10)
# #
#
# power_bi_member_descr_table_num$grouping_var_order <- ifelse(
#   power_bi_member_descr_table_num$grouping_var_levels == "Other",
#   max(power_bi_member_descr_table_num$grouping_var_order) + 1,
#   power_bi_member_descr_table_num$grouping_var_order
# )
#
# power_bi_member_descr_table_num$grouping_var_used  <- stringr::str_replace_all(power_bi_member_descr_table_num$grouping_var_used, "_", " ") %>%
#   stringr::str_to_title()
#
# power_bi_member_descr_table_num$response_var_used  <- stringr::str_replace_all(power_bi_member_descr_table_num$response_var_used, "NUM__EC__", "") %>%
#   stringr::str_replace_all("_", " ") %>%
#   stringr::str_to_title()
#
#
#
# power_bi_member_descr_table_num_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/power_bi_member_descr_table_num.csv")
# readr::write_csv(power_bi_member_descr_table_num, power_bi_member_descr_table_num_path)
#
#
# #
# # power_bi_member_descr_table_num %>%
# #   group_by(grouping_var_order, grouping_var_levels) %>%
# #   count() %>% View()
# #
# # unique(power_bi_member_descr_table_num$grouping_var_order) %>% length()
# # unique(power_bi_member_descr_table_num$grouping_var_levels) %>% length()
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
# ###############################################################################
# ###############################################################################
#
# survey_data_donor$wave <- stringr::str_extract(survey_data_donor$version_name, "Wave [0-9]{1,2}")
# survey_data_donor$study_version <- stringr::str_extract(survey_data_donor$version_name, "(Member)|(Donor)")
#
# survey_data_donor$age_group <- dplyr::case_when(
#   survey_data_donor$age >= 18 & survey_data_donor$age < 40 ~ "18 - 39",
#   survey_data_donor$age >= 40 & survey_data_donor$age < 60 ~ "40 - 59",
#   survey_data_donor$age >= 60 & survey_data_donor$age < 101 ~ "60 - 100"
# )
#
# # Get named vectors
# path_to_named_vectors_workbook <- paste0(survey_directory_path, "/named_vectors.xlsx")
#
# named_vectors_workbook_sheets <- readxl::excel_sheets(path_to_named_vectors_workbook)
#
# named_vectors_list <- purrr::map(named_vectors_workbook_sheets, ~{
#   df <- readxl::read_excel(path_to_named_vectors_workbook, sheet = .x)
#
#   named_vector <- df$vector_of_factor_levels
#
#   names(named_vector) <- df$names_of_factor_levels
#
#   return(named_vector)
# }) %>% purrr::set_names(named_vectors_workbook_sheets)
#
# # create a list that contains all variables that you want to turn into factors
# donor_variables_to_reorder <- list(
#   "first_donation_why",
#   "confident_donation_will_help",
#   "monthly_donor",
#   "familiarity_with_donor_program",
#   "considering_becoming_donor",
#   "why_not_monthly_donor",
#   "last_donation_how",
#   "last_online_donation_how",
#   "horci__email",
#   "horci__mail",
#   "horci__website",
#   "horci__at_park",
#   "horci__social_media",
#   "horci__word_of_mouth",
#   "support_sdzwa_again",
#   "will_change_donation",
#   "net_promoter",
#   "gender",
#   "age_group",
#   "ethnicity",
#   "marital_status",
#   "annual_household_income",
#   "ec__relates_to_me_personally",
#   "ec__relates_to_f_and_f",
#   "ec__deserves_my_support",
#   "ec__made_substantial_progress",
#   "ec__is_trustworthy",
#   "ec__uses_donations_responsibly",
#   "ec__is_compassionate_and_caring",
#   "ec__is_innovative_and_a_leader",
#   "ec__is_doing_enough",
#   "ec__improve_lives_like_mine",
#   "ec__does_unique_important_work",
#   "ec__heard_pos_from_media",
#   "ec__heard_pos_from_friends",
#   "ec__heard_neg_from_media",
#   "ec__heard_neg_from_friends",
#   "ec__is_a_good_organization",
#   "ec__strong_positive_feelings",
#   "ec__is_transparent_and_clear",
#   "ec__one_of_the_best",
#   "ec__addresses_problems_i_t_m",
#   "wave")
#
# # Create a second list that contains the named vector that specifies the appropriate levels
# donor_named_vectors <- list(
#   named_vectors_list$first_donation_why,
#   named_vectors_list$confidence,
#   named_vectors_list$yes_no,
#   named_vectors_list$familiarity,
#   named_vectors_list$likelihood_2,
#   named_vectors_list$why_not_monthly_donor,
#   named_vectors_list$last_donation_how,
#   named_vectors_list$last_online_donation_how,
#   named_vectors_list$frequency_of_info,
#   named_vectors_list$frequency_of_info,
#   named_vectors_list$frequency_of_info,
#   named_vectors_list$frequency_of_info,
#   named_vectors_list$frequency_of_info,
#   named_vectors_list$frequency_of_info,
#   named_vectors_list$likelihood,
#   named_vectors_list$will_change_donation,
#   named_vectors_list$net_promoter,
#   named_vectors_list$gender,
#   named_vectors_list$age_group,
#   named_vectors_list$ethnicity,
#   named_vectors_list$marital_status,
#   named_vectors_list$annual_household_income,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$agreement,
#   named_vectors_list$wave
# )
#
# # Convert variables to factors - Note that'ord' is an ordered factor
# survey_data_donor <- factor_vars_with_named_vectors(survey_data_donor, donor_variables_to_reorder, donor_named_vectors)
#
# # Save the text vars to the correct location
# survey_data_donor$text_wwcs__other <- survey_data_donor$wwcs__other
# survey_data_donor$text_wns__other <- survey_data_donor$wns__other
# survey_data_donor$text_wwctsa__other <- survey_data_donor$wwctsa__other
#
# SELECT_ALL_VARS <- c(
#   "wwcs__",
#   "wns__",
#   "wwctsa__",
#   "h_c__"
# )
#
# for(i in SELECT_ALL_VARS) {
#   survey_data_donor <- process_select_all_question(survey_data_donor, i)
# }
#
# survey_data_donor <- survey_data_donor %>%
#   mutate(dplyr::across(where(is.factor), as.numeric, .names = "VAR_ORDER__{.col}"))
#
# survey_data_donor <- survey_data_donor %>%
#   dplyr::filter(!is.na(annual_household_income))
#
# survey_data_donor <- survey_data_donor %>%
#   mutate(
#     across(
#       c("net_promoter", starts_with("ec__")),
#       as.numeric,
#       .names = "NUM__{.col}")
#   )
#
# survey_data_donor$NUM__ec_total <- survey_data_donor %>%
#   select(starts_with("NUM__ec__")) %>%
#   rowMeans()
#
# ################################################################################
#
# clean_donor_data_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/clean_donor_data.csv")
# readr::write_csv(survey_data_donor, clean_donor_data_path)
#
# ###############################################################################
#
# text_survey_data_donor <- survey_data_donor %>%
#   dplyr::select(
#     respondent_id, start_date, end_date, wave,	study_version,
#     gender,	age, age_group,	ethnicity,	text_ethnicity_other,
#     marital_status, h_c__no_children,
#     h_c__under_12,	h_c__12_to_17,
#     h_c__18_to_65,	h_c__65_and_up,
#     annual_household_income,	zip_code,
#     text_first_donation_why_other,
#     text_why_not_monthly_donor,
#     text_last_donation_how_other,
#     text_last_online_donation_how,
#     text_wwcs__other,
#     text_wns__other,
#     text_wwctsa__other,
#     text_why_net_promoter,
#     text_any_other_feedback,
#     text_ethnicity_other
#   )
#
# ################################################################################
#
# text_survey_data_donor_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/text_survey_data_donor.csv")
# readr::write_csv(text_survey_data_donor, text_survey_data_donor_path)
#
#
#
#
#
#
#
#
# power_bi_donor_text <- text_survey_data_member %>%
#   tidyr::pivot_longer(
#     cols = -c(respondent_id, start_date, wave, study_version, gender, age, age_group,
#               ethnicity, marital_status, h_c__no_children, h_c__under_12,
#               h_c__12_to_17, h_c__18_to_65, h_c__65_and_up,
#               annual_household_income, zip_code),
#     names_to = "response_var_used",
#     names_prefix = "text_",
#     values_to = "textual_responses"
#   ) %>%
#   dplyr::filter(!is.na(textual_responses))
#
# power_bi_donor_text_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/power_bi_donor_text.csv")
# readr::write_csv(power_bi_donor_text, power_bi_donor_text_path)
#
#
#
# item_names <- c(
#   "first_donation_why",
#   "confident_donation_will_help",
#   "monthly_donor",
#   "familiarity_with_donor_program",
#   "considering_becoming_donor",
#   "why_not_monthly_donor",
#   "last_donation_how",
#   "last_online_donation_how",
#   "horci__email",
#   "horci__mail",
#   "horci__website",
#   "horci__at_park",
#   "horci__social_media",
#   "horci__word_of_mouth",
#   "support_sdzwa_again",
#   "will_change_donation",
#   "net_promoter",
#   "gender",
#   "age_group",
#   "ethnicity",
#   "marital_status",
#   "annual_household_income",
#   "ec__relates_to_me_personally",
#   "ec__relates_to_f_and_f",
#   "ec__deserves_my_support",
#   "ec__made_substantial_progress",
#   "ec__is_trustworthy",
#   "ec__uses_donations_responsibly",
#   "ec__is_compassionate_and_caring",
#   "ec__is_innovative_and_a_leader",
#   "ec__is_doing_enough",
#   "ec__improve_lives_like_mine",
#   "ec__does_unique_important_work",
#   "ec__heard_pos_from_media",
#   "ec__heard_pos_from_friends",
#   "ec__heard_neg_from_media",
#   "ec__heard_neg_from_friends",
#   "ec__is_a_good_organization",
#   "ec__strong_positive_feelings",
#   "ec__is_transparent_and_clear",
#   "ec__one_of_the_best",
#   "ec__addresses_problems_i_t_m",
#   "wave"
# )
#
#
# power_bi_donor_multiple_choice <- purrr::map_df(item_names, ~{
#   table_mc_df <- create_perc_table_mc(
#     data = survey_data_donor,
#     variable = .x,
#     grouping_vars = c(
#       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
#     )
#   )
#
#   table_mc_df$response_var_used <- {{.x}}
#
#   table_mc_df
# })
#
# power_bi_donor_multiple_choice_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/power_bi_donor_multiple_choice.csv")
# readr::write_csv(power_bi_donor_multiple_choice, power_bi_donor_multiple_choice_path)
#
# # writeData(wb, "Multiple Choice", table_mc)
#
# SELECT_ALL_VARS <- c(
#   "wwcs__",
#   "wns__",
#   "wwctsa__",
#   "h_c__"
# )
#
# power_bi_donor_select_all <- purrr::map_df(SELECT_ALL_VARS, ~{
#   create_perc_table_sa(
#     data = survey_data_donor,
#     item_set_selector = .x,
#     grouping_vars = c(
#       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
#     )
#   )
# })
#
# power_bi_donor_select_all_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/power_bi_donor_select_all.csv")
# readr::write_csv(power_bi_donor_select_all, power_bi_donor_select_all_path)
#
#
#
#
#
# numeric_variables <- survey_data_donor %>%
#   select( starts_with("NUM__ec_")) %>%
#   colnames()
#
# power_bi_donor_descr_table_num <- purrr::map_df(numeric_variables, ~{
#   create_descr_table_num(
#     data = survey_data_donor,
#     variable = .x,
#     grouping_vars = c(
#       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
#     )
#   )
# })
#
# power_bi_donor_descr_table_num_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/power_bi_donor_descr_table_num.csv")
# readr::write_csv(power_bi_donor_descr_table_num, power_bi_donor_descr_table_num_path)
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
# #
# #
# #
# #
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# #
# # # Published Version Below
# #
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# # ################################################################################
# #
# #
# #
# # # Import the package that contains the functions this doc will be explaining.
# # library(datacollectiontools)
# #
# # sum2 <- function(x) {
# #   x <- as.numeric(x)
# #   sum(x, na.rm = TRUE)
# # }
# #
# # # Create a function that processes the data
# # create_perc_table_mc <- function(data, variable, grouping_vars){
# #   # Create the name of the reponse_variable's order var
# #   order_variable <- paste0("VAR_ORDER__", {{variable}})
# #
# #   # Rename the variable as all
# #   data$all <- data[[{{variable}}]]
# #
# #   # Obtain the order for all
# #   data$VAR_ORDER__all <- data[[{{order_variable}}]]
# #
# #   # grouping_vars <- c(grouping_vars)
# #
# #   # Map through the grouping variables
# #   df <- purrr::map_df(grouping_vars, ~ {
# #     # Create the name of the grouping_variable's order var
# #     order_grouping_var <- paste0("VAR_ORDER__", {{.x}})
# #
# #     data1 <- data %>%
# #       # Rename these variables so that we can combine everything into the same column
# #       dplyr::mutate(
# #         grouping_var_levels = as.character(.data[[{{.x}}]]),
# #         response_var_levels = as.character(.data[[{{variable}}]]),
# #         grouping_var_order = .data[[order_grouping_var]],
# #         response_var_order = .data[[{{order_variable}}]],
# #       ) %>%
# #       dplyr::filter(!is.na(grouping_var_levels), !is.na(response_var_levels)) %>%
# #       # Group by these variables
# #       dplyr::group_by(
# #         wave,
# #         grouping_var_levels,
# #         response_var_levels,
# #         grouping_var_order,
# #         response_var_order) %>%
# #       # Get the counts for the grouping variable by the response variables
# #       dplyr::summarize(counts = n(), .groups = "drop") %>%
# #       # group just by the grouping var
# #       dplyr::group_by(grouping_var_levels) %>%
# #       dplyr::mutate(
# #         # Get the number you will use to calculate the percentage
# #         group_n = sum(counts),
# #         # Calculate the percentages
# #         percentage = (counts / group_n),
# #         # Label the grouping variable
# #         grouping_var_used = {{.x}}) %>%
# #       ungroup()
# #
# #     # Get the order for the grouping variables
# #     data1$grouping_var_used_order <- which(grouping_vars == {{.x}})
# #
# #     data1
# #   })
# #
# #   df_all <- data %>%
# #     # Rename the response variable and the order of those vars
# #     dplyr::mutate(
# #       response_var_levels = as.character(.data[[{{variable}}]]),
# #       response_var_order = .data[[{{order_variable}}]]
# #     ) %>%
# #     dplyr::filter(!is.na(response_var_levels)) %>%
# #     # Group by these variables
# #     dplyr::group_by(
# #       wave,
# #       response_var_levels,
# #       response_var_order) %>%
# #     # Calculate the counts
# #     dplyr::summarize(counts = n(), .groups = "drop") %>%
# #     dplyr::mutate(
# #       # Get the group count
# #       group_n = sum(counts),
# #       # Calculate the percentage
# #       percentage = (counts / group_n),
# #       # Name the variable all
# #       grouping_var_used = "All")
# #
# #   df_all$grouping_var_levels <- "All"
# #   df_all$grouping_var_order <- 1
# #   df_all$grouping_var_used_order <- 0
# #
# #   # Order the variables
# #   df_all <- df_all%>%
# #     dplyr::select(
# #       wave, grouping_var_levels, response_var_levels,
# #       grouping_var_order, response_var_order,
# #       counts, group_n, percentage, grouping_var_used,
# #       grouping_var_used_order
# #     )
# #
# #   # Combine the two data frames
# #   df <- df %>% rbind(df_all)
# #
# #   # # Recalculate the grouping var order
# #   # df$grouping_var_order <- (df$grouping_var_used_order * 10) + df$grouping_var_order
# #
# #   return(df)
# # }
# #
# # ################################################################################
# #
# # create_perc_table_sa <- function(data, item_set_selector, grouping_vars) {
# #
# #   # Obtain the variable names for this question
# #   list_of_variable_names <- data %>%
# #     dplyr::select(tidyselect::starts_with({{item_set_selector}})) %>%
# #     colnames()
# #
# #   prefix <- paste0({{item_set_selector}})
# #   # # Check if all columns are missing data
# #   # is_all_column_data_missing_for_row <- check_if_all_are_missing_from_row(data, list_of_variable_names)
# #
# #   #loop through the grouping variables
# #   processed_df <- purrr::map_df(grouping_vars, ~{
# #
# #     order_grouping_var <- paste0("VAR_ORDER__", {{..1}})
# #
# #     count_df <- data %>%
# #       dplyr::mutate(
# #         grouping_var_levels = .data[[{{..1}}]],
# #         grouping_var_order = .data[[order_grouping_var]]
# #       ) %>%
# #       dplyr::select("wave", grouping_var_levels, grouping_var_order, all_of(list_of_variable_names))%>%
# #
# #       tidyr::pivot_longer(
# #         cols = -c("wave", "grouping_var_levels", "grouping_var_order"),
# #         names_to = "response_var_levels",
# #         names_prefix = prefix,
# #         values_to = "counts"
# #       )  %>%
# #       dplyr::filter(!is.na(counts)) %>%
# #       dplyr::group_by(
# #         .data[["wave"]],
# #         .data[["grouping_var_levels"]],
# #         .data[["grouping_var_order"]],
# #         .data[["response_var_levels"]]
# #       ) %>%
# #       dplyr::summarize(counts = sum(counts))
# #
# #     count_df$grouping_var_used = {{..1}}
# #     count_df$grouping_var_used_order <- which(grouping_vars == {{..1}})
# #     count_df$grouping_var_levels <- as.character(count_df$grouping_var_levels)
# #
# #     count_df
# #
# #     n_df <- data %>%
# #       dplyr::mutate(
# #         grouping_var_levels = .data[[{{..1}}]],
# #         grouping_var_order = .data[[order_grouping_var]]) %>%
# #       dplyr::select("wave", grouping_var_levels, grouping_var_order, all_of(list_of_variable_names)) %>%
# #       tidyr::pivot_longer(
# #         cols = -c("wave", "grouping_var_levels", "grouping_var_order"),
# #         names_to = "response_var_levels",
# #         names_prefix = prefix,
# #         values_to = "group_n"
# #       ) %>%
# #       dplyr::filter(!is.na(group_n)) %>%
# #       dplyr::group_by(
# #         .data[["wave"]],
# #         .data[["grouping_var_levels"]],
# #         .data[["grouping_var_order"]],
# #         .data[["response_var_levels"]]
# #       ) %>%
# #       dplyr::summarize(group_n = n())
# #
# #     percentage_df <- count_df %>%
# #       dplyr::left_join(n_df, by = c("wave", "grouping_var_levels", "response_var_levels", "grouping_var_order")) %>%
# #       dplyr::mutate(percentage = counts / group_n)
# #
# #     percentage_df$percentage <- ifelse(percentage_df$counts == 0 & percentage_df$group_n == 0, 0, percentage_df$percentage)
# #
# #     percentage_df
# #   })
# #
# #   all_count_df <- data %>%
# #
# #     dplyr::select("wave", all_of(list_of_variable_names)) %>%
# #
# #     tidyr::pivot_longer(
# #       cols = -c("wave"),
# #       names_to = "response_var_levels",
# #       names_prefix = prefix,
# #       values_to = "counts"
# #     )  %>%
# #     dplyr::filter(!is.na(counts)) %>%
# #     dplyr::group_by(
# #       .data[["wave"]],
# #       .data[["response_var_levels"]]
# #     ) %>%
# #     dplyr::summarize(counts = sum(counts)) %>%
# #     dplyr::mutate(grouping_var_levels = "All",
# #                   grouping_var_order = 0,
# #                   grouping_var_used = "All",
# #                   grouping_var_used_order = 0)
# #
# #
# #
# #   all_n_df <- data %>%
# #     dplyr::select("wave", all_of(list_of_variable_names)) %>%
# #     tidyr::pivot_longer(
# #       cols = -c("wave"),
# #       names_to = "response_var_levels",
# #       names_prefix = prefix,
# #       values_to = "group_n"
# #     ) %>%
# #     dplyr::filter(!is.na(group_n)) %>%
# #     dplyr::group_by(
# #       .data[["wave"]],
# #       .data[["response_var_levels"]]
# #     ) %>%
# #     dplyr::summarize(group_n = n())
# #
# #   all_percentage_df <- all_count_df %>%
# #     dplyr::left_join(all_n_df, by = c("wave", "response_var_levels")) %>%
# #     dplyr::mutate(percentage = counts / group_n)
# #
# #   all_percentage_df$percentage <- ifelse(all_percentage_df$counts == 0 & all_percentage_df$group_n == 0, 0, all_percentage_df$percentage)
# #
# #
# #
# #
# #   all_percentage_df$response_var_used = {{item_set_selector}}
# #   all_percentage_df$response_var_order <- NA
# #
# #   all_percentage_df <- all_percentage_df %>%
# #     dplyr::select(
# #       wave, grouping_var_levels, response_var_levels, grouping_var_order,
# #       response_var_order, counts, group_n, percentage,
# #       grouping_var_used, grouping_var_used_order, response_var_used
# #     )
# #
# #   processed_df$response_var_used = {{item_set_selector}}
# #   processed_df$response_var_order <- NA
# #
# #   processed_df <- processed_df %>%
# #     dplyr::select(
# #       wave, grouping_var_levels, response_var_levels, grouping_var_order,
# #       response_var_order, counts, group_n, percentage,
# #       grouping_var_used, grouping_var_used_order, response_var_used
# #     )
# #
# #   processed_df <- processed_df %>% rbind(all_percentage_df)
# #
# #   processed_df$response_var_levels  <- stringr::str_replace_all(processed_df$response_var_levels, "_", " ") %>%
# #     stringr::str_to_title()
# #
# #   processed_df$response_var_order <- as.numeric(factor(processed_df$response_var_levels))
# #
# #   processed_df$grouping_var_order <- (processed_df$grouping_var_used_order * 10) + processed_df$grouping_var_order
# #
# #   return(processed_df)
# # }
# #
# #
# # create_descr_table_num <- function(data, variable, grouping_vars) {
# #
# #   # Create the name of the reponse_variable's order var
# #   order_variable <- paste0("VAR_ORDER__", {{variable}})
# #
# #   # Rename the variable as all
# #   data$all <- data[[{{variable}}]]
# #
# #   # # Obtain the order for all
# #   # data$VAR_ORDER__all <- data[[{{order_variable}}]]
# #
# #   # grouping_vars <- c(grouping_vars)
# #
# #   # Map through the grouping variables
# #   df <- purrr::map_df(grouping_vars, ~ {
# #     # Create the name of the grouping_variable's order var
# #     order_grouping_var <- paste0("VAR_ORDER__", {{.x}})
# #
# #     data1 <- data %>%
# #       # Rename these variables so that we can combine everything into the same column
# #       dplyr::mutate(
# #         grouping_var_levels = as.character(.data[[{{.x}}]]),
# #         grouping_var_order = .data[[order_grouping_var]]) %>%
# #       # Group by these variables
# #       dplyr::group_by(
# #         wave,
# #         grouping_var_levels,
# #         grouping_var_order) %>%
# #
# #       dplyr::filter(!is.na(.data[[{{variable}}]])) %>%
# #       # Get the counts for the grouping variable by the response variables
# #       dplyr::summarize(
# #         mean = mean(.data[[{{variable}}]], na.rm = TRUE),
# #         sd = sd(.data[[{{variable}}]], na.rm = TRUE),
# #         n = n(),
# #         se = sd/sqrt(n),
# #         ci_limit = se * 1.96,
# #         ci_upper = mean + ci_limit,
# #         ci_lower = mean - ci_limit)
# #     # Get the order for the grouping variables
# #     data1$grouping_var_used <- {{.x}}
# #
# #     data1$grouping_var_used_order <- which(grouping_vars == {{.x}})
# #
# #     data1
# #   })
# #
# #   df_all <- data %>%
# #     dplyr::mutate(
# #       grouping_var_levels = "All",
# #       grouping_var_order = 0) %>%
# #     # Group by these variables
# #     dplyr::group_by(
# #       wave,
# #       grouping_var_levels,
# #       grouping_var_order) %>%
# #     dplyr::filter(!is.na(.data[[{{variable}}]])) %>%
# #     # Calculate the counts
# #     dplyr::summarize(
# #       mean = mean(.data[[{{variable}}]], na.rm = TRUE),
# #       sd = sd(.data[[{{variable}}]], na.rm = TRUE),
# #       n = n(),
# #       se = sd/sqrt(n),
# #       ci_limit = se * 1.96,
# #       ci_upper = mean + ci_limit,
# #       ci_lower = mean - ci_limit)
# #
# #   df_all$grouping_var_used <- "All"
# #   df_all$grouping_var_levels <- "All"
# #   df_all$grouping_var_order <- 0
# #   df_all$grouping_var_used_order <- 0
# #
# #   # Order the variables
# #   df_all <- df_all %>%
# #     dplyr::select(
# #       wave, grouping_var_levels, grouping_var_order,
# #       mean, sd, n, se, ci_limit, ci_upper, ci_lower,
# #       grouping_var_used,
# #       grouping_var_used_order
# #     )
# #
# #   # Combine the two data frames
# #   df <- df %>% rbind(df_all)
# #
# #   # Recalculate the grouping var order
# #   df$grouping_var_order <- (df$grouping_var_used_order * 10) + df$grouping_var_order
# #
# #   return(df)
# # }
# #
# #
# # # Folder Set-Up -----------------------------------------------------------
# #
# # # Create the paths for the folders that need to exist for the survey project
# # survey_folder_structure_paths_list <- create_survey_folder_structure_paths_list(
# #   project_year = 2024,
# #   project_folder_name = "SDZ_BC__Quarterly Survey_Wave_2",
# #   list_of_survey_version_names = list("Wave 1 - Member", "Wave 1 - Donor")
# # )
# # #
# # # set_project_working_directory(
# # #     storage_platform = "dropbox",
# # #     storage_platform_name = "TCM Dropbox",
# # #     group_dir_name = "04 MDM Neuro-Fundraising Lab",
# # #     jobs_folder_name = "00 Jobs",
# # #     project_year = 2024,
# # #     project_folder_name = "SDZ_BC__Quarterly Survey_Wave_2"
# # # )
# #
# # # # Print the paths so you can see which are created
# # # print(survey_folder_structure_paths_list)
# #
# # # If theses do not exist, create them.
# # create_survey_folders_if_they_do_not_exist(survey_folder_structure_paths_list)
# #
# # # Point to where the survey data is located
# # survey_directory_path <- survey_folder_structure_paths_list$project_data_collection_survey_monkey_data_folder_location
# # home_dir <- survey_folder_structure_paths_list$home_dir
# #
# # # Read and Process Data ---------------------------------------------------
# #
# # # Read the survey data as a list
# # survey_data <- read_survey_data(survey_directory_path = survey_directory_path)
# #
# # # # Filter out bad data
# # # survey_data <- purrr::map(survey_data, ~{
# # #   df <- remove_rids_from_df(survey_directory_path, df = .x)
# # #   df
# # # })
# #
# # # Obtain the version names
# # version_name <- names(survey_data)
# #
# # # Label the different version types
# # MEMBER_VERSION <- stringr::str_detect(version_name, "Member")
# # DONOR_VERSION <- stringr::str_detect(version_name, "Donor")
# #
# # # Filter it into two different datasets
# # survey_data_member <- survey_data[MEMBER_VERSION] %>% purrr::map_df(~.x)
# # survey_data_donor <- survey_data[DONOR_VERSION] %>% purrr::map_df(~.x)
# #
# # # # Filter only for good RIDS: usually used at the end
# # # # good_rids <- readr::read_csv(paste0(survey_directory_path, "/good_rids.csv"))
# # # # survey_data <- survey_data %>%
# # # #   dplyr::filter(RID %in% good_rids$rids)
# #
# # # Adjust Data Types -------------------------------------------------------
# #
# # survey_data_member$wave <- stringr::str_extract(survey_data_member$version_name, "Wave [0-9]{1,2}")
# # survey_data_member$study_version <- stringr::str_extract(survey_data_member$version_name, "(Member)|(Donor)")
# #
# # # survey_data_member$ethnicity <- ifelse(is.na(survey_data_member$ethnicity), "Other (please specify)", as.character(survey_data_member$ethnicity))
# #
# # survey_data_member$age <- as.numeric(survey_data_member$age)
# # survey_data_member$age_group <- dplyr::case_when(
# #   survey_data_member$age >= 18 & survey_data_member$age < 40 ~ "18 - 39",
# #   survey_data_member$age >= 40 & survey_data_member$age < 60 ~ "40 - 59",
# #   survey_data_member$age >= 60 & survey_data_member$age < 101 ~ "60 - 100"
# # )
# #
# # # Get named vectors
# # path_to_named_vectors_workbook <- paste0(survey_directory_path, "/named_vectors.xlsx")
# #
# # named_vectors_workbook_sheets <- readxl::excel_sheets(path_to_named_vectors_workbook)
# #
# # named_vectors_list <- purrr::map(named_vectors_workbook_sheets, ~{
# #   df <- readxl::read_excel(path_to_named_vectors_workbook, sheet = .x)
# #
# #   named_vector <- df$vector_of_factor_levels
# #
# #   names(named_vector) <- df$names_of_factor_levels
# #
# #   return(named_vector)
# # }) %>% purrr::set_names(named_vectors_workbook_sheets)
# #
# # # create a list that contains all variables that you want to turn into factors
# # member_variables_to_reorder <- list(
# #   "length_of_membership",
# #   "number_of_visits_last_year",
# #   "number_of_planned_visits",
# #   "why_originally_became_member",
# #   "fuob__tour_or_tram",
# #   "fuob__support_conservation",
# #   "fuob__wild_perks_discounts",
# #   "fuob__admission_discount",
# #   "fuob__journal_subscription",
# #   "vob__tour_or_tram",
# #   "vob__support_conservation",
# #   "vob__wild_perks_discounts",
# #   "vob__admission_discount",
# #   "vob__journal_subscription",
# #   "ad_most_influenced",
# #   "likelihood_of_renewing",
# #   "method_of_renewal",
# #   "familiarity_of_efforts",
# #   "confidence_in_saving_species",
# #   "preferred_habitats",
# #   "donated_to_sdzwa_in_past",
# #   "considering_upgrading",
# #   "net_promoter",
# #   "gender",
# #   "age_group",
# #   "ethnicity",
# #   "marital_status",
# #   "annual_household_income",
# #   "ec__relates_to_me_personally",
# #   "ec__relates_to_f_and_f",
# #   "ec__deserves_my_support",
# #   "ec__made_substantial_progress",
# #   "ec__is_trustworthy",
# #   "ec__uses_donations_responsibly",
# #   "ec__is_compassionate_and_caring",
# #   "ec__is_innovative_and_a_leader",
# #   "ec__is_doing_enough",
# #   "ec__improve_lives_like_mine",
# #   "ec__does_unique_important_work",
# #   "ec__heard_pos_from_media",
# #   "ec__heard_pos_from_friends",
# #   "ec__heard_neg_from_media",
# #   "ec__heard_neg_from_friends",
# #   "ec__is_a_good_organization",
# #   "ec__strong_positive_feelings",
# #   "ec__is_transparent_and_clear",
# #   "ec__one_of_the_best",
# #   "ec__addresses_problems_i_t_m",
# #   "wave")
# #
# # # Create a second list that contains the named vector that specifies the appropriate levels
# # member_named_vectors <- list(
# #   named_vectors_list$length_of_membership,
# #   named_vectors_list$number_of_visits_last_year,
# #   named_vectors_list$number_of_planned_visits,
# #   named_vectors_list$why_originally_became_member,
# #   named_vectors_list$frequency_of_use,
# #   named_vectors_list$frequency_of_use,
# #   named_vectors_list$frequency_of_use,
# #   named_vectors_list$frequency_of_use,
# #   named_vectors_list$frequency_of_use,
# #   named_vectors_list$level_of_value,
# #   named_vectors_list$level_of_value,
# #   named_vectors_list$level_of_value,
# #   named_vectors_list$level_of_value,
# #   named_vectors_list$level_of_value,
# #   named_vectors_list$ad_most_influenced_you_to_join,
# #   named_vectors_list$likelihood_of_renewing,
# #   named_vectors_list$preferred_method_of_renewal,
# #   named_vectors_list$familiarity,
# #   named_vectors_list$confidence,
# #   named_vectors_list$preferred_habitats,
# #   named_vectors_list$donated_to_sdzwa_in_past,
# #   named_vectors_list$considering_upgrading,
# #   named_vectors_list$net_promoter,
# #   named_vectors_list$gender,
# #   named_vectors_list$age_group,
# #   named_vectors_list$ethnicity,
# #   named_vectors_list$marital_status,
# #   named_vectors_list$annual_household_income,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$wave
# # )
# #
# # # Convert variables to factors - Note that'ord' is an ordered factor
# # survey_data_member <- factor_vars_with_named_vectors(survey_data_member, member_variables_to_reorder, member_named_vectors)
# #
# # # Save the text vars to the correct location
# # survey_data_member$text_whtrm__other <- survey_data_member$whtrm__other
# # survey_data_member$text_wcctr__other <- survey_data_member$wcctr__other
# # survey_data_member$text_wyitr__other <- survey_data_member$wyitr__other
# # survey_data_member$text_fidtu__other <- survey_data_member$fidtu__other
# # survey_data_member$text_fmhtu__other <- survey_data_member$fmhtu__other
# # survey_data_member$text_wwctu__other <- survey_data_member$wwctu__other
# #
# # SELECT_ALL_VARS <- c(
# #   "whtrm__",
# #   "wcctr__",
# #   "wyitr__",
# #   "pmtrcn__",
# #   "fidtu__",
# #   "fmhtu__",
# #   "wwctu__",
# #   "h_c__"
# # )
# #
# # for(i in SELECT_ALL_VARS) {
# #   survey_data_member <- process_select_all_question(survey_data_member, i)
# # }
# #
# # survey_data_member <- survey_data_member %>%
# #   mutate(dplyr::across(where(is.factor), as.numeric, .names = "VAR_ORDER__{.col}"))
# #
# # survey_data_member <- survey_data_member %>%
# #   dplyr::filter(!is.na(annual_household_income))
# #
# # survey_data_member <- survey_data_member %>%
# #   mutate(
# #     across(
# #       c("net_promoter", starts_with("ec__")),
# #       as.numeric,
# #       .names = "NUM__{.col}")
# #   )
# #
# # survey_data_member$NUM__ec_total <- survey_data_member %>%
# #   select(starts_with("NUM__ec__")) %>%
# #   rowMeans()
# # ################################################################################
# #
# # clean_member_data_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/clean_member_data.csv")
# # readr::write_csv(survey_data_member, clean_member_data_path)
# #
# # ################################################################################
# #
# # text_survey_data_member <- survey_data_member %>%
# #   dplyr::select(
# #     respondent_id, start_date, end_date, wave,	study_version,
# #     gender,	age, age_group,	ethnicity,	text_ethnicity_other,
# #     marital_status, h_c__no_children,
# #     h_c__under_12,	h_c__12_to_17,
# #     h_c__18_to_65,	h_c__65_and_up,
# #     annual_household_income,	zip_code,
# #     text_originally_became_member,
# #     text_most_enjoyed,
# #     text_ad_most_influenced,
# #     text_whtrm__other,
# #     text_wcctr__other,
# #     text_what_other_organization,
# #     text_wyitr__other,
# #     text_method_of_renewal,
# #     text_fidtu__other,
# #     text_fmhtu__other,
# #     text_wwctu__other,
# #     text_why_net_promoter,
# #     text_any_other_feedback,
# #     text_ethnicity_other
# #   )
# #
# # ################################################################################
# #
# # text_survey_data_member_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/text_survey_data_member.csv")
# # readr::write_csv(text_survey_data_member, text_survey_data_member_path)
# #
# # ################################################################################
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# # # library(openxlsx)
# # #
# # # wb <- createWorkbook()
# # #
# # # addWorksheet(wb, "Textual Responses")
# #
# # power_bi_member_text <- text_survey_data_member %>%
# #   tidyr::pivot_longer(
# #     cols = -c(respondent_id, start_date, wave, study_version, gender, age, age_group,
# #               ethnicity, marital_status, h_c__no_children, h_c__under_12,
# #               h_c__12_to_17, h_c__18_to_65, h_c__65_and_up,
# #               annual_household_income, zip_code),
# #     names_to = "response_var_used",
# #     names_prefix = "text_",
# #     values_to = "textual_responses"
# #   ) %>%
# #   dplyr::filter(!is.na(textual_responses))
# #
# # power_bi_member_text_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/power_bi_member_text.csv")
# # readr::write_csv(power_bi_member_text, power_bi_member_text_path)
# #
# # # writeData(wb, "Textual Responses", textual_responses_df)
# #
# # item_names <- c(
# #   "length_of_membership",
# #   "number_of_visits_last_year",
# #   "number_of_planned_visits",
# #   "why_originally_became_member",
# #   "fuob__tour_or_tram",
# #   "fuob__support_conservation",
# #   "fuob__wild_perks_discounts",
# #   "fuob__admission_discount",
# #   "fuob__journal_subscription",
# #   "vob__tour_or_tram",
# #   "vob__support_conservation",
# #   "vob__wild_perks_discounts",
# #   "vob__admission_discount",
# #   "vob__journal_subscription",
# #   "ad_most_influenced",
# #   "likelihood_of_renewing",
# #   "method_of_renewal",
# #   "familiarity_of_efforts",
# #   "confidence_in_saving_species",
# #   "preferred_habitats",
# #   "donated_to_sdzwa_in_past",
# #   "considering_upgrading",
# #   "net_promoter",
# #   "gender",
# #   "age_group",
# #   "ethnicity",
# #   "marital_status",
# #   "annual_household_income",
# #   "ec__relates_to_me_personally",
# #   "ec__relates_to_f_and_f",
# #   "ec__deserves_my_support",
# #   "ec__made_substantial_progress",
# #   "ec__is_trustworthy",
# #   "ec__uses_donations_responsibly",
# #   "ec__is_compassionate_and_caring",
# #   "ec__is_innovative_and_a_leader",
# #   "ec__is_doing_enough",
# #   "ec__improve_lives_like_mine",
# #   "ec__does_unique_important_work",
# #   "ec__heard_pos_from_media",
# #   "ec__heard_pos_from_friends",
# #   "ec__heard_neg_from_media",
# #   "ec__heard_neg_from_friends",
# #   "ec__is_a_good_organization",
# #   "ec__strong_positive_feelings",
# #   "ec__is_transparent_and_clear",
# #   "ec__one_of_the_best",
# #   "ec__addresses_problems_i_t_m",
# #   "wave"
# # )
# #
# # # addWorksheet(wb, "Multiple Choice")
# #
# # power_bi_member_multiple_choice <- purrr::map_df(item_names, ~{
# #   table_mc_df <- create_perc_table_mc(
# #     data = survey_data_member,
# #     variable = .x,
# #     grouping_vars = c(
# #       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
# #     )
# #   )
# #
# #   table_mc_df$response_var_used <- {{.x}}
# #   table_mc_df$response_var_used_order <- which(item_names == {{.x}})
# #
# #   table_mc_df
# # })
# #
# # power_bi_member_multiple_choice <- power_bi_member_multiple_choice %>%
# #   dplyr::filter(!is.na(response_var_levels)) %>%
# #   dplyr::filter(!is.na(grouping_var_levels))
# #
# # response_var_multiplier <- dplyr::case_when(
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(1) ~ 1,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(2, 3) ~ 2,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(4) ~ 3,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(5,6,7,8,9) ~ 4,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(10, 11, 12, 13, 14) ~ 5,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(15) ~ 6,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(16) ~ 7,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(17) ~ 8,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(18) ~ 9,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(19) ~ 10,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(20) ~ 11,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(21) ~ 12,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(22) ~ 13,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(23) ~ 14,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(24) ~ 15,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(25) ~ 16,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(26) ~ 17,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(27) ~ 18,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(28) ~ 19,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(29:48) ~ 20,
# #   power_bi_member_multiple_choice$response_var_used_order %in% c(49) ~ 21)
# #
# # power_bi_member_multiple_choice$response_var_order <- power_bi_member_multiple_choice$response_var_order + (response_var_multiplier * 10)
# # power_bi_member_multiple_choice$grouping_var_order <- power_bi_member_multiple_choice$grouping_var_order + (power_bi_member_multiple_choice$grouping_var_used_order * 10)
# #
# # power_bi_member_multiple_choice$response_var_order <- ifelse(
# #   power_bi_member_multiple_choice$response_var_levels == "Other",
# #   max(power_bi_member_multiple_choice$response_var_order) + 1,
# #   power_bi_member_multiple_choice$response_var_order
# # )
# #
# # power_bi_member_multiple_choice$grouping_var_order <- ifelse(
# #   power_bi_member_multiple_choice$grouping_var_levels == "Other",
# #   max(power_bi_member_multiple_choice$grouping_var_order) + 1,
# #   power_bi_member_multiple_choice$grouping_var_order
# # )
# #
# # power_bi_member_multiple_choice$grouping_var_used  <- stringr::str_replace_all(power_bi_member_multiple_choice$grouping_var_used, "_", " ") %>%
# #   stringr::str_to_title()
# #
# # power_bi_member_multiple_choice$response_var_used  <- stringr::str_replace_all(power_bi_member_multiple_choice$response_var_used, "_", "") %>%
# #   stringr::str_to_lower()
# #
# #
# # power_bi_member_multiple_choice_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/power_bi_member_multiple_choice.csv")
# # readr::write_csv(power_bi_member_multiple_choice, power_bi_member_multiple_choice_path)
# #
# #
# #
# # # power_bi_member_multiple_choice %>%
# # #   group_by(response_var_order, response_var_levels) %>%
# # #   count() %>% View()
# # #
# # # unique(power_bi_member_multiple_choice$response_var_order) %>% length()
# # # unique(power_bi_member_multiple_choice$response_var_levels) %>% length()
# # #
# # # power_bi_member_multiple_choice %>%
# # #   group_by(grouping_var_order, grouping_var_levels) %>%
# # #   count() %>% View()
# # #
# # # unique(power_bi_member_multiple_choice$grouping_var_order) %>% length()
# # # unique(power_bi_member_multiple_choice$grouping_var_levels) %>% length()
# # # writeData(wb, "Multiple Choice", table_mc)
# #
# # SELECT_ALL_VARS <- c(
# #   "whtrm__",
# #   "wcctr__",
# #   "wyitr__",
# #   "pmtrcn__",
# #   "fidtu__",
# #   "fmhtu__",
# #   "wwctu__",
# #   "h_c__"
# # )
# #
# # power_bi_member_select_all <- purrr::map_df(SELECT_ALL_VARS, ~{
# #   table_sa_df <- create_perc_table_sa(
# #     data = survey_data_member,
# #     item_set_selector = .x,
# #     grouping_vars = c(
# #       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
# #     )
# #   )
# #
# #   table_sa_df$response_var_used <- {{.x}}
# #   table_sa_df$response_var_used_order <- which(SELECT_ALL_VARS == {{.x}})
# #
# #
# #   table_sa_df
# # })
# #
# # power_bi_member_select_all <- power_bi_member_select_all %>%
# #   dplyr::filter(!is.na(response_var_levels))%>%
# #   dplyr::filter(!is.na(grouping_var_levels))
# #
# # power_bi_member_select_all$response_var_order <- ifelse(
# #   power_bi_member_select_all$response_var_levels == "Other",
# #   max(power_bi_member_select_all$response_var_order) + 1,
# #   power_bi_member_select_all$response_var_order
# # )
# #
# # power_bi_member_select_all$grouping_var_order <- ifelse(
# #   power_bi_member_select_all$grouping_var_levels == "Other",
# #   max(power_bi_member_select_all$grouping_var_order) + 1,
# #   power_bi_member_select_all$grouping_var_order
# # )
# #
# # power_bi_member_select_all$response_var_order <- power_bi_member_select_all$response_var_order + (power_bi_member_select_all$response_var_used_order * 10)
# #
# # response_levels_check <- power_bi_member_select_all %>%
# #   group_by(response_var_order, response_var_levels) %>%
# #   count() %>%
# #   pull(response_var_levels)
# #
# # duplicate_indicator <- response_levels_check %>%
# #   duplicated()
# #
# # response_level_duplicates <- response_levels_check[duplicate_indicator]
# #
# # for(i in seq_along(response_level_duplicates)) {
# #   power_bi_member_select_all$response_var_order <- ifelse(
# #     power_bi_member_select_all$response_var_levels == response_level_duplicates[[i]],
# #     max(power_bi_member_select_all$response_var_order) + i,
# #     power_bi_member_select_all$response_var_order)
# # }
# #
# #
# # power_bi_member_select_all$grouping_var_used  <- stringr::str_replace_all(power_bi_member_select_all$grouping_var_used, "_", " ") %>%
# #   stringr::str_to_title()
# #
# # power_bi_member_select_all$response_var_used  <- stringr::str_replace_all(power_bi_member_select_all$response_var_used, "_", "") %>%
# #   stringr::str_to_lower()
# #
# #
# # power_bi_member_select_all$response_var_used <- dplyr::case_when(
# #   power_bi_member_select_all$response_var_used == "whtrm" ~ "Which of the following options best describes why you are hesitant to renew membership?",
# #   power_bi_member_select_all$response_var_used == "wcctr" ~ "Which of the following options best describes what would convince you to renew your San Diego Zoo and San Diego Zoo Safari Park membership?",
# #   power_bi_member_select_all$response_var_used == "wyitr" ~ "Which of the following best describes why you intend to renew your San Diego Zoo and San Diego Zoo Safari Park membership for the next year?",
# #   power_bi_member_select_all$response_var_used == "pmtrcn" ~ "How would you prefer to receive information on San Diego Zoo Wildlife Alliance’s conservation work?",
# #   power_bi_member_select_all$response_var_used == "fidtu" ~ "Which of these factors influenced your decision to upgrade to a higher membership level?",
# #   power_bi_member_select_all$response_var_used == "fmhtu" ~ "Which of these factors make you hesitant to upgrade to a higher membership level?",
# #   power_bi_member_select_all$response_var_used == "wwctu" ~ "What would convince you to upgrade to a higher membership level?",
# #   power_bi_member_select_all$response_var_used == "hc" ~ "Which describes your household composition?",
# # )
# #
# # power_bi_member_select_all_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/power_bi_member_select_all.csv")
# # readr::write_csv(power_bi_member_select_all, power_bi_member_select_all_path)
# #
# #
# # numeric_variables <- survey_data_member %>%
# #   select( starts_with("NUM__ec_")) %>%
# #   colnames()
# #
# # power_bi_member_descr_table_num <- purrr::map_df(numeric_variables, ~{
# #   table_num_df <- create_descr_table_num(
# #     data = survey_data_member,
# #     variable = .x,
# #     grouping_vars = c(
# #       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
# #     )
# #   )
# #
# #   table_num_df$response_var_used <- {{.x}}
# #   table_num_df$response_var_used_order <- which(numeric_variables == {{.x}})
# #
# #
# #   table_num_df
# # })
# #
# #
# # power_bi_member_descr_table_num$grouping_var_order <- ifelse(
# #   power_bi_member_descr_table_num$grouping_var_levels == "Other",
# #   max(power_bi_member_descr_table_num$grouping_var_order) + 1,
# #   power_bi_member_descr_table_num$grouping_var_order
# # )
# #
# #
# # grouping_levels_check <- power_bi_member_descr_table_num %>%
# #   group_by(grouping_var_order, grouping_var_levels) %>%
# #   count() %>%
# #   pull(grouping_var_levels)
# #
# # duplicate_indicator <- grouping_levels_check %>%
# #   duplicated()
# #
# # grouping_level_duplicates <- grouping_levels_check[duplicate_indicator]
# #
# # for(i in seq_along(grouping_level_duplicates)) {
# #   power_bi_member_descr_table_num$grouping_var_order <- ifelse(
# #     power_bi_member_descr_table_num$grouping_var_levels == grouping_level_duplicates[[i]],
# #     max(power_bi_member_descr_table_num$grouping_var_order) + i,
# #     power_bi_member_descr_table_num$grouping_var_order)
# # }
# #
# # #
# # #
# # # power_bi_member_multiple_choice$response_var_order <- power_bi_member_multiple_choice$response_var_order + (response_var_multiplier * 10)
# # # power_bi_member_multiple_choice$grouping_var_order <- power_bi_member_multiple_choice$grouping_var_order + (power_bi_member_multiple_choice$grouping_var_used_order * 10)
# # #
# #
# # power_bi_member_descr_table_num$grouping_var_order <- ifelse(
# #   power_bi_member_descr_table_num$grouping_var_levels == "Other",
# #   max(power_bi_member_descr_table_num$grouping_var_order) + 1,
# #   power_bi_member_descr_table_num$grouping_var_order
# # )
# #
# # power_bi_member_descr_table_num$grouping_var_used  <- stringr::str_replace_all(power_bi_member_descr_table_num$grouping_var_used, "_", " ") %>%
# #   stringr::str_to_title()
# #
# # power_bi_member_descr_table_num$response_var_used  <- stringr::str_replace_all(power_bi_member_descr_table_num$response_var_used, "NUM__EC__", "") %>%
# #   stringr::str_replace_all("_", " ") %>%
# #   stringr::str_to_title()
# #
# #
# #
# # power_bi_member_descr_table_num_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Member Version/Power_BI_Deck_Member/power_bi_member_descr_table_num.csv")
# # readr::write_csv(power_bi_member_descr_table_num, power_bi_member_descr_table_num_path)
# #
# #
# # #
# # # power_bi_member_descr_table_num %>%
# # #   group_by(grouping_var_order, grouping_var_levels) %>%
# # #   count() %>% View()
# # #
# # # unique(power_bi_member_descr_table_num$grouping_var_order) %>% length()
# # # unique(power_bi_member_descr_table_num$grouping_var_levels) %>% length()
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# # ################################################################################
# # ################################################################################
# #
# # survey_data_donor$wave <- stringr::str_extract(survey_data_donor$version_name, "Wave [0-9]{1,2}")
# # survey_data_donor$study_version <- stringr::str_extract(survey_data_donor$version_name, "(Member)|(Donor)")
# #
# # survey_data_donor$age_group <- dplyr::case_when(
# #   survey_data_donor$age >= 18 & survey_data_donor$age < 40 ~ "18 - 39",
# #   survey_data_donor$age >= 40 & survey_data_donor$age < 60 ~ "40 - 59",
# #   survey_data_donor$age >= 60 & survey_data_donor$age < 101 ~ "60 - 100"
# # )
# #
# # # Get named vectors
# # path_to_named_vectors_workbook <- paste0(survey_directory_path, "/named_vectors.xlsx")
# #
# # named_vectors_workbook_sheets <- readxl::excel_sheets(path_to_named_vectors_workbook)
# #
# # named_vectors_list <- purrr::map(named_vectors_workbook_sheets, ~{
# #   df <- readxl::read_excel(path_to_named_vectors_workbook, sheet = .x)
# #
# #   named_vector <- df$vector_of_factor_levels
# #
# #   names(named_vector) <- df$names_of_factor_levels
# #
# #   return(named_vector)
# # }) %>% purrr::set_names(named_vectors_workbook_sheets)
# #
# # # create a list that contains all variables that you want to turn into factors
# # donor_variables_to_reorder <- list(
# #   "first_donation_why",
# #   "confident_donation_will_help",
# #   "monthly_donor",
# #   "familiarity_with_donor_program",
# #   "considering_becoming_donor",
# #   "why_not_monthly_donor",
# #   "last_donation_how",
# #   "last_online_donation_how",
# #   "horci__email",
# #   "horci__mail",
# #   "horci__website",
# #   "horci__at_park",
# #   "horci__social_media",
# #   "horci__word_of_mouth",
# #   "support_sdzwa_again",
# #   "will_change_donation",
# #   "net_promoter",
# #   "gender",
# #   "age_group",
# #   "ethnicity",
# #   "marital_status",
# #   "annual_household_income",
# #   "ec__relates_to_me_personally",
# #   "ec__relates_to_f_and_f",
# #   "ec__deserves_my_support",
# #   "ec__made_substantial_progress",
# #   "ec__is_trustworthy",
# #   "ec__uses_donations_responsibly",
# #   "ec__is_compassionate_and_caring",
# #   "ec__is_innovative_and_a_leader",
# #   "ec__is_doing_enough",
# #   "ec__improve_lives_like_mine",
# #   "ec__does_unique_important_work",
# #   "ec__heard_pos_from_media",
# #   "ec__heard_pos_from_friends",
# #   "ec__heard_neg_from_media",
# #   "ec__heard_neg_from_friends",
# #   "ec__is_a_good_organization",
# #   "ec__strong_positive_feelings",
# #   "ec__is_transparent_and_clear",
# #   "ec__one_of_the_best",
# #   "ec__addresses_problems_i_t_m",
# #   "wave")
# #
# # # Create a second list that contains the named vector that specifies the appropriate levels
# # donor_named_vectors <- list(
# #   named_vectors_list$first_donation_why,
# #   named_vectors_list$confidence,
# #   named_vectors_list$yes_no,
# #   named_vectors_list$familiarity,
# #   named_vectors_list$likelihood_2,
# #   named_vectors_list$why_not_monthly_donor,
# #   named_vectors_list$last_donation_how,
# #   named_vectors_list$last_online_donation_how,
# #   named_vectors_list$frequency_of_info,
# #   named_vectors_list$frequency_of_info,
# #   named_vectors_list$frequency_of_info,
# #   named_vectors_list$frequency_of_info,
# #   named_vectors_list$frequency_of_info,
# #   named_vectors_list$frequency_of_info,
# #   named_vectors_list$likelihood,
# #   named_vectors_list$will_change_donation,
# #   named_vectors_list$net_promoter,
# #   named_vectors_list$gender,
# #   named_vectors_list$age_group,
# #   named_vectors_list$ethnicity,
# #   named_vectors_list$marital_status,
# #   named_vectors_list$annual_household_income,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$agreement,
# #   named_vectors_list$wave
# # )
# #
# # # Convert variables to factors - Note that'ord' is an ordered factor
# # survey_data_donor <- factor_vars_with_named_vectors(survey_data_donor, donor_variables_to_reorder, donor_named_vectors)
# #
# # # Save the text vars to the correct location
# # survey_data_donor$text_wwcs__other <- survey_data_donor$wwcs__other
# # survey_data_donor$text_wns__other <- survey_data_donor$wns__other
# # survey_data_donor$text_wwctsa__other <- survey_data_donor$wwctsa__other
# #
# # SELECT_ALL_VARS <- c(
# #   "wwcs__",
# #   "wns__",
# #   "wwctsa__",
# #   "h_c__"
# # )
# #
# # for(i in SELECT_ALL_VARS) {
# #   survey_data_donor <- process_select_all_question(survey_data_donor, i)
# # }
# #
# # survey_data_donor <- survey_data_donor %>%
# #   mutate(dplyr::across(where(is.factor), as.numeric, .names = "VAR_ORDER__{.col}"))
# #
# # survey_data_donor <- survey_data_donor %>%
# #   dplyr::filter(!is.na(annual_household_income))
# #
# # survey_data_donor <- survey_data_donor %>%
# #   mutate(
# #     across(
# #       c("net_promoter", starts_with("ec__")),
# #       as.numeric,
# #       .names = "NUM__{.col}")
# #   )
# #
# # survey_data_donor$NUM__ec_total <- survey_data_donor %>%
# #   select(starts_with("NUM__ec__")) %>%
# #   rowMeans()
# #
# # ################################################################################
# #
# # clean_donor_data_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/clean_donor_data.csv")
# # readr::write_csv(survey_data_donor, clean_donor_data_path)
# #
# # ################################################################################
# #
# # text_survey_data_donor <- survey_data_donor %>%
# #   dplyr::select(
# #     respondent_id, start_date, end_date, wave,	study_version,
# #     gender,	age, age_group,	ethnicity,	text_ethnicity_other,
# #     marital_status, h_c__no_children,
# #     h_c__under_12,	h_c__12_to_17,
# #     h_c__18_to_65,	h_c__65_and_up,
# #     annual_household_income,	zip_code,
# #     text_first_donation_why_other,
# #     text_why_not_monthly_donor,
# #     text_last_donation_how_other,
# #     text_last_online_donation_how,
# #     text_wwcs__other,
# #     text_wns__other,
# #     text_wwctsa__other,
# #     text_why_net_promoter,
# #     text_any_other_feedback,
# #     text_ethnicity_other
# #   )
# #
# # ################################################################################
# #
# # text_survey_data_donor_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/text_survey_data_donor.csv")
# # readr::write_csv(text_survey_data_donor, text_survey_data_donor_path)
# #
# #
# #
# #
# #
# #
# #
# #
# # power_bi_donor_text <- text_survey_data_member %>%
# #   tidyr::pivot_longer(
# #     cols = -c(respondent_id, start_date, wave, study_version, gender, age, age_group,
# #               ethnicity, marital_status, h_c__no_children, h_c__under_12,
# #               h_c__12_to_17, h_c__18_to_65, h_c__65_and_up,
# #               annual_household_income, zip_code),
# #     names_to = "response_var_used",
# #     names_prefix = "text_",
# #     values_to = "textual_responses"
# #   ) %>%
# #   dplyr::filter(!is.na(textual_responses))
# #
# # power_bi_donor_text_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/power_bi_donor_text.csv")
# # readr::write_csv(power_bi_donor_text, power_bi_donor_text_path)
# #
# #
# #
# # item_names <- c(
# #   "first_donation_why",
# #   "confident_donation_will_help",
# #   "monthly_donor",
# #   "familiarity_with_donor_program",
# #   "considering_becoming_donor",
# #   "why_not_monthly_donor",
# #   "last_donation_how",
# #   "last_online_donation_how",
# #   "horci__email",
# #   "horci__mail",
# #   "horci__website",
# #   "horci__at_park",
# #   "horci__social_media",
# #   "horci__word_of_mouth",
# #   "support_sdzwa_again",
# #   "will_change_donation",
# #   "net_promoter",
# #   "gender",
# #   "age_group",
# #   "ethnicity",
# #   "marital_status",
# #   "annual_household_income",
# #   "ec__relates_to_me_personally",
# #   "ec__relates_to_f_and_f",
# #   "ec__deserves_my_support",
# #   "ec__made_substantial_progress",
# #   "ec__is_trustworthy",
# #   "ec__uses_donations_responsibly",
# #   "ec__is_compassionate_and_caring",
# #   "ec__is_innovative_and_a_leader",
# #   "ec__is_doing_enough",
# #   "ec__improve_lives_like_mine",
# #   "ec__does_unique_important_work",
# #   "ec__heard_pos_from_media",
# #   "ec__heard_pos_from_friends",
# #   "ec__heard_neg_from_media",
# #   "ec__heard_neg_from_friends",
# #   "ec__is_a_good_organization",
# #   "ec__strong_positive_feelings",
# #   "ec__is_transparent_and_clear",
# #   "ec__one_of_the_best",
# #   "ec__addresses_problems_i_t_m",
# #   "wave"
# # )
# #
# #
# # power_bi_donor_multiple_choice <- purrr::map_df(item_names, ~{
# #   table_mc_df <- create_perc_table_mc(
# #     data = survey_data_donor,
# #     variable = .x,
# #     grouping_vars = c(
# #       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
# #     )
# #   )
# #
# #   table_mc_df$response_var_used <- {{.x}}
# #
# #   table_mc_df
# # })
# #
# # power_bi_donor_multiple_choice_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/power_bi_donor_multiple_choice.csv")
# # readr::write_csv(power_bi_donor_multiple_choice, power_bi_donor_multiple_choice_path)
# #
# # # writeData(wb, "Multiple Choice", table_mc)
# #
# # SELECT_ALL_VARS <- c(
# #   "wwcs__",
# #   "wns__",
# #   "wwctsa__",
# #   "h_c__"
# # )
# #
# # power_bi_donor_select_all <- purrr::map_df(SELECT_ALL_VARS, ~{
# #   create_perc_table_sa(
# #     data = survey_data_donor,
# #     item_set_selector = .x,
# #     grouping_vars = c(
# #       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
# #     )
# #   )
# # })
# #
# # power_bi_donor_select_all_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/power_bi_donor_select_all.csv")
# # readr::write_csv(power_bi_donor_select_all, power_bi_donor_select_all_path)
# #
# #
# #
# #
# #
# # numeric_variables <- survey_data_donor %>%
# #   select( starts_with("NUM__ec_")) %>%
# #   colnames()
# #
# # power_bi_donor_descr_table_num <- purrr::map_df(numeric_variables, ~{
# #   create_descr_table_num(
# #     data = survey_data_donor,
# #     variable = .x,
# #     grouping_vars = c(
# #       "wave", "gender", "age_group",  "ethnicity", "marital_status", "annual_household_income"
# #     )
# #   )
# # })
# #
# # power_bi_donor_descr_table_num_path <- paste0(home_dir, "/Analysis/Respondent Investigation/Donor Version/Power_BI_Deck_Donor/power_bi_donor_descr_table_num.csv")
# # readr::write_csv(power_bi_donor_descr_table_num, power_bi_donor_descr_table_num_path)
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
#
#
#
#
