# ################################################################################
#
# ################################################################################
# # Set up helper functions -------------------------------------------------
#
# # Import the package that contains the functions this doc will be explaining.
# library(datacollectiontools)
#
#
# if_cond_na_else_potential <- function(var, cond_string, potential) {
#   ifelse(var == {{cond_string}}, potential, NA)
# }
#
#
# if_not_cond_na_else_potential <- function(var, cond_string, potential) {
#   ifelse(var != {{cond_string}}, potential, NA)
# }
#
#
# # Create a function that will create random data for select all variables.
# create_fake_data_select_all_columns <- function(df, n_respondents, col_names, other_text_col = NULL, filter_var = NULL, filter_label_options = NULL) {
#   # Create random data for each column
#   response_options <- purrr::map_dfc(col_names, ~{
#     sample(0:1, n_respondents, replace = TRUE)
#   }) %>% purrr::set_names(col_names)
#   # If a text column is added, randomly create text when the text column indicates text should be there
#   if(!is.null(other_text_col)){
#     potential_strings <- stringi::stri_rand_lipsum(n_respondents)
#     new_name <- paste0("text_", {{other_text_col}})
#     response_options[new_name] <- ifelse(
#       response_options[[{{other_text_col}}]] == 1,
#       potential_strings,
#       NA
#     )
#   }
#   # If indicated, changed rows that match the condition to NAs
#   if(!is.null(filter_var) & !is.null(filter_label_options)){
#     should_loose <- !(df[[{{filter_var}}]] %in% c({{filter_label_options}}))
#     response_options[should_loose ,] <- NA
#   }
#   # Add the new columns to the df
#   df <- df %>% cbind(response_options)
#   # Return the df
#   return(df)
# }
#
#
# create_fake_data_multiple_choice <- function(df, n_respondents, var_name, response_options_named_list, other_text_col = NULL, other_text_cond = NULL, filter_var = NULL, filter_label_options = NULL) {
#   # Create random data for each column
#   response_options <- purrr::map_dfc({{var_name}}, ~{
#     sample(names({{response_options_named_list}}), n_respondents, replace = TRUE)
#   }) %>% purrr::set_names({{var_name}})
#
#   # If a text column is added, randomly create text when the text column indicates text should be there
#   if(!is.null(other_text_col)){
#     potential_strings <- stringi::stri_rand_lipsum(n_respondents)
#     response_options[[{{other_text_col}}]] <- ifelse(
#       response_options[[{{var_name}}]] == {{other_text_cond}},
#       potential_strings,
#       NA
#     )
#   }
#   # If indicated, changed rows that match the condition to NAs
#   if(!is.null(filter_var) & !is.null(filter_label_options)){
#     should_loose <- !(df[[{{filter_var}}]] %in% c({{filter_label_options}}))
#     response_options[should_loose ,] <- NA
#   }
#   # Add the new columns to the df
#   df <- df %>% cbind(response_options)
#   # Return the df
#   return(df)
# }
# ################################################################################
#
# ################################################################################
# # Create function that creates fake data for the sdwza member surv --------
#
# create_fake_data_for_sdwza_members <- function(n_respondents) {
#
#   respondent_id <- sample(100000000000:999999999999, n_respondents)
#
#   collector_id <- rep(123456789, n_respondents)
#
#   random_numbers_for_date <- seq(n_respondents + 499, 500)
#
#   random_dates <- Sys.time() - random_numbers_for_date
#
#   random_time_var <- sample(2:60, n_respondents, replace = TRUE)
#
#   random_dates_plus_time <- random_dates + (random_time_var * 60)
#
#   start_date <- format(random_dates, "%m/%d/%Y %H:%M")
#
#   end_date <- format(random_dates_plus_time, "%m/%d/%Y %H:%M")
#
#   ip_address <- rep("000.00.000.00", n_respondents)
#
#   email_address <- rep(NA, n_respondents)
#   first_name <- rep(NA, n_respondents)
#   last_name <- rep(NA, n_respondents)
#   custom_data_1 <- rep(NA, n_respondents)
#
#   dataframe <- tibble::tibble(
#     respondent_id,
#     collector_id,
#     start_date,
#     end_date,
#     ip_address,
#     email_address,
#     first_name,
#     last_name,
#     custom_data_1
#   )
#
#   ##############################################################################
#
#   named_vector_length_of_membership <- name_vector(
#     vector_of_factor_levels = c("Less than one year", "One to three years", "Three to five years", "More than five years"),
#     names_of_factor_levels = c("Less than one year", "One to three years", "Three to five years", "More than five years")
#   )
#
#   dataframe$length_of_membership <- sample(names(named_vector_length_of_membership), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   named_vector_number_of_visits_last_year <- name_vector(
#     vector_of_factor_levels = c("I did not visit the San Diego Zoo and San Diego Zoo Safari Park in the last 12 months", "1 - 3 times", "4 - 6 times", "7 - 9 times", "10+ times"),
#     names_of_factor_levels = c("None", "1 - 3 times", "4 - 6 times", "7 - 9 times", "10+ times")
#   )
#
#   potential_number_of_visits_last_year <- sample(names(named_vector_number_of_visits_last_year), n_respondents, replace = TRUE)
#
#   dataframe$number_of_visits_last_year <- ifelse(
#     dataframe$length_of_membership == "Less than one year",
#     NA,
#     potential_number_of_visits_last_year
#   )
#
#   ##############################################################################
#
#   named_vector_number_of_planned_visits <- name_vector(
#     vector_of_factor_levels = c("I do not plan to visit the San Diego Zoo and San Diego Zoo Safari Park in the coming 12 months", "1 - 3 times", "4 - 6 times", "7 - 9 times", "10+ times"),
#     names_of_factor_levels = c("None", "1 - 3 times", "4 - 6 times", "7 - 9 times", "10+ times")
#   )
#
#   dataframe$number_of_planned_visits <- sample(names(named_vector_number_of_planned_visits), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   named_vector_why_originally_became_member <- name_vector(
#     vector_of_factor_levels = c(
#       "Great value (e.g., admission to two parks, membership level benefits, etc.)",
#       "Safe activity during COVID",
#       "To support San Diego Zoo and San Diego Zoo Safari Park and ensure that future generations can enjoy them",
#       "I was looking for a family-friendly activity",
#       "I wanted to see animals from around the world",
#       "I wanted to support animal conservation efforts that are funded through the Zoo and Safari Park",
#       "None of the above: (Please provide your top reason)"
#     ),
#     names_of_factor_levels = c(
#       "Great value (e.g., admission to two parks, membership level benefits, etc.)",
#       "Safe activity during COVID",
#       "To support San Diego Zoo and San Diego Zoo Safari Park and ensure that future generations can enjoy them",
#       "I was looking for a family-friendly activity",
#       "I wanted to see animals from around the world",
#       "I wanted to support animal conservation efforts that are funded through the Zoo and Safari Park",
#       "None of the above: (Please provide your top reason)"
#     )
#   )
#
#
#   # named_vector_why_originally_became_member <- name_vector(
#   #   vector_of_factor_levels = c(
#   #     "Great value (e.g., admission to two parks, membership level benefits, etc.)",
#   #     "Safe activity during COVID",
#   #     "To support San Diego Zoo and San Diego Zoo Safari Park and ensure that future generations can enjoy them",
#   #     "I was looking for a family-friendly activity",
#   #     "I wanted to see animals from around the world",
#   #     "I wanted to support animal conservation efforts that are funded through the Zoo and Safari Park",
#   #     "None of the above: (Please provide your top reason)"
#   #   ),
#   #   names_of_factor_levels = c(
#   #     "Great value",
#   #     "Safe activity during COVID",
#   #     "To support the parks",
#   #     "Family-friendly activity",
#   #     "Wanted to see animals",
#   #     "To support animal conservation efforts",
#   #     "Other"
#   #   )
#   # )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "why_originally_became_member",
#     response_options_named_list = named_vector_why_originally_became_member,
#     other_text_col = "text_originally_became_member",
#     other_text_cond = "Other",
#     filter_var = NULL,
#     filter_label_options = NULL
#   )
#
#   ##############################################################################
#
#   named_vector_frequency_of_use <- name_vector(
#     vector_of_factor_levels = c(
#       "1 – Never Used",
#       "2 - Rarely Used",
#       "3 - Infrequently Used",
#       "4 - Occasionally Used",
#       "5 - Sometimes Used",
#       "6 - Frequently Used",
#       "7 – Very Frequently Used"
#     ),
#     names_of_factor_levels = c(
#       "1 – Never Used",
#       "2 - Rarely Used",
#       "3 - Infrequently Used",
#       "4 - Occasionally Used",
#       "5 - Sometimes Used",
#       "6 - Frequently Used",
#       "7 – Very Frequently Used"
#     )
#   )
#
#   # named_vector_frequency_of_use <- name_vector(
#   #   vector_of_factor_levels = c(
#   #     "1 – Never Used",
#   #     "2 - Rarely Used",
#   #     "3 - Infrequently Used",
#   #     "4 - Occasionally Used",
#   #     "5 - Sometimes Used",
#   #     "6 - Frequently Used",
#   #     "7 – Very Frequently Used"
#   #   ),
#   #   names_of_factor_levels = c(
#   #     "Never Used",
#   #     "Rarely Used",
#   #     "Infrequently Used",
#   #     "Occasionally Used",
#   #     "Sometimes Used",
#   #     "Frequently Used",
#   #     "Very Frequently Used"
#   #   )
#   # )
#
#   frequency_use_of_benefits_items <- c(
#     "frequency_use_of_benefits__tour_or_tram",
#     "frequency_use_of_benefits__support_global_conservation_efforts",
#     "frequency_use_of_benefits__wild_perks_discounts",
#     "frequency_use_of_benefits__50_perc_admission_discount",
#     "frequency_use_of_benefits__sdzwa_journal_subscription"
#   )
#
#   for(variable in frequency_use_of_benefits_items) {
#     dataframe <- create_fake_data_multiple_choice(
#       df = dataframe,
#       n_respondents = n_respondents,
#       var_name = variable,
#       response_options_named_list = named_vector_frequency_of_use
#     )
#   }
#
#   ##############################################################################
#
#   named_vector_level_of_value <- name_vector(
#     vector_of_factor_levels = c(
#       "1 – I do not value it at all",
#       "2 - I value it very little",
#       "3 – I value it somewhat",
#       "4 – I value it moderately",
#       "5 – I value it considerably",
#       "6 – I value it significantly",
#       "7 – I value it completely"
#     ),
#     names_of_factor_levels = c(
#       "1 – I do not value it at all",
#       "2 - I value it very little",
#       "3 – I value it somewhat",
#       "4 – I value it moderately",
#       "5 – I value it considerably",
#       "6 – I value it significantly",
#       "7 – I value it completely"
#     )
#   )
#
#   # named_vector_level_of_value <- name_vector(
#   #   vector_of_factor_levels = c(
#   #     "1 – I do not value it at all",
#   #     "2 - I value it very little",
#   #     "3 – I value it somewhat",
#   #     "4 – I value it moderately",
#   #     "5 – I value it considerably",
#   #     "6 – I value it significantly",
#   #     "7 – I value it completely"
#   #   ),
#   #   names_of_factor_levels = c(
#   #     "I do not value it at all",
#   #     "I value it very little",
#   #     "I value it somewhat",
#   #     "I value it moderately",
#   #     "I value it considerably",
#   #     "I value it significantly",
#   #     "I value it completely"
#   #   )
#   # )
#
#   create_fake_data_multiple_choice_2 <- function(df, n_respondents, var_name, response_options_named_list, other_text_col = NULL, other_text_cond = NULL, filter_var = NULL, filter_label_options = NULL) {
#     # Create random data for each column
#     rand_levels <- sample(names({{response_options_named_list}}), n_respondents, replace = TRUE)
#
#     # If a text column is added, randomly create text when the text column indicates text should be there
#     if(!is.null(other_text_col)){
#       potential_strings <- stringi::stri_rand_lipsum(n_respondents)
#       response_options[[{{other_text_col}}]] <- ifelse(
#         response_options[[{{var_name}}]] == {{other_text_cond}},
#         potential_strings,
#         NA
#       )
#     }
#     # If indicated, changed rows that match the condition to NAs
#     if(!is.null(filter_var) & !is.null(filter_label_options)){
#       should_loose <- !(df[[{{filter_var}}]] %in% c({{filter_label_options}}))
#       rand_levels[should_loose] <- NA
#     }
#
#     df[[var_name]] <- rand_levels
#     # Add the new columns to the df
#     # df <- df %>% cbind(response_options)
#     # Return the df
#     return(df)
#   }
#
#   for(variable in c("value_of_benefits__tour_or_tram", "value_of_benefits__support_global_conservation_efforts", "value_of_benefits__wild_perks_discounts", "value_of_benefits__50_perc_admission_discount", "value_of_benefits__sdzwa_journal_subscription")) {
#     dataframe <- create_fake_data_multiple_choice_2(
#       df = dataframe,
#       n_respondents = n_respondents,
#       var_name = variable,
#       response_options_named_list = named_vector_level_of_value,
#       filter_var = stringr::str_replace(variable, "^value_of", "frequency_use_of"),
#       filter_label_options = c("2 - Rarely Used", "3 - Infrequently Used", "4 - Occasionally Used", "5 - Sometimes Used", "6 - Frequently Used", "7 – Very Frequently Used")
#     )
#   }
#
#
#   ##############################################################################
#
#   dataframe$text_most_enjoyed_from_membership <- stringi::stri_rand_lipsum(n_respondents)
#
#   ##############################################################################
#
#   named_vector_ad_most_influenced_you_to_join_or_renew <- name_vector(
#     vector_of_factor_levels = c(
#       "Information from the website",
#       "Email offer",
#       "Mail offer",
#       "Online advertisement",
#       "Information provided on-site at the park",
#       "None of the above: Please provide the advertisement/opportunity that influenced your decision"
#     ),
#     names_of_factor_levels = c(
#       "Information from the website",
#       "Email offer",
#       "Mail offer",
#       "Online advertisement",
#       "Information provided on-site at the park",
#       "None of the above: Please provide the advertisement/opportunity that influenced your decision"
#     )
#   )
#
#   # named_vector_ad_most_influenced_you_to_join_or_renew <- name_vector(
#   #   vector_of_factor_levels = c(
#   #     "Information from the website",
#   #     "Email offer",
#   #     "Mail offer",
#   #     "Online advertisement",
#   #     "Information provided on-site at the park",
#   #     "None of the above: Please provide the advertisement/opportunity that influenced your decision"
#   #   ),
#   #   names_of_factor_levels = c(
#   #     "Information from the website",
#   #     "Email offer",
#   #     "Mail offer",
#   #     "Online advertisement",
#   #     "Information provided on-site at the park",
#   #     "Other"
#   #   )
#   # )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "ad_most_influenced_you_to_join_or_renew",
#     response_options_named_list = named_vector_ad_most_influenced_you_to_join_or_renew,
#     other_text_col = "text_ad_most_influenced_you_to_join_or_renew",
#     other_text_cond = "Other",
#     filter_var = NULL,
#     filter_label_options = NULL
#   )
#
#   ##############################################################################
#
#   named_vector_likelihood_of_renewing_membership <- name_vector(
#     vector_of_factor_levels = c(
#       "Very unlikely",
#       "Not Likely",
#       "Unsure",
#       "Likely",
#       "Very likely"
#     ),
#     names_of_factor_levels = c(
#       "Very unlikely",
#       "Not Likely",
#       "Unsure",
#       "Likely",
#       "Very likely"
#     )
#   )
#
#   dataframe$likelihood_of_renewing_membership <- sample(names(named_vector_likelihood_of_renewing_membership), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "why_hesitant_to_renew_membership__too_busy",
#       "why_hesitant_to_renew_membership__family_visits_less_than_expected",
#       "why_hesitant_to_renew_membership__too_expensive",
#       "why_hesitant_to_renew_membership__benefits_have_poor_value",
#       "why_hesitant_to_renew_membership__no_new_exhibits",
#       "why_hesitant_to_renew_membership__joined_another_venue",
#       "why_hesitant_to_renew_membership__it_was_a_gift",
#       "why_hesitant_to_renew_membership__relocation",
#       "why_hesitant_to_renew_membership__other"
#     ),
#     other_text_col = "why_hesitant_to_renew_membership__other",
#     filter_var = "likelihood_of_renewing_membership",
#     filter_label_options = c("Very unlikely", "Not Likely", "Unsure")
#   )
#
#   ##############################################################################
#
#   potential_text_what_other_organization_did_you_join <- stringi::stri_rand_lipsum(n_respondents)
#
#   dataframe$text_what_other_organization_did_you_join <- ifelse(dataframe$why_hesitant_to_renew_membership__joined_another_venue %in% c(1), potential_text_what_other_organization_did_you_join, NA)
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "what_could_convince_to_renew__nothing_could_convince_me",
#       "what_could_convince_to_renew__lower_membership_costs",
#       "what_could_convince_to_renew__different_benefits",
#       "what_could_convince_to_renew__more_member_activities",
#       "what_could_convince_to_renew__improve_zoo_exhibits",
#       "what_could_convince_to_renew__other"
#     ),
#     other_text_col = "what_could_convince_to_renew__other",
#     filter_var = "likelihood_of_renewing_membership",
#     filter_label_options = c("Very unlikely", "Not Likely", "Unsure")
#   )
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "why_you_intend_to_renew__there_are_large_number_of_benefits",
#       "why_you_intend_to_renew__benefits_are_useful",
#       "why_you_intend_to_renew__support_expansion",
#       "why_you_intend_to_renew__see_animals_from_around_the_world",
#       "why_you_intend_to_renew__support_global_conservation_efforts",
#       "why_you_intend_to_renew__other"
#     ),
#     other_text_col = "why_you_intend_to_renew__other",
#     filter_var = "likelihood_of_renewing_membership",
#     filter_label_options = c("Likely", "Very Likely")
#   )
#
#   ##############################################################################
#
#   # named_vector_preferred_method_of_renewal <- name_vector(
#   #   vector_of_factor_levels = c(
#   #     "Send check/credit card info via mail",
#   #     "Renew online",
#   #     "On site at the parks",
#   #     "Phone",
#   #     "None of the above: Please provide how you would prefer to renew your membership"
#   #   ),
#   #   names_of_factor_levels = c(
#   #     "Send via mail",
#   #     "Renew online",
#   #     "On site at the parks",
#   #     "Phone",
#   #     "Other"
#   #   )
#   # )
#
#   named_vector_preferred_method_of_renewal <- name_vector(
#     vector_of_factor_levels = c(
#       "Send check/credit card info via mail",
#       "Renew online",
#       "On site at the parks",
#       "Phone",
#       "None of the above: Please provide how you would prefer to renew your membership"
#     ),
#     names_of_factor_levels = c(
#       "Send check/credit card info via mail",
#       "Renew online",
#       "On site at the parks",
#       "Phone",
#       "None of the above: Please provide how you would prefer to renew your membership"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "preferred_method_of_renewal",
#     response_options_named_list = named_vector_preferred_method_of_renewal,
#     other_text_col = "text_preferred_method_of_renewal",
#     other_text_cond = "Other",
#     filter_var = "likelihood_of_renewing_membership",
#     filter_label_options = c("Likely", "Very Likely")
#   )
#
#   ##############################################################################
#
#   named_vector_familiarity <- name_vector(
#     vector_of_factor_levels = c(
#       "Not at all familiar",
#       "Slightly familiar",
#       "Somewhat familiar",
#       "Moderately familiar",
#       "Extremely familiar"
#     ),
#     names_of_factor_levels = c(
#       "Not at all familiar",
#       "Slightly familiar",
#       "Somewhat familiar",
#       "Moderately familiar",
#       "Extremely familiar"
#     )
#   )
#
#   dataframe$familiarity_of_conservation_efforts <- sample(names(named_vector_familiarity), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   named_vector_confidence <- name_vector(
#     vector_of_factor_levels = c(
#       "Not at all confident",
#       "Slightly confident",
#       "Somewhat confident",
#       "Moderately confident",
#       "Extremely confident"
#     ),
#     names_of_factor_levels = c(
#       "Not at all confident",
#       "Slightly confident",
#       "Somewhat confident",
#       "Moderately confident",
#       "Extremely confident"
#     )
#   )
#
#   dataframe$confidence_in_saving_species <- sample(names(named_vector_confidence), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   # named_vector_preferred_habitats <- name_vector(
#   #   vector_of_factor_levels = c(
#   #     "Amazonia: Jaguar, Andean bear, and giant otter",
#   #     "Australian Forest: Koala, platypus, and Tasmanian devil",
#   #     "Oceans: Polar bear and African penguin",
#   #     "Southwest: Burrowing owl, desert tortoise, and Pacific pocket mouse",
#   #     "Pacific Islands: ʻAlalā, akikiki, and Galápagos pink iguana",
#   #     "African Forest: Gorilla, chimpanzee, red colobus monkey, and coral tree",
#   #     "Savanna: Elephant, rhino, lion, and giraffe",
#   #     "Asian Rainforest: Tiger, orangutan, and sun bear"
#   #   ),
#   #   names_of_factor_levels = c(
#   #     "Amazonia",
#   #     "Australian Forest",
#   #     "Oceans",
#   #     "Southwest",
#   #     "Pacific Islands",
#   #     "African Forest",
#   #     "Savanna",
#   #     "Asian Rainforest"
#   #   )
#   # )
#
#
#   named_vector_preferred_habitats <- name_vector(
#     vector_of_factor_levels = c(
#       "Amazonia: Jaguar, Andean bear, and giant otter",
#       "Australian Forest: Koala, platypus, and Tasmanian devil",
#       "Oceans: Polar bear and African penguin",
#       "Southwest: Burrowing owl, desert tortoise, and Pacific pocket mouse",
#       "Pacific Islands: ʻAlalā, akikiki, and Galápagos pink iguana",
#       "African Forest: Gorilla, chimpanzee, red colobus monkey, and coral tree",
#       "Savanna: Elephant, rhino, lion, and giraffe",
#       "Asian Rainforest: Tiger, orangutan, and sun bear"
#     ),
#     names_of_factor_levels = c(
#       "Amazonia: Jaguar, Andean bear, and giant otter",
#       "Australian Forest: Koala, platypus, and Tasmanian devil",
#       "Oceans: Polar bear and African penguin",
#       "Southwest: Burrowing owl, desert tortoise, and Pacific pocket mouse",
#       "Pacific Islands: ʻAlalā, akikiki, and Galápagos pink iguana",
#       "African Forest: Gorilla, chimpanzee, red colobus monkey, and coral tree",
#       "Savanna: Elephant, rhino, lion, and giraffe",
#       "Asian Rainforest: Tiger, orangutan, and sun bear"
#     )
#   )
#
#   dataframe$preferred_habitats <- sample(names(named_vector_preferred_habitats), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "preferred_method_to_receive_conservation_news__at_zoo_or_safari_park",
#       "preferred_method_to_receive_conservation_news__social_media",
#       "preferred_method_to_receive_conservation_news__email",
#       "preferred_method_to_receive_conservation_news__mail",
#       "preferred_method_to_receive_conservation_news__website"
#     ),
#     other_text_col = NULL,
#     filter_var = NULL,
#     filter_label_options = NULL
#   )
#
#   ##############################################################################
#
#   named_vector_donated_to_sdzwa_in_past_too_years <- name_vector(
#     vector_of_factor_levels = c(
#       "No, I’m not interested in giving additional money",
#       "No, but I am interested in doing so",
#       "Yes"
#     ),
#     names_of_factor_levels = c(
#       "No, I’m not interested in giving additional money",
#       "No, but I am interested in doing so",
#       "Yes"
#     )
#   )
#
#   dataframe$donated_to_sdzwa_in_past_too_years <- sample(names(named_vector_donated_to_sdzwa_in_past_too_years), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   named_vector_considering_upgrading_membership <- name_vector(
#     vector_of_factor_levels = c(
#       "No, I have already upgraded recently",
#       "No, I have no desire to upgrade my membership",
#       "I am unsure about upgrading/what the membership levels are",
#       "Yes, I am planning to upgrade soon",
#       "Yes, but I am hesitant to upgrade"
#     ),
#     names_of_factor_levels = c(
#       "No, I have already upgraded recently",
#       "No, I have no desire to upgrade my membership",
#       "I am unsure about upgrading/what the membership levels are",
#       "Yes, I am planning to upgrade soon",
#       "Yes, but I am hesitant to upgrade"
#     )
#   )
#
#   dataframe$considering_upgrading_membership <- sample(names(named_vector_considering_upgrading_membership), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "factors_influenced_decision_to_upgrade__more_benefits",
#       "factors_influenced_decision_to_upgrade__help_conservation_efforts",
#       "factors_influenced_decision_to_upgrade__financially_able_to",
#       "factors_influenced_decision_to_upgrade__other"
#     ),
#     other_text_col = "factors_influenced_decision_to_upgrade__other",
#     filter_var = "considering_upgrading_membership",
#     filter_label_options = c("No, I have already upgraded recently", "Yes, I am planning to upgrade soon")
#   )
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "factors_made_hesitant_to_upgrade__not_financial_able_to",
#       "factors_made_hesitant_to_upgrade__do_not_want_benefits",
#       "factors_made_hesitant_to_upgrade__benefits_not_good_value",
#       "factors_made_hesitant_to_upgrade__do_not_know_enough_about_benefits",
#       "factors_made_hesitant_to_upgrade__other"
#     ),
#     other_text_col = "factors_made_hesitant_to_upgrade__other",
#     filter_var = "considering_upgrading_membership",
#     filter_label_options = c("No, I have no desire to upgrade my membership", "I am unsure about upgrading/what the membership levels are", "Yes, I am planning to upgrade soon")
#   )
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "what_would_convince_to_upgrade__nothing_would_convince_me",
#       "what_would_convince_to_upgrade__lower_membership_costs",
#       "what_would_convince_to_upgrade__different_membership_benefits",
#       "what_would_convince_to_upgrade__more_member_activities",
#       "what_would_convince_to_upgrade__improvements_to_zoo_exhibits",
#       "what_would_convince_to_upgrade__other"
#     ),
#     other_text_col = "what_would_convince_to_upgrade__other",
#     filter_var = "considering_upgrading_membership",
#     filter_label_options = c("No, I have no desire to upgrade my membership", "I am unsure about upgrading/what the membership levels are", "Yes, I am planning to upgrade soon")
#   )
#
#   ##############################################################################
#
#   named_vector_net_promoter <- name_vector(
#     vector_of_factor_levels = as.character(0:10),
#     names_of_factor_levels = as.character(0:10)
#   )
#
#   dataframe$net_promoter <- sample(names(named_vector_net_promoter), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe$why_net_promoter_text <- stringi::stri_rand_lipsum(n_respondents)
#
#   ##############################################################################
#
#   dataframe$any_other_feedback <- stringi::stri_rand_lipsum(n_respondents)
#
#   ##############################################################################
#
#   # named_vector_gender <- name_vector(
#   #   vector_of_factor_levels = c("Female", "Male", "Non-binary", "Prefer not to answer"),
#   #   names_of_factor_levels = c("Female", "Male", "Other", "Other")
#   # )
#
#   named_vector_gender <- name_vector(
#     vector_of_factor_levels = c("Female", "Male", "Non-binary", "Prefer not to answer"),
#     names_of_factor_levels = c("Female", "Male", "Non-binary", "Prefer not to answer")
#   )
#
#   dataframe$gender <- sample(names(named_vector_gender), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe$age <- sample(18:100, n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe$age_group <- dplyr::case_when(
#     dataframe$age >= 18 & dataframe$age < 40 ~ "18 - 39",
#     dataframe$age >= 40 & dataframe$age < 60 ~ "40 - 59",
#     dataframe$age >= 60 & dataframe$age < 101 ~ "60 - 100"
#   )
#
#   ##############################################################################
#
#   named_vector_ethnicity <- name_vector(
#     vector_of_factor_levels = c(
#       "White", "Hispanic, Latino, or Spanish origin",
#       "Black or African American", "Asian", "American Indian or Alaska Native",
#       "Middle Eastern or North African",
#       "Native Hawaiian or other pacific islander", "Other (please specify)"),
#     names_of_factor_levels = c(
#       "White", "Hispanic, Latino, or Spanish origin",
#       "Black or African American", "Asian", "American Indian or Alaska Native",
#       "Middle Eastern or North African",
#       "Native Hawaiian or other pacific islander", "Other (please specify)")
#   )
#
#   # named_vector_ethnicity <- name_vector(
#   #   vector_of_factor_levels = c(
#   #     "White", "Hispanic, Latino, or Spanish origin",
#   #     "Black or African American", "Asian", "American Indian or Alaska Native",
#   #     "Middle Eastern or North African",
#   #     "Native Hawaiian or other pacific islander", "Other (please specify)"),
#   #   names_of_factor_levels = c(
#   #     "White", "Hispanic", "African American", "Asian",
#   #     "Other", "Other", "Other", "Other")
#   # )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "ethnicity",
#     response_options_named_list = named_vector_ethnicity,
#     other_text_col = "text_ethnicity_other",
#     other_text_cond = "Other",
#     filter_var = NULL,
#     filter_label_options = NULL
#   )
#
#   ##############################################################################
#
#   named_vector_marital_status <- name_vector(
#     vector_of_factor_levels = c("Married", "Single, never married", "Single, divorced/separated/widowed", "Living with significant other/engaged"),
#     names_of_factor_levels = c("Married", "Single, never married", "Single, divorced/separated/widowed", "Living with significant other/engaged")
#   )
#
#   dataframe$marital_status <- sample(names(named_vector_marital_status), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "housebold_composition__no_children ",
#       "housebold_composition__under_12",
#       "housebold_composition__12_to_17",
#       "housebold_composition__18_to_65",
#       "housebold_composition__65_and_up"
#     ),
#     other_text_col = NULL,
#     filter_var = NULL,
#     filter_label_options = NULL
#   )
#
#   ##############################################################################
#
#   named_vector_annual_household_income <- name_vector(
#     vector_of_factor_levels = c(
#       "$0 - $29,999",
#       "$30,000 - $59,999",
#       "$60,000 - $89,999",
#       "$90,000 - $119,999",
#       "$120,000 - $149,999",
#       "$150,000+"
#     ),
#     names_of_factor_levels = c(
#       "$0 - $29,999",
#       "$30,000 - $59,999",
#       "$60,000 - $89,999",
#       "$90,000 - $119,999",
#       "$120,000 - $149,999",
#       "$150,000+"
#     )
#   )
#
#   dataframe$annual_household_income <- sample(names(named_vector_annual_household_income), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe$zip_code <- sample(11111:99999, n_respondents)
#
#   ##############################################################################
#
#   named_vector_agreement <- name_vector(
#     vector_of_factor_levels = c(
#       "Strongly Disagree",
#       "Disagree",
#       "Neutral",
#       "Agree",
#       "Strongly Agree"
#     ),
#     names_of_factor_levels = c(
#       "Strongly Disagree",
#       "Disagree",
#       "Neutral",
#       "Agree",
#       "Strongly Agree"
#     )
#   )
#
#   emotional_connection_items <- c(
#     "emotional_connection__relates_to_me_personally",
#     "emotional_connection__relates_to_close_friends_and_family",
#     "emotional_connection__deserves_my_financial_support",
#     "emotional_connection__made_substantial_progress",
#     "emotional_connection__is_trustworthy",
#     "emotional_connection__uses_donations_responsibly",
#     "emotional_connection__is_compassionate_and_caring",
#     "emotional_connection__is_innovative_and_a_leader",
#     "emotional_connection__is_doing_enough",
#     "emotional_connection__does_work_to_improve_lives_like_mine",
#     "emotional_connection__does_unique_important_work",
#     "emotional_connection__heard_positive_things_from_media",
#     "emotional_connection__heard_positive_things_from_friends",
#     "emotional_connection__heard_negative_things_from_media",
#     "emotional_connection__heard_negative_things_from_friends",
#     "emotional_connection__is_a_good_organization",
#     "emotional_connection__strong_positive_feelings_about_org",
#     "emotional_connection__is_transparent_and_clear",
#     "emotional_connection__one_of_the_best_non_profit_orgs",
#     "emotional_connection__addresses_problems_important_to_me"
#   )
#
#   for(variable in emotional_connection_items) {
#     dataframe <- create_fake_data_multiple_choice(
#       df = dataframe,
#       n_respondents = n_respondents,
#       var_name = variable,
#       response_options_named_list = named_vector_agreement
#     )
#   }
#
#   ##############################################################################
#
#   return(dataframe)
#
#   ##############################################################################
# }
#
#
# # Obtain fake data for the sdwza member survey ----------------------------
#
# wave_1_sdwza_member_fake <- create_fake_data_for_sdwza_members(n_respondents = 1001)
# wave_1_sdwza_member_fake$waves <- "Wave 1"
#
# wave_2_sdwza_member_fake <- create_fake_data_for_sdwza_members(n_respondents = 2222)
# wave_2_sdwza_member_fake$waves <- "Wave 2"
#
# wave_3_sdwza_member_fake <- create_fake_data_for_sdwza_members(n_respondents = 1003)
# wave_3_sdwza_member_fake$waves <- "Wave 3"
#
# wave_4_sdwza_member_fake <- create_fake_data_for_sdwza_members(n_respondents = 1045)
# wave_4_sdwza_member_fake$waves <- "Wave 4"
#
# all_sdwza_fake_member_data <- wave_1_sdwza_member_fake %>%
#   rbind(wave_2_sdwza_member_fake) %>%
#   rbind(wave_3_sdwza_member_fake) %>%
#   rbind(wave_4_sdwza_member_fake)
#
#
# # paste0(colnames(all_sdwza_fake_member_data), collapse = "', '")
#
# # c(
# #   'length_of_membership',
# #   'number_of_visits_last_year',
# #   'number_of_planned_visits',
# #   'why_originally_became_member',
# #   'frequency_use_of_benefits__tour_or_tram',
# #   'frequency_use_of_benefits__support_global_conservation_efforts',
# #   'frequency_use_of_benefits__wild_perks_discounts',
# #   'frequency_use_of_benefits__50_perc_admission_discount',
# #   'frequency_use_of_benefits__sdzwa_journal_subscription',
# #   'value_of_benefits__tour_or_tram',
# #   'value_of_benefits__support_global_conservation_efforts',
# #   'value_of_benefits__wild_perks_discounts',
# #   'value_of_benefits__50_perc_admission_discount',
# #   'value_of_benefits__sdzwa_journal_subscription',
# #   'ad_most_influenced_you_to_join_or_renew',
# #   'likelihood_of_renewing_membership',
# #   'preferred_method_of_renewal',
# #   'familiarity_of_conservation_efforts', 'confidence_in_saving_species', 'preferred_habitats', 'preferred_method_to_receive_conservation_news__at_zoo_or_safari_park', 'preferred_method_to_receive_conservation_news__social_media', 'preferred_method_to_receive_conservation_news__email', 'preferred_method_to_receive_conservation_news__mail', 'preferred_method_to_receive_conservation_news__website', 'donated_to_sdzwa_in_past_too_years', 'considering_upgrading_membership', 'factors_influenced_decision_to_upgrade__more_benefits', 'factors_influenced_decision_to_upgrade__help_conservation_efforts', 'factors_influenced_decision_to_upgrade__financially_able_to', 'factors_influenced_decision_to_upgrade__other', 'text_factors_influenced_decision_to_upgrade__other', 'factors_made_hesitant_to_upgrade__not_financial_able_to', 'factors_made_hesitant_to_upgrade__do_not_want_benefits', 'factors_made_hesitant_to_upgrade__benefits_not_good_value', 'factors_made_hesitant_to_upgrade__do_not_know_enough_about_benefits', 'factors_made_hesitant_to_upgrade__other', 'text_factors_made_hesitant_to_upgrade__other', 'what_would_convince_to_upgrade__nothing_would_convince_me', 'what_would_convince_to_upgrade__lower_membership_costs', 'what_would_convince_to_upgrade__different_membership_benefits', 'what_would_convince_to_upgrade__more_member_activities', 'what_would_convince_to_upgrade__improvements_to_zoo_exhibits', 'what_would_convince_to_upgrade__other', 'text_what_would_convince_to_upgrade__other', 'net_promoter', 'why_net_promoter_text', 'any_other_feedback', 'gender', 'age', 'age_group', 'ethnicity', 'text_ethnicity_other', 'marital_status', 'housebold_composition__no_children ', 'housebold_composition__under_12', 'housebold_composition__12_to_17', 'housebold_composition__18_to_65', 'housebold_composition__65_and_up', 'annual_household_income', 'zip_code', 'emotional_connection__relates_to_me_personally', 'emotional_connection__relates_to_close_friends_and_family', 'emotional_connection__deserves_my_financial_support', 'emotional_connection__made_substantial_progress', 'emotional_connection__is_trustworthy', 'emotional_connection__uses_donations_responsibly', 'emotional_connection__is_compassionate_and_caring', 'emotional_connection__is_innovative_and_a_leader', 'emotional_connection__is_doing_enough', 'emotional_connection__does_work_to_improve_lives_like_mine', 'emotional_connection__does_unique_important_work', 'emotional_connection__heard_positive_things_from_media', 'emotional_connection__heard_positive_things_from_friends', 'emotional_connection__heard_negative_things_from_media', 'emotional_connection__heard_negative_things_from_friends', 'emotional_connection__is_a_good_organization', 'emotional_connection__strong_positive_feelings_about_org', 'emotional_connection__is_transparent_and_clear', 'emotional_connection__one_of_the_best_non_profit_orgs', 'emotional_connection__addresses_problems_important_to_me', 'waves')
#
#
# all_sdwza_fake_member_data <- all_sdwza_fake_member_data %>%
#   mutate(dplyr::across(where(is.factor), as.numeric, .names = "VAR_ORDER__{.col}"))
#
# clipr::write_clip(all_sdwza_fake_member_data)
# ################################################################################
#
# ################################################################################
# # Create function that creates fake data for the sdwza donor survey -------
#
# create_fake_data_for_sdwza_donors <- function(n_respondents) {
#
#   ##############################################################################
#
#   respondent_id <- sample(100000000000:999999999999, n_respondents)
#
#   collector_id <- rep(123456789, n_respondents)
#
#   random_numbers_for_date <- seq(n_respondents + 499, 500)
#
#   random_dates <- Sys.time() - random_numbers_for_date
#
#   random_time_var <- sample(2:60, n_respondents, replace = TRUE)
#
#   random_dates_plus_time <- random_dates + (random_time_var * 60)
#
#   start_date <- format(random_dates, "%m/%d/%Y %H:%M")
#
#   end_date <- format(random_dates_plus_time, "%m/%d/%Y %H:%M")
#
#   ip_address <- rep("000.00.000.00", n_respondents)
#
#   email_address <- rep(NA, n_respondents)
#   first_name <- rep(NA, n_respondents)
#   last_name <- rep(NA, n_respondents)
#   custom_data_1 <- rep(NA, n_respondents)
#
#   dataframe <- tibble::tibble(
#     respondent_id,
#     collector_id,
#     start_date,
#     end_date,
#     ip_address,
#     email_address,
#     first_name,
#     last_name,
#     custom_data_1
#   )
#
#   ##############################################################################
#
#   named_vector_first_donation_why <- name_vector(
#     vector_of_factor_levels = c(
#       "I trust San Diego Zoo Wildlife Alliance’s history of impactful conservation efforts",
#       "I am passionate about saving wildlife and protecting endangered species in San Diego and around the world",
#       "I want to support the San Diego Zoo and San Diego Zoo Safari Park and ensure they remain open for future generations",
#       "I was invited to make a donation",
#       "I want to give back to my community",
#       "Giving back is the right thing to do",
#       "None of the above: (Please provide your top reason)"
#     ),
#     names_of_factor_levels = c(
#       "History of conservation efforts",
#       "Passionate about saving wildlife",
#       "Support the parks",
#       "I was invited to donate",
#       "To give back to my community",
#       "It is the right thing to do",
#       "Other"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "first_donation_why",
#     response_options_named_list = named_vector_first_donation_why,
#     other_text_col = "text_first_donation_why",
#     other_text_cond = "Other",
#     filter_var = NULL,
#     filter_label_options = NULL
#   )
#
#   ##############################################################################
#
#   named_vector_confidence <- name_vector(
#     vector_of_factor_levels = c(
#       "Not at all confident",
#       "Slightly confident",
#       "Somewhat confident",
#       "Moderately confident",
#       "Extremely confident"
#     ),
#     names_of_factor_levels = c(
#       "Not at all confident",
#       "Slightly confident",
#       "Somewhat confident",
#       "Moderately confident",
#       "Extremely confident"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "confident_donation_will_help_acheive_goal",
#     response_options_named_list = named_vector_confidence
#   )
#
#   ##############################################################################
#
#   named_vector_yes_no <- name_vector(
#     vector_of_factor_levels = c(
#       "Yes",
#       "No"
#     ),
#     names_of_factor_levels = c(
#       "Yes",
#       "No"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "monthly_donor",
#     response_options_named_list = named_vector_yes_no
#   )
#
#   ##############################################################################
#
#   named_vector_familiarity <- name_vector(
#     vector_of_factor_levels = c(
#       "Not at all familiar",
#       "Slightly familiar",
#       "Somewhat familiar",
#       "Moderately familiar",
#       "Extremely familiar"
#     ),
#     names_of_factor_levels = c(
#       "Not at all familiar",
#       "Slightly familiar",
#       "Somewhat familiar",
#       "Moderately familiar",
#       "Extremely familiar"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "familiarity_with_sdzwa_monthly_donor_program",
#     response_options_named_list = named_vector_familiarity,
#     other_text_col = NULL,
#     other_text_cond = NULL,
#     filter_var = "monthly_donor",
#     filter_label_options = "No"
#   )
#
#   ##############################################################################
#
#   named_vector_likelihood_2 <- name_vector(
#     vector_of_factor_levels = c(
#       "Very unlikely",
#       "Not Likely",
#       "Unsure",
#       "Likely",
#       "Very likely",
#       "I would need to learn more before deciding"
#     ),
#     names_of_factor_levels = c(
#       "Very unlikely",
#       "Not Likely",
#       "Unsure",
#       "Likely",
#       "Very likely",
#       "Other"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "considering_becoming_monthly_donor",
#     response_options_named_list = named_vector_likelihood_2,
#     other_text_col = NULL,
#     other_text_cond = NULL,
#     filter_var = "monthly_donor",
#     filter_label_options = "No"
#   )
#
#   ##############################################################################
#
#   named_vector_why_not_monthly_donor <- name_vector(
#     vector_of_factor_levels = c(
#       "I prefer to give a single lump sum donation",
#       "I don’t like subscription/monthly commitments",
#       "Monthly donations are not affordable for me right now",
#       "I already give monthly to another organization(s)",
#       "Need more information first",
#       "Other (please specify)"
#     ),
#     names_of_factor_levels = c(
#       "Prefer lump-sum",
#       "Do not want monthly commitments",
#       "Not affordable",
#       "Already give monthly to others",
#       "Need more information",
#       "Other"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "why_not_monthly_donor",
#     response_options_named_list = named_vector_why_not_monthly_donor,
#     other_text_col = "text_why_not_monthly_donor_other",
#     other_text_cond = "Other",
#     filter_var = "considering_becoming_monthly_donor",
#     filter_label_options = c("Very unlikely", "Not Likely", "Unsure")
#   )
#
#   ##############################################################################
#
#   named_vector_last_donation_how <- name_vector(
#     vector_of_factor_levels = c(
#       "Online",
#       "In person at the Zoo or Safari Park",
#       "By mail",
#       "By phone",
#       "At an event",
#       "Other (please specify)"
#     ),
#     names_of_factor_levels = c(
#       "Online",
#       "In person at the Zoo or Safari Park",
#       "By mail",
#       "By phone",
#       "At an event",
#       "Other"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "last_donation_how",
#     response_options_named_list = named_vector_last_donation_how,
#     other_text_col = "text_last_donation_how_other",
#     other_text_cond = "Other",
#     filter_var = NULL,
#     filter_label_options = NULL
#   )
#
#   ##############################################################################
#
#   named_vector_last_online_donation_how <- name_vector(
#     vector_of_factor_levels = c(
#       "Going to the website",
#       "Through an email",
#       "Through a social media ad",
#       "Through an online ad",
#       "None of the above: (Please indicate where you donated online)"
#     ),
#     names_of_factor_levels = c(
#       "Going to the website",
#       "Through an email",
#       "Through a social media ad",
#       "Through an online ad",
#       "Other"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "last_online_donation_how",
#     response_options_named_list = named_vector_last_online_donation_how,
#     other_text_col = "text_last_online_donation_how_other",
#     other_text_cond = "Other",
#     filter_var = "last_donation_how",
#     filter_label_options = "Online"
#   )
#
#   ##############################################################################
#
#   named_vector_frequency_of_info <- name_vector(
#     vector_of_factor_levels = c(
#       "Unsure",
#       "Never",
#       "Rarely",
#       "Occasionally",
#       "Frequently"
#     ),
#     names_of_factor_levels = c(
#       "Unsure",
#       "Never",
#       "Rarely",
#       "Occasionally",
#       "Frequently"
#     )
#   )
#
#   how_often_receive_conservation_information_variables <- c(
#     "how_often_receive_conservation_information__email",
#     "how_often_receive_conservation_information__mail",
#     "how_often_receive_conservation_information__website",
#     "how_often_receive_conservation_information__at_park",
#     "how_often_receive_conservation_information__social_media",
#     "how_often_receive_conservation_information__word_of_mouth"
#   )
#
#   for(variable in how_often_receive_conservation_information_variables) {
#     dataframe <- create_fake_data_multiple_choice(
#       df = dataframe,
#       n_respondents = n_respondents,
#       var_name = variable,
#       response_options_named_list = named_vector_frequency_of_info
#     )
#   }
#
#   ##############################################################################
#
#   named_vector_likelihood <- name_vector(
#     vector_of_factor_levels = c(
#       "Very unlikely",
#       "Not Likely",
#       "Unsure",
#       "Likely",
#       "Very likely"
#     ),
#     names_of_factor_levels = c(
#       "Very unlikely",
#       "Not Likely",
#       "Unsure",
#       "Likely",
#       "Very likely"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "support_sdzwa_again",
#     response_options_named_list = named_vector_likelihood
#   )
#
#   ##############################################################################
#
#   named_vector_will_change_donation <- name_vector(
#     vector_of_factor_levels = c(
#       "No, I will continue donating the same amount just as frequently",
#       "Yes, I will decrease the amount or frequency of my donation",
#       "Yes, I will increase the amount or frequency of my donation",
#       "I am unsure if the amount or frequency of my donation will change"
#     ),
#     names_of_factor_levels = c(
#       "No, I will continue donating the same amount just as frequently",
#       "Yes, I will decrease the amount or frequency of my donation",
#       "Yes, I will increase the amount or frequency of my donation",
#       "I am unsure if the amount or frequency of my donation will change"
#     )
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "will_change_donation",
#     response_options_named_list = named_vector_will_change_donation,
#     other_text_col = NULL,
#     other_text_cond = NULL,
#     filter_var = "support_sdzwa_again",
#     filter_label_options = c("Likely", "Very likely")
#   )
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "why_will_change_support__conservation_work",
#       "why_will_change_support__use_funds_appropriately",
#       "why_will_change_support__expand_parks",
#       "why_will_change_support__value_giving_back",
#       "why_will_change_support__enjoy_donor_benefits",
#       "why_will_change_support__other"
#     ),
#     other_text_col = "why_will_change_support__other",
#     filter_var = "support_sdzwa_again",
#     filter_label_options = c("Likely", "Very likely")
#   )
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "why_not_support__current_budget",
#       "why_not_support__other_priorities",
#       "why_not_support__not_convinced_on_their_conservation_efforts",
#       "why_not_support__do_not_use_funds_appropriately",
#       "why_not_support__do_not_like_benefits",
#       "why_not_support__other"
#     ),
#     other_text_col = "why_not_support__other",
#     filter_var = "support_sdzwa_again",
#     filter_label_options = c("Very unlikely", "Not Likely", "Unsure")
#   )
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "what_would_convince_to_support_again__information",
#       "what_would_convince_to_support_again__benefits",
#       "what_would_convince_to_support_again__info_on_conservation_efforts",
#       "what_would_convince_to_support_again__donor_appreciation",
#       "what_would_convince_to_support_again__other"
#     ),
#     other_text_col = "what_would_convince_to_support_again__other",
#     filter_var = "support_sdzwa_again",
#     filter_label_options = c("Very unlikely", "Not Likely", "Unsure")
#   )
#
#   ##############################################################################
#
#   named_vector_net_promoter <- name_vector(
#     vector_of_factor_levels = as.character(0:10),
#     names_of_factor_levels = as.character(0:10)
#   )
#
#   dataframe$net_promoter <- sample(names(named_vector_net_promoter), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe$why_net_promoter_text <- stringi::stri_rand_lipsum(n_respondents)
#
#   ##############################################################################
#
#   dataframe$any_other_feedback <- stringi::stri_rand_lipsum(n_respondents)
#
#   ##############################################################################
#
#   named_vector_gender <- name_vector(
#     vector_of_factor_levels = c("Female", "Male", "Non-binary", "Prefer not to answer"),
#     names_of_factor_levels = c("Female", "Male", "Other", "Other")
#   )
#
#   dataframe$gender <- sample(names(named_vector_gender), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe$age <- sample(18:100, n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe$age_group <- dplyr::case_when(
#     dataframe$age >= 18 & dataframe$age < 40 ~ "18 - 39",
#     dataframe$age >= 40 & dataframe$age < 60 ~ "40 - 59",
#     dataframe$age >= 60 & dataframe$age < 101 ~ "60 - 100"
#   )
#
#   ##############################################################################
#
#   named_vector_ethnicity <- name_vector(
#     vector_of_factor_levels = c(
#       "White", "Hispanic, Latino, or Spanish origin",
#       "Black or African American", "Asian", "American Indian or Alaska Native",
#       "Middle Eastern or North African",
#       "Native Hawaiian or other pacific islander", "Other (please specify)"),
#     names_of_factor_levels = c(
#       "White", "Hispanic", "African American", "Asian",
#       "Other", "Other", "Other", "Other")
#   )
#
#   dataframe <- create_fake_data_multiple_choice(
#     df = dataframe,
#     n_respondents = n_respondents,
#     var_name = "ethnicity",
#     response_options_named_list = named_vector_ethnicity,
#     other_text_col = "text_ethnicity_other",
#     other_text_cond = "Other",
#     filter_var = NULL,
#     filter_label_options = NULL
#   )
#
#   ##############################################################################
#
#   named_vector_marital_status <- name_vector(
#     vector_of_factor_levels = c("Married", "Single, never married", "Single, divorced/separated/widowed", "Living with significant other/engaged"),
#     names_of_factor_levels = c("Married", "Single, never married", "Single, divorced/separated/widowed", "Living with significant other/engaged")
#   )
#
#   dataframe$marital_status <- sample(names(named_vector_marital_status), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe <- create_fake_data_select_all_columns(
#     df = dataframe,
#     n_respondents = n_respondents,
#     col_names = c(
#       "housebold_composition__no_children ",
#       "housebold_composition__under_12",
#       "housebold_composition__12_to_17",
#       "housebold_composition__18_to_65",
#       "housebold_composition__65_and_up"
#     ),
#     other_text_col = NULL,
#     filter_var = NULL,
#     filter_label_options = NULL
#   )
#
#   ##############################################################################
#
#   named_vector_annual_household_income <- name_vector(
#     vector_of_factor_levels = c(
#       "$0 - $29,999",
#       "$30,000 - $59,999",
#       "$60,000 - $89,999",
#       "$90,000 - $119,999",
#       "$120,000 - $149,999",
#       "$150,000+"
#     ),
#     names_of_factor_levels = c(
#       "$0 - $29,999",
#       "$30,000 - $59,999",
#       "$60,000 - $89,999",
#       "$90,000 - $119,999",
#       "$120,000 - $149,999",
#       "$150,000+"
#     )
#   )
#
#   dataframe$annual_household_income <- sample(names(named_vector_annual_household_income), n_respondents, replace = TRUE)
#
#   ##############################################################################
#
#   dataframe$zip_code <- sample(11111:99999, n_respondents)
#
#   ##############################################################################
#
#   named_vector_agreement <- name_vector(
#     vector_of_factor_levels = c(
#       "Strongly Disagree",
#       "Disagree",
#       "Neutral",
#       "Agree",
#       "Strongly Agree"
#     ),
#     names_of_factor_levels = c(
#       "Strongly Disagree",
#       "Disagree",
#       "Neutral",
#       "Agree",
#       "Strongly Agree"
#     )
#   )
#
#   emotional_connection_items <- c(
#     "emotional_connection__relates_to_me_personally",
#     "emotional_connection__relates_to_close_friends_and_family",
#     "emotional_connection__deserves_my_financial_support",
#     "emotional_connection__made_substantial_progress",
#     "emotional_connection__is_trustworthy",
#     "emotional_connection__uses_donations_responsibly",
#     "emotional_connection__is_compassionate_and_caring",
#     "emotional_connection__is_innovative_and_a_leader",
#     "emotional_connection__is_doing_enough",
#     "emotional_connection__does_work_to_improve_lives_like_mine",
#     "emotional_connection__does_unique_important_work",
#     "emotional_connection__heard_positive_things_from_media",
#     "emotional_connection__heard_positive_things_from_friends",
#     "emotional_connection__heard_negative_things_from_media",
#     "emotional_connection__heard_negative_things_from_friends",
#     "emotional_connection__is_a_good_organization",
#     "emotional_connection__strong_positive_feelings_about_org",
#     "emotional_connection__is_transparent_and_clear",
#     "emotional_connection__one_of_the_best_non_profit_orgs",
#     "emotional_connection__addresses_problems_important_to_me"
#   )
#
#   for(variable in emotional_connection_items) {
#     dataframe <- create_fake_data_multiple_choice(
#       df = dataframe,
#       n_respondents = n_respondents,
#       var_name = variable,
#       response_options_named_list = named_vector_agreement
#     )
#   }
#
#   ##############################################################################
#
#   return(dataframe)
#
#   ##############################################################################
# }
#
#
# # Obtain fake data for the sdwza donor survey -----------------------------
#
# wave_1_sdwza_donors_fake <- create_fake_data_for_sdwza_donors(1001)
# wave_1_sdwza_donors_fake$waves <- "Wave 1"
#
# wave_2_sdwza_donors_fake <- create_fake_data_for_sdwza_donors(1005)
# wave_2_sdwza_donors_fake$waves <- "Wave 2"
#
# wave_3_sdwza_donors_fake <- create_fake_data_for_sdwza_donors(1003)
# wave_3_sdwza_donors_fake$waves <- "Wave 3"
#
# wave_4_sdwza_donors_fake <- create_fake_data_for_sdwza_donors(1045)
# wave_4_sdwza_donors_fake$waves <- "Wave 4"
#
# all_sdwza_fake_donors_data <- wave_1_sdwza_donors_fake %>%
#   rbind(wave_2_sdwza_donors_fake) %>%
#   rbind(wave_3_sdwza_donors_fake) %>%
#   rbind(wave_4_sdwza_donors_fake)
#
# clipr::write_clip(all_sdwza_fake_donors_data)
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
# ################################################################################
#
# ################################################################################
