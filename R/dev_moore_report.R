
# draft1 ------------------------------------------------------------------

#
# RUN_MOORE_REPORT <- function() {
#   library(dplyr)
#   library(stringr)
#   library(purrr)
#
#   path_to_moore_report_data <- file.path(
#     "C:",
#     "Users",
#     "Brian",
#     "TCM Dropbox",
#     "Brian Kissell",
#     "04 MDM Neuro-Fundraising Lab",
#     "Research and Development",
#     "00 Jobs",
#     "2024",
#     "006_RD_Moore Report Second Edition",
#     "Cleaning_2024_Data.xlsx"
#   )
#
#   moore_report_data <- readxl::read_excel(
#     path = path_to_moore_report_data,
#     sheet = "Combined Data",
#     col_types = c("text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric")
#   )
#
#   # glimpse(moore_report_data)
#
#   moore_report_data$Campaign_Type <- tidyr::replace_na(moore_report_data$Campaign_Type, "")
#   moore_report_data$Package_Type <- tidyr::replace_na(moore_report_data$Package_Type, "")
#
#   moore_report_data$String_to_Process <- stringr::str_c(moore_report_data$Campaign_Type, " | ", moore_report_data$Package_Type)
#   moore_report_data$String_to_Process <- stringr::str_to_lower(moore_report_data$String_to_Process)
#
#
#   text_to_remove <- c(
#     "(no 2022 equivalent)",
#     "straight"
#   ) %>% paste0(collapse = "|")
#
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, text_to_remove, "")
#
#
#   month_strings_to_extract <- c(
#     "january",
#     "february",
#     "march",
#     "april",
#     "may",
#     "june",
#     "july",
#     "august",
#     "september",
#     "october",
#     "november",
#     "december",
#     "janaury",
#     "septtember",
#     "janjary",
#     "ocotber",
#     "septmeber",
#     "feburary",
#     "jan ",
#     " jan$",
#     "^jan$",
#     "feb ",
#     " feb$",
#     "^feb$",
#     "mar ",
#     " mar$",
#     "^mar$",
#     "apr ",
#     " apr$",
#     "^apr$",
#     "may ",
#     " may$",
#     "^may$",
#     "jun ",
#     " jun$",
#     "^jun$",
#     "jul ",
#     " jul$",
#     "^jul$",
#     "aug ",
#     " aug$",
#     "^aug$",
#     "sep ",
#     " sep$",
#     "^sep$",
#     "oct ",
#     " oct$",
#     "^oct$",
#     "nov ",
#     " nov$",
#     "^nov$",
#     "dec ",
#     " dec$",
#     "^dec$"
#   )
#
#   month_strings_to_extract_regex <- paste0(month_strings_to_extract, collapse = "|")
#   moore_report_data$month_in_info <- stringr::str_extract(moore_report_data$String_to_Process, month_strings_to_extract_regex)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, month_strings_to_extract_regex, "")
#
#   campaign_strings_to_extract <- c(
#     "acquisition",
#     "acqusition",
#     "acquistion",
#     "acq$",
#     "acq ",
#     "membership renewal",
#     "early renewal",
#     "renewal",
#     "pre-lapsed",
#     "lapsed",
#     "middle$",
#     "middle ",
#     "middle\\/",
#     "mid-level",
#     "midlevel",
#     "mid year",
#     "mid$",
#     "mid ",
#     "mid\\$",
#     "mid\\/",
#     "high-value",
#     "hi$",
#     "hi ",
#     "hi\\$",
#     "high$",
#     "high ",
#     "high\\$",
#     "lo$",
#     "lo ",
#     "lo\\$",
#     "major-level",
#     "major",
#     "donor",
#     "cultivation",
#     "upgrade",
#     "expired",
#     "sustainer",
#     "warm prospect",
#     "initial",
#     "follow-up",
#     "folow-up",
#     "follow up",
#     "followup",
#     "multi$",
#     "multi ",
#     "non-members",
#     "fup\\/",
#     "fup ",
#     "fup$",
#     "segments \\(.+\\)",
#     "active prospect",
#     "tribute donors",
#     "walk donors",
#     "non-renewed",
#     "renewed",
#     "follow"
#   )
#
#   campaign_strings_to_extract_regex <- paste0(campaign_strings_to_extract, collapse = "|")
#   captured_campaign_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, campaign_strings_to_extract_regex)
#   moore_report_data$campaign_strings <- purrr::map_chr(captured_campaign_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$campaign_strings <- ifelse(moore_report_data$campaign_strings == "", NA, moore_report_data$campaign_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, campaign_strings_to_extract_regex, "")
#
#   people_strings_to_extract <- c(
#     "hispanic",
#     "[0-9]{1,2}-[0-9]{1,2} ?[mf]{1} ?",
#     "\\$[0-9\\.]{1,6}-\\$?[0-9\\.]{1,6}",
#     "[0-9]{1,4}[mf]{1} ?",
#     "[0-9]{1,4}-[0-9]{1,4}"
#   )
#
#   people_strings_to_extract_regex <- paste0(people_strings_to_extract, collapse = "|")
#   people_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, people_strings_to_extract_regex)
#   moore_report_data$people_strings <- purrr::map_chr(people_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$people_strings <- ifelse(moore_report_data$people_strings == "", NA, moore_report_data$people_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, people_strings_to_extract_regex, "")
#
#
#
#
#
#   content_premium_strings_to_extract <- c(
#     "non-premium",
#     "non premium",
#     "no premium",
#     "non-prem",
#     "non prem",
#     "no prem",
#     "premium",
#     "prem",
#     "pocket calendar",
#     "deluxe card",
#     "Table Top",
#     "Table card",
#     "partnercard",
#     "phone card",
#     "wall calendar",
#     "calendar",
#     "certificate",
#     "dove package (physical piece returned with donation)",
#     "wreath package (physical piece to be returned with donation)",
#     "medallion",
#     "real pen",
#     "bounceback (vouchers)",
#     "cert\\/",
#     "cert ",
#     "cert$",
#     "booklet",
#     "coin\\/",
#     "coin ",
#     "coin$",
#     "dog tag",
#     "postcard",
#     "name labels",
#     "newsletter",
#     "check vouchers",
#     "vouchers",
#     "notepads",
#     "notepad",
#     "stickers",
#     "magazine",
#     "membership card",
#     "stewardship card",
#     "member card",
#     "supporter card",
#     "id card",
#     "greeting card",
#     "inline card",
#     "card",
#     "address label",
#     "label",
#     "nickel",
#     "coupon",
#     "bookmark",
#     "candle",
#     "car magnet",
#     "magnet",
#     "notecard",
#     "garden seed packet",
#     "ornament",
#     "hate by numbers",
#     "backpack tags",
#     "prayer card",
#     "garden seed",
#     "recipe",
#     "decal",
#     "catalog",
#     "placemat",
#     "poster",
#     "bep plushie",
#     "dime"
#   )
#
#   content_premium_strings_to_extract_regex <- paste0(content_premium_strings_to_extract, collapse = "|")
#   content_premium_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, content_premium_strings_to_extract_regex)
#   moore_report_data$content_premium_strings <- purrr::map_chr(content_premium_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$content_premium_strings <- ifelse(moore_report_data$content_premium_strings == "", NA, moore_report_data$content_premium_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, content_premium_strings_to_extract_regex, "")
#
#
#   ask_info_strings_to_extract <- c(
#     "matching gift",
#     "strong match",
#     "triple match",
#     "3x match",
#     "match ",
#     "match$",
#     "match\\/"
#   )
#
#   ask_info_strings_to_extract_regex <- paste0(ask_info_strings_to_extract, collapse = "|")
#   ask_info_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, ask_info_strings_to_extract_regex)
#   moore_report_data$ask_info_strings <- purrr::map_chr(ask_info_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$ask_info_strings <- ifelse(moore_report_data$ask_info_strings == "", NA, moore_report_data$ask_info_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, ask_info_strings_to_extract_regex, "")
#
#
#   content_info_strings_to_extract <- c(
#     "envelope",
#     "self-mailer",
#     "brochure",
#     "digital dm",
#     "insert",
#     "wish appeal",
#     "wish society",
#     "appeal",
#     "internal",
#     "rental",
#     "prev wish",
#     "fill a bus",
#     "invitation",
#     "invite",
#     "letter",
#     "rosh hashanah",
#     "statement",
#     "newseltter",
#     "announcment",
#     "package",
#     "why now",
#     "winners circle",
#     "wish$",
#     "wish ",
#     "wish\\/",
#     "check pkg",
#     "mailing",
#     "thank you",
#     "holiday",
#     "highlights",
#     "stamp and invoice",
#     "bounceback card",
#     "bounceback",
#     "sticker",
#     "headlines",
#     "petition",
#     "story",
#     "giving summary",
#     "impact report",
#     "first class stamp",
#     "impact",
#     "back to school",
#     "national day of prayer",
#     "operation sharing",
#     "stamp",
#     "notebook",
#     "note",
#     "spanish",
#     "english",
#     "challenge",
#     "campaign",
#     "survey",
#     "report",
#     "ask",
#     "coke challenge",
#     "control",
#     "double buckslip",
#     "early",
#     "dad's devotion",
#     "a child's art",
#     "chaser",
#     "main",
#     "from the field",
#     "hispanic heritage month",
#     "kathy & angel",
#     "low cost",
#     "double",
#     "single slip",
#     "triple slip",
#     "neon green",
#     "neon orange",
#     "stretch",
#     "brefaux",
#     "proposal",
#     "value",
#     "total",
#     "mini",
#     "^[a-z]{3} ",
#     "[0-9]x\\+?",
#     "chapter",
#     "test",
#     " ?lag ?$?",
#     "gp$",
#     "gp ",
#     "tippers",
#     "grateful patients",
#     "event",
#     "ag",
#     " ?pssp ?",
#     "active",
#     "doctor's day",
#     "leadership",
#     "lead",
#     "prospectus"
#
#
#   )
#
#   content_info_strings_to_extract_regex <- paste0(content_info_strings_to_extract, collapse = "|")
#   content_info_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, content_info_strings_to_extract_regex)
#   moore_report_data$content_info_strings <- purrr::map_chr(content_info_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$content_info_strings <- ifelse(moore_report_data$content_info_strings == "", NA, moore_report_data$content_info_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, content_info_strings_to_extract_regex, "")
#
#
#   time_period_strings_to_extract <- c(
#     "giving tuesday",
#     "fall",
#     "winter",
#     "spring",
#     "summer",
#     "internal",
#     "annual fund",
#     "year end",
#     "tornado emergency",
#     "emergency",
#     "thanksgiving",
#     "mid-year",
#     "easter",
#     "annual",
#     "christmas"
#   )
#
#   time_period_strings_to_extract_regex <- paste0(time_period_strings_to_extract, collapse = "|")
#   time_period_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, time_period_strings_to_extract_regex)
#   moore_report_data$time_period_strings <- purrr::map_chr(time_period_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$time_period_strings <- ifelse(moore_report_data$time_period_strings == "", NA, moore_report_data$time_period_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, time_period_strings_to_extract_regex, "")
#
#
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "third", "3")
#   moore_report_data$number_in_desc <- readr::parse_number(stringr::str_replace_all(moore_report_data$String_to_Process, "\\|", "")) %>% as.character()
#   unique_number_in_desc <- unique(as.integer(moore_report_data$number_in_desc))
#   unique_number_in_desc <- unique_number_in_desc[!is.na(unique_number_in_desc)] %>% sort(decreasing = TRUE)
#   unique_number_to_extract_regex <- paste0(unique_number_in_desc, collapse = "|")
#   moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, unique_number_to_extract_regex, "")
#
#
#
#
#
#
#   moore_report_data$month_in_info <- tidyr::replace_na(moore_report_data$month_in_info, "Month not found")
#   moore_report_data$campaign_strings <- tidyr::replace_na(moore_report_data$campaign_strings, "campaign_string not found")
#   moore_report_data$number_in_desc <- tidyr::replace_na(moore_report_data$number_in_desc, "Number not found")
#   moore_report_data$content_premium_strings <- tidyr::replace_na(moore_report_data$content_premium_strings, "Premium not found")
#   moore_report_data$ask_info_strings <- tidyr::replace_na(moore_report_data$ask_info_strings, "Ask info not found")
#   moore_report_data$content_info_strings <- tidyr::replace_na(moore_report_data$content_info_strings, "Content info not found")
#   moore_report_data$time_period_strings <- tidyr::replace_na(moore_report_data$time_period_strings, "Time period info not found")
#   moore_report_data$people_strings <- tidyr::replace_na(moore_report_data$people_strings, "People info not found")
#
#
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^[ |]+", "")
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ |]+$", "")
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ |]+", " ")
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^[ \\:\\,\\-\\(\\)\\#\\=\\+]+", " ")
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ \\:\\,\\-\\(\\)\\#\\=\\+\\$\\&\\/\\-//:]", " ")
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " s ", " ")
#
#   clipr::write_clip(moore_report_data)
#
#
#
# }
#


























































# draft 2 -----------------------------------------------------------------




RUN_MOORE_REPORT <- function() {

  path_to_moore_report_data <- file.path(
    "C:",
    "Users",
    "Brian",
    "TCM Dropbox",
    "Brian Kissell",
    "04 MDM Neuro-Fundraising Lab",
    "Research and Development",
    "00 Jobs",
    "2024",
    "006_RD_Moore Report Second Edition",
    "Cleaning_2024_Data.xlsx"
  )

  moore_report_data <- readxl::read_excel(
    path = path_to_moore_report_data,
    sheet = "Combined Data",
    col_types = c("text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text")
  )

  word_replacement_df <- readxl::read_excel(
    path = path_to_moore_report_data,
    sheet = "word replacements",
    col_types = c("text", "text")
  )

  # glimpse(moore_report_data)

  moore_report_data$Campaign_Type <- tidyr::replace_na(moore_report_data$Campaign_Type, "")
  moore_report_data$Package_Type <- tidyr::replace_na(moore_report_data$Package_Type, "")

  moore_report_data$String_to_Process <- stringr::str_c(moore_report_data$Campaign_Type, " | ", moore_report_data$Package_Type)
  moore_report_data$String_to_Process <- stringr::str_to_lower(moore_report_data$String_to_Process)

  # moore_report_data$String_to_Process %>% length()
  # moore_report_data$String_to_Process %>% unique() %>% length()

  text_to_remove <- c(
    "(no 2022 equivalent)",
    "straight"
  ) %>% paste0(collapse = "|")

  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, text_to_remove, "")

  month_strings_to_extract <- c(
    # "mid-january",
    # "mid-february",
    # "mid-march",
    # "mid-april",
    # "mid-may",
    # "mid-june",
    # "mid-july",
    # "mid-august",
    # "mid-september",
    # "mid-october",
    # "mid-november",
    # "mid-december",

    "january",
    "february",
    "march",
    "april",
    "may",
    "june",
    "july",
    "august",
    "september",
    "october",
    "november",
    "december",
    "janaury",
    "septtember",
    "janjary",
    "ocotber",
    "septmeber",
    "feburary",
    "jan ",
    " jan$",
    "^jan$",
    "feb ",
    " feb$",
    "^feb$",
    "mar ",
    " mar$",
    "^mar$",
    "apr ",
    " apr$",
    "^apr$",
    "may ",
    " may$",
    "^may$",
    "jun ",
    " jun$",
    "^jun$",
    "jul ",
    " jul$",
    "^jul$",
    "aug ",
    " aug$",
    "^aug$",
    "sept ",
    " sept$",
    "^sept$",
    "sep ",
    " sep$",
    "^sep$",
    "oct ",
    " oct$",
    "^oct$",
    "nov ",
    " nov$",
    "^nov$",
    "dec ",
    " dec$",
    "^dec$"
  )

  month_strings_to_extract_regex <- paste0(month_strings_to_extract, collapse = "|")
  moore_report_data$month_in_info <- stringr::str_extract(moore_report_data$String_to_Process, month_strings_to_extract_regex)
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, month_strings_to_extract_regex, "")
#
#
#   purrr::walk(seq(1, nrow(word_replacement_df)), ~{
#     iteration <- .x
#     word_find <- word_replacement_df$what_to_replace[[iteration]]
#     word_replacement <- word_replacement_df$replace_with_what[[iteration]]
#     v1_pattern_search <- paste0(" ", word_find, " ")
#     v1_pattern_replacement <- paste0(" ", word_replacement, " ")
#     moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, v1_pattern_search, v1_pattern_replacement)
#     v2_pattern_search <- paste0("^", word_find, " ")
#     v2_pattern_replacement <- paste0(word_replacement, " ")
#     moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, v2_pattern_search, v2_pattern_replacement)
#     v3_pattern_search <- paste0("^", word_find, "$")
#     v3_pattern_replacement <- paste0(word_replacement)
#     moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, v3_pattern_search, v3_pattern_replacement)
#     v4_pattern_search <- paste0(" ", word_find, "$")
#     v4_pattern_replacement <- paste0(" ", word_replacement)
#     moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, v4_pattern_search, v4_pattern_replacement)
#   })

  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^npd ", "notepad ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^npd$", "notepad")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "w\\/ and w\\/o NP", "notepad non-premium")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "reinstatement", " lapsed ")
  # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "renewal reinstatement", "lapsed")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "stop lapsed", "follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "year end", "year-end")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "non-renewed", " lapsed ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "nonpremium", "non-premium")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "non premium", "non-premium")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "no premium", "non-premium")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "non prem$", "non-premium")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "non prem ", "non-premium")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "post-it note", "post-it")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "post it", "post-it")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "pre lapsed", "follow-up")
  moore_report_data %>% dplyr::filter(Costs == 46446.01) %>% pull(String_to_Process)
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "actives", " follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "acqusition", "acquisition")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "acquistion", "acquisition")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "acquision", "acquisition")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "acquisiton", "acquisition")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " acq$", " acquisition")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^acq$", "acquisition")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " acq ", " acquisition ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^acq ", "acquisition ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^and ", " ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^and$", " ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " and$", " ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " and ", " ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "bouceback", "bounceback")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "boucneback", "bounceback")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "bournceback", "bounceback")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "boucenbac", "bounceback")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "cultivation", "acquisition")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "expired", " lapsed ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "folow-up", "follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "followup", "follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "follow up", "follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^follow ", "follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^follow$", "follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " follow$", " follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " follow ", " follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^fup\\$", "follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " fup\\$", " follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^fup\\-", "follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " fup\\-", " follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^fup$", "follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " fup$", " follow-up")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^fup ", "follow-up ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " fup ", " follow-up ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " gp ", " grateful patients ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^gp ", "grateful patients ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^gp$", " grateful patients")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " gp$", " grateful patients")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "high-value", "high-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "high value", "high-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^high\\$", "high-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " high\\$", " high-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "hi-value", "high-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "hi value", "high-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^hi\\$", "high-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " hi\\$", " high-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/hi\\$", " high-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "hi\\/mid", "high-level mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "leadershp", "leadership")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "low-value", "low-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "low value", "low-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^low\\$", "low-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " low\\$", " low-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/low\\$", " low-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "lo-value", "low-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "lo value", "low-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/lo\\$", " low-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^lo\\$", "low-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " lo\\$", " low-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "major donors", "major-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "major donor", "major-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^mg$", "major-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " mg ", " major-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^mg ", "major-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " mg$", " major-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/major ", " major-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/major$", " major-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " major ", " major-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^major ", " major-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^major$", "major-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " major$", " major-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "middle\\/", "mid-level\\/")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/middle ", "\\/mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/middle$", "\\/mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " middle ", " mid-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^middle ", "mid-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^middle$", "mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " middle$", " mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "midlevel", "mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^mid value", "mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " mid value", " mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^mid\\$", "mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " mid\\$", " mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^mid\\/", "mid-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " mid\\/", "mid-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^mid ", "mid-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^mid$", "mid-level")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " mid$", "mid-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " mid ", "mid-level ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^prem ", "premium ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^prem$", "premium")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^prem\\/", "premium ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " prem ", "premium ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " prem$", "premium")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " prem\\/", "premium ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "-prem ", "premium ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "-prem$", "premium")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "-prem\\/", "premium ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/premium", " premium ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " ye ", " year-end ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^ye ", "year-end ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^ye$", "year-end")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " ye$", " year-end ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "houselist \\(middle only\\)", "houselist")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "voucer", "voucher")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "keep", "")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "renewed", "renewal")
  #



  moore_report_data %>% dplyr::filter(Costs == 45095) %>% pull(String_to_Process)

  # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "( \\|){1,7}$", "")
  # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " \\|$", "")
  # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ ]{1,5}", " ")
  # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^[ ]{1,5}", "")

  # audience <- c(
  #   "prospect",
  #   "multi",
  #   "warm",
  #   "internal",
  #   "mid-level",
  #   "high-level",
  #   "low-level",
  #   "stop-lapsed",
  #   "pre-lapsed")

moore_report_data$acquisition <- dplyr::case_when(
  stringr::str_detect(moore_report_data$String_to_Process, " acquisition") ~ "Acquisition",
  stringr::str_detect(moore_report_data$String_to_Process, "^acquisition") ~ "Acquisition",
  stringr::str_detect(moore_report_data$String_to_Process, " multis") ~ "Acquisition",
  stringr::str_detect(moore_report_data$String_to_Process, "^multis") ~ "Acquisition",
  stringr::str_detect(moore_report_data$String_to_Process, " prospect") ~ "Acquisition",
  stringr::str_detect(moore_report_data$String_to_Process, "^prospect") ~ "Acquisition",
  stringr::str_detect(moore_report_data$String_to_Process, "non-members") ~ "Acquisition"

)


moore_report_data$renewal <- dplyr::case_when(
  stringr::str_detect(moore_report_data$String_to_Process, " renewal") ~ "Renewal",
  stringr::str_detect(moore_report_data$String_to_Process, "^renewal") ~ "Renewal"

)

moore_report_data$lapsed <- dplyr::case_when(
  stringr::str_detect(moore_report_data$String_to_Process, " lapsed") ~ "Lapsed",
  stringr::str_detect(moore_report_data$String_to_Process, "^lapsed") ~ "Lapsed",
  stringr::str_detect(moore_report_data$String_to_Process, "annual fund") ~ "Lapsed",
  stringr::str_detect(moore_report_data$String_to_Process, "annual") ~ "Lapsed"
)

moore_report_data$upgrade <- dplyr::case_when(
  stringr::str_detect(moore_report_data$String_to_Process, "upgrade") ~ "Upgrade"
)

moore_report_data$appeal <- dplyr::case_when(
  stringr::str_detect(moore_report_data$String_to_Process, "appeal") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, " appeals") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, "^appeals") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, "donor") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, "high-level") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, "low-level") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, "major-level") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, "mid-level") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, "pre-lapsed") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, "stop-lapsed") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, "sustainer") ~ "Appeal",
  stringr::str_detect(moore_report_data$String_to_Process, "follow-up") ~ "Renewal"
)



moore_report_data$leadership <- dplyr::case_when(
  stringr::str_detect(moore_report_data$String_to_Process, "leadership") ~ "Leadership"
)

n_campaign_types <- as.numeric(!is.na(moore_report_data$acquisition)) +
  as.numeric(!is.na(moore_report_data$renewal)) +
  as.numeric(!is.na(moore_report_data$lapsed)) +
  as.numeric(!is.na(moore_report_data$upgrade)) +
  as.numeric(!is.na(moore_report_data$appeal)) +
  as.numeric(!is.na(moore_report_data$leadership))

campaign_type_label <- paste0(
  moore_report_data$acquisition, "|",
  moore_report_data$renewal, "|",
  moore_report_data$lapsed, "|",
  moore_report_data$upgrade, "|",
  moore_report_data$appeal, "|",
  moore_report_data$leadership) %>%
  stringr::str_replace_all("NA\\||\\|NA", "") %>%
  stringr::str_replace_all("\\|", " \\| ")

moore_report_data$campaign_type_label <- dplyr::case_when(
  n_campaign_types > 1 ~ "Multiple",
  n_campaign_types == 1 ~ campaign_type_label,
  .default = "Not Specified"
  )

moore_report_data$campaign_type_label_with_multiples <- tidyr::replace_na(campaign_type_label, "Appeal")


moore_report_data$supporter_type_label <- dplyr::case_when(
  (!is.na(moore_report_data$renewal) | !is.na(moore_report_data$lapsed) | !is.na(moore_report_data$upgrade) | !is.na(moore_report_data$appeal)) & !is.na(moore_report_data$acquisition) ~ "Mixed",
  !is.na(moore_report_data$renewal) | !is.na(moore_report_data$lapsed) | !is.na(moore_report_data$upgrade) | !is.na(moore_report_data$appeal)  ~ "Supporters",
  !is.na(moore_report_data$acquisition) ~ "Non-Supporters",
  .default = "Not Specified"
)

moore_report_data$supporter_low_level <- dplyr::case_when(
  stringr::str_detect(moore_report_data$String_to_Process, "low-level") ~ "Low Level"
)

moore_report_data$supporter_mid_level <- dplyr::case_when(
  stringr::str_detect(moore_report_data$String_to_Process, "mid-level") ~ "Mid Level"
)

moore_report_data$supporter_high_level <- dplyr::case_when(
  stringr::str_detect(moore_report_data$String_to_Process, "high-level") ~ "High Level"
)

moore_report_data$supporter_major_level <- dplyr::case_when(
  stringr::str_detect(moore_report_data$String_to_Process, "major-level") ~ "Major Level"
)

n_supporter_level_types <- as.numeric(!is.na(moore_report_data$supporter_low_level)) +
  as.numeric(!is.na(moore_report_data$supporter_mid_level)) +
  as.numeric(!is.na(moore_report_data$supporter_high_level)) +
  as.numeric(!is.na(moore_report_data$supporter_major_level))

supporter_level_label <- paste0(
  moore_report_data$supporter_low_level, "|",
  moore_report_data$supporter_mid_level, "|",
  moore_report_data$supporter_high_level, "|",
  moore_report_data$supporter_major_level) %>%
  stringr::str_replace_all("NA\\||\\|NA", "")

moore_report_data$audience_label <- dplyr::case_when(
  n_supporter_level_types > 1 ~ "Multiple",
  n_supporter_level_types == 1 ~ supporter_level_label,
  n_supporter_level_types == 0 ~ "NA"
)




moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "pre-lapsed", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "stop-lapsed", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "acquisition", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "aquisition", "")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "appeal", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "annual fund", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " annual ", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^annual ", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^annual$", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "annual$", "")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "appeal", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " donor ", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^donor ", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^donor$", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " donor$", "")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "follow-up", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "high-level", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " lapsed ", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^lapsed ", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^lapsed$", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " lapsed$", "")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/lapsed ", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/lapsed$", "")

# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " lapsed\\|", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "low-level", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "major-level", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "non-member", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " member ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^member ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^member$", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " member$", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "mid-level", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "multis", "")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "prospect", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "renewal", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "upgrade", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "sustainer", "")


content_non_premium_strings_to_extract <- c(
  "non-premium",
  "3x match",
  "4x8",
  "6x9",
  "letter size",
  "bounceback card",
  "coke challenge",
  "double buckslip",
  "double slip",
  "first class stamp",
  "giving statement",
  "giving summary",
  "id card",
  "impact report",
  "inline card",
  "low cost",
  "main",
  "matching gift",
  "membership card",
  "member card",
  "neon green",
  "neon orange",
  "non-premium (no 2022 equivalent)",
  "re-test",
  "self mailer",
  "single slip",
  "stewardship card",
  "strong match",
  "supporter card",
  "teaser message",
  "triple match",
  "triple slip",
  "certificate of appreciation",
  "announcment",
  "ask",
  "booklet",
  "bounceback",
  "brochure",
  "buckslip",
  "campaign",
  "catalog",
  "certificate",
  " cert\\/",
  " cert ",
  " cert$",
  "^cert\\/",
  "^cert ",
  "^cert$",
  "\\/cert ",
  "\\/cert$",
  "challenge",
  "chapter",
  "chaser",
  "control",
  "coupon",
  "early",
  "envelope",
  "headlines",
  "highlights",
  "impact",
  "insert",
  "invitation",
  "invite",
  "invoice",
  "letter",
  " lead ",
  "^lead ",
  "^lead$",
  "lead$",
  " match ",
  "^match ",
  "^match$",
  " match$",
  "mailing",
  "medallion",
  "mini",
  "multis",
  "multi",
  "newsletter",
  "newseltter",
  "partnercard",
  "petition",
  "print",
  "proposal",
  "prospectus",
  "report",
  "self-mailer",
  "survey",
  "stamp",
  "statement",
  "story",
  "stretch",
  "tested",
  "test",
  "total",
  "thank you",
  "value",
  "shortfall"
)

content_non_premium_strings_to_extract_regex <- paste0(content_non_premium_strings_to_extract, collapse = "|")
content_non_premium_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, content_non_premium_strings_to_extract_regex)
moore_report_data$content_non_premium_strings <- purrr::map_chr(content_non_premium_strings, ~paste0(.x, collapse = " | "))
moore_report_data$content_non_premium_strings <- ifelse(moore_report_data$content_non_premium_strings == "", NA, moore_report_data$content_non_premium_strings)
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, content_non_premium_strings_to_extract_regex, "")



content_premium_strings_to_extract <- c(
  "magnetic notepad",
  "address label",
  "backpack tags",
  " premium ",
  " premium$",
  "^premium ",
  "^premium$",
  "car magnet",
  "check package",
  "check pkg",
  "check vouchers",
  "deluxe card",
  "dog tag",
  "dove package \\(physical piece returned with donation\\)",
  "faux check",
  "garden seed packet",
  "garden seed",
  "tote bag",
  " pens ",
  "^pens ",
  "^pens$",
  " pens$",
  "greeting card",
  "hate by numbers",
  "board calendar",
  "holiday calendar",
  "holiday card",
  "lunch ticket",
  "name labels",
  "phone card",
  "pocket calendar",
  "prayer card",
  "real pen",
  "real pens",
  "recipie card",
  "return address labels",
  "table card",
  "table top",
  "thank you card",
  "wall calendar",
  "window sign conversion",
  "window cling",
  "wreath package (physical piece to be returned with donation)",
  "faux d-gram",
  "bookmark",
  "calendar",
  "candle",
  "cards",
  "card",
  "check",
  " coin ",
  "^coin$",
  "^coin ",
  " coin$",
  "decal",
  "dime",
  "gift wrap",
  "keychain",
  "lapel pin",
  "label",
  "magazine",
  "magnet",
  "medallion",
  "nickel",
  "notebook",
  "notecard",
  "notepads",
  "notepad",
  " note ",
  "^note ",
  "^note$",
  " note$",
  "ornament",
  "placemat",
  "plushie",
  "postcard",
  "poster",
  "post-it",
  "recipe",
  "stamp",
  "sticker",
  "stickers",
  " tax ",
  " tax$",
  "^tax$",
  "^tax ",
  "tags",
  "vouchers",
  "voucher",
  "patch",
  "engagement",
  "flood response update",
  "wreath",
  "socks",
  "pocket organizer"
)

content_premium_strings_to_extract_regex <- paste0(content_premium_strings_to_extract, collapse = "|")
content_premium_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, content_premium_strings_to_extract_regex)
moore_report_data$content_premium_strings <- purrr::map_chr(content_premium_strings, ~paste0(.x, collapse = " | "))
moore_report_data$content_premium_strings <- ifelse(moore_report_data$content_premium_strings == "", NA, moore_report_data$content_premium_strings)
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, content_premium_strings_to_extract_regex, "")

# moore_report_data %>% dplyr::filter(Costs == 87608.4945)

moore_report_data$premium_type_label <- dplyr::case_when(
  stringr::str_detect(moore_report_data$content_non_premium_strings, "non-premium") & !is.na(moore_report_data$content_premium_strings) ~ "Mixed",
  !is.na(moore_report_data$content_premium_strings) ~ "Premium",
  !is.na(moore_report_data$content_non_premium_strings) ~ "No Premium",
  .default = "No Premium"
)


content_misc_strings_to_extract <- c(
  "3-month response",
  "a child's art",
  "annual fund",
  "back to school",
  "childhood cancer awareness month",
  "dad's devotion",
  "doctor's day",
  "fill a bus",
  "from the field",
  "grateful patients",
  "giving tuesday",
  "harvesters circle",
  "hispanic heritage month",
  "kathy & angel",
  "mid-year",
  "national day of prayer",
  "operation sharing",
  "prev wish",
  "rosh hashanah",
  "winners circle",
  "wish list",
  "wish society",
  "why now",
  "year-end",
  "year end",
  "annual",
  " ag ",
  "^ag ",
  " ag$",
  "^ag$",
  "youth camp",
  " camp ",
  "^camp ",
  "^camp$",
  " camp$",
  "pre-christmas",
  "christmas",
  "easter",
  "english",
  "emergency",
  "event",
  "shortfall",
  "fall",
  "floods",
  "gp$",
  " gp ",
  "^gp$",
  "^gp ",
  "gratitude",
  "holiday care",
  "holiday",
  "houselist",
  "hunger",
  "inspirational",
  "initial",
  "internal",

  "leadership",
  "midyear",
  "prospects",
  "regular",
  "rental",
  "summer",
  "spanish",
  "spring",
  "\\(tent ",
  " tent ",
  "^tent ",
  "^tent$",
  " tent$",
  "tippers",
  "pre-thanksgiving",
  "thanksgiving",
  "tornado",
  "tribute",
  "winter",
  "wish",
  "yellow",
  "no hungry neighbors",
  "anti-human trafficking",
  "homelessness",
  "homeless",
  "food drive",
  "drive",
  "location",
  "operation phone home",
  "hometown parade",
  "npnl",
  "double",
  "brefaux")

content_misc_strings_to_extract_regex <- paste0(content_misc_strings_to_extract, collapse = "|")
content_misc_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, content_misc_strings_to_extract_regex)
moore_report_data$content_misc_strings <- purrr::map_chr(content_misc_strings, ~paste0(.x, collapse = " | "))
moore_report_data$content_misc_strings <- ifelse(moore_report_data$content_misc_strings == "", NA, moore_report_data$content_misc_strings)
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, content_misc_strings_to_extract_regex, "")


moore_report_data$Costs

moore_report_data %>% dplyr::filter(Gifts == 22474) %>% pull(String_to_Process)

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " to ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^to ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^to$", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " to$", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " w ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^w ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^w$", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " w$", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/w$", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\/w ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^w\\/", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " w\\/", " ")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " s ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^s ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^s$", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " s$", " ")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\(which won\\)", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "with", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "third", "3")
moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "fy23", "2023")
moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "fy22", "2022")
moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "fy24", "2024")
moore_report_data$number_in_desc <- stringr::str_extract_all(moore_report_data$String_to_Process, "[0-9]{1,10}") %>% purrr::map_chr(~paste0(.x, collapse = " | "))
# moore_report_data$campaign_strings <- purrr::map_chr(captured_campaign_strings, ~paste0(.x, collapse = " | "))
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[0-9]{1,10}", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\#", " ")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "package", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "donors", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "donor", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "annual fund", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "annual", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "active sustainers", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "digital dm", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "included", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "to keep", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " m ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^m ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^m$", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " m$", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\|m ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\|m$", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\-m ", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\-m$", " ")




moore_report_data$String_to_Process <- stringr::str_squish(moore_report_data$String_to_Process)
moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "^s$", "")
moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, " s$", " ")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\|", "") %>%
    stringr::str_trim("both")
moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "[\\s]{2,8}", " | ")

moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "^[ \\|]{1,3}", "")
# moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "[ \\|]{1,3}$", "")
# moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "^[\\| ]{1,3}$", "")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\-", "\\|")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^[ \\:\\,\\-\\(\\)\\#\\=\\+]+", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ \\:\\,\\-\\(\\)\\#\\=\\+\\$\\&\\/\\-//:\\.\\;]", " ")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ ]+", " ")
# moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "\\| \\|", "\\|")
# moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "\\| \\|", "\\|")
# moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "\\| \\|", "\\|")
# moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "^\\|$", "")
# moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "^\\| $", "")
# moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, "^ \\| $", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "\\|", "")

moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "_mem$", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "_mem ", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "_non$", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "_non ", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "_s$", "")
moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "_s ", "")

moore_report_data$content_misc_strings <- paste0(tidyr::replace_na(moore_report_data$content_misc_strings, ""), " ", moore_report_data$String_to_Process)
moore_report_data$content_misc_strings <- tidyr::replace_na(moore_report_data$content_misc_strings, "NA")
moore_report_data$content_misc_strings <- stringr::str_replace(moore_report_data$content_misc_strings, "^[ ]{1,10}$", "NA")
moore_report_data$content_misc_strings <- stringr::str_squish(moore_report_data$content_misc_strings)

moore_report_data$content_premium_strings <- tidyr::replace_na(moore_report_data$content_premium_strings, "NA")
moore_report_data$content_non_premium_strings <- tidyr::replace_na(moore_report_data$content_non_premium_strings, "NA")




# clipr::write_clip(moore_report_data)


moore_report_data_coding_df <- moore_report_data %>%
  dplyr::select(
    "Agency",	"Organization",	"Campaign_Year",	"Campaign_Month",
    "Campaign_Type",	"Package_Type",	"Mailed",	"Gifts",	"Revenue",	"Costs", "Sector",
    "campaign_type_label",
    "campaign_type_label_with_multiples", "supporter_type_label", "audience_label",
    "premium_type_label", "content_premium_strings",
    "content_non_premium_strings", "content_misc_strings"
)


  clipr::write_clip(moore_report_data_coding_df)














}


























































#
#
#
#
#
#
#
#   campaign_strings_to_extract <- c(
#     "acquisition",
#     "renewal",
#     " lapsed",
#     "^lapsed",
#     " donor",
#     "^donor",
#     "non-members",
#     "walk donors",
#     "renewed",
#     "follow-up"
#   )
#
#   campaign_strings_to_extract_regex <- paste0(campaign_strings_to_extract, collapse = "|")
#   captured_campaign_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, campaign_strings_to_extract_regex)
#   moore_report_data$campaign_strings <- purrr::map_chr(captured_campaign_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$campaign_strings <- ifelse(moore_report_data$campaign_strings == "", NA, moore_report_data$campaign_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, campaign_strings_to_extract_regex, "")
#
#   clipr::write_clip(moore_report_data %>% dplyr::arrange(campaign_strings))
#
#
#
#
#
#
#
#
#   audience <- c(
#     "prospect",
#     "multi",
#     "warm",
#     "internal",
#     "mid-level",
#     "high-level",
#     "low-level",
#     "stop lapsed",
#     "pre-lapsed")
#
#   campaign_strings_to_extract <- c(
#
#     "membership renewal",
#     "early renewal",
#     "renewal",
#     "lapsed",
#     "middle$",
#     "middle ",
#     "middle\\/",
#     "mid-level",
#     "midlevel",
#     "mid year",
#     "mid$",
#     "mid ",
#     "mid\\$",
#     "mid\\/",
#     "high-value",
#     "hi$",
#     "hi ",
#     "hi\\$",
#     "high$",
#     "high ",
#     "high\\$",
#     "lo$",
#     "lo ",
#     "lo\\$",
#     "major-level",
#     "major",
#     "donor",
#     "cultivation",
#     "upgrade",
#     "expired",
#     "sustainer",
#     "warm prospect",
#     "initial",
#     "follow-up",
#     "folow-up",
#     "follow up",
#     "followup",
#     "multi$",
#     "multi ",
#     "non-members",
#     "fup\\/",
#     "fup ",
#     "fup$",
#     "segments \\(.+\\)",
#     "active prospect",
#     "tribute donors",
#     "walk donors",
#     "non-renewed",
#     "renewed",
#     "follow"
#   )
#
#   campaign_strings_to_extract_regex <- paste0(campaign_strings_to_extract, collapse = "|")
#   captured_campaign_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, campaign_strings_to_extract_regex)
#   moore_report_data$campaign_strings <- purrr::map_chr(captured_campaign_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$campaign_strings <- ifelse(moore_report_data$campaign_strings == "", NA, moore_report_data$campaign_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, campaign_strings_to_extract_regex, "")
#
#   clipr::write_clip(moore_report_data)
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
#   # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^[ |]+", "")
#   # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ |]+$", "")
#   # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ |]+", " ")
#
#   moore_report_data %>% clipr::write_clip()
#
#   moore_report_data <- moore_report_data %>% dplyr::filter(Agency != "City of Hope")
#
#   unique_package_strings <- moore_report_data$String_to_Process %>% unique()
#
#   test <- purrr::map(seq_along(unique_package_strings), ~{
#     iteration = .x
#     x <- unique_package_strings[[iteration]] %>% stringr::str_split_1("\\|")
#     x %>% stringr::str_trim(side = "both")
#   })
#
#   package_parts_vector <- purrr::reduce(test, c)
#
#   # 10736 rows
#   package_parts_vector %>% length()
#
#   unique_package_parts_vector <- package_parts_vector %>% unique()
#
#   unique_package_parts_vector <- stringr::str_replace_all(unique_package_parts_vector, "\\|", "")
#
#   # 1084
#   unique_package_parts_vector %>% length()
#   unique_package_parts_vector <- unique_package_parts_vector %>% stringr::str_sort()
#
#   unique_package_parts_vector %>% clipr::write_clip()
#
#
#
#
#
#
#   words_in_list <- purrr::map(seq_along(unique_package_parts_vector), ~{
#     iteration = .x
#     x <- stringr::str_trim(unique_package_strings[[iteration]], side = "both")
#     x <- x %>% stringr::str_split_1(" ")
#     x
#   })
#
#   package_parts_vector_words <- purrr::reduce(words_in_list, c)
#
#   # 7359 rows
#   package_parts_vector_words %>% length()
#
#   unique_package_parts_vector_words <- package_parts_vector_words %>% unique()
#   unique_package_parts_vector_words <- stringr::str_replace_all(unique_package_parts_vector_words, "\\|", "")
#
#   unique_package_parts_vector_words %>% length()
#   unique_package_parts_vector_words <- unique_package_parts_vector_words %>% stringr::str_sort()
#
#   unique_package_parts_vector_words %>% clipr::write_clip()
#
#
#
#
#
#
#
#   ask_info_strings_to_extract <- c(

#   )
#
#   ask_info_strings_to_extract_regex <- paste0(ask_info_strings_to_extract, collapse = "|")
#   ask_info_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, ask_info_strings_to_extract_regex)
#   moore_report_data$ask_info_strings <- purrr::map_chr(ask_info_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$ask_info_strings <- ifelse(moore_report_data$ask_info_strings == "", NA, moore_report_data$ask_info_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, ask_info_strings_to_extract_regex, "")
#
#
#   content_info_strings_to_extract <- c(
#     "envelope",
#     "self-mailer",
#     "brochure",
#     "digital dm",
#     "insert",
#     "wish appeal",
#     "wish society",
#     "appeal",
#     "internal",
#     "rental",
#     "prev wish",
#     "fill a bus",
#     "invitation",
#     "invite",
#     "letter",
#     "rosh hashanah",
#     "statement",
#     "newseltter",
#     "announcment",
#     "package",
#     "why now",
#     "winners circle",
#     "wish$",
#     "wish ",
#     "wish\\/",
#     "check pkg",
#     "mailing",
#     "thank you",
#     "holiday",
#     "highlights",
#     "stamp and invoice",
#     "bounceback card",
#     "bounceback",
#     "sticker",
#     "headlines",
#     "petition",
#     "story",
#     "giving summary",
#     "impact report",
#     "first class stamp",
#     "impact",
#     "back to school",
#     "national day of prayer",
#     "operation sharing",
#     "stamp",
#     "notebook",
#     "note",
#     "spanish",
#     "english",
#     "challenge",
#     "campaign",
#     "survey",
#     "report",
#     "ask",
#     "coke challenge",
#     "control",
#     "double buckslip",
#     "early",
#     "dad's devotion",
#     "a child's art",
#     "chaser",
#     "main",
#     "from the field",
#     "hispanic heritage month",
#     "kathy & angel",
#     "low cost",
#     "double",
#     "single slip",
#     "triple slip",
#     "neon green",
#     "neon orange",
#     "stretch",
#     "brefaux",
#     "proposal",
#     "value",
#     "total",
#     "mini",
#     "^[a-z]{3} ",
#     "[0-9]x\\+?",
#     "chapter",
#     "test",
#     " ?lag ?$?",
#     "gp$",
#     "gp ",
#     "tippers",
#     "grateful patients",
#     "event",
#     "ag",
#     " ?pssp ?",
#     "active",
#     "doctor's day",
#     "leadership",
#     "lead",
#     "prospectus"
#   )
#
#   content_info_strings_to_extract_regex <- paste0(content_info_strings_to_extract, collapse = "|")
#   content_info_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, content_info_strings_to_extract_regex)
#   moore_report_data$content_info_strings <- purrr::map_chr(content_info_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$content_info_strings <- ifelse(moore_report_data$content_info_strings == "", NA, moore_report_data$content_info_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, content_info_strings_to_extract_regex, "")
#
#
#   time_period_strings_to_extract <- c(
#     "giving tuesday",
#     "fall",
#     "winter",
#     "spring",
#     "summer",
#     "internal",
#     "annual fund",
#     "year end",
#     "tornado emergency",
#     "emergency",
#     "thanksgiving",
#     "mid-year",
#     "easter",
#     "annual",
#     "christmas"
#   )
#
#   time_period_strings_to_extract_regex <- paste0(time_period_strings_to_extract, collapse = "|")
#   time_period_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, time_period_strings_to_extract_regex)
#   moore_report_data$time_period_strings <- purrr::map_chr(time_period_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$time_period_strings <- ifelse(moore_report_data$time_period_strings == "", NA, moore_report_data$time_period_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, time_period_strings_to_extract_regex, "")
#
#
#
#
#
#   people_strings_to_extract <- c(
#     "hispanic",
#     "[0-9]{1,2}-[0-9]{1,2} ?[mf]{1} ?",
#     "\\$[0-9\\.]{1,6}-\\$?[0-9\\.]{1,6}",
#     "[0-9]{1,4}[mf]{1} ?",
#     "[0-9]{1,4}-[0-9]{1,4}"
#   )
#
#   people_strings_to_extract_regex <- paste0(people_strings_to_extract, collapse = "|")
#   people_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, people_strings_to_extract_regex)
#   moore_report_data$people_strings <- purrr::map_chr(people_strings, ~paste0(.x, collapse = " | "))
#   moore_report_data$people_strings <- ifelse(moore_report_data$people_strings == "", NA, moore_report_data$people_strings)
#   moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, people_strings_to_extract_regex, "")
#
#
#
#
#
#   moore_report_data$month_in_info <- tidyr::replace_na(moore_report_data$month_in_info, "Month not found")
#   moore_report_data$campaign_strings <- tidyr::replace_na(moore_report_data$campaign_strings, "campaign_string not found")
#   moore_report_data$number_in_desc <- tidyr::replace_na(moore_report_data$number_in_desc, "Number not found")
#   moore_report_data$content_premium_strings <- tidyr::replace_na(moore_report_data$content_premium_strings, "Premium not found")
#   moore_report_data$ask_info_strings <- tidyr::replace_na(moore_report_data$ask_info_strings, "Ask info not found")
#   moore_report_data$content_info_strings <- tidyr::replace_na(moore_report_data$content_info_strings, "Content info not found")
#   moore_report_data$time_period_strings <- tidyr::replace_na(moore_report_data$time_period_strings, "Time period info not found")
#   moore_report_data$people_strings <- tidyr::replace_na(moore_report_data$people_strings, "People info not found")
#
#
#
#
#   clipr::write_clip(moore_report_data)
#
#
#
# }
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
# #
# #
# #
# #  #   readr::parse_number(stringr::str_replace_all(moore_report_data$String_to_Process, "\\|", "")) %>% as.character()
# # unique_number_in_desc <- unique(as.integer(moore_report_data$number_in_desc))
# # unique_number_in_desc <- unique_number_in_desc[!is.na(unique_number_in_desc)] %>% sort(decreasing = TRUE)
# # unique_number_to_extract_regex <- paste0(unique_number_in_desc, collapse = "|")
# # moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, unique_number_to_extract_regex, "")
# #
# # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^[ |]+", "")
# # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ |]+$", "")
# # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ |]+", " ")
# # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^[ \\:\\,\\-\\(\\)\\#\\=\\+]+", " ")
# # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ \\:\\,\\-\\(\\)\\#\\=\\+\\$\\&\\/\\-//:]", " ")
# # moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " s ", " ")
# #
# #
# # moore_report_data$String_to_Process %>% clipr::write_clip()
# # moore_report_data$String_to_Process %>% unique() %>% length()













# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "prem", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "pocket calendar", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "wall calendar", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "holiday calendar", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "calendar", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "dove package (physical piece returned with donation)", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "wreath package (physical piece to be returned with donation)", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "real pen", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "dog tag", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "postcard", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "name labels", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "notepads", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "notepad", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "magazine", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "car magnet", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "magnet", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "ornament", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "garden seed packet", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "garden seed", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "poster", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "stickers", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "backpack tags", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "address label", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "label", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "notecard", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "deluxe card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "Table Top", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "Table card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "partnercard", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "phone card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "certificate", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "medallion", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "bounceback (vouchers)", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "cert\\/", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "cert ", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "cert$", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "coin", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "newsletter", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "check vouchers", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "vouchers", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "membership card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "stewardship card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "member card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "supporter card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "id card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "greeting card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "inline card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "notecard", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "prayer card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "nickel", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "card", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "coupon", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "bookmark", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "recipe", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "decal", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "catalog", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "placemat", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "plushie", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "dime", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "check pkg", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "check package", "")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "brochure", "")

# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^multi\\$", " multi-level")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " multi\\$", " multi-level")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "non-prem", "non-premium")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "non prem ", "non-premium ")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "no prem", "non-premium")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " prem ", " premium ")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^prem ", " premium ")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " prem$", " premium ")
# moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^prem$", " premium ")

#
#   moore_report_data$IS_SUPPORTER <- dplyr::case_when(
#     stringr::str_detect(moore_report_data$String_to_Process, " acquisition") ~ "Aquisition",
#     stringr::str_detect(moore_report_data$String_to_Process, "^acquisition") ~ "Aquisition",
#     stringr::str_detect(moore_report_data$String_to_Process, " prospect") ~ "Aquisition",
#     stringr::str_detect(moore_report_data$String_to_Process, "^prospect") ~ "Aquisition",
#     stringr::str_detect(moore_report_data$String_to_Process, " multis") ~ "Aquisition",
#     stringr::str_detect(moore_report_data$String_to_Process, "^multis") ~ "Aquisition"
#   )
#
#
