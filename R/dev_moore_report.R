RUN_MOORE_REPORT <- function() {
  library(dplyr)
  library(stringr)
  library(purrr)

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
    col_types = c("text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric")
  )

  # glimpse(moore_report_data)

  moore_report_data$Campaign_Type <- tidyr::replace_na(moore_report_data$Campaign_Type, "")
  moore_report_data$Package_Type <- tidyr::replace_na(moore_report_data$Package_Type, "")

  moore_report_data$String_to_Process <- str_c(moore_report_data$Campaign_Type, " | ", moore_report_data$Package_Type)
  moore_report_data$String_to_Process <- str_to_lower(moore_report_data$String_to_Process)


  text_to_remove <- c(
    "(no 2022 equivalent)",
    "straight"
  ) %>% paste0(collapse = "|")

  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, text_to_remove, "")


  month_strings_to_extract <- c(
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

  campaign_strings_to_extract <- c(
    "acquisition",
    "acqusition",
    "acquistion",
    "acq$",
    "acq ",
    "membership renewal",
    "early renewal",
    "renewal",
    "pre-lapsed",
    "lapsed",
    "middle$",
    "middle ",
    "middle\\/",
    "mid-level",
    "midlevel",
    "mid year",
    "mid$",
    "mid ",
    "mid\\$",
    "mid\\/",
    "high-value",
    "hi$",
    "hi ",
    "hi\\$",
    "high$",
    "high ",
    "high\\$",
    "lo$",
    "lo ",
    "lo\\$",
    "major-level",
    "major",
    "donor",
    "cultivation",
    "upgrade",
    "expired",
    "sustainer",
    "warm prospect",
    "initial",
    "follow-up",
    "folow-up",
    "follow up",
    "followup",
    "multi$",
    "multi ",
    "non-members",
    "fup\\/",
    "fup ",
    "fup$",
    "segments \\(.+\\)",
    "active prospect",
    "tribute donors",
    "walk donors",
    "non-renewed",
    "renewed",
    "follow"
  )

  campaign_strings_to_extract_regex <- paste0(campaign_strings_to_extract, collapse = "|")
  captured_campaign_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, campaign_strings_to_extract_regex)
  moore_report_data$campaign_strings <- map_chr(captured_campaign_strings, ~paste0(.x, collapse = " | "))
  moore_report_data$campaign_strings <- ifelse(moore_report_data$campaign_strings == "", NA, moore_report_data$campaign_strings)
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, campaign_strings_to_extract_regex, "")

  people_strings_to_extract <- c(
    "hispanic",
    "[0-9]{1,2}-[0-9]{1,2} ?[mf]{1} ?",
    "\\$[0-9\\.]{1,6}-\\$?[0-9\\.]{1,6}",
    "[0-9]{1,4}[mf]{1} ?",
    "[0-9]{1,4}-[0-9]{1,4}"
  )

  people_strings_to_extract_regex <- paste0(people_strings_to_extract, collapse = "|")
  people_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, people_strings_to_extract_regex)
  moore_report_data$people_strings <- map_chr(people_strings, ~paste0(.x, collapse = " | "))
  moore_report_data$people_strings <- ifelse(moore_report_data$people_strings == "", NA, moore_report_data$people_strings)
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, people_strings_to_extract_regex, "")





  content_premium_strings_to_extract <- c(
    "non-premium",
    "non premium",
    "no premium",
    "non-prem",
    "non prem",
    "no prem",
    "premium",
    "prem",
    "pocket calendar",
    "deluxe card",
    "Table Top",
    "Table card",
    "partnercard",
    "phone card",
    "wall calendar",
    "calendar",
    "certificate",
    "dove package (physical piece returned with donation)",
    "wreath package (physical piece to be returned with donation)",
    "medallion",
    "real pen",
    "bounceback (vouchers)",
    "cert\\/",
    "cert ",
    "cert$",
    "booklet",
    "coin\\/",
    "coin ",
    "coin$",
    "dog tag",
    "postcard",
    "name labels",
    "newsletter",
    "check vouchers",
    "vouchers",
    "notepads",
    "notepad",
    "stickers",
    "magazine",
    "membership card",
    "stewardship card",
    "member card",
    "supporter card",
    "id card",
    "greeting card",
    "inline card",
    "card",
    "address label",
    "label",
    "nickel",
    "coupon",
    "bookmark",
    "candle",
    "car magnet",
    "magnet",
    "notecard",
    "garden seed packet",
    "ornament",
    "hate by numbers",
    "backpack tags",
    "prayer card",
    "garden seed",
    "recipe",
    "decal",
    "catalog",
    "placemat",
    "poster",
    "bep plushie",
    "dime"
  )

  content_premium_strings_to_extract_regex <- paste0(content_premium_strings_to_extract, collapse = "|")
  content_premium_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, content_premium_strings_to_extract_regex)
  moore_report_data$content_premium_strings <- map_chr(content_premium_strings, ~paste0(.x, collapse = " | "))
  moore_report_data$content_premium_strings <- ifelse(moore_report_data$content_premium_strings == "", NA, moore_report_data$content_premium_strings)
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, content_premium_strings_to_extract_regex, "")


  ask_info_strings_to_extract <- c(
    "matching gift",
    "strong match",
    "triple match",
    "3x match",
    "match ",
    "match$",
    "match\\/"
  )

  ask_info_strings_to_extract_regex <- paste0(ask_info_strings_to_extract, collapse = "|")
  ask_info_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, ask_info_strings_to_extract_regex)
  moore_report_data$ask_info_strings <- map_chr(ask_info_strings, ~paste0(.x, collapse = " | "))
  moore_report_data$ask_info_strings <- ifelse(moore_report_data$ask_info_strings == "", NA, moore_report_data$ask_info_strings)
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, ask_info_strings_to_extract_regex, "")


  content_info_strings_to_extract <- c(
    "envelope",
    "self-mailer",
    "brochure",
    "digital dm",
    "insert",
    "wish appeal",
    "wish society",
    "appeal",
    "internal",
    "rental",
    "prev wish",
    "fill a bus",
    "invitation",
    "invite",
    "letter",
    "rosh hashanah",
    "statement",
    "newseltter",
    "announcment",
    "package",
    "why now",
    "winners circle",
    "wish$",
    "wish ",
    "wish\\/",
    "check pkg",
    "mailing",
    "thank you",
    "holiday",
    "highlights",
    "stamp and invoice",
    "bounceback card",
    "bounceback",
    "sticker",
    "headlines",
    "petition",
    "story",
    "giving summary",
    "impact report",
    "first class stamp",
    "impact",
    "back to school",
    "national day of prayer",
    "operation sharing",
    "stamp",
    "notebook",
    "note",
    "spanish",
    "english",
    "challenge",
    "campaign",
    "survey",
    "report",
    "ask",
    "coke challenge",
    "control",
    "double buckslip",
    "early",
    "dad's devotion",
    "a child's art",
    "chaser",
    "main",
    "from the field",
    "hispanic heritage month",
    "kathy & angel",
    "low cost",
    "double",
    "single slip",
    "triple slip",
    "neon green",
    "neon orange",
    "stretch",
    "brefaux",
    "proposal",
    "value",
    "total",
    "mini",
    "^[a-z]{3} ",
    "[0-9]x\\+?",
    "chapter",
    "test",
    " ?lag ?$?",
    "gp$",
    "gp ",
    "tippers",
    "grateful patients",
    "event",
    "ag",
    " ?pssp ?",
    "active",
    "doctor's day",
    "leadership",
    "lead",
    "prospectus"


  )

  content_info_strings_to_extract_regex <- paste0(content_info_strings_to_extract, collapse = "|")
  content_info_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, content_info_strings_to_extract_regex)
  moore_report_data$content_info_strings <- map_chr(content_info_strings, ~paste0(.x, collapse = " | "))
  moore_report_data$content_info_strings <- ifelse(moore_report_data$content_info_strings == "", NA, moore_report_data$content_info_strings)
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, content_info_strings_to_extract_regex, "")


  time_period_strings_to_extract <- c(
    "giving tuesday",
    "fall",
    "winter",
    "spring",
    "summer",
    "internal",
    "annual fund",
    "year end",
    "tornado emergency",
    "emergency",
    "thanksgiving",
    "mid-year",
    "easter",
    "annual",
    "christmas"
  )

  time_period_strings_to_extract_regex <- paste0(time_period_strings_to_extract, collapse = "|")
  time_period_strings <- stringr::str_extract_all(moore_report_data$String_to_Process, time_period_strings_to_extract_regex)
  moore_report_data$time_period_strings <- map_chr(time_period_strings, ~paste0(.x, collapse = " | "))
  moore_report_data$time_period_strings <- ifelse(moore_report_data$time_period_strings == "", NA, moore_report_data$time_period_strings)
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, time_period_strings_to_extract_regex, "")


  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "third", "3")
  moore_report_data$number_in_desc <- readr::parse_number(stringr::str_replace_all(moore_report_data$String_to_Process, "\\|", "")) %>% as.character()
  unique_number_in_desc <- unique(as.integer(moore_report_data$number_in_desc))
  unique_number_in_desc <- unique_number_in_desc[!is.na(unique_number_in_desc)] %>% sort(decreasing = TRUE)
  unique_number_to_extract_regex <- paste0(unique_number_in_desc, collapse = "|")
  moore_report_data$String_to_Process <- stringr::str_replace(moore_report_data$String_to_Process, unique_number_to_extract_regex, "")






  moore_report_data$month_in_info <- tidyr::replace_na(moore_report_data$month_in_info, "Month not found")
  moore_report_data$campaign_strings <- tidyr::replace_na(moore_report_data$campaign_strings, "campaign_string not found")
  moore_report_data$number_in_desc <- tidyr::replace_na(moore_report_data$number_in_desc, "Number not found")
  moore_report_data$content_premium_strings <- tidyr::replace_na(moore_report_data$content_premium_strings, "Premium not found")
  moore_report_data$ask_info_strings <- tidyr::replace_na(moore_report_data$ask_info_strings, "Ask info not found")
  moore_report_data$content_info_strings <- tidyr::replace_na(moore_report_data$content_info_strings, "Content info not found")
  moore_report_data$time_period_strings <- tidyr::replace_na(moore_report_data$time_period_strings, "Time period info not found")
  moore_report_data$people_strings <- tidyr::replace_na(moore_report_data$people_strings, "People info not found")


  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^[ |]+", "")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ |]+$", "")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ |]+", " ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "^[ \\:\\,\\-\\(\\)\\#\\=\\+]+", " ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, "[ \\:\\,\\-\\(\\)\\#\\=\\+\\$\\&\\/\\-//:]", " ")
  moore_report_data$String_to_Process <- stringr::str_replace_all(moore_report_data$String_to_Process, " s ", " ")

  clipr::write_clip(moore_report_data)



}

