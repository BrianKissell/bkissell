#' process_ddtv_data <- function() {
#'   # Create path to the DRTV data folder
#'   DRTV_DATA_PATH <- "C:/Users/Brian/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research and Development/00 Jobs/2023/003_RD_CodingVideoContent/DDTV Data/DRTV"
#'
#'   # Create a vector with all of the table names
#'   DRTV_FILE_NAMES <- c("KTDtl", "LookupDisposition", "LookupNetwork", "LookupSpotcode", "PostlogHdr", "ResponseMaster","ResultsHdr")
#'
#'   # Read in the data for all of the tables
#'   LIST_DRTV_DFS <- purrr::map(DRTV_FILE_NAMES, ~{
#'     # Create the path
#'     file_path_to_read <- paste0(DRTV_DATA_PATH, "/", .x, ".csv")
#'     # Read it in
#'     readr::read_csv(file_path_to_read, show_col_types = FALSE)
#'   }) %>%
#'     # And set the names of the lists so you can easily access the tables
#'     purrr::set_names(DRTV_FILE_NAMES)
#'
#'   # KTDtl provides the information about every run, for multiple demographics.
#'   # Since I am not sure where the demograhpic data is being tracked, I created
#'   # a new table that contains a row for every unique run.
#'   unique_aired_events <- LIST_DRTV_DFS$KTDtl %>%
#'     dplyr::group_by(AirDetectedEvent) %>%
#'     slice_head(n=1) %>%
#'     select(-c("TypeOfDemographic", "Demographic"))
#'
#'
#'
#'
#'
#'
#'   #
#'   # df_for_unique_isci <- KTDtl %>% dplyr::select(ISCILength, ISCIAir, CmmlTitle)
#'   # df_for_unique_isci$CmmlTitle_additional_info <- stringr::str_extract(df_for_unique_isci$CmmlTitle, " [A-Z]{2} :.+")
#'   # df_for_unique_isci$CmmlTitle_clean <- stringr::str_replace(df_for_unique_isci$CmmlTitle, " [A-Z]{2} :.+", "")
#'   # df_for_unique_isci <- df_for_unique_isci %>%
#'   #   distinct() %>%
#'   #   select(ISCIAir, CmmlTitle_clean, ISCILength, CmmlTitle_additional_info, CmmlTitle)
#'   #
#'   # PostlogHdr %>%
#'   #   left_join(df_for_unique_isci, by = c("SpotCode" = "ISCIAir")) %>% pull(CmmlTitle_clean)
#'   #
#'
#'
#'
#'
#'
#'   ### Set up phone results table
#'   # Convert all variables that start with T1R to character strings and then pivot them to the long format
#'   Long_Results <-  LIST_DRTV_DFS$ResultsHdr %>%
#'     dplyr::mutate(across(starts_with("T1R"), as.character)) %>%
#'     tidyr::pivot_longer(starts_with("T1R"), names_to = "names", values_to = "values")
#'
#'   # Use the name to extract the time point
#'   Long_Results$T_Point <- stringr::str_extract(Long_Results$names, "^T[0-9]{1,2}")
#'
#'   # Use the name to extract the name
#'   Long_Results$names <- stringr::str_replace(Long_Results$names, "^T[0-9]{1,2}", "")
#'
#'   # Use the name to extract the R Point
#'   Long_Results$R_Point <- stringr::str_extract(Long_Results$names, "^R[0-9]{1,2}")
#'
#'   # Use the name to extract the names
#'   Long_Results$names <- stringr::str_replace(Long_Results$names, "^R[0-9]{1,2}", "")
#'
#'   # Pivot it back to the wide format
#'   Long_Results <- Long_Results %>%
#'     tidyr::pivot_wider(names_from = "names", values_from = "values")
#'
#'   # Convert the Donat variable to numeric
#'   Long_Results$Donat <- as.numeric(Long_Results$Donat)
#'
#'   # If it is zero, convert it to NA
#'   Long_Results$Donat <- ifelse(Long_Results$Donat == 0, NA, Long_Results$Donat)
#'
#'   # Remove missing data and unmatched data
#'   Long_Results <- Long_Results %>%
#'     dplyr::filter(!is.na(Donat)) %>%
#'     dplyr::filter(Spot != "Unmatched")
#'
#'   # Create the Video_code_BK table
#'   Long_Results$Video_code_BK <- stringr::str_replace(Long_Results$Spot, "[a-zA-Z]{3}$", "")
#'
#'
#'   Video_names_lookup <- LookupSpotcode %>% dplyr::select(UIC_Spot_Lookup = UIC, Video_code_BK, SpotDesc)
#'
#'   Video_names_lookup <- Video_names_lookup %>%
#'     dplyr::group_by(Video_code_BK) %>%
#'     slice_head(n=1)
#'
#'   results_with_video_name <- Long_Results %>%
#'     dplyr::left_join(Video_names_lookup, by = "Video_code_BK")
#'
#'   prep_LookupDisposition <- LookupDisposition %>%
#'     dplyr::select(-c("UIC", "Client", "Notes"))
#'
#'   # Add disposition to the results
#'   results_with_video_name_w_disposition <- results_with_video_name %>%
#'     dplyr::left_join(prep_LookupDisposition, by = c("Disposition" = "DispCode"))
#'
#'   results_with_video_name$date_time_of_donation <- lubridate::mdy_hms(paste0(results_with_video_name$ResponseDate, " ", results_with_video_name$AirTime))
#'
#'
#'   clipr::write_clip(results_with_video_name)
#'
#'   summarized_results_by_video <- results_with_video_name %>%
#'     dplyr::group_by(Client, Video_code_BK, SpotDesc) %>%
#'     dplyr::summarise(sum = sum(Donat), mean = mean(Donat), sd = sd(Donat), n = n())
#'
#'   summarized_results_by_video  <- summarized_results_by_video %>%
#'     dplyr::filter(!is.na(SpotDesc))
#'
#'   clipr::write_clip(summarized_results_by_video)
#'   #
#'
#'
#'
#'   results_with_video_name
#'
#'   PostlogHdr$AirDate
#'
#'   PostlogHdr$date_time_of_donation <- lubridate::mdy_hms(paste0(PostlogHdr$AirDate, " ", PostlogHdr$AirTime))
#'
#'
#' }
#'
#'
#'
#' #' find_segments_that_are_missing_start
#' #'
#' #' @param data_for_calcs_wide data_for_calcs_wide
#' #'
#' #' @return segments_that_are_missing_start
#' #' @export
#' #'
#' find_segments_that_are_missing_start <- function(data_for_calcs_wide) {
#'   # # Obtain the sections that are missing the start mark
#'   # segments_that_are_missing_start <- data_for_calcs_wide %>%
#'   #   dplyr::group_by(.data[["section_label"]], .data[["video_name"]], .data[["coded_by"]]) %>%
#'   #   summarize(
#'   #     not_coded = is.na(.data[["section"]]) %>% all(),
#'   #     time_point_start =.data[["time_point"]]
#'   #     ) %>%
#'   #   dplyr::filter(not_coded == TRUE)
#'
#'   # Obtain the sections that are missing the start mark
#'   segments_that_are_missing_start <- data_for_calcs_wide %>%
#'     dplyr::group_by(.data[["section_label"]], .data[["video_name"]], .data[["coded_by"]]) %>%
#'     summarize(
#'       not_coded = is.na(.data[["section"]]) %>% all(),
#'       .groups = "drop"
#'     ) %>%
#'     dplyr::filter(not_coded == TRUE)
#'
#'   # Return the data
#'   return(segments_that_are_missing_start)
#' }
#'
