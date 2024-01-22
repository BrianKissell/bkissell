#' ################################################################################
#'
#'
#'
#'
#' ################################################################################
#'
#'
#' #' convert_to_time_character
#' #' Create a function to convert the seconds to a character that is easier to interpret
#' #'
#' #' @param seconds seconds
#' #'
#' #' @return duration_time
#' #' @export
#' #'
#'
#' convert_to_time_character <- function(seconds){
#'   # Obtain the minutes
#'   minutes_part <- round(as.integer(seconds) / 60, 0)
#'
#'   # Obtain the seconds
#'   seconds_part <- as.integer(seconds) %% 60
#'
#'   # Combine the minutes and the seconds
#'   duration_time <- paste0(minutes_part, ":", seconds_part)
#'
#'   # Return the duration
#'   return(duration_time)
#' }
#'
#' #' convert_to_zeros
#' #'
#' #' @param variable variable
#' #'
#' #' @return x
#' #' @export
#' #'
#' convert_to_zeros <- function(variable){
#'   # If it is not missing make it 1
#'   x <-ifelse(!is.na(variable), 1, 0)
#'
#'   # Return it
#'   return(x)
#' }
#'
#' #' convert_missing_to_na
#' #'
#' #' @param variable variable
#' #' @param missing missing
#' #'
#' #' @return x
#' #' @export
#' #'
#' convert_missing_to_na <- function(variable, missing){
#'   # If missing, NA or variable
#'   x <-ifelse(missing, NA, variable)
#'
#'   # Return
#'   return(x)
#' }
#'
#' #' convert_missing_to_zeros
#' #'
#' #' @param variable variable
#' #'
#' #' @return x
#' #' @export
#' #'
#' convert_missing_to_zeros <- function(variable){
#'   x <- ifelse(!is.na(variable), variable, 0)
#'
#'   # Return x
#'   return(x)
#' }
#'
#'
#'
#' ################################################################################
#' # Create Folders if they do not exist ------------------------------------------------
#'
#' #' create_folders_if_they_do_not_exist
#' #'
#' #' @param list_of_folder_paths list_of_folder_paths
#' #'
#' #' @export
#' #'
#' create_folders_if_they_do_not_exist <- function(list_of_folder_paths) {
#'
#'   if(is.list(list_of_folder_paths)) {
#'     # If the folder does not exist, create it
#'     for(folder_path in list_of_folder_paths) {
#'       if (!dir.exists(folder_path)){
#'         dir.create(folder_path)
#'         message("New directory folders have been created")
#'       }
#'     }
#'   } else {
#'     rlang::abort("`list_of_folder_paths` is not a list")
#'   }
#'
#' }
#'
#'
#'
#' #' if_no_rids_csv_make_one
#' #'
#' #' @param path_to_csv path_to_csv
#' #'
#' #' @export
#' #'
#' if_no_rids_csv_make_one <- function(path_to_csv) {
#'
#'   path_exists_lgl <- file.exists(path_to_csv)
#'
#'   if(path_exists_lgl == FALSE) {
#'
#'     rids_df <- data.frame("rids" = character(0))
#'
#'     readr::write_csv(rids_df, path_to_csv)
#'   }
#' }
#'
#' #' if_no_rids_csv_make_one
#' #'
#' #' @param path_to_csv path_to_csv
#' #' @param df df
#' #'
#' #' @export
#' #'
#' if_no_rids_csv_to_keep_make_one <- function(path_to_csv, df) {
#'
#'   path_exists_lgl <- file.exists(path_to_csv)
#'
#'   if(path_exists_lgl == FALSE) {
#'
#'     rids_df <- data.frame("rids" = unique(df$RID))
#'
#'     readr::write_csv(rids_df, path_to_csv)
#'   }
#' }
#'
#'
#' #' remove_rids_from_df
#' #'
#' #' @param df df
#' #' @param dir_path dir_path
#' #'
#' #' @return df
#' #' @export
#' #'
#' remove_rids_from_df <- function(dir_path, df) {
#'   rids_to_remove_path <- paste0(dir_path, "/rids_to_remove.csv")
#'
#'   # Add empty rid df if it does not exist
#'   if_no_rids_csv_make_one(rids_to_remove_path)
#'
#'   # Read in the csv that contains the rids to remove
#'   rids_to_remove <- readr::read_csv(rids_to_remove_path, show_col_types = FALSE) %>% dplyr::pull(rids)
#'
#'   # Keep the completed data
#'   df <- df %>%
#'     dplyr::filter(!(RID %in% rids_to_remove))
#'
#'   # return the df
#'   return(df)
#' }
#'
#'
#' #' keep_rids_for_df
#' #'
#' #' @param df df
#' #' @param dir_path dir_path
#' #'
#' #' @return df
#' #' @export
#' #'
#' keep_rids_for_df <- function(dir_path, df) {
#'
#'   rids_to_keep_path <- paste0(dir_path, "/rids_to_keep.csv")
#'
#'   # Add empty rid df if it does not exist
#'   if_no_rids_csv_to_keep_make_one(rids_to_keep_path, df)
#'
#'   # Read in the csv that contains the rids to remove
#'   rids_to_keep <- readr::read_csv(rids_to_keep_path, show_col_types = FALSE) %>% dplyr::pull(rids)
#'
#'   # Keep the completed data
#'   df <- df %>%
#'     dplyr::filter((RID %in% rids_to_keep))
#'
#'   # return the df
#'   return(df)
#' }
