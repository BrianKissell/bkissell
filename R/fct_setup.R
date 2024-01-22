#' #' get_path_part_to_adjust
#' #'
#' #' @return path_part_to_adjust
#' #' @export
#' #'
#' get_path_part_to_adjust <- function() {
#'
#'   # Get the parts of the working directory
#'   parts_of_wd <- unlist(strsplit(getwd(), .Platform$file.sep))
#'
#'   # Put the first 3 parts together
#'   path_part_to_adjust <- paste0(parts_of_wd[1:3], collapse = "/")
#'
#'   # Return the string
#'   return(path_part_to_adjust)
#' }
#'
#'
#' #' obtain_path_for_user_credentials
#' #'
#' #' @param path_part_to_adjust path_part_to_adjust
#' #'
#' #' @return path_to_credentials
#' #' @export
#' #'
#' obtain_path_for_user_credentials <- function(path_part_to_adjust){
#'
#'   # Create the path to the credentials file
#'   path_to_credentials <- paste0(path_part_to_adjust, "/Dropbox (TCM)/04 MDM Neuro-Fundraising Lab/Research Tools/MNFLAB TOOLS/Misc/user_credential.csv")
#'
#'   # Return the path
#'   return(path_to_credentials)
#' }
#'
#'
#' #' initiate_project_and_design_details
#' #'
#' #' @return user_credentials
#' #' @export
#' #'
#' initiate_project_and_design_details <- function() {
#'
#'   # Break the parts down
#'   the_path_part_to_adjust <- get_path_part_to_adjust()
#'
#'   # Get the path for the credentials
#'   path_for_user_credentials <- obtain_path_for_user_credentials(the_path_part_to_adjust)
#'
#'   # Read in the credentials, and filter for the person using the program
#'   user_credentials <- readr::read_csv(path_for_user_credentials, show_col_types = FALSE) %>%
#'
#'     # and filter for the person using the program
#'     dplyr::filter(path_part_to_adjust == {{the_path_part_to_adjust}})
#'
#'   # Set username for user
#'   user_credentials_username <<- user_credentials$username
#'
#'   # Set password for user
#'   user_credentials_password <<- user_credentials$password
#'
#'   # Return the dataframe
#'   return(user_credentials)
#' }
