#' # Create engine for selenium ------------------------------------------------
#'
#' #' Create remDr, but this does not seem to work, so do not use it.
#' #'
#' #' @param browser what browser should be used
#' #' @param port What port should be used
#' #' @param chromever What version
#' #' @param verbose Should it show messages
#' #'
#' #' @return remDr
#' #' @export
#' #'
#'
#' create_remDr <- function(
#'     browser = "chrome",
#'     port = netstat::free_port(),
#'     chromever  = "114.0.5735.90",
#'     verbose = FALSE) {
#'   # Create the driver
#'   rs_driver <- RSelenium::rsDriver(
#'     browser = browser,
#'     port = port,
#'     chromever  = chromever,
#'     verbose = verbose
#'   )
#'   # prepare driver
#'   remDr <- rs_driver[["client"]]
#'   # Return it
#'   return(remDr)
#' }
#'
#'
#' ################################################################################
#' # Initiate User Credentials -----------------------------------------------
#'
#' #' initiate_credentials_for_user
#' #' Provide Credentials for User account through the use of selenium
#' #'
#' #' @param remDr Selenium client used to control the browser
#' #'
#' #' @return remDr
#' #' @export
#' #'
#'
#' initiate_credentials_for_user <- function(remDr){
#'
#'   # Obtain the credentials
#'   user_credentials <- initiate_project_and_design_details()
#'
#'   # Set username for user
#'   user_credentials_username <- user_credentials$username
#'
#'   # Set password for user
#'   user_credentials_password <- user_credentials$password
#'
#'   # Navigate to User's sign-in page
#'   remDr$navigate("https://www.userinterviews.com/accounts/signin?source=signin-page")
#'
#'   # Give the page a half second to load
#'   Sys.sleep(.5)
#'
#'   # Enter your username
#'   remDr$findElement('xpath', '//*[@id="email"]')$sendKeysToElement(list(user_credentials_username))
#'
#'   # Enter your password
#'   remDr$findElement('xpath', '//*[@id="password"]')$sendKeysToElement(list(user_credentials_password))
#'
#'   # Submit your credentials to initialize the two step process
#'   remDr$findElements("css", "button.Button:nth-child(1)")[[1]]$clickElement()
#'
#'   # To finish, you will need to mannually enter your verification code into the browser
#'   message("Please go to your email and obtain the verification code. Manually enter the code into the selenium web-browser and click enter")
#'
#'   return(remDr)
#' }
#'
#' ################################################################################
#' # Wait to detect and element ----------------------------------------------
#'
#'
#' #' While using R Selenium, wait until an element appears
#' #'
#' #' @param remDr Selenium client used to control the browser.
#' #' @param element A css element as a string.
#' #' @param sleep_time second to wait between loops.
#' #' @param give_up_count_max how many times to try the loops.
#' #' @param using type of selector
#' #'
#' #' @return ele
#' #' @export
#' #'
#' #'
#' wait_to_detect_element <- function(remDr, element, using = "css selector", sleep_time = .1, give_up_count_max = 999){
#'   # Set the search status to "Search" to begin looking for the element
#'   search_status <- "Search"
#'   give_up_count <- give_up_count_max
#'   # Begin the while loop
#'   while(search_status == "Search"){
#'     # Try to find the specified element, and store the results of the attempt in ele.
#'     ele <- try(remDr$findElement(using, element), silent = TRUE)
#'     # Check to see if an error was detected, which would mean it has not been found.
#'     if("try-error" %in% class(ele)){
#'       # If not found, wait the specified time
#'       Sys.sleep(sleep_time)
#'       # And subtract 1 from the give_up_count
#'       give_up_count <- give_up_count - 1
#'       # Check whether count is at zero, and if so, stop
#'       if(give_up_count == 0){
#'         search_status <- "Stop"
#'         # Throw error and indicate that the element was not found.
#'         rlang::abort(paste0("We were not able to detect the css element ", element))
#'       }
#'     } else {
#'       # If the element is found, change search status
#'       search_status <- "Stop"
#'     }
#'   }
#'   return(invisible(ele))
#' }
#'
#'
#' ################################################################################
#' # Click element once it appears -------------------------------------------
#'
#'
#' #' click on element once it appears
#' #'
#' #' @param remDr Selenium client used to control the browser.
#' #' @param element A css element as a string.
#' #' @param sleep_time second to wait between loops.
#' #' @param give_up_count_max how many times to try the loops.
#' #' @param using type of selector
#' #'
#' #' @return ele
#' #' @export
#' #'
#'
#' click_once_element_appears <- function(remDr, element, using = "css selector", sleep_time = .1, give_up_count_max = 999){
#'   # Wait until the css element appears
#'   wait_to_detect_element(remDr, element, using, sleep_time, give_up_count_max)
#'   # Assign the element to ele
#'   ele <- try(remDr$findElement(using, element), silent = TRUE)
#'   # If the element is found,
#'   if(!("try-error" %in% class(ele))){
#'     # Click on the element
#'     ele$clickElement()}
#'   return(invisible(ele))
#' }
#'
#'
#' ################################################################################
#' # Wait for text to appear -------------------------------------------------
#'
#' #' wait_for_text
#' #' While using R Selenium, wait until specified text appears in an element
#' #'
#' #' @param remDr Selenium client used to control the browser.
#' #' @param element A css element as a string.
#' #' @param desired_text Specified text you are looking for
#' #' @param sleep_time second to wait between loops.
#' #' @param give_up_count_max ow many times to try the loops.
#' #' @param using which type of selector is being used
#' #'
#' #' @export
#' #'
#'
#' wait_for_text <- function(remDr, element, desired_text, using = "css selector", sleep_time = .1, give_up_count_max = 999){
#'   # wait_to_detect_element(remDr, element, using, sleep_time, give_up_count_max)
#'   search_status <- "Search"
#'   give_up_count <- give_up_count_max
#'   # Begin for loop
#'   while(search_status == "Search"){
#'     ele <- try(remDr$findElement(using, element), silent = TRUE)
#'     if(!("try-error" %in% class(ele))){
#'       # Read the html
#'       html <- remDr$getPageSource()[[1]] %>% rvest::read_html()
#'       text_of_html <- html %>% rvest::html_node(element) %>% rvest::html_text()
#'       # Test if it matches the desired text and stop loop if found
#'       if(give_up_count_max == 0) rlang::warn("Text was not found, so program was terminated")
#'       if(identical(text_of_html, desired_text)){
#'         search_status <- "Stop"
#'       } else {
#'         give_up_count_max <- give_up_count_max - 1
#'       }
#'     }
#'   }
#' }
#'
#' ################################################################################
#' # Download the User Data from the projects --------------------------------
#'
#' #' Download the user data from a vector of project urls
#' #'
#' #' @param user_participant_urls user_participant_urls
#' #' @param remDr engine
#' #'
#' #' @export
#' #'
#' download_user_data_from_projects <- function(remDr, user_participant_urls) {
#'
#'   # Loop through each url
#'   for(url in user_participant_urls){
#'
#'     # Navigate to the correct url
#'     remDr$navigate(url)
#'
#'     # Click on Actions once it appears
#'     suppressMessages(click_once_element_appears(remDr = remDr, element = "#ui-window-root > div > div > div.project-workspace > div.project-workspace__container > div.project-workspace__participants > div.card.ui-card.project-workspace__participants__heading > div.btn-toolbar > div.project-workspace__participants__heading__actions > div > div.Dropdown.dropdown > button", using = "css selector"))
#'
#'     # Click first option in dropdown
#'     suppressMessages(click_once_element_appears(remDr = remDr, element = "#ui-window-root > div > div > div.project-workspace > div.project-workspace__container > div.project-workspace__participants > div.card.ui-card.project-workspace__participants__heading > div.btn-toolbar > div.project-workspace__participants__heading__actions > div > div.Dropdown.show.dropdown > div > a:nth-child(2)", using = "css selector"))
#'
#'     # Click to begin to export the data once it appears
#'     suppressMessages(click_once_element_appears(remDr = remDr, element = "div.ModalFooter > button.Button.btn.btn-primary", using = "css selector"))
#'
#'     # Wait for export to become ready
#'     suppressMessages(wait_for_text(remDr = remDr, element = "#export-participants-modal-title", desired_text = "Your export is ready for download", using = "css selector", sleep_time = 1, give_up_count_max = 500))
#'
#'     # Wait for the button to appear
#'     suppressMessages(wait_to_detect_element(remDr, element = "div.ModalFooter > button.btn.btn-primary", using = "css selector"))
#'
#'     # Click Download data
#'     suppressMessages(click_once_element_appears(remDr, element = "div.ModalFooter > button.btn.btn-primary", using = "css selector"))
#'
#'     # Print a message to inform the user that it has been scraped
#'     message(paste0("Website to scrape: ", url))
#'   }
#'
#'   # Make it sleep a second
#'   Sys.sleep(1)
#' }
#' #
#' ################################################################################
#' # Download and save html schedules ----------------------------------------
#'
#' #' download_schedule_data_from_user
#' #' Run to scrape the schedule and save to html files
#' #'
#' #' @param home_dir home directory
#' #' @param remDr engine
#' #' @param project_numbers project numbers
#' #'
#' #' @return file_paths
#' #' @export
#' #'
#' #
#' # project_numbers = 109614
#'
#' download_schedule_data_from_user <- function(remDr, home_dir, project_numbers){
#'
#'   user_session_urls <- paste0("https://www.userinterviews.com/projects/", project_numbers, "/sessions")
#'
#'   file_paths <- c()
#'
#'   for(i in user_session_urls){
#'     # Navigate to the website
#'     remDr$navigate(i)
#'
#'     is_the_element_present  <- is_element_or_text_present(
#'       remDr,
#'       element = "section.card.ui-card",
#'       using = "css selector",
#'       text_element = "h4.EmptyState__title",
#'       desired_text = "No sessions currently scheduled",
#'       num_of_iterations = 10000,
#'       wait_time = .25
#'     )
#'
#'     # obtain the html from this page
#'     html <- remDr$getPageSource()[[1]] %>% rvest::read_html()
#'
#'     # Scrape the project name
#'     project_name <- html %>% rvest::html_node("h1") %>% rvest::html_text() %>% stringr::str_trim()
#'
#'     # Set where the file should be saved
#'     dir_path <- paste0(home_dir, "/Data Collection/Project Files/Recruitment/Schedule")
#'     file_path <- paste0(dir_path, "/", snakecase::to_snake_case(project_name), ".html")
#'     file_paths <- c(file_paths, file_path)
#'
#'     # Save the file
#'     fileConn <- file(file_path)
#'     writeLines(as.character(html), fileConn)
#'     close(fileConn)
#'
#'     # Provide Message
#'     message(paste0("Successfully downloaded project # ", project_name))
#'   }
#'
#'   return(file_paths)
#' }
#'
#' ################################################################################
#' # Test if text is present -------------------------------------------------
#'
#' #' is_text_present
#' #'
#' #' @param desired_text desired_text
#' #' @param wait_time wait_time
#' #' @param text_element text_element
#' #' @param num_of_iterations num_of_iterations
#' #' @param remDr remDr
#' #'
#' #' @return found_text
#' #' @export
#' #'
#' is_text_present <- function(
#'     remDr,
#'     text_element,
#'     desired_text,
#'     num_of_iterations = 10,
#'     wait_time = .25
#' ) {
#'   # Make N attempts in a loop
#'   for(attempt in seq(1, num_of_iterations)){
#'
#'     # Look to see if the element is present
#'     ele <- try(remDr$findElement(using, text_element), silent = TRUE)
#'
#'     # If it is not
#'     if(("try-error" %in% class(ele))){
#'
#'       # Mark that it is not, and
#'       found_text <- FALSE
#'
#'       # And wait for assigned ammount of time
#'       Sys.sleep(wait_time)
#'
#'       # Move to next iteration
#'       next()
#'     } else {
#'
#'       # Read the html
#'       html <- remDr$getPageSource()[[1]] %>% rvest::read_html()
#'
#'       # Obtain the text
#'       text_of_html <- html %>% rvest::html_node(text_element) %>% rvest::html_text()
#'
#'       # Test if it matches the desired text and stop loop if found
#'       if(identical(text_of_html, desired_text)){
#'
#'         # If it does, label it true
#'         found_text <- TRUE
#'
#'         # and end the function
#'         break()
#'       } else {
#'         # Otherwise mark it false
#'         found_text <- FALSE
#'
#'         # and wait
#'         sys.sleep(wait_time)
#'
#'         # and then move to next iteration
#'         next()
#'       }
#'     }
#'   }
#'
#'   # Return the result of the test
#'   return(found_text)
#' }
#'
#'
#' # Test if either an element or text is present on the screen --------------
#'
#'
#'
#'
#' #' is_element_or_text_present
#' #'
#' #' @param remDr Selenium client used to control the browser.
#' #' @param element A css element as a string.
#' #' @param using type of selector
#' #' @param text_element text_element
#' #' @param desired_text desired_text
#' #' @param num_of_iterations num_of_iterations
#' #' @param wait_time wait_time
#' #'
#' #' @return found_element
#' #' @export
#' #'
#' #'
#' is_element_or_text_present <- function(
#'     remDr,
#'     element,
#'     using = "css selector",
#'     text_element,
#'     desired_text,
#'     num_of_iterations = 10,
#'     wait_time = .25
#' ){
#'   for(iteration in seq(1, num_of_iterations)){
#'
#'     # Try to find the specified element, and store the results of the attempt in ele.
#'     ele <- try(remDr$findElement(using, element), silent = TRUE)
#'
#'     # Check to see if an error was detected, which would mean it has not been found.
#'     if("try-error" %in% class(ele)){
#'
#'       found_element <- FALSE
#'
#'       found_text <- is_text_present(
#'         text_element = text_element, desired_text = desired_text,
#'         num_of_iterations = 2, wait_time = wait_time
#'       )
#'
#'       if(found_text) {
#'         found_element <- FALSE
#'         break()
#'       } else {
#'         next()
#'       }
#'     } else {
#'       found_element <- TRUE
#'       break()
#'     }
#'   }
#'   return(found_element)
#' }
