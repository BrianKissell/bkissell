# tcm_video_Web_Scraper <- function() {
#   library(rvest)
#   library(dplyr)
#
#   scraplinks <- function(url){
#     # Create an html document from the url
#     webpage <- xml2::read_html(url)
#     # Extract the URLs
#     url_ <- webpage %>%
#       rvest::html_nodes("a") %>%
#       rvest::html_attr("href")
#     # Extract the link text
#     link_ <- webpage %>%
#       rvest::html_nodes("a") %>%
#       rvest::html_text()
#     return(tibble(link = link_, url = url_))
#   }
#
#   first_page <- scraplinks("http://targetedcontentmarketing.com/projects/")
#
#
#   pages <- first_page %>% filter(link %in% as.character(1:17)) %>% pull(url)
#
#   data <- data.frame(link = c(), url = c())
#
#   for(i in pages){
#     data <- rbind(data, scraplinks(i))
#   }
#
#   data <- data %>%
#     filter(stringr::str_detect(data$url, "portfolio"))
#
#   data$link <- stringr::str_replace(data$link, "DRTV", "") %>% stringr::str_replace(" / Featured / Video", "") %>% stringr::str_replace(" / Video", "")
#
#   video_links <- c()
#   name <- c()
#   url <- c()
#   for(i in seq(1, length(data$url))){
#     x <- xml2::read_html(data$url[[i]]) %>%
#       rvest::html_nodes(".wistia_embed") %>%
#       rvest::html_attr("src")
#
#     if(length(x) == 0){
#       x <- "NA"
#     }
#
#     video_links <- c(video_links, x)
#     print(x)
#     name <- c(name, data$link[[i]])
#     print(data$link[[i]])
#     url <- c(url, data$url[[i]])
#     print(data$url[[i]])
#   }
#
#   tcm_video_links <- data.frame(name = name, url = url, video_links = video_links)
#   tcm_video_links$unique_identifier <- as.character(unique_identifier[[1]])
#
#   tcm_video_links$text_for_formr <- paste0(
#     "You have been selected to watch [",
#     tcm_video_links$name,
#     "](",
#     tcm_video_links$video_links,
#     "). Please use '",
#     tcm_video_links$unique_identifier,
#     "_' followed by your initials in the unique identifier section."
#   )
#
#   tcm_video_links$name_for_formr <- paste0("video_", 1:146)
#   tcm_video_links$condition_for_formr <- paste0("random_for_video == ", 1:146)
#
#
#   readr::write_csv(tcm_video_links, "C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research and Development/Content Evaluation Measure/TCM Videos/tcm_video_links.csv")
#
# }
