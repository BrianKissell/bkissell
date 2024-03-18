# #https://www.youtube.com/watch?v=GnpJujF9dBw&t=825s
#
# # Install Java - https://www.azul.com/downloads/
# # Select and downloadJava 8 (LTS) 64 bit JDK
#
# # Create folder - "C:/Program Files/Azul Java"
# # Extract the zip file from download to Azul Java folder
# # Once it is extracted, rename it as  "jdk
# # Open that folder, open bin, then copy the path of the location (example - C:\Program Files\Azul Java\jdk\bin)
#
# # Open your computer settings and type in "environment" and click on "Edit the system environment variables" then on "Environment Variables".
# # Then click on path at the bottom (system variables) and click on edit
# # On the final box, add the path of the location to the jave file.
# # Next click on "New" and name it JAVA_HOME and give the value that same path, but remove the bin portion of it.
#
# library(RSelenium)
# library(wdman)
# library(netstat)
#
# selenium()
#
# selenium_object <- selenium(retcommand = T, check = F)
# # Install R
# # Install RStudio
# # Install RTools
# # Install Selenium driver https://www.geeksforgeeks.org/how-to-set-up-rselenium-for-r/
# # Install Java Software Development Kit (JDK) - https://www.oracle.com/java/technologies/downloads/#jdk19-windows
#
#
#
# # https://rdrr.io/cran/RSelenium/f/vignettes/basics.Rmd
#
# install.packages("openxlsx")
# install.packages("tidyverse")
# install.packages("shiny")
# install.packages("RSelenium")
# install.packages("gganimate")
# install.packages("ggthemes")
# install.packages("AMR")
# install.packages("snakecase")
# install.packages("devtools")
# install.packages("plyr")
#
#
# #Others packages
# install.packages("usethis")
# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
#
#
#
# library(devtools)
# use_devtools()
#
#       # # Use in .Rprofile
#       # if (interactive()) {
#       #   suppressMessages(require(devtools))
#       #   suppressMessages(require(testthat))
#       # }
#       #
#       # suppressMessages(require(dplyr))
#       #
#       # options(
#       #   usethis.full_name = "Brian Kissell",
#       #   usethis.description = list(
#       #     `Authors@R` = 'person("Brian", "Kissell", email = "BKissell@mnflab.com", role = "aut"',
#       #     License = "MIT + file LICENSE",
#       #     Version = "0.0.0.9000"
#       #   )
#       # )
#
# devtools::dev_sitrep()
#
# # Set which files to ignore
# usethis::use_build_ignore()
#
#
#
# devtools::document("C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research Tools/000_Lab_Tools/bkrselenium")
# devtools::install("C:/Users/Brian/Dropbox (TCM Creative)/04 MDM Neuro-Fundraising Lab/Research Tools/000_Lab_Tools/bkrselenium")
#
#
#
#
#
# #### Set Up R Selenium
#
#
# https://www.docker.com/products/docker-desktop/
#
#
#   ## Download
#   # docker pull selenium/standalone-firefox:2.53.0
#   # Run
#   docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#
# https://learn.microsoft.com/en-us/archive/blogs/canitpro/step-by-step-enabling-hyper-v-for-use-on-windows-10
#
# https://grishagin.com/r/rselenium/2017/11/11/setup-rselenium-windows10.html
