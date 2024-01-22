# currentwd <- getwd()
# setwd("C:/Users/Brian/TCM Dropbox/Brian Kissell/04 MDM Neuro-Fundraising Lab/Research and Development/00 Jobs/2024/003_RD_CodingVideoContent")
#
# prepared_video_data <- FULL_qualitative_coding_data(
# man_wd = "C:/Users/Brian/TCM Dropbox/Brian Kissell/04 MDM Neuro-Fundraising Lab/Research and Development/00 Jobs/2024/003_RD_CodingVideoContent"
# )
#
#
#
#
#
# setwd(currentwd)
#

# Create a temporary file path that can be used to test function
temp_csv_path <- tempfile(fileext = ".csv")

temp_csv_path_with_time <- bkissell::combine_file_string_with_time(file_string = temp_csv_path)




stringr::str_length(temp_csv_path_with_time[[1]])

test_that("number throws abort error for `arg`", {
  bkissell::abort_bad_argument(arg = 6, must = "be character", not = 6) %>%
    testthat::expect_error()
})

test_that("`arg` does not throw abort error", {
  testthat::expect_no_error(
    if(!is.character("arg")){
      bkissell::abort_bad_argument(arg = "arg", must = "be character", not = arg)
    }
  )
})
