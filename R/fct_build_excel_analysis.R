#'
#' #' create_frequency_table
#' #'
#' #' @param data data
#' #' @param var1 var1
#' #' @param var2_string var2_string
#' #'
#' #' @return table
#' #' @export
#' #'
#' create_frequency_table <- function(data, var1, var2_string) {
#'   table <- data %>%
#'     count(.data[[var1]], .data[[var2_string]], .drop = FALSE) %>%
#'     tidyr::pivot_wider(names_from = .data[[var2_string]], values_from = n) %>%
#'     dplyr::mutate(Total = rowSums(across(where(is.numeric)), na.rm=TRUE)) %>%
#'     ungroup()
#'   total <- table %>%
#'     dplyr::summarize(across(where(is.numeric),  sum), {{var1}} := "Total") %>%
#'     dplyr::select({{var1}}, everything())
#'   table <- rbind(table, total)
#'   return(table)
#' }
#'
#'
#' #' create_percentage_table
#' #'
#' #' @param data data
#' #' @param var1 var1
#' #' @param var2_string var2_string
#' #'
#' #' @return table
#' #' @export
#' #'
#' create_percentage_table <- function(data, var1, var2_string) {
#'   table <- data %>%
#'     count(.data[[var1]], .data[[var2_string]], .drop = FALSE) %>%
#'     group_by(.data[[var1]]) %>%
#'     mutate('%' = round(n / sum(n), 2)) %>%
#'     select(-n) %>%
#'     tidyr::pivot_wider(names_from = .data[[var2_string]], values_from = '%') %>%
#'     ungroup()
#'   total <- table %>%
#'     dplyr::summarize(across(where(is.numeric),  mean), {{var1}} := "Total") %>%
#'     dplyr::select({{var1}}, everything())
#'   table <- rbind(table, total)
#'   return(table)
#' }
#'
#' #' create_numeric_table
#' #'
#' #' @param data data
#' #' @param var1 var1
#' #' @param var2_string var2_string
#' #'
#' #' @return table
#' #' @export
#' #'
#' create_numeric_table <- function(data, var1, var2_string) {
#'   table <- data %>%
#'     # count(.data[[var1]], .data[[var2_string]], .drop = FALSE) %>%
#'     group_by(.data[[var1]]) %>%
#'     summarise('Mean' = mean(.data[[var2_string]], na.rm = TRUE),
#'               'SD' = sd(.data[[var2_string]], na.rm = TRUE),
#'               'N' = n(),
#'               'SE' = SD/sqrt(N),
#'               'ci_limit' = SE * 1.96)
#'   total <- data %>%
#'     dplyr::summarize(
#'       'Mean' = mean(.data[[var2_string]], na.rm = TRUE),
#'       'SD' = sd(.data[[var2_string]], na.rm = TRUE),
#'       'N' = n(),
#'       'SE' = SD/sqrt(N),
#'       'ci_limit' = SE * 1.96,
#'       {{var1}} := "Total") %>%
#'     dplyr::select({{var1}}, everything())
#'   table <- rbind(table, total)
#'   return(table)
#' }
#'
#' #' headStyle
#' #'
#' #' @return headStyle
#' #' @export
#' #'
#' analysisHeadStyle <- function(){
#'   headStyle <- openxlsx::createStyle(
#'     border = "TopBottomLeftRight",
#'     borderColour = "#3c3f43",
#'     fontName = "Cambria",
#'     halign = "center",
#'     valign = "center",
#'     bgFill = "#0070C0",
#'     fgFill = "#0070C0",
#'     fontColour = "#FFFFFF")
#'   return(headStyle)
#' }
#'
#' #' Body Style
#' #'
#' #' @return bodyStyle
#' #' @export
#' #'
#' analysisBodyStyle <- function(){
#'   bodyStyle <- openxlsx::createStyle(
#'     border = "TopBottomLeftRight",
#'     borderColour = "#3c3f43",
#'     fontName = "Cambria",
#'     halign = "center",
#'     valign = "center")
#'   return(bodyStyle)
#' }
#'
#' #' analysisHeadStyleInput
#' #'
#' #' @return headStyleInput
#' #' @export
#' #'
#' analysisHeadStyleInput <- function(){
#'   headStyleInput <- openxlsx::createStyle(
#'     border = "TopBottomLeftRight",
#'     borderColour = "#3c3f43",
#'     fontName = "Cambria",
#'     halign = "center",
#'     valign = "center",
#'     bgFill = "#ed7d31",
#'     fgFill = "#ed7d31")
#'   return(headStyleInput)
#' }
#'
#' #' obtain_location_from_table_list
#' #'
#' #' @param table_list table_list
#' #' @param initial_value initial_value
#' #' @param spacing_size spacing_size
#' #' @param direction direction
#' #'
#' #' @return location_values_for_list_of_tables
#' #' @export
#' #'
#' obtain_location_from_table_list <- function(table_list, initial_value = 2, spacing_size = 1, direction = "horizontal"){
#'
#'   if(direction == "horizontal"){
#'     value_list <- purrr::map(table_list, ~{ncol(.x)})
#'   } else if(direction == "vertical") {
#'     value_list <- purrr::map(table_list, ~{nrow(.x)})
#'   }
#'
#'   cum_values <- cumsum(value_list)
#'
#'   n_to_add_to_values  <- c(0, cum_values [seq(1, length(cum_values) - 1)])
#'
#'   initial_added_to_values  <- n_to_add_to_values + initial_value
#'
#'   spacing_to_add <- (seq_along(table_list) - 1) * spacing_size
#'
#'   location_values_for_list_of_tables <- initial_added_to_values + spacing_to_add
#'
#'   return(location_values_for_list_of_tables)
#' }
#'
#' #' add_tables_to_analysis_wb
#' #'
#' #' @param wb wb
#' #' @param df df
#' #' @param demographic_variable_names demographic_variable_names
#' #' @param where_the_tables_should_start_in_excel_row where_the_tables_should_start_in_excel_row
#' #' @param where_the_tables_should_start_in_excel_col where_the_tables_should_start_in_excel_col
#' #' @param sheet_name sheet_name
#' #' @param group_by group_by
#' #'
#' #' @return wb
#' #' @export
#' #'
#' add_tables_to_analysis_wb <- function(
#'     wb,
#'     df,
#'     demographic_variable_names,
#'     where_the_tables_should_start_in_excel_row,
#'     where_the_tables_should_start_in_excel_col,
#'     sheet_name,
#'     group_by
#' ){
#'   headStyle <- analysisHeadStyle()
#'
#'   addWorksheet(wb, sheet_name)
#'
#'   counts_list <- purrr::map(demographic_variable_names, ~ {
#'     counts <- create_frequency_table(df, group_by, .x)
#'     counts$var_name <- .x
#'     counts
#'   })
#'
#'   percentages_list <- purrr::map(demographic_variable_names, ~ {
#'     percentages <- create_percentage_table(df, group_by, .x)
#'     percentages$var_name <- .x
#'     percentages
#'   })
#'
#'
#'   purrr::map_df(percentages_list[6:25], ~{
#'     .x %>% dplyr::filter(version == "Total")
#'   }) %>% clipr::write_clip()
#'
#'
#'   stats_list <- purrr::map(demographic_variable_names, ~ broom::glance(chisq.test(df[[group_by]], df[[.x]])))
#'
#'
#'   goodness_of_fit_list <- purrr::map(counts_list, ~ {
#'
#'     observed <- .x %>%
#'       dplyr::filter(.data[[{{group_by}}]] == "Total") %>%
#'       dplyr::select(-{{group_by}}, -var_name, -Total)
#'
#'     n_columns <- ncol(observed)
#'
#'     expected <- rep(1/n_columns, n_columns)
#'
#'     print(observed)
#'     print(expected)
#'
#'     broom::glance(chisq.test(x=observed, p=expected))
#'
#'   })
#'
#'
#'
#'
#'   count_table_location_row <- purrr::map(counts_list, ~{where_the_tables_should_start_in_excel_row})
#'
#'   percentage_table_location_row <- purrr::map(counts_list, ~{
#'     nrow(.x) + where_the_tables_should_start_in_excel_row + 2
#'   })
#'
#'   stat_table_location_row <- purrr::map(counts_list, ~{
#'     nrow(.x) + where_the_tables_should_start_in_excel_row + 2 +
#'       nrow(.x) + 2
#'   })
#'
#'   goodness_of_fit_location <- purrr::map(stat_table_location_row, ~{
#'     .x + 4
#'   })
#'
#'
#'
#'   table_location_cols <- obtain_location_from_table_list(counts_list, initial_value = 2, spacing_size = 1, direction = "horizontal")
#'
#'   for(i in seq_along(demographic_variable_names)){
#'
#'     writeData(
#'       wb,
#'       sheet = sheet_name,
#'       counts_list[[i]],
#'       startCol = table_location_cols[[i]],
#'       startRow = count_table_location_row[[i]],
#'       headerStyle = headStyle,
#'       borders = "all"
#'     )
#'
#'     writeData(
#'       wb,
#'       sheet = sheet_name,
#'       percentages_list[[i]],
#'       startCol = table_location_cols[[i]],
#'       startRow = percentage_table_location_row[[i]],
#'       headerStyle = headStyle,
#'       borders = "all")
#'
#'     writeData(
#'       wb,
#'       sheet = sheet_name,
#'       stats_list[[i]],
#'       startCol = table_location_cols[[i]],
#'       startRow = stat_table_location_row[[i]],
#'       headerStyle = headStyle,
#'       borders = "all")
#'
#'     writeData(
#'       wb,
#'       sheet = sheet_name,
#'       goodness_of_fit_list[[i]],
#'       startCol = table_location_cols[[i]],
#'       startRow = goodness_of_fit_location[[i]],
#'       headerStyle = headStyle,
#'       borders = "all")
#'   }
#'
#'   return(wb)
#' }
#'
#'
