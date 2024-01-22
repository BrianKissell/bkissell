#
# ###############################################################################
# # Create Workbook Styles --------------------------------------------------
# create_overall_header_style <- function(){
#   createStyle(
#     fontSize = 12,
#     border = "TopBottomLeftRight",
#     borderColour = "black",
#     textDecoration = "bold",
#     halign = "center",
#     valign = "center",
#     fgFill = "#BFBFBF",
#     wrapText = TRUE
#   )
# }
#
# create_individual_header_style <- function() {
#   createStyle(
#     fontSize = 10,
#     border = "TopBottomLeftRight",
#     borderColour = "black",
#     textDecoration = "bold",
#     halign = "center",
#     valign = "center",
#     fgFill = "#d9d9d9",
#     wrapText = TRUE
#   )
# }
#
# create_individual_values_style <- function() {
#   createStyle(
#     fontSize = 10,
#     border = "TopBottomLeftRight",
#     borderColour = "black",
#     halign = "center",
#     valign = "center",
#     fgFill = "white"
#   )
# }
#
# create_individual_values_style_left <- function() {
#   createStyle(
#     fontSize = 10,
#     border = "TopBottomLeftRight",
#     borderColour = "black",
#     halign = "left",
#     valign = "center",
#     fgFill = "white"
#   )
# }
#
# create_blank_values_style <- function() {
#   blank_values_style <- createStyle(
#     fontSize = 10,
#     border = "TopBottom",
#     borderColour = "black",
#     halign = "center",
#     valign = "center",
#     fgFill = "white"
#   )
# }
#
# create_empty_style <- function() {
#   blank_values_style <- createStyle(
#     fontSize = 10,
#     border = "TopBottom",
#     borderColour = "black",
#     halign = "center",
#     valign = "center",
#     fgFill = "black",
#     bgFill = "black"
#   )
# }
#
# ###############################################################################
# # Add Content to Excel Workbook -------------------------------------------
#
# #Create the header in the workbook
# add_overall_header <- function(wb, start_row = 2, start_col = 2) {
#
#   top_column_names_cost_report <- list("Production Cost Report")
#
#   writeData(
#     wb,
#     sheet = 1,
#     x = top_column_names_cost_report,
#     startRow = start_row,
#     startCol = start_col,
#     colNames = TRUE,
#     rowNames = FALSE
#   )
#
#   header_style <- create_overall_header_style()
#
#   cols_to_add <- start_col - 1
#
#   n_columns <- length(top_column_names_cost_report)
#
#   end_col <- as.numeric(n_columns) + as.numeric(cols_to_add)
#
#   vector_of_columns_to_style <- seq(start_col, end_col)
#
#   addStyle(
#     wb,
#     sheet = 1,
#     style = header_style,
#     rows = start_row,
#     cols = vector_of_columns_to_style,
#     gridExpand = TRUE,
#     stack = TRUE
#   )
#
#   return(wb)
# }
#
# # Add a df to the workbook
# add_df_to_workbook <- function(wb, df, start_row, start_col = 2) {
#
#   individual_header_style <- create_individual_header_style()
#   individual_values_style <- create_individual_values_style()
#   individual_values_style_left <- create_individual_values_style_left()
#   blank_values_style <- create_blank_values_style()
#
#   column_names <- colnames(df)
#
#   correct_column_names <- ifelse(
#     !stringr::str_detect(column_names, "^X\\_NA"),
#     column_names,
#     "") %>%
#     stringr::str_replace_all("_", " ") %>%
#     stringr::str_to_title() %>%
#     as.list()
#
#   writeData(
#     wb,
#     sheet = 1,
#     x = df,
#     colNames = TRUE,
#     rowNames = FALSE,
#     startRow = start_row,
#     startCol = start_col
#   )
#
#   writeData(
#     wb,
#     sheet = 1,
#     x = correct_column_names,
#     colNames = TRUE,
#     rowNames = FALSE,
#     startRow = start_row,
#     startCol = start_col
#   )
#
#   cols_to_add <- start_col - 1
#
#   n_columns <- length(column_names)
#
#   end_col <- as.numeric(n_columns) + as.numeric(cols_to_add)
#
#   vector_of_columns_to_style <- seq(start_col, end_col)
#
#   addStyle(
#     wb,
#     sheet = 1,
#     style = individual_header_style,
#     rows = start_row,
#     cols = vector_of_columns_to_style,
#     gridExpand = FALSE,
#     stack = FALSE
#   )
#
#   values_start_row <- start_row + 1
#
#   rows_for_values_style <- values_start_row:(values_start_row + nrow(df) - 1)
#
#   addStyle(
#     wb,
#     sheet = 1,
#     style = individual_values_style,
#     rows = rows_for_values_style,
#     cols = vector_of_columns_to_style,
#     gridExpand = TRUE,
#     stack = TRUE
#   )
#
#   end_row <- values_start_row + nrow(df) - 1
#
#   rows_for_values_style <- seq(values_start_row, end_row)
#
#   addStyle(
#     wb,
#     sheet = 1,
#     style = individual_values_style_left,
#     rows = rows_for_values_style,
#     cols = start_col,
#     gridExpand = TRUE,
#     stack = TRUE
#   )
#
#   blank_start_row <- values_start_row + nrow(df)
#
#   blank_values <- list(
#     "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
#   )
#
#   writeData(
#     wb,
#     sheet = 1,
#     x = blank_values,
#     colNames = TRUE,
#     rowNames = FALSE,
#     startRow = blank_start_row,
#     startCol = start_col
#   )
#
#   addStyle(
#     wb,
#     sheet = 1,
#     style = blank_values_style,
#     rows = blank_start_row,
#     cols = vector_of_columns_to_style,
#     gridExpand = TRUE,
#     stack = TRUE
#   )
#
#   return(wb)
# }
#
# # Add totals to the workbook
# add_total_project_costs <- function(wb, df, start_row, start_col = 2) {
#
#   individual_header_style <- create_individual_header_style()
#   individual_values_style <- create_individual_values_style()
#
#   writeData(
#     wb,
#     sheet = 1,
#     x = df,
#     colNames = TRUE,
#     rowNames = FALSE,
#     startRow = start_row,
#     startCol = start_col
#   )
#
#   column_names <- colnames(df)
#
#   # correct_column_names <- ifelse(
#   #   !stringr::str_detect(column_names, "^X\\.NA"),
#   #   column_names,
#   #   "") %>%
#   #   as.list()
#
#   correct_column_names <- ifelse(
#     !stringr::str_detect(column_names, "^X\\_NA"),
#     column_names,
#     "") %>%
#     stringr::str_replace_all("_", " ") %>%
#     stringr::str_to_title() %>%
#     as.list()
#
#   writeData(
#     wb,
#     sheet = 1,
#     x = correct_column_names,
#     colNames = TRUE,
#     rowNames = FALSE,
#     startRow = start_row,
#     startCol = start_col
#   )
#
#   cols_to_add <- start_col - 1
#
#   n_columns <- length(column_names)
#
#   end_col <- as.numeric(n_columns) + as.numeric(cols_to_add)
#
#   vector_of_columns_to_style <- seq(start_col, end_col)
#
#   addStyle(
#     wb,
#     sheet = 1,
#     style = individual_header_style,
#     rows = start_row,
#     cols = vector_of_columns_to_style,
#     gridExpand = TRUE,
#     stack = TRUE
#   )
#
#   values_start_row <- start_row + 1
#
#   end_row <- values_start_row + nrow(df) - 1
#
#   rows_for_values_style <- seq(values_start_row, end_row)
#
#   addStyle(
#     wb,
#     sheet = 1,
#     style = individual_values_style,
#     rows = rows_for_values_style,
#     cols = vector_of_columns_to_style,
#     gridExpand = TRUE,
#     stack = TRUE
#   )
#
#   return(wb)
# }
#
# # Create the full workbook
# create_excel_workbook <- function(
#     input_client_name,
#     input_moore_production_staff,
#     input_production_freelance,
#     input_travel_lodge_food,
#     input_location_rental,
#     input_cast,
#     input_equipment,
#     input_post_production,
#     save_location = "dev/cost_report_demo.xlsx"
# ) {
#
#   empty_style <- create_empty_style()
#   # input_client_name <- add_summary_row(input_client_name)
#   input_moore_production_staff <- add_summary_row(input_moore_production_staff)
#   input_production_freelance <- add_summary_row(input_production_freelance)
#   input_travel_lodge_food <- add_summary_row(input_travel_lodge_food)
#   input_location_rental <- add_summary_row(input_location_rental)
#   input_cast <- add_summary_row(input_cast)
#   input_equipment <- add_summary_row(input_equipment)
#   input_post_production <- add_summary_row(input_post_production)
#
#   input_total_project_costs <- create_DF_summary_table(
#     input_client_name = input_client_name,
#     input_moore_production_staff = input_moore_production_staff,
#     input_production_freelance = input_production_freelance,
#     input_travel_lodge_food = input_travel_lodge_food,
#     input_location_rental = input_location_rental,
#     input_cast = input_cast,
#     input_equipment = input_equipment,
#     input_post_production = input_post_production,
#     Total_Fee_projected = 85000,
#     COG_percent_Goal = .35
#   )
#
#   wb <- createWorkbook()
#
#   addWorksheet(wb, sheetName = "Cost Report", gridLines = FALSE)
#
#   modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Verdana")
#
#   n_columns_for_cost_report_sheet <- 18
#
#   n_columns_for_cost_report_sheet_cond <- n_columns_for_cost_report_sheet + 1
#
#   vector_cond_cols <- seq(1, n_columns_for_cost_report_sheet_cond)
#
#   setColWidths(
#     wb,
#     sheet = 1,
#     cols = 1:n_columns_for_cost_report_sheet,
#     widths = c(4, 38, 10, 16, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 4, 38, 12, 12),
#     ignoreMergedCells = FALSE,
#     hidden = c(rep(FALSE, 10), rep(TRUE, 4),rep(FALSE, 4))
#   )
#
#   start_row_client_name = 3
#   start_row_moore_production = start_row_client_name + nrow(input_client_name) + 2
#   start_row_production_freelance = start_row_moore_production + nrow(input_moore_production_staff) + 2
#   start_row_travel_lodge_food = start_row_production_freelance + nrow(input_production_freelance) + 2
#   start_row_location_rental = start_row_travel_lodge_food + nrow(input_travel_lodge_food) + 2
#   start_row_cast = start_row_location_rental + nrow(input_location_rental) + 2
#   start_row_equipment = start_row_cast + nrow(input_cast) + 2
#   start_row_post_production = start_row_equipment + nrow(input_equipment) + 2
#   start_row_total_project_costs = start_row_post_production + nrow(input_post_production) + 2
#   end_row_total = start_row_total_project_costs + nrow(input_total_project_costs) + 2
#   vector_cond_rows <- seq(1, end_row_total)
#   start_column_total_project_costs = ncol(start_row_client_name)-2
#
#   row_for_total_client_name <- start_row_client_name + 1
#   row_for_total_moore_production = start_row_moore_production + nrow(input_moore_production_staff) + 2
#   row_for_total_production_freelance = start_row_production_freelance + nrow(input_production_freelance) + 2
#   row_for_total_travel_lodge_food = start_row_travel_lodge_food + nrow(input_travel_lodge_food) + 2
#   row_for_total_location_rental = start_row_location_rental + nrow(input_location_rental) + 2
#   row_for_total_cast = start_row_cast + nrow(input_cast) + 2
#   row_for_total_equipment = start_row_equipment + nrow(input_equipment) + 2
#   row_for_total_post_production = start_row_post_production + nrow(input_post_production) + 2
#
#   forumula_for_COG_dollar_Est <- glue::glue("= {row_for_total_client_name} + {row_for_total_moore_production} + {row_for_total_production_freelance} + {row_for_total_travel_lodge_food} + {row_for_total_location_rental} + {row_for_total_cast} + {row_for_total_equipment} + {row_for_total_post_production}")
#
#   rows_for_each_of_the_totals <- c(
#     row_for_total_client_name,
#     row_for_total_moore_production,
#     row_for_total_production_freelance,
#     row_for_total_travel_lodge_food,
#     row_for_total_location_rental,
#     row_for_total_cast,
#     row_for_total_equipment,
#     row_for_total_post_production)
#
#   # rows_for_each_of_the_totals <- c(1,3,4,8,9,20)
#   start_col <- 2
#
#   excel_columns_for_totals <- get_excel_columns_from_df_column(
#     df = input_production_freelance,
#     column_name = "Total_Costs",
#     start_col = start_col
#   )
#
#   excel_var_loc_for_totals <- paste0(excel_columns_for_totals, rows_for_each_of_the_totals)
#
#   print(forumula_for_COG_dollar_Est)
#
#   #   input_moore_production_staff <- add_formulas_for_moore_production_staff(input_moore_production_staff, start_row_moore_production, 2)
#
#   input_production_freelance <- add_formulas_for_freelance_df(
#     df = input_production_freelance,
#     start_row = start_row_production_freelance,
#     start_col = start_col
#   )
#
#   input_moore_production_staff <- add_formulas_for_moore_production_staff(
#     df = input_moore_production_staff,
#     start_row = start_row_moore_production,
#     start_col = start_col
#   )
#
#   input_cast <- add_formulas_for_cast(
#     df = input_cast,
#     start_row = start_row_cast,
#     start_col = start_col
#   )
#
#   input_travel_lodge_food <- add_formulas_for_travel_lodge_food(
#     df = input_travel_lodge_food,
#     start_row = start_row_travel_lodge_food,
#     start_col = start_col
#   )
#
#   input_post_production <- add_formulas_for_post_production(
#     df = input_post_production,
#     start_row = start_row_post_production,
#     start_col = start_col
#   )
#
#   input_location_rental <- add_formulas_for_location_rental(
#     df = input_location_rental,
#     start_row = start_row_location_rental,
#     start_col = start_col
#   )
#
#   input_equipment <- add_formulas_for_equipment(
#     df = input_equipment,
#     start_row = start_row_equipment,
#     start_col = start_col
#   )
#   #
#   #   input_total_project_costs <- add_formulas_for_summary_COG(
#   #     df = input_total_project_costs,
#   #     excel_var_loc_for_totals = excel_var_loc_for_totals,
#   #     start_row = start_row_total_project_costs,
#   #     start_col = start_col
#   #     )
#
#   #
#   # create_formula_for_df <- function(
#     #   df,
#   #   var_vector,
#   #   formula_to_enter,
#   #   start_row,
#   #   start_col) {
#   #
#   #   # Obtain the location for the total
#   #   var_loc_list <- purrr::map(var_vector, ~ {
#   #     get_excel_var_loc(
#   #       df = df,
#   #       column_name = .x,
#   #       start_row = start_row,
#   #       start_col = start_col
#   #     )}) %>% purrr::set_names(var_vector)
#   #
#   #   # Enter a custom formula
#   #   formula <- glue::glue(formula_to_enter)
#   #
#   #   # Classify the formula
#   #   class(formula) <- c(class(formula), "formula")
#   #
#   #   # Return the formula
#   #   return(formula)
#   # }
#
#   readr::write_csv(input_client_name, "dev/data/df_input_client_name.csv")
#   readr::write_csv(input_moore_production_staff, "dev/data/df_input_moore_production_staff.csv")
#   readr::write_csv(input_production_freelance, "dev/data/df_input_production_freelance.csv")
#   readr::write_csv(input_travel_lodge_food, "dev/data/df_input_travel_lodge_food.csv")
#   readr::write_csv(input_location_rental, "dev/data/df_input_location_rental.csv")
#   readr::write_csv(input_cast, "dev/data/df_input_cast.csv")
#   readr::write_csv(input_equipment, "dev/data/df_input_equipment.csv")
#   readr::write_csv(input_post_production, "dev/data/df_input_post_production.csv")
#   readr::write_csv(input_total_project_costs, "dev/data/df_input_total_project_costs.csv")
#
#
#   wb <- add_overall_header(wb)
#   wb <- add_df_to_workbook(wb, df = input_client_name, start_row = 3)
#   wb <- add_df_to_workbook(wb, df = input_moore_production_staff, start_row = start_row_moore_production)
#   wb <- add_df_to_workbook(wb, df = input_production_freelance, start_row = start_row_production_freelance)
#   wb <- add_df_to_workbook(wb, df = input_travel_lodge_food, start_row = start_row_travel_lodge_food)
#   wb <- add_df_to_workbook(wb, df = input_location_rental, start_row = start_row_location_rental)
#   wb <- add_df_to_workbook(wb, df = input_cast, start_row = start_row_cast)
#   wb <- add_df_to_workbook(wb, df = input_equipment, start_row = start_row_equipment)
#   wb <- add_df_to_workbook(wb, df = input_post_production, start_row = start_row_post_production)
#   wb <- add_total_project_costs(wb, input_total_project_costs, start_row = start_row_total_project_costs)
#
#   # print(vector_cond_cols)
#   # print(vector_cond_rows)
#   print("BRIAN HERE IT IS !!!!!")
#   print(input_total_project_costs)
#   print("END")
#   openxlsx::conditionalFormatting(
#     wb = wb,
#     sheet = "Cost Report",
#     cols = vector_cond_cols,
#     rows = vector_cond_rows,
#     rule = "=OR(ISBLANK(A1), A1 = \"\")",
#     style = empty_style,
#     type = "expression"
#   )
#
#   print(getwd())
#
#   saveWorkbook(wb, save_location, overwrite = TRUE)
#
#   return(wb)
# }
#
#
#
#
# ###############################################################################
