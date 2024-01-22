#'
#' # Functions to allow assertion of Excel Formulas into a df ---------------------
#'
#' #' get_excel_rows_from_df
#' #' Indicate relevant excel rows for the df
#' #'
#' #' @param df dataframe
#' #' @param start_row the row in excel where this will start
#' #'
#' #' @return n_rows_seq
#' #' @export
#' #'
#' get_excel_rows_from_df <- function(
#'     df,
#'     start_row) {
#'   # Add 1 to account for the column names
#'   add_initial_row <- start_row + 1
#'
#'   # How many rows are in the df
#'   n_rows <- nrow(df)
#'
#'   # Create a vector with the rows for the excel doc
#'   n_rows_seq <- seq(add_initial_row, n_rows - 1 + add_initial_row, 1)
#'
#'   # Return the df
#'   return(n_rows_seq)
#' }
#'
#' #' get_excel_columns_from_df_column
#' #' Indicate the relevant excel column letters for a particular column
#' #'
#' #' @param df dataframe
#' #' @param column_name What column are the wanting the column location info for
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return location_letters_for_df
#' #' @export
#' #'
#' get_excel_columns_from_df_column <- function(df, column_name, start_col) {
#'
#'   # Subtract from 1
#'   add_initial_cols <- start_col - 1
#'
#'   # Get the number of rows
#'   n_rows <- nrow(df)
#'
#'   # Find the column of the df and add tne initial to it
#'   excel_letter_column_number <- which(colnames(df) == column_name) + add_initial_cols
#'
#'   # If this is null, cause an error
#'   if(identical(excel_letter_column_number, numeric(0))) {
#'     stop(glue::glue("Variable '{column_name}' was not found"))
#'   }
#'
#'   # What letter should it be
#'   excel_letter_column_letter <- LETTERS[excel_letter_column_number]
#'
#'   # Create the vector of letters for the column
#'   location_letters_for_df <- rep(excel_letter_column_letter, n_rows)
#'
#'   # Return the vector
#'   return(location_letters_for_df)
#' }
#'
#' #' get_excel_var_loc
#' #' Create the Excel Reference points for a column in a dataframe
#' #'
#' #' @param df dataframe
#' #' @param column_name What column are the wanting the column location info for
#' #' @param start_row the row in excel where this will start
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return excel_var_loc
#' #' @export
#' #'
#' get_excel_var_loc <- function(df, column_name, start_row, start_col) {
#'
#'   # Get the vector for the rows
#'   n_rows_seq <- get_excel_rows_from_df(df = df, start_row = start_row)
#'
#'   # Get the letters for the column
#'   excel_columns <- get_excel_columns_from_df_column(
#'     df = df,
#'     column_name = column_name,
#'     start_col = start_col
#'   )
#'
#'   # Combine these together as a location vector
#'   excel_var_loc <- paste0(excel_columns, n_rows_seq)
#'
#'   # Return the vector
#'   return(excel_var_loc)
#' }
#'
#' #' formula_add_sum_of_column
#' #' Add sum of the column (Get total fot the df)
#' #'
#' #' @param df dataframe
#' #' @param var_1_name Name of total column
#' #' @param start_row the row in excel where this will start
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return formula
#' #' @export
#' #'
#' formula_add_sum_of_column <- function(
#'     df = input_production_freelance_reactive_df(),
#'     var_1_name = "Total_Costs",
#'     start_row = 14,
#'     start_col = 2) {
#'
#'   # Obtain the location for the total
#'   excel_var_1_loc <- get_excel_var_loc(
#'     df = df,
#'     column_name = var_1_name,
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Create the location for the totals
#'   vector_of_total_letters <- paste0(excel_var_1_loc[-length(excel_var_1_loc)], collapse = ", ")
#'
#'   # Create the formula
#'   total_of_column <- paste0("= SUM(", vector_of_total_letters, ")")
#'
#'   # Add the formula to the data frame
#'   df$Total_Costs[length(excel_var_1_loc)] <- total_of_column
#'
#'   # Return the formulas
#'   formula <- df$Total_Costs
#'
#'   # Use class to mark this as a formula
#'   class(formula) <- c(class(formula), "formula")
#'
#'   # Return the formula
#'   return(formula)
#' }
#'
#' #' create_formula_for_df
#' #' Create formulas from df data
#' #'
#' #' @param df dataframe
#' #' @param var_vector Name of total column
#' #' @param formula_to_enter Formula
#' #' @param start_row the row in excel where this will start
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return formula
#' #' @export
#' #'
#' create_formula_for_df <- function(
#'     df,
#'     var_vector,
#'     formula_to_enter,
#'     start_row,
#'     start_col) {
#'
#'   # Obtain the location for the total
#'   var_loc_list <- purrr::map(var_vector, ~ {
#'     get_excel_var_loc(
#'       df = df,
#'       column_name = .x,
#'       start_row = start_row,
#'       start_col = start_col
#'     )}) %>% purrr::set_names(var_vector)
#'
#'   # Enter a custom formula
#'   formula <- glue::glue(formula_to_enter)
#'
#'   # Classify the formula
#'   class(formula) <- c(class(formula), "formula")
#'
#'   # Return the formula
#'   return(formula)
#' }
#'
#'
#' # Insert all formulas for Freelance ------------------------------------
#'
#' #' add_formulas_for_freelance_df
#' #' Create Function to add the formulas for the freelancers
#' #'
#' #' @param df dataframe
#' #' @param start_row the row in excel where this will start
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return df
#' #' @export
#' #'
#' add_formulas_for_freelance_df <- function(df, start_row, start_col) {
#'
#'   # Create the formula for the day rate
#'   df$total_day_rate <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("day_rate", "shoot_days"),
#'     formula_to_enter = "= {var_loc_list[['day_rate']]} * {var_loc_list[['shoot_days']]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Create the formula for the travel pay
#'   df$travel_pay <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("day_rate", "travel_days"),
#'     formula_to_enter = "= ({var_loc_list[['day_rate']]} / 2) * {var_loc_list[['travel_days']]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Create the formula for the total costs
#'   df$Total_Costs <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("total_day_rate", "travel_pay", "per_diem_pay", "kit_total_rate", "OT_pay"),
#'     formula_to_enter = "= SUM({var_loc_list[['total_day_rate']]}, {var_loc_list[['travel_pay']]}, {var_loc_list[['per_diem_pay']]}, {var_loc_list[['kit_total_rate']]}, {var_loc_list[['OT_pay']]})",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Create the formula for the sum
#'   df$Total_Costs <- formula_add_sum_of_column(
#'     df = df,
#'     var_1_name = "Total_Costs",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Create the formula for the per_diem_pay
#'   df$per_diem_pay <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("shoot_days", "travel_days"),
#'     formula_to_enter = "= IF({var_loc_list[['travel_days']]} > 0, ({var_loc_list[['shoot_days']]} + {var_loc_list[['travel_days']]}) * 75, 0)",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Create the formula for the overtime pay
#'   df$OT_pay <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("day_rate", "OT_hours"),
#'     formula_to_enter = "= (({var_loc_list[['day_rate']]} * 1.5) / 10) * {var_loc_list[['OT_hours']]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Convert all of the first 14 variables to NA
#'   df[nrow(df), 1:14] <- NA
#'
#'   # Return the df
#'   return(df)
#' }
#'
#' # Insert all formulas for moore staff ------------------------------------
#'
#' #' add_formulas_for_moore_production_staff
#' #' Create Function to add the formulas for the moore staff
#' #'
#' #' @param df dataframe
#' #' @param start_row the row in excel where this will start
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return df
#' #' @export
#' #'
#' add_formulas_for_moore_production_staff <- function(
#'     df = input_moore_production_staff_reactive_df(),
#'     start_row = 14,
#'     start_col = 2
#' ) {
#'
#'   # Add formula for total day rate
#'   df$total_day_rate <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("day_rate", "shoot_days"),
#'     formula_to_enter = "= {var_loc_list[['day_rate']]} * {var_loc_list[['shoot_days']]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for the travel pay
#'   df$travel_pay <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("day_rate", "travel_days"),
#'     formula_to_enter = "= ({var_loc_list[['day_rate']]} / 2) * {var_loc_list[['travel_days']]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for the total costs
#'   df$Total_Costs <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("total_day_rate", "travel_pay", "per_diem_pay", "kit_total_rate", "OT_pay"),
#'     formula_to_enter = "= SUM({var_loc_list[['total_day_rate']]}, {var_loc_list[['travel_pay']]}, {var_loc_list[['per_diem_pay']]}, {var_loc_list[['kit_total_rate']]}, {var_loc_list[['OT_pay']]})",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for the sum of total costs
#'   df$Total_Costs <- formula_add_sum_of_column(
#'     df = df,
#'     var_1_name = "Total_Costs",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for the over time
#'   df$OT_pay <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("day_rate", "OT_hours"),
#'     formula_to_enter = "= (({var_loc_list[['day_rate']]} * 1.5) / 10) * {var_loc_list[['OT_hours']]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Remove first 14 columns for the last row
#'   df[nrow(df), 1:14] <- NA
#'
#'   # Return df
#'   return(df)
#' }
#'
#'
#'
#' # Insert all formulas for Talent / Cast  ------------------------------------
#'
#' #' add_formulas_for_cast
#' #' Create Function to add the formulas for the cast
#' #'
#' #' @param df dataframe
#' #' @param start_row the row in excel where this will start
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return df
#' #' @export
#' #'
#' add_formulas_for_cast <- function(df, start_row, start_col) {
#'
#'   # Add formulas for the total cast rate
#'   df$total_cast_rate <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("shoot_days", "day_rate", "n_cast_to_hire"),
#'     formula_to_enter = "= {var_loc_list[[1]]} * {var_loc_list[[2]]} * {var_loc_list[[3]]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add the formulas for the total cost rates
#'   df$Total_Costs <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("total_cast_rate"),
#'     formula_to_enter = "= {var_loc_list[[1]]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add the formulas for the sum of the total cost column
#'   df$Total_Costs <- formula_add_sum_of_column(
#'     df = df,
#'     var_1_name = "Total_Costs",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Remove the first 14 columns for the last row
#'   df[nrow(df), 1:14] <- NA
#'
#'   # Return the df
#'   return(df)
#' }
#'
#' # Insert all formulas for travel_lodge_food  ------------------------------------
#'
#' #' add_formulas_for_travel_lodge_food
#' #'
#' #' @param df dataframe
#' #' @param start_row the row in excel where this will start
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return df
#' #' @export
#' #'
#' add_formulas_for_travel_lodge_food <- function(df, start_row, start_col) {
#'
#'   # Add formulas for total
#'   df$total <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("cost_per_person_per_day", "number_per_day", "applicable_days"),
#'     formula_to_enter = "= {var_loc_list[[1]]} * {var_loc_list[[2]]} * {var_loc_list[[3]]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for total cost
#'   df$Total_Costs <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("total"),
#'     formula_to_enter = "= {var_loc_list[[1]]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for the sum of total costs
#'   df$Total_Costs <- formula_add_sum_of_column(
#'     df = df,
#'     var_1_name = "Total_Costs",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Remove columns from the last row
#'   df[nrow(df), 1:14] <- NA
#'
#'   # Return the df
#'   return(df)
#' }
#'
#' # Insert all formulas for post_production ------------------------------------
#'
#' #' add_formulas_for_post_production
#' #' Create Function to add the formulas for post production
#' #'
#' #' @param df dataframe
#' #' @param start_row the row in excel where this will start
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return df
#' #' @export
#' #'
#' add_formulas_for_post_production <- function(df, start_row, start_col) {
#'
#'   # Add formulas for the total
#'   df$total <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("rate", "qty"),
#'     formula_to_enter = "= {var_loc_list[[1]]} * {var_loc_list[[2]]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for the total costs
#'   df$Total_Costs <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("total"),
#'     formula_to_enter = "= {var_loc_list[[1]]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for the sum of the total costs
#'   df$Total_Costs <- formula_add_sum_of_column(
#'     df = df,
#'     var_1_name = "Total_Costs",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Remove data from last row
#'   df[nrow(df), 1:14] <- NA
#'
#'   # Return df
#'   return(df)
#' }
#'
#' # Insert all formulas for location_rental ------------------------------------
#'
#' #' add_formulas_for_location_rental
#' #' Create Function to add the formulas for location rentals
#' #'
#' #' @param df dataframe
#' #' @param start_row the row in excel where this will start
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return df
#' #' @export
#' #'
#' add_formulas_for_location_rental <- function(df, start_row, start_col) {
#'
#'   # Add formulas for total
#'   df$total <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("avg_cost_day_ea", "amount", "day_quantity"),
#'     formula_to_enter = "= {var_loc_list[[1]]} * {var_loc_list[[2]]} * {var_loc_list[[3]]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for total costs
#'   df$Total_Costs <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("total"),
#'     formula_to_enter = "= {var_loc_list[[1]]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for sum of total costs
#'   df$Total_Costs <- formula_add_sum_of_column(
#'     df = df,
#'     var_1_name = "Total_Costs",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Remove some data from last row
#'   df[nrow(df), 1:14] <- NA
#'
#'   # Return df
#'   return(df)
#' }
#'
#' # Insert all formulas for equipment ------------------------------------
#'
#' #' add_formulas_for_equipment
#' #' Create Function to add the formulas for the equipment
#' #'
#' #' @param df dataframe
#' #' @param start_row the row in excel where this will start
#' #' @param start_col the column in excel where this will start
#' #'
#' #' @return df
#' #' @export
#' #'
#' add_formulas_for_equipment <- function(df, start_row, start_col) {
#'
#'   # Add formulas for the totals
#'   df$total <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("day_quantity", "amount", "avg_cost_day_ea"),
#'     formula_to_enter = "= {var_loc_list[[1]]} * {var_loc_list[[2]]} * {var_loc_list[[3]]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for total costs
#'   df$Total_Costs <- create_formula_for_df(
#'     df = df,
#'     var_vector = c("total"),
#'     formula_to_enter = "= {var_loc_list[[1]]}",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Add formulas for total costs
#'   df$Total_Costs <- formula_add_sum_of_column(
#'     df = df,
#'     var_1_name = "Total_Costs",
#'     start_row = start_row,
#'     start_col = start_col
#'   )
#'
#'   # Remove data from the last row
#'   df[nrow(df), 1:14] <- NA
#'
#'   # Return the df
#'   return(df)
#' }
#'
#' #
#' # add_formulas_dfs_with_one_formula <- function(
#'     #     df,
#' #     total_column_name,
#' #     var_vector,
#' #     formula_string,
#' #     start_row = start_row,
#' #     start_col = start_col) {
#' #   df <- df %>%
#' #     dplyr::mutate({{ total_column_name }} := create_formula_for_df(
#' #       df = df,
#' #       var_vector = var_vector,
#' #       formula_to_enter = formula_string,
#' #       start_row = start_row,
#' #       start_col = start_col
#' #     ))
#' #
#' #   df$Total_Costs <- create_formula_for_df(
#' #     df = df,
#' #     var_vector = {{ total_column_name }},
#' #     formula_to_enter = "= {var_loc_list[[{{total_column_name}}]]}",
#' #     start_row = start_row,
#' #     start_col = start_col
#' #   )
#' #
#' #   return(df)
#' # }
#'
#' #
#' # add_formulas_for_summary_COG <- function(df, excel_var_loc_for_totals, start_row, start_col) {
#' #
#' #   df$Overall_Estimates[df$Overall_Metrics == "COG_dollar_Est"] <- create_formula_for_df(
#' #     df = df,
#' #     var_vector = excel_var_loc_for_totals,
#' #     formula_to_enter = "= {var_loc_list[[1]]} + {var_loc_list[[2]]} + {var_loc_list[[3]]} + {var_loc_list[[4]]} + {var_loc_list[[5]]} + {var_loc_list[[6]]} + {var_loc_list[[7]]} + {var_loc_list[[8]]} + {var_loc_list[[9]]} + {var_loc_list[[10]]} + {var_loc_list[[11]]} + {var_loc_list[[12]]} + {var_loc_list[[13]]} + {var_loc_list[[14]]} + {var_loc_list[[15]]} + {var_loc_list[[16]]} + {var_loc_list[[17]]} + {var_loc_list[[18]]} + {var_loc_list[[19]]} + {var_loc_list[[20]]} + {var_loc_list[[21]]} + {var_loc_list[[22]]} + {var_loc_list[[23]]} + {var_loc_list[[24]]}",
#' #     start_row = start_row,
#' #     start_col = start_col
#' #   )
#' #
#' #   # Return the df
#' #   return(df)
#' # }
