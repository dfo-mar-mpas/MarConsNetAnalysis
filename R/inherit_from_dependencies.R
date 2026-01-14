#' Inherit Assumptions and Caveats from Dependencies
#'
#' Automatically inherits assumptions and caveats from all data frame objects
#' in the parent environment (typically data frames used to create the current one).
#' This function searches the calling environment for data frames and consolidates
#' their assumptions and caveats, prefixing each with the source object name.
#'
#' @param df A data frame to which inherited assumptions and caveats will be added.
#' @param n Integer specifying how many frames to go back in the call stack.
#'   Default is 1 (immediate parent frame, i.e. the target). Increase to search further up the
#'   call stack.
#' @param attribute Logical indicating how to attach the inherited metadata.
#'   If TRUE (default), assumptions and caveats are stored as attributes.
#'   If FALSE, they are added as regular columns to the data frame.
#'
#' @returns If `attribute = TRUE`, returns the input data frame with "assumptions"
#'   and "caveats" attributes containing semicolon-separated strings of all
#'   inherited metadata. If `attribute = FALSE`, returns the data frame with
#'   two new columns: "assumptions" and "caveats".
#'
#' @export
#'
#' @examples
#' # Create source data frames with assumptions
#' sales_data <- data.frame(revenue = c(100, 200, 300))
#' sales_data <- add_assumptions(
#'   sales_data,
#'   assumptions = "Q1 data only",
#'   caveats = "Excludes returns"
#' )
#'
#' costs_data <- data.frame(cost = c(50, 75, 100))
#' costs_data <- add_assumptions(
#'   costs_data,
#'   assumptions = "Fixed costs excluded"
#' )
#'
#' # Create a derived data frame and inherit metadata
#' profit_data <- data.frame(
#'   profit = sales_data$revenue - costs_data$cost
#' )
#' profit_data <- inherit_from_dependencies(profit_data)
#'
#' # View inherited assumptions
#' attr(profit_data, "assumptions")
#' # "Inherited from 'sales_data': Q1 data only;Inherited from 'costs_data': Fixed costs excluded"
#'
#' # Alternative: add as columns instead of attributes
#' profit_data2 <- inherit_from_dependencies(profit_data, attribute = FALSE)
#' profit_data2$assumptions
#'
inherit_from_dependencies <- function(df, n = 1, attribute = TRUE) {
  dep_names <- ls(envir = parent.frame(n))
  all_assumptions <- character(0)
  all_caveats <- character(0)
  for (dep_name in dep_names) {
    dep_obj <- get(dep_name, envir = parent.frame(n))
    if (inherits(dep_obj, "data.frame")) {
      if (length(attr(dep_obj, "assumptions"))>1){
        all_assumptions <- c(all_assumptions, paste0("Inherited from '", dep_name, "': ", attr(dep_obj, "assumptions")))
      }

      if (length(attr(dep_obj, "caveats"))>1){
        all_caveats <- c(all_caveats, paste0("Inherited from '", dep_name, "': ", attr(dep_obj, "caveats")))
      }


    }
  }
  if (attribute) {
    # Set as attributes
    attr(df, "assumptions") <- paste(unique(all_assumptions), collapse = ";")
    attr(df, "caveats") <- paste(unique(all_caveats), collapse = ";")
    df
  } else {
    # Return df with attributes
    df |>
      mutate(assumptions = paste(unique(all_assumptions), collapse = ";"),
             caveats = paste(unique(all_caveats), collapse = ";"))
  }
}
