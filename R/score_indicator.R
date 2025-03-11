#' Title
#'
#' @param data
#' @param indicator_var_name
#' @param indicator
#' @param type
#' @param units
#' @param scoring
#' @param project
#' @param climate
#' @param design_target
#' @param crs
#' @param latitude
#' @param longitude
#' @param year
#' @param other_nest_variables
#'
#' @returns
#' @export
#'
#' @examples
score_indicator <- function(data, indicator_var_name = NA, indicator, type = NA, units = NA, scoring = NA, project = NA, climate = FALSE, design_target = FALSE, crs = 4326, latitude = "latitude", longitude = "longitude", year = "year",other_nest_variables = NA){
  if(!all(is.na(data))){
    if(!year %in% names(data)){
      stop("year column not found")
    }

    if (!inherits(data, "sf")) {
      if(!latitude %in% names(data)){
        stop("latitude column not found")
      }
      if(!longitude %in% names(data)){
        stop("longitude column not found")
      }
      data <- st_as_sf(data,
                       coords = c(longitude, latitude),
                       crs = crs)
    }

    nest_cols <- c(year,
                   indicator_var_name,
                   attr(data, "sf_column"),
                   other_nest_variables)


      data.frame(data,
                 indicator = indicator,
                 type = type,
                 units = units,
                 scoring = scoring,
                 project = project,
                 climate = climate,
                 design_target = design_target) |>
        nest(data = nest_cols[!is.na(nest_cols)])
  } else {
    data.frame(data,
               indicator = indicator,
               type = type,
               units = units,
               scoring = scoring,
               project = project,
               climate = climate,
               design_target = design_target)
  }
}

