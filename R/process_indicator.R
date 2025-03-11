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
#' @param areas
#' @param areaID
#'
#' @returns
#' @importFrom dplyr case_when select rename mutate
#' @importFrom purrr map map_dbl
#' @importFrom sf st_as_sf st_join
#' @importFrom stats lm
#'
#' @export
#'
#' @examples
process_indicator <- function(data, indicator_var_name = NA, indicator, type = NA, units = NA, scoring = NA, project = NA, climate = FALSE, design_target = FALSE, crs = 4326, latitude = "latitude", longitude = "longitude", year = "year",other_nest_variables = NA, areas = NA, areaID = "NAME_E"){
  if(!all(is.na(data))){
    if(!year %in% names(data)){
      stop("year column not found")
    }

    # check if data is an sf object
    if (!inherits(data, "sf")) {
      if(!latitude %in% names(data)){
        stop("latitude column not found")
      }
      if(!longitude %in% names(data)){
        stop("longitude column not found")
      }
      # convert to sf object and join with areas
      data <- st_as_sf(data,
                       coords = c(longitude, latitude),
                       crs = crs)|>
        st_join(select(areas,{{areaID}})) |>
        rename(areaID = {{areaID}})
    } else {
      # join with areas
      data <- data |>
        st_join(select(areas,{{areaID}})) |>
        rename(areaID = {{areaID}})
    }



    # identify the columns to nest
    nest_cols <- c(year,
                   indicator_var_name,
                   attr(data, "sf_column"),
                   other_nest_variables)


    nesteddata <- data.frame(data,
                             indicator = indicator,
                             type = type,
                             units = units,
                             scoring = scoring,
                             project = project,
                             climate = climate,
                             design_target = design_target) |>
      nest(data = nest_cols[!is.na(nest_cols)])

    # score the data
    if(startsWith(scoring,"desired state:")) {
      nesteddata <- nesteddata |>
        mutate(model = map(data, ~lm(as.formula(paste0(indicator_var_name,"~",year)), data = .x)),
               summaries = map(model,summary),
               coeffs = map(summaries,coefficients),
               slope_year = map_dbl(coeffs,~.x[2,1]),
               p = map_dbl(summaries,~.x$coefficients[2,4]),
               score = case_when(
                 endsWith(scoring, "increase") & p < 0.05 & slope_year > 0 ~ 100,
                 endsWith(scoring, "increase") & p < 0.05 & slope_year < 0 ~ 0,
                 endsWith(scoring, "increase") & p >= 0.05 ~ 50,
                 endsWith(scoring, "decrease") & p < 0.05 & slope_year < 0 ~ 100,
                 endsWith(scoring, "decrease") & p < 0.05 & slope_year > 0 ~ 0,
                 endsWith(scoring, "decrease") & p >= 0.05 ~ 50,
                 endsWith(scoring, "stable") & p < 0.05 ~ 0,
                 endsWith(scoring, "stable") & p >= 0.05 ~ 100,
                 .default = NA
               )) |>
        select(-model,-summaries,-coeffs,-slope_year,-p)

      # TODO: confer with Jaimie on how best to generate these statements (e.g.):

      # The most recent year (2023) shows a mean of 3.99 Calanus_finmarchicus_biomass (log_10) (sd=(only one measurement available)). The most recent 5 year mean was 3.88 Calanus_finmarchicus_biomass (log_10) (sd=0.15). No outside comparison was available.
      #
      #
      # A linear regression has shown a decrease of -0.01 Calanus_finmarchicus_biomass  (log_10) over the last 24 years (p=0). The linear trend for the last 5 years was a decrease of 0 Calanus_finmarchicus_biomass  (log_10). There is no outside comparison available.


    } else {
      warning("scoring method not supported")

    }



    nesteddata


  } else {
    # NA data case
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

