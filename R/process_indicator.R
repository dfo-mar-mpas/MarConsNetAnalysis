#' Title
#'
#' @param data a data frame or sf data frame that contains the indicator data, the year, and either the latitude and longitude (i.e. data frame) or the geometry (i.e. sf data frame). Other optional columns (e.g. depth) can also be included by naming them in the other_nest_variables parameter.
#' @param indicator_var_name character string of the name of the column in the data that contains the indicator data
#' @param indicator character string of the name of the indicator
#' @param type character string of the instrument/platform/model type of the indicator
#' @param PPTID numeric project planning tool project ID
#' @param project_short_title character string of a short title for the project
#' @param units  character string of the units of the indicator
#' @param scoring character string of the scoring method for the indicator
#' @param climate logical indicating whether the indicator is a climate indicator
#' @param design_target logical indicating whether the indicator is a design target
#' @param crs coordinate reference system of the data
#' @param latitude character string of the name of the column in the data that contains the latitude
#' @param longitude character string of the name of the column in the data that contains the longitude
#' @param year character string of the name of the column in the data that contains the year
#' @param other_nest_variables character vector of the names of other columns in the data that should be nested
#' @param areas an sf data frame that contains the areas to which the data should be joined (i.e. MPAs)
#' @param areaID character string of the name of the column in the areas data that contains the area ID (i.e. NAME_E by default)
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
process_indicator <- function(data, indicator_var_name = NA, indicator, type = NA, units = NA, scoring = NA, PPTID = NA, project_short_title = NA, climate = FALSE, design_target = FALSE, crs = 4326, latitude = "latitude", longitude = "longitude", year = "year",other_nest_variables = NA, areas = NA, areaID = "NAME_E"){
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
                             PPTID =  PPTID,
                             project_short_title = project_short_title,
                             climate = climate,
                             design_target = design_target) |>
      nest(data = nest_cols[!is.na(nest_cols)])

    # score the data
    if(startsWith(scoring,"desired state:")) {
      nesteddata <- nesteddata |>
        mutate(model = map(data, ~lm(as.formula(paste0(indicator_var_name,"~",year)), data = .x)),
               summaries = map(model,summary),
               coeffs = map(summaries,coefficients),
               slope_year = map_dbl(coeffs,~{
                 # Check if coeffs has at least 2 rows
                 if(nrow(.x) >= 2) {
                   return(.x[2,1])
                 } else {
                   # Return NA or some other default value
                   return(NA_real_)
                 }}),
               p = map_dbl(summaries, ~{
                 # Check if coefficients has at least 2 rows
                 if(is.null(.x$coefficients) || nrow(.x$coefficients) < 2) {
                   return(NA_real_)
                 } else {
                   return(.x$coefficients[2,4])
                 }
               }),
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
               ),
               status_statement = "TBD",
               trend_statement = "TBD") |>
        select(-model,-summaries,-coeffs,-slope_year,-p)

      # TODO: confer with Jaimie on how best to generate these statements (e.g.):

      # The most recent year (2023) shows a mean of 3.99 Calanus_finmarchicus_biomass (log_10) (sd=(only one measurement available)). The most recent 5 year mean was 3.88 Calanus_finmarchicus_biomass (log_10) (sd=0.15). No outside comparison was available.
      #
      #
      # A linear regression has shown a decrease of -0.01 Calanus_finmarchicus_biomass  (log_10) over the last 24 years (p=0). The linear trend for the last 5 years was a decrease of 0 Calanus_finmarchicus_biomass  (log_10). There is no outside comparison available.


    } else {
      warning("scoring method not supported")

    }



    # make sure this data has a row for each site
    select(as.data.frame(areas),{{areaID}}) |>
      unique() |>
      left_join(nesteddata, by = setNames("areaID", areaID))|>
      rename(areaID = {{areaID}})


  } else {
    # NA data case
    data.frame(data,
               indicator = indicator,
               type = type,
               units = units,
               scoring = scoring,
               PPTID =  PPTID,
               project_short_title = project_short_title,
               climate = climate,
               design_target = design_target,
               areaID = unique(select(as.data.frame(areas),{{areaID}})))
  }
}

