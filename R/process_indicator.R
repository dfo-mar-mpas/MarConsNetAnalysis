#' Process ecological or environmental indicator data
#'
#' This function standardizes, scores, summarizes, and generates plots for
#' ecological or environmental indicator datasets for use in monitoring,
#' reporting, and visualization. It supports different data types (tabular,
#' spatial, raster) and scoring methods (e.g., desired trend, representation,
#' control-site comparison).
#'
#' @param data A data frame or `sf` object containing indicator data. Must include
#'   at minimum columns for year, value, and an identifier for spatial location
#'   (e.g., area ID, site, or coordinates).
#' @param areas An `sf` polygon object defining the areas of interest (e.g.,
#'   conservation areas, regions). Used for spatial joins and reporting.
#' @param areaID Character. The name of the column in `areas` and `data` used to
#'   match records to geographic areas.
#' @param indicator Character. Indicator name or short description.
#' @param rationale Character. Rationale for including this indicator (why it
#'   matters for conservation objectives).
#' @param units Character. Units of the indicator (e.g., "kg/ha", "% cover").
#' @param type Character. Type of indicator (e.g., "biological", "habitat",
#'   "climate").
#' @param scoring Character. Method used to score the indicator. Options include:
#'   \itemize{
#'     \item `"desired trend"` – Uses regression to score based on increase,
#'       decrease, or stability.
#'     \item `"representation"` – Scores based on % overlap of feature within
#'       protected areas.
#'     \item `"median"` – Uses median value of raster or continuous data, rescaled
#'       to 0–100.
#'     \item `"control site comparison"` – Compares trends inside vs. outside
#'       managed areas.
#'   }
#' @param direction Character. If `"inverse"`, scores are reversed (e.g., lower
#'   values are better).
#' @param climate_expectation Character. Expected effect of climate change on this
#'   indicator (e.g., `"increase"`, `"decrease"`, `"stable"`).
#' @param plot Logical. If `TRUE`, generates indicator-specific plots (time-series,
#'   violin plots, maps, etc.).
#'
#' @param objectives A character string specifying which conservation or
#'   management objectives the indicator informs. Please directly copy and paste
#'   the objective from the objectives.xlsx. (See examples)
#'
#' @param readiness a character argument that is either 'Ready', 'Readily Available',
#' 'Not currently collected', 'Conceptual', or 'Unknown'.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A data frame (tibble) with one row per area and indicator, containing:
#'   \itemize{
#'     \item \code{areaID}, \code{indicator}, \code{score}, \code{status},
#'       \code{trend}, \code{units}, \code{type}, \code{rationale}
#'     \item Plain-language \code{status_statement} and \code{trend_statement}
#'     \item If `plot = TRUE`, a list-column of ggplot objects
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates inputs and joins data with `areas`.
#'   \item Nests data by area and indicator.
#'   \item Applies the specified `scoring` method.
#'   \item Generates plain-language narrative statements about status and trend.
#'   \item Optionally produces plots for visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Example with a data frame of biomass values
#' results <- process_indicator(
#'   data = biomass_df,
#'   areas = mpa_polygons,
#'   areaID = "site_id",
#'   indicator = "Biomass",
#'   rationale = "Biomass reflects ecosystem productivity",
#'   units = "kg/ha",
#'   type = "biological",
#'   scoring = "desired trend",
#'   direction = "positive",
#'   climate_expectation = "increase",
#'   plot = TRUE,
#'   objectives=c("Minimize aquaculture escapes", "Conserve 30% of Land, Waters, and Seas")
#' )
#' }
#' @importFrom dplyr case_when select rename mutate
#' @importFrom purrr map map_dbl
#' @importFrom sf st_as_sf st_join st_transform st_difference
#' @importFrom stats lm
#' @importFrom tidyr nest
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_classic ylab geom_smooth
#' @importFrom rlang .data
#' @importFrom units set_units
#' @importFrom tibble as_tibble
#' @importFrom patchwork wrap_plots
#' @export

process_indicator <- function(data, indicator_var_name = NA, indicator, type = NA, units = NA, scoring = NA, direction = "normal",
                              PPTID = NA, source=NA, project_short_title = NA, climate = FALSE, design_target = FALSE, crs = 4326,
                              latitude = "latitude", longitude = "longitude", year = "year", other_nest_variables = NA, areas = NA,
                              areaID = "NAME_E", regionID = "region", plot_type = "time-series",bin_width = 5, plot_lm = TRUE, plot_lm_se = TRUE,
                              control_polygon=NA, climate_expectation=NA,indicator_rationale=NA,bin_rationale=NA, objectives=NA,
                              readiness="Ready"){

  if ("map-species" %in% plot_type) {
    if (all(is.na(other_nest_variables))) {
      stop("Must provide other_nest_variable named containing subclass and class when plot_type = 'map-species'")
    } else if (!('subclass' %in% other_nest_variables)) {
        stop("Must provide other_nest_variable named containing subclass and class when plot_type = 'map-species'")
      }

  }

  if(climate) {
    if(is.na(climate_expectation)) {
      stop("Must provide a climate_expectation argument for climate indicators.")
    }
  }

  if (!(readiness %in% c('Ready', 'Readily Available','Not currently collected','Conceptual', 'Unknown'))) {
    stop('readiness must be one of the following: Ready, Readily Available,Not currently collected, Conceptual, or Unknown.')
  }

  if(is.na(indicator_rationale)) {
    stop("Must provide a indicator_rationale argument")
  }


  if(is.na(bin_rationale)) {

    stop("Must provide a bin_rationale argument")
  }

  if (inherits(data, "stars")) {
    dataisna <- all(is.na(unclass(data[[1]])))
  } else {
    dataisna <- all(is.na(data))
  }

    if(!dataisna){

      nesteddata <- assess_indicator(data=data, scoring=scoring, direction=direction,
                                     areas=areas, year=year, indicator_var_name=indicator_var_name,
                                     areaID= areaID, other_nest_variables=other_nest_variables,
                                     type=type,units = units,
                                     PPTID =  PPTID,
                                     project_short_title = project_short_title,
                                     climate = climate,
                                     design_target = design_target,latitude=latitude,
                                     longitude=longitude, crs=crs,indicator=indicator, control_polygon=control_polygon, regionID=regionID)

      final <- dplyr::select(as.data.frame(areas),{{areaID}}) |>
      unique() |>
      left_join(nesteddata, by = setNames("areaID", areaID))|>
      rename(areaID = {{areaID}}) |>
      mutate(indicator = coalesce(indicator, !!indicator),
             type = coalesce(type, !!type),
             units = coalesce(units, !!units),
             scoring = coalesce(scoring, !!scoring),
             PPTID = coalesce(PPTID, !!PPTID),
             source = coalesce(source, !!source),
             climate_expectation = coalesce(climate_expectation, !!climate_expectation),
             indicator_rationale = coalesce(indicator_rationale, !!indicator_rationale),
             objectives=paste0(objectives, collapse=" ;;; "),
             bin_rationale = coalesce(bin_rationale, !!bin_rationale),
             project_short_title = coalesce(project_short_title, !!project_short_title),
             climate = coalesce(climate, !!climate),
             design_target = coalesce(design_target, !!design_target)) |>
      # plot!
      mutate(plot = pmap(list(data,indicator,units,areaID), function(data, indicator, units,id) plot_indicator(data=data,indicator=indicator,units=units,id=id, plot_type=plot_type, year=year, indicator_var_name=indicator_var_name, scoring=scoring, areaID=!!areaID, areas=areas, bin_width=bin_width)
      ))  |>
        mutate(readiness=readiness)

  } else {
    # NA data case
    final <- data.frame(
      areaID = as.vector(unique(dplyr::select(as.data.frame(areas),{{areaID}}))[,1]),
      data,
      plot = NA,
      source=source,
      climate_expectation=climate_expectation,
      indicator_rationale=indicator_rationale,
      objectives="TBD",
      bin_rationale=bin_rationale,
      indicator = indicator,
      type = type,
      units = units,
      scoring = scoring,
      PPTID =  PPTID,
      project_short_title = project_short_title,
      climate = climate,
      design_target = design_target,
      trend_statement = "TBD",
      status_statement = "TBD",
      readiness=readiness
    )
  }

  return(as_tibble(final))
}

