#' Process, Score, and Plot an Ecological Indicator
#'
#' Processes raw indicator data into a standardized, scored, and plot-ready
#' indicator object suitable for site- and region-level reporting. This function
#' validates metadata, applies scoring logic, nests data by area, generates plots,
#' and returns a tidy tibble with indicator summaries and visualizations.
#'
#' The function supports time-series, spatial, and species-mapping indicators,
#' integrates climate and design-target logic, and enforces required metadata
#' (e.g., rationale, theme, readiness).
#'
#' @param data A data frame or `stars` object containing indicator observations.
#'   May be `NA` to create a placeholder indicator record.
#' @param indicator_var_name Character. Name of the column in `data` containing
#'   the indicator values.
#' @param indicator Character. Human-readable indicator name.
#' @param type Character. Instrument type used to collect the data
#' (e.g. Ecosystem Trawl Survey).
#' @param units Character. Units of measurement.
#' @param scoring Character. Method used to score the indicator. Options include:
#'   \itemize{
#'     \item `"desired trend"` – Uses regression to score based on increase,
#'       decrease, or stability.
#'     \item `"representation"` – Scores based on number of features that
#'     intersect with a protected area.
#'     \item `"coverage"` – Scores based on % overlap of a feature within
#'       protected areas.
#'     \item `"median"` – Uses median value of raster or continuous data, rescaled
#'       to 0–100.
#'     \item `"control site comparison"` – Compares trends inside vs. outside
#'       managed areas.
#'   }
#' @param direction Character. Direction of desired change for scoring.
#'   Defaults to `"normal"`.
#' @param PPTID Character or numeric. Unique indicator or project identifier in
#' the project planning tool (if internal to DFO).
#' @param source Character. Data source description.
#' @param project_short_title Character. Short project title associated with
#'   the project planning tool (if internal to DFO).
#' @param climate Logical. Whether this is a climate-related indicator.
#' @param design_target Logical. Whether the indicator has a design target.
#' @param crs Integer. Coordinate reference system EPSG code. Defaults to 4326.
#' @param latitude Character. Name of latitude column in dataset.
#' @param longitude Character. Name of longitude column in dataset.
#' @param year Character. Name of year column in dataset.
#' @param other_nest_variables Character vector. Additional grouping variables
#'   required for nesting (e.g., `"subclass"` for species maps).
#' @param areas A spatial or tabular object defining assessment areas.
#' @param areaID Character. Name of the area identifier column in `areas`.
#' @param regionID Character. Name of the region identifier column in `areas`.
#' @param plot_type Character. Plot type(s) to generate
#'   (e.g., `"time-series"`, `"map-species"`).
#' @param bin_width Numeric. Bin width used for plotting or scoring.
#' @param plot_lm Logical. Whether to plot a linear trend.
#' @param plot_lm_se Logical. Whether to plot confidence intervals for trends.
#' @param control_polygon Optional spatial polygon used for control comparisons.
#' @param climate_expectation Character. Expected climate response
#'   (required if `climate = TRUE`).
#' @param indicator_rationale Character. Scientific rationale for the indicator
#'   (required).
#' @param bin_rationale Character. Rationale for binning or thresholds (required).
#' @param objectives Character vector. Conservation or management objectives
#'   associated with the indicator.
#' @param readiness Character. Indicator readiness category. Must be one of
#'   `"Ready"`, `"Readily Available"`, `"Not currently collected"`,
#'   `"Conceptual"`, or `"Unknown"`.
#' @param scale Character. Assessment scale (e.g., `"site"`, `"region-site"`).
#' @param theme a character string of length 1 of either "Ocean Conditions",
#' "Ocean Structure and Movement","Primary Production","Secondary Production",
#' "Marine Mammals and Other Top Predators", "Trophic Structure and Function",
#' "Benthic Environment", "Fish and Fishery Resources",
#' or "Anthropogenic Pressure and Impacts"
#' @param SME subject matter expert for the data/ indicator (e.g. John Doe)
#' @param indicator_assumptions indicator assumptions
#' @param indicator_caveats indicator caveats
#'
#' @details
#' The function performs extensive validation of metadata and arguments.
#' Climate indicators require a `climate_expectation`. Species mapping plots
#' require `other_nest_variables` including `"subclass"`.
#'
#' When valid data are provided, indicator values are assessed using
#' `assess_indicator()`, joined to the provided `areas`, and plotted using
#' `plot_indicator()`. When all data are `NA`, a placeholder indicator record
#' is returned with status fields set to `"TBD"`.
#'
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
#'
#'
#' @examples
#' \dontrun{
#' # Example indicator data
#' indicator_data <- data.frame(
#'   year = rep(2015:2020, times = 2),
#'   latitude = runif(12, 44, 46),
#'   longitude = runif(12, -62, -60),
#'   value = rnorm(12, mean = 10, sd = 2),
#'   area = rep(c("Site A", "Site B"), each = 6)
#' )
#'
#' # Example areas table
#' areas <- data.frame(
#'   NAME_E = c("Site A", "Site B"),
#'   region = c("Region 1", "Region 1")
#' )
#'
#' # Process a time-series indicator
#' result <- process_indicator(
#'   data = indicator_data,
#'   indicator_var_name = "value",
#'   indicator = "Mean Bottom Temperature",
#'   type = "Physical",
#'   units = "°C",
#'   scoring = "trend",
#'   direction = "increasing",
#'   PPTID = "PPT-001",
#'   source = "Example Monitoring Program",
#'   project_short_title = "Demo Project",
#'   climate = TRUE,
#'   climate_expectation = "Expected to increase under climate change",
#'   indicator_rationale = "Temperature influences species distribution and ecosystem function.",
#'   bin_rationale = "Bins reflect ecologically meaningful thresholds.",
#'   objectives = c("Track warming trends", "Support climate reporting"),
#'   areas = areas,
#'   areaID = "NAME_E",
#'   regionID = "region",
#'   plot_type = "time-series",
#'   readiness = "Ready",
#'   scale = "site",
#'   theme = "Ocean Conditions"
#' )
#'
#' result
#' }
#'
#' @return A tibble with one row per area containing:
#' \itemize{
#'   \item indicator metadata (name, type, units, theme, readiness)
#'   \item scoring outputs (score, trend, status, quality statements)
#'   \item nested data and plots
#' }
#'
#' @seealso
#' \code{\link{assess_indicator}},
#' \code{\link{plot_indicator}}
#'
#' @export


process_indicator <- function(data, indicator_var_name = NA, indicator, type = NA, units = NA, scoring = NA, direction = "normal",
                              PPTID = NA, source=NA, project_short_title = NA, climate = FALSE, design_target = FALSE, crs = 4326,
                              latitude = "latitude", longitude = "longitude", year = "year", other_nest_variables = NA, areas = NA,
                              areaID = "NAME_E", regionID = "region", plot_type = "time-series",bin_width = 5, plot_lm = TRUE, plot_lm_se = TRUE,
                              control_polygon=NA, climate_expectation=NA,indicator_rationale=NA,bin_rationale=NA, objectives=NA,
                              readiness="Ready", scale='site', theme = NA, SME=NA, indicator_assumptions=NA, indicator_caveats=NA){


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

  if (is.na(SME)) {
    stop("Must include a SME (Subject Matter Expert) argument")
  }

  if (!(readiness %in% c('Ready', 'Readily Available','Not currently collected','Conceptual', 'Unknown'))) {
    stop('readiness must be one of the following: Ready, Readily Available,Not currently collected, Conceptual, or Unknown.')
  }

  if(is.na(indicator_rationale)) {
    stop("Must provide a indicator_rationale argument")
  }

  if(is.na(project_short_title)) {
    stop("Must provide a project_short_title argument")
  }


  if(is.na(bin_rationale)) {
    stop("Must provide a bin_rationale argument")
  }
  valid_themes <- c(
    "Ocean Conditions",
    "Ocean Structure and Movement",
    "Primary Production",
    "Secondary Production",
    "Marine Mammals and Other Top Predators",
    "Trophic Structure and Function",
    "Benthic Environment",
    "Fish and Fishery Resources",
    "Anthropogenic Pressure and Impacts"
  )

  if (is.na(theme) || !theme %in% valid_themes) {
    stop(paste0("Must provide a theme argument of either", paste0(valid_themes, collapse=", ")))

  }
  if (!(length(theme) == 1)) {
    stop("Can only provide one theme.")
  }

  if (is.na(type)) {
    stop("Must include a type argument")
  }


  if (inherits(data, "stars")) {
    dataisna <- all(is.na(unclass(data[[1]])))
  } else {
    dataisna <- all(is.na(data))
  }

  if (startsWith(scoring,"coverage")){

    if (scale != "region-site") stop('scale must be set to "region-site" to use the "coverage" type scoring')

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




      # TEST
      # Creating outside data frame if there is data and control_polygon is not NA
      if (!(all(is.na(control_polygon)))) {
        control_nesteddata <- assess_indicator(data=data, scoring=scoring, direction=direction,
                                       areas=control_polygon[control_polygon$buffer_distance == 'forty_km',], year=year, indicator_var_name=indicator_var_name,
                                       areaID= areaID, other_nest_variables=other_nest_variables,
                                       type=type,units = units,
                                       PPTID =  PPTID,
                                       project_short_title = project_short_title,
                                       climate = climate,
                                       design_target = design_target,latitude=latitude,
                                       longitude=longitude, crs=crs,indicator=indicator, control_polygon=control_polygon, regionID=regionID)

      }

      # END TEST


      #regional scores for "region-site" are calculated in assess_indicator(),
      # so at this point scale can be sites to fill in the blanks
      if(scale == "region-site") scale <- "site"
      final <- dplyr::select(as.data.frame(areas),{{areaID}},{{regionID}}) |>
      unique() |>
      #full_join(nesteddata, by = c(setNames("areaID", areaID),setNames("region", regionID)))|>
      full_join(nesteddata, by = c(setNames("areaID", areaID)))|>
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
        mutate(readiness=readiness,
               scale=coalesce(scale, !!scale),
               theme=theme,
               SME=SME)

      # Adding outside data/score
      if (!(all(is.na(control_polygon)))) {
        # Pre-allocate
        final$adjacent_score <- NA_real_
        final$adjacent_data  <- vector("list", nrow(final))
        for (j in seq_len(nrow(control_nesteddata))) {

          # Find rows in final that match this areaID
          idx <- which(final$areaID == control_nesteddata$areaID[j])

          if (length(idx) > 0) {
            final$adjacent_score[idx] <- control_nesteddata$score[j]
            final$adjacent_data[idx]  <- control_nesteddata$data[j]
          }
        }
      } else {
        final$adjacent_data <- NA
        final$adjacent_score <- NA
      }

      if (any(names(final) == "region.y")) {
        names(final)[which(names(final) == "region.x")] <- 'region'
      }


      ## Adding assumptions and caveats
      ## DATA assumptions:

      x <- inherit_from_dependencies(as_tibble(final), n = 2, attribute = TRUE)

      if (all(attr(x, 'assumptions') == "")) {
        data_assumptions <- paste0(attr(nesteddata, 'assumptions'), collapse="; ") # PULLING FROM DATA SOURCE that is fed into process_indicator
      } else {
        # Running in a target
        data_assumptions <- paste0(unique(x$assumptions), collapse=';')
      }

      if (all(attr(x, 'caveats') == "")) {
        data_caveats <- paste0(attr(nesteddata, 'assumptions'), collapse="; ")
      } else {
        # Running in a target
        data_caveats <- paste0(unique(x$caveats), collapse=';')
      }

      # Combining data and indicator assumptions / caveats

      if (!(is.na(indicator_assumptions))) {
        data_assumptions <- paste0(data_assumptions, " Indicator assumptions: ", indicator_assumptions)
      }

      if (!(is.na(indicator_caveats))) {
        data_caveats <- paste0(data_caveats, "Indicator caveats: ", indicator_caveats)
      }

      final$assumptions <- data_assumptions
      final$caveats <- data_caveats


  } else {
    # NA data case
    final <- data.frame(
      areaID = as.vector(unique(dplyr::select(as.data.frame(areas),{{areaID}}))[,1]),
      region=areas$region,
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
      quality_statement = "TBD",
      readiness=readiness,
      scale=scale,
      theme=theme,
      SME=SME,
      adjacent_data = NA,
      adjacent_score=NA
    )

    if (any(names(final) == "region.y")) {
      names(final)[which(names(final) == "region.x")] <- 'region'
    }
  }

  desired_order <- c(
    "areaID", "region", "indicator", "type", "units", "scoring",
    "PPTID", "project_short_title", "climate", "design_target", "data",
    "score", "status_statement", "trend_statement","quality_statement", "source", "climate_expectation",
    "indicator_rationale", "objectives", "bin_rationale", "plot", "readiness", "scale", "theme", "SME", 'assumptions', 'caveats', 'adjacent_data', 'adjacent_score'
  )

  final <- final[ , desired_order]

  return(as_tibble(final))
}

