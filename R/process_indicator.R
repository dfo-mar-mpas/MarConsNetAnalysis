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
    if (startsWith(scoring,"desired state:")){

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
          st_join(dplyr::select(areas,{{areaID}})) |>
          rename(areaID = {{areaID}})



      } else {
        # join with areas
        data <- data |>
          st_join(dplyr::select(areas,{{areaID}})) |>
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
        )

      # TEST

      status_statement <- list()
      trend_statement <- list()
#browser()
      for (i in seq_along(nesteddata$data)) {
        DATA <- nesteddata$data[[i]]
        if (!(is.null(DATA))) {
          DATA_5_YEARS <- nesteddata$data[[i]][which(nesteddata$data[[i]]$year %in% tail(sort(as.numeric(
            unique(DATA$year)
          )), 5)),]

          # STATUS

          status_statement[[i]] <- paste0(
            "The most recent year, ", tail(sort(as.numeric(DATA$year)), 1),
            ", shows a mean of ", round(mean(DATA[[indicator_var_name]], na.rm = TRUE), 2),
            if (!is.na(units)) paste0(" (", units, ")") else "",
            " (sd = ", round(sd(DATA[[indicator_var_name]], na.rm = TRUE), 2), "). ",
            "The most recent 5 years of sampling (", paste0(tail(sort(as.numeric(unique(DATA$year))), 5), collapse = ","),
            ") showed a mean of ", round(mean(DATA_5_YEARS[[indicator_var_name]], na.rm = TRUE), 2),
            if (!is.na(units)) paste0(" (", units, ")") else "",
            " (sd = ", round(sd(DATA_5_YEARS[[indicator_var_name]], na.rm = TRUE), 2), ")"
          )


          # TREND

          if (length(unique(data$year[which(!(is.na(data[[indicator_var_name]])))])) > 1) {
          data_5_year <- data[which(data$year %in% tail(sort(as.numeric(data$year)),5)),]
          if (length(unique(data_5_year$year)) > 1) { # Can perform linear regression on 5 year
          nesteddata_5_year <- data.frame(data_5_year,
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
          nesteddata_5_year <- nesteddata_5_year |>
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
            )



          trend_direction_5_years <- ifelse(unname(nesteddata_5_year$model[[i]][1]$coefficients[2]) > 0, "increase", "decrease")
          five_year_trend_value <- unname(nesteddata_5_year$model[[i]][1]$coefficients[2])
          } # more than 1 year condition
          current_trend_direction <- ifelse(unname(nesteddata$model[[i]][1]$coefficients[2]) > 0, "increase", "decrease")
          current_trend_value <- unname(nesteddata$model[[i]][1]$coefficients[2])



          if (length(unique(data_5_year$year)) > 1) { # Condition what type of statement to print out.
            trend_statement[[i]] <- paste0(
              "A linear regression has shown a ", current_trend_direction, " of ", round(current_trend_value, 2),
              if (!is.na(units) && units != "") paste0(" (", units, ")") else "",
              ", over ", length(unique(DATA$year)), " years (pval = ", round(nesteddata$p[i], 2), "). The linear trend for the last 5 years sampled (",
              paste0(tail(sort(unique(DATA$year)), 5), collapse = ","),
              "), showed a ", trend_direction_5_years, " of ", round(five_year_trend_value, 2), " ", indicator_var_name,
              if (!is.na(units) && units != "") paste0(" (", units, ")") else "",
              " (pval = ", round(nesteddata_5_year$p[i], 2), ")"
            )

          }  else {
            trend_statement[[i]] <- paste0(
              "A linear regression has shown a ", current_trend_direction, " of ", round(current_trend_value, 2),
              if (!is.na(units) && units != "") paste0(" (", units, ")") else "",
              ", over ", length(unique(DATA$year)), " years (pval = ", round(nesteddata$p[i], 2), "). ",
              "There is only one year of data sampled in the last 5 years, and therefore a linear regression is not possible"
            )

            }# condition (statement)
        } else {
          trend_statement[[i]] <- "There is only one year of data, and therefore a linear regression is not possible"

        } # condition here

        } else {
          status_statement[[i]] <- "TBD"
        }

      }

      nesteddata <- nesteddata |>
        dplyr::select(-model,-summaries,-coeffs,-slope_year,-p)

      nesteddata$status_statement <- unlist(status_statement)
      nesteddata$trend_statement <- unlist(trend_statement)
    } else if (startsWith(scoring,"representation")){
      #areas <- areas[-which(areas$NAME_E == "Non_Conservation_Area"),]

      if (!inherits(data, "sf")) stop("data must be an sf object for 'representation' scoring")
      if (!(indicator_var_name %in% names(data))) stop("indicator_var_name column not found in data")
      if (st_crs(data) != st_crs(areas)) stop("data and areas must have the same CRS")
      if (endsWith(scoring, "regional thresholds") & !(regionID %in% names(areas))) stop(paste0("Scoring methods that use 'regional thresholds' require a '",regionID,"' (set by the regionID argument) in your 'areas' argument"))



      # identify the columns to nest
      nest_cols <- c(indicator_var_name,
                     "geoms",
                     other_nest_variables)

      if (all(endsWith(as.character(st_geometry_type(data)),"POLYGON"))){
        # if data contains only polygons, use the area based statuses

        mpaareas <- st_area(areas[[attr(areas, "sf_column")]]) |>
          set_units("km^2") |>
          as.numeric()
        layerareas <- st_area(data[[attr(data, "sf_column")]]) |>
          set_units("km^2") |>
          as.numeric()

        nesteddata <- data |>
          # intersection with areas
          st_intersection(areas) |>
          rename(IDtemp = {{areaID}})|>
          dplyr::select(c("IDtemp",nest_cols[!is.na(nest_cols)])) |>
          filter(!is.na({{indicator_var_name}})) |>
          rowwise() |>
          mutate(layerareakm2=st_area(geoms) |>
                   set_units("km^2") |>
                   as.numeric() |>
                   round(1),
                 layerpercentmpa=layerareakm2/mpaareas[as.data.frame(areas)[areaID]==IDtemp]*100,
                 layerpercentlayer=layerareakm2/layerareas[as.data.frame(data)[indicator_var_name]==.data[[indicator_var_name]]]*100,
          ) |>
          rename(areaID = IDtemp)|>
          ungroup() |>
          nest(rawdata=nest_cols[!is.na(nest_cols)],
               layerpercents=c({{indicator_var_name}},layerareakm2 ,layerpercentmpa,layerpercentlayer)) |>
          rowwise() |>
          # calculate score based on the proportion of non-NA values
          mutate(score=nrow(rawdata)/nrow(data)*100,
                 indicator = indicator,
                 type = type,
                 units = units,
                 scoring = scoring,
                 PPTID =  PPTID,
                 project_short_title = project_short_title,
                 climate = climate,
                 design_target = design_target,
                 status_statement = paste0(areaID,
                                           pmap_chr(layerpercents,
                                                    function(layerareakm2,
                                                             layerpercentmpa,
                                                             layerpercentlayer,
                                                             ...){
                                                      paste0(" is ",
                                                             round(layerpercentmpa,1),
                                                             "% covered by ",
                                                             list(...)[[indicator_var_name]],
                                                             " which represents ",
                                                             round(layerpercentlayer,1),
                                                             "% of that feature")}) |>
                                             paste(collapse = ", and"),
                                           "."),
                 trend_statement = "There is no temporal dimension in this data.")|>
          rename(data = rawdata) |>
          dplyr::select(-layerpercents) |>
          right_join(areas |>
                       as.data.frame() |>
                       dplyr::select({{areaID}}) |>
                       unique() |>
                       rename(areaID = {{areaID}}),
                     by = "areaID") |>
          mutate(score = if_else(is.na(score),0,score),
                 indicator = coalesce(indicator, !!indicator),
                 type = coalesce(type, !!type),
                 units = coalesce(units, !!units),
                 scoring = coalesce(scoring, !!scoring),
                 PPTID = coalesce(PPTID, !!PPTID),
                 project_short_title = coalesce(project_short_title, !!project_short_title),
                 climate = coalesce(climate, !!climate),
                 design_target = coalesce(design_target, !!design_target),
                 status_statement = if_else(is.na(status_statement),
                                            "No features are represented.",
                                            status_statement),
                 trend_statement = "There is no temporal dimension in this data.")
      } else {
        totaloccurrences <- data |>
          mutate(occurrences = case_when(
            st_geometry_type(geoms) == "POINT" ~ 1,
            st_geometry_type(geoms) == "MULTIPOINT" ~ lengths(st_geometry(geoms))/2,
            TRUE ~ NA_integer_
          ))

        nesteddata <- data |>
          # intersection with areas
          st_intersection(areas) |>
          rename(areaID = {{areaID}}) |>
          dplyr::select(c("areaID",nest_cols[!is.na(nest_cols)])) |>
          filter(!is.na({{indicator_var_name}})) |>
          rowwise() |>
          mutate(occurrences = case_when(
            st_geometry_type(geoms) == "POINT" ~ 1,
            st_geometry_type(geoms) == "MULTIPOINT" ~ lengths(st_geometry(geoms))/2,
            TRUE ~ NA_integer_),
            total_occurrences = totaloccurrences$occurrences[totaloccurrences[[indicator_var_name]]==.data[[indicator_var_name]]]) |>
          nest(rawdata=nest_cols[!is.na(nest_cols)],
               layeroccurrences=c({{indicator_var_name}},total_occurrences ,occurrences)) |>
          rowwise() |>
          # calculate score based on the proportion of non-NA values
          mutate(score=nrow(rawdata)/nrow(data)*100,
                 indicator = indicator,
                 type = type,
                 units = units,
                 scoring = scoring,
                 PPTID =  PPTID,
                 project_short_title = project_short_title,
                 climate = climate,
                 design_target = design_target,
                 status_statement = paste0(areaID,
                                           " has had recorded occurrences of ",
                                           nrow(rawdata),
                                           " taxa which represents ",
                                           round(nrow(rawdata)/nrow(data)*100,1),
                                           "% of taxa recorded in this dataset"),
                 trend_statement = "There is no temporal dimension in this data.")|>
          rename(data = rawdata) |>
          dplyr::select(-layeroccurrences) |>
          right_join(areas |>
                       as.data.frame() |>
                       dplyr::select({{areaID}}) |>
                       unique() |>
                       rename(areaID = {{areaID}}),
                     by = "areaID") |>
          mutate(score = if_else(is.na(score),0,score),
                 indicator = coalesce(indicator, !!indicator),
                 type = coalesce(type, !!type),
                 units = coalesce(units, !!units),
                 scoring = coalesce(scoring, !!scoring),
                 PPTID = coalesce(PPTID, !!PPTID),
                 project_short_title = coalesce(project_short_title, !!project_short_title),
                 climate = coalesce(climate, !!climate),
                 design_target = coalesce(design_target, !!design_target),
                 status_statement = if_else(is.na(status_statement),
                                            "No features are represented.",
                                            status_statement),
                 trend_statement = "There is no temporal dimension in this data.")
      }



      if(endsWith(scoring, "regional thresholds")){

        nesteddata <- nesteddata |>
          left_join(areas |>
                      as.data.frame() |>
                      dplyr::select({{areaID}}, {{regionID}}) |>
                      unique() |>
                      rename(areaID = {{areaID}}),
                    by = "areaID") |>
          group_by(across(all_of(regionID))) |>
          mutate(score = case_when(grepl("site-maximum",scoring) ~ score/max(score)*100,
                                   grepl("cumulative distribution",scoring) ~ cume_dist(score)*100,
                                   .default = NA)) |>
                   ungroup() |>
          dplyr::select(-{{regionID}})
      }

    } else if (startsWith(scoring,"median")){

      # check if data is an stars object
      if (!inherits(data, "stars")) {
        stop("data must be an stars object for 'median' scoring")
      }
      # identify the columns to nest
      nest_cols <- c(indicator_var_name,
                     "geometry",
                     other_nest_variables)


      nesteddata <- st_as_sf(data, as_points = TRUE) |>
        st_join(areas, left = FALSE)|>
        rename(areaID = {{areaID}}) |>
        dplyr::select(c("areaID",nest_cols[!is.na(nest_cols)])) |>
        filter(!is.na({{indicator_var_name}})) |>
        nest(rawdata=nest_cols[!is.na(nest_cols)]) |>
        mutate(median = map_dbl(rawdata,~median(.x[[indicator_var_name]],na.rm=TRUE)),
               nrowdata = map_dbl(rawdata,~nrow(.x)),
               score = round(median/max(data[[indicator_var_name]], na.rm = TRUE)*100),
               indicator = indicator,
               type = type,
               units = units,
               scoring = scoring,
               PPTID =  PPTID,
               project_short_title = project_short_title,
               climate = climate,
               design_target = design_target,
               status_statement = paste0(areaID,
                                         " has median value of ",
                                         round(median,2),
                                         " (n = ",
                                         nrowdata,
                                         ") and ranges from ",
                                         map_dbl(rawdata,~round(min(.x[[indicator_var_name]],na.rm=TRUE),2)),
                                         " to ",
                                         map_dbl(rawdata,~round(max(.x[[indicator_var_name]],na.rm=TRUE),2)),
                                         "."),
               trend_statement = "There is no temporal dimension in this data.")|>
        rename(data = rawdata) |>
        dplyr::select(-median,-nrowdata)



    } else if (startsWith(scoring, "control site linear trend")) {
      if (any(areas$NAME_E == "Non_Conservation_Area")) {
        unioncontrolpolygon <- st_union(control_polygon) |>
          st_make_valid()
        areas2 <- areas
        areas2$geoms[areas$NAME_E == "Non_Conservation_Area"] <- areas$geoms[areas$NAME_E == "Non_Conservation_Area"] |>
          st_difference(unioncontrolpolygon) |>
          st_combine()
      }
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
                         crs = crs)

      }
        # join with areas

      buffers_sorted <- c("twenty_km", "forty_km", "sixty_km", "eighty_km")

        data <- data |>
          st_join(dplyr::select(areas,{{areaID}})) |>
          rename(site_areaID = {{areaID}}) |>
          st_join(dplyr::select(control_polygon, buffer_distance,  {{areaID}})) |>
          rename(control_areaID = {{areaID}}) |>
          group_by(control_areaID) |>
          mutate(
            buffer_order = match(buffer_distance, buffers_sorted)
          ) |>
          mutate(
            # For each buffer, check if any smaller buffer has 5+ years
            needs_this_buffer = purrr::map_lgl(buffer_order, function(current_order) {
              if(is.na(current_order)) return(FALSE)
              if(current_order == 1) return(TRUE)  # Always keep smallest buffer
              # Check cumulative years in each smaller buffer level
              max_years_in_smaller <- max(purrr::map_dbl(1:(current_order - 1), function(smaller_order) {
                dplyr::n_distinct(year[buffer_order <= smaller_order])
              }))

              max_years_in_smaller < 5
            }),
            max_buffer_used = buffers_sorted[max(buffer_order[needs_this_buffer],na.rm=TRUE)]
          ) |>
          ungroup() |>
          mutate(
            areaID=if_else(is.na(site_areaID) | site_areaID == "Non_Conservation_Area", control_areaID, site_areaID),
            control = case_when(
              is.na(site_areaID) & is.na(control_areaID) ~ NA,
              !is.na(site_areaID) & is.na(control_areaID) ~ FALSE,
              (is.na(site_areaID) | site_areaID == "Non_Conservation_Area") &
                !is.na(control_areaID) & needs_this_buffer ~ TRUE,
              .default = FALSE
            )
          ) |>
          dplyr::select(-site_areaID, -control_areaID, -buffer_order, -needs_this_buffer, -buffer_distance)

      nest_cols <- c(year,
                     indicator_var_name,
                     attr(data, "sf_column"),
                     other_nest_variables,
                     "control",
                     "max_buffer_used")
      nesteddata <- data.frame(data|>
                                 filter(!is.na(control)) |>
                                 filter(!is.na(.data[[indicator_var_name]])) |>
                                 group_by(areaID) |>
                                 #mutate(garbage = case_when(all(control)~TRUE,
                                 #                           all(!control)~TRUE,
                                  #                          .default = FALSE)) |>
                                 #filter(!garbage) |>
                                 #dplyr::select(-garbage) |>
                                 ungroup(),
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
      nesteddata <- nesteddata |>
        mutate(model = map(data, ~lm(as.formula(paste0(indicator_var_name,"~",year, "+ control")), data = .x)),
               summaries = map(model,summary),
               coeffs = map(summaries,coefficients),
               controlTRUE = map_dbl(coeffs,~{
                 # Check if coeffs has at least 2 rows
                 if(nrow(.x) >= 3) {
                   return(.x[3,1])
                 } else {
                   # Return NA or some other default value
                   return(NA_real_)
                 }}),
               p = map_dbl(summaries, ~{
                 # Check if coefficients has at least 2 rows
                 if(is.null(.x$coefficients) || nrow(.x$coefficients) < 3) {
                   return(NA_real_)
                 } else {
                   return(.x$coefficients[3,4])
                 }
               }),
               score = case_when(
                 endsWith(scoring, "less inside") & p < 0.05 & controlTRUE > 0 ~ 100,
                 endsWith(scoring, "less inside") & p < 0.05 & controlTRUE < 0 ~ 0,
                 endsWith(scoring, "less inside") & p >= 0.05 ~ 50,

                 endsWith(scoring, "more inside") & p < 0.05 & controlTRUE < 0 ~ 100,
                 endsWith(scoring, "more inside") & p < 0.05 & controlTRUE > 0 ~ 0,
                 endsWith(scoring, "more inside") & p >= 0.05 ~ 50,

                 .default = NA
               ),
               #status_statement = map(data, ~analysis(data = .x, type = "status")),
               #trend_statement = map(data, ~analysis(data = .x, type = "trend"))
        )

      status_statement <- list()
      trend_statement <- list()
      for (i in seq_along(nesteddata$data))  {
        if (length(unique(nesteddata$data[[i]]$year)) > 1) {
      status_statement[[i]] <-ifelse(nesteddata$p[i] < 0.05, paste0("There was a significant difference between the inside and outside comparison (p=",round(nesteddata$p[i],2), ").Protection could therefore be positively impacting this variable"), paste0("There was a significant difference between the inside and outside comparison (p=",round(nesteddata$p[i],2),"Protection could therefore not be having a direct impact on this variable"))
      trend_statement[[i]] <- paste0('There is ',ifelse(nesteddata$p[i] < 0.05, "a significant", "no"), " change between the MPA and outer boundary (p= ", round(nesteddata$p[i],2), ")")
        } else {
          status_statement[[i]] <- paste0("Data only sampled in year ", unique(nesteddata$data[[i]]$year))
          trend_statement[[i]] <- paste0("Data only sampled in year ", unique(nesteddata$data[[i]]$year))

        }
      }
      nesteddata <- nesteddata |>
        dplyr::select(-model,-summaries,-coeffs,-controlTRUE,-p)

      nesteddata$status_statement <- unlist(status_statement)
      nesteddata$trend_statement <- unlist(trend_statement)

    } else {
      warning("scoring method not supported")

    }

    if (direction == "inverse") {
      nesteddata <- nesteddata |>
        mutate(score = 100-score)
    } else if (direction != "normal") {
      stop("direction must be 'normal' or 'inverse'")
    }
      # browser()

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
      mutate(plot = pmap(list(data,indicator,units,areaID), function(d,ind,u,id){
        p <- NULL
        if(!is.null(d)){
          plot_list <- list()

          for (i in seq_along(plot_type)) {
            par(mar = c(4, 4.5, 0.5, 1))

          if ('map-species' %in% plot_type[i]) {
            if (!(length(data[[which(areas$NAME_E == id)]][[indicator_var_name]]) > 25)) {
              plot_type <- "map"
            }
          }
          if("time-series" %in% plot_type[i]) {
            plot_list[[i]] <-  ggplot(d,aes(x=.data[[year]], y=.data[[indicator_var_name]]))+
              geom_point()+
              geom_line()+
              theme_classic()+
              ylab(paste0(ind, " (", u, ")"))
          }
          if("time-series-no-line" %in% plot_type[i]) {
            plot_list[[i]] <-  ggplot(d,aes(x=.data[[year]], y=.data[[indicator_var_name]]))+
              geom_point()+
              theme_classic()+
              ylab(paste0(ind, " (", u, ")"))
          }
          if("boxplot" %in% plot_type[i]) {
            # Create decade grouping
            d$decade_group <- floor(d[[year]] / bin_width) * bin_width

            # Plot with position_dodge to control width
            plot_list[[i]] <- ggplot(d, aes(x = decade_group + bin_width/2, y=.data[[indicator_var_name]], group = decade_group)) +
              geom_boxplot(width = bin_width*0.9) +
              scale_x_continuous(name = year,
                                 breaks = unique(d$decade_group),
                                 minor_breaks = NULL) +
              theme_classic()


          }
          if("violin" %in% plot_type[i]) {
            # Create decade grouping
            d$decade_group <- floor(d[[year]] / bin_width) * bin_width

            violin_ok <- d %>%
              group_by(decade_group) %>%
              summarise(n_obs = n()) %>%
              summarise(any_group_has_enough = any(n_obs >= 2)) %>%
              pull(any_group_has_enough)

            if (!(violin_ok)) {
              # violin plot not possible
              plot_list[[i]] <-  ggplot(d,aes(x=.data[[year]], y=.data[[indicator_var_name]]))+
                geom_point()+
                theme_classic()+
                ylab(paste0(ind, " (", u, ")"))


            } else {

            # Plot with position_dodge to control width
            plot_list[[i]] <- ggplot(d, aes(x = decade_group + bin_width/2, y = .data[[indicator_var_name]], group = decade_group)) +
              geom_violin(width = bin_width * 0.9) +
              scale_x_continuous(name = year,
                                 breaks = unique(d$decade_group),
                                 minor_breaks = NULL) +
              theme_classic() +
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
            }


          }
          if("map" %in% plot_type[i]){
            if ((!("sf" %in% class(d)))) {
              aes_geom <- d$geometry
            } else {
              if ("geoms" %in% names(d)) {
                aes_geom <- d$geoms
              } else {
                aes_geom <- d$geometry
              }

            }
            if (any(grepl("control site", unique(scoring), ignore.case = TRUE))) {
              coords <- st_coordinates(aes_geom)
              d_coords <- cbind(d, coords)
              plot_list[[i]] <- ggplot() +
                geom_sf(
                  data = areas[areaID == id, ],
                  fill = "white",
                  color = "black"
                ) +

                # Main spatial layer without contributing to shape legend
                geom_sf(
                  data = d,
                  aes(
                    geometry = aes_geom,
                    fill = .data[[indicator_var_name]]
                  ),
                  shape = 21,
                  color = "black",
                  size = 2,
                  show.legend = FALSE  # Disable broken shape legend here
                ) +

                # Add a separate geom_point layer just for legend
                geom_point(
                  data = d_coords,
                  aes(
                    x = X,
                    y = Y,
                    shape = as.factor(control),
                    fill = .data[[indicator_var_name]]
                  ),
                  color = "black",
                  size = 2
                ) +

                # Shape legend (works now because of geom_point)
                scale_shape_manual(
                  values = c("FALSE" = 21, "TRUE" = 24),  # Circle = Inside, Triangle = Outside
                  name = "Site Type",
                  labels = c("FALSE" = "Inside", "TRUE" = "Outside")
                ) +

                theme_classic() +
                labs(
                  fill = indicator_var_name,
                  title = id,
                  x=NULL,
                  y=NULL
                ) +
                theme(
                  plot.title = element_text(size = 10),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
                ) +
                coord_sf(crs = st_crs(areas))

            } else {

              # Standard case (no control site logic)
              plot_list[[i]] <- ggplot() +
                geom_sf(data = areas[areaID == id, ], fill = "white", color = "black") +
                geom_sf(
                  data = d,
                  aes(geometry = aes_geom, fill = .data[[indicator_var_name]]),
                  shape = 21,
                  color = "black",
                  size = 2
                ) +
                theme_classic() +
                labs(
                  fill = indicator_var_name,
                  title = id
                ) +
                theme(
                  plot.title = element_text(size = 10),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
                ) +
                coord_sf(crs = st_crs(areas))
            }
          }
          if ("map-species" %in% plot_type[i]) {
            plot_list[[i]] <- ggplot() +
              geom_sf(data = areas[areaID == id, ], fill = "white", color = "black") +
              geom_sf(data = d, aes(fill = subclass), shape = 21, color = "black", size = 2) +
              theme_classic() +
              labs(fill = "Subclass", title = id) +
              theme(
                plot.title = element_text(size = 10),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
              ) +
              coord_sf(crs = st_crs(areas))

          }
          if ("outside-comparison" %in% plot_type[i]) {
            summary_data <- d %>%
              group_by(.data[[year]], control) %>%
              summarize(mean_value = mean(.data[[indicator_var_name]], na.rm = TRUE), .groups = "drop")

            # Plot with separate lines for inside vs outside
            plot_list[[i]] <- ggplot(summary_data, aes(x = .data[[year]], y = mean_value, color = control)) +
              geom_point() +
              geom_line() +
              theme_classic() +
              ylab(paste0(ind, " (", u, ")")) +
              scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                                 labels = c("Inside MPA", "Outside MPA"),
                                 name = "Location")

          }
            if ("community-composition" %in% plot_type[i]) {
             if ("station" %in% names(d)) {
                plot_list[[i]] <- ggplot(d, aes(x = indicator_var_name, y = year, fill = taxa)) +
                  geom_bar(stat = "identity") +
                  facet_wrap(~ station) +
                  labs(x = indicator_var_name, y = "Year", fill = "Taxa") +
                  theme_minimal() +
                  theme(
                    legend.text = element_text(size = 8),
                    legend.title = element_text(size = 9),
                    legend.key.size = unit(0.5, "cm")
                  )
              } else {
                plot_list[[i]] <- ggplot(d, aes(x = year, y = indicator_var_name, fill = taxa)) +
                  geom_area() +
                  labs(x = "Year", y = indicator_var_name, fill = "Taxa") +
                  theme_minimal() +
                  theme(
                    legend.text = element_text(size = 8),
                    legend.title = element_text(size = 9),
                    legend.key.size = unit(0.5, "cm")
                  )
              }

            }
          }
          p <- try(patchwork::wrap_plots(plot_list, ncol = min(length(plot_type), 3)), silent=TRUE)

          if (inherits(p, "try-error")) {
            p <- try(patchwork::wrap_plots(plot_list[!vapply(plot_list, is.null, logical(1))], ncol = min(length(plot_type), 3)), silent=TRUE)
          }
          return(p)


        } else {
          return(p)
        }
      }
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
  #browser()

  return(as_tibble(final))
}

