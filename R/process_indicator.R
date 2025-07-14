#' Title
#'
#' @param data a data frame or sf data frame that contains the indicator data, the year, and either the latitude and longitude (i.e. data frame) or the geometry (i.e. sf data frame). Other optional columns (e.g. depth) can also be included by naming them in the other_nest_variables parameter.
#' @param indicator_var_name character string of the name of the column in the data that contains the indicator data
#' @param indicator character string of the name of the indicator
#' @param type character string of the instrument/platform/model type of the indicator
#' @param PPTID numeric project planning tool project ID
#' @param source source where the data was obtained from
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
#' @param plot_type character string of the type of plot to generate (e.g. time-series, boxplot, violin)
#' @param bin_width numeric width of the bins for the boxplot or violin plot
#' @param plot_lm logical indicating whether to plot a linear model on the plot
#' @param plot_lm_se logical indicating whether to plot the standard error of the linear model on the plot
#' @param direction character string of the direction of the indicator (e.g. normal, inverse)
#' @param regionID character string of the name of the column in the areas data that contains the region ID (e.g. region)
#' @param control_polygon a polygon of class "sfc_POLYGON" "sfc" that is used as a buffer for outside comparison.
#' @param climate_expectation a statement for climate indicators indicating what we expect to happen to that indicator
#' with the impacts of climate change
#' @param indicator_rationale a string indicating why the rational is of significance
#' @param bin_rationale a string indicator why the indicator is associated with a certain bin
#'
#' @returns
#' @importFrom dplyr case_when select rename mutate
#' @importFrom purrr map map_dbl
#' @importFrom sf st_as_sf st_join st_transform st_difference
#' @importFrom stats lm
#' @importFrom tidyr nest
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_classic ylab geom_smooth
#' @importFrom rlang .data
#' @importFrom units set_units
#' @importFrom tibble as_tibble
#' @importFrom worrms wm_records_name wm_classification
#' @importFrom patchwork wrap_plots
#'
#'
#' @export
#'
#' @examples
process_indicator <- function(data, indicator_var_name = NA, indicator, type = NA, units = NA, scoring = NA, direction = "normal",
                              PPTID = NA, source=NA, project_short_title = NA, climate = FALSE, design_target = FALSE, crs = 4326,
                              latitude = "latitude", longitude = "longitude", year = "year", other_nest_variables = NA, areas = NA,
                              areaID = "NAME_E", regionID = "region", plot_type = "time-series",bin_width = 5, plot_lm = TRUE, plot_lm_se = TRUE,
                              control_polygon=NA, climate_expectation=NA,indicator_rationale=NA,bin_rationale=NA){


  if ("map-species" %in% plot_type) {
    if (is.na(other_nest_variables)) {
      stop("Must provide other_nest_variable named containing subclass when plot_type = 'map-species'")
    } else {
      if (!('subclass' %in% other_nest_variables)) {
        stop("Must provide other_nest_variable named containing subclass when plot_type = 'map-species'")
      }
    }
  }

  if(climate) {
    if(is.na(climate_expectation)) {
      stop("Must provide a climate_expectation argument for climate indicators.")
    }
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
                 # status_statement = paste0(areaID,
                 #                           " has had recorded occurrences of ",
                 #                           nrow(rawdata),
                 #                           " taxa which represents ",
                 #                           round(nrow(rawdata)/nrow(data)*100,1),
                 #                           "% of taxa recorded in this dataset",
                 #                           pmap_chr(layeroccurrences,
                 #                                    function(total_occurrences,
                 #                                             occurrences,
                 #                                             ...){
                 #                                      if_else(occurrences/total_occurrences>0.8,
                 #                                              paste0(", and ",
                 #                                                     occurrences,
                 #                                                     " occurrences of ",
                 #                                                     list(...)[[indicator_var_name]],
                 #                                                     " which represents ",
                 #                                                     round(occurrences/total_occurrences*100,1),
                 #                                                     "% of occurrences for that taxa"),
                 #                                              "")
                 #                                    }) |>
                 #                             paste(collapse = ""),
                 #                           "."),
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
          rename(site_areaID = {{areaID}}) |>
          st_join(dplyr::select(control_polygon, {{areaID}})) |>
          rename(control_areaID = {{areaID}}) |>
          mutate(areaID=if_else(is.na(site_areaID), control_areaID, site_areaID),
                 control=case_when(is.na(site_areaID)&is.na(control_areaID)~NA,
                                   !is.na(site_areaID)&is.na(control_areaID)~FALSE,
                                   is.na(site_areaID)&!is.na(control_areaID)~TRUE,
                                   .default = NA)) |>
          dplyr::select(-site_areaID, -control_areaID)

      } else {
        # join with areas
        data <- data |>
          st_join(dplyr::select(areas,{{areaID}})) |>
          rename(site_areaID = {{areaID}}) |>
          st_join(dplyr::select(control_polygon, {{areaID}})) |>
          rename(control_areaID = {{areaID}}) |>
          mutate(areaID=if_else(is.na(site_areaID), control_areaID, site_areaID),
                 control=case_when(is.na(site_areaID)&is.na(control_areaID)~NA,
                                   !is.na(site_areaID)&is.na(control_areaID)~FALSE,
                                   is.na(site_areaID)&!is.na(control_areaID)~TRUE,
                                   .default = NA))|>
          dplyr::select(-site_areaID, -control_areaID)
      }

      nest_cols <- c(year,
                     indicator_var_name,
                     attr(data, "sf_column"),
                     other_nest_variables,
                     "control")


      nesteddata <- data.frame(data|>
                                 filter(!is.na(control)) |>
                                 filter(!is.na(.data[[indicator_var_name]])) |>
                                 group_by(areaID) |>
                                 mutate(garbage = case_when(all(control)~TRUE,
                                                            all(!control)~TRUE,
                                                            .default = FALSE)) |>
                                 filter(!garbage) |>
                                 dplyr::select(-garbage) |>
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

            # Plot with position_dodge to control width
            plot_list[[i]] <- ggplot(d, aes(x = decade_group + bin_width/2, y=.data[[indicator_var_name]], group = decade_group)) +
              geom_violin(width = bin_width*0.9) +
              scale_x_continuous(name = year,
                                 breaks = unique(d$decade_group),
                                 minor_breaks = NULL) +
              theme_classic()


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

            plot_list[[i]] <- ggplot() +
              geom_sf(data = areas[areaID == id, ], fill = "white", color = "black") +
              geom_sf(
                # ifelse((!("sf" %in% class(d))),geometry,eval(parse(text = attr(d, "sf_column"))))
                data = d,
                aes(geometry=aes_geom, fill = .data[[indicator_var_name]]),
                shape = 21,      # Use a fillable shape
                color = "black",
                size = 2
              ) +
              theme_classic() +
              labs(
                fill = indicator_var_name,
                title = id
              ) +
              coord_sf(crs = st_crs(areas))
          }
          if ("map-species" %in% plot_type[i]) {
            #subclass <- NULL

            # for (i in seq_along(data[[which(areas$NAME_E == id)]][[indicator_var_name]])) {
            #   result <- try(worrms::wm_records_name(data[[which(areas$NAME_E == id)]][[indicator_var_name]][i]), silent=TRUE)
            #   if (inherits(result, "try-error")) {
            #     subclass[i] <- NA
            #   } else {
            #     aphia_id <- result$AphiaID[1]  # Use the first match, or refine if needed
            #     classification <- worrms::wm_classification(id = aphia_id)
            #     subclass[i] <- ifelse(length(classification$scientificname[which(classification$rank == "Subclass")]) == 0, NA, classification$scientificname[which(classification$rank == "Subclass")])
            #   }
            # }
            #
            # data$subclass <- subclass

            plot_list[[i]] <- ggplot() +
              geom_sf(data = areas[areaID == id, ], fill = "white", color = "black") +
              geom_sf(data = d, aes(fill = subclass), shape = 21, color = "black", size = 2) +
              theme_classic() +
              labs(fill = "Subclass", title = id) +
              coord_sf(crs = st_crs(areas))
            #data$subclass <- NULL

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

            # FIXME

          # if (plot_lm) {
          #   return(p + geom_smooth(method = "lm", se = plot_lm_se))
          # } else {
          #   return(p)
          # }

            # END FIXME
        } #END LOOP
          p <- try(patchwork::wrap_plots(plot_list, ncol = min(length(plot_type), 3)), silent=TRUE)

          if (inherits(p, "try-error")) {
            p <- try(patchwork::wrap_plots(plot_list[!vapply(plot_list, is.null, logical(1))], ncol = min(length(plot_type), 3)), silent=TRUE)
          }

          if (inherits(p, "try-error")) {
            #browser()  # Enter debug mode here if there's an error
          }

          return(p)


        } else {
          return(p)
        }
      }
      ))
    #browser() it worked here



  } else {
    # NA data case
    final <- data.frame(
      areaID = as.vector(unique(dplyr::select(as.data.frame(areas),{{areaID}}))[,1]),
      data,
      plot = NA,
      source=source,
      climate_expectation=climate_expectation,
      indicator_rationale=indicator_rationale,
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
      status_statement = "TBD"
    )
  }
  return(final)
}

