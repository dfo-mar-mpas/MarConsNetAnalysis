#' Title
#'
#' @inheritParams process_indicator
#' @inherit process_indicator params data scoring direction areas year
#'  indicator_var_name areaID other_nest_variables type units PPTID
#'  project_short_title climate design_target latitude longitude crs
#' indicator control_polygon
#'
#' @returns
#' @export
#'
#' @examples
assess_indicator <- function(data, scoring, direction,
                             areas, year, indicator_var_name,
                             areaID, other_nest_variables,
                             type,units,
                             PPTID,
                             project_short_title,
                             climate,
                             design_target,
                             latitude,
                             longitude,
                             crs,
                             indicator,
                             control_polygon) {


  if (startsWith(scoring,"desired state:")){
    #browser()

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

    # browser()

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
        inmpa = !is.na(site_areaID) & site_areaID != "Non_Conservation_Area",
        control = case_when(
          is.na(site_areaID) & is.na(control_areaID) ~ NA,
          !is.na(site_areaID) & is.na(control_areaID) ~ FALSE,
          (is.na(site_areaID) | site_areaID == "Non_Conservation_Area") &
            !is.na(control_areaID) & needs_this_buffer ~ TRUE,
          .default = FALSE
        ),
        areaID=case_when(inmpa ~ site_areaID,
                         control ~ control_areaID,
                         .default = "Non_Conservation_Area"),
      ) |>
      dplyr::select(-site_areaID, -control_areaID, -buffer_order, -needs_this_buffer, -buffer_distance, -inmpa)

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

  return(nesteddata)

}
