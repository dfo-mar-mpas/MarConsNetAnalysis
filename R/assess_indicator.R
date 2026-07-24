#' Perform indicator scoring and assessment
#'
#' Calculates indicator scores and associated status and trend summaries
#' for a given indicator dataset, based on a specified scoring method.
#'
#' This function supports multiple indicator scoring frameworks used in
#' the MarConsNet analysis workflow, including:
#' * **trend‑based scoring** (linear regression over time),
#' * **representation** (proportion of area or occurrences within MPAs),
#' * **coverage** (coverage of features relative to conservation targets),
#' * **median** (median value relative to maximum),
#' * **control site comparisons** (difference inside vs outside MPAs).
#'
#' The function returns a tibble with one row per area, including
#' numeric scores (0–100), data summaries, and human‑readable
#' status and trend statements.
#'
#' @importFrom rang as_name
#'
#' @examples
#' @export
assess_indicator <- function(
  data,
  scoring,
  direction,
  areas,
  year,
  indicator_var_name,
  areaID,
  other_nest_variables,
  type,
  units,
  PPTID,
  project_short_title,
  climate,
  design_target,
  latitude,
  longitude,
  crs,
  indicator,
  control_polygon,
  regionID
) {
  assumptions_storage <- attr(data, 'assumptions')
  caveats_storage <- attr(data, 'caveats')

  score_note <- "No year_of_data_collection column available. Score based on full dataset."


  ## Determining which geom to use for join (for externalData, e.g. seals)

  old_geom_col <- attr(areas, "sf_column")  # original geometry

  geom_to_use <- if ("geom_external_buffer" %in% names(areas)) {
    "geom_external_buffer"   # 🔴 use buffered
  } else {
    attr(areas, "sf_column") # 🔴 use original geometry
  }

  areas_use <- areas
  if (geom_to_use == 'geom_external_buffer') {
    areas_use <- areas_use |>
      st_set_geometry("geom_external_buffer")
  }
  ## end externalData


  if (startsWith(scoring, "desired state")) {
    if (!year %in% names(data)) {
      stop("year column not found")
    }
    # check if data is an sf object
    if (!inherits(data, "sf")) {
      if (!latitude %in% names(data)) {
        stop("latitude column not found")
      }
      if (!longitude %in% names(data)) {
        stop("longitude column not found")
      }

      # convert to sf object and join with areas
     data <- data |>
        dplyr::filter(!is.na({{ longitude }}), !is.na({{ latitude }})) |>
        st_as_sf(coords = c(rlang::as_name(ensym(longitude)),
                            rlang::as_name(ensym(latitude))),
                 crs = crs) |>
        st_join(dplyr::select(areas_use, {{ areaID }})) |>
        rename(areaID = {{ areaID }})


    } else {
      # join with areas
      data <- data |>
        st_join(dplyr::select(areas_use, {{ areaID }})) |>
        rename(areaID = {{ areaID }})
    }

    old_geom_col <- attr(areas, "sf_column")  # original geometry

    # identify the columns to nest
    nest_cols <- c(
      year,
      indicator_var_name,
      attr(data, "sf_column"),
      other_nest_variables
    )

    if (any(is.na(data$areaID))) {
      data$areaID[which(is.na(data$areaID))] <- 'Non_Conservation_Area'
    }


    if (inherits(data, "sf")) {
      if (latitude %in% names(data)) {
        data <- data[, -(which(names(data) == latitude))]
      }

      if (longitude %in% names(data)) {
        data <- data[, -(which(names(data) == longitude))]
      }
    }



    nesteddata <- data.frame(
      data,
      indicator = indicator,
      type = type,
      units = units,
      scoring = scoring,
      PPTID = PPTID,
      project_short_title = project_short_title,
      climate = climate,
      design_target = design_target
    ) |>
      nest(data = nest_cols[!is.na(nest_cols)])

    ## Score, trend, and status statement
    # Preallocate storage
    n <- nrow(nesteddata)

    models <- vector("list", n)
    summaries <- vector("list", n)
    coeffs <- vector("list", n)

    slope_year <- numeric(n)
    p <- numeric(n)
    score <- numeric(n)

    status_statement <- vector("list", n)
    trend_statement <- vector("list", n)
    score_note <- character(n)

    for (i in seq_len(n)) {
      message("nested i = ", i)

      DATA <- nesteddata$data[[i]]

      if (is.null(DATA) || nrow(DATA) == 0) {
        status_statement[[i]] <- "No data available."
        trend_statement[[i]] <- "No data available."
        score_note[i] <- "No data available."
        next
      }

      # -----------------------------
      # GET ESTABLISHMENT DATE
      # -----------------------------
      estab_date <- areas$date_of_establishment[
        areas[[areaID]] == unique(nesteddata$areaID[i])
      ]

      # -----------------------------
      # SELECT DATA FOR TREND + SCORE
      # -----------------------------

      if (!(nesteddata$areaID[i] == "Non_Conservation_Area")) {
        if ("year_of_data_collection" %in% names(DATA)) {
          if (!(grepl("since establishment", nesteddata$scoring[i]))) {
            DATA_post <- DATA[
              !is.na(DATA$year_of_data_collection) &
                DATA$year_of_data_collection >= estab_date,
            ]

            if (nrow(DATA_post) >= 1) {
              DATA_use <- DATA_post
              score_note[i] <- "Score only based on data post establishment."
            } else {
              DATA_use <- DATA
              score_note[
                i
              ] <- "Not enough post-establishment data; score based on full dataset."
            }
          } else {
            DATA_use <- DATA
            score_note[i] <- 'Score based on all data.'
          }
        } else {
          DATA_use <- DATA
          score_note[
            i
          ] <- "No year_of_data_collection column available. Score based on full dataset."
        }
      } else {
        DATA_use <- DATA
        score_note[
          i
        ] <- 'Score based on all data (no establishment date for Non Conservation Areas).'
      }

      # -----------------------------
      # MODEL FOR TREND + SCORE
      # -----------------------------
      if (
        length(unique(DATA_use[[year]][
          !is.na(DATA_use[[indicator_var_name]])
        ])) >
          1
      ) {
        model_i <- lm(
          as.formula(paste0(indicator_var_name, "~", year)),
          data = DATA_use
        )

        models[[i]] <- model_i
        summaries[[i]] <- summary(model_i)
        coeffs[[i]] <- coefficients(summary(model_i))

        slope_year[i] <- coeffs[[i]][2, 1]
        p[i] <- coeffs[[i]][2, 4]
      } else {
        slope_year[i] <- NA_real_
        p[i] <- NA_real_
      }

      # -----------------------------
      # SCORE (BASE R)
      # -----------------------------
      sc <- nesteddata$scoring[i]

      if (endsWith(sc, "increase")) {
        if (!is.na(p[i]) && p[i] < 0.05 && slope_year[i] > 0) {
          score[i] <- 100
        } else if (!is.na(p[i]) && p[i] < 0.05 && slope_year[i] < 0) {
          score[i] <- 0
        } else {
          score[i] <- 50
        }
      }

      if (endsWith(sc, "decrease")) {
        if (!is.na(p[i]) && p[i] < 0.05 && slope_year[i] < 0) {
          score[i] <- 100
        } else if (!is.na(p[i]) && p[i] < 0.05 && slope_year[i] > 0) {
          score[i] <- 0
        } else {
          score[i] <- 50
        }
      }

      if (endsWith(sc, "stable")) {
        if (!is.na(p[i]) && p[i] < 0.05) score[i] <- 0 else score[i] <- 100
      }

      # -----------------------------
      # STATUS (FULL DATA)
      # -----------------------------
      yrs <- sort(unique(as.numeric(DATA[[year]])))
      last_year <- tail(yrs, 1)

      DATA_5_YEARS <- DATA[DATA[[year]] %in% tail(yrs, 5), ]

      status_statement[[i]] <- paste0(
        "The most recent year, ",
        last_year,
        ", shows a mean of ",
        round(mean(DATA[[indicator_var_name]], na.rm = TRUE), 2),
        if (!is.na(units) && units != "") paste0(" (", units, ")") else "",
        " (sd = ",
        round(sd(DATA[[indicator_var_name]], na.rm = TRUE), 2),
        "). ",
        "The most recent 5 years (",
        paste(tail(yrs, 5), collapse = ","),
        ") showed a mean of ",
        round(mean(DATA_5_YEARS[[indicator_var_name]], na.rm = TRUE), 2),
        if (!is.na(units) && units != "") paste0(" (", units, ")") else "",
        " (sd = ",
        round(sd(DATA_5_YEARS[[indicator_var_name]], na.rm = TRUE), 2),
        "). "
      )

      # -----------------------------
      # TREND (DATA_USE ONLY)
      # -----------------------------
      yrs_use <- sort(unique(as.numeric(DATA_use[[year]])))

      if (length(yrs_use) > 1 && !is.na(slope_year[i])) {
        trend_dir <- ifelse(slope_year[i] > 0, "increase", "decrease")

        DATA_use_5 <- DATA_use[DATA_use[[year]] %in% tail(yrs_use, 5), ]

        if (length(unique(DATA_use_5[[year]])) > 1) {
          model5 <- lm(
            as.formula(paste0(indicator_var_name, "~", year)),
            data = DATA_use_5
          )

          slope5 <- coef(model5)[2]
          p5 <- summary(model5)$coefficients[2, 4]
          trend5 <- ifelse(slope5 > 0, "increase", "decrease")

          trend_statement[[i]] <- paste0(
            "Linear regression shows a ",
            trend_dir,
            " of ",
            round(slope_year[i], 2),
            if (!is.na(units) && units != "") paste0(" (", units, ")") else "",
            " over ",
            length(yrs_use),
            " years (p = ",
            round(p[i], 3),
            "). ",
            "Over the last 5 years (",
            paste(tail(yrs_use, 5), collapse = ","),
            ") the trend shows a ",
            trend5,
            " of ",
            round(slope5, 2),
            if (!is.na(units) && units != "") paste0(" (", units, ")") else "",
            " (p = ",
            round(p5, 3),
            ")."
          )
        } else {
          trend_statement[[i]] <- paste0(
            "Linear regression shows a ",
            trend_dir,
            " of ",
            round(slope_year[i], 2),
            " over ",
            length(yrs_use),
            " years (p = ",
            round(p[i], 3),
            "). ",
            "Insufficient data for 5-year trend."
          )
        }
      } else {
        trend_statement[[i]] <- "Insufficient data for trend analysis."
      }
    }

    # -----------------------------
    # ASSIGN BACK TO NESTEDDATA
    # -----------------------------
    nesteddata$model <- models
    nesteddata$summaries <- summaries
    nesteddata$coeffs <- coeffs
    nesteddata$slope_year <- slope_year
    nesteddata$p <- p
    nesteddata$score <- score
    nesteddata$status_statement <- status_statement
    nesteddata$trend_statement <- trend_statement
    nesteddata$score_note <- score_note

    nesteddata <- nesteddata |>
      dplyr::select(-model, -summaries, -coeffs, -slope_year, -p)

    nesteddata$status_statement <- unlist(status_statement)
    nesteddata$trend_statement <- unlist(trend_statement)
  } else if (startsWith(scoring, "representation")) {
    #areas <- areas[-which(areas$NAME_E == "Non_Conservation_Area"),]

    if (!inherits(data, "sf")) {
      stop("data must be an sf object for 'representation' scoring")
    }
    if (!(indicator_var_name %in% names(data))) {
      stop("indicator_var_name column not found in data")
    }
    if (st_crs(data) != st_crs(areas)) {
      stop("data and areas must have the same CRS")
    }
    if (
      endsWith(scoring, "regional thresholds") & !(regionID %in% names(areas))
    ) {
      stop(paste0(
        "Scoring methods that use 'regional thresholds' require a '",
        regionID,
        "' (set by the regionID argument) in your 'areas' argument"
      ))
    }

    # identify the columns to nest
    nest_cols <- c(
      indicator_var_name,
      attr(data, "sf_column"),
      other_nest_variables
    )

    if (all(endsWith(as.character(st_geometry_type(data)), "POLYGON"))) {
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
        rename(IDtemp = {{ areaID }}) |>
        dplyr::select(c("IDtemp", nest_cols[!is.na(nest_cols)])) |>
        filter(!is.na({{ indicator_var_name }})) |>
        rowwise() |>
        mutate(
          layerareakm2 = st_area(.data[[attr(data, "sf_column")]]) |>
            set_units("km^2") |>
            as.numeric() |>
            round(1),
          layerpercentmpa = layerareakm2 /
            mpaareas[as.data.frame(areas)[areaID] == IDtemp] *
            100,
          layerpercentlayer = layerareakm2 /
            layerareas[
              as.data.frame(data)[indicator_var_name] ==
                .data[[indicator_var_name]]
            ] *
            100,
        ) |>
        rename(areaID = IDtemp) |>
        ungroup() |>
        nest(
          rawdata = nest_cols[!is.na(nest_cols)],
          layerpercents = c(
            {{ indicator_var_name }},
            layerareakm2,
            layerpercentmpa,
            layerpercentlayer
          )
        ) |>
        rowwise() |>
        # calculate score based on the proportion of non-NA values
        mutate(
          score = nrow(rawdata) / nrow(data) * 100,
          indicator = indicator,
          type = type,
          units = units,
          scoring = scoring,
          PPTID = PPTID,
          project_short_title = project_short_title,
          climate = climate,
          design_target = design_target,
          status_statement = paste0(
            areaID,
            pmap_chr(
              layerpercents,
              function(layerareakm2, layerpercentmpa, layerpercentlayer, ...) {
                paste0(
                  " is ",
                  round(layerpercentmpa, 1),
                  "% covered by ",
                  list(...)[[indicator_var_name]],
                  " which represents ",
                  round(layerpercentlayer, 1),
                  "% of that feature"
                )
              }
            ) |>
              paste(collapse = ", and"),
            "."
          ),
          trend_statement = "There is no temporal dimension in this data."
        ) |>
        rename(data = rawdata) |>
        dplyr::select(-layerpercents) |>
        right_join(
          areas |>
            as.data.frame() |>
            dplyr::select({{ areaID }}) |>
            unique() |>
            rename(areaID = {{ areaID }}),
          by = "areaID"
        ) |>
        mutate(
          score = if_else(is.na(score), 0, score),
          indicator = coalesce(indicator, !!indicator),
          type = coalesce(type, !!type),
          units = coalesce(units, !!units),
          scoring = coalesce(scoring, !!scoring),
          PPTID = coalesce(PPTID, !!PPTID),
          project_short_title = coalesce(
            project_short_title,
            !!project_short_title
          ),
          climate = coalesce(climate, !!climate),
          design_target = coalesce(design_target, !!design_target),
          status_statement = if_else(
            is.na(status_statement),
            "No features are represented.",
            status_statement
          ),
          trend_statement = "There is no temporal dimension in this data."
        )
    } else {
      if (attr(data, "sf_column") == "geometry") {
        data[["geoms"]] <- data[["geometry"]]
      }

      totaloccurrences <- data |>
        mutate(
          total_occurrences = case_when(
            st_geometry_type(geoms) == "POINT" ~ 1,
            st_geometry_type(geoms) == "MULTIPOINT" ~ lengths(st_geometry(
              geoms
            )) /
              2,
            TRUE ~ NA_integer_
          )
        ) |>
        st_drop_geometry()

      nesteddata <- data |>
        # intersection with areas
        st_intersection(areas) |>
        rename(areaID = {{ areaID }}) |>
        dplyr::select(c("areaID", nest_cols[!is.na(nest_cols)])) |>
        filter(!is.na({{ indicator_var_name }})) |>
        left_join(
          totaloccurrences |>
            dplyr::select({{ indicator_var_name }}, total_occurrences),
          by = indicator_var_name
        ) |>
        rowwise() |>
        mutate(
          occurrences = case_when(
            st_geometry_type(geoms) == "POINT" ~ 1,
            st_geometry_type(geoms) == "MULTIPOINT" ~ length(st_geometry(
              geoms
            )) /
              2,
            TRUE ~ NA_integer_
          )
        ) |>
        nest(
          rawdata = nest_cols[!is.na(nest_cols)],
          layeroccurrences = c(
            {{ indicator_var_name }},
            total_occurrences,
            occurrences
          )
        ) |>
        rowwise() |>
        # calculate score based on the proportion of non-NA values
        mutate(
          score = nrow(rawdata) / nrow(data) * 100,
          indicator = indicator,
          type = type,
          units = units,
          scoring = scoring,
          PPTID = PPTID,
          project_short_title = project_short_title,
          climate = climate,
          design_target = design_target,
          status_statement = if_else(
            !endsWith(scoring, "count"),
            paste0(
              areaID,
              " has had recorded occurrences of ",
              nrow(rawdata),
              " taxa which represents ",
              round(nrow(rawdata) / nrow(data) * 100, 1),
              "% of taxa recorded in this dataset"
            ),
            paste0(
              areaID,
              " has had recorded occurrences of ",
              nrow(rawdata), " ", {{ indicator }},
              " which represents ",
              round(nrow(rawdata) / nrow(data) * 100, 1),
              "% of ", {{ indicator }}
            )
          ),
          trend_statement = "There is no temporal dimension in this data."
        ) |>
        rename(data = rawdata) |>
        dplyr::select(-layeroccurrences) |>
        right_join(
          areas |>
            as.data.frame() |>
            dplyr::select({{ areaID }}) |>
            unique() |>
            rename(areaID = {{ areaID }}),
          by = "areaID"
        ) |>
        mutate(
          score = if_else(is.na(score), 0, score),
          indicator = coalesce(indicator, !!indicator),
          type = coalesce(type, !!type),
          units = coalesce(units, !!units),
          scoring = coalesce(scoring, !!scoring),
          PPTID = coalesce(PPTID, !!PPTID),
          project_short_title = coalesce(
            project_short_title,
            !!project_short_title
          ),
          climate = coalesce(climate, !!climate),
          design_target = coalesce(design_target, !!design_target),
          status_statement = if_else(
            is.na(status_statement),
            "No features are represented.",
            status_statement
          ),
          trend_statement = "There is no temporal dimension in this data."
        )
    }

    if (endsWith(scoring, "regional thresholds")) {
      nesteddata <- nesteddata |>
        left_join(
          areas |>
            as.data.frame() |>
            dplyr::select({{ areaID }}, {{ regionID }}) |>
            unique() |>
            rename(areaID = {{ areaID }}),
          by = "areaID"
        ) |>
        group_by(across(all_of(regionID))) |>
        mutate(
          score = case_when(
            grepl("site-maximum", scoring) ~ score / max(score) * 100,
            grepl("cumulative distribution", scoring) ~ cume_dist(score) * 100,
            .default = NA
          )
        ) |>
        ungroup() |>
        dplyr::select(-{{ regionID }})
    }
  } else if (startsWith(scoring, "coverage")) {
    if (!inherits(data, "sf")) {
      stop("data must be an sf object for 'representation' scoring")
    }
    if (!(indicator_var_name %in% names(data))) {
      stop("indicator_var_name column not found in data")
    }
    if (st_crs(data) != st_crs(areas)) {
      stop("data and areas must have the same CRS")
    }
    if (
      endsWith(scoring, "regional thresholds") & !(regionID %in% names(areas))
    ) {
      stop(paste0(
        "Scoring methods that use 'regional thresholds' require a '",
        regionID,
        "' (set by the regionID argument) in your 'areas' argument"
      ))
    }

    # identify the columns to nest
    nest_cols <- c(
      indicator_var_name,
      attr(data, "sf_column"),
      other_nest_variables
    )

    if (endsWith(scoring, "landings")) {
      # landings representation type
      landingsvar <- other_nest_variables[1]
      message(paste(
        "Using",
        landingsvar,
        "as the landings variable for coverage calculation."
      ))
      intersect <- st_intersection(
        data |>
          mutate(
            originalarea = as.numeric(st_area(.data[[attr(data, "sf_column")]]))
          ),
        areas
      ) |>
        mutate(
          totallandingsadjusted = !!sym(landingsvar) /
            originalarea *
            as.numeric(st_area(.data[[attr(data, "sf_colum")]])),
          areaID = !!sym(areaID)
        )

      site_prot_cp <- intersect |>
        group_by(
          areaID,
          region,
          layername,
          plainname,
          min_target,
          max_target
        ) |>
        reframe(
          cp_landings := sum(totallandingsadjusted),
          !!attr(intersect, "sf_column") := st_union(
            !!sym(attr(intersect, "sf_column"))
          ),
          cp_percent = cp_landings / sum(data[[landingsvar]]) * 100
        ) |>
        mutate(
          score = cume_dist(100 - cp_percent) * 100, #TODO is cume_dist the way to go here?
          scale = "site"
        ) |>
        st_as_sf()

      region_prot_cp <- site_prot_cp |>
        group_by(region) |>
        reframe(
          areaID = unique(region),
          region = unique(region),
          !!sym(indicator_var_name) := unique(!!sym(indicator_var_name)),
          cp_landings = sum(cp_landings),
          cp_percent = sum(cp_percent),
          score = if_else(
            100 - cp_percent < unique(data$min_target),
            (100 - cp_percent) / unique(data$min_target) * 100,
            100
          ),
          scale = "region",
          !!attr(intersect, "sf_column") := st_union(
            !!sym(attr(intersect, "sf_column"))
          )
        )

      nesteddata <- bind_rows(region_prot_cp, site_prot_cp) |>
        mutate(
          indicator = indicator,
          type = type,
          units = units,
          scoring = scoring,
          PPTID = PPTID,
          project_short_title = project_short_title,
          climate = climate,
          design_target = design_target,
          status_statement = paste0(
            areaID,
            " covers ",
            round(cp_landings),
            " tonnes of ",
            unique(data$plainname),
            " landings which is ",
            round(cp_percent, 2),
            "% of the area of this feature while ",
            unique(data$min_target),
            " to ",
            unique(data$max_target),
            "% was targeted"
          ),
          trend_statement = "There is no temporal dimension in this data."
        ) |>
        nest(
          data = c(
            cp_landings,
            cp_percent,
            attr(intersect, "sf_column"),
            {{ indicator_var_name }}
          )
        )
    } else {
      intersect <- select(areas, {{ areaID }}) |>
        st_make_valid() |>
        st_intersection(st_geometry(data)) |>
        st_make_valid() |>
        full_join(st_drop_geometry(areas), by = areaID) |>
        cross_join(st_drop_geometry(data))

      # else normal coverage type
      site_prot_cp <- intersect |>
        as.data.frame() |>
        dplyr::select(
          region,
          areaID = {{ areaID }},
          {{ indicator_var_name }},
          !!attr(intersect, "sf_column")
        ) |>
        st_as_sf() |>
        mutate(
          cp_area = as.numeric(st_area(.data[[attr(intersect, "sf_column")]])),
          cp_percent = cp_area / sum(as.numeric(st_area(data))) * 100,
          score = cume_dist(cp_percent) * 100, #TODO is cume_dist the way to go here?
          scale = "site"
        )

      region_prot_cp <- site_prot_cp |>
        group_by(region) |>
        reframe(
          areaID = unique(region),
          !!sym(indicator_var_name) := unique(!!sym(indicator_var_name)),
          cp_area = sum(cp_area),
          cp_percent = sum(cp_percent),
          score = case_when(
            cp_percent < data$min_target ~ cp_percent / data$min_target * 100,
            cp_percent > data$max_target ~ 100 -
              (cp_percent - data$max_target) / (100 - data$max_target) * 100,
            TRUE ~ 100
          ),
          scale = "region",
          !!attr(intersect, "sf_column") := st_union(
            !!sym(attr(intersect, "sf_column"))
          )
        )

      nesteddata <- bind_rows(region_prot_cp, site_prot_cp) |>
        mutate(
          indicator = indicator,
          type = type,
          units = units,
          scoring = scoring,
          PPTID = PPTID,
          project_short_title = project_short_title,
          climate = climate,
          design_target = design_target,
          status_statement = paste0(
            areaID,
            " covers ",
            round(cp_area / 1000000),
            " km^2 of ",
            unique(data$plainname),
            " which is ",
            round(cp_percent, 2),
            "% of the area of this feature while ",
            unique(data$min_target),
            " to ",
            unique(data$max_target),
            "% was targeted"
          ),
          trend_statement = "There is no temporal dimension in this data."
        ) |>
        nest(
          data = c(
            cp_area,
            cp_percent,
            attr(intersect, "sf_column"),
            {{ indicator_var_name }}
          )
        )
    }
  } else if (startsWith(scoring, "median")) {
    # check if data is an stars object
    if (!inherits(data, "stars")) {
      stop("data must be an stars object for 'median' scoring")
    }
    # identify the columns to nest
    nest_cols <- c(indicator_var_name, "geometry", other_nest_variables)

    nesteddata <- st_as_sf(data, as_points = TRUE) |>
      st_join(areas_use, left = FALSE) |>
      rename(areaID = {{ areaID }}) |>
      dplyr::select(c("areaID", nest_cols[!is.na(nest_cols)])) |>
      filter(!is.na({{ indicator_var_name }})) |>
      nest(rawdata = nest_cols[!is.na(nest_cols)]) |>
      mutate(
        median = map_dbl(
          rawdata,
          ~ median(.x[[indicator_var_name]], na.rm = TRUE)
        ),
        nrowdata = map_dbl(rawdata, ~ nrow(.x)),
        score = round(
          median / max(data[[indicator_var_name]], na.rm = TRUE) * 100
        ),
        indicator = indicator,
        type = type,
        units = units,
        scoring = scoring,
        PPTID = PPTID,
        project_short_title = project_short_title,
        climate = climate,
        design_target = design_target,
        status_statement = paste0(
          areaID,
          " has median value of ",
          round(median, 2),
          " (n = ",
          nrowdata,
          ") and ranges from ",
          map_dbl(
            rawdata,
            ~ round(min(.x[[indicator_var_name]], na.rm = TRUE), 2)
          ),
          " to ",
          map_dbl(
            rawdata,
            ~ round(max(.x[[indicator_var_name]], na.rm = TRUE), 2)
          ),
          "."
        ),
        trend_statement = "There is no temporal dimension in this data."
      ) |>
      rename(data = rawdata) |>
      dplyr::select(-median, -nrowdata)
  } else if (startsWith(scoring, "control site linear trend")) {
    if (any(areas$NAME_E == "Non_Conservation_Area")) {
      unioncontrolpolygon <- st_union(control_polygon) |>
        st_make_valid()
      areas2 <- areas
      areas2$geoms[areas$NAME_E == "Non_Conservation_Area"] <- areas$geoms[
        areas$NAME_E == "Non_Conservation_Area"
      ] |>
        st_difference(unioncontrolpolygon) |>
        st_combine()
    }
    if (!inherits(data, "sf")) {
      if (!latitude %in% names(data)) {
        stop("latitude column not found")
      }
      if (!longitude %in% names(data)) {
        stop("longitude column not found")
      }

      # convert to sf object and join with areas
      data <- st_as_sf(data, coords = c(longitude, latitude), crs = crs)
    }
    # join with areas

    buffers_sorted <- c("twenty_km", "forty_km", "sixty_km", "eighty_km")

    data <- data |>
      st_join(dplyr::select(areas_use, {{ areaID }})) |>
      rename(site_areaID = {{ areaID }}) |>
      st_join(dplyr::select(control_polygon, buffer_distance, {{ areaID }})) |>
      rename(control_areaID = {{ areaID }}) |>
      group_by(control_areaID) |>
      mutate(
        buffer_order = match(buffer_distance, buffers_sorted)
      ) |>
      mutate(
        # For each buffer, check if any smaller buffer has 5+ years
        needs_this_buffer = purrr::map_lgl(
          buffer_order,
          function(current_order) {
            if (is.na(current_order)) {
              return(FALSE)
            }
            if (current_order == 1) {
              return(TRUE)
            } # Always keep smallest buffer
            # Check cumulative years in each smaller buffer level
            max_years_in_smaller <- max(purrr::map_dbl(
              1:(current_order - 1),
              function(smaller_order) {
                dplyr::n_distinct(year[buffer_order <= smaller_order])
              }
            ))

            max_years_in_smaller < 5
          }
        ),
        max_buffer_used = ifelse(
          any(needs_this_buffer),
          buffers_sorted[max(buffer_order[needs_this_buffer], na.rm = TRUE)],
          NA_character_ # 🔴 default if no buffer needed
        )
      ) |>
      ungroup() |>
      mutate(
        inmpa = !is.na(site_areaID) & site_areaID != "Non_Conservation_Area",
        control = case_when(
          is.na(site_areaID) & is.na(control_areaID) ~ NA,
          !is.na(site_areaID) & is.na(control_areaID) ~ FALSE,
          (is.na(site_areaID) | site_areaID == "Non_Conservation_Area") &
            !is.na(control_areaID) &
            needs_this_buffer ~ TRUE,
          .default = FALSE
        ),
        areaID = case_when(
          inmpa ~ site_areaID,
          control ~ control_areaID,
          .default = "Non_Conservation_Area"
        ),
      ) |>
      dplyr::select(
        -site_areaID,
        -control_areaID,
        -buffer_order,
        -needs_this_buffer,
        -buffer_distance,
        -inmpa
      )

    nest_cols <- c(
      year,
      indicator_var_name,
      attr(data, "sf_column"),
      other_nest_variables,
      "control",
      "max_buffer_used"
    )
    nesteddata <- data.frame(
      data |>
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
      PPTID = PPTID,
      project_short_title = project_short_title,
      climate = climate,
      design_target = design_target
    ) |>
      nest(data = nest_cols[!is.na(nest_cols)])

    # score the data
    nesteddata <- nesteddata |>
      mutate(
        model = map(
          data,
          ~ lm(
            as.formula(paste0(indicator_var_name, "~", year, "+ control")),
            data = .x
          )
        ),
        summaries = map(model, summary),
        coeffs = map(summaries, coefficients),
        controlTRUE = map_dbl(
          coeffs,
          ~ {
            # Check if coeffs has at least 2 rows
            if (nrow(.x) >= 3) {
              return(.x[3, 1])
            } else {
              # Return NA or some other default value
              return(NA_real_)
            }
          }
        ),
        p = map_dbl(
          summaries,
          ~ {
            # Check if coefficients has at least 2 rows
            if (is.null(.x$coefficients) || nrow(.x$coefficients) < 3) {
              return(NA_real_)
            } else {
              return(.x$coefficients[3, 4])
            }
          }
        ),
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
    for (i in seq_along(nesteddata$data)) {
      if (length(unique(nesteddata$data[[i]]$year)) > 1) {
        status_statement[[i]] <- ifelse(
          nesteddata$p[i] < 0.05,
          paste0(
            "There was a significant difference between the inside and outside comparison (p=",
            round(nesteddata$p[i], 2),
            ").Protection could therefore be positively impacting this variable"
          ),
          paste0(
            "There was a significant difference between the inside and outside comparison (p=",
            round(nesteddata$p[i], 2),
            "Protection could therefore not be having a direct impact on this variable"
          )
        )
        trend_statement[[i]] <- paste0(
          'There is ',
          ifelse(nesteddata$p[i] < 0.05, "a significant", "no"),
          " change between the MPA and outer boundary (p= ",
          round(nesteddata$p[i], 2),
          ")"
        )
      } else {
        status_statement[[i]] <- paste0(
          "Data only sampled in year ",
          unique(nesteddata$data[[i]]$year)
        )
        trend_statement[[i]] <- paste0(
          "Data only sampled in year ",
          unique(nesteddata$data[[i]]$year)
        )
      }
    }
    nesteddata <- nesteddata |>
      dplyr::select(-model, -summaries, -coeffs, -controlTRUE, -p)

    nesteddata$status_statement <- unlist(status_statement)
    nesteddata$trend_statement <- unlist(trend_statement)
  } else if (scoring %in% c('proportion of species', 'community composition') |
             grepl('probability of detection', scoring, ignore.case = TRUE)) { # JAIM
    data <- data %>%
      filter(!is.na(latitude), !is.na(longitude))
    data <- st_as_sf(
      data,
      coords = c("longitude", "latitude"),
      crs = crs(areas)
    )

    nest_cols <- c(indicator_var_name, "geometry", other_nest_variables)

    areas_use <- st_make_valid(areas_use)
    data <- st_make_valid(data)

    nesteddata <- st_as_sf(data, as_points = TRUE) |>
      st_join(areas_use, left = FALSE) |>
      rename(areaID = {{ areaID }}) |>
      #st_drop_geometry() |>
      filter(!is.na({{ indicator_var_name }})) |>
      group_by(areaID) |>
      nest()

    nesteddata$score <- NA
    nesteddata$status_statement <- NA
    nesteddata$trend_statement <- NA
    nesteddata$quality_statement <- NA


    if (scoring == 'community composition') {
      for (n in seq_along(1:nrow(nesteddata))) {
        message(n)
        ## 1. Convert data into community matrix

        community <- nesteddata$data[[n]] %>%
          sf::st_drop_geometry() %>%
          select(ID, year_of_data_collection, method, species, detections) %>%
          pivot_wider(
            names_from = species,
            values_from = detections,
            values_fill = list(detections = 0),
            values_fn = sum
          )

        # Keep sample metadata separate
        metadata <- community %>%
          select(ID, year_of_data_collection)

        community_matrix <- community %>%
          select(-ID, -year_of_data_collection)

        ## 2. Identify historical vs recent samples
        recent_year <- max(data$year_of_data_collection)

        historical <- metadata$year_of_data_collection < recent_year

        recent <- metadata$year_of_data_collection == recent_year

        ## 3: Estimate expected richness using historical community
        # Collapse detections across all historical and recent samples
        historical_matrix <- community_matrix[historical, ] %>%
          sf::st_drop_geometry() %>%
          select(-method)

        recent_matrix <- community_matrix[recent, ] %>%
          sf::st_drop_geometry() %>%
          select(-method)

        historical_total <- colSums(historical_matrix)
        recent_total <- colSums(recent_matrix)

        # Estimate expected historical richness (accounts for unseen species)
        historical_est <- vegan::estimateR(historical_total)

        expected_species <- as.numeric(historical_est["S.chao1"]) #Chao1 estimated species richness


        ## 4: Calculate observed recent richness

        observed_recent <- sum(recent_total > 0)


        ## 5. Calculate the score

        score <- (observed_recent / expected_species) * 100

        # Cap score at 100
        # If, 68 it means the number of species detected in the most recent
        # sampling year was 68% of the number of species expected based on the
        # historical eDNA community (after accounting for undetected species
        # using the richness estimator).
        nesteddata$score[n] <- min(score, 100)

        historical_species <- names(historical_total[historical_total > 0])

        recent_species <- names(recent_total[recent_total > 0])

        missing_species <- setdiff(historical_species, recent_species)

        new_species <- setdiff(recent_species, historical_species)


        ## NEW
        #Create lookup of scientific -> formatted name
        species_lookup <- nesteddata$data[[n]] |>
          dplyr::distinct(species, common_name) |>
          dplyr::group_by(species) |>
          dplyr::summarise(
            common_name = dplyr::first(na.omit(common_name)),
            .groups = "drop"
          )

        species_labels <- setNames(
          ifelse(
            is.na(species_lookup$common_name) | species_lookup$common_name == "",
            species_lookup$species,
            paste0(species_lookup$species, " (", species_lookup$common_name, ")")
          ),
          species_lookup$species
        )

        # Helper function
        format_species <- function(x) {
          out <- species_labels[x]
          out[is.na(out)] <- x[is.na(out)]  # fallback if species isn't in lookup
          out
        }

        ## END NEW


        nesteddata$status_statement[n] <- paste0(
          "In ", recent_year, ", ", observed_recent,
          " species were detected: ", paste(format_species(recent_species), collapse = ", "),
          ". Historical eDNA observations indicate an expected community of approximately ",
          round(expected_species, 0), " species, with ",
          length(missing_species),
          " historical species not detected in the most recent sampling period: ",
          paste(format_species(missing_species), collapse = ", "), "."
        )


        nesteddata$trend_statement[n] <- paste0(
          "Compared to the historical eDNA community, the current community has ",
          observed_recent, " detected species compared to ",
          length(historical_species),
          " historically detected species. ",
          ifelse(length(missing_species) > 0,
                 paste0("Species detected historically but not in the most recent survey include: ",
                        paste(format_species(missing_species), collapse = ", "), "."),
                 "All historically detected species were also detected in the most recent survey."),
          ifelse(length(new_species) > 0,
                 paste0(" New species detected in the most recent survey include: ",
                        paste(format_species(new_species), collapse = ", "), "."),
                 "")
        )


        nesteddata$quality_statement[n] <- paste0(
          "The assessment was based on ", nrow(recent_matrix),
          " eDNA samples collected in ", recent_year,
          " and compared against ", nrow(historical_matrix),
          " historical eDNA samples. Species richness estimates were adjusted using the Chao1 estimator ",
          "to account for potentially undetected species in the historical dataset."
        )



      }


    } else if (scoring == 'proportion of species') {
    nesteddata <- nesteddata |>
      mutate(

        score = purrr::map_dbl(data, ~ {
          yearly_species <- .x |>
            filter(!is.na(species)) |>
            group_by(year_of_data_collection) |>
            summarise(richness = n_distinct(species), .groups = "drop")

          if (nrow(yearly_species) <= 1) {
            return(NA_real_)
          }

          if (last(yearly_species$richness) >= first(yearly_species$richness)) 100 else 0
        }),

        trend_statement = purrr::map_chr(data, ~ {
          yearly_species <- .x |>
            filter(!is.na(species)) |>
            group_by(year_of_data_collection) |>
            summarise(richness = n_distinct(species), .groups = "drop") |>
            arrange(year_of_data_collection)
          # Determine the appropriate name to describe the group
          unique_species <- unique(na.omit(.x$species))

          if (length(unique_species) == 1) {
            # One species
            group_name <- unique_species

          } else {
            # Find common word(s) among species names (e.g., wolffish)
            common_words <- Reduce(
              intersect,
              strsplit(tolower(unique_species), "\\s+")
            )

            if (length(common_words) > 0) {
              # Multiple species sharing a common name
              group_name <- paste(common_words, collapse = " ")

            } else {
              # Fall back to taxonomy
              group_name <- unique(na.omit(.x$subclass))[1]

              if (is.na(group_name) || length(group_name) == 0) {
                group_name <- unique(na.omit(.x$class))[1]
              }

              if (is.na(group_name) || length(group_name) == 0) {
                group_name <- unique(na.omit(.x$superclass))[1]
              }
            }
          }

          if (grepl("trophic", indicator, ignore.case=TRUE)) {
            group_name <- unique(na.omit(.x$ai_trophic_level))
          }

          if (nrow(yearly_species) <= 1) {
            paste0(
              "There is only one year of sampling data available for ",
              group_name,
              "."
            )
          } else if (last(yearly_species$richness) > first(yearly_species$richness)) {
            paste0(
              "The number of unique ",
              group_name,
              " species detected increased from ",
              first(yearly_species$richness),
              " to ",
              last(yearly_species$richness),
              " between ",
              first(yearly_species$year_of_data_collection),
              " and ",
              last(yearly_species$year_of_data_collection),
              "."
            )
          } else if (last(yearly_species$richness) < first(yearly_species$richness)) {
            paste0(
              "The number of unique ",
              group_name,
              " species detected declined from ",
              first(yearly_species$richness),
              " to ",
              last(yearly_species$richness),
              " between ",
              first(yearly_species$year_of_data_collection),
              " and ",
              last(yearly_species$year_of_data_collection),
              "."
            )
          } else {
            paste0(
              "The number of unique ",
              group_name,
              " species detected remained stable at ",
              first(yearly_species$richness),
              " between ",
              first(yearly_species$year_of_data_collection),
              " and ",
              last(yearly_species$year_of_data_collection),
              "."
            )
          }
        }),


        status_statement = purrr::map_chr(data, ~ {
          latest_year <- max(.x$year_of_data_collection, na.rm = TRUE)

          latest_species <- .x |>
            filter(year_of_data_collection == latest_year, !is.na(species)) |>
            distinct(species) |>
            pull(species)

          unique_species <- unique(na.omit(.x$species))

          if (length(unique_species) == 1) {
            # One species
            group_name <- unique_species

          } else {
            # Find common word(s) among species names (e.g., wolffish)
            common_words <- Reduce(
              intersect,
              strsplit(tolower(unique_species), "\\s+")
            )

            if (length(common_words) > 0) {
              # Multiple species sharing a common name
              group_name <- paste(common_words, collapse = " ")

            } else {
              # Fall back to taxonomy
              group_name <- unique(na.omit(.x$subclass))[1]

              if (is.na(group_name) || length(group_name) == 0) {
                group_name <- unique(na.omit(.x$class))[1]
              }

              if (is.na(group_name) || length(group_name) == 0) {
                group_name <- unique(na.omit(.x$superclass))[1]
              }
            }
          }

          if (grepl("trophic", indicator, ignore.case=TRUE)) {
            group_name <- unique(na.omit(.x$ai_trophic_level))
          }

          species_lookup <- .x |>
            distinct(species, common_name) |>
            group_by(species) |>
            summarise(
              common_name = first(na.omit(common_name)),
              .groups = "drop"
            )

          species_labels <- setNames(
            ifelse(
              is.na(species_lookup$common_name) | species_lookup$common_name == "",
              species_lookup$species,
              paste0(species_lookup$species, " (", species_lookup$common_name, ")")
            ),
            species_lookup$species
          )

          format_species <- function(x) {
            out <- species_labels[as.character(x)]
            out[is.na(out)] <- x[is.na(out)]
            unname(out)
          }



          paste0(
            "The most recent sampling year was ",
            latest_year,
            ", during which ",
            length(latest_species),
            " unique ",
            group_name,
            " species were detected: ",
            paste(format_species(latest_species), collapse = ", "),
            "."
          )



        })
      )
    } else {

      #probability of detection
      # JAIM

      # Create columns
      nesteddata$score <- NA_real_
      nesteddata$status_statement <- NA_character_
      nesteddata$trend_statement <- NA_character_

      for (n in seq_along(nesteddata$data)) {

        df <- nesteddata$data[[n]]

        # Get species keyword from scoring (after :)
        target <- strsplit(scoring, ":")[[1]][2] |>
          trimws() |>
          tolower()

        # Calculate yearly proportion of samples detecting target species
        yearly_detection <- aggregate(
          detected ~ year_of_data_collection + ID,
          data = transform(
            df,
            detected = grepl(target, species, ignore.case = TRUE)
          ),
          FUN = any
        )

        yearly_detection <- aggregate(
          detected ~ year_of_data_collection,
          data = yearly_detection,
          FUN = mean
        )

        names(yearly_detection)[2] <- "proportion_detected"

        yearly_detection <- yearly_detection[
          order(yearly_detection$year_of_data_collection),
        ]

        # Cannot calculate trend with one year
        if (nrow(yearly_detection) <= 1) {

          nesteddata$score[n] <- NA_real_

          nesteddata$status_statement[n] <- paste0(
            "There is only one year of sampling data available for ",
            target,
            "."
          )

          nesteddata$trend_statement[n] <- paste0(
            "A trend in probability of detection for ",
            target,
            " cannot be assessed."
          )

        } else {

          first_detection <- yearly_detection$proportion_detected[1]
          last_detection <- yearly_detection$proportion_detected[nrow(yearly_detection)]

          # Score
          nesteddata$score[n] <- ifelse(
            last_detection >= first_detection,
            100,
            0
          )

          # Status
          nesteddata$status_statement[n] <- paste0(
            "In the most recent sampling year (",
            tail(yearly_detection$year_of_data_collection, 1),
            "), ",
            round(last_detection * 100, 1),
            "% of samples detected ",
            target,
            "."
          )

          # Trend
          if (last_detection > first_detection) {

            nesteddata$trend_statement[n] <- paste0(
              "The probability of detection for ",
              target,
              " increased from ",
              round(first_detection * 100, 1),
              "% to ",
              round(last_detection * 100, 1),
              "% between ",
              yearly_detection$year_of_data_collection[1],
              " and ",
              tail(yearly_detection$year_of_data_collection, 1),
              "."

            )

          } else if (last_detection < first_detection) {

            nesteddata$trend_statement[n] <- paste0(
              "The probability of detection for ",
              target,
              " declined from ",
              round(first_detection * 100, 1),
              "% to ",
              round(last_detection * 100, 1),
              "% between ",
              yearly_detection$year_of_data_collection[1],
              " and ",
              tail(yearly_detection$year_of_data_collection, 1),
              "."

            )

          } else {

            nesteddata$trend_statement[n] <- paste0(
              "The probability of detection for ",
              target,
              " remained stable at ",
              round(last_detection * 100, 1),
              "% between ",
              yearly_detection$year_of_data_collection[1],
              " and ",
              tail(yearly_detection$year_of_data_collection, 1),
              "."
            )
          }
        }
      }



      ## END

    }
    nesteddata <- nesteddata |>
      mutate(
        indicator = indicator,
        type = type,
        units = units,
        scoring = scoring,
        PPTID = PPTID,
        project_short_title = project_short_title,
        climate = climate,
        design_target = design_target
      )

    } else {
    warning("scoring method not supported")
  }

  if (direction == "inverse") {
    nesteddata <- nesteddata |>
      mutate(score = 100 - score)
  } else if (direction != "normal") {
    stop("direction must be 'normal' or 'inverse'")
  }

  nesteddata$quality_statement <- NA

  ## FIX PROBLEM IS MPAs is not the areas argument we don't have Non_Conservation_Area (e.g. ind_musquash_ph). It assigns it to a areaID of NA, which causes problems with save_plots
  if (any(is.na(nesteddata$areaID))) {
    nesteddata$areaID[which(is.na(
      nesteddata$areaID
    ))] <- "Non_Conservation_Area"
  }
#good until here
  for (i in seq_along(nesteddata$data)) {
    # Note a sample means unique date and geometry. If there are multiple depths in a single sample it counts as one sample
    message(i)
    quality_data <- nesteddata$data[[i]]

    if (!(is.null(quality_data))) {
      if (!(grepl("Network design", indicator))) {
        GEOM <- attr(quality_data, "sf_column")

        if (is.null(GEOM)) {
          GEOM <- names(quality_data)[which(grepl("geom", names(quality_data)))]
        }
        if (any(grepl("GEOMETRYCOLLECTION", class(quality_data[[GEOM]][1])))) {
          nesteddata$quality_statement[i] <- paste0(
            nesteddata$areaID[i],
            ": ",
            "There are no quality statements available for GEOMETRYCOLLECTION type"
          )
        } else if (any(grepl("POLYGON", class(quality_data[[GEOM]][1])))) {
          nesteddata$quality_statement[i] <- paste0(
            nesteddata$areaID[i],
            ": ",
            "There are no quality statements available for POLYGON type"
          )
        } else if (year %in% names(quality_data)) {
          number_of_samples <- quality_data %>%
            distinct({{ year }}, .data[[GEOM]]) %>%
            summarise(n_samples = n())

          if (any(grepl("data.frame", class(number_of_samples)))) {
            number_of_samples <- number_of_samples$n_samples
          }
          min_year <- min(sort(as.numeric(unique(quality_data[[year]]))))
          max_year <- max(sort(as.numeric(unique(quality_data[[year]]))))
          if (min_year == max_year) {
            nesteddata$quality_statement[i] <- paste0(
              nesteddata$areaID[i],
              ": ",
              number_of_samples,
              " samples taken (",
              min_year,
              ")"
            )
          } else {
            nesteddata$quality_statement[i] <- paste0(
              nesteddata$areaID[i],
              ": ",
              number_of_samples,
              " samples taken (",
              min_year,
              "-",
              max_year,
              ")"
            )
          }
        } else {
          nesteddata$quality_statement[i] <- paste0(
            nesteddata$areaID[i],
            ": ",
            nrow(quality_data)
          )
        }
      } else {
        nesteddata$quality_statement[i] <- NA
      }
    } else {
      nesteddata$quality_statement[i] <- NA
    }
  }


  if ("geom_external_buffer" %in% names(areas)) {
    assumptions_storage <- paste0(assumptions_storage, " Note: This analysis includes data that is outside of the conservation area boundary. It assumes that the data outside of the boundary is comparible.")

  }

  attr(nesteddata, "assumptions") <- assumptions_storage
  attr(nesteddata, "caveats") <- caveats_storage

  # SWITCH BACK TO ORIGINAL GEOMETRY
  areas_use <- areas_use |>
    st_set_geometry(old_geom_col)


  return(nesteddata)
}
