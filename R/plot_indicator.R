#' Generate indicator plots for visualization and reporting
#'
#' Creates one or more plots for a given indicator based on the
#' specified plot types. Supported visualizations include
#' time series, boxplots, violin plots, spatial maps, and
#' community composition plots. When multiple plot types are
#' requested, plots are combined into a single layout using
#' \pkg{patchwork}.
#'
#' @inheritParams process_indicator
#' @param data data processed in `assess_indicator()`
#' @inherit process_indicator params indicator units plot_type year
#'  indicator_var_name scoring areaID areas bin_width control_polygon
#'  habitat_display externalData
#' @param id Equivalent to `areaID` in [process_indicator()]
#'
#' @returns a plot
#' @export
#'
#' @examples
plot_indicator <- function(data,indicator,units,id, plot_type, year, indicator_var_name, scoring, areaID, areas, bin_width, control_polygon, control_nesteddata,control_polygon_out,habitat_display, externalData){


  p <- NULL

  unit_text <- if (!is.na(units) && trimws(units) != "") {
    paste0(" (", units, ")")
  } else {
    ""
  }


  if (!(is.null(habitat_display))) {
  if (!(class(habitat_display) == 'list')) {
    stop('habitat_display must be a list')
  }
  }


  if(!is.null(data) & !(all(is.na(data)))){
    plot_list <- list()

    for (i in seq_along(plot_type)) {


      par(mar = c(4, 4.5, 0.5, 1))

      if("time-series" %in% plot_type[i]) {
          est_year <- areas$date_of_establishment[areas$NAME_E == id]
          if(length(est_year)<1) est_year <- Inf

          # ---- Prepare inside data ----
          inside_data <- data
          inside_data$location <- "MPA"
          inside_data$period <- ifelse(
            inside_data[[year]] < est_year,
            "Before",
            "After"
          )

          # ---- Prepare control data (if exists) ----
          control_data <- NULL

          if (!(all(is.na(control_polygon)))) {
            control_row <- control_nesteddata[control_nesteddata$areaID == id, ]
            if (nrow(control_row) > 0) {
              control_data <- control_row$data[[1]]
              control_data$location <- "Control"
              control_data$period <- ifelse(
                control_data[[year]] < est_year,
                "Before",
                "After"
              )
            }
          }

          # Combine
          #combined_data <- rbind(inside_data,if(!is.null(control_data)) control_data)

          if (!(is.null(control_data))) {
            combined_data <- rbind(inside_data,control_data)
          } else {
            combined_data <- inside_data
          }

          combined_data <- combined_data |>
            dplyr::filter(!is.na(.data[[indicator_var_name]]))

          if (!("sf" %in% class(combined_data))) {
            combined_data <- combined_data %>% droplevels()   # 🔴 THIS IS THE KEY LINE
          }

          # ---- Compute global axis limits ----
          x_range <- range(combined_data[[year]], na.rm = TRUE)
          y_range <- range(combined_data[[indicator_var_name]], na.rm = TRUE)

          # ---- Create all 4 combinations explicitly ----
          combined_data$location <- factor(combined_data$location,
                                           levels = c("MPA", "Control"))
          combined_data$period <- factor(combined_data$period,
                                         levels = c("Before", "After"))
          # ---- Base plot ----
          p <- ggplot(combined_data,
                      aes(x = .data[[year]],
                          y = .data[[indicator_var_name]])) +
            geom_point() +
            geom_vline(xintercept = est_year,
                       colour = "red",
                       linewidth = 1) +
            facet_grid(period ~ location, drop=TRUE) +
            coord_cartesian(xlim = x_range,
                            ylim = y_range) +
            ylab(paste0(indicator, unit_text))

          # ---- Add regressions + R² per panel ----
          panel_info <- split(combined_data,
                              list(combined_data$location,
                                   combined_data$period),
                              drop = TRUE)


          for (df in panel_info) {

            # Make sure df is a data frame with rows
            if (is.null(df) || !is.data.frame(df) || nrow(df) < 2) {

              # Create a dummy data frame for facet-awareness
              max_global_x <- max(combined_data[[year]], na.rm = TRUE)
              max_global_y <- max(combined_data[[indicator_var_name]], na.rm = TRUE)

              # Add facet-specific "Insufficient data" text
              p <- p +
                geom_text(
                  data = data.frame(
                    location = if (!is.null(df$location)) df$location[1] else "MPA",
                    period   = if (!is.null(df$period)) df$period[1] else "Before",
                    x = max_global_x,
                    y = max_global_y
                  ),
                  aes(x = x, y = y, label = "Insufficient data"),
                  inherit.aes = FALSE,
                  hjust = 1,
                  vjust = 1,
                  size = 4
                )

            } else {
              # Fit linear model using base R
              model <- lm(as.formula(paste0(indicator_var_name, " ~ ", year)), data = df)
              coefs <- coef(model)
              intercept <- coefs[1]
              slope <- coefs[2]
              r2 <- summary(model)$r.squared

              # Determine the x-range for this panel
              x_min <- min(df[[year]], na.rm = TRUE)
              x_max <- max(df[[year]], na.rm = TRUE)
              y_max <- max(df[[indicator_var_name]], na.rm = TRUE)

              # Build a small data frame with endpoints for this panel
              label_text <- paste0("R² = ", round(r2, 2))

              r2_df <- data.frame(
                location = df$location[1],
                period   = df$period[1],
                x = min(combined_data[[year]], na.rm = TRUE)
,
                y = max(combined_data[[indicator_var_name]], na.rm = TRUE)
,
                label = label_text
              )

              r2_layer <- geom_text(
                data = r2_df,
                aes(x = x, y = y, label = label),
                inherit.aes = FALSE,
                hjust = 0,
                vjust = 1,
                size = 3
              )

              p <- p + r2_layer

              # Build a small data frame with endpoints for this panel
              line_df <- data.frame(
                x = x_min,
                xend = x_max,
                y = intercept + slope * x_min,
                yend = intercept + slope * x_max,
                location = df$location[1],
                period   = df$period[1]
              )
              # Add the regression line as a segment (respects facets)
              p <- p +
                geom_segment(
                  data = line_df,
                  aes(x = x, xend = xend, y = y, yend = yend),
                  linewidth = 1
                )
            }
          }



          p <- p +
            scale_x_continuous(
              breaks = scales::breaks_width(1),
              labels = function(x) as.character(x)
            )

          plot_list[[i]] <- p

      }


      if ('detections' %in% plot_type[i]) {
        # ------------------------------------------------------------
        # Current area
        # ------------------------------------------------------------

        current_area <- areas[areas[[areaID]] == id, ]

        # ------------------------------------------------------------
        # Species column
        # ------------------------------------------------------------

        if (!('species' %in% names(data))) {

          if ('common_name' %in% names(data)) {

            names(data)[which(names(data) == 'common_name')] <- 'species'

          } else if ('scientific_name' %in% names(data)) {

            names(data)[which(names(data) == 'scientific_name')] <- 'species'

          } else if ('scientificName' %in% names(data)) {

            names(data)[which(names(data) == 'scientificName')] <- 'species'
          }
        }

        # ------------------------------------------------------------
        # Detection counts
        # ------------------------------------------------------------

        if ('species' %in% names(data)) {

          n_detections <- data |>
            group_by(geometry, species) |>
            mutate(n_collections = n()) |>
            ungroup() |>
            st_as_sf()
        }

        # ------------------------------------------------------------
        # Find control buffer if one was supplied
        # ------------------------------------------------------------

        control_detections <- NULL
        selected_buffer <- NULL

        habitat_area <- current_area




        original_data <- get("data", envir = parent.frame())

        if (!is.null(control_polygon)) {

          control_polygon <- control_polygon |>
            dplyr::filter(NAME_E == id) |>
            dplyr::mutate(
              buffer_order = match(
                buffer_distance,
                c(
                  "twenty_km",
                  "forty_km",
                  "sixty_km",
                  "eighty_km"
                )
              )
            ) |>
            dplyr::arrange(buffer_order)


          for (k in seq_len(nrow(control_polygon))) {

            test_polygon <- control_polygon[k, ]

            test_data <- original_data |>
              sf::st_join(
                test_polygon,
                left = FALSE
              )

            if (nrow(test_data) > 0) {

              control_polygon <- test_polygon
              control_detections <- test_data
              selected_buffer <- test_polygon$buffer_distance

              break
            }
          }

            if (!is.null(control_detections)) {

              habitat_area <- sf::st_union(
                current_area,
                control_polygon
              ) |>
                sf::st_as_sf()

            }


        }

        # ------------------------------------------------------------
        # Create one plot for each habitat display
        # ------------------------------------------------------------

        habitat_plots <- list()

        if (length(habitat_display) > 0) {

          for (j in seq_along(habitat_display)) {

            message(names(habitat_display)[j])

            habitat <- habitat_display[[j]]

            print(
              paste0(
                "Processing habitat_display[[", j, "]] of ",
                length(habitat_display),
                " | class: ",
                paste(class(habitat), collapse = ", ")
              )
            )

            # Name of the habitat variable
            habitat_name <- names(habitat_display)[j]

            # ----------------------------------------------------------
            # Start a NEW plot for this habitat
            # ----------------------------------------------------------


            d <- ggplot() +
              geom_sf(
                data = current_area, # HERE JAIM
                fill = "white",
                color = "black"
              ) +
              labs(
                title = habitat_name
              )

            # Track whether there is overlap
            has_overlap <- FALSE

            # ----------------------------------------------------------
            # Habitat display is an sf object
            # ----------------------------------------------------------

            if (inherits(habitat, "sf")) {

              habitat <- st_transform(
                habitat,
                st_crs(current_area)
              )

              habitat <- st_make_valid(habitat)

              habitat_overlap <- habitat[
                lengths(
                  st_intersects(
                    habitat,
                    habitat_area
                  )
                ) > 0,
              ]

              if (nrow(habitat_overlap) > 0) {

                has_overlap <- TRUE

                d <- d +
                  geom_sf(
                    data = habitat_overlap,
                    aes(fill = Class_name),
                    colour = "black",
                    linewidth = 0.2
                  ) +
                  scale_fill_discrete(
                    name = habitat_name
                  )
              }

              # ----------------------------------------------------------
              # Habitat display is a SpatRaster
              # ----------------------------------------------------------

            } else if (inherits(habitat, "SpatRaster")) {

              print(
                paste0(
                  "Converting raster to polygons: ",
                  terra::ncell(habitat),
                  " cells"
                )
              )

              habitat_poly <- terra::as.polygons(
                habitat,
                dissolve = FALSE,
                na.rm = TRUE
              )

              habitat_poly <- sf::st_as_sf(habitat_poly)

              habitat_poly <- sf::st_transform(
                habitat_poly,
                sf::st_crs(current_area)
              )

              habitat_overlap <- habitat_poly[
                lengths(
                  sf::st_intersects(
                    habitat_poly,
                    habitat_area
                  )
                ) > 0,
              ]

              if (nrow(habitat_overlap) > 0) {

                has_overlap <- TRUE

                value_col <- setdiff(
                  names(habitat_overlap),
                  attr(
                    habitat_overlap,
                    "sf_column"
                  )
                )[1]

                d <- d +
                  geom_sf(
                    data = habitat_overlap,
                    aes(fill = .data[[value_col]]),
                    colour = NA,
                    alpha = 0.6
                  )
              }
            }

            # ----------------------------------------------------------
            # If no habitat overlap, make this obvious
            # ----------------------------------------------------------

            if (!has_overlap) {

              d <- d +
                annotate(
                  "text",
                  x = Inf,
                  y = Inf,
                  label = "No overlap",
                  hjust = 1.1,
                  vjust = 1.5,
                  size = 5
                )
            }

            # ----------------------------------------------------------
            # Add control buffer if one was found
            # ----------------------------------------------------------

            if (!is.null(control_detections)) {

              d <- d +
                geom_sf(
                  data = control_polygon,
                  fill = NA,
                  color = "grey40",
                  linewidth = 0.8,
                  linetype = "dashed"
                )
            }

            # ----------------------------------------------------------
            # Put area boundary on top
            # ----------------------------------------------------------

            d <- d +
              geom_sf(
                data = current_area,
                fill = NA,
                color = "black",
                linewidth = 0.8
              )

            # ----------------------------------------------------------
            # Add detections
            # ----------------------------------------------------------

            if ('species' %in% names(data)) {

              d <- d +
                geom_sf(
                  data = n_detections,
                  aes(
                    geometry = geometry,
                    size = n_collections,
                    colour = species
                  ),
                  alpha = 0.6,
                  show.legend = c(
                    size = FALSE,
                    colour = (j == 1)
                  )
                )

              # --------------------------------------------------------
              # Add control detections
              # --------------------------------------------------------

              if (!is.null(control_detections)) {

                d <- d +
                  geom_sf(
                    data = control_detections,
                    aes(
                      geometry = geometry,
                      colour = species
                    ),
                    alpha = 0.6,
                    show.legend = FALSE
                  )
              }

              d <- d +
                labs(
                  colour = "Species",
                  subtitle = if (!is.null(selected_buffer)) {
                    paste(
                      "Control buffer:",
                      gsub("_", " ", selected_buffer)
                    )
                  } else {
                    NULL
                  }
                ) +
                theme_minimal() +
                theme(
                  legend.position = "right"
                )
            }

            # ----------------------------------------------------------
            # Store this habitat plot
            # ----------------------------------------------------------

            habitat_plots[[j]] <- d
          }

        } else {

          # ------------------------------------------------------------
          # No habitat displays -- make one normal detections plot
          # ------------------------------------------------------------

          d <- ggplot() +
            geom_sf(
              data = current_area,
              fill = "white",
              color = "black"
            )

          # ------------------------------------------------------------
          # Add control buffer
          # ------------------------------------------------------------

          if (!is.null(control_detections)) {

            d <- d +
              geom_sf(
                data = control_polygon,
                fill = NA,
                color = "grey40",
                linewidth = 0.8,
                linetype = "dashed"
              )
          }

          # ------------------------------------------------------------
          # Area boundary
          # ------------------------------------------------------------

          d <- d +
            geom_sf(
              data = current_area,
              fill = NA,
              color = "black",
              linewidth = 0.8
            )

          # ------------------------------------------------------------
          # Add detections
          # ------------------------------------------------------------

          if ('species' %in% names(data)) {

            d <- d +
              geom_sf(
                data = n_detections,
                aes(
                  geometry = geometry,
                  size = n_collections,
                  colour = species
                ),
                alpha = 0.6,
                show.legend = c(
                  size = FALSE,
                  colour = TRUE
                )
              )

            # ----------------------------------------------------------
            # Add control detections
            # ----------------------------------------------------------

            if (!is.null(control_detections)) {

              d <- d +
                geom_sf(
                  data = control_detections,
                  aes(
                    geometry = geometry,
                    colour = species
                  ),
                  alpha = 0.6,
                  show.legend = FALSE
                )
            }

            d <- d +
              labs(
                colour = "Species",
                subtitle = if (!is.null(selected_buffer)) {
                  paste(
                    "Control buffer:",
                    gsub("_", " ", selected_buffer)
                  )
                } else {
                  NULL
                }
              ) +
              theme_minimal() +
              theme(
                legend.position = "right"
              )
          }

          habitat_plots[[1]] <- d
        }

        # ------------------------------------------------------------
        # Automatically arrange habitat plots
        # ------------------------------------------------------------

        n_plots <- length(habitat_plots)

        n_cols <- ceiling(sqrt(n_plots))
        n_rows <- ceiling(n_plots / n_cols)

        plot_list[[length(plot_list) + 1]] <- patchwork::wrap_plots(
          habitat_plots,
          ncol = n_cols,
          nrow = n_rows
        )
      }

      if("boxplot" %in% plot_type[i]) {
        # Create decade grouping
        data$decade_group <- floor(data[[year]] / bin_width) * bin_width

        # Plot with position_dodge to control width
        est_year <- areas$date_of_establishment[areas$NAME_E == id]
        year_range <- range(data[[year]], na.rm = TRUE)

        plot_list[[i]] <- ggplot(data, aes(x = decade_group + bin_width/2,
                         y = .data[[indicator_var_name]],
                         group = decade_group)) +
          geom_boxplot(width = bin_width * 0.9) +
          {
            if (length(est_year) > 0 &&
                !is.na(est_year) &&
                est_year >= year_range[1] &&
                est_year <= year_range[2]) {

              geom_vline(xintercept = est_year,
                         colour = "red",
                         linewidth = 1)
            }
          } +
          scale_x_continuous(
            name = year,
            breaks = unique(data$decade_group),
            minor_breaks = NULL
          ) +
          theme_classic()
      }
      if("map" %in% plot_type[i]){
        idx <- areas[[areaID]] == id

        if (length(idx) == 0 || !any(idx)) {
          # Non_Conservation_Area
          next
        }

        if (any(grepl("SpatRaster", class(data)))) {

          plot_list[[i]] <-
            ggplot2::ggplot() +
            tidyterra::geom_spatraster(
              data = data
            ) +
            ggplot2::geom_sf(
              data = areas[which(areas[[areaID]] == id), ],
              fill = NA,
              colour = "red",
              linewidth = 1
            ) +
            ggplot2::theme_minimal()

          next
        }

        if ((!("sf" %in% class(data)))) {
          aes_geom <- st_as_sf(data)[[attr(st_as_sf(data), "sf_column")]]
        } else {
          if ("geoms" %in% names(data)) {
            aes_geom <- data$geoms
          } else {
            aes_geom <- data$geometry
          }

        }

        # if (id == MPAs$NAME_E[9]) {
        #   browser()
        # }


        if (all(st_is_empty(aes_geom))) {
          plot_list[[i]] <- NULL
        } else {
          if (any(grepl("control site", unique(scoring), ignore.case = TRUE))) {
            coords <- st_coordinates(aes_geom)
            d_coords <- cbind(data, coords)

            plot_list[[i]] <- ggplot() +
              geom_sf(
                data = areas[areas[[areaID]] == id,],
                fill = "white",
                color = "black"
              ) +
              # Main spatial layer without contributing to shape legend
              geom_sf(
                data = data,
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

            # External data buffer
            if (!is.null(externalData) && !all(externalData == 0)) {

              area_idx <- which(areas[[areaID]] == id)

              buffer_distance <- externalData[area_idx]

              if (!is.na(buffer_distance) && buffer_distance > 0) {

                area_polygon <- areas[area_idx, ]

                # Transform to a projected CRS in metres
                area_polygon <- sf::st_transform(area_polygon, 6933)

                # Create buffer in metres
                external_buffer <- sf::st_buffer(
                  area_polygon,
                  dist = buffer_distance * 1000
                )

                # Transform back to the original CRS
                external_buffer <- sf::st_transform(
                  external_buffer,
                  sf::st_crs(areas)
                )

                # Draw buffer
                d <- ggplot() +
                  geom_sf(
                    data = external_buffer,
                    fill = NA,
                    color = "blue",
                    linetype = "dashed",
                    linewidth = 0.8
                  ) +
                  labs(
                    title = paste0(externalData[area_idx], " km buffer")
                  )+
                  theme_classic() +
                  geom_sf(data = areas[areas[[areaID]] == id,], fill = "white", color = "black")
              }
            } else {
              d <- ggplot() +
              geom_sf(data = areas[areas[[areaID]] == id,], fill = "white", color = "black")
            }


             if (!(all(is.na(control_polygon)))) {
                ctrl_40 <- control_polygon_out[
                  control_polygon_out$buffer_distance == "forty_km" &
                    control_polygon_out$NAME_E == areas[areas[[areaID]] == id,]$NAME_E,
                ]

                # 🔴 keep only control points inside the polygon
                if (any(control_nesteddata$areaID %in% areas$NAME_E[areas[[areaID]] == id])) {
                  if (!(is.null(control_nesteddata$data[control_nesteddata$areaID == areas[areas[[areaID]] == id,]$NAME_E][[1]]))) {
                ctrl_points <-control_nesteddata$data[control_nesteddata$areaID == areas[areas[[areaID]] == id,]$NAME_E][[1]]

                ctrl_points <- sf::st_as_sf(ctrl_points)


                # 🔴 plot the control points (red)
                d <- d +   geom_sf(
                  data = ctrl_40,
                  aes(color = "Outside comparison")
                ) +
                  scale_color_manual(
                    name = NULL,
                    values = c("Outside comparison" = "red")
                  ) +
                  geom_sf(
                  data = ctrl_points,
                  color = "red",
                  size = 2,
                  shape = 21
                )
                }
                }


             }


vals <- data[[indicator_var_name]][is.finite(data[[indicator_var_name]]) & data[[indicator_var_name]] > 0]
use_log <- length(vals) > 0 && (max(vals) / min(vals) > 100)


if (use_log) {
  d <- d +
    scale_fill_viridis_c(trans = "log10",
                         name = paste0(indicator_var_name, "\n(log10 scale)"))
} else {
  d <- d +
    scale_fill_viridis_c()

}


if (any(grepl("sf", class(data)))) {
  if (any(sf::st_geometry_type(data) %in% c("LINESTRING", "MULTILINESTRING"))) {
    d <- d + geom_sf(
      data = data,
      aes(geometry = aes_geom, color = .data[[indicator_var_name]]),
      size = 3
    )
  } else {
    d <- d + geom_sf(
      data = data,
      aes(geometry = aes_geom, fill = .data[[indicator_var_name]]),
      shape = 21,
      size = 3
    )
  }
} else {
  d <- d + geom_sf(
    data = data,
    aes(geometry = aes_geom, fill = .data[[indicator_var_name]]),
    shape = 21,
    color = "black",
    size = 0.5
  )
}

              d <- d +
              theme_classic() +
              theme(
                plot.title = element_text(size = 10),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
              ) +
              coord_sf(crs = st_crs(areas))

              plot_list[[i]] <- d
          }
        }

        plot_list[[i]] <- plot_list[[i]] +
               geom_sf(
                    data = areas[areas[[areaID]] == id, ],
                    fill = NA,
                    color = "grey",
                    linewidth = 0.8
               )
      }
      if ("map-species" %in% plot_type[i]) {
        # Choose fill column and legend title
        if ("subclass" %in% names(data)) {
          fill_col <- "subclass"
          legend_title <- "Subclass"
        } else {
          fill_col <- "scientificName"
          legend_title <- "Species"
        }

        plot_list[[i]] <- ggplot() +
          geom_sf(data = areas[areas[[areaID]] == id,],
                  fill = "white",
                  color = "black") +
          geom_sf(data = data,
                  aes(fill = .data[[fill_col]]),
                  shape = 21,
                  color = "black",
                  size = 2) +
          theme_classic() +
          labs(fill = legend_title, title = id) +
          theme(
            plot.title = element_text(size = 10),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
          ) +
          coord_sf(crs = st_crs(areas))

      }

      if ("region-mpa-comparison" %in% plot_type[i]) {
        full_data <- get("data", envir = parent.frame())

        library(dplyr)
        library(ggplot2)

        # MPA NIS
        mpa_nis <- data %>%
          distinct(scientificName) %>%
          mutate(
            count = 1,
            source = "MPA"
          )

        # Regional NIS
        region_nis <- full_data %>%
          distinct(scientificName) %>%
          mutate(
            count = 1,
            source = "Region"
          )

        # Combine
        nis_summary <- bind_rows(
          mpa_nis,
          region_nis
        )

        plot_list[[i]] <- ggplot(
          nis_summary,
          aes(
            x = source,
            y = count,
            fill = scientificName
          )
        ) +
          geom_col() +
          theme_bw() +
          labs(
            x = NULL,
            y = "Number of non-indigenous species",
            fill = "Species"
          )
      }

      if ("indicator_by_taxa" %in% plot_type[i]) {
        data <- data |>
          mutate(year_of_data_collection = as.integer(year_of_data_collection))

        plot_list[[i]] <- ggplot(
          data,
          aes(
            x = year_of_data_collection,
            y = detections,
            fill = species
          )
        ) +
          geom_col() +
          #facet_wrap(~ location) +
          scale_x_continuous(
            breaks = sort(unique(data$year_of_data_collection))
          ) +
          labs(
            x = "Year",
            y = "Detections",
            fill = "Species"
          ) +
          theme_minimal() +
          theme(
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9),
            legend.key.size = unit(0.5, "cm")
          )

      }

      if ('water column profile' %in% plot_type[i]) {
        plot_list[[i]] <- ggplot(data, aes(
          x = .data[[indicator_var_name]],
          y = depth,
          colour = year_of_data_collection
        )) +
          geom_point() +
          scale_y_reverse() +
          theme_minimal() +
          labs(
            x = indicator_var_name,
            y = "Depth (m)",
            colour = "Year"
          )

      }
    }

    if(length(plot_list)==0){
      return(p)
    } else {
      p <- try(patchwork::wrap_plots(plot_list, ncol = min(length(plot_type), 3)), silent=TRUE)

      if (inherits(p, "try-error")) {
        p <- try(patchwork::wrap_plots(plot_list[!vapply(plot_list, is.null, logical(1))], ncol = min(length(plot_type), 3)), silent=TRUE)
      }
      return(p)
    }



  } else {
    return(p)
  }
}
