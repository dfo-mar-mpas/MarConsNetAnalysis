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
#' @inherit process_indicator params indicator units plot_type year indicator_var_name scoring areaID areas bin_width control_polygon
#' @param id Equivalent to `areaID` in [process_indicator()]
#'
#' @returns a plot
#' @export
#'
#' @examples
plot_indicator <- function(data,indicator,units,id, plot_type, year, indicator_var_name, scoring, areaID, areas, bin_width, control_polygon, control_nesteddata){
  p <- NULL

  if(!is.null(data)){
    plot_list <- list()

    for (i in seq_along(plot_type)) {

      par(mar = c(4, 4.5, 0.5, 1))

      if ('map-species' %in% plot_type[i]) {
        # if (!(length(data[[which(areas$NAME_E == id)]][[indicator_var_name]]) > 25)) {
        #   plot_type <- "map"
        # }
      }

      if("time-series" %in% plot_type[i]) {

          est_year <- MPAs$date_of_establishment[MPAs$NAME_E == id]

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
          combined_data <- rbind(
            inside_data,
            if (!is.null(control_data)) control_data
          )

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
            facet_grid(period ~ location) +
            coord_cartesian(xlim = x_range,
                            ylim = y_range) +
            ylab(paste0(indicator, " (", units, ")"))

          # ---- Add regressions + R² per panel ----
          panel_info <- split(combined_data,
                              list(combined_data$location,
                                   combined_data$period),
                              drop = FALSE)

          #browser()

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
                    location = if (!is.null(df$location)) df$location[1] else "MPA",   # facet
                    period   = if (!is.null(df$period)) df$period[1] else "Before",     # facet
                    x = max_global_x,
                    y = max_global_y
                  ),
                  aes(label = "Insufficient data"),
                  inherit.aes = FALSE,   # very important!
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

          #browser()


          plot_list[[i]] <- p


      }
      if("boxplot" %in% plot_type[i]) {
        # Create decade grouping
        data$decade_group <- floor(data[[year]] / bin_width) * bin_width

        # Plot with position_dodge to control width
        est_year <- MPAs$date_of_establishment[MPAs$NAME_E == id]
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
      if("violin" %in% plot_type[i]) {
        # Create decade grouping
        data$decade_group <- floor(data[[year]] / bin_width) * bin_width

        violin_ok <- data %>%
          group_by(decade_group) %>%
          summarise(n_obs = n()) %>%
          summarise(any_group_has_enough = any(n_obs >= 2)) %>%
          pull(any_group_has_enough)

        if (!(violin_ok)) {
          # violin plot not possible
          plot_list[[i]] <-  ggplot(data,aes(x=.data[[year]], y=.data[[indicator_var_name]]))+
            geom_point()+
            theme_classic()+
            ylab(paste0(indicator, " (", units, ")"))


        } else {

          # Plot with position_dodge to control width
          est_year <- MPAs$date_of_establishment[MPAs$NAME_E == id]

          year_range <- range(data[[year]], na.rm = TRUE)

          plot_list[[i]] <- ggplot(data, aes(x = decade_group + bin_width/2,
                           y = .data[[indicator_var_name]],
                           group = decade_group)) +
            geom_violin(width = bin_width * 0.9) +
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
            theme_classic() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

        }


      }
      if("map" %in% plot_type[i]){

        idx <- areas[[areaID]] == id

        # trigger browser if no match (length 0 or all FALSE)
        if (length(idx) == 0 || !any(idx)) {
          # Non_Conservation_Area
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
              d <- ggplot() +
              geom_sf(data = areas[areas[[areaID]] == id,], fill = "white", color = "black")

             if (!(all(is.na(control_polygon)))) {
                ctrl_40 <- control_polygon[
                  control_polygon$buffer_distance == "forty_km" &
                    control_polygon$NAME_E == areas[areas[[areaID]] == id,]$NAME_E,
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
              d <- d+ geom_sf(
                data = data,
                aes(geometry = aes_geom, fill = .data[[indicator_var_name]]),
                shape = 21,
                color = "black",
                size = 2
              ) +
              theme_classic() +
              theme(
                plot.title = element_text(size = 10),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
              ) +
              coord_sf(crs = st_crs(areas))

              plot_list[[i]] <- d
          }
        }
      }
      if ("map-species" %in% plot_type[i]) {
        plot_list[[i]] <- ggplot() +
          geom_sf(data = areas[areas[[areaID]] == id,], fill = "white", color = "black") +
          geom_sf(data = data, aes(fill = subclass), shape = 21, color = "black", size = 2) +
          theme_classic() +
          labs(fill = "Subclass", title = id) +
          theme(
            plot.title = element_text(size = 10),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
          ) +
          coord_sf(crs = st_crs(areas))

      }
      if ("outside-comparison" %in% plot_type[i]) {
        summary_data <- data %>%
          group_by(.data[[year]], control) %>%
          summarize(mean_value = mean(.data[[indicator_var_name]], na.rm = TRUE), .groups = "drop")

        # Plot with separate lines for inside vs outside
        plot_list[[i]] <- ggplot(summary_data, aes(x = .data[[year]], y = mean_value, color = control)) +
          geom_point() +
          geom_line() +
          theme_classic() +
          ylab(paste0(indicator, " (", units, ")")) +
          scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                             labels = c("Inside MPA", "Outside MPA"),
                             name = "Location")

      }
      if ("community-composition" %in% plot_type[i]) {
        if ("station" %in% names(data)) {
          plot_list[[i]] <- ggplot(data, aes(x = indicator_var_name, y = year, fill = taxa)) +
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
          plot_list[[i]] <- ggplot(data, aes(x = year, y = indicator_var_name, fill = taxa)) +
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
