#' Plot Indicator
#'
#' @inheritParams process_indicator
#' @param data data processed in `assess_indicator()`
#' @inherit process_indicator params indicator units plot_type year indicator_var_name scoring areaID areas bin_width
#' @param id Equivalent to `areaID` in [process_indicator()]
#'
#' @returns a plot
#' @export
#'
#' @examples
plot_indicator <- function(data,indicator,units,id, plot_type, year, indicator_var_name, scoring, areaID, areas, bin_width){
  p <- NULL

  if(!is.null(data)){
    plot_list <- list()

    for (i in seq_along(plot_type)) {

      par(mar = c(4, 4.5, 0.5, 1))

      if ('map-species' %in% plot_type[i]) {
        #browser()
        # if (!(length(data[[which(areas$NAME_E == id)]][[indicator_var_name]]) > 25)) {
        #   plot_type <- "map"
        # }
      }
      if("time-series" %in% plot_type[i]) {

        plot_list[[i]] <-  ggplot(data,aes(x=.data[[year]], y=.data[[indicator_var_name]]))+
          geom_point()+
          geom_line()+
          theme_classic()+
          ylab(paste0(indicator, " (", units, ")"))
      }
      if("time-series-no-line" %in% plot_type[i]) {
        plot_list[[i]] <-  ggplot(data,aes(x=.data[[year]], y=.data[[indicator_var_name]]))+
          geom_point()+
          theme_classic()+
          ylab(paste0(indicator, " (", units, ")"))
      }
      if("boxplot" %in% plot_type[i]) {
        # Create decade grouping
        data$decade_group <- floor(data[[year]] / bin_width) * bin_width

        # Plot with position_dodge to control width
        plot_list[[i]] <- ggplot(data, aes(x = decade_group + bin_width/2, y=.data[[indicator_var_name]], group = decade_group)) +
          geom_boxplot(width = bin_width*0.9) +
          scale_x_continuous(name = year,
                             breaks = unique(data$decade_group),
                             minor_breaks = NULL) +
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
          plot_list[[i]] <- ggplot(data, aes(x = decade_group + bin_width/2, y = .data[[indicator_var_name]], group = decade_group)) +
            geom_violin(width = bin_width * 0.9) +
            scale_x_continuous(name = year,
                               breaks = unique(data$decade_group),
                               minor_breaks = NULL) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        }


      }
      if("map" %in% plot_type[i]){
        if ((!("sf" %in% class(data)))) {
          aes_geom <- st_as_sf(data)[[attr(st_as_sf(data), "sf_column")]]
        } else {
          if ("geoms" %in% names(data)) {
            aes_geom <- data$geoms
          } else {
            aes_geom <- data$geometry
          }

        }

        if ( st_is_empty(aes_geom) ) {
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
            #browser()


            # Standard case (no control site logic)
            plot_list[[i]] <- ggplot() +
              geom_sf(data = areas[areas[[areaID]] == id,], fill = "white", color = "black") +

              geom_sf(
                data = data,
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
