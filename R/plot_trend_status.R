#' Plot Indicator Trends
#'
#' This function plots the trends of indicators. It also provides the values
#' of indicators for each year that exists
#'
#' @param mpa an object of class "sf", likely from data_CPCAD_areas that contains
#' the name and geoms of the protected areas
#' @param df data frame either from a getFunction specified in dataTable or from
#' a tar_load that includes latitude, longitude, year, type, and the parameter
#' to average
#' @param area area of interest (from the NAME_E column of MPAs)
#' @param type surface or bottom
#' @param dataframe FALSE a Boolean indicating if a data frame is returned or not
#' @param parameter a character indicating which parameter to measure
#' @param outside a Boolean indicating if an outside comparison is happening
#' @param map a Boolean indicating if the latitude, longitude, boundary, and outside boundary
#' should be returned. This is likely used for plotting purposes
#' @importFrom azmpdata Discrete_Occupations_Sections
#' @importFrom sf st_within st_as_sf
#' @importFrom dplyr slice_max ungroup group_by
#' @return
#' @export
#'
#' @examples
plot_trend_status <- function(df=NULL, mpa=NULL, area="Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)", type="surface",
                              dataframe=FALSE, parameter="temperature",outside=FALSE, map=FALSE) {

  # Derived_Monthly_Stations
  multipolygon <- mpa$geoms[which(mpa$NAME_E == area)]
  if (!(parameter == "bloom_amplitude")) {
  points_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  inside <- sf::st_within(points_sf, multipolygon, sparse = FALSE)

  # Filter points that are inside the polygon
  points_inside <- points_sf[inside, ]
  }

  if (area == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)") {

    Outside <- st_transform(read_sf(system.file("data","WEBCA_10k_85k.shp", package = "MarConsNetAnalysis"))$geometry, crs=4326)


    #Outside <- st_transform(read_sf("../WesternEmerald_CSAS_2025/data/WEBCA_10k_85k.shp")$geometry, crs=4326)
  } else {
    stop("Must code in other buffers.")
  }

  outside_exclusive_multipolygon <- sf::st_difference(Outside, multipolygon)


  # OUTSIDE BUFFER
  if (outside) {
    if (!(parameter == "bloom_amplitude")) {

    inside <- sf::st_within(points_sf, outside_exclusive_multipolygon, sparse = FALSE)

    # Filter points that are inside the polygon
    points_inside <- points_sf[inside, ]
    } else {
      message("No outside comparison available.")
    }
  }
  if (!(parameter == "bloom_amplitude")) {
  if (any(inside[,1])) {
    keep <- df[which(inside[,1]),]
  } else {
    # None in, but find the closest
    latitude <- df$latitude
    longitude <- df$longitude
    points <- sf::st_as_sf(data.frame(latitude,longitude),
                           coords = c("longitude", "latitude"),
                           crs = 4326)  # WGS84 CRS
    distances <- st_distance(points, multipolygon)
    closest_index <- apply(distances, 1, which.min)
    closest_distance <- apply(distances, 1, min)
    closest_point <- unique(points[closest_index, ])
    closest_coordinates <- st_coordinates(closest_point)
    station <- unique(df$station[which(df$latitude %in% unname(closest_coordinates[,2]) & df$longitude %in% unname(closest_coordinates[,1]))])
    keep <- df[which(df$station == station),]
  }
  } else {
    df$date <- df$year
    keep <- df
  }

  if (!("date" %in% names(df))) {
    if ("month" %in% names(df)) {
      keep$date <- as.Date(paste(keep$year, keep$month, "1", sep = "-"), format = "%Y-%m-%d")
    } else {
      keep$date <- as.POSIXct(paste0(keep$year, "-01-01"), format = "%Y-%m-%d")
    }
  }

  keep <- keep[order(keep$date),]

  if (parameter %in% names(azmpdata::Discrete_Occupations_Sections)) {
    if (type=="surface") {
      keep <- keep[which(keep$depth < 5),]
    } else if (type == "bottom") {
      keep <- keep %>%
        dplyr::group_by(date) %>%
        dplyr::slice_max(depth, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup()
    }
  }

  if (!("year" %in% names(keep))) {
  keep$year <- format(keep$date, "%Y")
  }

  grouped_list <- split(keep, keep$year)
  if (!(parameter %in% c("Zooplankton", "fish_weight", "fish_length", "haddock_biomass", "haddock_abundance", "whale_biodiversity"))) {
    yearly_avg <- sapply(grouped_list, function(df) mean(df[[parameter]], na.rm=TRUE))
  } else if (parameter == "Zooplankton") {
    yearly_avg <- NULL
    for (i in seq_along(grouped_list)) {
      l <- as.data.frame(grouped_list[[i]])
      ll <- l[which(grepl("log10", names(l), ignore.case=TRUE))]
      yearly_avg[[i]] <- sum(unname(unlist(ll)), na.rm=TRUE)
    }
    } else if (parameter %in% c("fish_weight", "fish_length")) {
      yearly_avg <- NULL
      for (i in seq_along(grouped_list)) {
        l <- as.data.frame(grouped_list[[i]])
        yearly_avg[[i]] <-  ifelse(parameter=="fish_weight", mean(l$FWT,na.rm=TRUE), mean(l$FLEN, na.rm=TRUE))
      }
    } else if (parameter %in% c("haddock_abundance", "haddock_biomass")) {
      yearly_avg <- NULL
      for (i in seq_along(grouped_list)) {
        l <- as.data.frame(grouped_list[[i]])
        yearly_avg[[i]] <-  ifelse(parameter=="haddock_abundance", mean(l$TOTNO,na.rm=TRUE), mean(l$TOTWGT, na.rm=TRUE))
      }
    } else if (parameter == "bloom_amplitude") {
      yearly_avg <- NULL
      for (i in seq_along(grouped_list)) {
        l <- as.data.frame(grouped_list[[i]])
        yearly_avg[[i]] <-  mean(l$bloom_amplitude,na.rm=TRUE)
      }


    } else if (parameter == "whale_biodiversity") {
      yearly_avg <- NULL
      for (i in seq_along(grouped_list)) {
        l <- as.data.frame(grouped_list[[i]])
        yearly_avg[[i]] <-  length(unique(grouped_list[[i]]$species_name))
      }


    }
    names(yearly_avg) <- names(grouped_list)
    yearly_avg <- unlist(yearly_avg)

  # Convert to a data frame for plotting
  plot_data <- data.frame(
    year = as.numeric(names(yearly_avg)),  # Convert year to numeric
    avg_parameter = yearly_avg,
    parameter_name=rep(parameter),
    type=ifelse(!(is.null(type)), rep(type), "")
  )

  if (dataframe & (!(map))) {
    return(plot_data)
  }
  # Base R plot

  if (!(dataframe) & !(map)) {
  plot(
    plot_data$year, plot_data$avg_parameter,
    type = "b",                    # Line and points
    col = "blue",                  # Line color
    pch = 19,                      # Point style (solid circle)
    xlab = "Year",                 # Label for x-axis
    ylab = ifelse(!(is.null(type)), paste0("Average ", parameter),ifelse(type=="surface", paste0("Average Surface ",parameter), paste0("Average Bottom ", parameter)))
  )
  }

  if (map) {
    if (!(parameter == "bloom_amplitude")) {
      inside2 <- which(sf::st_within(points_sf, outside_exclusive_multipolygon, sparse = FALSE)[,1])
      insideKeep <- unique(c(inside2, which(inside[,1])))

      latitude <- df$latitude[insideKeep]
      longitude <- df$longitude[insideKeep]



      if (length(df$latitude[insideKeep]) > 1000) {
      latitude <- round(latitude,1)
      longitude <- round(longitude,1)
      coord <- data.frame(latitude, longitude)

      # Get unique pairs
      unique_coords <- unique(coord)
      latitude <- unique_coords$latitude
      longitude <- unique_coords$longitude

      }

      mapdf <- list(
        latitude=latitude,
        longitude=longitude,
        area=multipolygon,
        outside=outside_exclusive_multipolygon
      )
    } else {
      st_crs(df$geom) <- 4326
      geometry <- st_transform(df$geom, st_crs(multipolygon))
      geometry <- st_make_valid(geometry)
      overlaps_poly <- st_intersects(geometry, multipolygon, sparse = FALSE)


      sf_df <- st_sf(df, geometry = geometry)
      geometry <- unique(geometry[which(overlaps_poly[,1])])[[1]]

      geometry_sf <- st_sfc(geometry)
      multipolygon_sf <- st_sfc(multipolygon)
      outside_sf <- st_sfc(outside_exclusive_multipolygon)

      # Create the data frame with sf objects
      # mapdf <- data.frame(
      #   geom = geometry_sf,
      #   area = multipolygon_sf,
      #   outside = outside_sf
      # )

      mapdf <- list(
        geom=geometry_sf,
        area=multipolygon_sf,
        outside=outside_sf
      )
    }

    return(mapdf)

  }

  # Optional: Add a grid for better readability
  grid()
}

