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
#' @importFrom sf st_within st_as_sf st_crs
#' @importFrom dplyr slice_max ungroup group_by
#' @return
#' @export
#'
#' @examples
plot_trend_status <- function(df=NULL, mpa=NULL, area="Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)", type=NULL,
                              dataframe=FALSE, outside=FALSE, map=FALSE) {

  # Derived_Monthly_Stations
  multipolygon <- mpa$geoms[which(mpa$NAME_E == area)]

  if (area == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)") {
    Outside <- st_transform(read_sf(system.file("data","WEBCA_10k_85k.shp", package = "MarConsNetAnalysis"))$geometry, crs=4326)
  } else {
    stop("Must code in outside buffer for outside comparison in this area")
  }

  outside_exclusive_multipolygon <- sf::st_difference(Outside, multipolygon)

  if (!("geom" %in% names(df))) {
    points_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
    inside <- sf::st_within(points_sf, multipolygon, sparse = FALSE)

    # Filter points that are inside the polygon
    points_inside <- points_sf[inside, ]
  } else {
    polys <- unique(df$geom)
    if (outside) {
      overlap <- lapply(polys, function(x) st_overlaps(x, Outside))
    } else {
      overlap <- lapply(polys, function(x) st_overlaps(x, multipolygon))

    }
    yon <- NULL # 1 means overlap
    for (i in seq_along(overlap)) {
      yon[[i]] <- overlap[[i]][[1]]
    }
    polyKeep <- polys[which(yon == 1)]

    keepers <- vector("list", length(df$geom))
    for (i in seq_along(df$geom)) {
      dfk <- unlist(df$geom[i])
      for (j in seq_along(polyKeep)) {
        pg <- unlist(polyKeep[[j]])
        if (identical(dfk,pg)) {
          keepers[[i]][[j]] <- i
        }
      }
    }

    keepers <- unlist(keepers)

    inside <-  matrix(FALSE, nrow = length(df$type), ncol = 1)
    inside[,1][unlist(keepers)] <- TRUE
  }

  # OUTSIDE BUFFER
  if (outside) {
    if (!("geom" %in% names(df))) {
      inside <- sf::st_within(points_sf, outside_exclusive_multipolygon, sparse = FALSE)
      if (!(any(inside[,1]) == 0)) {
        # Filter points that are inside the polygon
        points_inside <- points_sf[inside, ]
      } else {
        stop("No outside comparison available.")
      }
    }
  }
  if (any(inside[,1])) {
    keep <- df[which(inside[,1]),]
  } else {
    if (!("geom" %in% names(df))) {
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
      latKeep <- which(df$latitude %in% unname(closest_coordinates[,2]) & df$longitude %in% unname(closest_coordinates[,1]))
      keep <- df[(latKeep),]
    }
  }

  keep <- keep[order(keep$year),]

  if (!(is.null(type))) {
    if (type=="surface" && "depth" %in% names(df)) {
      keep <- keep[which(keep$depth < 5),]
    } else if (type == "bottom") {
      keep <- keep %>%
        dplyr::group_by(date) %>%
        dplyr::slice_max(depth, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup()
    }
  }


  NAMES <- names(df)
  parameter <- NAMES[which(!(NAMES %in% c('latitude', "longitude", "type", "geom", "year", "units", "species_name", "area", "depth")))]

  grouped_list <- split(keep, keep$year)
  if (!("whale_biodiversity" %in% names(df))) {
    yearly_avg <- sapply(grouped_list, function(df) mean(df[[parameter]], na.rm=TRUE))
  } else {
    yearly_avg <- sapply(grouped_list, function(df) length(unique(df$whale_biodiversity)))

  }

  names(yearly_avg) <- names(grouped_list)
  yearly_avg <- unlist(yearly_avg)

  # Convert to a data frame for plotting
  if (!(dataframe)) {
    plot_data <- data.frame(
      year = as.numeric(names(yearly_avg)),  # Convert year to numeric
      avg_parameter = unname(yearly_avg),
      parameter_name=rep(parameter),
      type=ifelse(!(is.null(type)), rep(type), ""),
      units=unique(df$units)
    )
  } else {
    plot_data <- keep
  }

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
      ylab = ifelse(is.null(type), paste0("Average ", parameter, " (", unique(plot_data$units), ")"), paste0(ifelse(type=="surface", paste0("Average Surface ",parameter), paste0("Average Bottom ", parameter)), " (", unique(plot_data$units, ")")))
    )
  }

  if (map) {
    if (!("geom" %in% names(df))) {
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
      sf::st_crs(df$geom) <- 4326
      geometry <- st_transform(df$geom, sf::st_crs(multipolygon))
      geometry <- st_make_valid(geometry)
      overlaps_poly <- st_intersects(geometry, multipolygon, sparse = FALSE)


      sf_df <- st_sf(df, geometry = geometry)
      geometry <- unique(geometry[which(overlaps_poly[,1])])[[1]]

      geometry_sf <- st_sfc(geometry)
      multipolygon_sf <- st_sfc(multipolygon)
      outside_sf <- st_sfc(outside_exclusive_multipolygon)

      mapdf <- list(
        geom=geometry_sf,
        area=multipolygon_sf,
        outside=outside_sf
      )
    }

    return(mapdf)

  }
}
