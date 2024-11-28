#' Plot Physical AZMP
#'
#' @param mpa an object of class "sf", likely from data_CPCAD_areas that contains
#' the name and geoms of the protected areas
#' @param area area of interest (from the NAME_E column of MPAs)
#' @param type surface or bottom
#' @param dataframe FALSE a Boolean indicating if a data frame is returned or not
#' @param parameter a character indicating which parameter to measure
#' @importFrom azmpdata Discrete_Occupations_Sections
#' @importFrom sf st_within st_as_sf
#' @importFrom dplyr slice_max ungroup group_by
#' @return
#' @export
#'
#' @examples
plot_azmp_physical <- function(mpa=NULL, area="Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)", type="surface",
                               dataframe=FALSE, parameter="temperature") {

  if (parameter %in% names(azmpdata::Discrete_Occupations_Sections)) {
  df <- azmpdata::Discrete_Occupations_Sections
  } else {
    df <- azmpdata::Derived_Monthly_Stations
    # Add latitude and longitude
    df$latitude <- 0
    df$longitude <- 0
    type <- NULL
    df$latitude[which(df$station == "Halifax")] <- 43.5475
    df$longitude[which(df$station == "Halifax")] <- 63.5714

    df$latitude[which(df$station == "Yarmouth")] <- 43.8377
    df$longitude[which(df$station == "Yarmouth")] <- 66.1150

    df$latitude[which(df$station == "North Sydney")] <- 46.2051
    df$longitude[which(df$station == "North Sydney")] <- 60.2563

  }
  # Derived_Monthly_Stations
  multipolygon <- mpa$geoms[which(mpa$NAME_E == area)]
  points_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  inside <- sf::st_within(points_sf, multipolygon, sparse = FALSE)

  # Filter points that are inside the polygon
  points_inside <- points_sf[inside, ]

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

  if (!("date" %in% names(df))) {
    keep$date <- as.Date(paste(keep$year, keep$month, "1", sep = "-"), format = "%Y-%m-%d")
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

  keep$year <- format(keep$date, "%Y")

  grouped_list <- split(keep, keep$year)
  yearly_avg <- sapply(grouped_list, function(df) mean(df[[parameter]], na.rm=TRUE))

  # Convert to a data frame for plotting
  plot_data <- data.frame(
    year = as.numeric(names(yearly_avg)),  # Convert year to numeric
    avg_parameter = yearly_avg
  )

  if (dataframe) {
    return(plot_data)
  }
  # Base R plot
  plot(
    plot_data$year, plot_data$avg_parameter,
    type = "b",                    # Line and points
    col = "blue",                  # Line color
    pch = 19,                      # Point style (solid circle)
    xlab = "Year",                 # Label for x-axis
    ylab = ifelse(!(is.null(type)), paste0("Average ", parameter),ifelse(type=="surface", paste0("Average Surface ",parameter), paste0("Average Bottom ", parameter)))
  )



  # Optional: Add a grid for better readability
  grid()


}

