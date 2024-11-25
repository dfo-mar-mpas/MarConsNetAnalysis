#' Plot Physical AZMP
#'
#' @param mpa an object of class "sf", likely from data_CPCAD_areas that contains
#' the name and geoms of the protected areas
#' @param area area of interest (from the NAME_E column of MPAs)
#' @param type surface or bottom
#' @param dataframe FALSE a Boolean indicating if a data frame is returned or not
#' @param parameter a character indicating which parameter to measure
#' @importFrom azmpdata Discrete_Occupations_Sections
#' @return
#' @export
#'
#' @examples
plot_azmp_physical <- function(mpa=MPAs, area="Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)", type="surface",
                               dataframe=FALSE, parameter="temperature") {
  df <- azmpdata::Discrete_Occupations_Sections
  multipolygon <- mpa$geoms[which(mpa$NAME_E == area)]
  points_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  inside <- st_within(points_sf, multipolygon, sparse = FALSE)

  # Filter points that are inside the polygon
  points_inside <- points_sf[inside, ]

  keep <- df[which(inside[,1]),]
  keep <- keep[order(keep$date),]

  if (type=="surface") {
    keep <- keep[which(keep$depth < 5),]
  } else if (type == "bottom") {
    keep <- keep %>%
      group_by(date) %>%
      slice_max(depth, n = 1, with_ties = FALSE) %>%
      ungroup()
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
    ylab = ifelse(type=="surface", paste0("Average Surface ",parameter), paste0("Average Bottom ", parameter))
  )

  # Optional: Add a grid for better readability
  grid()


}

