#' Calculate species abundance from the RV survey
#'
#' This function calculates the average species catch per standardized tow
#' to 1.75 km for each year.
#' @param species character indicating which species to analyze in the RV
#' @param area area of interest for species
#' @param MPAs an object of class "sf", likely from data_CPCAD_areas that contains
#' the name and geoms of the protected areas
#' @param data GSSPECIES from get_data in the Mar.datawrangling package
#' @return
#' \dontrun{
#' rva <- ind_rv_abundance()
#' }
#' @export
#'
#' @examples

ind_rv_abundance <- function(species="HADDOCK", area="Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)", MPAs=NULL, data=NULL) {

  if (is.null(MPAs)) {
    stop("Must provide an MPAs argument.")
  }

  if (is.null(data)) {
    stop("Must provide an data argument.")
  }

  # FIXME:

  if (area == "st_Anns_Bank_MPA") {
    area <- "St. Anns Bank Marine Protected Area"

  } else if (area == "musquash_MPA") {
    area <- "Musquash Estuary Marine Protected Area"

  } else if (area == "gully_MPA") {
    area <- "Gully Marine Protected Area"

  } else if (area == "WEBCA") {
    area <- "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)"
  } else {
    stop("In ind_rv_abundance. This area doesn't exist yet.")
  }

  #get_data('rv', data.dir="C:/Users/HarbinJ/Documents/data/rv")
  GSSPECIES <- GSSPECIES[which(GSSPECIES$COMM == species),]
  self_filter(keep_nullsets = F)

  test<- summarize_catches()

  latitude <- round(test$LATITUDE,2)
  longitude <- round(test$LONGITUDE,2)

  bioregion <- data_bioregion()
  if (!(area == "All")) {
  protectedArea <- MPAs$geoms[which(MPAs$NAME_E == area)]

  coords <- cbind(longitude, latitude)

  # Convert to sf object
  points_sf <- st_as_sf(data.frame(coords), coords = c("longitude", "latitude"), crs = st_crs(4326))

  points_within <- st_within(points_sf, protectedArea, sparse = FALSE)
  } else {
    points_within <- matrix(TRUE, length(test$MISSION), 1, byrow = FALSE, dimnames = NULL)
  }
  #within_points <- points_sf[apply(points_within, 1, any), ]

  # Now keeping haddock info
  SPEC <- test[which(points_within[,1]),]
  SPEC <- SPEC[which(SPEC$COMM == species),]

  if (!(length(SPEC$MISSION) == 0)) {
    SPEC$Standardized_TOTAL <- (SPEC$TOTNO / SPEC$DIST) * 1.75 # STANDARDIZE DISTANCE
    years <- sort(unique(SPEC$YEAR))
    average_by_year <- data.frame(year=years)
    average_by_year$abundance <- 0

    for (i in seq_along(years)) {
      keep <- which(SPEC$YEAR == years[i])
      average_by_year$abundance[i] <- round(sum(SPEC$Standardized_TOTAL[keep])/length(keep),0)
    }
    average_by_year$species <- rep(species, length(average_by_year$abundance))

  } else {
    return(paste("No subset for species = ", species, " and area = ", area))
  }

  return(average_by_year)
}
