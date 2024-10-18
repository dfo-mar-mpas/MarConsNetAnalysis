#' Plot leaflet raster of fishing effort
#'
#' FIXME
#'
#' @param area a name of an area of which to obtain the objectives from. Options include
#' `st_Anns_Bank_MPA`, `musquash_MPA`, `gully_MPA`, `WEBCA`
#' @param CPCAD a "sf" "dataframe" object from [data_CPCAD_areas()]
#' @param indicator a Boolean indicating
#' @export
#'
#' @examples
#' \dontrun{
#' bioregion <- data_bioregion()
#' CPCAD <- data_CPCAD_areas(bioregion,  zones = FALSE)
#' plot_fishing_abundance <- function(area="WEBCA", CPCAD=CPCAD)
#' }
#'
#' @return a "sf" "dataframe" object

plot_fishing_abundance <- function(area="WEBCA", CPCAD=NULL, indicator=FALSE) {

  if (!(area %in% c("st_Anns_Bank_MPA", "musquash_MPA", "gully_MPA", "WEBCA"))) { # FIX ME: THIS SHOULD DO ALL PROTECTED AREAS NOT JUST MPAS
    stop("area must be either ", paste0(c("st_Anns_Bank_MPA", "musquash_MPA", "gully_MPA", "WEBCA"), sep+", "))
  }

  if (is.null(CPCAD)) {
    stop("Must provide CPCAD argument")
  }

  context <- data_context(type="site", area=area)


    if (area == "WEBCA") {
      area <- "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)"
    } else if (area == "st_Anns_Bank_MPA") {
        area <- "St. Anns Bank Marine Protected Area"
      } else if (area == "musquash_MPA") {
        area <- "Musquash Estuary Marine Protected Area"
      } else if (area == "gully_MPA") {
        area <- "Gully Marine Protected Area"
      }

    site <- CPCAD$geoms[which(CPCAD$NAME_E == area)]

    if (!st_crs(fishing) == st_crs(site)) {
      fishing <- st_transform(fishing, st_crs(site))
    }


    # WITHIN SITE
    within_indices <- st_within(fishing$geoms, site)
    within_logical <- lengths(within_indices) > 0
    fishing_within_site <- fishing[within_logical, ]

    ## FINDING BUFFER
    line <- context[which(grepl("approximate size", context, ignore.case=TRUE))+1]
    radius_m <- sqrt((as.numeric(gsub(",", "",sub(" km.*", "", line)))/pi))*1000



    # 1. Define a large buffer around webca (buffer outwards)
    initial_buffer <- st_buffer(site, dist = radius_m)  # The radius is an approximation

    # 2. Exclude webca itself from the buffer using st_difference
    buffer_outside_site <- st_difference(initial_buffer, site)

    # 3. Calculate the area of the new buffer
    buffer_area_km2 <- st_area(buffer_outside_site) / 1e6  # Convert from m² to km²

    # 4. Check if the buffer area is larger than 10,234 km², and adjust if necessary
    target_area_km2 <- as.numeric(gsub(",", "",sub(" km.*", "", line)))

    while (as.numeric(buffer_area_km2) > target_area_km2) {
      # Reduce the buffer distance and recalculate
      initial_buffer <- st_buffer(site, dist = 57060 * target_area_km2 / as.numeric(buffer_area_km2))
      buffer_outside_site <- st_difference(initial_buffer, site)
      buffer_area_km2 <- st_area(buffer_outside_site) / 1e6
    }

    # OUTSIDE OF THE SITE

    fishing_within_buffer <- fishing[st_intersects(fishing$geoms, buffer_outside_site, sparse = FALSE), ]


    # raster
    r <- raster(extent(st_bbox(fishing_within_site)), res = 0.01)  # Adjust the resolution as needed
    raster_site <- rasterize(fishing_within_site, r, field = "weight", fun = mean)


    rp <- raster(extent(st_bbox(fishing_within_buffer)), res = 0.01)  # Adjust the resolution as needed
    raster_buffer <- rasterize(fishing_within_buffer, rp, field = "weight", fun = mean)

    range_buffer <- range(values(raster_buffer), na.rm = TRUE)
    range_site <- range(values(raster_site), na.rm = TRUE)

    # Combine the ranges to find the overall range
    combined_range <- range(c(range_buffer, range_site))

    pal <- colorNumeric(
      palette = "viridis",  # You can choose any color palette
      domain = combined_range,
      na.color = "transparent"
    )



    #pal <- colorNumeric(palette = "viridis", domain = range(values(c(ranger(values(raster_site), na.rm=TRUE), range(values(raster_buffer), na.rm=TRUE)))), na.color = "transparent")

    map <- leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_sf(geometry=buffer_outside_site), color = "black", weight = 2, opacity = 1) %>%
      #addPolygons(data = st_sf(geometry=site), color = "white", weight = 2, opacity = 1) %>%
      addRasterImage(raster_buffer, colors = pal, opacity = 0.8)%>%
      addRasterImage(raster_site, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = combined_range,
                title = "Weight",  # Legend title
                labFormat = labelFormat(digits = 2),  # Customize label format
                opacity = 1)
    map

}
