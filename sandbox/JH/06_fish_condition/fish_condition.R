library(Mar.datawrangling)
library(Mar.utils)
get_data('rv', data.dir = "C:/Users/HarbinJ/Documents/data/rv")
GSDET$LATITUDE <- 0
GSDET$LONGITUDE <- 0

missions <- unique(GSDET$MISSION)

for (i in seq_along(missions)) {
  GSDET$LATITUDE[which(GSDET$MISSION == missions[i])] <- GSINF$LATITUDE[which(GSINF$MISSION == missions[i])][1]
  GSDET$LONGITUDE[which(GSDET$MISSION == missions[i])]  <- GSINF$LONGITUDE[which(GSINF$MISSION == missions[i])][1]
}


multipolygon <- MPAs$geoms[which(MPAs$NAME_E == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)")]
points_sf <- sf::st_as_sf(GSDET, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
inside <- sf::st_within(points_sf, multipolygon, sparse = FALSE)

# Filter points that are inside the polygon
points_inside <- points_sf[inside, ]



# OUTSIDE
out <- st_transform(read_sf("../WesternEmerald_CSAS_2025/data/WEBCA_10k_85k.shp")$geometry, crs=4326)
outside_exclusive_multipolyon <- sf::st_difference(out, multipolygon)

outside <- sf::st_within(points_sf, outside_exclusive_multipolyon, sparse = FALSE)

# Filter points that are inside the polygon
points_outside <- points_sf[outside, ]
