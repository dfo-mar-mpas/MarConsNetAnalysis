library(azmpdata)
library(sf)
df <- RemoteSensing_Annual_Broadscale

rs <- unique(df$area)
rs <- sub("_.*", "", rs)

# Get polygons from PhytoFit

script_lines <- readLines("https://raw.githubusercontent.com/BIO-RSG/PhytoFit/refs/heads/master/tools/tools_01a_define_polygons.R")

k1 <- which(grepl("poly\\$atlantic = list", script_lines))
k2 <- which(grepl("-61.1957, -61.1957, -59.54983, -59.54983, -61.1957", script_lines))
script <- script_lines[k1:k2]
poly <- list()
eval(parse(text=script))

POLY <- NULL
for (i in seq_along(rs)) {
  message("i = ",i)
keep <- names(poly$atlantic$AZMP)[which(grepl(rs[i], names(poly$atlantic$AZMP), ignore.case=TRUE))]


if (rs[i] == "CS") {
  polygons <- polygons[which(!(grepl("CSS", polygons)))]
}

if (rs[i] == "LS") {
  polygons <- polygons[which(!(grepl("CLS", polygons)))]
}


if (any(grepl("V02", keep))) {
  polygons <- keep[which(grepl("V02", keep))]
}

POLY[[i]] <- polygons
}

POLY <- unlist(POLY)
names(POLY) <- paste0(rs, "_remote_sensing")

geom <- NULL
for (i in seq_along(unique(df$area))) {
  message(i)
  A <- unique(df$area)[i]
  k <- which(names(POLY) == A)
  k2 <-  unname(POLY[k])

  keep <- poly$atlantic$AZMP[which(names(poly$atlantic$AZMP) == k2)][[1]]
  latitude <- keep$lat
  longitude <- keep$lon

  DF <- data.frame(latitude=latitude, longitude=longitude)
  geom[[i]] <- sf::st_as_sf(DF,
               coords = c("longitude", "latitude"),
               crs = 4326)
}

names(geom) <- unique(df$area)


data <- data.frame(latitude=poly$atlantic$AZMP$ESS_V02$lat, longitude=poly$atlantic$AZMP$ESS_V02$lon)

data2 <- data.frame(latitude=poly$atlantic$AZMP$ESS_V02$lat, longitude=poly$atlantic$AZMP$ESS_V02$lon)


leaflet() %>%
  addTiles() %>%
  addPolygons(data=multipolygon) %>%
  addPolygons(data=outside_exclusive_multipolyon, color="black") %>%
  addPolygons(
    lng = data$longitude,
    lat = data$latitude,
    fillColor = "red",  # Set fill color
    fillOpacity = 0.5,   # Set fill transparency
    color = "black",     # Border color
    weight = 2           # Border weight
  ) %>%
  addPolygons(
    lng = data2$longitude,
    lat = data2$latitude,
    fillColor = "orange",  # Set fill color
    fillOpacity = 0.5,   # Set fill transparency
    color = "black",     # Border color
    weight = 2           # Border weight
  )


coords <- matrix(c(data$longitude, data$latitude), ncol = 2, byrow = FALSE)

# Ensure the polygon is closed by adding the first coordinate at the end
coords <- rbind(coords, coords[1,])

# Convert to an 'sf' object
polygon_sf <- st_sfc(st_polygon(list(coords)))
