mpa <- MPAs
df <- azmpdata::Discrete_Occupations_Sections

area <- "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)"
type <- "surface"
dataframe=FALSE
parameter="temperature"

multipolygon <- mpa$geoms[which(mpa$NAME_E == area)]
points_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
inside <- sf::st_within(points_sf, multipolygon, sparse = FALSE)

points_inside <- points_sf[inside, ]


buffer_10km <- sf::st_buffer(multipolygon, dist = 10000)

buffer_10km_exclusive <- sf::st_difference(buffer_10km, multipolygon)


within_buffer <- sf::st_within(points_sf, buffer_10km_exclusive, sparse = FALSE)
points_within_buffer <- points_sf[which(within_buffer), ]



# OUTSIDE BUFFER
eblat <- 43.46669
eblon <- -62.4704
outside <- st_buffer(st_sfc(st_point(c(eblon, eblat)), crs = 4326), dist=150000)
outside_exclusive <- sf::st_difference(outside, multipolygon)


# Create a Leaflet map
map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = multipolygon,
              color = "red") %>%
  addPolygons(data=buffer_10km_exclusive, color='black') %>%
  addPolygons(data=outside_exclusive, color="green")
map











leaflet() %>%
  addTiles() %>%
  addPolygons(data = buffer_extended,
              color = "green")







