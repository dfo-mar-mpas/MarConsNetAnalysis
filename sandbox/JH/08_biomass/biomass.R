library(Mar.datawrangling)
library(Mar.utils)
get_data('rv', data.dir = "C:/Users/HarbinJ/Documents/data/rv")

# All haddock
GSSPECIES = GSSPECIES[GSSPECIES$CODE %in% c(11),]
Mar.datawrangling::self_filter(keep_nullsets = F)
all_haddock = Mar.datawrangling::summarize_catches()


# SPECIES ABUNDANCE (HADDOCK)
df <- all_haddock





map <- leaflet() %>%
  addTiles() %>%
  addPolygons(lng=df$LONGITUDE, lat=df$LATITUDE)

map





