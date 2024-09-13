library(rvest)
library(httr)
url <- "https://www.glf.dfo-mpo.gc.ca/glf/en/snow-crab-fishing-areas"

pages <- lapply(url,read_html)
response <- lapply(url, GET)
lines <- strsplit(content(response[[1]], as="text"), "\n")

keep <- which(grepl("Coordinates Table", lines[[1]], ignore.case=TRUE))


headers <- lines[[1]][keep]

for (i in seq_along(keep)) {
  k <- keep[[i]]
}

