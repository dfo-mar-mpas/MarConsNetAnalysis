#' Creates map of Project Planning Tool data
#'
#' This function creates a plot of the location of
#' Project Planning Tool data
#'
#' @param datas a data frame or list of data frames obtained from `get_project_data()`
#' @examples
#' \dontrun{
#' library(MarConsNetData)
#' library(MarConsNetAnalysis)
#' d <- get_project_data(ids=c(1093, 642))
#' map_app_data(datas=d)
#' }
#' @importFrom grDevices rainbow
#' @importFrom leaflet leaflet addTiles addCircleMarkers addLegend
#' @export

map_app_data <- function(datas=NULL) {

  if (is.null(datas)) {
    stop("1. In mapAppData() must provide a datas argument, which is an output from getAppData()")
  }

  if(class(datas) == "list" && length(datas) > 1) {
    if (!(class(datas[[1]]) == "data.frame")) {
      stop("2. In mapAppData() must provide a datas argument, which is an output from getAppData()")

    }
  }

  map <- leaflet() %>%
    addTiles()

  layer_names <- list()

  if (class(datas) == "data.frame") { # only one id given
  DATAS <- vector(mode = "list", length = 1)
  DATAS[[1]] <- datas
  datas <- DATAS
  }
  COLORS <- rainbow(length(datas))

layer_names <- NULL
  for (i in seq_along(datas)) {
    # Extract id, lat, and lon from the current data frame
    id <- datas[[i]][, "id"]
    lat <- datas[[i]][, "lat"]
    lon <- datas[[i]][, "lon"]

    # Add points to the map for the current data frame
    map <- map %>%
      addCircleMarkers(
        data = data.frame(lat = lat, lon = lon),
        label = ~as.character(id),
        #labelOptions = labelOptions(noHide = TRUE),
        color = COLORS[i], # Set color for current data frame
        group = paste0("Group", i) # Group points by data frame for adding to legend
      )

    # Add the layer name for the legend
    layer_names[[i]] <- unique(as.character(id))
  }
  map <- map %>%
    addLegend(
      position = "bottomright",
      colors = unlist(COLORS),
      labels = unlist(layer_names),
      title = "Projects"
    )

  map
}
