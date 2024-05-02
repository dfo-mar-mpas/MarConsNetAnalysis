#' in_sea_distance
#'
#' @param cellsize numeric with target cellsize in meters of hexagonal cells the distance between opposite edges. The edge length is cellsize/sqrt(3))
#' @param bioregion sf polygon of the study area (i.e. the sea!)
#' @param areas sf polygons among which to calculate distances. e.g. protected areas
#' @param units character string for output distance matrix. Either "m" or "km" (by default).
#' @param crs target coordinate reference system: object of class crs, or input string for st_crs
#'
#' @return distance matrix
#' @export
#'
#' @examples
#' require(MarConsNetData)
#' bioregion <- get_bioregion()
#' areas <- data_CPCAD_areas(bioregion,zones=FALSE) |>
#'   dplyr::mutate(area=sf::st_area(geoms))
#' distkm <- calc_in_sea_distance(cellsize=100000,bioregion,areas)
#'
calc_in_sea_distance <- function(cellsize = 1000,
                            bioregion,
                            areas,
                            units="km",
                            crs=paste0("+proj=aeqd +lon_0=",
                                       sf::st_coordinates(sf::st_centroid(bioregion))[1,1],
                                       " +lat_0=",
                                       sf::st_coordinates(sf::st_centroid(bioregion))[1,2],
                                       " +datum=WGS84 +units=m +no_defs")){

  if(!units %in% c("m","km")) warning("Argument 'units' not recognized, returning distance matrix in meters")
  if(!"area" %in% names(areas)){
    areas$area <- sf::st_area(areas)
  }

  grid <- (sf::st_make_grid(sf::st_transform(bioregion,
                                             crs=crs),
                            square = FALSE,
                            cellsize=cellsize) |>
             sf::st_transform(sf::st_crs(bioregion)))[bioregion] |>
    sf::st_as_sf()

  intgrid <- sf::st_intersects(grid,areas)
  grid$protected <- lengths(intgrid)>0
  grid$protname <- lapply(intgrid,function(x){
    if(length(x)<1) return(NA)
    areas$NAME_E[x][1]
  }) |>
    unlist()

  combinedgridall <- grid |>
    dplyr::filter(protname %in% areas$NAME_E) |>
    dplyr::group_by(protname) |>
    dplyr::reframe(x=sf::st_combine(x),
                   protected=TRUE,
                   combined=TRUE) |>
    dplyr::ungroup() |>
    sf::st_as_sf() |>
    sf::st_make_valid()|>
    dplyr::bind_rows(dplyr::mutate(grid,combined=FALSE)) |>
    sf::st_as_sf()

  gall <- sf::st_intersects(combinedgridall,combinedgridall) |>
    as.data.frame() |>
    dplyr::filter(row.id!=col.id) |>
    as.matrix() |>
    igraph::graph_from_edgelist(directed=FALSE)

  igraph::V(gall)$protname <- combinedgridall$protname
  igraph::V(gall)$protected <- combinedgridall$protected
  igraph::V(gall)$combined <- combinedgridall$combined



  areanames <- areas$NAME_E[areas$NAME_E %in% unique(grid$protname)]
  distm <- matrix(NA,
                   nrow=length(areanames),
                   ncol=length(areanames),
                   dimnames = list(areanames,areanames))
  for(f in areanames){
    for(t in areanames){
      # print(paste("Calculating distances for ",f," and ",t))
      if(f==t){
        distm[areanames %in% c(f,t),areanames %in% c(f,t)] <- 0
      } else if(all(is.na(c(distm[areanames==t,areanames==f],distm[areanames==t,areanames==f])))){

        g <- igraph::delete_vertices(gall,(igraph::V(gall)$combined&!igraph::V(gall)$protname %in% c(t,f))|
                                       (!(igraph::V(gall)$combined)&igraph::V(gall)$protname %in% c(t,f)))

        distmat <- (igraph::distances(g,
                                      v = igraph::V(g)[igraph::V(g)$protected&igraph::V(g)$combined],
                                      to = igraph::V(g)[igraph::V(g)$protected&igraph::V(g)$combined]))*cellsize
        distm[areanames==t,areanames==f] <- distmat[1,2]
        distm[areanames==f,areanames==t] <- distmat[1,2]
      }
    }
  }

  # return final matrix in correct units
  if(units=="km"){
    return(distm/1000)
  } else {
    return(distm)
  }
}
