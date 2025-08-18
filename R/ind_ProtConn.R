#' ProtConn
#'
#' @param distkm distance matrix in km. Can be created by the 'in_sea_distance()' in this package
#' @param dkm median dispersal distance in km for the negative exponential dispersal kernal (i.e. `p <- 1-pexp(distkm,log(2)/dkm)`)
#' @param bioregion sf polygon of the study area (i.e. the sea!)
#' @param area output of data_CPCAS_areas
#' @importFrom stats median pexp
#' @importFrom igraph graph_from_adjacency_matrix all_simple_paths V E
#' @importFrom dplyr bind_rows mutate
#' @importFrom sf st_area
#' @return numeric
#' @export
#'
#' @examples
#' \dontrun{
#' require(MarConsNetData)
#' bioregion <- data_bioregion()
#' areas <- data_CPCAD_areas(bioregion,zones=FALSE) |>
#'   dplyr::mutate(area=sf::st_area(geoms))
#' distkm <- calc_in_sea_distance(cellsize=100000,bioregion,areas)
#' PC <- ind_ProtConn(distkm,dkm=100, area=areas)
#' }
ind_ProtConn <- function(distkm,dkm,bioregion,area=NULL){
  from_area <- to_area <- NULL
  if (is.null(area)) {
    stop("Must provide an area argument")
  }
  p <- 1-pexp(distkm,log(2)/dkm)
  p[p<median(p)/10] <- 0
  protg <- igraph::graph_from_adjacency_matrix(p,weighted = TRUE)

  edgelist <- data.frame()

  for(f in row.names(distkm)){
    for(t in colnames(distkm)){
      # print(paste("dkm = ",dkm," calculating ProtConn for ",f," and ",t))
      if(t==f){
        pstar=1
      } else {
        paths <- igraph::all_simple_paths(protg,
                                          igraph::V(protg)[which(row.names(distkm)==f)],
                                          igraph::V(protg)[which(colnames(distkm)==t)],
                                          cutoff = 4)
        if(length(paths)<1){
          pstar=0
        } else {
          pstar <- sum(unlist(lapply(paths, function(pa){
            prod(igraph::E(protg,path=pa)$weight)

          })))
        }
      }


      edgelist <- dplyr::bind_rows(
        edgelist,
        data.frame(to=t,
                   to_area=area$area[area$NAME_E==t],
                   from=f,
                   from_area=area$area[area$NAME_E==f],
                   pstar=pstar
        ) |>
          dplyr::mutate(product=as.numeric(to_area*from_area*pstar))
      )
    }
  }
  as.numeric(100*sqrt(sum(edgelist$product))/sum(sf::st_area(bioregion)))
}
