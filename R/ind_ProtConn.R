#' ProtConn
#'
#' @param distkm distance matrix in km. Can be created by the 'in_sea_distance()' in this package
#' @param dkm median dispersal distance in km for the negative exponential dispersal kernal (i.e. `p <- 1-pexp(distkm,log(2)/dkm)`)
#' @param bioregion sf polygon of the study area (i.e. the sea!)
#' @param stepcutoff Maximum length of the paths (i.e. stepping stones) that are considered. Default is 3 (i.e. start -> stepping stone -> end) for a single stepping stone. If negative, no cutoff is used but will greatly increase computational requirements.
#' @param probcutoff Minimum probability of a connection between two areas. Default is 0.0001, i.e. any probability lower than 1 in 10000 is considered unconnected.
#' @param returns string describing what the function should return. By default, "PC", the function returns the ProtConn value. To return a data frame with the edge list of the network use "EL".
#' @param area output of data_CPCAS_areas
#'
#' @importFrom stats median pexp
#' @importFrom igraph graph_from_adjacency_matrix all_simple_paths V E
#' @importFrom dplyr bind_rows mutate
#' @importFrom sf st_area
#' @returns numeric
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
ind_ProtConn <- function(distkm,dkm,bioregion,area=NULL,stepcutoff=3,probcutoff=0.0001,returns = "PC"){
  from_area <- to_area <- NULL
  if (is.null(area)) {
    stop("Must provide an area argument")
  }
  p <- 1-pexp(distkm,log(2)/dkm)
  p[p<probcutoff] <- 0
  protg <- igraph::graph_from_adjacency_matrix(p,weighted = TRUE)

  edgelist <- data.frame()

  for(f in area$NAME_E){
    for(t in area$NAME_E){
      print(paste("f = ",f,"; t = ",t))
      if (t==f){
        pstar = 1
      } else {
        paths <- igraph::all_simple_paths(protg,
                                          igraph::V(protg)[which(row.names(distkm)==f)],
                                          igraph::V(protg)[which(colnames(distkm)==t)],
                                          cutoff = stepcutoff)
        if(length(paths)<1){
          pstar=0
        } else {
          pstar <- max(unlist(lapply(paths, function(pa){
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
                   pstar=pstar,
                   p = p[f,t]
        ) |>
          dplyr::mutate(product=as.numeric(to_area*from_area*pstar))
      )
    }
  }

  if(returns == "EL"){
    edgelist
  } else {
    if(returns != "PC") warning("Argument 'returns' not recognized, must be either 'PC' or 'EL'. Returning PC value")
    as.numeric(100*sqrt(sum(edgelist$product))/sum(sf::st_area(bioregion)))
  }

}
