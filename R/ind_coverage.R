#' Spatial coverage of x in y
#'
#' Calculates the percent or proportion of y (e.g. bioregion) that is spatially covered by x (e.g. protected areas)
#'
#' @param x sf polygon whose area acts as the numerator in the fraction (e.g. protected areas)
#' @param y sf polygon whose area acts as the denominator in the fraction (e.g. bioregion)
#' @param intersection logical that determines if the area of the spatial intersection of `x` and `y` should be used as the numerator (TRUE) or, by default, if the area of `x` should be used as the numerator.
#' @param proportion Logical that determines if this function returns a proportion (TRUE) or, by default (FALSE), a percentage (i.e. proportion X 100)
#'
#' @return numeric
#' @export
#'
#' @examples
#' \dontrun{
#' require(MarConsNetData)
#' bioregion <- data_bioregion()
#' areas <- data_bioregion()
#' ind_coverage(x=areas, y=bioregion)
#' }
#'
ind_coverage <- function(x,y,intersection=FALSE,proportion=FALSE){
  if(intersection){
    p <- (sf::st_geometry(x) |>
            sf::st_intersection(sf::st_geometry(y)) |>
            sf::st_area() |>
            sum() |>
            as.numeric())/(sf::st_geometry(y) |>
                             sf::st_area() |>
                             sum() |>
                             as.numeric()
            )
  } else {
    p <- (sf::st_geometry(x) |>
      sf::st_area() |>
      sum() |>
        as.numeric())/(sf::st_geometry(y) |>
                         sf::st_area() |>
                         sum() |>
                         as.numeric()

      )
  }

  if(proportion){
    return(p)
  } else {
    return(p*100)
  }
}
