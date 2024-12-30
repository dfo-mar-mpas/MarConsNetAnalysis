#' Create example FAKE data
#'
#' @param ind_name
#' @param areas
#' @param areaName
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # FIXME placeholder
#' }
ind_placeholder <- function(ind_name = paste0("placeholder_",round(runif(1,1,100))),areas,areaName = "NAME_E"){
  data.frame(area_name = areas[[areaName]],
             ind_name = ind_name,
             ind_status = round(100 * rbeta(nrow(areas), shape1=5, shape2=2))
             ind_trend = runif(nrow(areas),min=-30,max=30),
             ind_projects = paste(
               sample(c(229,395,480,642,691,835,857,1438,1448,1874,1921,1938,1957,2009,2052,2202,2224,2576,2604,2609),
                      round(runif(1,1,4))),
               collapse = ", "),
             ind_rawdata_type = sample(c("direct in-situ measurements","validated model data","unvalidated model data","Expert opinion"),1),
             ind_certainy = sample(c("very certain","certain","uncertain","very uncertain"),1))
  }
