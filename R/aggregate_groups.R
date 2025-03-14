#' aggregate indicator values into bins or objectives
#'
#' @param group_level Character string for the name of the group level (e.g. "bin" or "objective")
#' @param group_name Character string for the name of the group (e.g. "Biomass Metrics" or "Biodiversity")
#' @param weights numeric vector for the relative weight of each group component
#' @param ... group components (e.g. the indicator data.frames in the case of bins)
#' @export
#'
#' @examples
#' \dontrun{
#' # FIXME placeholder
#' }
aggregate_groups <- function(group_level,group_name,weights=1,...){
  if(all(unlist(lapply(list(...),function(x) "weight" %in% colnames(x))))){
    df <- bind_rows(...) |>
      mutate({{group_level}} := group_name)
  } else {
    df <- bind_rows(...) |>
      mutate({{group_level}} := group_name,
             weight = weights)
  }
  distinct(df)


}
