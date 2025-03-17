#' aggregate indicator values into bins or objectives
#'
#' @param group_level Character string for the name of the group level (e.g. "bin" or "objective")
#' @param group_name Character string for the name of the group (e.g. "Biomass Metrics" or "Biodiversity")
#' @param weights_ratio numeric vector for the relative weight of each group component (e.g. the ratio of weights between indicator)
#' @param weights_sum numeric value for the sum of the weights of each group (e.g. the sum of the indicator weights in the bin)
#' @param ... group components (e.g. the indicator data.frames in the case of bins)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # FIXME placeholder
#' }
aggregate_groups <- function(group_level,group_name,weights_ratio=1,weights_sum=1,...){
  args <- list(...)
  if(all(unlist(lapply(list(...),function(x) "weight" %in% colnames(x))))){
    df <- bind_rows(...) |>
      mutate({{group_level}} := group_name)
  } else {
    df <- bind_rows(...) |>
      mutate({{group_level}} := group_name,
             weight = weights_ratio/sum(weights_ratio)/length(args)*weights_sum)
  }
  distinct(df)


}
