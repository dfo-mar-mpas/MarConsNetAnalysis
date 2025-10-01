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

  if (length(weights_ratio) == 1){
    weights_ratio <- rep(weights_ratio,length(args))
  } else if (length(weights_ratio) != length(args)){
    stop("weights_ratio must be of length 1 or equal to the number of group components")
  }

  if(all(unlist(lapply(list(...),function(x) "weight" %in% colnames(x))))){
    df <- bind_rows(...) |>
      mutate({{group_level}} := group_name) |>
      distinct(across(-plot), .keep_all = TRUE)
  } else {
    #browser()
    for (i in seq_along(args)){
      message(i)
      args[[i]] <- args[[i]] |>
        mutate(weight = weights_ratio[i])
    }


    # df <- bind_rows(args) |>
    #   distinct() |>
    #   group_by(areaID) |>
    #   mutate({{group_level}} := group_name,
    #          weight = weight/sum(weight)*weights_sum) |>
    #   ungroup()

    df <- bind_rows(args) |>
      distinct(across(-plot), .keep_all = TRUE) |>
      group_by(areaID) |>
      mutate({{group_level}} := group_name,
             weight = weight/sum(weight)*weights_sum) |>
      ungroup()
  }

  return(df)


}
