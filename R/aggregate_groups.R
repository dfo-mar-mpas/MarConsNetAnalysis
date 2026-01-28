#' Aggregate multiple groups into a single group
#'
#' Combines multiple data frames or tibbles representing different components
#' into a single group-level data frame, applying optional weighting for
#' aggregation.
#'
#' @param group_level Character string for the name of the group level (e.g. "bin" or "objective")
#' @param group_name Character string for the name of the group (e.g. "Biomass Metrics" or "Biodiversity")
#' @param weights_ratio numeric vector for the relative weight of each group component (e.g. the ratio of weights between indicator)
#' @param weights_sum numeric value for the sum of the weights of each group (e.g. the sum of the indicator weights in the bin)
#' @param ... group components (e.g. the indicator data.frames in the case of bins)
#' @details
#' If the input data frames already contain a `weight` column, the function
#' simply combines them, adds the group-level column, and removes duplicate
#' rows (keeping `plot` columns intact). Otherwise, it assigns weights
#' according to `weights_ratio`, normalizes them to sum to `weights_sum`, and
#' tracks the original component name in a `target_name` column.
#'
#' @examples
#' df1 <- tibble::tibble(areaID = c("A", "B"), score = c(80, 90))
#' df2 <- tibble::tibble(areaID = c("A", "B"), score = c(70, 95))
#' aggregate_groups("group", "CombinedGroup", weights_ratio = c(0.6, 0.4), df1, df2)
#'
#' @export
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
      distinct(across(!any_of("plot")), .keep_all = TRUE)
  } else {
    arg_names <- as.character(substitute(list(...)))[-1]

    args_weighted <- purrr::pmap(
      list(df = args, wt = weights_ratio, nm = arg_names),
      function(df, wt, nm) {
        df |> mutate(weight = wt, target_name = nm)
      }
    )

    df <- bind_rows(args_weighted) |>
      distinct(across(!any_of("plot")), .keep_all = TRUE) |>
      group_by(areaID) |>
      mutate({{group_level}} := group_name,
             weight = weight/sum(weight)*weights_sum) |>
      ungroup()
  }

  return(df)


}
