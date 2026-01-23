#' Calculate group scores recursively
#'
#' @param df data.frame containing the data
#' @param grouping_var character vector for the name(s) of the grouping column(s) in `df`.
#'   Use `NULL` (default) to calculate the overall score. When multiple grouping variables
#'   are provided, the function recursively calculates scores from finest to coarsest granularity,
#'   with each level using the weighted scores from the previous level.
#' @param score_var character string for the name of the score column in `df`
#' @param weight_var character string for the name of the weight column in `df`
#'
#' @return data.frame with group scores at all levels of hierarchy
#' @importFrom dplyr select group_by reframe bind_rows
#' @export
#'
#' @examples
#' df <- data.frame(region = rep(c("Atlantic", "Pacific"), each = 6),
#'                  objective=rep(c("Biodiversity",
#'                                  "Habitat",
#'                                  "Productivity"),
#'                                times=4),
#'                  bins=c("Genetic Diversity",
#'                         "Species Diversity",
#'                         "Environmental Representativity",
#'                         "Key Fish Habitat",
#'                         "Biomass Metrics",
#'                         "Structure and Function",
#'                         "Functional Diversity",
#'                         "Connectivity",
#'                         "Uniqueness",
#'                         "Threats to Habitat",
#'                         "Threats to Productivity",
#'                         "Genetic Diversity"),
#'                  score=runif(12,45,100),
#'                  weight=1)
#'
#' # overall score
#' overall <- calc_group_score(df)
#'
#' # single group score
#' group <- calc_group_score(df, grouping_var = "objective")
#'
#' # 2-level hierarchy (bins -> objective)
#' # objective scores are weighted means of bin scores
#' two_level <- calc_group_score(df, grouping_var = c("bins", "objective"))
#'
#' # 3-level hierarchy (bins -> objective -> region)
#' # objective scores from bins, region scores from objectives
#' three_level <- calc_group_score(df, grouping_var = c("bins", "objective", "region"))
calc_group_score <- function(df, grouping_var = NULL, score_var = "score", weight_var = "weight") {

  # Base case: no grouping variable (overall score)
  if (is.null(grouping_var) || length(grouping_var) == 0) {
    result <- data.frame(score = df[[score_var]],
                         weight = df[[weight_var]]) |>
      reframe(score = weighted.mean(score, weight, na.rm = TRUE),
              weight = sum(weight))
    return(result)
  }

  # Handle single grouping variable
  if (length(grouping_var) == 1) {
    result <- data.frame(grouping = df[[grouping_var]],
                         score = df[[score_var]],
                         weight = df[[weight_var]]) |>
      group_by(grouping) |>
      reframe(!!grouping_var := unique(grouping),
              score = weighted.mean(score, weight, na.rm = TRUE),
              weight = sum(weight))

    if (grouping_var == "grouping") {
      return(result)
    } else {
      return(result |> select(-grouping))
    }
  }

  # Recursive case: multiple grouping variables
  # Step 1: Calculate scores for the finest level (first grouping variable)
  smallest_level <- calc_group_score(df,
                                    grouping_var = grouping_var[1],
                                    score_var = score_var,
                                    weight_var = weight_var) |>
    # Step 2: join the other grouping_var back in
    left_join(df |>
                select(grouping_var) |>
                distinct(),
              by = grouping_var[1])



  # Step 3: Recursively calculate scores for coarser levels using aggregated scores
  result <- calc_group_score(smallest_level,
                                 grouping_var = grouping_var[-1],
                                 score_var = "score",
                                 weight_var = "weight")


  return(result)
}
