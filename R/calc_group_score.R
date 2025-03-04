#' Calculate group scores
#'
#' @param df
#' @param grouping_var character string for the name of the grouping column in `df`. Use `NULL` (default) to calculate the overall score.
#' @param score_var character string for the name of the score column in `df`
#' @param weight_var character string for the name of the weight column in `df`
#'
#' @return
#' @export
#'
#' @examples
#' df <- data.frame(objective=rep(c("Biodiversity",
#'                                  "Habitat",
#'                                  "Productivity"),
#'                                times=c(3,5,3)),
#'                  bins=c("Genetic Diversity",
#'                         "Species Diversity",
#'                         "Functional Diversity",
#'
#'                         "Environmental Representativity",
#'                         "Key Fish Habitat",
#'                         "Connectivity",
#'                         "Uniqueness",
#'                         "Threats to Habitat",
#'
#'                         "Biomass Metrics",
#'                         "Structure and Function",
#'                         "Threats to Productivity"),
#'                  score=runif(11,45,100),
#'                  weight=1,
#'                  area_name = "Random Example MPA")
#' # overall score
#' overall <- calc_group_score(df)
#'
#' # group score
#' group <- calc_group_score(df, grouping_var = "objective")
calc_group_score <- function(df, grouping_var = NULL, score_var = "score", weight_var = "weight") {
  if (is.null(grouping_var)) {
    df <- data.frame(score = df[[score_var]],
                     weight = df[[weight_var]]) |>
      reframe(score = weighted.mean(score, weight, na.rm = TRUE),
                weight=sum(weight))

    return(df)
  } else {
    df <- data.frame(grouping = df[[grouping_var]],
                     score = df[[score_var]],
                     weight = df[[weight_var]]) |>
      group_by(grouping) |>
      reframe({{grouping_var}} := unique(grouping),
                score = weighted.mean(score, weight, na.rm = TRUE),
                weight=sum(weight))
    if (grouping_var=="grouping"){
      return(df)
    } else {
      return(df |> select(-grouping))
    }
  }

}
