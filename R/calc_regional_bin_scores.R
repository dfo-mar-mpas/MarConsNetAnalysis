#' Calculate regional bin-level weighted scores
#'
#' Calculates weighted mean indicator scores at the regional bin level.
#' Multiscale indicators are handled by assigning the regional-scale score
#' and excluding site-level contributions from regional calculations.
#' Results are aggregated by objective and bin, reweighted within each bin,
#' and then recombined with the original data, including
#' \code{"Non_Conservation_Area"} entries.
#'
#' @param df A data frame containing indicator scores and metadata. Must
#'   include at least the columns \code{target_name}, \code{region},
#'   \code{scale}, \code{score}, \code{weight}, \code{objective},
#'   \code{bin}, \code{areaID}, and \code{PPTID}.
#' @param target_bin_weight Numeric scalar giving the total weight assigned
#'   to each bin when calculating regional scores. Default is \code{1}.
#'
#' @return A data frame with regional bin-level scores calculated and
#'   appended to the original input data.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Identifies multiscale indicators (those with more than one scale
#'   per target and region).
#'   \item Assigns regional-scale scores to multiscale indicators and excludes
#'   site-level scores from regional aggregation.
#'   \item Calculates weighted mean scores at the bin level within regions.
#'   \item Redistributes bin weights evenly across indicators in each bin.
#'   \item Binds the calculated regional scores back to the full original
#'   dataset.
#' }
#'
#' @export

calc_regional_bin_scores <- function(df, target_bin_weight = 1) {
  result <- df |>
    ##### filter results to calculate weighted means at bin level
    group_by(target_name,region) |>
    mutate(multiscale = length(unique(scale)) > 1,
           # give all the multiscale indicators the score of the region scale
           # for the multiscale indicators, the individual site contributions do not count towards regional scores
           score = ifelse(multiscale,
                          score[scale!="site"],
                          score)) |>
    ungroup() |>
    filter(areaID != "Non_Conservation_Area" & scale=="site") |>
    group_by(objective, bin, areaID, region) |>
    reframe(
      indicator = unique(areaID),
      areaID = unique(region),
      score = weighted.mean(score, weight, na.rm = TRUE),
      score = if_else(is.nan(score), NA, score),
      PPTID = paste(PPTID, collapse = "; ")
    ) |>
    group_by(bin, areaID) |>
    mutate(weight = target_bin_weight / n()) |>
    ungroup() |>

    ##### Bind in full data, including Non_Conservation_Area
    bind_rows(df)
}
