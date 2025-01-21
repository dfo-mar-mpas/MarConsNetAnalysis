#' Climate Change Analysis
#'
#' This function creates a basic interpretation of the impact of climate change
#' on indicators. If:
#' 1. There is a statistically significant change in results between inside
#' and outside comparison, we determine the change between the two could be a
#' result of protection efforts in the protected area
#' 2. There is not a statistically significant change in results between inside
#' and outside comparison and :
#'  A) The trend is going in the opposite direction that we would expect and
#'  statistically significant,we point out that according to our analysis
#'  climate change could be having the opposite impact than we could expect
#'  B) The trend is not statistically significant, we  explain that climate
#'  change does not seem to be having a significant impact on that particular
#'  indicator and
#'  C) The trend is going in the direction that we would expect and
#'  statistically significant,we point out that according to our analysis
#'  climate change could be having a significant impact on that indicator
#'
#' @param trend trend column in indicator_to_plot
#' @param itp indicator_to_plot data frame, likely from the targets file
#'
#' @return a string interpreting results of indicators from a climate change lens
#' @export
#'
#' @examples
climate_analysis <- function(trend=NULL, itp=indicator_to_plot) {
  if (!(grepl("BLANK", trend))) {
    TREND <- str_split(trend, "(?<!\\d)\\.(?!\\d)")[[1]]
    TREND <- paste0(TREND[1], ". ",trimws(TREND[3], "both"),". ", str_extract(TREND[4], "The difference between.*"))


    ioPVAL <- str_extract(trend, "\\d+(\\.\\d+)?(?=\\D*$)")
    if (ioPVAL < 0.05) {
      conclusion <- " and therfore implies that protection in this area may directly be impacting this indicator. It is therefore difficult to determine if changes in this indicator are a result of projection
      efforts or climate change."
    } else {
      # No statistical difference (therefore potentially climate change)
      conclusion <- " and therefore implies that protection in this area may not be directly impacting this indicator. Trends in this indicator could therefore be linked to climate change"

      if (itp$status_grade[which(tolower(itp$trend) == tolower(trend))] == "A") {
        # the direction is going in the direction we want and statistically significant

       conclusion <- paste0(conclusion, " The results show us that climate change doesn't seem to be impacting this indicator the way we would expect, and in fact, the trends are showing a statistically significant
        trend in the opposite direction than we would expect.")



      } else if (itp$status_grade[which(tolower(itp$trend) == tolower(trend))] %in% c("B", "C", "D")) {
        # "there is not a significant change"

        conclusion <- paste0(conclusion, " , but the change is not significant.")



      } else if (itp$status_grade[which(tolower(itp$trend) == tolower(trend))] == "F") {
        "stat.sig going in the wrong direction"
        conclusion <- paste0(conclusion, " The results show us that climate change does seem to be impacting this indicator the way we would expect, and in fact, the trends are showing a statistically significant
        trend in the direction than we would expect as a result of climate change.")
      }

    }
    final <- paste0(TREND, conclusion)
    return(final)
  }

}
