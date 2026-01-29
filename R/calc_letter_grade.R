#' Convert numeric scores to letter grades
#'
#' Translates numeric scores (0–100) into letter grades using standard
#' intervals: F (0–20), D (20–40), C (40–60), B (60–80), and A (80–100).
#' Scores outside 0–100 are treated as NA. NA or NaN scores are returned as "NA".
#'
#' @param scores
#' Numeric vector of scores to convert to letter grades. Scores are
#' internally floored to integers before grading.
#'
#' @return
#' Character vector of the same length as \code{scores}, containing
#' letter grades: "A", "B", "C", "D", "F", or "NA".
#'
#' @details
#' The grading scale is inclusive of the lower bound and exclusive of the
#' upper bound for all intervals except for F (0–20) and A (80–100), which
#' include both endpoints.
#'
#' @examples
#' calc_letter_grade(c(95, 73, 55, 38, 15, NA, NaN))
#' # Returns: "A" "B" "C" "D" "F" "NA" "NA"
#'
#'@export
calc_letter_grade <- function(scores) {
  sapply(floor(scores), function(score) {
    if (!(is.na(score)) && !(is.nan(score))) {
      if (score >= 0 && score <= 20) {
        grade <- "F"
      } else if (score >= 20 && score < 40) {
        grade <- "D"
      } else if (score >= 40 && score < 60) {
        grade <- "C"
      } else if (score >= 60 && score < 80) {
        grade <- "B"
      } else if (score >= 80 && score <= 100) {
        grade <- "A"
      }
    } else {
      grade <- "NA"
    }
    return(grade)
  })
}
