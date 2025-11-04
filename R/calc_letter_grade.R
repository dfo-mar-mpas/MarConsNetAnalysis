#' Calculate letter grades
#'
#' @param scores vector of numeric scores to be converted to letter grades
#'
#' @return vector of letter grades
#' @export
#'
#' @examples
#'
#' scores <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
#'
#' calc_letter_grade(scores)
#'
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
