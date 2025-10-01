#' Create example FAKE data
#'
#' @param ind_name
#' @param areas
#' @param areaName
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # FIXME placeholder
#' }
ind_placeholder <- function(ind_name = "placeholder",
                            areas, areaName = "NAME_E") {

  n <- nrow(areas)

  x <- tibble::tibble(
    areaID = areas[[areaName]],
    indicator = ind_name,
    type = NA_character_,
    units = NA_character_,
    scoring = "placeholder score",
    PPTID = NA_integer_,
    project_short_title = NA_character_,
    climate = FALSE,
    design_target = FALSE,
    data = rep(list(NULL), n),   # list-column of NULL
    score = NA_real_,
    status_statement = "TBD",
    trend_statement = "TBD",
    source = NA_character_,
    climate_expectation = "FIXME",
    indicator_rationale = "FIXME",
    objectives = NA_character_,
    bin_rationale = "FIXME",
    plot = rep(list(NULL), n)    # list-column of NULL
  )

  return(x)
}
