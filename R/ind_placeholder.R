#' Create example FAKE data
#'
#' @param ind_name An indicator name
#' @param areas A data frame containing MPA names in both English and French as
#'  well as the MPA's shape file (see MPAs target)
#' @param areaName Indicates what column in areas data frame MPA names can be
#' found
#' @param readiness a character argument that is either 'Ready', 'Readily
#'  Available', 'Not currently collected', 'Conceptual', or 'Unknown'.
#'
#' @param source indicates where data comes from
#' @param objectives indicates the management objective an indicator informs
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' # FIXME placeholder
#' }
ind_placeholder <- function(ind_name = "placeholder",
                            areas, areaName = "NAME_E", readiness=NA, source=NA, objectives=NA) {
  #browser()

  if (is.na(readiness)) {
    stop('Must specify a readiness argument of either Ready, Readily Available,Not currently collected, Conceptual, or Unknown.')
  }

  if (!(readiness %in% c('Ready', 'Readily Available','Not currently collected','Conceptual', 'Unknown'))) {
    stop('readiness must be one of the following: Ready, Readily Available,Not currently collected, Conceptual, Unknown.')
  }

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
    source = source,
    climate_expectation = "FIXME",
    indicator_rationale = "FIXME",
    objectives = paste0(objectives, collapse=" ;;; "),
    bin_rationale = "FIXME",
    plot = rep(list(NULL), n),    # list-column of NULL
    readiness=readiness
  )

  return(x)
}
