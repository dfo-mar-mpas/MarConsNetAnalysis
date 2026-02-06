#' Create a placeholder indicator data frame
#'
#' Generates a standardized placeholder data frame for an indicator,
#' suitable for initializing indicator structures when data are not
#' yet available. This allows the MarConsNet framework to maintain
#' consistent columns and metadata across sites or areas, even for
#' indicators that are not yet collected or conceptual.
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
#' @param site a character string of either 'site' or 'network'
#' @param theme a character string of length 1 of either "Ocean Conditions",
#' "Ocean Structure and Movement","Primary Production","Secondary Production",
#' "Marine Mammals and Other Top Predators", "Trophic Structure and Function",
#' "Benthic Environment", "Fish and Fishery Resources",
#' or "Anthropogenic Pressure and Impacts"
#' @param SME subject matter expert for the data/ indicator (e.g. John Doe)
#' @param assumptions data assumptions
#' @param caveats data caveats
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' placeholder <- ind_placeholder(
#'   ind_name = "Habitat Quality",
#'   areas = MPAs,
#'   readiness = "Conceptual",
#'   objectives = c("Maintain biodiversity", "Protect habitat"),
#'   scale = "site"
#' )
#' }
ind_placeholder <- function(ind_name = "placeholder",
                            areas, areaName = "NAME_E", readiness=NA, source=NA, objectives=NA, scale='site', theme=NA, SME=NA, assumptions=NA, caveats=NA) {
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
    region=areas$region,
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
    quality_statement="TBD",
    source = source,
    climate_expectation = "FIXME",
    indicator_rationale = "FIXME",
    objectives = paste0(objectives, collapse=" ;;; "),
    bin_rationale = "FIXME",
    plot = rep(list(NULL), n),    # list-column of NULL
    readiness=readiness,
    scale=scale,
    theme=theme,
    SME=SME
  )

  return(x)
}
