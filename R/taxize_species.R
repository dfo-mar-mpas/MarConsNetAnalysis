#' Retrieve taxonomic subclass information for species from WoRMS
#'
#' Uses the World Register of Marine Species (WoRMS) API to retrieve the
#' taxonomic classification of scientific names and extract the subclass rank.
#'
#' @param scientific_names A character vector of accepted scientific species
#'   names to query in WoRMS.
#'
#' @return A character vector containing the subclass for each species. Returns
#'   `NA` when no match is found, the query fails, or no subclass is available.
#'
#' @details
#' For each scientific name, the function first queries WoRMS using
#' `worrms::wm_records_name()` to obtain an AphiaID. It then uses
#' `worrms::wm_classification()` to retrieve the taxonomic hierarchy and
#' extracts the classification at the "Subclass" rank.
#'
#' @importFrom worrms wm_records_name wm_classification
#'
#' @examples
#' \dontrun{
#' species <- c("Gadus morhua", "Scomber scombrus")
#' taxize_species(species)
#' }
#'

taxize_species <- function(scientific_names, level='Subclass') {

  subclass <- vector("character", length(scientific_names))

  for (i in seq_along(scientific_names)) {
    message(i, "/", length(scientific_names))

    result <- try(
      worrms::wm_records_name(scientific_names[i]),
      silent = TRUE
    )

    if (inherits(result, "try-error") || nrow(result) == 0) {
      subclass[i] <- NA_character_

    } else {
      aphia_id <- result$AphiaID[1]

      classification <- try(
        worrms::wm_classification(id = aphia_id),
        silent = TRUE
      )

      if (inherits(classification, "try-error")) {
        subclass[i] <- NA_character_

      } else {
        subclass_match <- classification$scientificname[
          classification$rank == level
        ]

        subclass[i] <- ifelse(
          length(subclass_match) == 0,
          NA_character_,
          subclass_match[1]
        )
      }
    }
  }

  subclass
}
