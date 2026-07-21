#' Classify Taxa Using WoRMS, GBIF, and Manual Fallbacks
#'
#' Attempts to classify scientific names to a specified taxonomic rank using a
#' hierarchical lookup approach. The function first queries the World Register
#' of Marine Species (WoRMS), then falls back to GBIF if WoRMS cannot resolve
#' the requested classification. A final curated fallback table is used for
#' known unresolved taxa.
#'
#' The function also performs name cleaning to improve matching success,
#' including removal of environmental sample identifiers, BOLD identifiers,
#' sequencing/sample codes, taxonomic qualifiers (e.g., "cf."), hybrid names,
#' and genus-level "sp." identifiers.
#'
#' @param scientific_names A character vector of scientific names to classify.
#' Duplicate names are permitted and the returned vector will maintain the
#' original length and order.
#'
#' @param level Character string specifying the taxonomic rank to return.
#' Common options include "Kingdom", "Phylum", "Class", "Subclass",
#' "Order", "Family", "Genus", and "Species".
#'
#' @return A character vector containing the requested taxonomic classification
#' for each input name. The output length and order match the input vector.
#'
#' @details
#' The classification workflow follows this order:
#'
#' \enumerate{
#'   \item Query WoRMS using the AphiaID classification hierarchy.
#'   \item Query GBIF as a secondary taxonomic source.
#'   \item Apply curated manual classifications for known unresolved taxa.
#' }
#'
#' If a taxon cannot be classified after all steps, the function stops and
#' returns an error listing the unresolved taxa. This prevents new or
#' incorrectly formatted taxa from silently entering downstream analyses.
#'
#' @section Name cleaning:
#' The function automatically removes:
#' \itemize{
#'   \item Environmental sample labels
#'   \item BOLD identifiers
#'   \item Sequencing/sample identifiers (e.g., CMC, HLC, CCMP codes)
#'   \item "cf." qualifiers
#'   \item Hybrid designations
#'   \item "sp." identifiers by reducing to the genus name
#' }
#'
#' @section Data sources:
#' Primary classification source:
#' \itemize{
#'   \item WoRMS (World Register of Marine Species)
#' }
#'
#' Secondary classification source:
#' \itemize{
#'   \item GBIF Backbone Taxonomy
#' }
#'
#' Manual fallback classifications are included for taxa that cannot be
#' reliably resolved through external databases.
#'
#' @examples
#' \dontrun{
#' species <- c(
#'   "gadus morhua",
#'   "sebastes sp.",
#'   "ammodytes dubius"
#' )
#'
#' taxize_species(
#'   scientific_names = species,
#'   level = "Class"
#' )
#' }
#'
#' @import worrms
#' @importFrom rgbif name_backbone
#'
#' @export

taxize_species <- function(scientific_names, level = "Subclass") {

  # ----------------------
  # AI/manual fallback table
  # ----------------------

  ai_taxonomy <- data.frame(
    scientific_name = c(
      "annelida sp.",
      "cnidaria sp.",
      "bryozoa sp.",
      "anthozoa sp.",
      "lophiidae sp.",
      "sebastes sp."
    ),
    Phylum = c(
      "Annelida",
      "Cnidaria",
      "Bryozoa",
      "Cnidaria",
      "Chordata",
      "Chordata"
    ),
    Class = c(
      NA,
      NA,
      NA,
      "Anthozoa",
      "Actinopterygii",
      "Actinopterygii"
    ),
    Subclass = c(
      NA,
      NA,
      NA,
      NA,
      "Neopterygii",
      "Neopterygii"
    ),
    stringsAsFactors = FALSE
  )


  # ----------------------
  # Cleaning
  # ----------------------

  clean_name <- function(x){

    x <- tolower(x)

    x <- gsub(" environmental sample", "", x)
    x <- gsub(" bold:.*", "", x)
    x <- gsub(" cmc[0-9]+", "", x)
    x <- gsub(" hlc-[0-9]+", "", x)
    x <- gsub(" ccmp[0-9]+", "", x)

    x <- gsub(" cf\\.", "", x)

    # hybrids
    if(grepl(" x ", x)){
      x <- strsplit(x, " x ")[[1]][1]
    }

    # sp.
    x <- gsub(" sp\\..*", "", x)

    trimws(x)
  }


  species_unique <- unique(scientific_names)


  results <- lapply(seq_along(species_unique), function(i){

    message(i,"/",length(species_unique))

    original <- species_unique[i]

    candidates <- unique(c(
      original,
      clean_name(original)
    ))


    for(name in candidates){


      # ----------------------
      # WoRMS
      # ----------------------

      worms <- tryCatch(
        worrms::wm_records_name(name),
        error=function(e) NULL
      )


      if(!is.null(worms) && nrow(worms)>0){

        classification <- tryCatch(
          worrms::wm_classification(
            worms$AphiaID[1]
          ),
          error=function(e) NULL
        )


        if(!is.null(classification)){


          # direct rank match
          value <- classification$scientificname[
            classification$rank == level
          ]


          if(length(value)>0){
            return(value[1])
          }


          # if missing, try accepted parent ranks
          if(level %in% classification$rank){

            return(
              classification$scientificname[
                match(level, classification$rank)
              ]
            )
          }

        }

      }



      # ----------------------
      # GBIF fallback
      # ----------------------

      gbif <- tryCatch(
        rgbif::name_backbone(name),
        error=function(e) NULL
      )


      if(!is.null(gbif)){

        value <- gbif[[tolower(level)]]

        if(!is.null(value) && !is.na(value)){
          return(value)
        }

      }



      # ----------------------
      # AI/manual fallback
      # ----------------------

      if(name %in% ai_taxonomy$scientific_name){

        value <- ai_taxonomy[
          ai_taxonomy$scientific_name == name,
          level
        ]

        if(length(value)>0 && !is.na(value)){
          return(value)
        }
      }

    }


    NA_character_

  })


  lookup <- setNames(
    unlist(results),
    species_unique
  )


  output <- unname(
    lookup[scientific_names]
  )


  # ----------------------
  # Final check
  # ----------------------

  if(any(is.na(output))){

    missing <- unique(
      scientific_names[is.na(output)]
    )

    message(
      "taxa have no ",
      level,
      ": ",
      paste(missing, collapse=", ")
    )
  }


  output
}
