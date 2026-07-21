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
