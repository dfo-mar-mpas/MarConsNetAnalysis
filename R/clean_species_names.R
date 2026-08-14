#' Clean scientific species names
#'
#' Standardizes scientific species names by replacing underscores with spaces
#' and formatting the first part of the name with title case.
#'
#' @param scientific_names A character vector of scientific species names.
#'
#' @return A character vector containing the cleaned scientific species names.
#'
#' @examples
#' clean_species_names(c("asterias_rubens", "cancer borealis"))
#'
#' @export

clean_species_names <- function(scientific_names) {

  output <- rep(NA_character_, length(scientific_names))

  for (i in seq_along(scientific_names)) {
  name <- scientific_names[i]
  name <- gsub("_", " ", name)


  name_split <- strsplit(tolower(name), " ")[[1]]
  ns <- paste(
    tools::toTitleCase(name_split[1]),
    paste(name_split[-1], collapse = " "),
    sep = " "
  )

  output[i] <- trimws(ns, 'both')
  }

  return(output)
}
