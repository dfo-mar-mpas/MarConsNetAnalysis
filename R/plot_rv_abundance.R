#' Plot Average number of species per tow per year
#'
#' This function plots the average species catch per standardized tow
#' to 1.75 km for each year.
#' @param abundance data.frame likely from ind_rv_abundance
#' @export
#'
#' @examples
#' \dontrun{
#' rva <- ind_rv_abundance()
#' plot_rv_abundance(rva)
#' }

plot_rv_abundance <- function(abundance=NULL) {
  if (is.null(abundance)) {
    stop("Must provide an abundance argument")
  }

  if (!(all(names(abundance) %in% c("year", "abundance", "species")))) {
    stop("Must obtain abundance from ind_rv_abundance")
  }

  plot(x=1:length(abundance$year), y=abundance$abundance, ylab=paste("Average # of", unique(abundance$species), "Per Tow"), xlab="Year", type="o", pch=20, xaxt="n")
  axis(side = 1, at = seq_along(abundance$year), labels = abundance$year, las=2)
}
