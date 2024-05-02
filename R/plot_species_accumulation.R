#' Creates a species accumulation curve
#'
#' This function creates a species accumulation plot for
#' data collected in MPAs
#'
#' @param datas a data frame or list of data frames obtained from `get_project_data()`
#' @importFrom ggplot2 geom_line
#' @examples
#' \dontrun{
#' library(MarConsNetData)
#' library(MarConsNetAnalysis)
#' d <- get_project_data(ids=1093)
#' plot_species_accumulation(datas=d)
#' }
#' @importFrom vegan specaccum
#' @importFrom ggplot2 ggplot aes
#' @export

plot_species_accumulation <- function(datas=NULL) {

  if (is.null(datas)) {
    stop("1. In mapAppData() must provide a datas argument, which is an output from getAppData()")
  }

  if(class(datas) == "list" && length(datas) > 1) {
    if (!(class(datas[[1]]) == "data.frame")) {
      stop("2. In mapAppData() must provide a datas argument, which is an output from getAppData()")

    }
  }

  if (class(datas) == "data.frame") { # only one id given
    DATAS <- vector(mode = "list", length = 1)
    DATAS[[1]] <- datas
    datas <- DATAS
  }


  spacc <- lapply(datas, function(x) specaccum(x, "random"))

  plotdata <- NULL
  for (DD in seq_along(spacc)) {
    plotdata[[DD]] <- data.frame(Sites=spacc[[DD]]$sites,
                           Richness=spacc[[DD]]$richness,
                           SD=spacc[[DD]]$sd,
                           Source = paste(unique(datas[[DD]]$id)))
  }

  ymax <- max(unlist(lapply(plotdata, function(x) x$Richness))) + 10
  xmax <- max(unlist(lapply(plotdata, function(x) x$Sites))) + 10

  ggplot() +
    lapply(plotdata, function(df) {
      geom_line(data=df, aes(x=Sites, y=Richness,col=Source),linewidth=2)
    }
      )

}
