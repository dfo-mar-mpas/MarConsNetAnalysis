#' Creates reports of Project Planning Tool data
#'
#' This function creates reports of Project Planning Tool
#' data
#'
#' @param datas a data frame or list of data frames obtained from `get_project_data()`
#' @param combine a Boolean indicating if the `datas` should be combined
#' into a single report. If `combine=FALSE` each item in the `datas`
#' argument will be an individual reports
#' @param destdir parameter indicating where to save the html report
#' @importFrom rmarkdown render
#' @examples
#' \dontrun{
#' library(MarConsNetData)
#' library(MarConsNetAnalysis)
#' d <- get_project_data(ids=c(1093, 642))
#' create_data_report(datas=d)
#' }
#' @export

create_data_report <- function(datas=NULL, combine=FALSE, destdir=".") {
  if (is.null(datas)) {
    stop("1. In createDataRerport() must provide a datas argument, which is an output from getAppData()")
  }


  if(class(datas) == "list" && length(datas) > 1) {
    if (!(class(datas[[1]]) == "data.frame")) {
      stop("2. In createDataRerport() must provide a datas argument, which is an output from getAppData()")

    }
  }

  Rmdpath <- file.path(system.file(package="MarConsNetAnalysis"),"templates")

  if (class(datas) == "data.frame") { # only one id given
    DATAS <- vector(mode = "list", length = 1)
    DATAS[[1]] <- datas
    datas <- DATAS
  }

  if (combine == FALSE) {
#browser("in createDataReport")
  for (i in seq_along(datas)) {
    D <- datas[[i]]
  rmarkdown::render(
    file.path(Rmdpath, "report.Rmd"),
    output_dir = destdir,
    output_file = unique(D$id),
    output_format = "html_document"
  )
  }
  } else {
    D <- datas
    rmarkdown::render(
      file.path(Rmdpath, "report.Rmd"),
      output_dir = destdir,
      output_file = "summary_report",
      output_format = "html_document"
    )
  }



}
