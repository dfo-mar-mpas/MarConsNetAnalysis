#' Resolve file paths for MarConsNet targets configurations
#'
#' Returns validated file system paths used for accessing
#' MarConsNet \pkg{targets} resources, depending on the requested
#' execution context.
#'
#' @param path
#' Character string specifying which path to return. Must be one
#' of:
#' \describe{
#'   \item{\code{"onedrive"}}{Path to the OneDrive-synchronized
#'   \code{app_targets} directory.}
#'   \item{\code{"inst"}}{Path to the local \code{_targets.R} file
#'   within an \code{inst/} directory.}
#'   \item{\code{"pkg"}}{Path to the installed package copy of
#'   \code{_targets.R}.}
#' }
#'
#' @details
#' When \code{path = "onedrive"}, the function checks for the
#' existence of the \code{MarConsNetTargets/app_targets} directory
#' under the \code{OneDriveCommercial} environment variable.
#'
#' When \code{path = "inst"}, the function returns the path to a
#' local \code{inst/_targets.R} file, typically used during
#' package development.
#'
#' When \code{path = "pkg"}, the function resolves the location of
#' \code{_targets.R} within the installed \pkg{civi} package.
#'
#' If the requested path does not exist, or if \code{path} is not
#' recognized, the function stops with an informative error
#' message.
#'
#' @return
#' A character string giving the resolved file or directory path.
#' @examples
#' \dontrun{
#' # Get the OneDrive targets store path
#' tar_paths("onedrive")
#' }
#'
#' @export
tar_paths <- function(path="onedrive"){
  if(path=="onedrive"){
    od <- file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","app_targets")
    if(dir.exists(od)){
      return(od)
    } else {
      stop(paste("You do not appear to have access to",od))
    }

  } else if(path=="inst"){
    p <- file.path("inst","_targets.R")
    if(file.exists(p)){
      return(p)
    } else {
      stop(paste("You do not appear to have access to",p))
    }
  } else if(path=="pkg"){
    i <- file.path(system.file(package = 'civi'),"_targets.R")
    if(file.exists(i)){
      return(i)
    } else {
      stop(paste("You do not appear to have access to",i))
    }
  } else {
    stop(paste(path,"is not a recognized 'path' argument"))
  }
}
