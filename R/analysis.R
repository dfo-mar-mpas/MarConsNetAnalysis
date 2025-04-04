#' Analyse Status and Trends of indicators
#'
#' This function automatically calculates status and trends
#' for a list of indicators used in the Marine Conservation
#' Target (MCT) app. This function looks for a shape file
#' in MarConsNetAnalaysis/data that is made with the function
#' make.names() of the area of interest.
#' @param data a indicator in the ind_ format.
#' @param type a character of either 'trend' or 'status' indicating if the
#'
#'
#' @return data frame with trends and status'
#' @export
#'
analysis <- function(data = NULL, type=NULL) {
  datasets <- data$data

  good <- which(unlist(lapply(datasets, function(x)!is.null(x)))))

  returns <- NULL
  for (i in seq_along(data$areaID)) {
    message(i)
    if (i %in% good) {
      # We have data
      DATA <- datasets[[i]]
      DATA <- DATA[-(which(is.na(DATA[[which(!(names(DATA) %in% c("year", "geometry")))]]))),]

      # Looking at the last 5 years sampled
      DATA_5_YEARS <- DATA[which(DATA$year %in% tail(sort(unique(DATA$year)), 5)), ]

      if (type == 'trend') {

        if (length(unique(DATA$year)) > 1) {
          y <- DATA[[which(!(names(DATA) %in% c("year", "geometry")))]]
          x <- DATA$year
          model <- lm(y ~ x)
          current_trend_value <- unname(coef(model)[2])
          if (current_trend_value > 0) {
            current_trend_direction <- "increase"
          } else {
            current_trend_direction <- "decrease"
          }
          y_5_years <- DATA_5_YEARS[[which(!(names(DATA_5_YEARS) %in% c("year", "geometry")))]]
          x_5_years <- DATA_5_YEARS$year
          model_5_years <- lm(y_5_years ~ x_5_years)

          five_year_trend_value <- unname(coef(model_5_years)[2])

          if (five_year_trend_value > 0) {
            trend_direction_5_years <- "increase"
          } else {
            trend_direction_5_years <- "decrease"
          }
          #if (any(grepl(make.names(data$areaID[i]), list.files(system.file("data", package = "MarConsNetAnalysis"), pattern="shp")))) {
          #outside <- TRUE

          #  When comparing to outside of the protected area, a linear regression has shown a TREND_DIRECTION_OUTSIDE of TREND_VALUE_OUTSIDE PARAMETER
          # over the last LENGTHYEARS_OUTSIDE years (PVAL_OUTSIDE). The linear trend for the last 5 years was a TREND_VALUE_OUTSIDE_5_YEARS of
          # TREND_VALUE_OUTSIDE_5_YEARS PARAMETER")
          #}
          returns[i] <- paste0(
            "A linear regression has shown a ",current_trend_direction," of ",round(current_trend_value,2),"(",data$units[i],
            "),over ",length(unique(DATA$year))," years (pval=",round(summary(model)$coefficients[2, 4],2),").The linear trend for the last 5 years sampled (",
            paste0(tail(sort(
              unique(DATA$year)
            ), 5), collapse = ","),
            "), showed a ", trend_direction_5_years," of ",round(five_year_trend_value,2)," ",names(DATA_5_YEARS)[which(!(names(DATA_5_YEARS) %in% c("year", "geometry")))],
            " (" , data$units[i],") (pval =",round(summary(model_5_years)$coefficients[2, 4],2),")."
          )

        } else {
          returns[i] <- "Only one year of data available, and therefore no trend analysis available."
        }

      } else if (type == 'status') {
        # STATUS
        #When comparing to outside the protected area, the most
        #recent year (RECENTYEAR_OUTSIDE) shows an average OUTSIDE_AVERAGE PARAMETER (UNITS) (sd=STANDAND_DEVIATION_OUTSIDE). The most recent
        #5 year mean was MEAN_OUTSIDE_5_YEARS PARAMETER (UNITS) (sd=STANDAND_DEVIATION_OUTSIDE_5_YEARS

        returns[i] <- paste0("The most recent year ," ,tail(DATA$year, 1),", shows a mean of ", round(mean(DATA[[which(!(names(DATA) %in% c("year", "geometry")))]]),2), " (", data$units[i], ") (sd=",round(mean(DATA[[which(!(names(DATA) %in% c("year", "geometry")))]]),2) ,
                             "). The most recent 5 years of sampling (",paste0(tail(sort(
                               unique(DATA$year)
                             ), 5), collapse = ","),") showed a mean of ",round(mean(DATA_5_YEARS[[which(!(names(DATA_5_YEARS) %in% c("year", "geometry")))]]),2),
                             "(",data$units[i],") (sd=",round(sd(DATA_5_YEARS[[which(!(names(DATA_5_YEARS) %in% c("year", "geometry")))]]),2),").")
      }
    } else {
      returns[i] <- "TBD"
    }

  }
}
