#' Analyse Status and Trends of indicators
#'
#' This function automatically calculates status and trends
#' for a list of indicators used in the Marine Conservation
#' Target (MCT) app
#'
#' @param bi binned_indicators data frame likely found in the
#' data folder of the MarConsNetAnalysis package
#' @param mpa CPCAD a "sf" "dataframe" object from [data_CPCAD_areas()]
#'
#' @return data frame with trends and status'
#' @export
#'
analysis <- function(bi=binned_indicators, mpa=MPAs) {
ITP <- bi
ITP$status <- 0
ITP$trend <- 0

for (i in seq_along(ITP$indicators)) {
  message(i)
  itp <- ITP$indicators[i]


  TREND <- "A linear regression has shown a XX  of YY UU over the last ZZ years. The linear trend for the last 5 years was a TID of LR UU."
  STATUS <- "The most recent year (RR) shows NN UU. The most recent 5 year mean was MM UU."
  t <- "BLANK"
  y <- "BLANK"
  u <- "BLANK"
  r <- "BLANK"
  n <- "BLANK"
  m <- "BLANK"
  lr <- "BLANK"
  tid <- "BLANK"

  # Filling in actual status and trends
  if (!(ITP$plot[i]) == 0) {
    if (grepl("mpa=MPAs", ITP$plot[i])) {
      ITP$plot[i] <- gsub("mpa=MPAs", "mpa=mpa", ITP$plot[i])
    }

    # We actually have a plot
    if (!(ITP$plot[i] == "plot_rv_abundance(RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]])")) {
      if (grepl("dataframe", ITP$plot[i])) {
        df <- eval(parse(text=ITP$plot[i]))

      } else {
        df <- eval(parse(text=paste0(substr(ITP$plot[i], 1, nchar(ITP$plot[i]) - 1), ",dataframe=TRUE)")))
      }
    } else {
      df <- RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]]
    }
    paste0(names(df), collapse=" , ")
    if ("avg_parameter" %in% names(df)) {
      t <- round(unname(coef(lm(df$avg_parameter ~ df$year))[2]),2)
      y <- length(df$year)
      u <- "average surface parameter" # FIXME
      r <- sort(df$year)[length(df$year)]
      n <- df$avg_parameter[which(df$year == r)]
      m <- mean(df$avg_parameter[which(df$year %in% tail(sort(df$year),5))], na.rm=TRUE)
      lr <- round(unname(coef(lm(df$avg_parameter[which(df$year %in% tail(sort(df$year),5))] ~ df$year[which(df$year %in% tail(sort(df$year),5))]))[2]),2)
      tid <- ifelse(lr > 0, "increase", "decrease")
    } else {
      t <- round(unname(coef(lm(df$abundance ~ df$year))[2]),2)
      y <- length(df$year)
      u <- "average # of haddock per tow"
      r <- sort(df$year)[length(df$year)]
      n <- df$abundance[which(df$year == r)]
      m <- mean(df$abundance[which(df$year %in% tail(sort(df$year),5))], na.rm=TRUE)
      lr <- round(unname(coef(lm(df$abundance[which(df$year %in% tail(sort(df$year),5))] ~ df$year[which(df$year %in% tail(sort(df$year),5))]))[2]),2)
      tid <- ifelse(lr > 0, "increase", "decrease")

    }


  }


  if (!(t == "BLANK")) {
    if (t > 0) {
      TREND <- gsub("XX", "increase", TREND)
    } else {
      TREND <- gsub("XX", "decrease", TREND)
    }
  } else {
    TREND <- gsub("XX", "BLANK", TREND)
  }


  TREND <- gsub("YY", t, TREND)
  TREND <- gsub("ZZ", y, TREND)
  TREND <- gsub("UU", u, TREND)
  TREND <- gsub("LR", lr, TREND)
  TREND <- gsub("TID", tid, TREND)



  ITP$trend[i] <- TREND

  # STATUS
  STATUS <- gsub("RR", r, STATUS)
  STATUS <- gsub("UU", u, STATUS)
  STATUS <- gsub("NN", n, STATUS)
  STATUS <- gsub("MM", m, STATUS)

  ITP$status[i] <- STATUS

}

return(ITP)


}

