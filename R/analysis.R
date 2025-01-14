#' Analyse Status and Trends of indicators
#'
#' This function automatically calculates status and trends
#' for a list of indicators used in the Marine Conservation
#' Target (MCT) app. It then uses this information to assign
#' a letter grade for the status of either A, B,
#' or C by doing the following:
#'
#' It determines the desired trend of the indicator, then
#' looks at the actual trend of the indicator. If the trend
#' 1) is statistically significant AND matches the desired direction for the
#' indicator, a score of A is assigned.
#' 2) is not statistically significant but matches the desired direction for
#' the indicator, a B is assigned
#' 3) Has no change a C is assigned
#' 4) is not statistically significant and going is the opposite direction
#' of the desired direction a D is assigned
#' 5) is statistically significant and going in the opposite direction,
#'  a F is assigned.
#'
#' @param DF list of data framed needed for all binned_indicators
#' @param bi binned_indicators data frame likely found in the
#' data folder of the MarConsNetAnalysis package
#'
#' @return data frame with trends and status'
#' @export
#'
analysis <- function(DF=list(bloom_df=bloom_df, all_haddock=all_haddock, gsdet=gsdet, zooplankton=zooplankton, surface_height=surface_height), bi=binned_indicators, Discrete_Occupations_Sections=azmpdata::Discrete_Occupations_Sections) {

ITP <- bi
ITP$status <- 0
ITP$trend <- 0
ITP$status_grade <- 0
MPAs <- data_CPCAD_areas(data_bioregion("Scotian Shelf"),  zones = FALSE)


for (i in seq_along(ITP$indicators)) {
  message(i)

  itp <- ITP$indicators[i]

  TREND <- "A linear regression has shown a XX  of YY UU over the last ZZ years. The linear trend for the last 5 years was a TID of LR UU. When comparing to outside of the protected area, a linear regression has shown a XX2  of YY2 UU2 over the last ZZ2 years. The linear trend for the last 5 years was a TID2 of LR2 UU2"
  STATUS <- "The most recent year (RR) shows NN UU. The most recent 5 year mean was MM UU. When comparing to outside the protected area, the most recent year (RR2) shows NN2 UU2. The most recent 5 year mean was MM2 UU2"
  t <- "BLANK"
  y <- "BLANK"
  u <- "BLANK"
  r <- "BLANK"
  n <- "BLANK"
  m <- "BLANK"
  lr <- "BLANK"
  tid <- "BLANK"

  t2 <- "BLANK"
  y2 <- "BLANK"
  u2 <- "BLANK"
  r2 <- "BLANK"
  n2 <- "BLANK"
  m2 <- "BLANK"
  lr2 <- "BLANK"
  tid2 <- "BLANK"

  # Filling in actual status and trends
  if (!(ITP$plot[i]) == "0") {

    text <- ITP$plot[i]
    if (!(grepl("dataframe=TRUE", text))) {
      text <- substr(text, 1, nchar(text) - 1)
      text <- paste0(text, ", dataframe=TRUE)")
      text <- gsub("dataframe=FALSE", "", text)
    }
    old <- sub(".*=(.*?),.*", "\\1", text)
    if (grepl("azmpdata::", old)) {
      old <- gsub("azmpdata::","", old)
    }

    text <- sub("=(.*?),", "=new_value,", text)
    text <- gsub("new_value", "DF[[which(names(DF) == old)]]", text)

    ddff <- eval(parse(text=text))

    if (!(grepl("outside=TRUE", text))) {
      text <- substr(text, 1, nchar(text) - 1)
      text <- paste0(text, ", outside=TRUE)")
      text <- gsub("outside=FALSE", "", text)
    }
    df2 <- eval(parse(text=text))


    if ("avg_parameter" %in% names(ddff)) {
      t <- round(unname(coef(lm(ddff$avg_parameter ~ ddff$year))[2]),2)
      y <- length(ddff$year)
      u <- paste0("average ", unique(ddff$type), " ", unique(ddff$parameter_name))
      r <- sort(ddff$year)[length(ddff$year)]
      n <- ddff$avg_parameter[which(ddff$year == r)]
      m <- round(mean(ddff$avg_parameter[which(ddff$year %in% tail(sort(ddff$year),5))], na.rm=TRUE),2)
      lr <- round(unname(coef(lm(ddff$avg_parameter[which(ddff$year %in% tail(sort(ddff$year),5))] ~ ddff$year[which(ddff$year %in% tail(sort(ddff$year),5))]))[2]),2)
      tid <- ifelse(lr > 0, "increase", "decrease")


      t2 <- round(unname(coef(lm(df2$avg_parameter ~ df2$year))[2]),2)
      y2 <- length(df2$year)
      u2 <- u
      r2 <- sort(df2$year)[length(df2$year)]
      n2 <- df2$avg_parameter[which(df2$year == r2)]
      m2 <- round(mean(df2$avg_parameter[which(df2$year %in% tail(sort(df2$year),5))], na.rm=TRUE),2)
      lr2 <- round(unname(coef(lm(df2$avg_parameter[which(df2$year %in% tail(sort(df2$year),5))] ~ df2$year[which(df2$year %in% tail(sort(df2$year),5))]))[2]),2)
      tid2 <- ifelse(lr2 > 0, "increase", "decrease")

    } else {
      t <- round(unname(coef(lm(df$abundance ~ ddff$year))[2]),2)
      y <- length(ddff$year)
      u <- "average # of haddock per tow"
      r <- sort(ddff$year)[length(ddff$year)]
      n <- ddff$abundance[which(ddff$year == r)]
      m <- mean(ddff$abundance[which(ddff$year %in% tail(sort(ddff$year),5))], na.rm=TRUE)
      lr <- round(unname(coef(lm(ddff$abundance[which(ddff$year %in% tail(sort(ddff$year),5))] ~ ddff$year[which(ddff$year %in% tail(sort(ddff$year),5))]))[2]),2)
      tid <- ifelse(lr > 0, "increase", "decrease")


      t2 <- round(unname(coef(lm(df2$abundance ~ df2$year))[2]),2)
      y2 <- length(df2$year)
      u2 <- "average # of haddock per tow"
      r2 <- sort(df2$year)[length(df2$year)]
      n2 <- df2$abundance[which(df2$year == r2)]
      m2 <- mean(df2$abundance[which(df2$year %in% tail(sort(df2$year),5))], na.rm=TRUE)
      lr2 <- round(unname(coef(lm(df2$abundance[which(df2$year %in% tail(sort(df2$year),5))] ~ df2$year[which(df2$year %in% tail(sort(df2$year),5))]))[2]),2)
      tid2 <- ifelse(lr2 > 0, "increase", "decrease")
    }


  }

  if (!(t == "BLANK")) {
    if (t > 0) {
      TREND <- gsub("XX ", " increase ", TREND)
    } else {
      TREND <- gsub("XX ", " decrease ", TREND)
    }
  } else {
    TREND <- gsub("XX ", " BLANK ", TREND)
  }


  TREND <- gsub("YY ", paste0(t, " "), TREND)
  TREND <- gsub("ZZ ", paste0(y, " "), TREND)
  TREND <- gsub("UU.", paste0(u, " ."), TREND)
  TREND <- gsub("LR ", paste0(lr," "), TREND)
  TREND <- gsub("TID ", paste0(tid," "), TREND)


  # STATUS
  STATUS <- gsub("(RR)", paste0(r), STATUS)
  STATUS <- gsub("UU.", paste0(u," ."), STATUS)
  STATUS <- gsub("NN ", paste0(n," "), STATUS)
  STATUS <- gsub("MM ", paste0(m," "), STATUS)

  ## NEW
  if (!(t2 == "BLANK")) {
    if (t2 > 0) {
      TREND <- gsub("XX2", "increase", TREND)
    } else {
      TREND <- gsub("XX2", "decrease", TREND)
    }
  } else {
    TREND <- gsub("XX2", "BLANK", TREND)
  }


  TREND <- gsub("YY2", t2, TREND)
  TREND <- gsub("ZZ2", y2, TREND)
  TREND <- gsub("UU2", u2, TREND)
  TREND <- gsub("LR2", lr2, TREND)
  TREND <- gsub("TID2", tid2, TREND)



  ITP$trend[i] <- TREND

  # STATUS
  STATUS <- gsub("RR2", r2, STATUS)
  STATUS <- gsub("UU2", u2, STATUS)
  STATUS <- gsub("NN2", n2, STATUS)
  STATUS <- gsub("MM2", m2, STATUS)

  ITP$status[i] <- STATUS

  if (!(ITP$desired_state[i] %in% c("desired", "stable"))) { # FIXME
    if (!(ITP$plot[i] == "0")) {
    # STATUS LETTER GRADE
    desired <- ITP$desired_state[i]
    message("desired = ", desired)
    pval <- coef(summary(lm(ddff$avg_parameter ~ ddff$year)))["ddff$year", "Pr(>|t|)"]
    if (t < 0) {
      actual <- "decrease"
    } else {
      actual <- "increase"
    }


    #' if (!(pval < 0.05)) {
    #'   # B) The trend is not statistically significant (i.e. there is no true trend) a grade of B is assigned.
    #'   ITP$status_grade[i] <- "B"
    #' } else {
    #'   if (desired == actual) {
    #'     # A) The trend is statistically significant AND matches matches the desired direction
    #'     # for the indicator, a score of A is assigned.
    #'     ITP$status_grade[i] <- "A"
    #'
    #'   } else {
    #'     #' C) The trend is statistically significant AND going in the opposite direction
    #'     #' to the of the desired direction for that indicator it receives a C.
    #'     ITP$status_grade[i] <- "C"
    #'   }
    #' }


    # NEW A-F Assigning
    if (pval < 0.05 & desired == actual) {
      ITP$status_grade[i] <- "A"
    } else if (pval > 0.05 & desired == actual) {
      ITP$status_grade[i] <- "B"
    } else if (round(t,0) == 0) {
      ITP$status_grade[i] <- "C"
    } else if (pval > 0.05 & (!(desired == actual))) {
      ITP$status_grade[i] <- "D"
    } else if (pval < 0.05 & (!(desired == actual))) {
      ITP$status_grade[i] <- "F"
    }

    message(paste0("pval = ", pval, " and desired = ", desired, " and actual = ", actual, " therefore grade = ", ITP$status_grade[i], " for ", ITP$indicators[i]))

  }
  }


}

return(ITP)


}

