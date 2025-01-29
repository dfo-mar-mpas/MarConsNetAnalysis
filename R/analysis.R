#' Analyse Status and Trends of indicators
#'
#' This function automatically calculates status and trends
#' for a list of indicators used in the Marine Conservation
#' Target (MCT) app. It then uses this information to assign
#' a letter grade in two different scenarios:
#'
#' # Scenario 1: If there is a desired trend we want the indicator to go in
#' A grade of either A, B, or C by doing the following:
#'
#' It determines the desired trend of the indicator, then
#' looks at the actual trend of the indicator. If the trend
#' 1) is statistically significant AND matches the desired direction for the
#' indicator, a score of A is assigned.
#' 2) Has no statistically significant change a C is assigned
#' 3) is statistically significant and going in the opposite direction,
#'  a F is assigned.
#'
#' # Scenario 2: If we want the trend of the indicator to be stable
#' A grade of either A or F is assigned based on:
#' 1) If the trend of the indicator has no significant change an A is assigned
#' 2) If there is a significant change, a F in assigned
#'
#' @param DF list of data framed needed for all binned_indicators
#' @param bi binned_indicators data frame likely found in the
#' data folder of the MarConsNetAnalysis package
#' @importFrom car leveneTest
#'
#' @return data frame with trends and status'
#' @export
#'
analysis <- function(DF=NULL, bi=binned_indicators) {

ITP <- bi
ITP$status <- 0
ITP$trend <- 0
ITP$status_grade <- NA
MPAs <- data_CPCAD_areas(data_bioregion("Scotian Shelf"),  zones = FALSE)

for (i in seq_along(ITP$indicators)) {
  message(i)
  itp <- ITP$indicators[i]

  # "A linear regression has shown a  decrease of 0 average  sea_surface_height  over the last 101 years (p=0). The linear trend for the last 5 years was a decrease of
  #0 average  sea_surface_height . When comparing to outside of the protected area, a linear regression has shown a decrease of 0 average  sea_surface_height 2 over the last 101 years (p = 0).
  #The linear trend for the last 5 years was a decrease of 0 average  sea_surface_height 2. The difference between the inside and outside boundary is not significant (p =1)."

  TREND <- "A linear regression has shown a DIRECTION of TRENDY PARAMETER (UNITS) over the last LENGTHYEARS years (PVAL). The linear trend for the last 5 years was a SECONDDIRECTION of LR PARAMETER (UNITS). When comparing to outside of the protected area, a linear regression has shown a DIRECTION2 of TRENDY2 PARAMETER2 over the last LENGTHYEARS2 years (PVAL2). The linear trend for the last 5 years was a SECONDDIRECTION2 of LR2 PARAMETER2"
  STATUS <- "The most recent year (RR) shows a mean of NN PARAMETER (UNITS) (sd=STANDARD1). The most recent 5 year mean was MM PARAMETER (UNITS) (sd=STANDARD2). When comparing to outside the protected area, the most recent year (RR2) shows an average NN2 PARAMETER2 (UNITS) (sd=STANDARD3). The most recent 5 year mean was MM2 PARAMETER2 (UNITS) (sd=STANDARD4)"
  t <- "BLANK"
  y <- "BLANK"
  u <- "BLANK"
  r <- "BLANK"
  n <- "BLANK"
  m <- "BLANK"
  lr <- "BLANK"
  seconddirection <- "BLANK"
  pval <- "BLANK"
  units <- "BLANK"
  direction <- "BLANK"

  t2 <- "BLANK"
  y2 <- "BLANK"
  u2 <- "BLANK"
  r2 <- "BLANK"
  n2 <- "BLANK"
  m2 <- "BLANK"
  lr2 <- "BLANK"
  seconddirection2 <- "BLANK"
  pval2 <- "BLANK"
  standard1 <- "BLANK"
  standard2 <- "BLANK"
  standard3 <- "BLANK"
  standard4 <- "BLANK"
  direction2 <- "BLANK"

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


    if ("whale_biodiversity" %in% names(ddff)) {
      years <-sort(unique(ddff$year))
      wb <- NULL
      for (w in seq_along(years)) {
        keep <- which(ddff$year == years[w])
        wb[[w]] <- length(unique(ddff$whale_biodiversity[keep]))
      }
      ddff <- data.frame(whale_biodiversity=unlist(wb), year=years,type=unique(ddff$type), units=unique(ddff$units))
    }


    if (!(grepl("outside=TRUE", text))) {
      text <- substr(text, 1, nchar(text) - 1)
      text <- paste0(text, ", outside=TRUE)")
      text <- gsub("outside=FALSE", "", text)
    }
    df2 <- try(eval(parse(text=text)), silent=TRUE)
    if (inherits(df2, "try-error")) {
      outsideComparison <- FALSE
    } else {
      outsideComparison <- TRUE
    }


    if ("whale_biodiversity" %in% names(ddff)) {
      years <-sort(unique(df2$year))
      wb <- NULL
      for (w2 in seq_along(years)) {
        keep <- which(df2$year == years[w2])
        wb[[w2]] <- length(unique(df2$whale_biodiversity[keep]))
      }
      df2 <- data.frame(whale_biodiversity=unlist(wb), year=years, type=unique(df2$type), units=unique(df2$units))
    }

    NAMES <- names(ddff)
    parameter <- NAMES[which(!(NAMES %in% c('latitude', "longitude", "type", "geom", "year", "units", "species_name", "area", "depth")))]


      t <- round(unname(coef(lm(ddff[[parameter]] ~ ddff$year))[2]),2)
      y <- range(ddff$year, na.rm=TRUE)[2]-range(ddff$year, na.rm=TRUE)[1]
      u <- parameter
      r <- sort(ddff$year)[length(ddff$year)]

      n <- round(mean(ddff[[parameter]][which(ddff$year == r)], na.rm=TRUE),2)
      standard1 <- round(sd(ddff[[parameter]][which(ddff$year == r)], na.rm=TRUE),2)
      if (is.na(standard1)) {
        standard1 <- "(only one measurement available)"
      }
      m <- round(mean(ddff[[parameter]][which(ddff$year %in% tail(sort(unique(ddff$year)),5))], na.rm=TRUE),2)
      standard2 <- round(sd(ddff[[parameter]][which(ddff$year %in% tail(sort(unique(ddff$year)),5))], na.rm=TRUE),2)

      if (is.na(standard2)) {
        standard2 <- "(only one measurement available)"
      }
      lr <- round(unname(coef(lm(ddff[[parameter]][which(ddff$year %in% tail(sort(unique(ddff$year)),5))] ~ ddff$year[which(ddff$year %in% tail(sort(unique(ddff$year)),5))]))[2]),2)
      seconddirection <- ifelse(lr > 0, "increase", "decrease")
      units <- unique(ddff$units)

      if (outsideComparison) {
      t2 <- round(unname(coef(lm(df2[[parameter]] ~ df2$year))[2]),2)
      y2 <- range(df2$year, na.rm=TRUE)[2]-range(df2$year, na.rm=TRUE)[1]
      u2 <- u
      r2 <- sort(df2$year)[length(df2$year)]
      n2 <- round(mean(df2[[parameter]][which(df2$year == r2)], na.rm=TRUE),2)
      standard3 <- round(sd(df2[[parameter]][which(df2$year == r2)], na.rm=TRUE),2)

      if (is.na(standard3)) {
        standard3 <- "(only one measurement available)"
      }
      m2 <- round(mean(df2[[parameter]][which(df2$year %in% tail(sort(unique(df2$year)),5))], na.rm=TRUE),2)
      standard4 <- round(sd(df2[[parameter]][which(df2$year %in% tail(sort(unique(df2$year)),5))], na.rm=TRUE),2)

      if (is.na(standard4)) {
        standard4 <- "(only one measurement available)"
      }
      lr2 <- round(unname(coef(lm(df2[[parameter]][which(df2$year %in% tail(sort(unique(df2$year)),5))] ~ df2$year[which(df2$year %in% tail(sort(unique(df2$year)),5))]))[2]),2)
      seconddirection2 <- ifelse(lr2 > 0, "increase", "decrease")
      }


  } else {
    outsideComparison <- FALSE
  }

  if (outsideComparison) {
    TREND <- gsub("SECONDDIRECTION2", seconddirection2, TREND)

  if (!(t2 == "BLANK")) {
    if (t2 > 0) {
      TREND <- gsub("DIRECTION2", "increase", TREND)
    } else {
      TREND <- gsub("DIRECTION2", "decrease", TREND)
    }
  } else {
    TREND <- gsub("DIRECTION2", "BLANK", TREND)
  }

    STATUS <- gsub("RR2", r2, STATUS)
    STATUS <- gsub("NN2", n2, STATUS)
    STATUS <- gsub("MM2", m2, STATUS)
    STATUS <- gsub("PARAMETER2", paste0(u), STATUS)
    TREND <- gsub("PARAMETER2", u, TREND)
    STATUS <- gsub("STANDARD3", standard3, STATUS)
    STATUS <- gsub("STANDARD4", standard4, STATUS)
    TREND <- gsub("TRENDY2", t2, TREND)
    TREND <- gsub("LENGTHYEARS2", y2, TREND)
    TREND <- gsub("LR2", lr2, TREND)
    STATUS <- gsub("PARAMETER2", u2, STATUS)

  }

  # JAIMIE
  TREND <- gsub("SECONDDIRECTION", seconddirection, TREND)

  if (!(t == "BLANK")) {
    if (t > 0) {
      TREND <- gsub("DIRECTION ", "increase ", TREND)
    } else {
      TREND <- gsub("DIRECTION ", "decrease ", TREND)
    }
  } else {
    TREND <- gsub("DIRECTION", " BLANK ", TREND)
  }

  TREND <- gsub("TRENDY ", paste0(t, " "), TREND)
  TREND <- gsub("LENGTHYEARS ", paste0(y, " "), TREND)
  TREND <- gsub("PARAMETER", paste0(u, " "), TREND)
  TREND <- gsub("LR ", paste0(lr," "), TREND)

  # STATUS

  STATUS <- gsub("(RR)", paste0(r), STATUS)
  STATUS <- gsub("PARAMETER", paste0(u), STATUS)

  STATUS <- gsub("NN", n, STATUS)

  STATUS <- gsub("MM", m, STATUS)
  STATUS <- gsub("UNITS", units, STATUS)

  STATUS <- gsub("STANDARD1", standard1, STATUS)
  STATUS <- gsub("STANDARD2", standard2, STATUS)

  TREND <- gsub("UNITS", units, TREND)

  if (!(ITP$desired_state[i] %in% "desired")) {
    if (!(ITP$plot[i] == "0")) {
    # STATUS LETTER GRADE
    desired <- ITP$desired_state[i]
    message("desired = ", desired)
    # Do ANOVA to test the effect of year on fish weight.

    #pval <- summary(aov(ddff[[parameter]] ~ factor(year), data = ddff))[[1]]$`Pr(>F)`[1]
    #pval2 <- summary(aov(df2[[parameter]] ~ factor(year), data = df2))[[1]]$`Pr(>F)`[1]

    pval <- coef(summary(lm(ddff[[parameter]] ~ ddff$year)))["ddff$year", "Pr(>|t|)"]
    ddff_clean <- ddff %>% filter(!is.na(parameter))

    if (outsideComparison) {
    pval2 <- coef(summary(lm(df2[[parameter]] ~ df2$year)))["df2$year", "Pr(>|t|)"]
    df2_clean <- df2 %>% filter(!is.na(parameter))

    combined_data <- bind_rows(
      ddff_clean %>% mutate(group = "ddff"),
      df2_clean %>% mutate(group = "df2")
    )

    model <- lm(combined_data[[parameter]] ~ year * group, data = combined_data)

    # Extract the interaction term significance
    interaction_p_value <- summary(model)$coefficients["year:groupdf2", "Pr(>|t|)"]

    TREND <- gsub("PVAL2", paste0("p = ", round(pval2,2)), TREND)

    }

    if (t < 0) {
      actual <- "decrease"
    } else {
      actual <- "increase"
    }

    TREND <- gsub("PVAL", paste0("p=", round(pval,2)), TREND)

    # NEW A-F Assigning
    if (!(desired == "stable")) {
    if (pval < 0.05 & desired == actual) {
      ITP$status_grade[i] <- "A"
    } else if (pval > 0.05) {
      ITP$status_grade[i] <- "C"
    } else if (pval < 0.05 & (!(desired == actual))) {
      ITP$status_grade[i] <- "F"
    }
    } else {
      # A or F scoring
      if (pval < 0.05) {
        ITP$status_grade[i] <- "F"
      } else {
        ITP$status_grade[i] <- "A"

      }
    }

    if (outsideComparison) {
    if (interaction_p_value < 0.05) {
      TREND <- paste0(TREND, ". The difference between the inside and outside boundary is significant (p =",round(interaction_p_value,2), ").")
    } else {
      TREND <- paste0(TREND, ". The difference between the inside and outside boundary is not significant (p =",round(interaction_p_value,2), ").")
    }
    } else {
      TREND <- paste0(TREND, "There is no outside comparison available.")

    }

    message(paste0("pval = ", pval, " and desired = ", desired, " and actual = ", actual, " therefore grade = ", ITP$status_grade[i], " for ", ITP$indicators[i]))
    }

  }

  if (!(outsideComparison)) {
    TREND <- gsub("When comparing to outside of the protected area.*?There is no outside comparison available\\.",
                  "There is no outside comparison available.", TREND)

    STATUS <- gsub("When comparing to.*", "No outside comparison was available.", STATUS)
  }
  message("TREND = ", TREND)

  ITP$trend[i] <- TREND
  ITP$status[i] <- STATUS
}

return(ITP)
}

