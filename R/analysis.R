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
#'
#' @return data frame with trends and status'
#' @export
#'
analysis <- function(DF=list(bloom_df=bloom_df, all_haddock=all_haddock, gsdet=gsdet, zooplankton=zooplankton, surface_height=surface_height, whale_sighting), bi=binned_indicators, Discrete_Occupations_Sections=azmpdata::Discrete_Occupations_Sections) {

ITP <- bi
ITP$status <- 0
ITP$trend <- 0
ITP$status_grade <- NA
MPAs <- data_CPCAD_areas(data_bioregion("Scotian Shelf"),  zones = FALSE)

for (i in seq_along(ITP$indicators)) {
  message(i)
  itp <- ITP$indicators[i]

  TREND <- "A linear regression has shown a XX of YY UU over the last ZZ years (PVAL). The linear trend for the last 5 years was a TID of LR UU. When comparing to outside of the protected area, a linear regression has shown a XX2 of YY2 UU2 over the last ZZ2 years (PVAL2). The linear trend for the last 5 years was a TID2 of LR2 UU2"
  STATUS <- "The most recent year (RR) shows NN UU. The most recent 5 year mean was MM UU. When comparing to outside the protected area, the most recent year (RR2) shows NN2 UU2. The most recent 5 year mean was MM2 UU2"
  t <- "BLANK"
  y <- "BLANK"
  u <- "BLANK"
  r <- "BLANK"
  n <- "BLANK"
  m <- "BLANK"
  lr <- "BLANK"
  tid <- "BLANK"
  pval <- "BLANK"

  t2 <- "BLANK"
  y2 <- "BLANK"
  u2 <- "BLANK"
  r2 <- "BLANK"
  n2 <- "BLANK"
  m2 <- "BLANK"
  lr2 <- "BLANK"
  tid2 <- "BLANK"
  pval2 <- "BLANK"

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
  TREND <- gsub("UU", paste0(u, " "), TREND)
  TREND <- gsub("LR ", paste0(lr," "), TREND)
  TREND <- gsub("TID ", paste0(tid," "), TREND)


  # STATUS
  STATUS <- gsub("(RR)", paste0(r), STATUS)
  STATUS <- gsub("U ", paste0(u," "), STATUS)
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

  # STATUS
  STATUS <- gsub("RR2", r2, STATUS)
  STATUS <- gsub("UU2", u2, STATUS)
  STATUS <- gsub("NN2", n2, STATUS)
  STATUS <- gsub("MM2", m2, STATUS)


  if (!(ITP$desired_state[i] %in% "desired")) { # FIXME
    if (!(ITP$plot[i] == "0")) {
    # STATUS LETTER GRADE
    desired <- ITP$desired_state[i]
    message("desired = ", desired)
    pval <- coef(summary(lm(ddff$avg_parameter ~ ddff$year)))["ddff$year", "Pr(>|t|)"]
    pval2 <- coef(summary(lm(df2$avg_parameter ~ df2$year)))["df2$year", "Pr(>|t|)"]

    # Determine if the inside/outside different is statistically significant
    # Clean data: Remove rows with NaN values
    ddff_clean <- ddff %>% filter(!is.na(avg_parameter))
    df2_clean <- df2 %>% filter(!is.na(avg_parameter))

    combined_data <- bind_rows(
      ddff_clean %>% mutate(group = "ddff"),
      df2_clean %>% mutate(group = "df2")
    )
    model <- lm(avg_parameter ~ year * group, data = combined_data)


    # Extract the interaction term significance
    interaction_p_value <- summary(model)$coefficients["year:groupdf2", "Pr(>|t|)"]

    if (t < 0) {
      actual <- "decrease"
    } else {
      actual <- "increase"
    }
    TREND <- gsub("PVAL2", paste0("p = ", round(pval2,2)), TREND)

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


    if (interaction_p_value < 0.05) {
      TREND <- paste0(TREND, ". The difference between the inside and outside boundary is significant (p =",round(interaction_p_value,2), ").")
    } else {
      TREND <- paste0(TREND, ". The difference between the inside and outside boundary is not significant (p =",round(interaction_p_value,2), ").")

    }

    message(paste0("pval = ", pval, " and desired = ", desired, " and actual = ", actual, " therefore grade = ", ITP$status_grade[i], " for ", ITP$indicators[i]))
  }
  }
  message("TREND = ", TREND)


  ITP$trend[i] <- TREND
  ITP$status[i] <- STATUS


}

return(ITP)


}

