# indicators <- climate_change$indicators
# indicator <- indicators[[11]]


# If the trend is statistically significant we can speak to what direction it is going in
# we would also indicate the statistical significance.

# If it is not statistically significantly different from the outside border we could
# conclude that as a whole global warming is playing a role.

## If the results are statistically different than the outter border, we could conclude that
## it is a result of restriction to fishing effort


climate_analysis <- function(indicator) {
  if (grepl("BLANK", trend)) {
    return("There is no data for this indicator yet.")
  } else {
    # 1. Determine the score of outside
    keep <- which(tolower(indicator_to_plot$indicators) == tolower(indicator))

    trend <- indicator_to_plot$trend[keep]
    trend <- sub(".*?(When comparing to outside)", "\\1", trend)
    actual <- str_extract(trend, "(?<=has shown a )\\w+")

    text <- indicator_to_plot$plot[keep]

    if (!(grepl("dataframe", text))) {
      paste0(substr(text, 1, nchar(text) - 1), ", dataframe=TRUE)")
    } else {
      if (grepl("dataframe=FALSE", text)) {
        text <- gsub("dataframe=FALSE", "dataframe=TRUE", text)

      }
    }
    if (!(grepl("outside", text))) {
      paste0(substr(text, 1, nchar(text) - 1), ", outside=TRUE)")
    } else {
      if (grepl("outside=FALSE", text)) {
        text <- gsub("outside=FALSE", "outside=TRUE", text)

      }
    }

    ddff <- eval(parse(text=text))

    desired_direction <- indicator_to_plot$desired_state[keep]
    pval <- coef(summary(lm(ddff$avg_parameter ~ ddff$year)))["ddff$year", "Pr(>|t|)"]


  }
}
