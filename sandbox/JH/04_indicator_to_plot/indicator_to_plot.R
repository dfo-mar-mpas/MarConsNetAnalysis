##  MUST RUN DATA_APP IN MARCONSNETDATA FIRST

library(readxl)
o <- read_excel("./data/indicator_binning.xlsx")
indicators <- c(o$indicators, odf$objectives)
indicator_to_plot <- data.frame(indicator=unique(indicators), plot=rep(0))
indicator_to_plot$plot[which(indicator_to_plot$indicator == "-Support productivity objectives for\n groundfish species of Aboriginal,\n commercial, and/or recreational\n importance, particularly NAFO Division\n 4VW haddock \n" )]
