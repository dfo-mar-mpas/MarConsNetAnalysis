
  library(targets)
  library(dplyr)
  
  # Define the indicators
  ind_calc_indicator1 <- function() { runif(1, min = 0, max = 30) }
  ind_calc_indicator2 <- function() { runif(1, min = 50, max = 80) }
  ind_calc_indicator3 <- function() { runif(1, min = 1, max = 80) }
  ind_calc_indicator4 <- function() { runif(1, min = 10, max = 40) }
  ind_calc_indicator5 <- function() { runif(1, min = 20, max = 70) }
  ind_calc_indicator6 <- function() { runif(1, min = 5, max = 90) }
  ind_calc_indicator7 <- function() { runif(1, min = 15, max = 60) }
  ind_calc_indicator8 <- function() { runif(1, min = 25, max = 55) }
  ind_calc_indicator9 <- function() { runif(1, min = 35, max = 65) }
  ind_calc_indicator10 <- function() { runif(1, min = 45, max = 75) }
  ind_calc_indicator11 <- function() { runif(1, min = 55, max = 85) }
  
  # Define the bins as separate targets
  list(
  tar_target(bin1, data.frame(bin = "threats to productivity", value = ind_calc_indicator1(), name = "indicator1", weight = 1)),
  tar_target(bin2, data.frame(bin = "threats to habitat", value = ind_calc_indicator2(), name = "indicator2", weight = 2)),
  tar_target(bin3, data.frame(bin = "genetic diversity", value = ind_calc_indicator3(), name = "indicator3", weight = 5)),
  tar_target(bin4, data.frame(bin = "species diversity", value = ind_calc_indicator4(), name = "indicator4", weight = 1)),
  tar_target(bin5, data.frame(bin = "functional diversity", value = ind_calc_indicator5(), name = "indicator5", weight = 2)),
  tar_target(bin6, data.frame(bin = "environmental representativity", value = ind_calc_indicator6(), name = "indicator6", weight = 5)),
  tar_target(bin7, data.frame(bin = "key fish habitat", value = ind_calc_indicator7(), name = "indicator7", weight = 1)),
  tar_target(bin8, data.frame(bin = "uniqueness", value = ind_calc_indicator8(), name = "indicator8", weight = 2)),
  tar_target(bin9, data.frame(bin = "connectivity", value = ind_calc_indicator9(), name = "indicator9", weight = 5)),
  tar_target(bin10, data.frame(bin = "biomass metrics", value = ind_calc_indicator10(), name = "indicator10", weight = 1)),
  tar_target(bin11, data.frame(bin = "structure and function", value = ind_calc_indicator11(), name = "indicator11", weight = 2)),
  
  
  tar_target(productivity, {bind_rows(bin1, bin10, bin11) %>% 
      mutate(objective = "Productivity")}),
  
  tar_target(biodivesity, {bind_rows(bin3, bin4, bin5) %>% 
      mutate(objective = "Biodivesity")}),
  
  tar_target(habitat, {bind_rows(bin2, bin6, bin7, bin8, bin9) %>% 
      mutate(objective = "Habitat")}),
  
  tar_target(Objectives, {bind_rows(productivity, biodivesity, habitat)})
  
  )
  
 
  
  

  
 
