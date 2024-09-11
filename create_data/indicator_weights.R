require(dplyr)

indicatorbins <- data.frame(objective=rep(c("Biodiversity",
                                           "Habitat",
                                           "Productivity"),
                                         times=c(3,5,3)),
                            indicator_bin=c("Genetic Diversity",
                                     "Species Diversity",
                                     "Functional Diversity",

                                     "Environmental Representativity",
                                     "Key Fish Habitat",
                                     "Connectivity",
                                     "Uniqueness",
                                     "Threats to Habitat",

                                     "Biomass Metrics",
                                     "Structure and Function",
                                     "Threats to Productivity"))


indicator_weights <- read.csv("create_data/indicator_binning.csv") |>
  tidyr::separate_longer_delim(cols = "indicator_bin",delim = ";") |>
  mutate(indicator_bin = trimws(indicator_bin)) |>
  left_join(indicatorbins,by = "indicator_bin") |>
  group_by(objective) |>
  mutate(nbins = length(unique(indicator_bin))) |>
  group_by(objective,indicator_bin) |>
  mutate(weight = 1/3/nbins/n()*100) |>
  select(-nbins)


usethis::use_data(indicator_weights)
sinew::makeOxygen(indicator_weights)
