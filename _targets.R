# load packages that are NOT tracked by targets
library(targets)
library(tarchetypes)
library(dplyr)
library(arcpullr)
library(sf)

tar_option_set(format = "qs") # new default format

# # delete targets that are older than 30 days
# if(file.exists("_targets/meta/meta")){
#   tar_meta()[["name"]][
#     Sys.time()-tar_meta()[["time"]]>30*24 &
#       !is.na(tar_meta()[["time"]])] |>
#   all_of() |>
#   tar_delete()
# }


# sourced functions will be tracked!
# For functions from another package, sourcing directly from github to avoid local directory issues
source("https://raw.githubusercontent.com/dfo-mar-mpas/MarConsNetData/main/R/data_bioregion.R")
source("https://raw.githubusercontent.com/dfo-mar-mpas/MarConsNetData/main/R/data_CPCAD_areas.R")
source("https://raw.githubusercontent.com/dfo-mar-mpas/MarConsNetData/main/R/data_OBIS.R")
source("https://raw.githubusercontent.com/dfo-mar-mpas/MarConsNetData/main/R/data_DFO_sp_lists.R")

##### create list of targets ####
list(
  #### Load spatial data: ####
  # Download Scotian Shelf bioregion
  tar_target(name=bioregion,
             data_bioregion()),

  # get the Protected and Conserved areas in the bioregion
  tar_target(name=consAreas,
             data_CPCAD_areas(bioregion,zones=TRUE)
             ),

  #### Create species lists ####
  # extract OBIS data:
  tar_target(name=OBISdf,
             data_OBIS(bioregion),
             format = "feather",
             cue = tar_cue_age(OBISdf,
                               as.difftime(1,units = "days"))
             # or use this for 'quick' data downloads for testing
             # consAreas  |>
             #   st_buffer(10000) |>
             #   data_OBIS()
  ),

  # filter OBISdf to return only records inside consAreas boundaries
  tar_target(name=OBISinNetwork,
             OBISdf |>
               st_as_sf(coords = c("decimalLongitude","decimalLatitude"),
                        remove = FALSE,
                        crs=4326) |>
               st_filter(consAreas) |>
               as.data.frame()
  ),

  # return only the species name from OBISdf
  tar_target(name=spListBioregion,
             OBISdf |>
               select(species) |>
               unique() |>
               unlist()
  ),

  # return only the species name from OBISinNetwork
  tar_target(name=spListInNetwork,
             OBISinNetwork |>
               select(species) |>
               unique() |>
               unlist()
             ),

  # return species at risk (SAR) data.frame
  tar_target(name=SARdf,
             data_DFO_sp_lists(status = "At-Risk") |>
               filter(grepl("Maritimes",data.region_english)|
                        grepl("Atlantic",data.region_english),
                      data.group_english!="Fishes (Freshwater)",
                      data.group_english!="Fishes (freshwater) ",
                      data.status_sara_english!="Extirpated",
                      data.status_sara_english!="Extinct",
                      data.name_english!="Brook Floater",
                      data.name_english!="Acadian Redfish Bonne Bay population")
  ),

  # return only the species name from SARdf
  tar_target(name=SAR,
             SARdf |>
               select(data.name_latin) |>
               unique() |>
               unlist()
  ),


  # filter OBISdf to return only species at risk (SAR)
  tar_target(name=OBIS_only_SAR,
             OBISdf |>
               filter(species %in% SAR)
  ),


  # return only the species name from
  tar_target(name=SARinOBIS,
             OBIS_only_SAR |>
               select(species) |>
               unique() |>
               unlist()
  ),

  # return only the SAR species name from OBISinNetwork
  tar_target(SARinOBISinNetwork,
             OBISinNetwork[OBISinNetwork$species %in% SAR,]|>
               select(species) |>
               unique() |>
               unlist()
  ),

  #### Calculate indicators ####
  # indicator: representation of SAR in the network
  tar_target(ind_SAR_representation,
             length(SARinOBISinNetwork)/length(SARinOBIS)
  ),


  # indicator: representation of all species in the network
  tar_target(ind_sp_representation,
             length(spListInNetwork)/length(spListBioregion)
  ),

  #### Indicator binning ####
  # Species Diversity
  tar_target(Species_Diversity,
             mean(ind_sp_representation,ind_SAR_representation)
  )
)
