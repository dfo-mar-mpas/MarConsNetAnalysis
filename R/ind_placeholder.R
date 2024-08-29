ind_placeholder <- function(ind_name = paste0("placeholder_",round(runif(1,1,100))),areas,areaName = "NAME_E"){
  data.frame(area_name = areas[[areaName]],
             ind_name = ind_name,
             ind_value = runif(nrow(areas),min=0,max=100))
  }
