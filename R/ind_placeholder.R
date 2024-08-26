ind_placeholder <- function(ind_name = paste0("placeholder_",round(runif(1,1,100)))){
  data.frame(ind_name = ind_name,ind_value = runif(1,min=0,max=100))
  }
