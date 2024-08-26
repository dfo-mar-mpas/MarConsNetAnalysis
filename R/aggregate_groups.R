aggregate_groups <- function(group_level,group_name,weights=1,...){
  if(all(unlist(lapply(list(...),function(x) "weight" %in% colnames(x))))){
    df <- bind_rows(...) |>
      mutate({{group_level}} := group_name)
  } else {
    df <- bind_rows(...) |>
      mutate({{group_level}} := group_name,
             weight = weights)
  }


}
