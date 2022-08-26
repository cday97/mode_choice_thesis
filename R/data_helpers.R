#' sets column types by name
get_coltypes <- function(event_cols){
  coltypes <- read_csv(event_cols, col_names = F)
  coltypes <- set_names(pull(coltypes, 2), pull(coltypes ,1))
  coltypes
}


#' join tibbles
all_join <- function(events_list, func, join_col, col_1_name, ...){
  
  events <- future_map(events_list, func, ...)
  
  full <- events[[1]]
  
  for(i in 2:length(events_list)){
    full <- full %>% 
      full_join(events[[i]], by = join_col)
  }
  
  full %>% 
    `colnames<-`(c(col_1_name, names(events_list)))
}

rbind_join <- function(events_list, func){
  full <-list()
  for(i in 1:length(events_list)){
    full[[i]] <- func(events_list[[i]]) %>% 
      mutate(ScenarioName = names(events_list)[i])
  }
  
  bind_rows(full, .id = "id") %>%
    select(-id)
}

