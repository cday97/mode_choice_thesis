
#plans comparison
read_plans <- function(plans_raw){
  plans <- plans_raw %>% 
    fread(select=c("personId","planElementIndex","planSelected","activityType","activityEndTime","legMode")) %>% as.tibble() %>% 
    mutate(
      personElement = paste0(personId,"_",planElementIndex)
    ) %>%
    filter(planSelected == TRUE)
}

read_wRH_events <- function(events_raw){
  events <- events_raw %>%
    fread(select=c("person","type","mode","tourIndex","activityIndex")) %>%
    filter(type == "TripArrivalEvent") %>%
    #arrange(person,tourIndex,activityIndex) %>%
    mutate(personElement = paste0(person,"_",activityIndex))
}


rh_switch <- function(plans0,plans10){
  final_rh_users <- plans10 %>%
    filter(grepl("ride",legMode))
  
  original_rh_users <- plans0 %>%
    filter(legMode != "") %>% filter(!is.na(legMode)) %>%
    filter(personElement %in% final_rh_users$personElement)
  
  summary <- original_rh_users %>%
    group_by(legMode) %>%
    summarize(n = n()) %>%
    mutate(share = n / sum(n)) %>%
    mutate(share = round(share*100,3))
  summary
}

bind_plans <- function(plans1,plans2,plans3,plans4){
  bind_rows(plans1,plans2,plans3,plans4, .id = "Scenario") %>%
    mutate(ScenarioName = ifelse(Scenario == 1, "wRH-AllModes-AllVars",
                                 ifelse(Scenario == 2, "wRH-AllModes-PathVars",
                                        ifelse(Scenario == 3, "noRH-AllModes-AllVars", "noRH-AllModes-PathVars"))))
}


read_full_plans <- function(path, plansbind){
  plans <- list()
  for (i in 1:13){
    plans[[i]] <- paste0(
      path, i-1, ".plans.csv") %>%
      read_plans()
  }
  plans
}

read_full_events <- function(path){
  beam_events <- list()
  for (i in 1:13){
    beam_events[[i]] <- paste0(
      path, i-1, ".events.csv") %>%
      read_wRH_events()
  }
  beam_events
}

mode_shifts <- function(plans, start, end, mode){
  riders <- plans[[end]] %>%
    filter(grepl("ride",{{mode}}))
  
  selected_plans <- list()
  for(i in start:end){
    selected_plans[[i]] <- plans[[i]] %>%
      filter(personElement %in% riders$personElement) %>%
      mutate(id = row_number(), iteration = i) %>%
      dplyr::select(id, iteration, {{mode}})
  }
  
  plan_mode_shifts <- bind_rows(selected_plans, .id = "bindid") %>%
    mutate(bindid = as.numeric(bindid))
  plan_mode_shifts
}

mode_facet_shifts <- function(plans, start, end, mode){
  riders <-  list()
  for(i in start:(end-1)){
    riders[[i]] <- plans[[i + 1]] %>%
    filter(grepl("ride",{{mode}})) %>%
    mutate(iteration = i+0.5) %>%
    dplyr::select(personElement,iteration, {{mode}})
  }
  
  selected_plans <- list()
  for(i in start:(end-1)){
    selected_plans[[i]] <- plans[[i]] %>%
      filter(personElement %in% riders[[i]]$personElement) %>%
      mutate(iteration = i) %>%
      dplyr::select(personElement, iteration, {{mode}})
  }
  
  plan_mode_shifts <- bind_rows(selected_plans, .id = "bindid") %>%
    mutate(bindid = as.numeric(bindid))
  plan_rh_ends <- bind_rows(riders, .id = "bindid") %>%
    mutate(bindid = as.numeric(bindid))
  
  shifts <- bind_rows(plan_mode_shifts,plan_rh_ends)
  
  
  shifts
}

