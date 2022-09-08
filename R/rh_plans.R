
#plans comparison
read_plans <- function(plans_raw){
  plans <- plans_raw %>% 
    fread(select=c("personId","planElementIndex","planSelected","activityType","activityEndTime","legMode")) %>% as.tibble() %>% 
    mutate(
      personElement = paste0(personId,"_",planElementIndex)
    ) %>%
    filter(planSelected == TRUE)
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

mode_shifts <- function(plans){
  riders <- plans[[13]] %>%
    filter(grepl("ride",legMode))
  
  selected_plans <- list()
  for(i in 1:13){
    selected_plans[[i]] <- plans[[i]] %>%
      filter(personElement %in% riders$personElement) %>%
      mutate(id = row_number()) %>%
      select(id,legMode)
  }
  
  plan_mode_shifts <- bind_rows(selected_plans, .id = "iteration") %>%
    mutate(iteration = as.numeric(iteration))
  plan_mode_shifts
}

