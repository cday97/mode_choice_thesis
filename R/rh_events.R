
#events_raw <- "data/events/12.events-15pct-wRH-all-all.csv"
#events1 <- read_events(events_raw,cols)
#mc1 <- mode_choice(events1)
#ta1 <- trip_arrivals(events1)

#rhpass1 <- rh_pass(events1)
#rhtimes1 <- rh_times(events1)
#rhtimes2 <- rh_travel_times(events1)
#rht_transfer1 <- count_rh_transit_transfers(events1) # why 0?



# functions ---------------------------------------------------------------------#
read_events <- function(events_raw, cols){
  events <- events_raw %>% 
    fread(select=cols) %>% as.tibble() %>% 
    mutate(
      travelTime = arrivalTime - departureTime,
      avgSpeed = length / travelTime,
      vehicleType = ifelse(
        grepl("ride",vehicle),"ride_hail",vehicleType)
    ) %>% 
    {data.table::as.data.table(.)[order(person,time)]} %>%
    as_tibble()
  
  events
}

# mode choice
mode_choice <- function(events){
  events %>% 
    filter(type == "ModeChoice") %>% 
    group_by(mode) %>% 
    summarize(n = n()) %>%
    mutate(share = n / sum(n)) %>%
    mutate(share = round(share*100,3)) %>%
    select(-n)
}
# trip arrivals
trip_arrivals <- function(events){
  events %>% 
    filter(type == "TripArrivalEvent") %>% 
    group_by(mode) %>% 
    summarize(n = n())
}

# rh passengers
rh_pass <- function(events){
  rhPassengers <- events %>%
    filter(type == "PathTraversal",
           vehicleType == "ride_hail") %>%
    select(numPassengers) %>%
    table() %>% as_tibble() %>%
    `colnames<-`(c("numPassengers", "n"))
  
  rhPassengers
}

#' rh wait and replanning times
rh_times <- function(events){
  times <- events %>%
    arrange(person, time) %>%
    mutate(
      rhReserveTime = ifelse(
        type == "ReserveRideHail" & person == lead(person),
        lead(time) - time,
        NA
      ),
      rhReserveOutcome = ifelse(
        type == "ReserveRideHail" & person == lead(person),
        lead(type),
        NA
      )
    ) %>%
    filter(!is.na(rhReserveTime)) %>%
    group_by(rhReserveOutcome) %>%
    filter(rhReserveOutcome == "PersonEntersVehicle") %>%
    mutate(rhReserveTime = rhReserveTime / 60)
  times %>% 
    summarise(mean = mean(rhReserveTime),
              sd = sd(rhReserveTime)) %>%
    pivot_longer(!rhReserveOutcome, names_to = "summary", values_to = "values") %>%
    select(-rhReserveOutcome)
  }

#' rh travel times
rh_travel_times <- function(events){
  times <- events %>% 
    filter(!is.na(travelTime),
           type == "PathTraversal",
           str_detect(vehicle, "rideHail")) %>% 
    mutate(travelTime = (travelTime) / 60,
           variable = "travelTime") %>%
    group_by(variable)
  
  times %>%
    summarise(mean = mean(travelTime),
              q25 = quantile(travelTime,c(.25)),
              q50 = quantile(travelTime,c(.5)),
              q75 = quantile(travelTime,c(.75))) %>%
    pivot_longer(!variable, names_to = "summary", values_to = "values") %>%
    select(-variable)
}

#' count transfers to/from transit and rh
count_rh_transit_transfers <- function(events){
  transfer_rht <- events %>% 
    filter(
      type %in% c("PersonEntersVehicle",
                  "PersonLeavesVehicle"),
      str_detect(vehicle, "rideHail") | str_detect(vehicle, "gtfs")
    ) %>% 
    arrange(person, time) %>% 
    select(person:type) %>% 
    filter(type == "PersonLeavesVehicle" & lead(type) == "PersonEntersVehicle",
           str_detect(vehicle, "rideHail") & str_detect(lead(vehicle), "gtfs"),
           person == lead(person)) %>%
    mutate(transferType = "rideHail-transit")
  
  transfer_trh <- events %>%
    filter(
      type %in% c("PersonEntersVehicle",
                  "PersonLeavesVehicle"),
      str_detect(vehicle, "rideHail") | str_detect(vehicle, "gtfs")
    ) %>% 
    arrange(person, time) %>% 
    select(person:type) %>% 
    filter(lag(type) == "PersonLeavesVehicle" & type == "PersonEntersVehicle",
           str_detect(lag(vehicle), "gtfs") & str_detect(vehicle, "rideHail"),
           person == lag(person)) %>%
    mutate(transferType = "transit-rideHail") 
  
  bind_rows(transfer_rht,transfer_trh) %>%
    group_by(transferType) %>%
    summarize(n = n())
}


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
  
  
#  summary0 <- final_rh_users %>%
#    group_by(legMode) %>%
#    summarize(n = n()) %>%
#    mutate(share = n / sum(n)) %>%
#    mutate(share = round(share*100,3))
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

pie_chart <- function(plans_sum){
  ggplot(plans_sum, aes(x=" ", y=share, group=legMode, colour=legMode, fill=legMode)) +
    geom_bar(width = 1, stat = "identity") +
    geom_col(color = "black") +
    geom_text(aes(label = round(share,0)),
              color = "black",
              position = position_stack(vjust = 0.5),
              size = 2.5) +
    coord_polar("y", start=0) + 
    facet_grid(.~ ScenarioName) +theme_void() + 
    theme(text = element_text(size = 8)) +
    scale_fill_brewer(palette="Set3")
}


# create tables/graphs used in results section--------------------------------------------------#
format_ridership_table <- function(mode_choice_table){
  ridership <- mode_choice_table %>%
    #instead of renaming, just fix the names in targets and rerun (will take like 1hr)
    rename(rename_list) %>% select(1,6,11,2,7,3,8,4,9,5,10) %>%
    pivot_longer(!mode,names_to = "Scenario Name", values_to = "ridership") %>%
    filter(mode %in% c("ride_hail","ride_hail_pooled","ride_hail_transit")) %>%
    pivot_wider("Scenario Name", names_from=mode,values_from=ridership) %>%
    mutate_all(~replace(., is.na(.), 0.000)) %>%
    #mutate(across(!"Scenario Name", ~paste0(.,"%")))
    mutate("Scenario Number" = row_number(),
           "Total" = ride_hail + ride_hail_pooled+ride_hail_transit) %>%
    select(5,1,2,3,4,6)
}

format_transfers_table <- function(rh_to_transit){
  transfers <- rh_to_transit %>%
    rename(rename_list_graph) %>% select(1,6,11,2,7,3,8,4,9,5,10) %>%
    pivot_longer(!transfer_type, names_to="Scenario Name") %>%
    mutate(transfer_type = ifelse(transfer_type == "rideHail-transit", "ride_hail-to-transit","transit-to-ride_hail")) %>%
    pivot_wider("Scenario Name", names_from = transfer_type, values_from = value) %>%
    mutate_all(~replace(., is.na(.), 0))
}

format_transfers_graph <- function(transfers){
  transfer_long <- transfers %>%
    pivot_longer(!`Scenario Name`, names_to="transfertype",values_to="numtransfers") %>%
    rename("scenario" = "Scenario Name")
  
  transfer_long$scenario <- factor(transfer_long$scenario, 
                                   levels=c("wRH-None (1)", "noRH-None (2)", "wRH-AllModes-AllVars (3)", "noRH-AllModes-AllVars (4)", "wRH-AllModes-PathVars (5)",
                                            "noRH-AllModes-PathVars (6)", "wRH-RHModes-AllVars (7)","noRH-RHModes-AllVars (8)", "wRH-RHModes-PathVars (9)", "noRH-RHModes-PathVars (10)"))
  
  ggplot(transfer_long) +
    aes(x = as.factor(scenario), y = numtransfers, fill = transfertype) +
    geom_col_pattern(
      aes(pattern = transfertype, pattern_angle = transfertype, pattern_spacing = transfertype),
      fill            = 'white', 
      colour          = 'black',
      pattern_spacing = 0.04,
      position = "dodge2") + 
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    xlab("Scenario Name") +
    ylab("Number of Transfers")  +
    theme(legend.position="top") +
    geom_text(aes(label = numtransfers, y = numtransfers + 80), position = position_dodge(1.2))    
}

format_waits_graph <- function(wait_times){
  summary <- wait_times %>%
    rename(rename_list_graph) %>% select(1,6,11,2,7,3,8,4,9,5,10) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    pivot_longer(!values, names_to="scenario", values_to="vals") %>%
    pivot_wider(scenario, names_from="values",values_from="vals") %>%
    mutate(scenario = as.factor(scenario))
  summary$scenario <- factor(summary$scenario, 
    levels=c("wRH-None (1)", "noRH-None (2)", "wRH-AllModes-AllVars (3)", "noRH-AllModes-AllVars (4)", "wRH-AllModes-PathVars (5)",
             "noRH-AllModes-PathVars (6)", "wRH-RHModes-AllVars (7)","noRH-RHModes-AllVars (8)", "wRH-RHModes-PathVars (9)", "noRH-RHModes-PathVars (10)"))
  
  ggplot(summary, aes(x = scenario)) +
    geom_boxplot(aes(
      lower = mean - sd, 
      upper = mean + sd, 
      middle = mean, 
      ymin = mean - 3*sd, 
      ymax = mean + 3*sd),
      stat = "identity") +
    xlab("Scenario Name") +
    ylab("Wait Time (sec)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    geom_text(aes(label = round(mean,2), y = mean + 2.3))
}

rename_list = c(
  "wRH-AllModes-AllVars" = "All Modes - All Variables - W/ RH",
  "wRH-AllModes-PathVars" = "All Modes - Path Variables - W/ RH",
  "wRH-RHModes-AllVars" = "RH Modes - All Variables - W/ RH",
  "wRH-RHModes-PathVars" = "RH Modes - Path Variables - W/ RH",
  "wRH-None" = "No Modes - W/ RH",
  "noRH-AllModes-AllVars" = "All Modes - All Variables - No RH",
  "noRH-AllModes-PathVars" = "All Modes - Path Variables - No RH",
  "noRH-RHModes-AllVars" = "RH Modes - All Variables - No RH",
  "noRH-RHModes-PathVars" = "RH Modes - Path Variables - No RH",
  "noRH-None" = "No Modes - No RH"
)

rename_list_graph = c(
  "wRH-AllModes-AllVars (3)" = "All Modes - All Variables - W/ RH",
  "wRH-AllModes-PathVars (5)" = "All Modes - Path Variables - W/ RH",
  "wRH-RHModes-AllVars (7)" = "RH Modes - All Variables - W/ RH",
  "wRH-RHModes-PathVars (9)" = "RH Modes - Path Variables - W/ RH",
  "wRH-None (1)" = "No Modes - W/ RH",
  "noRH-AllModes-AllVars (4)" = "All Modes - All Variables - No RH",
  "noRH-AllModes-PathVars (6)" = "All Modes - Path Variables - No RH",
  "noRH-RHModes-AllVars (8)" = "RH Modes - All Variables - No RH",
  "noRH-RHModes-PathVars (10)" = "RH Modes - Path Variables - No RH",
  "noRH-None (2)" = "No Modes - No RH"
)
