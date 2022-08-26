

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

# old wait time calculation function (no sampling and only summary table ability)
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
              sd = sd(rhReserveTime),
              n = n(),
              max = max(rhReserveTime),
              min = min(rhReserveTime)) %>%
    pivot_longer(!rhReserveOutcome, names_to = "summary", values_to = "values") %>%
    select(-rhReserveOutcome)
}


#' rh wait and replanning times
rh_waittimes <- function(events){
  s_pop <- events %>%
    filter(type == "ReserveRideHail") %>%
    filter(duplicated(person) == FALSE) %>% 
    select(person) %>%
    sample_frac(.25)
  
  times <- events %>%
    filter(person %in% s_pop$person) %>%
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
    mutate(rhReserveTime = rhReserveTime / 60) %>%
    select(rhReserveOutcome,rhReserveTime) %>%
    #ungroup() %>% unlist() %>% unname() %>%
    as.data.frame()
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
    summarise(sum = sum(travelTime),
              mean = mean(travelTime),
              q25 = quantile(travelTime,c(.25)),
              q50 = quantile(travelTime,c(.5)),
              q75 = quantile(travelTime,c(.75))) %>%
    pivot_longer(!variable, names_to = "summary", values_to = "values") %>%
    select(-variable)
}

total_fleet_hours <- function(fleet){
  fleet <- read_csv("data/Driverfleet_SLC.csv") %>%
    mutate(shift2 = str_sub(shifts,2,-2)) %>%
    separate(shift2,c("StartTime","EndTime")) %>%
    mutate(shiftLength = (as.numeric(EndTime) - as.numeric(StartTime))/3600)
  fleetSummary <- fleet %>%
    summarize(sum = sum(shiftLength))
  
  as.numeric(fleetSummary[1,])
}

rh_utilization <- function(travel_times, num_passengers, totalFleetHours){
  tt <- travel_times %>%
    rename(rename_list_graph) %>% select(1,6,11,2,7,3,8,4,9,5,10) %>%
    pivot_longer(!values, names_to = "ScenarioName", values_to = "TotalTravelTime") %>%
    filter(values == "sum") %>% select(-values) %>%
    mutate(TotalTravelTime = TotalTravelTime / 60)
  np <- num_passengers %>%
    rename(rename_list_graph) %>% select(1,6,11,2,7,3,8,4,9,5,10) %>%
    pivot_longer(!num_passengers, names_to = "ScenarioName", values_to = "vals") %>%
    filter(num_passengers != 0) %>%
    group_by(ScenarioName) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(TotalPassengers = sum(vals)) %>%
    filter(num_passengers == 1) %>% select(-num_passengers,-vals)
  
  sumTable <- left_join(tt,np,by = "ScenarioName") %>%
    mutate(TotalDriverHours = totalFleetHours) %>%
    mutate(RideHailTimeUtilization = round(TotalTravelTime / TotalDriverHours * 100,3),
           RideHailPersonUtilization = round(TotalPassengers / TotalDriverHours,3))
  
  sumTable <- sumTable[-2,]  
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


# create tables/graphs used in results section--------------------------------------------------#
pie_chart <- function(plans_sum){
  ggplot(plans_sum, aes(x=ScenarioName, y=share, group=legMode, fill=fct_inorder(legMode))) +
    geom_bar(position = "stack",stat = "identity") +
    geom_col(color = 1) +
    geom_label_repel(aes(x = ScenarioName, 
                        y = share, 
                        label = paste0(round(share,2),"%")),
                    color = "black",
                    position = position_stack(vjust = .65),
                    size = 2,
                    show.legend = FALSE) + 
    theme_bw() + 
    guides(fill = guide_legend(title = "Mode")) +
    theme(text = element_text(size = 8)) +
    scale_fill_brewer(palette="Set3", labels=c('Bike', 'Car', 'Drive to Transit', 'HOV2','HOV2 Passenger', 'HOV3', 'HOV3 Passenger', "Ride Hail", "Pooled Ride Hail", "Walk", "Walk to Transit")) +
    xlab("Scenario Name") +
    ylab("Share") +
    theme(axis.text.x = element_text(angle=90, hjust=1))
}

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
    geom_col(position = "dodge2") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    xlab("Scenario Name") +
    ylab("Number of Transfers")  +
    theme(legend.position="top") +
    geom_text(aes(label = numtransfers, y = numtransfers + 80), position = position_dodge(1.2))    
}

format_waits_graph <- function(wait_times){
  waittimes <- wait_times %>%
    group_by(ScenarioName) %>%
    mutate(mean = mean(rhReserveTime)) %>%
    mutate(ScenarioName = as.factor(ScenarioName)) %>%
    mutate(ScenarioName = case_when(
      ScenarioName == "All Modes - All Variables - W/ RH" ~ "wRH-AllModes-AllVars (3)",
      ScenarioName == "All Modes - Path Variables - W/ RH" ~ "wRH-AllModes-PathVars (5)",
      ScenarioName == "RH Modes - All Variables - W/ RH" ~ "wRH-RHModes-AllVars (7)",
      ScenarioName == "RH Modes - Path Variables - W/ RH" ~ "wRH-RHModes-PathVars (9)",
      ScenarioName == "No Modes - W/ RH" ~ "wRH-None (1)",
      ScenarioName == "All Modes - All Variables - No RH" ~ "noRH-AllModes-AllVars (4)",
      ScenarioName == "All Modes - Path Variables - No RH" ~ "noRH-AllModes-PathVars (6)",
      ScenarioName == "RH Modes - All Variables - No RH" ~ "noRH-RHModes-AllVars (8)",
      ScenarioName == "RH Modes - Path Variables - No RH" ~ "noRH-RHModes-PathVars (10)"
    ))
  waittimes$ScenarioName <- factor(waittimes$ScenarioName, 
    levels=c("wRH-None (1)", "wRH-AllModes-AllVars (3)", "noRH-AllModes-AllVars (4)", "wRH-AllModes-PathVars (5)",
             "noRH-AllModes-PathVars (6)", "wRH-RHModes-AllVars (7)","noRH-RHModes-AllVars (8)", "wRH-RHModes-PathVars (9)", "noRH-RHModes-PathVars (10)"))

  ggplot(waittimes, aes(x = ScenarioName, y = rhReserveTime, fill = ScenarioName, alpha = .3)) + 
    geom_violin() + geom_boxplot(width = 0.3) +
    scale_fill_brewer(palette="Set3") +
    geom_text(aes(label = round(mean,1), y = mean + 1), size = 3)  +  
    xlab("Scenario Name") +
    ylab("Wait Time (min)") +
    theme_bw() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle=90, hjust=1))
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

