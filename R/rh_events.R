
# functions ---------------------------------------------------------------------#
#' Read all the events files
#' 
#' @param scenario_list A named list containing the paths 
read_all_events <- function(scenario_list, cols){
  
  all_events <- lapply(scenario_list, function(scenario){
    read_events(scenario, cols)
  })
  
  all_events
}

#' Read the events list from one scenario
#' 
#' @param events_raw path to the events file
#' @param cols list of columns to keep
read_events <- function(events_raw, cols){
  events <- events_raw %>% 
    fread(select=cols) %>% as_tibble() %>% 
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
    select(-share)
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

rh_pass_time <- function(events){
  passengerTime <- events %>%
    filter(type == "PathTraversal",
           vehicleType == "ride_hail") %>%
    mutate(travelTime = arrivalTime - departureTime) %>%
    filter(numPassengers > 0) %>%
    summarize(name = "passenger", totalPassengerTime = sum(travelTime))
    
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

rh_utilization <- function(rh_passenger_times, num_passengers, totalFleetHours){
  tt <- rh_passenger_times %>%
    rename(rename_list_graph) %>% select(1,11,6,10,9,5,4,8,7,3,2)  %>%
    pivot_longer(!totalPassengerTime, names_to = "ScenarioName", values_to = "TotalTravelTime") %>%
    mutate(TotalTravelTime = TotalTravelTime / 3600)
  np <- num_passengers %>%
    rename(rename_list_graph) %>% select(1,11,6,10,9,5,4,8,7,3,2) %>%
    pivot_longer(!num_passengers, names_to = "ScenarioName", values_to = "vals") %>%
    filter(num_passengers != 0) %>%
    group_by(ScenarioName) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(TotalPassengers = sum(vals)) %>%
    filter(num_passengers == 1) %>% select(-num_passengers,-vals)
  
  sumTable <- left_join(tt,np,by = "ScenarioName") %>%
    mutate(TotalDriverHours = totalFleetHours) %>%
    mutate(RideHailTimeUtilization = round(TotalTravelTime / TotalDriverHours * 100,1),
           RideHailPersonUtilization = round(TotalPassengers / TotalDriverHours,1))
  
  sumTable <- sumTable[-1,]  
  sumTable
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

#walk analysis
walk_analysis <- function(events1){
  eventchoice <- events1 %>%
    filter(type %in% c("ModeChoice")) %>%
    group_by(person,tourIndex) %>%
    arrange(person,tourIndex,time) %>%
    mutate(activityIndex = row_number()-1, person = as.numeric(person)) %>%
    ungroup()
  
  walk_trips <- eventchoice %>%
    filter(mode == "walk")
  walkers <- eventchoice %>%
    filter(person %in% walk_trips$person) %>%
    mutate(switchWalker = ifelse(activityIndex == 0 & mode != "walk",TRUE,FALSE)) %>%
    filter(switchWalker == TRUE)
  
  switch_walker_trips <- eventchoice %>%
    filter(person %in% walkers$person) %>%
    mutate(hour = as.numeric(sub("\\..*","",paste(time/3600))),
           count = 1)
  switch_walker_trips
}







#rhttest <- transfer_rht %>%
#  filter(!person %in% transfer_trh$person)


#rheventstest <- events %>%
#  filter(person %in% rhttest$person) %>%
#  select(1:7,13)
