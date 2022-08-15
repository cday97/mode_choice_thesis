
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
    summarize(n = n())
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
    filter(rhReserveOutcome == "PersonEntersVehicle")
  
  times %>% 
    summarise(mean = mean(rhReserveTime) / 60,
              q25 = quantile(rhReserveTime,c(.25)) / 60,
              q50 = quantile(rhReserveTime,c(.5)) / 60,
              q75 = quantile(rhReserveTime,c(.75)) / 60) %>%
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









#' rh utilization
#rh_utilization <- function(events, rh_info){
#  rhUtil <- events %>%
#    filter(type == "PathTraversal",
#           vehicleType == "micro") %>% 
#    separate(vehicle, c("vehicle_number", "area"), sep = "@") %>% 
#    select(area, numPassengers) %>% 
#    table() %>% as_tibble() %>% 
#    left_join(rh_info, by = c("area" = "Area")) %>% 
#    transmute(area,
#              utilization = as.numeric(numPassengers) * n
#              / fleetSize / operating_hours) %>% 
#    group_by(area) %>% 
#    summarise(utilization = sum(utilization))
#  
#  rhUtil
#}


#' format rh info
#format_rh_info <- function(rh_raw){
#  rh_info <- rh_raw %>% 
#    read_csv() %>% 
#    mutate(shift_start = str_replace(shifts, "\\{(\\d+):\\d+\\}", "\\1") %>% 
#             as.numeric(),
#           shift_end = str_replace(shifts,"\\{\\d+:(\\d+)\\}", "\\1") %>% 
#             as.numeric(),
#           operating_hours = (shift_end - shift_start) / 3600) %>% 
#    select(Area, fleetSize, operating_hours)
#  
#  rh_info
#}



