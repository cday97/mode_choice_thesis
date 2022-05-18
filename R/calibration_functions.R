
# not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

#---------------------------------------------------------------------------------------------------------#
# function used to create the final table that displays trip data from activitysim using as inputs
# the final_trips.csv and final_households.csv files
get_final_asim <- function(asim_trips_csv, asim_households_csv) {
  
  #read the asim trips csv file and manipulate it a little bit
  asim_final_trips <- read_csv(asim_trips_csv) %>%
    # determine start and end time of each tour
    arrange(person_id, tour_id) %>%
    group_by(person_id, tour_id) %>%
    mutate(tour_start_time = min(depart), tour_end_time = max(depart)) %>% ungroup() %>%
    # restructure tour index order using tour start/end times
    arrange(person_id,depart,tour_start_time,tour_end_time,trip_id) %>% 
    group_by(person_id) %>%
    # create new attribute called activity_index
    mutate(activity_index = row_number() -1) %>% ungroup() %>%
    # select the needed columns
    select(trip_id, person_id, tour_id, household_id, activity_index, trip_count, purpose, 
           origin, destination, primary_purpose, trip_mode)

  #read in the asim households csv file and create an auto_ownership variable
  asim_final_households <- read_csv(asim_households_csv) %>%
    mutate(autoWorkRatio = ifelse(auto_ownership == 0, "no_auto", 
                                  ifelse(auto_ownership >= num_workers, "auto_sufficient", 
                                         "auto_deficient")))
  
  #merge the trips and households datasets into one table
  asim_table <- left_join(asim_final_trips, asim_final_households, by = "household_id") %>%
    select(trip_id, person_id, tour_id, household_id, activity_index, trip_count, purpose, 
           origin, destination, autoWorkRatio, primary_purpose, trip_mode)
  
  asim_table
  
}

# function used to read in the final beam events and then manipulate it so it is consistent with
# the way we are analyzing the data. Also note that we eliminte teleport modes and simply group
# them with normal hov modes
get_final_beam <- function(beam_final_events){
  read_csv(beam_final_events, col_types = cols(.default = "c")) %>%
  filter(type == "TripArrivalEvent") %>%
  select(person,time,type,mode,currentTourMode,income,vehicleOwnership,length,tourIndex,tourPurpose,
         activityIndex,actType,locationX,locationY) %>%
  arrange(person,tourIndex,activityIndex) %>%
  mutate(person = as.numeric(person),
         activityIndex = as.numeric(activityIndex),
         primary_purpose = tourPurpose,
         autoWorkRatio = vehicleOwnership) %>%
  # change all teleport modes to hov2 or hov3 for overall statistical analysis
  mutate(mode = ifelse(mode == "hov2_teleportation", "hov2", ifelse(mode == "hov3_teleportation", 
                                                                    "hov3", mode)))
}

#---------------------------------------------------------------------------------------------------------#
## function used to create a table that shows which activitysim modes correspond to which BEAM
## modes. 
get_modes_table <- function(){
  data.frame(trip_mode = c("BIKE","WALK","DRIVEALONEFREE","DRIVEALONEPAY","SHARED2FREE","SHARED2PAY",
                         "SHARED3FREE","SHARED3PAY","DRIVE_COM","DRIVE_EXP","DRIVE_LOC","DRIVE_LRF",
                         "DRIVE_HVY","WALK_COM","WALK_EXP","WALK_LOC","WALK_LRF","WALK_HVY",
                         "TNC_SINGLE","TAXI","TNC_SHARED"),
                mode = c("bike","walk","car","car","hov2","hov2","hov3","hov3","drive_transit",
                          "drive_transit","drive_transit","drive_transit","drive_transit","walk_transit",
                          "walk_transit","walk_transit","walk_transit","walk_transit","ride_hail",
                          "ride_hail","ride_hail_pooled"))
}

#---------------------------------------------------------------------------------------------------------#
# overloaded function that created the summary table and percentages of the overall trips based on
# mode choice, autoWorkRatio, and primaryPurpose values
get_summary <- function(table, modes, boolean){
  if (boolean == TRUE){
    inner_join(table,modes, by = "trip_mode") %>%
      group_by(mode, autoWorkRatio, primary_purpose) %>%
      summarize(tripTotals = sum(n())) %>% ungroup() %>%
      mutate(total = sum(tripTotals)) %>%
      mutate(tripPercents = tripTotals/total*100) %>%
      select(-total)
  } else {
    table %>% group_by(mode, autoWorkRatio, primary_purpose) %>%
      summarize(tripTotals = sum(n())) %>% ungroup() %>%
      mutate(total = sum(tripTotals)) %>%
      mutate(tripPercents = tripTotals/total*100) %>%
      select(-total)
  }
}

# a function that creates a graph based on the summary table
get_sumgraph <- function(summary){
  ggplot(summary) +
    aes(autoWorkRatio,tripPercents,fill=primary_purpose) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Set3") + 
    facet_wrap(~mode) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5))
}

#---------------------------------------------------------------------------------------------------------#
# function used to combine the summary table statistics with the asim_long.csv file
# after combining the files, it manipulate the ratios and creates a new asim_long file to be used
build_new_ascs<- function(long, beam_summary, asim_summary){
  # read in asim_long csv
  asim_long <- read_csv(long) %>%
    filter(grepl("asc", variable))
  #manipulate beam_summary table
  beam_sum <- beam_summary %>%
    mutate(variable = ifelse(autoWorkRatio == "auto_sufficient", "ascAutoSufficient", 
                             ifelse(autoWorkRatio == "no_auto", "ascNoAuto", "ascAutoDeficient")),
           alternative = mode,
           latentClass = primary_purpose,
           beamP = tripPercents) %>%
    select(alternative,variable,latentClass,beamP)
  #manipulate asim_summary table
  asim_sum <- asim_summary %>%
    mutate(variable = ifelse(autoWorkRatio == "auto_sufficient", "ascAutoSufficient", 
                             ifelse(autoWorkRatio == "no_auto", "ascNoAuto", "ascAutoDeficient")),
           alternative = mode,
           latentClass = primary_purpose,
           asimP = tripPercents) %>%
    select(alternative,variable,latentClass,asimP)
  
  #merge all three files together
  asim_long_final <- asim_long %>% left_join(asim_sum, by = c("alternative", "variable","latentClass")) %>% 
    left_join(beam_sum, by = c("alternative", "variable", "latentClass"))
  
  asim_long_final
}

calibrate_new_ascs<- function(new_long){
  newascs<- new_long %>% 
    filter(alternative %!in% c("hov2_teleportation", "hov3_teleportation")) %>%
    mutate(value = ifelse(is.na(value),0,value),
           asimP = ifelse(is.na(asimP),0,asimP)) %>%
    mutate(beamP = ifelse(is.na(beamP),ifelse(asimP==0,0,asimP/2),beamP)) %>%
    mutate(value = ifelse(asimP==0,value,value + log(asimP/beamP))) %>%
    select(-asimP,-beamP)
    
  # lets add a hov2_teleportation and hov3_teleportation mode to the final file
  teleport <- newascs %>% filter(alternative %in% c("hov2","hov3")) %>%
    mutate(alternative = ifelse(alternative =="hov2","hov2_teleportation","hov3_teleportation"))
  purps <- c("work","univ","school","escort","shopping","eatout","othmaint","social","othdiscr","atwork")
  final_long <- rbind(newascs,teleport) %>%
    arrange(factor(variable, levels = c("ascNoAuto","ascAutoDeficient","ascAutoSufficient")),
            alternative,factor(latentClass, levels = purps))
  
  final_long
}

update_asim_long <- function(long, calibrated){
  asim_long <- read_csv(long) %>%
    left_join(calibrated, by = c('variable',"alternative","latentClass","model","tourType","units")) %>% #DOESN"T REPLACE VALUES. FIGURE IT OUT
    mutate(value = ifelse(is.na(value.y),value.x,value.y)) %>%
    mutate(value = ifelse(is.na(value),"",value)) %>%
    select(-value.x,-value.y)
}
