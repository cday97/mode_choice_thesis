
#beam calibration figure
beam_calib_graph <- function(calibration_shares, target_shares){
  target_shares2 <- target_shares %>%
    mutate(
      autoWorkRatio = case_when(
        autoWorkRatio == "auto_deficient" ~ "Auto Deficient",
        autoWorkRatio == "auto_sufficient" ~ "Auto Sufficient",
        autoWorkRatio == "no_auto" ~ "No Auto"
      ),
      primary_purpose = case_when(
        primary_purpose == "atwork" ~ "At Work",
        primary_purpose == "eatout" ~ "Eatout",
        primary_purpose == "escort" ~ "Escort",
        primary_purpose == "othdiscr" ~ "Discr.",
        primary_purpose == "othmaint" ~ "Maint.",
        primary_purpose == "school" ~ "School",
        primary_purpose == "univ" ~ "Univ.",
        primary_purpose == "work" ~ "Work",
        primary_purpose == "social" ~ "Social",
        primary_purpose == "shopping" ~ "Shopping"
      ))
  
  modesP <- c(bike = "springgreen2", bike_transit = "springgreen4", car = "dodgerblue",drive_transit = "coral3",hov2 = "skyblue",hov3 = "cadetblue1", ride_hail = "orchid",ride_hail_pooled = "plum1", ride_hail_transit = "violet",walk = "goldenrod2",walk_transit = "coral1")
  modesL2 <- c('Bike', 'Bike to Transit', 'Car','Drive to Transit', 'HOV2','HOV3','Ride-hail','Pooled Ride-hail', "Ride-hail to Transit", "Walk", "Walk to Transit")
  
  calibration_shares %>%  
    mutate(
      autoWorkRatio = case_when(
        autoWorkRatio == "auto_deficient" ~ "Auto Deficient",
        autoWorkRatio == "auto_sufficient" ~ "Auto Sufficient",
        autoWorkRatio == "no_auto" ~ "No Auto"
      ),
      primary_purpose = case_when(
        primary_purpose == "atwork" ~ "At Work",
        primary_purpose == "eatout" ~ "Eatout",
        primary_purpose == "escort" ~ "Escort",
        primary_purpose == "othdiscr" ~ "Discr.",
        primary_purpose == "othmaint" ~ "Maint.",
        primary_purpose == "school" ~ "School",
        primary_purpose == "univ" ~ "Univ.",
        primary_purpose == "work" ~ "Work",
        primary_purpose == "social" ~ "Social",
        primary_purpose == "shopping" ~ "Shopping"
      )) %>%
    ggplot() +
    geom_line(aes(x = iteration, y = tripPercents, color = mode)) +
    geom_hline(data = target_shares2, aes(yintercept = tripPercents, color = mode), lty = "dashed") +
    facet_grid(primary_purpose~ autoWorkRatio)+
    scale_color_manual(values = modesP, labels = modesL2) +
    xlab("Iteration") +ylab("Percent of Trips") +
    labs(color = "Mode") + 
    theme_bw()
}




# create tables/graphs used in results section--------------------------------------------------#
format_ridership_table <- function(mode_choice_table, asim_plans){
  ridership <- mode_choice_table %>%
    #instead of renaming, just fix the names in targets and rerun (will take like 1hr)
    rename(rename_list) %>% select(1,11,6,10,9,5,4,8,7,3,2) %>%
    pivot_longer(!mode,names_to = "Scenario Name", values_to = "ridership") %>%
    filter(mode %in% c("ride_hail","ride_hail_pooled","ride_hail_transit")) %>%
    pivot_wider("Scenario Name", names_from=mode,values_from=ridership) %>%
    mutate_all(~replace(., is.na(.), 0.0)) %>%
    #mutate(across(!"Scenario Name", ~paste0(.,"%")))
    filter(ride_hail > 0) %>%
    mutate("Total" = ride_hail + ride_hail_pooled+ride_hail_transit) %>%
    rename("Ride-hail" = "ride_hail",
           "Pooled Ride-hail" = "ride_hail_pooled",
           "Ride-hail to Transit" = "ride_hail_transit") 
  asim_rh <- asim_plans %>%
    filter(legMode %in% c("ride_hail","ride_hail_pooled","ride_hail_transit")) %>%
    group_by(legMode) %>%
    summarize(n = round(n() * .15)) %>% mutate(bob = "bob") %>%
    pivot_wider(!bob, names_from = legMode, values_from = n) %>%
    mutate("Scenario Name" = "ActivitySim - Inputs to BEAM", ride_hail_transit = 0) %>%
    mutate(Total = sum(ride_hail) + sum(ride_hail_pooled)) %>%
    rename("Ride-hail" = "ride_hail",
           "Pooled Ride-hail" = "ride_hail_pooled",
           "Ride-hail to Transit" = "ride_hail_transit") %>%
    select(3,1,2,4,5)
    
  
  bind_rows(asim_rh,ridership)
}

add_p_to_ridership <- function(ridershipTable, mode_choice_table,asim_plans){
  ridershipT <- mode_choice_table %>%
    #instead of renaming, just fix the names in targets and rerun (will take like 1hr)
    rename(rename_list) %>% select(1,11,6,10,9,5,4,8,7,3,2) %>%
    mutate_all(~replace(., is.na(.), 0.0)) %>%
    pivot_longer(!mode,names_to = "scenario", values_to = "ridership") %>%
    group_by(scenario) %>%
    summarize(Totals = sum(ridership))
  asimT <- asim_plans %>%
    filter(!is.na(legMode)) %>%
    group_by(legMode) %>%
    summarize(n = round(n() * .15)) %>%
    mutate(Totals = sum(n), scenario = "ActivitySim - Inputs to BEAM") %>%
    select(scenario,Totals)
  totals <- bind_rows(ridershipT, asimT[1,]) %>%
    filter(scenario != "NoRideHail")
  
  ridershipP <- left_join(ridershipTable,totals, by = c("Scenario Name" = "scenario")) %>%
    mutate(TotalP = round((Total / Totals)*100,2)) %>%
    select(-Totals) %>%
    rename("Total (%)" = "TotalP")
}

quantile_groups <- function(wait_times){
  wait_times_groups <- wait_times %>%
    mutate(mcModel = ifelse(grepl("All M", ScenarioName), "All", ifelse(grepl("RH M", ScenarioName), "RH", "None"))) %>%
    group_by(mcModel) %>%
    summarize("20" = quantile(rhReserveTime, .2),
              "50" = quantile(rhReserveTime, .5),
              "80" = quantile(rhReserveTime, .8)
    ) %>%
    pivot_longer(!mcModel, names_to = "Quantile", values_to = "Value") %>%
    pivot_wider(names_from = mcModel, values_from = Value)
}

quantile_times <- function(wait_times){
  wait_times_groups <- wait_times %>%
    mutate(mcModel = ifelse(grepl("All M", ScenarioName), "All", ifelse(grepl("RH M", ScenarioName), "RH", "None")))
  
  Z1dt <- wait_times_groups %>%
    filter(mcModel == "All")
  All <- Z1dt$rhReserveTime
  Z2dt <- wait_times_groups %>%
    filter(mcModel == "RH")
  RH <- Z2dt$rhReserveTime
  Z3dt <- wait_times_groups %>%
    filter(mcModel == "None")
  None <- Z3dt$rhReserveTime
  
  Q_All_RH <- qcomhd(All,RH,q=c(.2,.5,.8)) %>%
    mutate(Comparison = "All-RideHail")
  Q_All_NN <- qcomhd(All,None,q=c(.2,.5,.8)) %>%
    mutate(Comparison = "All-None")
  Q_RH_NN <- qcomhd(RH,None, q=c(.2,.5,.8)) %>%
    mutate(Comparison = "RideHail-None")
  
  QuantileTests <- bind_rows(Q_All_RH,Q_All_NN,Q_RH_NN)
  QuantileTests
  
  #' (https://freakonometrics.hypotheses.org/4199)
  #' Those tests are based on the procedure proposed in Wilcox, Erceg-Hurn,  Clark and Carlson (2013), 
  #' online on http://tandfonline.com/…. They rely on the use of bootstrap samples. The idea is quite 
  #' simple actually (even if, in the paper, they use Harrell–Davis estimator to estimate quantiles, 
  #' i.e. a weighted sum of ordered statistics – as described in http://freakonometrics.hypotheses.org/1755 – 
  #' but the idea can be understood with any estimator): we generate several bootstrap samples, and 
  #' compute the median for all of them (since our interest was initially on the median)
}

quantile_times_rh <- function(wait_times){
  wtg <- wait_times %>%
    group_by(ScenarioName) %>%
    summarize(n = n())
  
  
  Z1dt <- wait_times %>%
    filter(ScenarioName == "All Modes - All Variables - W/ RH")
  Z1 <- Z1dt$rhReserveTime
  Z2dt <- wait_times %>%
    filter(ScenarioName == "All Modes - All Variables - No RH")
  Z2 <- Z2dt$rhReserveTime
  
  Z3dt <- wait_times %>%
    filter(ScenarioName == "All Modes - Path Variables - W/ RH")
  Z3 <- Z3dt$rhReserveTime
  Z4dt <- wait_times %>%
    filter(ScenarioName == "All Modes - Path Variables - No RH")
  Z4 <- Z4dt$rhReserveTime
  
  Z5dt <- wait_times %>%
    filter(ScenarioName == "RH Modes - All Variables - W/ RH")
  Z5 <- Z5dt$rhReserveTime
  Z6dt <- wait_times %>%
    filter(ScenarioName == "RH Modes - All Variables - No RH")
  Z6 <- Z6dt$rhReserveTime
  
  Z7dt <- wait_times %>%
    filter(ScenarioName == "RH Modes - Path Variables - W/ RH")
  Z7 <- Z7dt$rhReserveTime
  Z8dt <- wait_times %>%
    filter(ScenarioName == "RH Modes - Path Variables - No RH")
  Z8 <- Z8dt$rhReserveTime
  
  
  All_PPL <- qcomhd(Z1,Z2,q=c(.2,.5,.8)) %>%
    mutate(Comparison = "All-PPL")
  All_Path <- qcomhd(Z3,Z4,q=c(.2,.5,.8)) %>%
    mutate(Comparison = "All-Path")
  RH_PPL <- qcomhd(Z5,Z6, q=c(.2,.5,.8)) %>%
    mutate(Comparison = "RH-PPL")
  RH_Path <- qcomhd(Z7,Z8, q=c(.2,.5,.8)) %>%
    mutate(Comparison = "RH-Path")
  
  
  QuantileTests <- bind_rows(All_PPL, All_Path, RH_PPL, RH_Path)
  QuantileTests
  
  #' (https://freakonometrics.hypotheses.org/4199)
  #' Those tests are based on the procedure proposed in Wilcox, Erceg-Hurn,  Clark and Carlson (2013), 
  #' online on http://tandfonline.com/…. They rely on the use of bootstrap samples. The idea is quite 
  #' simple actually (even if, in the paper, they use Harrell–Davis estimator to estimate quantiles, 
  #' i.e. a weighted sum of ordered statistics – as described in http://freakonometrics.hypotheses.org/1755 – 
  #' but the idea can be understood with any estimator): we generate several bootstrap samples, and 
  #' compute the median for all of them (since our interest was initially on the median)
}

format_waits_graph <- function(wait_times){
  waittimes <- wait_times %>%
    group_by(ScenarioName) %>%
    mutate(mean = mean(rhReserveTime),
           max = max(rhReserveTime),
           min = min(rhReserveTime)) %>%
    mutate(ScenarioName = as.factor(ScenarioName)) %>%
    mutate(ScenarioName = case_when(
      ScenarioName == "All Modes - All Variables - W/ RH" ~ "AsimBeamAll:PPL",
      ScenarioName == "All Modes - Path Variables - W/ RH" ~ "AsimBeamAll:Path",
      ScenarioName == "RH Modes - All Variables - W/ RH" ~ "AsimBeamRideHail:PPL",
      ScenarioName == "RH Modes - Path Variables - W/ RH" ~ "AsimBeamRideHail:Path",
      ScenarioName == "No Modes - W/ RH" ~ "AsimRideHail",
      ScenarioName == "All Modes - All Variables - No RH" ~ "BeamAll:PPL",
      ScenarioName == "All Modes - Path Variables - No RH" ~ "BeamAll:Path",
      ScenarioName == "RH Modes - All Variables - No RH" ~ "BeamRideHail:PPL",
      ScenarioName == "RH Modes - Path Variables - No RH" ~ "BeamRideHail:Path"
    )) %>%
    mutate(BeamModel = ifelse(grepl("BeamRideHail",ScenarioName),"RideHail",ifelse(grepl("BeamAll",ScenarioName),"All","None")))
  waittimes$ScenarioName <- factor(waittimes$ScenarioName, 
                                   levels=c("AsimRideHail", "BeamRideHail:Path", "BeamRideHail:PPL", "AsimBeamRideHail:Path","AsimBeamRideHail:PPL",
                                            "BeamAll:Path","BeamAll:PPL","AsimBeamAll:Path", "AsimBeamAll:PPL"))
  
  ggplot(waittimes, aes(x = ScenarioName, y = rhReserveTime, fill = factor(BeamModel)), alpha = .3) + 
    geom_violin() + geom_boxplot(width = 0.3) +
    scale_fill_manual(values = c("dodgerblue1","gold1","lightpink1")) +
    geom_text(aes(label = round(mean,1), y = mean + 1), size = 2)  +  
    xlab("Scenario Name") +
    ylab("Wait Time (min)") +
    theme_bw() +
    labs(fill="Beam Mode Choice") +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    geom_text(aes(label = round(max,1),  y = max + 1), size = 1.8) +
    geom_text(aes(label = round(min,1),  y = min - 1), size = 1.8)
}

rename_list = c(
  "AsimBeamAll:PPL" = "All Modes - All Variables - W/ RH",
  "AsimBeamAll:Path" = "All Modes - Path Variables - W/ RH",
  "AsimBeamRideHail:PPL" = "RH Modes - All Variables - W/ RH",
  "AsimBeamRideHail:Path" = "RH Modes - Path Variables - W/ RH",
  "AsimRideHail" = "No Modes - W/ RH",
  "BeamAll:PPL" = "All Modes - All Variables - No RH",
  "BeamAll:Path" = "All Modes - Path Variables - No RH",
  "BeamRideHail:PPL" = "RH Modes - All Variables - No RH",
  "BeamRideHail:Path" = "RH Modes - Path Variables - No RH",
  "NoRideHail" = "No Modes - No RH"
)

rename_list_graph = c(
  "AsimBeamAll:PPL" = "All Modes - All Variables - W/ RH",
  "AsimBeamAll:Path" = "All Modes - Path Variables - W/ RH",
  "AsimBeamRideHail:PPL" = "RH Modes - All Variables - W/ RH",
  "AsimBeamRideHail:Path" = "RH Modes - Path Variables - W/ RH",
  "AsimRideHail" = "No Modes - W/ RH",
  "BeamAll:PPL" = "All Modes - All Variables - No RH",
  "BeamAll:Path" = "All Modes - Path Variables - No RH",
  "BeamRideHail:PPL" = "RH Modes - All Variables - No RH",
  "BeamRideHail:Path" = "RH Modes - Path Variables - No RH",
  "NoRideHail" = "No Modes - No RH"
)

#modesP <- c(bike = "springgreen2", bike_transit = "springgreen4", car = "dodgerblue",hov2 = "skyblue",hov2_teleportation = "skyblue3",hov3 = "cadetblue1",hov3_teleportation = 'cadetblue3', ride_hail = "orchid",ride_hail_pooled = "plum1", ride_hail_transit = "violet",walk_transit = "coral1", drive_transit = "coral3", walk = "goldenrod2")
#modesL1 <- c('Bike', 'Bike to Transit', 'Car', 'HOV2','HOV2 Passenger', 'HOV3', 'HOV3 Passenger', "Ride-hail", "Pooled Ride-hail", "Ride-hail to Transit", "Walk to Transit", 'Drive to Transit', "Walk")


pie_chart <- function(plans_sum){
  modesP1 <- c(bike = "springgreen2", bike_transit = "springgreen4", car = "dodgerblue",hov2 = "skyblue",hov2_teleportation = "skyblue3",hov3 = "cadetblue1",hov3_teleportation = 'cadetblue3', ride_hail = "orchid",ride_hail_pooled = "plum1", ride_hail_transit = "violet",walk_transit = "coral1", drive_transit = "coral3", walk = "goldenrod2")
  modesL1 <- c('Bike', 'Bike to Transit', 'Car', 'HOV2','HOV2 Passenger', 'HOV3', 'HOV3 Passenger', "Ride-hail", "Pooled Ride-hail", "Ride-hail to Transit", "Walk to Transit", 'Drive to Transit', "Walk")

  plans_sum <- plans_sum %>% filter(!is.na(legMode)) %>%
    mutate(ScenarioName = case_when(
      ScenarioName == "wRH-AllModes-AllVars" ~ "AsimBeamAll:PPL",
      ScenarioName == "wRH-AllModes-PathVars" ~ "AsimBeamAll:Path",
      ScenarioName == "noRH-AllModes-AllVars" ~ "BeamAll:PPL",
      ScenarioName == "noRH-AllModes-PathVars" ~ "BeamAll:Path"
    ))
  plans_sum$legMode <- factor(plans_sum$legMode, 
                                   levels=c("bike","car","hov2","hov2_teleportation","hov3","hov3_teleportation",
                                            "ride_hail","ride_hail_pooled","walk","walk_transit"))
  
  ggplot(plans_sum, aes(x=ScenarioName, y=share, group=legMode, fill=fct_inorder(legMode))) +
    geom_bar(position = "stack",stat = "identity") +
    geom_col(color = 1) +
    theme_bw() + 
    guides(fill = guide_legend(title = "Mode")) +
    theme(text = element_text(size = 8)) +
    scale_fill_manual(values = modesP1, labels=modesL1) +
    xlab("Scenario Name") +
    ylab("Share") +
    theme(axis.text.x = element_text(angle=90, hjust=1))
}

make_events_shift_chart <- function(plan_mode_shifts){
  modesP1 <- c(bike = "springgreen2", bike_transit = "springgreen4", car = "dodgerblue",hov2 = "skyblue",hov2_teleportation = "skyblue3",hov3 = "cadetblue1",hov3_teleportation = 'cadetblue3', ride_hail = "orchid",ride_hail_pooled = "plum1", ride_hail_transit = "violet",walk_transit = "coral1", drive_transit = "coral3", walk = "goldenrod2")
  modesL1 <- c('Bike', 'Bike to Transit', 'Car', 'HOV2','HOV2 Passenger', 'HOV3', 'HOV3 Passenger', "Ride-hail", "Pooled Ride-hail", "Ride-hail to Transit", "Walk to Transit", 'Drive to Transit', "Walk")
  
  plan_mode_shifts$mode <- factor(plan_mode_shifts$mode, 
                          levels=c("bike","bike_transit","car","hov2","hov2_teleportation","hov3","hov3_teleportation",
                                   "ride_hail","ride_hail_pooled","ride_hail_transit","walk","walk_transit","drive_transit"))
  
  
  ggplot(plan_mode_shifts, aes(x = factor(iteration-1), stratum = mode, alluvium = id, fill = mode, label = mode)) +
    scale_fill_manual(values = modesP1, labels = modesL1) +
    geom_flow(color = "darkgray") +
    geom_stratum() +
    theme_bw() + 
    xlab("Iteration") +
    ylab("Number of Trips") +
    theme(legend.position="bottom")
}

make_plans_shift_chart <- function(plan_mode_shifts){
  modesP2 <-c(nomode = "darkgrey",bike = "springgreen2", bike_transit = "springgreen4", car = "dodgerblue",hov2 = "skyblue",hov2_teleportation = "skyblue3",hov3 = "cadetblue1",hov3_teleportation = 'cadetblue3', ride_hail = "orchid",ride_hail_pooled = "plum1", ride_hail_transit = "violet",walk_transit = "coral1", drive_transit = "coral3", walk = "goldenrod2")
  modesL2 <- c('Cleared Mode','Bike', 'Bike to Transit', 'Car', 'HOV2','HOV2 Passenger', 'HOV3', 'HOV3 Passenger', "Ride-hail", "Pooled Ride-hail", "Ride-hail to Transit", "Walk to Transit", 'Drive to Transit', "Walk")
  
  plan_mode_shifts <-plan_mode_shifts %>%
    mutate(legMode = ifelse(legMode == "","nomode",legMode)) %>%
    filter(iteration > 1, iteration < 13)
  
  plan_mode_shifts$legMode <- factor(plan_mode_shifts$legMode, 
                                  levels=c("nomode","bike","bike_transit","car","hov2","hov2_teleportation","hov3","hov3_teleportation",
                                           "ride_hail","ride_hail_pooled","ride_hail_transit","walk","walk_transit","drive_transit"))
  
  
  ggplot(plan_mode_shifts, aes(x = factor(iteration-1), stratum = legMode, alluvium = id, fill = legMode, label = legMode)) +
    scale_fill_manual(values = modesP2, labels = modesL2) +
    geom_flow(color = "darkgray") +
    geom_stratum() +
    theme_bw() + 
    xlab("Iteration") +
    ylab("Number of Trips") +
    theme(legend.position="bottom")
}

make_plans_facet_chart <- function(plan_mode_shifts){
  modesP2 <-c(nomode = "darkgrey",bike = "springgreen2", bike_transit = "springgreen4", car = "dodgerblue",hov2 = "skyblue",hov2_teleportation = "skyblue3",hov3 = "cadetblue1",hov3_teleportation = 'cadetblue3', ride_hail = "orchid",ride_hail_pooled = "plum1", ride_hail_transit = "violet",walk_transit = "coral1", drive_transit = "coral3", walk = "goldenrod2")
  modesL2 <- c('Cleared Mode','Bike', 'Bike to Transit', 'Car', 'HOV2','HOV2 Passenger', 'HOV3', 'HOV3 Passenger', "Ride-hail", "Pooled Ride-hail", "Ride-hail to Transit", "Walk to Transit", 'Drive to Transit', "Walk")
  
  plan_mode_shifts <-plan_mode_shifts %>%
    mutate(legMode = ifelse(legMode == "","nomode",legMode)) %>%
    mutate(bindid = bindid - 1) %>%
    group_by(bindid,iteration) %>%
    arrange(personElement) %>%
    mutate(id = row_number()) %>%
    mutate(iteration = iteration -1) %>%
    filter(iteration >= 1) %>%
    mutate(iteration = ifelse(grepl(".5",as.character(iteration)), "end", "begin"))
  
  plan_mode_shifts$legMode <- factor(plan_mode_shifts$legMode, 
                                     levels=c("nomode","bike","bike_transit","car","hov2","hov2_teleportation","hov3","hov3_teleportation",
                                              "ride_hail","ride_hail_pooled","ride_hail_transit","walk","walk_transit","drive_transit"))

  ggplot(plan_mode_shifts, aes(x = iteration, stratum = legMode, alluvium = id, fill = legMode, label = legMode)) +
    facet_wrap(~bindid, scales="free_x", ncol = 11)+
    scale_fill_manual(values = modesP2, labels = modesL2) +
    geom_flow(color = "darkgray") +
    geom_stratum() +
    theme_bw() + 
    xlab("Iteration") +
    ylab("Number of Trips") +
    theme(legend.position="bottom") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

switch_to_walk <- function(dayhours){
  modesP <- c(bike = "springgreen2", bike_transit = "springgreen4", car = "dodgerblue",hov2 = "skyblue",hov2_teleportation = "skyblue3",hov3 = "cadetblue1",hov3_teleportation = 'cadetblue3', ride_hail = "orchid",ride_hail_pooled = "plum1", ride_hail_transit = "violet",walk_transit = "coral1", drive_transit = "coral3", walk = "goldenrod2")
  modesL1 <- c('Bike', 'Bike to Transit', 'Car', 'HOV2','HOV2 Passenger', 'HOV3', 'HOV3 Passenger', "Ride-hail", "Pooled Ride-hail", "Ride-hail to Transit", "Walk to Transit", 'Drive to Transit', "Walk")

  dayhours$mode <- factor(dayhours$mode, 
                              levels=c("bike","bike_transit","car","hov2","hov2_teleportation","hov3","hov3_teleportation",
                                       "ride_hail","ride_hail_pooled","ride_hail_transit","walk","walk_transit","drive_transit"))
  
  ggplot(dayhours, aes(x = factor(hour),group = mode,fill = factor(mode), y = count)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(values = modesP, labels=modesL1) +
    theme_bw() + 
    guides(fill = guide_legend(title = "Mode")) +
    xlab("Hour of Plan Day") +
    ylab("Number of Trips")
}

