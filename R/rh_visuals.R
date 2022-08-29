
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
    mutate(mean = mean(rhReserveTime),
           max = max(rhReserveTime),
           min = min(rhReserveTime)) %>%
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
    geom_text(aes(label = round(mean,1), y = mean + 1), size = 2)  +  
    xlab("Scenario Name") +
    ylab("Wait Time (min)") +
    theme_bw() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    geom_text(aes(label = round(max,1),  y = max + 1), size = 1.8) +
    geom_text(aes(label = round(min,1),  y = min - 1), size = 1.8)
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

make_plans_shift_chart <- function(plan_mode_shifts){
  getPalette = colorRampPalette(brewer.pal(14, "Set3"))
  
  ggplot(plan_mode_shifts, aes(x = factor(iteration-1), stratum = legMode, alluvium = id, fill = legMode, label = legMode)) +
    scale_fill_manual(values = getPalette(14), labels=c('No Mode', 'Bike','Bike to Transit','Car', 'Drive to Transit', 'HOV2','HOV2 Passenger', 'HOV3', 'HOV3 Passenger', "Ride Hail", "Pooled Ride Hail",'Ride Hail to Transit', "Walk", "Walk to Transit")) +
    geom_flow(color = "darkgray") +
    geom_stratum() +
    theme_bw() + 
    xlab("Iteration") +
    ylab("Number of Trips") +
    theme(legend.position="bottom")
}

switch_to_walk <- function(dayhours){
  getPalette = colorRampPalette(brewer.pal(14, "Set3"))
  ggplot(dayhours, aes(x = factor(hour),group = mode,fill = factor(mode), y = count)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(values = getPalette(14), labels=c('Bike','Bike to Transit','Car', 'Drive to Transit', 'HOV2','HOV2 Passenger', 'HOV3', 'HOV3 Passenger', "Ride Hail", "Pooled Ride Hail",'Ride Hail to Transit', "Walk", "Walk to Transit")) +
    theme_bw() + 
    guides(fill = guide_legend(title = "Mode")) +
    xlab("Hour of Plan Day") +
    ylab("Number of Trips")
}

