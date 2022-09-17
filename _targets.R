# target libraries
library(targets)
library(tarchetypes)
library(future)
library(future.apply)
library(furrr)

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "bookdown", "readr", "dotwhisker", 
                            "data.table",
                            "ggpubr", "scales", "future", "future.apply", "furrr",
                            "data.table", "ggrepel", "knitr", "kableExtra",
                            "ggalluvial", "RColorBrewer"))

#Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/asim_path_validation.R")
source("R/table_maker.R")
source("R/data_helpers.R")
source("R/rh_events.R")
source("R/rh_plans.R")
source("R/rh_visuals.R")

message("Virtual memory size: ", Sys.getenv("R_MAX_VSIZE"))


data_targets <- tar_plan(
  ## Data
  tar_target(data_zip, get_data_from_box()),
  
  
  ## ASIM Utility Coefficient Validation
  tar_target(utah_hbw,  get_utah_hbw()),
  tar_target(utah_hbs,  get_utah_hbs()),
  tar_target(utah_hbo,  get_utah_hbo()),
  tar_target(nchrp_hbw, get_nchrp_hbw()),
  tar_target(nchrp_hbs, get_nchrp_hbs()),
  tar_target(nchrp_hbo, get_nchrp_hbo()),
  tar_target(wfrc_hbw,  get_wfrc_hbw()),
  tar_target(wfrc_hbo,  get_wfrc_hbo()),
  tar_target(wfrc_hbs,  get_wfrc_hbs()),
  tar_target(asim_hbs,  get_asim_hbs()),
  tar_target(asim_hbw,  get_asim_hbw()),
  tar_target(asim_hbo,  get_asim_hbo()),
  
  ## ASIM Statistics
  tar_target(asim_plans, read_csv("data/asim_plans_rh.csv")),
  
  ## Ride Hail Event Handler data
  tar_target(all_all_wrh,  unzip_data("data/events/12.events-15pct-wRH-all-all.csv",   data_zip), format = "file"),
  tar_target(all_path_wrh, unzip_data("data/events/12.events-15pct-wRH-all-path.csv",  data_zip), format = "file"),
  tar_target(rh_all_wrh,   unzip_data("data/events/12.events-15pct-wRH-rh-all.csv",    data_zip), format = "file"),
  tar_target(rh_path_wrh,  unzip_data("data/events/12.events-15pct-wRH-rh-path.csv",   data_zip), format = "file"),
  tar_target(none_wrh,     unzip_data("data/events/12.events-15pct-wRH-none.csv",      data_zip), format = "file"),
  tar_target(all_all_norh, unzip_data("data/events/12.events-15pct-noRH-all-all.csv",  data_zip), format = "file"),
  tar_target(all_path_norh,unzip_data("data/events/12.events-15pct-noRH-all-path.csv", data_zip), format = "file"),
  tar_target(rh_all_norh,  unzip_data("data/events/12.events-15pct-noRH-rh-all.csv",   data_zip), format = "file"),
  tar_target(rh_path_norh, unzip_data("data/events/12.events-15pct-noRH-rh-path.csv",  data_zip), format = "file"),
  tar_target(none_norh,    unzip_data("data/events/12.events-15pct-noRH-none.csv",     data_zip), format = "file"),
  
  tar_target(driverfleet, unzip_data("data/Driverfleet_SLC.csv", data_zip), format = "file"),
  
  ## Scenario List
  tar_target(scenario_list,  list(
    "All Modes - All Variables - W/ RH" = all_all_wrh,
    "All Modes - Path Variables - W/ RH" = all_path_wrh,
    "RH Modes - All Variables - W/ RH" = rh_all_wrh,
    "RH Modes - Path Variables - W/ RH" = rh_path_wrh,
    "No Modes - W/ RH" = none_wrh,
    "All Modes - All Variables - No RH" = all_all_norh,
    "All Modes - Path Variables - No RH" = all_path_norh,
    "RH Modes - All Variables - No RH" = rh_all_norh,
    "RH Modes - Path Variables - No RH" = rh_path_norh,
    "No Modes - No RH" = none_norh
  )),
  
  ## Important Columns
  tar_target(cols,  c("person",
           "vehicle",
           "time",
           "type",
           "mode",
           "legMode",
           "vehicleType",
           "arrivalTime",
           "departureTime",
           "departTime",
           "length",
           "numPassengers",
           "actType",
           "personalVehicleAvailable"
  )),
  
  tar_target(events_list, read_all_events(scenario_list, cols)),
  events_waittime_list = events_list[-10],
  
  short_list = list("All Modes - All Variables - W/ RH" = all_all_wrh),
  short_cols = c("person","vehicle","time","type","mode","legMode","vehicleType","arrivalTime","departureTime","departTime","length","numPassengers","actType","personalVehicleAvailable","tourIndex"),
  events1 = future_map(short_list, read_events,short_cols),
  
  # plan files
  tar_target(wRH_all_all_p0file,    unzip_data("data/plans/final-plans/0.plans-wRH-All-All.csv",    data_zip), format = "file"),
  tar_target(wRH_all_all_p12file,   unzip_data("data/plans/final-plans/12.plans-wRH-All-All.csv",   data_zip), format = "file"),
  tar_target(wRH_all_path_p0file,   unzip_data("data/plans/final-plans/0.plans-wRH-All-Path.csv",   data_zip), format = "file"),
  tar_target(wRH_all_path_p12file,  unzip_data("data/plans/final-plans/12.plans-wRH-All-Path.csv",  data_zip), format = "file"),
  tar_target(noRH_all_all_p0file,   unzip_data("data/plans/final-plans/0.plans-noRH-All-All.csv",   data_zip), format = "file"),
  tar_target(noRH_all_all_p12file,  unzip_data("data/plans/final-plans/12.plans-noRH-All-All.csv",  data_zip), format = "file"),
  tar_target(noRH_all_path_p0file,  unzip_data("data/plans/final-plans/0.plans-noRH-All-Path.csv",  data_zip), format = "file"),
  tar_target(noRH_all_path_p12file, unzip_data("data/plans/final-plans/12.plans-noRH-All-Path.csv", data_zip), format = "file"),
  
  
  # Ride Hail Plan Handler
  tar_target(wRH_all_all_p0,    read_plans(wRH_all_all_p0file)),
  tar_target(wRH_all_all_p12,   read_plans(wRH_all_all_p12file)),
  tar_target(wRH_all_path_p0,   read_plans(wRH_all_path_p0file)),
  tar_target(wRH_all_path_p12,  read_plans(wRH_all_path_p12file)),
  tar_target(noRH_all_all_p0,   read_plans(noRH_all_all_p0file)),
  tar_target(noRH_all_all_p12,  read_plans(noRH_all_all_p12file)),
  tar_target(noRH_all_path_p0,  read_plans(noRH_all_path_p0file)),
  tar_target(noRH_all_path_p12, read_plans(noRH_all_path_p12file))
)

analysis_targets <- tar_plan(
  #Ride Hail Statistics
  tar_target(mode_choice_table, all_join(events_list, mode_choice, "mode", "mode")),
  tar_target(num_passengers, all_join(events_list, rh_pass, "numPassengers", "num_passengers")),
  tar_target(wait_times, rbind_join(events_waittime_list, rh_waittimes)),
  tar_target(old_wait_times, all_join(events_list, rh_times, "summary", "values")),
  tar_target(travel_times, all_join(events_list, rh_travel_times, "summary", "values")),
  tar_target(rh_to_transit, all_join(events_list, count_rh_transit_transfers, "transferType", "transfer_type")),
  tar_target(fleet_hours, total_fleet_hours(driverfleet)),
  tar_target(rh_passenger_time, all_join(events_list,rh_pass_time,"name","totalPassengerTime")),
  tar_target(ridehail_utilization, rh_utilization(rh_passenger_time,num_passengers,fleet_hours)),
  
  #Across Day Statistics
  tar_target(wRH_all_all_sum, rh_switch(wRH_all_all_p0,wRH_all_all_p12)), 
  tar_target(wRH_all_path_sum, rh_switch(wRH_all_path_p0,wRH_all_path_p12)),
  tar_target(noRH_all_all_sum, rh_switch(noRH_all_all_p0,noRH_all_all_p12)),
  tar_target(noRH_all_path_sum, rh_switch(noRH_all_path_p0,noRH_all_path_p12)),
  
  tar_target(plansbind, bind_plans(wRH_all_all_sum,wRH_all_path_sum,noRH_all_all_sum,noRH_all_path_sum)),
  tar_target(full_plans, read_full_plans(unzip_data("data/plans/wRH-all-all-plans/", data_zip))),
  tar_target(mode_shift, mode_shifts(full_plans)),
  
  #Daily Plan Statistics
  tar_target(dayhours, walk_analysis(events1[[1]]))
  
)

visual_targets <- tar_plan(
  #ASIM Coefficient Graphs
  tar_target(coef_graph, supergrapher(utah_hbw,utah_hbs,utah_hbo,nchrp_hbw,nchrp_hbs,nchrp_hbo,
                                      wfrc_hbw,wfrc_hbs,wfrc_hbo,asim_hbw,asim_hbs,asim_hbo)),
 
  # Ride Hail Result Visuals
  tar_target(ridership, format_ridership_table(mode_choice_table, asim_plans)),
  tar_target(waits, format_waits_graph(wait_times)),
  
  #Across Day and Daily Plan Analysis
  tar_target(piechart, pie_chart(plansbind)),
  tar_target(planshifts, make_plans_shift_chart(mode_shift)),
  tar_target(walk_switchers, switch_to_walk(dayhours))
)

tar_plan(
  data_targets,
  analysis_targets,
  visual_targets
)

