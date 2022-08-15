library(targets)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "bookdown", "sf", "cowplot", "ggspatial",
                            "DiagrammeR", "RAM", "readr", "tufte", "dotwhisker",
                            "ggpubr", "scales"))
 
# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/asim_path_validation.R")
source("R/table_maker.R")
source("R/map_maker.R")

# Targets necessary to build your data and run your model
map_targets <- list(
  tar_target(tazmap, make_taz_map()),
  tar_target(bignet,make_bignet_map()),
  tar_target(bignetpic, get_bignet_pic(bignet)),
  tar_target(smallnet,make_smallnet_map()),
  tar_target(smallnetpic, get_smallnet_pic(smallnet))
)

diagram_targets <- list(
  tar_target(mnlflow, make_mnl_flow()),
  tar_target(lccmflow, make_lccm_flow()),
  tar_target(tpcmflow, make_tpcm_flow())
)

validation_targets <- list(
  tar_target(asim_hbw, get_asim_hbw()),
  tar_target(utah_hbw, get_utah_hbw()),
  tar_target(wfrc_hbw, get_wfrc_hbw()),
  tar_target(nchrp_hbw, get_nchrp_hbw()),
  tar_target(hbw_graph, ivtt_ratio_grapher("Home-Based Work", asim_hbw, utah_hbw,wfrc_hbw,nchrp_hbw)),

  tar_target(asim_hbs, get_asim_hbs()),
  tar_target(utah_hbs, get_utah_hbs()),
  tar_target(wfrc_hbs, get_wfrc_hbs()),
  tar_target(nchrp_hbs, get_nchrp_hbs()),
  tar_target(hbs_graph, ivtt_ratio_grapher("Home-Based School",asim_hbs,utah_hbs,wfrc_hbs,nchrp_hbs)),
  
  tar_target(asim_hbo, get_asim_hbo()),
  tar_target(utah_hbo, get_utah_hbo()),
  tar_target(wfrc_hbo, get_wfrc_hbo()),
  tar_target(nchrp_hbo, get_nchrp_hbo()),
  tar_target(hbo_graph, ivtt_ratio_grapher("Home-Based Other",asim_hbo,utah_hbo,wfrc_hbo,nchrp_hbo))
)

# End this file with a list of target objects.
list(
  map_targets,
  diagram_targets,
  validation_targets
)
