
# the following functions create the basic coefficient and constant tables of the models by reading in 
# the input files iteratively or by making the table by scratch. it is put in this file so that they can
# easily be read and created in other files. 

read_utah_cf <- function(cf_files,cf_path){
  for(i in 1:length(cf_files)) { 
    assign(paste0("cf", i),
           read_fwf(file = paste0(cf_path, cf_files[i]), skip = 1, fwf_widths(c(30,1,19,1,100))) %>% 
             na_if("") %>% na.omit %>%
             mutate(purpose = substr(cf_files[i],14,16),
                    variable_type = "coefficient") %>%
             rename(variable = X1, value = X3, description = X5) %>%
             select(purpose, variable, variable_type, value, description)
    )
  } 
  rbind(cf1,cf2,cf3,cf4,cf5)
} 

read_utah_cst <- function(cst_files,cst_path) {
  for(i in 1:length(cst_files)) { 
    assign(paste0("cst", i),
           read_fwf(file = paste0(cst_path, cst_files[i]), skip = 0, fwf_widths(c(13,1,8))) %>% 
             na_if("") %>% na.omit %>%
             mutate(purpose = substr(cst_files[i],11,13),
                    variable_type = "constant") %>%
             rename(variable = X1, value = X3) %>%
             select(purpose, variable, variable_type, value)
    )
  } 
  rbind(cst1,cst2,cst3,cst4,cst5)
}

read_wfrc_cf = function(cf_files,cf_path){
  for(i in 1:length(coeff_files)) { 
    assign(paste0("coeff", i),
           read_fwf(file = paste0(coeff_path, coeff_files[i]), skip = 1, fwf_widths(c(28,1,19,1,100))) %>% 
             na_if("") %>% na.omit %>%
             mutate(purpose = substr(coeff_files[i],1,3),
                    variable_type = "coefficient") %>%
             rename(variable = X1, value = X3, description = X5) %>%
             select(purpose, variable, variable_type, value, description,-X2,-X4)
    )
  }
  rbind(coeff1,coeff2,coeff3,coeff4)
}

read_wfrc_cst = function(cst_files,cst_path){
  for(i in 1:length(const_files)) { 
    assign(paste0("const", i),
           read_fwf(file = paste0(const_path, const_files[i]), skip = 0, fwf_widths(c(19,1,14))) %>% 
             na_if("") %>% na.omit %>%
             mutate(purpose = paste0(substr(const_files[i],1,3),"_",substr(const_files[i],18,19)),
                    variable_type = "constant") %>%
             rename(variable = X1, value = X3) %>%
             select(purpose, variable, variable_type, value)
    )
  }
  rbind(const1,const2,const3,const4,const5,const6,const7)
}

build_nchrp_hbw = function(){
  data.frame(
    purpose = "HBW", 
    variable = c("ivt_coef","ovt_coef","walk_coef","initwait_coef","xferwait_coef","cost_coef"), 
    variable_type = "coefficient",
    model_source = c("H","I","H","H","H","H"),
    value = c(-0.033,-0.05,-0.093,-0.038,-0.038,-0.0021))
}

build_nchrp_hbo = function(){
  data.frame(
    purpose = "HBO",
    variable = c("ivt_coef","ovt_coef","walk_coef","initwait_coef","xferwait_coef","cost_coef","autocost_coef","parkcost_coef","transitcost_coef"), 
    variable_type = "coefficient",
    model_source = c("I","I","J","J","J","D","I","I","I"),
    value = c(-0.008,-0.025,-0.075,-0.050,-0.050,-0.033,-0.01,-0.025,-0.01))
}

build_nchrp_nhb = function(){
  data.frame(
    purpose = "NHB",
    variable = c("ivt_coef","ovt_coef","walk_coef","initwait_coef","xferwait_coef","cost_coef","autocost_coef","parkcost_coef","transitcost_coef"), 
    variable_type = "coefficient",
    model_source = c("M","I","M","M","M","M","I","I","I"),
    value = c(-0.013,-0.05,-0.032,-0.032,-0.050,-0.002,-0.006,-0.016,-0.006))
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# The following table functions are used to build a table with matching variables across the models
# Since each dataset is slightly different, a different function is used to create each table

asim_table <- function(asim_tour_coeffs,purpose){
  asim1<- asim_tour_coeffs %>% select(Variable, {{purpose}}) %>% 
    slice(1:9,14:19) %>%
    mutate (
      term = case_when(grepl("ivt",Variable) ~ "ivtt", TRUE ~ Variable)
    ) %>%
    select(term,{{purpose}}) %>% 
    group_by(term) %>%
    summarize(estimate = {{purpose}}, ave:= mean({{purpose}}), std.error = sd({{purpose}})) %>%
    mutate(
      estimate := case_when(term == "ivtt" ~ ave,TRUE ~ estimate)
    ) %>% 
    distinct() %>% 
    select(-ave) %>%
    mutate(model = "ActivitySim")
}

utah_table <- function(utah_coeff,purp,ovtt_shortwalk,ovtt_longwalk,divider){
  utah_coeff %>% filter(purpose == purp) %>%
    mutate(
      estimate = value/divider,
      term = case_when( 
        variable == "cost_coef" ~ list("cost"),
        grepl("ivt",variable) ~ list("ivtt"),
        grepl("walk_coef_1",variable) ~ list(ovtt_shortwalk),
        grepl("walk_coef_gt_1",variable) ~ list(ovtt_longwalk))
    ) %>%
    unnest(term) %>%
    filter(term != list("none")) %>%
    select(term,estimate) %>%
    mutate(model = "Utah Statewide")
}

wfrc_table <- function(wfrc_coeff,purp,ovt_long,divider){
  wfrc_coeff %>% filter(purpose == purp) %>%
    mutate(
      estimate = value/divider,
      term = case_when( 
        grepl("ivt_coef",variable) ~ list("ivtt"),
        grepl("initwait",variable)~list("wait_time_under_10_min"),
        grepl("xferwait",variable)~list("transfer_time"),
        grepl("drive_coef",variable)~list("egress_time"),
        grepl("walk_coef_1",variable) ~ list("walk_short_dist"),
        grepl("walk_coef_gt_1",variable) ~ list(ovt_long),
        grepl("transfers_coef_drive",variable) ~ list("transfer_number_drive_transit"),
        grepl("transfers_coef_walk",variable)~ list("transfer_number_walk_transit"),
        grepl("cost",variable) ~ list("cost"),
        TRUE ~ list("none"))
    ) %>%
    unnest(term) %>%
    filter(term != list("none")) %>%
    group_by(term) %>%
    summarize(std.error = sd(estimate),estimate = mean(estimate)) %>%
    select(term,estimate,std.error) %>%
    mutate(model = "WFRC")
}

nchrp_table <- function(nchrp_coeff,purp,nchrp_ovt_long,divider){
  nchrp_coeff %>% filter(purpose == purp) %>%
    mutate(
      estimate = value/divider,
      term = case_when( 
        grepl("ivt",variable) ~ list("ivtt"),
        grepl("initwait",variable)~list("wait_time_under_10_min"),
        grepl("xferwait",variable)~list("transfer_time"),
        grepl("ovt",variable)~list(nchrp_ovt_long),
        grepl("cost",variable) ~ list("cost"),
        TRUE ~ list("none"))
    ) %>%
    unnest(term) %>%
    filter(term != list("none")) %>%
    group_by(term) %>%
    summarize(std.error = sd(estimate),estimate = mean(estimate)) %>%
    select(term,estimate,std.error) %>%
    mutate(model = "NCHRP 716")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

#this grapher is just a test I beleive
wgrapher <- function(purp,asim,utah,wfrc,nchrp){
  list(
  "ActivitySim" = asim,
  "Utah Statewide" = utah,
  "WFRC 2019" = wfrc,
  "NCHRP Report716" = nchrp
) %>%
  bind_rows(.id = "model") %>%
  dwplot() + 
  ggtitle(paste0(purp," Utility Parameter Values")) +
  xlab("Coefficient Value") + ylab("Path Variables in BEAM") + 
  coord_flip() + ggpubr::rotate_x_text() 
}

# this grapher is used to compare the utility values of all the models on a negative log based x axis
neg_log_trans = function() trans_new("neg_log", function(x) log(abs(x)), function(x) log(abs(x)))
loggrapher <- function(purp,asim,utah,wfrc,nchrp){ 
  list(
    "ActivitySim" = asim,
    "Utah Statewide" = utah,
    "WFRC 2019" = wfrc,
    "NCHRP Report716" = nchrp
  ) %>%
  bind_rows(.id = "model") %>%
  dwplot(dot_args = list(size=2)) + 
  ggtitle(paste0(purp," Utility Parameter Values")) +
  xlab("Abs(Coefficient Value)") + ylab("Path Variables in BEAM") +
  coord_trans(x="neg_log") + 
  theme_bw() +
  annotation_logticks(scaled = FALSE, alpha = .3,short = unit(15000,"mm"), colour = "gray")
}

# this grapher is used to compare the utility/IVTT ratio values of all the models based on a log based x axis
ivtt_ratio_grapher <- function(purp,asim,utah,wfrc,nchrp){
  list(
    "ActivitySim" = asim,
    "Utah Statewide" = utah,
    "WFRC 2019" = wfrc,
    "NCHRP Report716" = nchrp
  ) %>%
  bind_rows(.id = "model") %>%
  dwplot(dot_args = list(size=2)) + 
  #ggtitle(paste0(purp," Utility Parameter Values")) +
  xlab("(Coeff Value) / IVTT") + ylab("Path Variables in BEAM") +
  scale_x_continuous(trans = "log10", labels = function(x) format(x, scientific = FALSE)) +
   theme_bw()
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# a function that defines the opposite of the in function
'%!in%' <- function(x,y){!('%in%'(x,y))}

# a function that creates a cost coefficient table by purpose
cost_creator <-  function(asim){ asim %>%
  filter(grepl("ivtt",Variable) | grepl("time",Variable)) %>%
  pivot_longer(!Variable, names_to = "purpose", values_to = "value") %>%
  mutate(type = case_when(
    grepl("ivtt",Variable) ~ "ivtt",
    grepl("egress",Variable) ~ "egress",
    grepl("transfer",Variable) ~ "transfer",
    grepl("wait_time_under_10_min",Variable) ~ "wait_time")) %>%
  group_by(purpose,type) %>%
  summarise(ave = mean(value)) %>%
  summarise(time_coef = mean(ave)) %>%
  mutate(cost_coef = time_coef*5)
}



supergrapher <- function(utah_hbw,utah_hbs,utah_hbo,nchrp_hbw,nchrp_hbs,nchrp_hbo,
                       wfrc_hbw,wfrc_hbs,wfrc_hbo,asim_hbw,asim_hbs,asim_hbo){
  alldata <- bind_rows(utah_hbw,utah_hbs,utah_hbo,nchrp_hbw,nchrp_hbs,nchrp_hbo,
                       wfrc_hbw,wfrc_hbs,wfrc_hbo,asim_hbw,asim_hbs,asim_hbo) %>%
    complete(model,type,term) %>%
    mutate(term = case_when(
      term == "bike_long_dist" ~ "Long Bike Distance",
      term ==  "bike_short_dist" ~ "Short Bike Distnace",
      term == "cost" ~ "Cost",
      term == "egress_time" ~ "Egress Time",
      term == "ivtt" ~ "Vehicle Travel Time",
      term == "transfer_number_drive_transit" ~ "Drive to Transit Transfers",
      term == "transfer_number_walk_transit" ~ "Walk to Transit Transfers",
      term == "transfer_time" ~ "Transfer Time",
      term == "wait_time_over_10_min" ~ "Long Wait Time",
      term == "wait_time_under_10_min" ~ "Short Wait Time",
      term == "walk_long_dist" ~ "Long Walk Distance",
      term == "walk_short_dist" ~ "Short Walk Distance",
      TRUE ~ term
    ))
  
  alldata %>%
    dwplot(dot_args = list(size=2)) + 
    facet_wrap(~type) +
    #ggtitle(paste0(purp," Utility Parameter Values")) +
    xlab("(Coeff Value) / (Vehicle Travel Time)") + ylab("Path Utility Parameter Coefficients") +
    scale_x_continuous(trans = "log10", labels = function(x) format(x, scientific = FALSE)) +
    labs(color = "Coefficient Source") +
    theme_bw()  +
    theme(axis.text.x=element_text(angle=90,hjust=1))
}






