
#MTC ActivitySim =====================================================

get_asim_hbw <- function(data_zip){
  
  asim_tour_coeffs <- read_table("data/model_data/mtc/mtc_2012_tour_mode_choice_coefficients_path.txt")  # multiply all values in table by 2 (tour values need to be compared with trip values later on, and this will allow that)
  cols <- sapply(asim_tour_coeffs, is.numeric)
  asim_tour_coeffs[, cols] <- asim_tour_coeffs[, cols] * 2

  asim_hbw <- asim_table(asim_tour_coeffs,Work)
  asim_hbw_ivtt <- as.numeric(asim_hbw[4,"estimate"])
  asim_hbw_ratio <- asim_hbw %>% mutate(estimate = estimate/asim_hbw_ivtt,std.error = 0)
  asim_hbw_ratio%>%
    mutate(type = "Work")
}

get_asim_hbs <- function(data_zip){
  asim_tour_coeffs <- read_table("data/model_data/mtc/mtc_2012_tour_mode_choice_coefficients_path.txt")
  cols <- sapply(asim_tour_coeffs, is.numeric)
  asim_tour_coeffs[, cols] <- asim_tour_coeffs[, cols] * 2
  
  asim_hbs <- asim_table(asim_tour_coeffs,School)
  asim_hbs_ivtt <- as.numeric(asim_hbs[4,"estimate"])
  asim_hbs_ratio <- asim_hbs %>% mutate(estimate = estimate/asim_hbs_ivtt,std.error = 0)
  asim_hbs_ratio%>%
    mutate(type = "School")
}

get_asim_hbo <- function(data_zip){
  asim_tour_coeffs <- read_table("data/model_data/mtc/mtc_2012_tour_mode_choice_coefficients_path.txt")
  cols <- sapply(asim_tour_coeffs, is.numeric)
  asim_tour_coeffs[, cols] <- asim_tour_coeffs[, cols] * 2
  
  asim_hbo <- asim_table(asim_tour_coeffs,Shop)
  asim_hbo_ivtt <- as.numeric(asim_hbo[4,"estimate"])
  asim_hbo_ratio <- asim_hbo %>% mutate(estimate = estimate/asim_hbo_ivtt,std.error = 0)
  asim_hbo_ratio%>%
    mutate(type = "Other")
}

# Utah Statewide =======================================================
get_utah_data <- function(){
  cf_files <- list.files("data/model_data/utah_statewide/coeffs/coeffs/")
  cf_path <- "data/model_data/utah_statewide/coeffs/coeffs/"
  
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
  utah_coeff <- rbind(cf1,cf2,cf3,cf4,cf5)
  utah_coeff
 }

get_utah_hbw <- function(){
  utah_coeff <- get_utah_data()
  ovtt_shortwalk <-c("wait_time_under_10_min","egress_time","transfer_time","walk_short_dist")
  ovtt_longwalk <-c("wait_time_over_10_min","walk_long_dist")
  utah_hbw_ivtt <- as.numeric(utah_coeff[15,"value"])
  utah_hbw_ratio <- utah_table(utah_coeff,"HBW",ovtt_shortwalk,ovtt_longwalk,utah_hbw_ivtt)
  utah_hbw_ratio%>%
    mutate(type = "Work")
}

get_utah_hbs <- function(){
  utah_coeff <- get_utah_data()
  ovtt_shortwalk <-c("wait_time_under_10_min","egress_time","transfer_time","walk_short_dist")
  ovtt_longwalk <-c("wait_time_over_10_min","walk_long_dist")
  utah_hbs_ivtt <- as.numeric(utah_coeff[8,"value"])
  utah_hbs_ratio <- utah_table(utah_coeff,"HBO",ovtt_shortwalk,ovtt_longwalk,utah_hbs_ivtt)
  utah_hbs_ratio%>%
    mutate(type = "School")
}

get_utah_hbo <- function(){
  utah_coeff <- get_utah_data()
  ovtt_shortwalk <-c("wait_time_under_10_min","egress_time","transfer_time","walk_short_dist")
  ovtt_longwalk <-c("wait_time_over_10_min","walk_long_dist")
  utah_hbo_ivtt <- as.numeric(utah_coeff[8,"value"])
  utah_hbo_ratio <- utah_table(utah_coeff,"HBO",ovtt_shortwalk,ovtt_longwalk,utah_hbo_ivtt)
  utah_hbo_ratio%>%
    mutate(type = "Other")
}

#WFRC =====================================================================
get_wfrc_data <- function(){
  coeff_files <- list.files("data/model_data/wfrc/coeffs/coeffs/")
  coeff_path <- "data/model_data/wfrc/coeffs/coeffs/"
  
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
  wfrc_coeff <- rbind(coeff1,coeff2,coeff3,coeff4)
  wfrc_coeff
}

get_wfrc_hbw <- function(){
  wfrc_coeff <- get_wfrc_data()
  ovt_long <-c("wait_time_over_10_min","walk_long_dist")
  wfrc_hbw_ivtt <- as.numeric(wfrc_coeff[41,"value"])
  wfrc_hbw_ratio <- wfrc_table(wfrc_coeff,"HBW",ovt_long,wfrc_hbw_ivtt)
  wfrc_hbw_ratio %>%
    mutate(type = "Work")
}

get_wfrc_hbs <- function(){
  wfrc_coeff <- get_wfrc_data()
  ovt_long <-c("wait_time_over_10_min","walk_long_dist")
  wfrc_hbs_ivtt <- as.numeric(wfrc_coeff[1,"value"])
  wfrc_hbs_ratio <- wfrc_table(wfrc_coeff,"HBC",ovt_long,wfrc_hbs_ivtt)
  wfrc_hbs_ratio%>%
    mutate(type = "School")
}

get_wfrc_hbo <- function(){
  wfrc_coeff <- get_wfrc_data()
  ovt_long <-c("wait_time_over_10_min","walk_long_dist")
  wfrc_hbo_ivtt <- as.numeric(wfrc_coeff[20,"value"])
  wfrc_hbo_ratio <- wfrc_table(wfrc_coeff,"HBO",ovt_long,wfrc_hbo_ivtt)
  wfrc_hbo_ratio%>%
    mutate(type = "Other")
}

#NCHRP ============================================================================
get_nchrp_data <- function(){
  nchrp_hbw <- data.frame(
    purpose = "HBW", 
    variable = c("ivt_coef","ovt_coef","walk_coef","initwait_coef","xferwait_coef","cost_coef"), 
    variable_type = "coefficient",
    model_source = c("H","I","H","H","H","H"),
    value = c(-0.033,-0.05,-0.093,-0.038,-0.038,-0.0021))
  nchrp_hbo <- data.frame(
    purpose = "HBO",
    variable = c("ivt_coef","ovt_coef","walk_coef","initwait_coef","xferwait_coef","cost_coef","autocost_coef","parkcost_coef","transitcost_coef"), 
    variable_type = "coefficient",
    model_source = c("I","I","J","J","J","D","I","I","I"),
    value = c(-0.008,-0.025,-0.075,-0.050,-0.050,-0.033,-0.01,-0.025,-0.01))
  nchrp_nhb <- data.frame(
    purpose = "NHB",
    variable = c("ivt_coef","ovt_coef","walk_coef","initwait_coef","xferwait_coef","cost_coef","autocost_coef","parkcost_coef","transitcost_coef"), 
    variable_type = "coefficient",
    model_source = c("M","I","M","M","M","M","I","I","I"),
    value = c(-0.013,-0.05,-0.032,-0.032,-0.050,-0.002,-0.006,-0.016,-0.006))
  nchrp_coeff <- rbind(nchrp_hbw,nchrp_hbo,nchrp_nhb)
  nchrp_coeff
}

get_nchrp_hbw <- function(){
  nchrp_coeff <- get_nchrp_data()
  nchrp_ovt_long <-c("wait_time_over_10_min","egress_time")
  nchrp_hbw_ivtt <- as.numeric(nchrp_coeff[1,"value"])
  nchrp_hbw_ratio <- nchrp_table(nchrp_coeff,"HBW",nchrp_ovt_long,nchrp_hbw_ivtt)
  nchrp_hbw_ratio%>%
    mutate(type = "Work")
}

get_nchrp_hbs <- function(){
  nchrp_coeff <- get_nchrp_data()
  nchrp_ovt_long <-c("wait_time_over_10_min","egress_time")
  nchrp_hbs_ivtt <- as.numeric(nchrp_coeff[7,"value"])
  nchrp_hbs_ratio <- nchrp_table(nchrp_coeff,"HBO",nchrp_ovt_long,nchrp_hbs_ivtt)
  nchrp_hbs_ratio%>%
    mutate(type = "School")
}

get_nchrp_hbo <- function(){
  nchrp_coeff <- get_nchrp_data()
  nchrp_ovt_long <-c("wait_time_over_10_min","egress_time")
  nchrp_hbo_ivtt <- as.numeric(nchrp_coeff[7,"value"])
  nchrp_hbo_ratio <- nchrp_table(nchrp_coeff,"HBO",nchrp_ovt_long,nchrp_hbo_ivtt)
  nchrp_hbo_ratio%>%
    mutate(type = "Other")
}  