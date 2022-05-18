

# not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

#---------------------------------------------------------------------------#
# This function will find and assign the trips that act as the beginning
# or end of a subtour. It filters out "subtours" that are two trips back
# back. It will only get subtours for specific purpose.
get_purpose_subtours <- function(asim_final_trips, purp){
  
  asim_subtable <- asim_final_trips %>%
    
    # make all purposes lowercase
    mutate(purpose = tolower(purpose)) %>%
    
    # find duplicated purposes in the same taz
    arrange(person_id,activity_index) %>%
    group_by(person_id, tour_id) %>%
    mutate(act = paste0(purpose,destination)) %>%
    mutate(sub1 = ifelse(purpose == purp, duplicated(act), FALSE)) %>%
    ungroup() %>% 
  
    # flip table and find the original purposes where dublicates come from
    arrange(person_id,desc(activity_index)) %>% 
    group_by(person_id,tour_id) %>%
    mutate(sub2 = ifelse(purpose == purp, duplicated(act), FALSE)) %>%
    # filter out embedded subtours
    mutate(sub1 = ifelse(duplicated(sub1),FALSE,sub1)) %>% 
    ungroup() %>%
  
    # classify beginning and end of subtours
    arrange(person_id,activity_index) %>% 
    group_by(person_id,tour_id) %>%
    # filter out embedded subtours
    mutate(sub2 = ifelse(duplicated(sub2),FALSE,sub2)) %>% 
    ungroup() %>%
    # define the start and end of a subtour
    mutate(sub = ifelse(sub1 == TRUE, "endsub", ifelse(sub2 == TRUE, "startsub", "none"))) %>%
    arrange(person_id,activity_index) %>%
  
    # delete "subtours" that are simply just two of the same purposes in a row
    mutate(sub = ifelse(sub == "startsub" & lead(sub) == "endsub", "none", 
                        ifelse(sub== "endsub" & lag(sub) == "startsub", "none", 
                               sub))) 
    #filter(person_id %in% c(61,133,1284, 6738, 28819))
  
  # fill in the gaps between the beginning and end of each subtour
  for(i in 1:length(asim_subtable$sub)){
    asim_subtable$sub[i] <- 
      ifelse(i == 0, asim_subtable$sub[i], 
             ifelse(asim_subtable$sub[i-1] %in% c("startsub","midsub") & 
                      asim_subtable$sub[i] %!in% c("startsub","endsub"), 
                    "midsub", 
                    asim_subtable$sub[i]
             )
      )  
  }
  
  # techincally the beginsub is the trip before the beginning of a subtour
  asim_subtable %>%
    select(-sub1, -sub2) %>%
    mutate(sub = ifelse(sub %in% c("endsub", "midsub"),
                        purp, "none"))

}  


#---------------------------------------------------------------------------#
# This function is used to merge together all the separate tables and create 
# a single column that houses information on if the trip is a subtour and if so,
# what type of subtour it is. It creates a list of multiple subtour types if the
# subtour is embedded in another subtour.
merge_subtours <- function(work_subtours, univ_subtours, school_subtours, escort_subtours, 
                           shopping_subtours, eatout_subtours, othmaint_subtours, 
                           social_subtours, othdiscr_subtours, atwork_subtours) {
  
  # merge together all the separatre subtours for each purpose into one table
  subtour_purps <- plyr::join_all(list(
    work_subtours,
    univ_subtours,
    school_subtours,
    escort_subtours,
    shopping_subtours,
    eatout_subtours,
    othmaint_subtours,
    social_subtours,
    othdiscr_subtours,
    atwork_subtours
  ), by = 'trip_id', type = 'left') %>%
    select(1:11,21,31,41,51,61,71,81,91,101) %>%
    
    # the single column schould be nested to account for embedded subtours
    na_if("none") %>%  
    nest(11:20) %>%
    mutate(subtours = lapply(data, function(x) x[!is.na(x)])) %>%
    select(-data) 
  
  subtour_purps
}

