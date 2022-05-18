
library(tidyverse)

## read in files here
eventsFile <- "R/mc_memos/events/slc-all-rhFleet_Existing_1pct_10its_2022-03-31_14-10-40_dtg/ITERS/it.10/10.events.csv"
benchmarkFile <- "R/mc_memos/benchmark.csv"



## modechoice and triparrivalevent tables
eventModes <- read_csv(eventsFile, col_types = cols(.default = "c")) %>%
  filter(type == "ModeChoice") %>%
  group_by(mode) %>%
  select(person, tourIndex, tourPurpose, mode,currentTourMode, income, vehicleOwnership, availableAlternatives, personalVehicleAvailable, length, time, type) %>%
  arrange(person)

eventTripModes <- read_csv(eventsFile, col_types = cols(.default = "c")) %>%
  filter(type == "TripArrivalEvent") %>% 
  group_by(mode) %>%
  select(person, tourIndex, tourPurpose, mode,currentTourMode, income, vehicleOwnership, activityIndex, actType, length, time, type) %>%
  arrange(person)



## modal splits and benchmarck splits comparison graph
modalsplits <- eventTripModes %>%
  group_by(mode) %>% 
  summarise(cnt = n()) %>%
  mutate(pct = cnt / sum(cnt) * 100,iter = 10, sim = "beam") 

benchmark <- read_csv(benchmarkFile) %>%
  select(!cav) %>%
  mutate(hov2_teleportation = hov2/2, hov2 = hov2/2, 
         hov3_teleportation = hov3*2/3, hov3 = hov3/3) %>%
  pivot_longer(!mode, names_to = "modep", values_to = "pct")%>%
  mutate(sim = "asim", mode = modep, iter = 10) %>% select(!modep)

jointmode <- bind_rows(benchmark, modalsplits)

ggplot(jointmode) +
  aes(x = sim, y = pct, fill = mode) +
  geom_bar(position="stack", stat="identity") +
  ggtitle("Mode Choice Comparison") +
  theme_bw() +
  scale_fill_brewer(palette = "Set3")
  


#graph that double checks hov outputs
hovsplits <- eventModes %>%
  group_by(mode) %>%
  filter(mode %in% c("hov2","hov3","hov2_teleportation","hov3_teleportation")) %>%
  summarise(cnt = n()) %>%
  mutate(pct = signif(cnt / sum(cnt) * 100,2),iter = 10)

ggplot(hovsplits) + 
  aes(x = mode,y = pct, fill = mode,label = pct) + 
  ggtitle("Carpooling Modal Split") +
  geom_bar(position="dodge", stat="identity") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  theme_bw()
scale_fill_brewer(palette = "Pastel2")
