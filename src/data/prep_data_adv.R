
library(tidyverse)
library(readxl)
miceadds::source.all("functions")

#Get player data
players <- read.csv("data/players.csv") %>%
  mutate(player = paste(first,last, sep = " "))

#Get trimmed version
players_trim <- players %>%
  select(player, player_id, first, last, suffix)

#Read in all raw data sources***
data_source_1 <- readxl::read_excel("data/inputs/ucla_v3.xlsx")
data_source_2 <- read.csv("data/inputs/sparta.csv", fileEncoding="UTF-8-BOM")
data_source_3 <- read.csv("data/inputs/nordbord.csv", fileEncoding="UTF-8-BOM")

#Fix data sources columns

### DATA SOURCE 1 ####
data_source_1 <- data_source_1 %>%
  separate(col = 'HS (L/R=F)', c("hs_left", "hs_right", "hs_final"), sep = "([/=])") %>%
  separate(col = 'ILL', c("ill_left", "ill_right", "ill_final", "ill_ac", "ill_extra"), sep = "([/=()])") %>%
  separate(col = 'SM', c("sm_left", "sm_right", "sm_final"), sep = "([/=])") %>%
  separate(col = 'ASLR', c("aslr_left", "aslr_right", "aslr_final"), sep = "([/=])") %>%
  separate(col = 'RS', c("rs_left", "rs_right", "rs_final"), sep = "([/=])") %>%
  rename(player = 'Player',
         height = 'Height',
         weight = '2021 Weight',
         wingspan = 'Wingspan',
         bench = 'Bench',
         vertical_jump = 'Vertical Jump',
         broad_jump = 'Broad Jump',
         delta_10 = 'Delta 10',
         delta_20 = 'Delta 20',
         delta_40 = 'Delta 40',
         peak_40 = 'Peak 40 MPH',
         x3_cone = '3-Cone',
         shuttle = 'Shuttle',
         ods = 'ODS',
         tspu = 'TSPU',
         fms = 'FMS')

#Handle AC for ill movement
data_source_1$ill_ac[data_source_1$ill_ac == "AC"] <- 1
data_source_1$ill_ac[data_source_1$ill_ac == "-AC"] <- -1
data_source_1$ill_ac[is.na(data_source_1$ill_ac)] <- 0

#Get FMS asymmetry 
data_source_1$hs_asym[data_source_1$hs_left != data_source_1$hs_right] <- 1
data_source_1$hs_asym[data_source_1$hs_left == data_source_1$hs_right] <- 0
data_source_1$ill_asym[data_source_1$ill_left != data_source_1$ill_right] <- 1
data_source_1$ill_asym[data_source_1$ill_left == data_source_1$ill_right] <- 0
data_source_1$sm_asym[data_source_1$sm_left != data_source_1$sm_right] <- 1
data_source_1$sm_asym[data_source_1$sm_left == data_source_1$sm_right] <- 0
data_source_1$aslr_asym[data_source_1$aslr_left != data_source_1$aslr_right] <- 1
data_source_1$aslr_asym[data_source_1$aslr_left == data_source_1$aslr_right] <- 0
data_source_1$rs_asym[data_source_1$rs_left != data_source_1$rs_right] <- 1
data_source_1$rs_asym[data_source_1$rs_left == data_source_1$rs_right] <- 0

data_source_1 <- data_source_1 %>%
  mutate(fms_asym = hs_asym + ill_asym + sm_asym + aslr_asym + rs_asym)


### DATA SOURCE 2 ####
data_source_2 <- data_source_2 %>%
  mutate(player = paste(First.name, Last.name, sep = " ")) %>%
  mutate(date = as.Date(Date, format = "%m/%d/%y")) %>%
  rename(sparta_load = 'Load.t.score',
         sparta_explode ='Explode.t.score',
         sparta_drive ='Drive.t.score')

data_source_2 <- get_latest_assessment(data_source_2)

### DATA SOURCE 3 ####
data_source_3 <- data_source_3 %>%
  rename(player = Name) %>%
  separate('Date.UTC', c("day", "month", "year"), "/") %>%
  mutate(date = as.Date(paste(month,day,year, sep = "/"), format = "%m/%d/%y")) %>%
  rename(max_imbalance = Max.Imbalance....,
         impulse_imbalance = Impulse.Imbalance....) %>%
  mutate(max_force = as.numeric(L.Max.Force..N.) + as.numeric(R.Max.Force..N.)) %>%
  mutate(max_impulse = as.numeric(L.Max.Impulse..Ns.) + as.numeric(R.Max.Impulse..Ns.))
  
data_source_3 <- get_latest_assessment(data_source_3)


### CALCULATE DATA ###
get_data(players, players_trim, data_source_1, data_source_2, data_source_3)
