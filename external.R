require(tidyverse)
require(scales)
require(markdown)

#Import data
commissioners <- read_csv("data/cc-21june2018.csv") 
voter <- read_csv("data/nc_voter_demographics.csv") 
voter[is.na(voter)] <- 0

###----

#Tidy Commissioners Data
nc_cc_party <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, `Party-VR`) %>%
  count(County, `Party-VR`) %>%
  spread(`Party-VR`, n) %>%
  mutate(GRE = 0, LIB = 0) %>%
  select(County, DEM, GRE, LIB, REP, UNA) %>%
  gather(`Party-VR`, n, DEM:UNA, factor_key=TRUE) %>%
  ungroup() %>%
  complete(County, `Party-VR`, fill = list(n = 0)) %>%
  mutate(`Party-VR` = str_replace(`Party-VR`, "DEM", "Democrats")) %>%
  mutate(`Party-VR` = str_replace(`Party-VR`, "REP", "Republicans")) %>%
  mutate(`Party-VR` = str_replace(`Party-VR`, "GRE", "Green")) %>%
  mutate(`Party-VR` = str_replace(`Party-VR`, "LIB", "Libertarians")) %>%
  mutate(`Party-VR` = str_replace(`Party-VR`, "UNA", "Unaffiliated")) %>%
  mutate(Type = "County Commissioners")

nc_voter_party = voter %>%
  select(county_desc, DEM, REP, GRE, LIB, UNA) %>%
  gather("DEM":"UNA", key = "variable", value = "value") %>%
  mutate(variable = str_replace(variable, "DEM", "Democrats")) %>%
  mutate(variable = str_replace(variable, "REP", "Republicans")) %>%
  mutate(variable = str_replace(variable, "GRE", "Green")) %>%
  mutate(variable = str_replace(variable, "LIB", "Libertarians")) %>%
  mutate(variable = str_replace(variable, "UNA", "Unaffiliated")) %>%
  rename(County = county_desc, `Party-VR` = variable, n = value) %>%
  mutate(Type = "Registered Voters")

party = rbind(nc_cc_party, nc_voter_party)

nc_cc_race <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, Race) %>%
  count(County, Race) %>%
  ungroup() %>%
  complete(County, Race, fill = list(n = 0)) %>%
  spread(Race, n) %>%
  mutate(A = 0, O = 0) %>%
  select(County, A, B, I, M, O, U, W) %>%
  gather(Race, n, A:W, factor_key=TRUE) %>%
  arrange(County)

nc_cc_eth <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, Ethnicity) %>%
  count(County, Ethnicity) %>%
  spread(Ethnicity, n) %>%
  mutate(HL = 0) %>%
  select(County, HL, NL, UN) %>%
  gather(Ethnicity, n, HL:UN, factor_key=TRUE) %>%
  ungroup() %>%
  complete(County, Ethnicity, fill = list(n = 0))

nc_cc_gender <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, Gender) %>%
  count(County, Gender) %>%
  spread(Gender, n)  %>%
  mutate(U = 0) %>%
  gather(Gender, n, F:U, factor_key=TRUE) %>%
  ungroup() %>%
  complete(County, Gender, fill = list(n = 0))

#Palettes
party_palette <- c("DEM" = "#2E86C1", "GRE" = "#17A589", "LIB" = "#D4AC0D", "REP" = "#CB4335", "UNA" = "#884EA0" )

race_palette <- c("A" = "#E74C3C", "B" = "#27AE60", "I" = "#9B59B6", "M" = "#1ABC9C",  "O" = "#F1C40F", "U" = "#C0392B", "W" = "#E67E22")

cc_gender_palette <- c("F" = "#9B59B6", "M" = "#1ABC9C", "U" = "#F4D03F")

voter_gender_palette <- c("Female" = "#9B59B6", "Male" = "#1ABC9C", "UnDisclosedGender" = "#F4D03F")

#Labels
party_labels <- c("DEM" = "Democrats", "REP" = "Republicans", "GRE" = "Green", "LIB" = "Libertarians", "UNA" = "Unaffiliated")

race_labels <- c("A" = "Asian", "B" = "Black", "I" = "Native American", "M" = "Multiracial", "O" = "Other", "U" = "Undisclosed", "W" = "White")

eth_labels <- c("HL" = "Hispanic-Latino", "NL" = "Not Hispanic-Latino", "UN" = "Undisclosed")

cc_gender_labels <- c("F" = "Female", "M" = "Male", "U" = "Undisclosed")

voter_gender_labels <- c("Female" = "Female", "Male" = "Male", "UnDisclosedGender" = "Undisclosed")
