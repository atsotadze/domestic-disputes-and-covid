library(tidyverse)
library(readr)
library(primer.data)
library(janitor)
library(ggplot2)
library(gcookbook)
library(lubridate)
library(tidycensus)
library(anytime)
library(rstanarm)
library(ggdist)
library(gt)
library(janitor)
library(broom.mixed)
library(gtsummary)

###################### COVID-19 DATA SECTION ############################

# Read covid data
covid_data <- read_csv("covid-and-domestic-dispuits/rawdata/covid_confirmed.csv", 
                       col_types = cols(.default = col_double(),
                                        `County Name` = col_character(),
                                        State = col_character(),
                                        StateFIPS = col_character())) %>% 
  filter(`County Name` == "Baltimore County" |
           `County Name` == "Hamilton County" |
           `County Name` == "Los Angeles County" |
           `County Name` == "King County" | 
           `County Name` == "Orange County") %>%
  filter(countyFIPS == "24005" |
           countyFIPS == "39061" |
           countyFIPS == "6037" |
           countyFIPS == "53033" |
           countyFIPS == "12095") %>%
  rowwise() %>%
  mutate(total_covid_cases = sum(c_across(c(`2020-01-22`:`2021-03-11`)))) %>%
  mutate(January_2020 = sum(c_across(c(`2020-01-22`:`2020-01-31`)))) %>%
  mutate(February_2020 = sum(c_across(c(`2020-02-01`:`2020-02-29`)))) %>%
  mutate(March_2020 = sum(c_across(c(`2020-03-01`:`2020-03-31`)))) %>%
  mutate(April_2020 = sum(c_across(c(`2020-04-01`:`2020-04-30`)))) %>%
  mutate(May_2020 = sum(c_across(c(`2020-05-01`:`2020-05-31`)))) %>%
  mutate(June_2020 = sum(c_across(c(`2020-06-01`:`2020-06-30`)))) %>%
  mutate(July_2020 = sum(c_across(c(`2020-07-01`:`2020-07-31`)))) %>%
  mutate(August_2020 = sum(c_across(c(`2020-08-01`:`2020-08-31`)))) %>%
  mutate(September_2020 = sum(c_across(c(`2020-09-01`:`2020-09-30`)))) %>%
  mutate(October_2020 = sum(c_across(c(`2020-10-01`:`2020-10-31`)))) %>%
  mutate(November_2020 = sum(c_across(c(`2020-11-01`:`2020-11-30`)))) %>%
  mutate(December_2020 = sum(c_across(c(`2020-12-01`:`2020-12-31`)))) %>%
  mutate(January_2021 = sum(c_across(c(`2021-01-01`:`2021-01-31`)))) %>%
  mutate(February_2021 = sum(c_across(c(`2021-02-01`:`2021-02-28`)))) %>%
  mutate(March_2021 = sum(c_across(c(`2021-03-01`:`2021-03-11`)))) %>%
  select(countyFIPS, `County Name`, State, total_covid_cases, January_2020, 
         February_2020, March_2020, April_2020, May_2020,
         June_2020, July_2020, August_2020, September_2020,
         October_2020, November_2020, December_2020, January_2021, 
         February_2021, March_2021) %>%
  pivot_longer(names_to = "Month", 
               values_to = "COVID-19 Cases",
               cols = January_2020 : March_2021) %>%
  separate(Month, c("Month", "Year"), "_") 


saveRDS(covid_data, "covid-and-domestic-dispuits/cleandata/covid-data.rds")

# create a plot for milestone 6
covid_data %>%
  ggplot(aes(x = `County Name`,
             y = total_covid_cases)) +
  geom_point() +
  labs(title = "Total Covid Cases by County between 2020-01-22 and 2021-03-11",
       x = " ",
       y = "Total Covid Cases",
       caption = "Source: USA Today COVID-19 Data") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  coord_flip() +
  theme_classic()


###################### RACE VARS DATA SECTION ############################

# Race variables 
racevars <- c(White = "B02001_002", 
              Black = "B02001_003", 
              Asian = "B02001_005",
              Hispanic = "B03003_003")

###################### BALTIMORE DATA SECTION ############################

# Read in police data for Baltimore, Maryland 

baltimore <- read_csv("covid-and-domestic-dispuits/rawdata/Baltimore_911.csv",
                      col_types = cols(OBJECTID = col_double(),
                                       recordId = col_double(),
                                       callKey = col_character(),
                                       callDateTime = col_character(),
                                       priority = col_character(),
                                       district = col_character(),
                                       description = col_character(),
                                       callNumber = col_character(),
                                       incidentLocation = col_character(),
                                       location = col_character(),
                                       Neighborhood = col_character(),
                                       PoliceDistrict = col_character(),
                                       PolicePost = col_double(),
                                       CouncilDistrict = col_double(),
                                       SheriffDistricts = col_character(),
                                       Community_Statistical_Areas = col_character(),
                                       Census_Tracts = col_character(),
                                       VRIZones = col_character(),
                                       ZIPCode = col_double())) %>%
  filter(description == "FAMILY DISTURB") %>%
  mutate(countyFIPS = "24005") %>%
  select(callDateTime, description, countyFIPS) %>%
  separate(callDateTime, into = c("Day", NA), sep = " ") %>%
  mutate(date = as.Date(Day)) %>%
  select(-(Day)) 

# Get ACS data on Baltimore County 

baltimore_acs <- get_acs(geography = "county",
                         variables = racevars,
                         year = 2018,
                         state = "MD",
                         county = "Baltimore County",
                         geometry = TRUE,
                         summary_var = "B02001_001") %>%
  select(GEOID, NAME, variable, estimate) %>%
  rename(countyFIPS = GEOID)

graph_baltimore <- baltimore %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(total = n(), .groups = "drop") 

# Join the two datasets for Baltimore data 

baltimore_merged_data <- baltimore %>%
  inner_join(baltimore_acs, by = "countyFIPS") %>%
  mutate(countyFIPS = as.integer(countyFIPS)) %>%
  arrange(date) %>%
  group_by(date) %>%
  inner_join(graph_baltimore) 


# Save the clean data to cleandata folder 

saveRDS(baltimore_merged_data, "covid-and-domestic-dispuits/cleandata/baltimore-data.rds")


###################### CINCINNATI DATA SECTION ############################

# Read police data for Cincinnati, Ohio 
cincinnati <- read_csv("covid-and-domestic-dispuits/rawdata/Cinncinati_911.csv", 
                       col_types = cols(ADDRESS_X = col_character(),
                                        LATITUDE_X = col_double(),
                                        LONGITUDE_X = col_double(),
                                        AGENCY = col_character(),
                                        CREATE_TIME_INCIDENT = col_character(),
                                        DISPOSITION_TEXT = col_character(),
                                        EVENT_NUMBER = col_character(),
                                        INCIDENT_TYPE_ID = col_character(),
                                        INCIDENT_TYPE_DESC = col_character(),
                                        PRIORITY = col_double(),
                                        PRIORITY_COLOR = col_character(),
                                        ARRIVAL_TIME_PRIMARY_UNIT = col_character(),
                                        CLOSED_TIME_INCIDENT = col_character(),
                                        DISPATCH_TIME_PRIMARY_UNIT = col_character(),
                                        BEAT = col_character(),
                                        COMMUNITY_COUNCIL_NEIGHBORHOOD = col_character(),
                                        DISTRICT = col_double(),
                                        SNA_NEIGHBORHOOD = col_character(),
                                        CPD_NEIGHBORHOOD = col_character())) %>%
  filter(INCIDENT_TYPE_ID == "FAMTRB") %>% 
  mutate(countyFIPS = "39061") %>%
  mutate(desc = INCIDENT_TYPE_ID) %>%
  mutate(description = "FAMILY DISTURB") %>%
  select(description, countyFIPS, CREATE_TIME_INCIDENT) %>%
  separate(CREATE_TIME_INCIDENT, into = c("Day", NA), sep = " ") %>%
  mutate(date = as.Date(Day)) %>%
  select(-(Day))


# Read in Hamilton County acs data

cincinnati_acs <- get_acs(geography = "county",
                          variables = racevars, 
                          year = 2018,
                          state = "OH",
                          county = "Hamilton County",
                          geometry = TRUE,
                          summary_var = "B02001_001") %>%
  select(GEOID, NAME, variable, estimate) %>%
  rename(countyFIPS = GEOID) 


# Join the two datasets for Cincinnati data 

cincinnati_merged_data <- cincinnati %>%
  inner_join(cincinnati_acs, by = "countyFIPS") %>%
  mutate(countyFIPS = as.integer(countyFIPS))

# Save the clean data to cleandata folder 

saveRDS(cincinnati_merged_data, "covid-and-domestic-dispuits/cleandata/cincinnati-data.rds")

###################### LOS ANGELES DATA SECTION ############################

# Read police data for Los Angeles, California
los_angeles <- read_csv("covid-and-domestic-dispuits/rawdata/LAPD_911.csv", 
                        col_types = cols(Incident_Number = col_character(),
                                         Area_Occ = col_character(),
                                         Rpt_Dist = col_character(),
                                         Dispatch_Date = col_character(),
                                         Dispatch_Time = col_time(format = ""),
                                         Call_Type_Code = col_character(),
                                         Call_Type_Text = col_character())) %>%
  filter(Call_Type_Text %in% c("DOM VIOL", "MAN/WMN", "DOM VIOL SUSP", 
                               "MAN ASSLTG WMN", "FAMILY", "DOM VIOL R/O",
                               "ABUSE INVEST")) %>%
  mutate(description = "FAMILY DISTURB") %>%
  select(description, Dispatch_Date) %>%
  mutate(countyFIPS = "6037") %>%
  separate(Dispatch_Date, into = c("Day", NA), sep = " ") %>%
  mutate(date = anydate(Day)) %>%
  select(-(Day))

# Get Los Angeles County acs data

los_angeles_acs <- get_acs(geography = "county",
                           variables = racevars, 
                           year = 2018,
                           state = "CA",
                           county = "Los Angeles County",
                           geometry = TRUE,
                           summary_var = "B02001_001") %>%
  mutate(countyFIPS = "6037") %>%
  select(countyFIPS, NAME, variable, estimate) 

# Join the two datasets for Los Angeles data 

los_angeles_merged_data <- los_angeles %>%
  inner_join(los_angeles_acs, by = "countyFIPS") %>%
  mutate(countyFIPS = as.integer(countyFIPS))

# Save the clean data to cleandata folder 

saveRDS(los_angeles_merged_data, "covid-and-domestic-dispuits/cleandata/los-angeles-data.rds")


###################### ORLANDO DATA SECTION ############################

# Read police data for Orlando, Florida

orlando <- read_csv("covid-and-domestic-dispuits/rawdata/Orlando_911.csv", 
                    col_types = cols(`Incident Number` = col_character(),
                                     `Incident Date Time` = col_character(),
                                     `Incident Location` = col_character(),
                                     `Incident Type` = col_character(),
                                     `Incident Disposition Class` = col_character(),
                                     `Incident Disposition` = col_character(),
                                     Status = col_character(),
                                     Location = col_character())) %>%
  filter(`Incident Type` %in% c("Domestic disturbanc", "House/business check")) %>%
  mutate(description = "FAMILY DISTURB") %>%
  select(`Incident Date Time`, description) %>%
  mutate(countyFIPS = "12095") %>%
  separate(`Incident Date Time`, into = c("Day", NA), sep = " ") %>%
  mutate(date = anydate(Day)) %>%
  select(-(Day)) %>%
  arrange(desc(date)) %>%
  filter(date >= "2020-01-01") 

# Get Orange County acs data

orlando_acs <- get_acs(geography = "county",
                       variables = racevars, 
                       year = 2018,
                       state = "FL",
                       county = "Orange County",
                       geometry = TRUE,
                       summary_var = "B02001_001") %>%
  select(GEOID, NAME, variable, estimate) %>%
  rename(countyFIPS = GEOID)

# Join the two datasets for Orlando data 

orlando_merged_data <- orlando %>%
  inner_join(orlando_acs, by = "countyFIPS") %>%
  mutate(countyFIPS = as.integer(countyFIPS))

# Save the clean data to cleandata folder 

# saveRDS(orlando_merged_data, "covid-and-domestic-dispuits/cleandata/orlando-data.rds")

###################### SEATTLE DATA SECTION ############################

# Read data for Seattle, Washington

seattle <- read_csv("covid-and-domestic-dispuits/rawdata/Seattle_911.csv", 
                    col_types = cols(`CAD Event Number` = col_double(),
                                     `Event Clearance Description` = col_character(),
                                     `Call Type` = col_character(),
                                     Priority = col_double(),
                                     `Initial Call Type` = col_character(),
                                     `Final Call Type` = col_character(),
                                     `Original Time Queued` = col_character(),
                                     `Arrived Time` = col_character(),
                                     Precinct = col_character(),
                                     Sector = col_character(),
                                     Beat = col_character())) %>%
  filter(`Initial Call Type` %in% c("PREMISE CHECK, OFFICER INITIATED ONVIEW ONLY",
                                    "DISTURBANCE, MISCELLANEOUS/OTHER", 
                                    "SERVICE - WELFARE CHECK", "DIST - DV - NO ASLT",
                                    "REQUEST TO WATCH", "ASLT - IP/JO - DV", 
                                    "PEACE-STANDBY TO ASSURE (NO COURT ORDR SVC)",
                                    "THREATS (INCLS IN-PERSON/BY PHONE/IN WRITING)")) %>%
  mutate(description = "FAMILY DISTURB") %>%
  select(description, `Original Time Queued`) %>%
  mutate(countyFIPS = "53033") %>%
  separate(`Original Time Queued`, into = c("Day", NA), sep = " ") %>%
  mutate(date = anydate(Day)) %>%
  select(-(Day)) %>%
  arrange(desc(date)) %>%
  filter(date >= "2020-01-01")


# King County acs data

seattle_acs <- get_acs(geography = "county",
                       variables = racevars, 
                       year = 2018,
                       state = "WA",
                       county = "King County",
                       geometry = TRUE,
                       summary_var = "B02001_001") %>%
  select(GEOID, NAME, variable, estimate) %>%
  rename(countyFIPS = GEOID)

# Join the two datasets for Orlando data 

seattle_merged_data <- seattle %>%
  inner_join(seattle_acs, by = "countyFIPS") %>%
  mutate(countyFIPS = as.integer(countyFIPS))



# Save the clean data to cleandata folder 

saveRDS(seattle_merged_data, "covid-and-domestic-dispuits/cleandata/seattle-data.rds")


####################### PLOTTING SECTION ############################

# baltimore domestic despute graph 


graph_cincinnati <- cincinnati %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(total = n(), .groups = "drop") %>%
  drop_na()


graph_los_angeles <- los_angeles %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(total = n(), .groups = "drop") 


graph_orlando <- orlando %>%
  group_by(date) %>%
  summarise(total = n(), .groups = "drop")


graph_seattle <- seattle %>%
  group_by(date) %>%
  summarise(total = n(), .groups = "drop")

saveRDS(graph_baltimore, "covid-and-domestic-dispuits/cleandata/baltimore-data.rds")
saveRDS(graph_cincinnati, "covid-and-domestic-dispuits/cleandata/cincinnati-data.rds")
saveRDS(graph_los_angeles, "covid-and-domestic-dispuits/cleandata/los-angeles-data.rds")
saveRDS(graph_orlando, "covid-and-domestic-dispuits/cleandata/orlando-data.rds")
saveRDS(graph_seattle, "covid-and-domestic-dispuits/cleandata/seattle-data.rds")

####################### BALTIMORE MODEL SECTION ############################

data_baltimore <- baltimore_merged_data %>%
  inner_join(graph_baltimore, by = "date")

 graph_baltimore_monthly <- tibble(Month = c("January", "February", "March", "April",
                                            "May", "June", "July", "August", "September",
                                            "October", "November", "December"),
                                  Year = rep("2020", 12),
                                  domestic_disputes = c(1697, 1504, 1860, 1708, 1932, 1888,
                                                  1742, 1925, 1875, 1815, 1759, 1768))

final_baltimore_data <- covid_data %>%
  filter(State == "MD") %>%
  filter(Year == "2020") %>%
  inner_join(graph_baltimore_monthly, by = "Month") %>%
  select(State, Month, Year.x, `COVID-19 Cases`, domestic_disputes) %>%
  mutate(Year = Year.x) %>%
  mutate(countyFIPS = "24005") %>%
  select(-(Year.x)) %>%
  inner_join(baltimore_acs, by = "countyFIPS")
  # pivot_wider(names_from = variable,
  #             values_from = estimate)


fit_1 <- stan_glm(data = final_baltimore_data,
                  formula = domestic_disputes ~ `COVID-19 Cases`,
                  family = gaussian,
                  seed = 12,
                  refresh = 0)

print(fit_1, digits = 5)

saveRDS(fit_1, "covid-and-domestic-dispuits/cleandata/baltimore-regression.rds")

newobs1 <- tibble(`COVID-19 Cases` = c(0, 1000, 10000, 50000, 100000))

baltimore_posterior <- posterior_epred(fit_1, newdata = newobs1) %>%
  as_tibble() %>%
  mutate(`0` = `1`) %>%
  mutate(`1000` = `2`) %>%
  mutate(`10000` = `3`) %>%
  mutate(`50000` = `4`) %>%
  mutate(`100000` = `5`) %>%
  select(`0`, `1000`, `10000`, `50000`, `100000`) %>%
  pivot_longer(names_to = "COVID-19 Cases",
               values_to = "Effect",
               cols = everything())

saveRDS(baltimore_posterior, "covid-and-domestic-dispuits/cleandata/baltimore-posterior.rds")


# levels of covid cases on y axis and average disputes on x axis 

####################### CINCINNATI MODEL SECTION ############################

data_cincinnati <- cincinnati_merged_data %>%
  inner_join(graph_cincinnati, by = "date")

graph_cincinnati_monthly <- tibble(Month = c("January", "February", "March", "April",
                                             "May", "June", "July", "August", "September",
                                             "October", "November", "December"),
                                   Year = rep("2020", 12),
                                   domestic_disputes = c(5703, 3457, 4818, 3281, 3425, 3394,
                                                         3480, 3265, 3311, 3762, 3788, 3680))

final_cincinnati_data <- covid_data %>%
  filter(State == "OH") %>%
  filter(Year == "2020") %>%
  inner_join(graph_cincinnati_monthly, by = "Month") %>%
  select(State, Month, Year.x, `COVID-19 Cases`, domestic_disputes) %>%
  mutate(Year = Year.x) %>%
  mutate(countyFIPS = "39061") %>%
  select(-(Year.x)) %>%
  inner_join(cincinnati_acs, by = "countyFIPS") %>%
  pivot_wider(names_from = variable,
              values_from = estimate)

fit_2 <- stan_glm(data = final_cincinnati_data,
                  formula = domestic_disputes ~ `COVID-19 Cases`,
                  family = gaussian,
                  seed = 12,
                  refresh = 0)

print(fit_2, digits = 5)

saveRDS(fit_2, "covid-and-domestic-dispuits/cleandata/cincinnati-regression.rds")


newobs2 <- tibble(`COVID-19 Cases` = c(0, 3000, 30000, 70000, 130000))


cincinnati_posterior <- posterior_epred(fit_2, newdata = newobs2) %>%
  as_tibble() %>%
  mutate(`0` = `1`) %>%
  mutate(`3000` = `2`) %>%
  mutate(`30000` = `3`) %>%
  mutate(`70000` = `4`) %>%
  mutate(`130000` = `5`) %>%
  select(`0`, `3000`, `30000`, `70000`, `130000`) %>%
  pivot_longer(names_to = "COVID-19 Cases",
               values_to = "Effect",
               cols = everything())

saveRDS(cincinnati_posterior, "covid-and-domestic-dispuits/cleandata/cincinnati-posterior.rds")


####################### LOS ANGELES MODEL SECTION ############################

data_los_angeles <- los_angeles_merged_data %>%
  inner_join(graph_los_angeles, by = "date")

graph_los_angeles_monthly <- tibble(Month = c("January", "February", "March", "April",
                                              "May", "June", "July", "August", "September",
                                              "October", "November", "December"),
                                    Year = rep("2020", 12),
                                    domestic_disputes = c(5856, 5629, 6234, 6574, 6572, 5385,
                                                          5992, 6179, 5824, 5592, 5590, 10447))

final_los_angeles_data <- covid_data %>%
  filter(State == "CA") %>%
  filter(Year == "2020") %>%
  inner_join(graph_los_angeles_monthly, by = "Month") %>%
  select(State, Month, Year.x, `COVID-19 Cases`, domestic_disputes) %>%
  mutate(Year = Year.x) %>%
  mutate(countyFIPS = "6037") %>%
  select(-(Year.x)) %>%
  inner_join(los_angeles_acs, by = "countyFIPS") %>%
  pivot_wider(names_from = variable,
              values_from = estimate)

fit_3 <- stan_glm(data = final_los_angeles_data,
                  formula = domestic_disputes ~ `COVID-19 Cases`,
                  family = gaussian,
                  seed = 12,
                  refresh = 0)

print(fit_3, digits = 5)

saveRDS(fit_3, "covid-and-domestic-dispuits/cleandata/losangeles-regression.rds")

newobs3 <- tibble(`COVID-19 Cases` = c(5, 9000, 45000, 85000, 175000))


losangeles_posterior <- posterior_epred(fit_3, newdata = newobs3) %>%
  as_tibble() %>%
  mutate(`5` = `1`) %>%
  mutate(`9000` = `2`) %>%
  mutate(`45000` = `3`) %>%
  mutate(`85000` = `4`) %>%
  mutate(`175000` = `5`) %>%
  select(`5`, `9000`, `45000`, `85000`, `175000`) %>%
  pivot_longer(names_to = "COVID-19 Cases",
               values_to = "Effect",
               cols = everything())

saveRDS(losangeles_posterior, "covid-and-domestic-dispuits/cleandata/losangeles-posterior.rds")


####################### ORLANDO MODEL SECTION ############################

data_orlando <- orlando_merged_data %>%
  inner_join(graph_orlando, by = "date")

graph_orlando_monthly <- tibble(Month = c("January", "February", "March", "April",
                                              "May", "June"),
                                    Year = rep("2020", 6),
                                    domestic_disputes = c(2330, 2562, 2923, 3337, 2654, 2508))

final_orlando_data <- covid_data %>%
  filter(State == "FL") %>%
  filter(Year == "2020") %>%
  inner_join(graph_orlando_monthly, by = "Month") %>%
  select(State, Month, Year.x, `COVID-19 Cases`, domestic_disputes) %>%
  mutate(Year = Year.x) %>%
  mutate(countyFIPS = "12095") %>%
  select(-(Year.x)) %>%
  inner_join(orlando_acs, by = "countyFIPS") %>%
  pivot_wider(names_from = variable,
              values_from = estimate)

fit_4 <- stan_glm(data = final_orlando_data,
                  formula = domestic_disputes ~ `COVID-19 Cases`,
                  family = gaussian,
                  seed = 12,
                  refresh = 0)

print(fit_4, digits = 5)

saveRDS(fit_4, "covid-and-domestic-dispuits/cleandata/orlando-regression.rds")

newobs4 <- tibble(`COVID-19 Cases` = c(0, 1000, 25000, 75000, 130000))


orlando_posterior <- posterior_epred(fit_4, newdata = newobs4) %>%
  as_tibble() %>%
  mutate(`0` = `1`) %>%
  mutate(`1000` = `2`) %>%
  mutate(`25000` = `3`) %>%
  mutate(`75000` = `4`) %>%
  mutate(`130000` = `5`) %>%
  select(`0`, `1000`, `25000`, `75000`, `130000`) %>%
  pivot_longer(names_to = "COVID-19 Cases",
               values_to = "Effect",
               cols = everything())

saveRDS(orlando_posterior, "covid-and-domestic-dispuits/cleandata/orlando-posterior.rds")

####################### SEATTLE MODEL SECTION ############################

data_seattle <- seattle_merged_data %>%
  inner_join(graph_seattle, by = "date")

graph_seattle_monthly <- tibble(Month = c("January", "February", "March", "April",
                                          "May", "June", "July", "August", "September",
                                          "October", "November", "December"),
                                Year = rep("2020", 12),
                                domestic_disputes = c(6421, 5798, 6114, 6220, 6220, 2721,
                                                      3160, 3255, 3161, 3158, 2818, 3262))

final_seattle_data <- covid_data %>%
   filter(State == "WA") %>%
   filter(Year == "2020") %>%
   inner_join(graph_seattle_monthly, by = "Month") %>%
   select(State, Month, Year.x, `COVID-19 Cases`, domestic_disputes) %>%
   mutate(Year = Year.x) %>%
   mutate(countyFIPS = "53033") %>%
   select(-(Year.x)) %>%
   inner_join(seattle_acs, by = "countyFIPS") %>%
   pivot_wider(names_from = variable,
              values_from = estimate)

 fit_5 <- stan_glm(data = final_seattle_data,
                   formula = domestic_disputes ~ `COVID-19 Cases`,
                   family = gaussian,
                   seed = 12,
                   refresh = 0)

print(fit_5, digits = 5)

saveRDS(fit_5, "covid-and-domestic-dispuits/cleandata/seattle-regression.rds")

newobs5 <- tibble(`COVID-19 Cases` = c(10, 9000, 45000, 90000, 165000))


seattle_posterior <- posterior_epred(fit_5, newdata = newobs5) %>%
  as_tibble() %>%
  mutate(`10` = `1`) %>%
  mutate(`9000` = `2`) %>%
  mutate(`45000` = `3`) %>%
  mutate(`90000` = `4`) %>%
  mutate(`165000` = `5`) %>%
  select(`10`, `9000`, `45000`, `90000`, `165000`) %>%
  pivot_longer(names_to = "COVID-19 Cases",
               values_to = "Effect",
               cols = everything())


saveRDS(seattle_posterior, "covid-and-domestic-dispuits/cleandata/seattle-posterior.rds")



