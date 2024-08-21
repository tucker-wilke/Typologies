rm(list=ls())
library(here)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(janitor)
library(foreign)
library(car)
library(jtools)
library(RColorBrewer)
library(stargazer)
library(hrbrthemes)

## State Codes List ##
State_codes <- read.csv('/Users/tuckerwilke/Desktop/Fact Base Scripts/State abbreviations.csv')

## Pre Existing State Variables##
State_vars <- read.csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/state_vars.csv") %>%
  distinct(State, .keep_all = TRUE)

##State Populations##
state_pop <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/NST-EST2023-POP.xlsx - Sheet1 (1).csv")

state_pop <- left_join(state_pop, State_codes, by = "State") 
state_pop <-  state_pop %>%
  rename(full = 1) %>%
  rename(State = 3)


##Professionalization Indecies##
Squire <- read_excel("/Users/tuckerwilke/Desktop/Fact Base Scripts/Squire Index.xlsx") %>%
  select(c(1,11,16)) %>%
  rename(State=1)

##Merging in Professionalization#
State_Vars_Expanded <- State_vars %>%
  left_join(Squire, by = "State")


##Climate Opinions Data##
ycom <- read_csv("/Users/tuckerwilke/Desktop/YCOM7_publicdata.xlsx - YCOM_2023.csv") %>%
  filter(geotype =="state") %>%
  rename(full=3) %>%
  rename(Governor_should_do_more =13) %>%
   select(c(3,13)) 

##Mering in ycom##
State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(ycom, by = "full")
  

#State Gov Payroll and Employment Data##
state_gov_payroll <- read.csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/2023_state_gov_payroll.xlsx - state_2023.csv")
  
state_totals <- state_gov_payroll %>%
  filter(Government.Function == "Total - All Government Employment Functions") %>%
  select(State,Total.Full.Time.and.Part.Time.Employment,Total.Payroll..whole.dollars.)


state_totals <- full_join(state_totals, state_pop, by = "State") %>%
  rename(Total_Employment = 2) %>%
  rename(Total_Payroll = 3)

state_totals <- state_totals %>%
  filter(!is.na(Total_Employment), !is.na(Population))

state_totals$Total_Employment <- as.numeric(gsub(",", "", state_totals$Total_Employment))
state_totals$Total_Payroll <- as.numeric(gsub(",", "", state_totals$Total_Payroll))

state_totals <- state_totals %>%
  mutate(
    Total_Employment = as.numeric(Total_Employment),
    Population = as.numeric(Population)
  )

state_totals <- state_totals %>%
  mutate(State_Gov_Employment_Per_10k = Total_Employment / (Population / 10000)) %>%
  mutate(State_Gov_Payroll_Per_Cap = Total_Payroll / Population) %>%
  select(1,4,6,7)

## Merging in Payroll Data##
State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(state_totals, by = c("State", "full"))


## Gini Coefficient##
Gini_Coefficeint_Raw <- read.csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/ACSDT1Y2022.B19083-2024-06-21T205604.csv") %>%
  rename(full = 1) %>%
  rename(Gini = 2)

states <- c()
estimates <- c()

for (i in seq(1, nrow(Gini_Coefficeint_Raw), by = 2)) {
  state <- Gini_Coefficeint_Raw$full[i]
  estimate <- Gini_Coefficeint_Raw$Gini[i + 1]
  states <- c(states, state)
  estimates <- c(estimates, estimate)
}

Gini_Coefficeint <- data.frame(State = states, Estimate = estimates) %>%
  rename(full = 1) %>%
  rename(Gini = 2)

## Merging in Gini##
State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(Gini_Coefficeint, by = "full")

## Energy Employment Data ##
USEER <- read.csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/USEER Public Data.xlsx - state data.csv")

USEER_Totals <- USEER %>%
  select(c(1,2,3,4,17,51)) %>%
  rename(full=2) %>%
  rename(State = 1) %>%
  rename(Total_Employees=3) %>%
  rename(Total_Energy_Employees=4) %>%
  rename(Total_TD_Employees=5)%>%
  rename(Total_Clean_Without_Transmission=6)%>%
  mutate(Energy_Share = Total_Energy_Employees/Total_Employees) %>%
  mutate(Total_FF_Jobs = Total_Energy_Employees -Total_Clean_Without_Transmission -
           Total_TD_Employees) %>%
  mutate(FF_Job_Share_Total = Total_FF_Jobs/Total_Employees) %>%
  mutate(Clean_Job_Share_Total=Total_Clean_Without_Transmission/Total_Employees) 

USEER_Selected <- USEER_Totals %>%
  select(1,2,7,9,10)





##Mering in Employment Data##
State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(USEER_Selected, by = c("State", "full"))


##Campaign Finance Donations Import##
US_Reps_FF_Donations <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/FF Donations/Money from Oil & Gas to US Representatives, 2023-2024.csv")
US_Reps_Candiates_FF_Donations <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/FF Donations/Money from Oil & Gas to US House candidates, 2023-2024.csv")
US_Senators_FF_Donations <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/FF Donations/Money from Oil & Gas to US Senators, 2023-2024.csv")
US_Senators_Candiates_FF_Donations <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/FF Donations/Money from Oil & Gas to US Senate candidates, 2023-2024.csv")

# Cleaning the Amount Column##
US_Reps_Candiates_FF_Donations$Amount <- gsub("\\$", "", US_Reps_Candiates_FF_Donations$Amount)
US_Reps_Candiates_FF_Donations$Amount <- as.numeric(US_Reps_Candiates_FF_Donations$Amount) 
US_Reps_FF_Donations$Amount <- gsub("\\$", "", US_Reps_FF_Donations$Amount)
US_Reps_FF_Donations$Amount <- as.numeric(US_Reps_FF_Donations$Amount)
US_Senators_FF_Donations$Amount <- gsub("\\$", "", US_Senators_FF_Donations$Amount)
US_Senators_FF_Donations$Amount <- as.numeric(US_Senators_FF_Donations$Amount)
US_Senators_Candiates_FF_Donations$Amount <- gsub("\\$", "", US_Senators_Candiates_FF_Donations$Amount)
US_Senators_Candiates_FF_Donations$Amount <- as.numeric(US_Senators_Candiates_FF_Donations$Amount)

##Cleaning Campaign Finance##
US_Reps_Total <- full_join(US_Reps_Candiates_FF_Donations, US_Reps_FF_Donations, by = c("Representative", "State",
                                                                                        "Amount")) %>%
  rename(Recipient = 1)
US_Senators_Total <- full_join(US_Senators_Candiates_FF_Donations, US_Senators_FF_Donations,
                               by = c("Senator", "State", "Amount")) %>%
rename(Recipient = 1)

US_Candidates_FF_Total <- full_join(US_Reps_Total, US_Senators_Total,
                                     by = c("Recipient", "State", "Amount"))

State_contributions <- US_Candidates_FF_Total %>%
  group_by(State) %>%
  summarize(Total_Contributions = sum(Amount, na.rm = TRUE)) %>%
  rename(full = 1)

State_contributions <- left_join(State_contributions, state_pop, by = "full") 
State_contributions <- State_contributions %>%
  filter(!is.na(State_contributions$Population)) %>%
  mutate(Contributions_Per_10k = Total_Contributions/(Population / 10000)) %>%
  select(c(4,1,2,5))

## Merging in Campaign Finance ##
State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(State_contributions, by = c("State", "full"))


##Tax Revenues 2023 ($ Amounts in Thousands) ##
State_Tax_Revenues_2023 <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/FY2023-STC-Detailed-Table-Transposed.xlsx - State Tax Collection 2023.csv") %>%
  rename(Tax_Type =1) %>%
  filter(Tax_Type %in% c("Total Taxes","Motor fuels", "Severance")) %>%
  select(-Item) %>%
  select(-"United States")

convert_to_numeric <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", x))
}

# Convert all columns except Tax_Type to numeric
State_Tax_Revenues_2023 <- State_Tax_Revenues_2023 %>%
  mutate(across(-Tax_Type, ~ convert_to_numeric(.)))

State_Tax_Revenues_2023_Long <- State_Tax_Revenues_2023 %>%
  pivot_longer(
    cols = -Tax_Type,
    names_to = "State",
    values_to = "Tax"
  )

# Pivot wider to create Total Tax and Severance columns
State_Tax_Revenue_Final <- State_Tax_Revenues_2023_Long %>%
  pivot_wider(
    id_cols = State,                    # Column(s) to keep as identifier(s)
    names_from = Tax_Type,              # Values in Tax_Type to spread into new columns
    values_from = Tax                   # Values in Tax to fill the new columns
  )

State_Tax_Revenue_Final[is.na(State_Tax_Revenue_Final)] <- 0
State_Tax_Revenue_Final <- State_Tax_Revenue_Final %>%
  rename(Total_Taxes = 2) %>%
  rename(Motor_Fuels = 3)
State_Tax_Revenue_Final <- State_Tax_Revenue_Final %>%
   mutate(FF_Total = Motor_Fuels+Severance)

State_Tax_Revenue_Final <- State_Tax_Revenue_Final %>%
  mutate(FF_Percent_Total = 100*(FF_Total / Total_Taxes)) %>%
  rename(full= 1) 

FF_State_Tax <- State_Tax_Revenue_Final %>%
  select(1,6)

## Merging in Tax##
State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(FF_State_Tax, by = "full")



##Unionization Rates##
Unionization_Rates <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/Unionization Rates - Sheet1.csv") %>%
  rename(full = 1) %>%
  rename(unionzation=3) %>%
  select(1,3)
median(Unionization_Rates$unionzation)

##Merging in Unions##
State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(Unionization_Rates, by = "full")



##Energy Production and Consumption##
Energy_Production_Trillion <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/EIA Energy Data - Production Trillion 2021 BTU.csv")
Energy_Consumption_Billion <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/EIA Energy Data - Consumption Billion BTU 2021 .csv")

Energy_Consumption_Billion <- Energy_Consumption_Billion %>%
  rename(Natural_Gas = 3) %>%
  mutate(Total = Coal + Natural_Gas + Petroleum + Nuclear + Renewable) %>%
  mutate(Total_FF_Consumpion = Coal + Natural_Gas + Petroleum)%>%
  rename(Abbreviation = 1) %>%
  full_join(State_codes, by = "Abbreviation")

Energy_Consumption_Totals <- Energy_Consumption_Billion %>%
  mutate(Total_Consumption = Total/1000) %>%
  mutate(Total_FF_Consumption = Total_FF_Consumpion/1000) %>%
  select(1,9,10,11) %>%
  rename(full=2)

Energy_Production_Trillion <- Energy_Production_Trillion %>%
  rename(Natural_Gas = 3) %>%
  rename(Oil = 4)

Energy_Production_Trillion <- Energy_Production_Trillion %>%
  mutate(
    Coal = as.numeric(Coal),
    Natural_Gas = as.numeric(Natural_Gas),
    Oil = as.numeric(Oil)
  )
 
Energy_Production_Trillion[is.na(Energy_Production_Trillion)] <- 0

Energy_Production_Trillion <- Energy_Production_Trillion %>%
 mutate(Total_FF_Production = Coal + Natural_Gas + Oil) %>%
  select(1,9,10)

Energy_Net_Totals <- Energy_Production_Trillion %>%
  full_join(Energy_Consumption_Totals, by="full") %>%
  drop_na(Abbreviation) %>%
  rename(Total_Production = 2) %>%
  rename(State = 4) %>%
  mutate(Net_FF = Total_FF_Production - Total_FF_Consumption) %>%
  mutate(Net_Energy = Total_Production - Total_Consumption) %>%
  select(1,4,7,8) 

State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(Energy_Net_Totals, by = c("full", "State"))

##Employment by Sector##
Employment_By_Sector <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/Employment Sector.csv")%>%
  filter(!is.na(Jobs))

Employment_By_Sector <- Employment_By_Sector %>%
  mutate(Jobs = as.numeric(Jobs)) %>%
  mutate(Jobs = replace_na(Jobs, 0))

Employment_By_Sector <- Employment_By_Sector %>% 
  pivot_wider(
    names_from = Industry,
    values_from = Jobs
  ) %>%
  rename(State = 1)

Employment_By_Sector <- Employment_By_Sector %>%
  inner_join(State_codes, by = "State")

Employment_By_Sector <- Employment_By_Sector %>%
  mutate(Primary_Sector = rowSums(select(., 3:5),)) %>%
  mutate(Secondary_Sector= rowSums(select(., 6:8),)) %>%
  mutate(Tertiary_Sector = rowSums(select(., 9:23),)) %>%
  select(c(1,24,2,25,26,27)) %>%
  rename(Total_Jobs = 3)
  
Employmnet_Sector_Share <- Employment_By_Sector %>%
  rename(full = 1) %>%
  rename(State=2) %>%
  mutate(Primary_Share = Primary_Sector/Total_Jobs) %>%
  mutate(Secondary_Share = Secondary_Sector/Total_Jobs) %>%
  mutate(Tertiary_Share = Tertiary_Sector/Total_Jobs) %>%
  select(c(1,2,7,8,9))

State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(Employmnet_Sector_Share, by = c("full", "State"))


##Unemployment Data##
Unemployment <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/State Unemployment - Sheet1.csv") %>%
  rename(full=1)

State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(Unemployment, by = "full")

median(Unemployment$`Unemployment (2024)`)

##Income Per Capita##
Income_Per_Capita <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/Income Per Capita BEA.xlsx - Table.csv") %>%
  rename(full =1)

##Merging in Income Per Capita##
State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(Income_Per_Capita, by = "full")


##GDP##
GDP <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/GDP_Milions.csv") %>%
  rename(full =1 ) %>%
  rename(GDP_millions=2)

##Merging in Income##
State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(GDP, by = "full")

##Manufacturing##

Manufacturing_GDP <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/Manufacturing Share of Output - Sheet1.csv") %>%
  rename(Manufacturing_GDP=2)

State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(Manufacturing_GDP, by = "full")

##Typologies##
Typologies_Brainstorming <- read_csv("/Users/tuckerwilke/Desktop/Fact Base Scripts/Categories Brainstorming - Sheet1.csv")

State_Vars_Expanded <- State_Vars_Expanded %>%
  left_join(Typologies_Brainstorming, by = c("State", "full")) 


