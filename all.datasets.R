#JORY POTTGEN - SCRIPT 1
#Code Revised - 8/11/2025
#INSTALLING, RENAMING, AND CLEANING UP DATASETS

#-------------------------------------------------------------------------------------------------------------------------------------------------
#Install packages
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)

#Read in csv files
data1 <- read.csv("Stonernew.csv")
data1
data2 <- read.csv("CEDS.biomass.csv")
data2
data3 <- read.csv("pop.data.csv")
data3
data4 <- read.csv("stoner.fueltype3.csv")
data4

#Clean up data from Stoner et al 2021 analysis
Stonerdata <- data1 %>%
  rename(country = Location,
         year = Period,
         CentPopPercent = FactValueNumeric,
         iso = SpatialDimValueCode,
         type = Dim1) %>%
  select(iso, country, year, type, CentPopPercent, FactValueNumericLow, 
         FactValueNumericHigh, Value)

#Clean up CEDS biomass data
CEDSdata <- data2 %>%
  rename(ruralpop = pop2) %>%
  select(-Fern_pc) %>%
  select(-ceds_pc_orig) %>%
  select(-units) %>%
  select(-ceds_pc_ext) %>%
  select(-ceds_pc_final) %>%
  select(-src)

#Clean up UN population data
#Calculate Urban and Rural Population
#share data = proportion of population
#pop data = thousands of people
popdata <- data3 %>%
  mutate(urban_pop = pop*urban_share) %>%
  mutate(rural_share = 1-urban_share) %>%
  mutate(rural_pop = pop*(1-urban_share)) %>%
  select(-scenario)

#Clean up Stoner fuel type data
#Convert prop.total into usable decimals
fueltype3 <- data4 %>%
  select(country, year, fuel, prop.total, prop.rural, prop.urban) %>%
  mutate(prop.decimal = prop.total/100)

#Compute cases per 10,000
#Number of people using biomass per 10,000 people
#Not grouped by country, combined global estimate
Stonerdata.10 <- Stonerdata %>%
  mutate(cases = CentPopPercent * 10000) %>%
  group_by(year) %>%
  summarize(total_cases = sum(cases))

#Compute total cases per year
#Number of people using biomass per year
#Not grouped by country, combined global estimate
Stonerdata.year <- Stonerdata %>%
  mutate(cases = CentPopPercent * 10000) %>%
  group_by(year) %>%
  summarize(total_cases = sum(cases))
