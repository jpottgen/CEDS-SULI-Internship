#JORY POTTGEN - SCRIPT 3
#Code Revised - 8/11/2025
#BIOMASS DATA
#DOES NOT INCLUDE CHARCOAL
#FOR TOTAL SOLID BIOFUEL (CHARCOAL + BIOMASS) --> SEE SCRIPT 4

#------------------------------------------------------------------------------------------------------------------------------------------------
#Install packages
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)

#RUN AFTER SCRIPT 1 & 2

#Matching columns "year" and "iso" between popdata & CEDSdata
#Creating new column of Total Biomass/Rural Pop
#Creating new column of Total Biomass/Urban Pop
popdata <- data3 %>%
  mutate(urban_pop = pop*urban_share) %>%
  mutate(rural_pop = pop*(1-urban_share)) %>%
  select(-scenario)

popdata %>%
  left_join(CEDSdata, by = c("year","iso")) %>%
  mutate(value = if_else(is.na(rural_pop), ceds_tot_final, ceds_tot_final / rural_pop)) %>%
  mutate(value = if_else(is.na(urban_pop), ceds_tot_final, ceds_tot_final / urban_pop))

#Population using biofuels
Stonerdata %>%
  left_join(popdata, by = c("year","country")) %>%
  mutate(value = if_else(is.na(CentPopPercent), pop, pop * CentPopPercent))

#Equations
#value = ceds_tot_final / urban_pop
#Per_cap_urban_biomass = ceds_tot_final / urban_pop

#Filtering Fuel Type Data for only Biomass
bio <- filter(fueltype3, fuel == "Biomass")

#Multiply proportion of pop using biomass by the total population
#result = the total number of people using biomass as fuel
#biomass.population is the population of people using traditional biomass
#prop.decimal is Prop of pop with primary reliance on fuels and technologies for cooking, by fuel type (%) from Stoner et al
#pop is in 1000s of people from CEDS population data
trad.bio.pop <- popdata %>%
  left_join(bio, by = c("year","country")) %>%
  mutate(biomass.population = if_else(is.na(prop.decimal), pop, pop * prop.decimal))

#Removing NA 
#Keeping only data that has a value for prop.decimal
noNA.poptotals <- trad.bio.pop %>% filter(!(is.na(prop.decimal)))

#Line Graph of all countries across time
#Population of people using traditional biomass between 1990 and 2022 for every country

bio.pop.plot <- ggplot(noNA.poptotals, aes(x = year, y = biomass.population, color = country)) + 
  geom_line(show.legend = FALSE)

bio.pop.plot

#use ylim to limit the upper bound to a population of 500
bio.pop.plot500 <- ggplot(noNA.poptotals, aes(x = year, y = biomass.population, color = country)) + 
  geom_line(show.legend = FALSE) + 
  ylim(0,500)

bio.pop.plot500

#------------------------------------------------------------------------------------------------------------------------------------------------------
#Identifying charcoal data
#add the charcoal fraction (charcoal/(charcoal + all other biomass)

charcoal <- filter(fueltype3, fuel == "Charcoal")

#pop is in thousands of people
#charcoal.use is in thousands of people using charcoal
ppl.using.coal <- popdata %>%
  left_join(charcoal, by = c("year","country")) %>%
  mutate(charcoal.use = if_else(is.na(prop.decimal), pop, pop * prop.decimal))

#Removing NA 
#Keeping only data that has a value for prop.decimal
coal.noNA <- ppl.using.coal %>% filter(!(is.na(prop.decimal)))

#filter
coal.and.bio <- filter(fueltype3, fuel %in% c("Biomass", "Charcoal"))

combined.coal.bio <- coal.and.bio %>%
  group_by(country, year) %>% 
  mutate(combined.prop = sum(prop.decimal)) %>% ungroup() %>% 
  select(country, year, combined.prop) %>% distinct()

coal.bio.total.ppl <- combined.coal.bio %>%
  left_join(noNA.bio.per.person, by = c("year","country")) %>%
  mutate(ppl.using.combined = if_else(is.na(combined.prop), pop, pop * combined.prop)) %>%
  select(country, year, combined.prop, ppl.using.combined) %>%
  distinct()

#CHARCOAL FRACTION RESULTS
charcoal.fraction <- coal.bio.total.ppl %>%
  left_join(coal.noNA, by = c("year","country")) %>%
  mutate(coal.frac = if_else(is.na(ppl.using.combined), charcoal.use, charcoal.use / ppl.using.combined)) %>%
  filter(!(is.na(ppl.using.combined))) %>%
  filter(!(is.na(coal.frac)))

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Fraction of people using biomass
#Highlighting Urban vs. Rural
#ONLY FILTERED FOR BIOMASS

filtered.bio <- bio %>%
  mutate(PROP.RUR = prop.rural/100) %>%
  mutate(PROP.URB = prop.urban/100)

bio.fract <- filtered.bio %>%
  left_join(popdata, by = c("year","country")) %>%
  mutate(rural.bio.users = PROP.RUR * rural_pop) %>%
  mutate(urban.bio.users = PROP.URB * urban_pop) %>%
  mutate(tot.bio.users = rural.bio.users + urban.bio.users) %>%
  mutate(rural.fraction = rural.bio.users/tot.bio.users) %>%
  mutate(urban.fraction = urban.bio.users/tot.bio.users)