#JORY POTTGEN - SCRIPT 4
#Code Revised - 8/11/2025
#ADDING CHARCOAL TO SOLID BIOFUEL ESTIMATES (CHARCOAL + BIOMASS)

#-------------------------------------------------------------------------------------------------------------
#Install packages
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)

#RUN AFTER SCRIPTS 1, 2, & 3

# Matching columns "year" and "iso" between popdata & CEDSdata
# Creating new column of Total Solid Biomass/Rural Pop
popdata %>%
  left_join(CEDSdata, by = c("year","iso")) %>%
  mutate(value = if_else(is.na(rural_pop), ceds_tot_final, ceds_tot_final / rural_pop))

#Creating new column of Total Solid Biomass/Urban Pop
popdata %>%
  left_join(CEDSdata, by = c("year","iso")) %>%
  mutate(value = if_else(is.na(urban_pop), ceds_tot_final, ceds_tot_final / urban_pop))

Stonerdata %>%
  left_join(popdata, by = c("year","country")) %>%
  mutate(value = if_else(is.na(CentPopPercent), pop, pop * CentPopPercent))

#Equations
#value = ceds_tot_final / urban_pop
#Per_cap_urban_biomass = ceds_tot_final / urban_pop

#Filtering Fuel Type Data for only Biomass and Charcoal (Solid Biomass)
#KEEP PROP DECIMAL
stoner.bio.char1 <- filter(fueltype3, fuel == "Biomass"|fuel == "Charcoal") %>%
  select(-prop.total, -prop.rural, -prop.urban)

#KEEP PROP RURAL
stoner.bio.char2 <- filter(fueltype3, fuel == "Biomass"|fuel == "Charcoal") %>%
  select(-prop.total, -prop.decimal, -prop.urban)

#KEEP PROP URBAN
stoner.bio.char3 <- filter(fueltype3, fuel == "Biomass"|fuel == "Charcoal") %>%
  select(-prop.total, -prop.decimal, -prop.rural)

#TOTAL COMBINED PROPORTION OF BIOMASS AND CHARCOAL
#PROP DECIMAL
solid_wide1 <- stoner.bio.char1 %>%
  pivot_wider(names_from = "fuel", values_from = "prop.decimal") %>%
  mutate(solid.prop.dec = Biomass + Charcoal) %>%
  select(-Biomass, -Charcoal)

#PROP RURAL
solid_wide2 <- stoner.bio.char2 %>%
  pivot_wider(names_from = "fuel", values_from = "prop.rural") %>%
  mutate(solid.prop.rural = Biomass + Charcoal) %>%
  select(-Biomass, -Charcoal)

#PROP URBAN
solid_wide3 <- stoner.bio.char3 %>%
  pivot_wider(names_from = "fuel", values_from = "prop.urban") %>%
  mutate(solid.prop.urban = Biomass + Charcoal) %>%
  select(-Biomass, -Charcoal)

#COMBINE ALL PROPORTIONS
SOLID.props <- solid_wide1 %>%
  left_join(solid_wide2, by = c("country","year")) %>%
  left_join(solid_wide3, by = c("country","year"))

#START ----------------------------------------------------------------------------------------------------------

#Calculating the number of people using solid biomass (biomass + charcoal)
ppl.using.solid <- popdata %>%
  left_join(SOLID.props, by = c("year","country")) %>%
  mutate(ppl.using.solid = if_else(is.na(solid.prop.dec), pop, pop * solid.prop.dec)) %>%
  filter(!(is.na(solid.prop.dec)))

#Results of last step:
#solid.prop.dec is Prop of pop with primary reliance on fuels and technologies for 
#cooking, by fuel type (%) from Stoner et al
#pop is in 1000s of people from CEDS population data

#Identifying charcoal data
#add the charcoal fraction (charcoal/(charcoal + all other biomass)
#from Stoner to the data and then use that for the color scale

charcoal <- filter(fueltype3, fuel == "Charcoal")

#pop is in thousands of people
#so charcoal.use is in thousands of people using charcoal
ppl.using.coal <- popdata %>%
  left_join(charcoal, by = c("year","country")) %>%
  mutate(charcoal.use = if_else(is.na(prop.decimal), pop, pop * prop.decimal)) %>%
  filter(!(is.na(prop.decimal)))

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

#------------------------------------------------------------------------------------------------------------------------
#Fraction of people using biomass
#Highlighting Urban vs. Rural

SOLID.BIOFUEL <- ppl.using.solid %>%
  mutate(PROP.RUR = solid.prop.rural/100) %>%
  mutate(PROP.URB = solid.prop.urban/100)

solid.bio.fract <- SOLID.BIOFUEL %>%
  mutate(rural.solid.users = PROP.RUR * rural_pop) %>%
  mutate(urban.solid.users = PROP.URB * urban_pop) %>%
  mutate(tot.solid.users = rural.solid.users + urban.solid.users) %>%
  mutate(rural.fraction = rural.solid.users/tot.solid.users) %>%
  mutate(urban.fraction = urban.solid.users/tot.solid.users) %>%
  select(iso, country, year, pop, rural.solid.users, urban.solid.users, 
         tot.solid.users, rural.fraction, urban.fraction)

#Sample Bangladesh Graph
bangl <- solid.bio.fract %>%
  filter(country == "Bangladesh") %>%
  pivot_longer(cols = c(rural.fraction, urban.fraction), names_to = "fraction", values_to = "value")

Bangladesh.plot <- ggplot(bangl, aes(x = year, y = value, color = fraction)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Fraction of people using biomass") +
  ggtitle("Rural vs. Urban Biomass Use in Bangladesh")
Bangladesh.plot

#Filtering for countries with inconsistent data between lit and calc
inconsis <- c("Bangladesh","Eritrea","Fiji","Ghana","Papua New Guinea","South Sudan")
incon.count <- solid.bio.fract %>%
  filter(country %in% unique(inconsis)) %>%
  pivot_longer(cols = c(rural.fraction, urban.fraction), names_to = "fraction", values_to = "value")

inconsis.plot <- ggplot(incon.count, aes(x = year, y = value, color = fraction)) + geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(vars(country)) +
  xlab("Year") + ylab("Fraction of people using biomass") +
  ggtitle("Rural vs. Urban Biomass Use")
inconsis.plot

#NOTE - this graph is missing South Sudan
#see biomass.charcoal.R script for S-Z country data (script 5)