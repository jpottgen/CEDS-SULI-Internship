#JORY POTTGEN - SCRIPT 5
#Code Revised - 8/11/2025
#ADDING COUNTRIES S THROUGH Z FROM STONER ET AL (2021)

#-------------------------------------------------------------------------------------------------------------
#Install packages
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)

#RUN AFTER SCRIPTS 1, 2, 3, & 4

S.to.Z <- read.csv("S.to.Z.countries.csv")

#Charcoal data between 1990 and 2022 for countries S through Z
SZ.char <- S.to.Z %>%
  filter(Dim2 == "Charcoal") %>%
  filter(IndicatorCode == "PHE_HHAIR_PROP_POP_CATEGORY_FUELS") %>%
  rename(perc_char = FactValueNumeric,
         country = Location,
         iso = ParentLocationCode,
         year = Period) %>%
  select(iso, country, year, Dim1, perc_char)

#Biomass data between 1990 and 2022 for countries S through Z
SZ.bio <- S.to.Z %>%
  filter(Dim2 == "Biomass") %>%
  filter(IndicatorCode == "PHE_HHAIR_PROP_POP_CATEGORY_FUELS") %>%
  rename(perc_bio = FactValueNumeric,
         country = Location,
         iso = ParentLocationCode,
         year = Period) %>%
  select(iso, country, year, Dim1, perc_bio)

#Combine biomass and charcoal into one data set
SZ.char.bio <- SZ.char %>%
  right_join(SZ.bio, by = c("year","country","Dim1", "iso")) %>%
  mutate(perc_solid = perc_char + perc_bio) %>%
  mutate(prop_solid = perc_solid/100)

#TOTAL
SZ_solid1 <- SZ.char.bio %>%
  pivot_wider(names_from = "Dim1", values_from = "prop_solid") %>%
  select(-Urban, -Rural) %>%
  filter(!(is.na(Total)))

#URBAN
SZ_solid2 <- SZ.char.bio %>%
  pivot_wider(names_from = "Dim1", values_from = "prop_solid") %>%
  select(-Total, -Rural) %>%
  filter(!(is.na(Urban)))

#RURAL
SZ_solid3 <- SZ.char.bio %>%
  pivot_wider(names_from = "Dim1", values_from = "prop_solid") %>%
  select(-Urban, -Total) %>%
  filter(!(is.na(Rural)))

#COMBINE
SZ.SOLID.props <- SZ_solid1 %>%
  left_join(SZ_solid2, by = c("country","year")) %>%
  left_join(SZ_solid3, by = c("country","year")) %>%
  select(country, year, Total, Urban, Rural)

#Calculating the number of people using solid biomass (biomass + charcoal)
SZ.ppl.solid <- popdata %>%
  left_join(SZ.SOLID.props, by = c("year","country")) %>%
  mutate(SZ.ppl.using.solid = if_else(is.na(Total), pop, pop * Total)) %>%
  filter(!(is.na(Total))) %>%
  rename(SZ.PROP.RUR = Rural) %>%
  rename(SZ.PROP.URB = Urban)

SZ.solid.bio.fract <- SZ.ppl.solid %>%
  mutate(SZ.rur.solid.users = SZ.PROP.RUR * rural_pop) %>%
  mutate(SZ.urb.solid.users = SZ.PROP.URB * urban_pop) %>%
  mutate(SZ.tot.solid.users = SZ.rur.solid.users + SZ.urb.solid.users) %>%
  mutate(SZ.rural.fraction = SZ.rur.solid.users/SZ.tot.solid.users) %>%
  mutate(SZ.urban.fraction = SZ.urb.solid.users/SZ.tot.solid.users) %>%
  select(iso, country, year, pop, SZ.rur.solid.users, SZ.urb.solid.users, 
         SZ.tot.solid.users, SZ.rural.fraction, SZ.urban.fraction) %>%
  filter(!(is.na(SZ.rural.fraction)))

#Sample South Sudan Graph
south.sudan <- SZ.solid.bio.fract %>%
  filter(country == "South Sudan") %>%
  pivot_longer(cols = c(SZ.rural.fraction, SZ.urban.fraction), names_to = "fraction", values_to = "value")

SSudan.plot <- ggplot(south.sudan, aes(x = year, y = value, color = fraction)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Fraction of people using biomass") +
  ggtitle("Rural vs. Urban Biomass Use in South Sudan")
SSudan.plot

#NOTE----------------------------
#For countries with the beginning letter A-R = use "solid.bio.fract"
#For countries with the beginning letter S-Z use "SZ.solid.bio.fract"

