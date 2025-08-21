#JORY POTTGEN - SCRIPT 7
#Code Revised - 8/11/2025
#DOES NOT NEED TO BE RUN AFTER SCRIPTS 1-6
#BLACK CARBON EMISSIONS

#------------------------------------------------------------------------------------------------------------------------------------

#Residential Black Carbon Emissions Graph
#CEDS 2021 data, by sector and country
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)

#Black Carbon Data
BC.sector.country <- read.csv("BC_sector_country.csv")

#Region Data
master.country <- read.csv("mastercountry.csv")

master.country %>%
  filter(X1 == 1) %>%
  rename(country = Country_Name) %>%
  select(-X1, -Region) -> MASTERcountry

#Filter for residential
BC.residential <- filter(BC.sector.country, sector == "1A4b_Residential") %>%
  rename(iso = country) %>%
  select(-em, -units)

#Combine black carbon and master country 
BC.master <- BC.residential %>%
  left_join(MASTERcountry, by = c("iso")) %>%
  pivot_longer(cols = matches("^X\\d{4}$"), names_to = "year", values_to = "ktC") %>%
  mutate(year = as.numeric(sub("X", "", year)))

#Combine ktC estimates for each region in each year
regional_ktC <- BC.master %>%
  group_by(Figure_Region, year) %>%
  summarise(total_ktC = sum(ktC, na.rm = TRUE), .groups = "drop") %>%
  rename(Region = Figure_Region)

#Version of plot from 1750 to 2019
BC.plot <- ggplot(regional_ktC, aes(x = year, y = total_ktC, fill = Region)) +
  geom_area(position = "stack") +
  scale_fill_manual(
    values = c(
      "Africa" = "lightcoral",
      "Central and South America" = "seagreen4",
      "China" = "lightblue",
      "Eastern Europe" = "yellow2",
      "FSU" = "orange",
      "Global" = "turquoise",
      "Middle East" = "violet",
      "North America" = "lightgreen",
      "Other Asia" = "royalblue2",
      "South East Asia and Aust/NZ" = "orangered2",
      "Western Europe" = "pink"
    )
  ) +
  xlab("Year") + ylab("Black Carbon Emissions (ktC)") +
  ggtitle("Residential BC Emissions by Region")

BC.plot

ggsave("BlackCarbon.png", plot = BC.plot)

#Version of plot from 1960 to 2019
regional.1960 <- regional_ktC %>%
  filter(between(year, 1960, 2019))

BC.1960.plot <- ggplot(regional.1960, aes(x = year, y = total_ktC, fill = Region)) +
  geom_area(position = "stack") +
  scale_fill_manual(
    values = c(
      "Africa" = "lightcoral",
      "Central and South America" = "seagreen4",
      "China" = "lightblue",
      "Eastern Europe" = "yellow2",
      "FSU" = "orange",
      "Global" = "turquoise",
      "Middle East" = "violet",
      "North America" = "lightgreen",
      "Other Asia" = "royalblue2",
      "South East Asia and Aust/NZ" = "orangered2",
      "Western Europe" = "pink"
    )
  ) +
  xlab("Year") + ylab("Black Carbon Emissions (ktC)") +
  ggtitle("BC Emissions by Region")

BC.1960.plot
