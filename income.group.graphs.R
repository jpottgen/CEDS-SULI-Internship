#JORY POTTGEN - SCRIPT 2.5
#Code Revised - 8/11/2025
#INCOME GROUP GRAPHS
#SAME CODE AS SCRIPT 2
#STREAMLINED VERSION

#----------------------------------------------------------------------------------------------------------------------------
#Install packages
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)

#CALCULATIONS
#GOAL: estimate the amount of biomass used per person that uses biomass
#Residential biomass consumption / number of people using biomass
#Number of people using biomass = total population * fraction of population using biomass

#Open csv files
#Stonernew.csv comes from Stoner et al 2021 data
#When you export the csv straight from the article it shows up as "data(1)"
#Rename file to Stonernew.csv
stoner.et.al <- read.csv("Stonernew.csv")
popdata <- read.csv("A.UN_pop_master.csv")
CEDSdata <- read.csv("CEDS.biomass.csv")

#Rename variables
stonerdata <- stoner.et.al %>%
  rename(country = Location,
         year = Period,
         CentPopPercent = FactValueNumeric)

#Proportion of population using biomass (non-clean sources)
stoner.nonclean <- stonerdata %>%
  mutate(nonclean.prop = (100 - CentPopPercent)/100)

#Number of people using traditional biomass
total.pop.using.bio <- stoner.nonclean %>%
  left_join(popdata, by = c("year","country")) %>%
  mutate(ppl.biomass = if_else(is.na(nonclean.prop), pop, pop * nonclean.prop))

#Biomass per person (t/person)
biomass.per.cap <- CEDSdata %>%
  left_join(total.pop.using.bio, by = c("year","iso")) %>%
  mutate(bio.per.person = if_else(is.na(ceds_tot_final), ppl.biomass, ceds_tot_final / ppl.biomass))

#Removing NA 
#Keeping only data that has a value for bio.per.person
noNA.bio.per.person <- biomass.per.cap %>% filter(!(is.na(bio.per.person)))

#Filter out rural and urban categories, only keep totals
TOTALS.bpp <- filter(noNA.bio.per.person, Dim1 == "Total")

#Take out infinity ("inf" values)
no.infinity.bpp <- TOTALS.bpp %>% filter(!(is.infinite(bio.per.person)))

#REFERENCE WORLD BANK INFORMATION
#Grouping countries into lower, lower-middle, upper-middle, and high-income groups
income.groups <- read.csv("WB_income_groups.csv")
income.levels <- income.groups %>%
  rename(country = country_name)

#Filtering for different income levels
L.income2022 <- filter(income.levels, X2022 == "L")
LM.income2022 <- filter(income.levels, X2022 == "LM")
UM.income2022 <- filter(income.levels, X2022 == "UM")
H.income2022 <- filter(income.levels, X2022 == "H")

#Creating a vector with the names of all low-income countries
Lcountry <- c("Afghanistan","Burkina Faso", "Burundi","Central African Republic","Chad",
              "Congo Democratic Republic","Eritrea","Ethiopia","Gambia","Guinea-Bissau",
              "Korea Democratic Republic","Liberia","Madagascar","Malawi","Mali","Mozambique",
              "Niger","Rwanda","Sierra Leone","Somalia","South Sudan","Sudan","Syrian Arab Republic",
              "Togo","Uganda","Yemen")

#Filter biomass per person data for only low-income countries
no.infinity.bpp %>% 
  filter(country %in% unique(Lcountry))-> Lowfilter

library(ggplot2)

#Create ggplot showing biomass per person for all low-income countries
low.plot <- ggplot(Lowfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Low Income Country")
low.plot

#Save graph as an image
ggsave("low_income_bpp.png", plot = low.plot)

#Creating a vector with the names of all lower-middle income countries
LMcountry <- c("Algeria","Angola","Bangladesh","Benin","Bhutan","Bolivia","Cabo Verde","Cambodia",
               "Cameroon","Comoros","Congo Republic","Coted Ivoire","Djibouti","Egypt","Eswatini",
               "Ghana","Guinea","Haiti","Honduras","India","Iran","Jordan","Kenya","Kiribati",
               "Kyrgyz Republic","Lao PDR","Lebanon","Lesotho","Mauritania","Micronesia","Mongolia",
               "Morocco","Myanmar","Nepal","Nicaragua","Nigeria","Pakistan","Papua New Guinea",
               "Philippines","Samoa","Sao Tome and Principe","Senegal","Solomon Islands","Sri Lanka",
               "Tajikistan","Tanzania","Timor-Leste","Tunisia","Ukraine","Uzbekistan","Vanuatu","Vietnam",
               "Zambia","Zimbabwe")

#Filter biomass per person data for only lower-middle income countries
no.infinity.bpp %>% 
  filter(country %in% unique(LMcountry))-> LMfilter

#Bounded Lower-Middle income countries
lowmiddle.bounded.plot <- ggplot(LMfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  ylim(0,2) +
  geom_smooth(method = loess, formula = y ~ x) + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Lower-Middle Income Country (Bounded)")
lowmiddle.bounded.plot

#Save graph as an image
ggsave("BOUNDED_LM_income_bpp.png", plot = lowmiddle.bounded.plot)

#Creating a vector with the names of all upper-middle income countries
UMcountry <- c("Albania","Argentina","Armenia","Azerbaijan","Belarus","Belize","Bosnia and Herzegovina","Botswana",
               "Brazil","Bulgaria","China","Colombia","Costa Rica","Cuba","Dominica","Dominican Republic","Ecuador",
               "El Salvador","Equatorial Guinea","Fiji","Gabon","Georgia","Grenada","Guatemala","Indonesia",
               "Iraq","Jamaica","Kazakhstan","Kosovo","Libya","Malaysia","Maldives","Marshall Islands","Mauritius",
               "Mexico","Moldova","Montenegro","Namibia","North Macedonia","Palau","Paraguay","Peru","Russian Federation",
               "Serbia","South Africa","St. Lucia","St. Vincent and the Grenadines","Suriname","Thailand","Tonga",
               "Turkiye","Turkmenistan","Tuvalu","West Bank and Gaza")

#Filter biomass per person data for only upper-middle income countries
no.infinity.bpp %>% 
  filter(country %in% unique(UMcountry))-> UMfilter

#Bounded Upper Middle income countries
uppermiddle.bounded.plot <- ggplot(UMfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  ylim(0,2) +
  geom_smooth(method = loess, formula = y ~ x) + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Upper-Middle Income Country (Bounded)")
uppermiddle.bounded.plot

#Save graph as an image
ggsave("BOUNDED_UM_income_bpp.png", plot = uppermiddle.bounded.plot)

#ANALYSING INCOME GRAPHS W/O THE CONSISTENT COUNTRIES
#Remaining low income countries
L.remain <- c("Burkina Faso", "Burundi","Central African Republic","Chad",
              "Congo Democratic Republic","Gambia","Guinea-Bissau",
              "Korea Democratic Republic","Liberia","Malawi","Mali","Mozambique",
              "Rwanda","Sierra Leone","Somalia","Sudan","Syrian Arab Republic")

no.infinity.bpp %>% 
  filter(country %in% unique(L.remain))-> L.rem.filter

L.remain.plot <- ggplot(L.rem.filter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Low Income Country")
L.remain.plot

#Remaining lower middle income countries
LM.remain <- c("Algeria","Angola","Bhutan","Bolivia","Cabo Verde",
               "Cameroon","Comoros","Congo Republic","Coted Ivoire","Djibouti",
               "Egypt","Eswatini","Guinea","Honduras","India","Iran","Jordan",
               "Kenya","Kyrgyz Republic","Lao PDR","Lesotho","Mauritania",
               "Micronesia","Morocco","Myanmar","Nepal","Nicaragua","Pakistan",
               "Sao Tome and Principe","Sri Lanka","Tajikistan","Tanzania",
               "Tunisia","Ukraine","Uzbekistan","Vietnam","Zambia")

no.infinity.bpp %>% 
  filter(country %in% unique(LM.remain))-> LM.rem.filter

LM.remain.plot <- ggplot(LM.rem.filter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,1) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Lower Middle Income Country")
LM.remain.plot

#Remaining upper middle income countries
UM.remain <- c("Albania","Argentina","Armenia","Azerbaijan","Belarus","Bosnia and Herzegovina","Botswana",
               "Brazil","Bulgaria","Colombia","Costa Rica","Dominican Republic","Ecuador",
               "El Salvador","Equatorial Guinea","Gabon","Georgia","Grenada","Guatemala",
               "Iraq","Jamaica","Kazakhstan","Kosovo","Malaysia","Maldives","Marshall Islands","Mauritius",
               "Mexico","Moldova","Montenegro","Namibia","North Macedonia","Palau","Paraguay","Peru","Russian Federation",
               "Serbia","South Africa","St. Lucia","St. Vincent and the Grenadines","Suriname","Thailand","Tonga",
               "Turkiye","Turkmenistan","Tuvalu","West Bank and Gaza")

no.infinity.bpp %>% 
  filter(country %in% unique(UM.remain))-> UM.rem.filter

UM.remain.plot <- ggplot(UM.rem.filter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,0.6) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Upper-Middle Income Country")
UM.remain.plot
