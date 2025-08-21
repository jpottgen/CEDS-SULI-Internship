#JORY POTTGEN - CEDS
#EXTRA BIOMASS PROJECT 
#ANALYSIS NOT INCLUDED IN RESEARCH PROJECT

install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)

#Estimating world biomass usage
#Run after heating.degree.days R script

bccpp.data <- c("Bangladesh","Belize","Benin","Cambodia","Cuba","Eritrea","Ethiopia","Fiji","Ghana","Haiti",
               "Indonesia","Libya","Madagascar","Niger","Nigeria","Papua New Guinea","Philippines","Samoa","Senegal",
               "Timor-Leste","Togo","Uganda","Yemen","Zimbabwe")

#---------------------------------------------------------------------------------------------------------------------------
#OTHER ASIA - CONSISTENT
bccpp.other.asia <- c("Bangladesh","Cambodia","Fiji","Papua New Guinea","Samoa","Timor-Leste")

#Finding average bccpp for Other Asia
#RESULT = 0.2815453 t/person
heat.bpp %>% 
  filter(country %in% unique(bccpp.other.asia)) %>%
  mutate(ave.bccpp = mean(bio.per.person)) -> ave.asia.bccpp

#----------------------------------------------------------------------------------------------------------------------------
#CENTRAL AND SOUTH AMERICA - CONSISTENT
bccpp.cent.south.amer <- c("Belize","Cuba","Haiti")

#Finding average bccpp for Central and South America
#RESULT = 0.214739 t/person
heat.bpp %>% 
  filter(country %in% unique(bccpp.cent.south.amer)) %>%
  mutate(ave.bccpp = mean(bio.per.person)) -> ave.csa.bccpp

#----------------------------------------------------------------------------------------------------------------------------
#AFRICA - CONSISTENT
bccpp.africa <- c("Benin","Eritrea","Ethiopia","Ghana","Libya","Madagascar","Niger","Nigeria", "Senegal",
                  "Togo","Uganda","Zimbabwe")

#Finding average bccpp for Africa
#RESULT = 0.3963658 t/person
heat.bpp %>% 
  filter(country %in% unique(bccpp.africa)) %>%
  mutate(ave.bccpp = mean(bio.per.person)) -> ave.africa.bccpp

#----------------------------------------------------------------------------------------------------------------------------
#SOUTH EAST ASIA, AUSTRALIA, NEW ZEALAND - CONSISTENT
bccpp.se.asia.aust.nz <- c("Indonesia","Philippines")

#Finding average bccpp for South, East Asia, Australia, and New Zealand
#RESULT = 0.2437628 t/person
heat.bpp %>% 
  filter(country %in% unique(bccpp.se.asia.aust.nz)) %>%
  mutate(ave.bccpp = mean(bio.per.person)) -> ave.se.asia.au.nz.bccpp

#----------------------------------------------------------------------------------------------------------------------------
#MIDDLE EAST - CONSISTENT
bccpp.middle.east <- c("Yemen")

#Finding average bccpp for Middle East
#RESULT = 0.009696114 t/person
heat.bpp %>% 
  filter(country %in% unique(bccpp.middle.east)) %>%
  mutate(ave.bccpp = mean(bio.per.person)) -> ave.middle.east.bccpp

#----------------------------------------------------------------------------------------------------------------------------
#Creating data frame with average bccpp values filled in for countries with inconsistent data
#Add Africa inconsistent
AFRICA.countries <- c("Angola", "Botswana", "Burkina Faso", "Burundi", "Cameroon", 
                "Central African Republic", "Chad", "Comoros", "Congo", 
                "Djibouti", "Egypt", "Equatorial Guinea","Gabon","Guinea","Guinea-Bissau",
                "Kenya","Liberia","Malawi","Mali","Mauritania","Mozambique","Namibia",
                "Rwanda","Sierra Leone","Somalia","South Africa","Sudan","Zambia")

ave_bio_AFRICA <- 0.3963658

biomass_df <- data.frame(
  country = AFRICA.countries,
  ave_biomass = rep(ave_bio_AFRICA, length(AFRICA.countries)))

#Add Central and South America inconsistent
CENT.SOUTH.AMER.countries <- c("Brazil","Colombia","Costa Rica","Dominican Republic","Ecuador",
                               "El Salvador","Guatemala","Honduras","Jamaica","Mexico","Paraguay",
                               "Suriname")

ave_bio_CENT.S.AM <- 0.214739

#Append to the existing data frame
biomass_df <- rbind(
  biomass_df,
  data.frame(
    country = CENT.SOUTH.AMER.countries,
    ave_biomass = rep(ave_bio_CENT.S.AM, length(CENT.SOUTH.AMER.countries))))


#Add Other Asia inconsistent
OTHER.ASIA.countries <- c("India","Pakistan","Sri Lanka","Vanuatu")

ave_bio_OTHER_ASIA <- 0.2815453

#Append to the existing data frame
biomass_df <- rbind(
  biomass_df,
  data.frame(
    country = OTHER.ASIA.countries,
    ave_biomass = rep(ave_bio_OTHER_ASIA, length(OTHER.ASIA.countries))))

#Add Middle East inconsistent
MIDDLE.EAST.countries <- c("Iraq")

ave_bio_MIDDLE_EAST <- 0.009696114

#Append to the existing data frame
biomass_df <- rbind(
  biomass_df,
  data.frame(
    country = MIDDLE.EAST.countries,
    ave_biomass = rep(ave_bio_MIDDLE_EAST, length(MIDDLE.EAST.countries))))

#Add South East Asia, Australia, New Zealand inconsistent
SE.ASIA.AUST.NZ.countries <- c("Malaysia","Thailand")

ave_bio_SE_ASIA_AUST_NZ <- 0.2437628

#Append to the existing data frame
biomass_df <- rbind(
  biomass_df,
  data.frame(
    country = SE.ASIA.AUST.NZ.countries,
    ave_biomass = rep(ave_bio_SE_ASIA_AUST_NZ, length(SE.ASIA.AUST.NZ.countries))))

#GLOBAL AVERAGE
biomass_df %>% 
  mutate(global_ave = mean(ave_biomass)) -> ave.global.bccpp

#RESULT = 0.3255003 tonnes/person using biofuel
#-----------------------------------------------------------------------------------------------------------------------------------
#UNFCCC Regional Biomass Study

library(dplyr)

#(See table 5 and table 6).
#Could you add the regional data to this figure? 
#How about adding it to the same column as “literature” 
#but put it in with a different time series (so it has a different symbol).

#REFERENCE CALCULATED VS. LITERATURE MEAN BCCPP GRAPH
#SEE SCRIPT 2

#Add in iso to data frame from Script 2
install.packages("countrycode")
library(countrycode)

#Add iso code (uppercase)
ave.global.bccpp$iso <- countrycode(ave.global.bccpp$country, 
                                    origin = 'country.name', 
                                    destination = 'iso3c')

#Make iso code lowercase
ave.global.bccpp$iso <- tolower(ave.global.bccpp$iso)

#Rename "Value" to "ave_biomass" in long.lit.bio.coal 
long.lit.bio.coal %>%
  rename(ave_biomass = Value) -> lbc.ave.bio

#Combine my regional averages with country-specific literature and national calculated estimates
litcalc.bccpp <- ave.global.bccpp %>%
  full_join(lbc.ave.bio, by = c("iso","ave_biomass"))

litcalc.bccpp$subregion <- countrycode(litcalc.bccpp$iso, origin = 'iso3c', destination = 'un.regionsub.name')

#Assign region names
#Add in UNFCCC average regional estimates
subregion.bccpp <- litcalc.bccpp %>%
  mutate(region_custom = case_when(
    subregion %in% c("Sub-Saharan Africa", "Northern Africa") ~ "Africa",
    subregion == "Latin America and the Caribbean" ~ "Central and South America",
    subregion %in% c("Southern Asia", "Eastern Asia") ~ "Other Asia",
    subregion %in% c("South-eastern Asia", "Melanesia", "Micronesia", "Polynesia") ~ "South East Asia and Aust/NZ",
    subregion == "Western Asia" ~ "Middle East",
    TRUE ~ "Other")) %>%
  select(iso, region_custom, ave_biomass, ave.char.frac, dmy.x, Biomass) %>%
  rename(region = region_custom) %>%
  mutate(Biomass = ifelse(is.na(Biomass), "regional", Biomass)) %>%
  mutate(dmy.x = ifelse(is.na(dmy.x), "1", dmy.x)) %>%
  mutate(reg_biomass = case_when(
    region == "Africa" ~ 0.59,
    region == "Central and South America" ~ 1.1,
    region == "Other Asia" ~ 0.45,
    region == "South East Asia and Aust/NZ" ~ 0.44,
    region == "Middle East" ~ 0.59,
    TRUE ~ NA_real_))
#------------------------------------------------------------------------------------------------------------------------------------
#FINAL GRAPH
regional.lcbc.PLOT <- ggplot(subregion.bccpp, aes(x = dmy.x, y = ave_biomass)) +
  geom_point(
    aes(shape = Biomass, color = ave.char.frac), 
    size = 3, 
    position = position_jitterdodge(jitter.width = 0.08, dodge.width = 0.3)) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Per Person Cooking Biofuel Consumption",
       x = NULL,
       y = "Average Biomass Consumption (t/person)",
       color = "Average % Charcoal",
       shape = "Biomass")

regional.lcbc.PLOT
