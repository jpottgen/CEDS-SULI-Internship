#JORY POTTGEN - SCRIPT 6 
#Code Revised - 8/11/2025
#COUNTRIES W/ HEATING DEGREE DAYS <1500

#--------------------------------------------------------------------------------------------------------------------------------------
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)

#RUN AFTER SCRIPTS 1, 2, 3, 4, & 5

#Heating Degree Days
heatingDD <- read.csv("heatingDD.csv")

#heating degree days in 2024 below 1500
heatDD2024 <- heatingDD %>%
  select(country, X2024) %>%
  filter(X2024 < 1500)

#Region Data
master.country <- read.csv("mastercountry.csv")

master.country %>%
  filter(X1 == 1) %>%
  rename(country = Country_Name) %>%
  select(-X1, -Region) -> MASTERcountry

#Filter by region
#-------------------------------------------------------------------------------
#Central and South America
cent.south.amer <- MASTERcountry %>%
  filter(Figure_Region == "Central and South America")

CENT.SOUTH.AMER <- c("Aruba","Netherlands Antilles","Argentina","Antigua and Barbuda",
                     "Bahamas","Belize","Bermuda","Bolivia","Brazil","Barbados","Chile",
                     "Colombia","Costa Rica","Cuba","Curacao","Cayman Islands","Dominica",
                     "Dominican Republic","Ecuador","Falkland Islands","Faeroe Islands",
                     "Guadeloupe","Grenada","Guatemala","French Guiana","Guyana","Honduras",
                     "Haiti","Jamaica","Saint Kitts and Nevis","Saint Lucia","Mexico",
                     "Montserrat","Martinique","Nicaragua","Panama","Peru","Paraguay",
                     "El Salvador","Saint Pierre and Miquelon","Suriname","Sint Maarten",
                     "Turks and Caicos Islands","Trinidad and Tobago","Uruguay",
                     "Saint Vincent And Grenadines","Venezuela","British Virgin Islands",
                     "United States Virgin Islands")

heatDD2024 %>%
  filter(country %in% unique(CENT.SOUTH.AMER))-> cent.south.amer.heatDD

CENT.SOUTH.AMER.HEAT <- c("Belize","Brazil","Colombia","Costa Rica","Cuba","Dominican Republic",
                          "Ecuador","El Salvador","French Guiana","Guadeloupe","Guatemala",
                          "Guyana","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama",
                          "Paraguay","Suriname","Venezuela")

no.infinity.bpp %>% 
  filter(country %in% unique(CENT.SOUTH.AMER.HEAT))-> filter.cent.south.amer

cent.south.amer.plot <- ggplot(filter.cent.south.amer, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,2) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in Central South America")
cent.south.amer.plot

ggsave("cent.south.amer.png", plot = cent.south.amer.plot)

#-------------------------------------------------------------------------------
#Other Asia
other.asia <- MASTERcountry %>%
  filter(Figure_Region == "Other Asia")

OTHER.ASIA <- c("Afghanistan","American Samoa","Bangladesh","Bhutan","Cook Islands","Fiji",
                "Guam", "Hong Kong","India","Japan","Cambodia","Kiribati","Republic of Korea",
                "Laos","Sri Lanka","Macao","Maldives","Marshall Islands","Montenegro","Mongolia",
                "New Caledonia","Niue","Nepal","Pakistan","Palau","Papua New Guinea",
                "Democratic Peoples Republic of Korea","French Polynesia","Soloman Islands",
                "Tokelau","Timor-Leste","Tonga","Chinese Taipei","Vanuatu","Wallis and Futuna Islands",
                "Samoa","Yugoslavia")

heatDD2024 %>%
  filter(country %in% unique(OTHER.ASIA))-> other.asia.heatDD

OTHER.ASIA.HEAT <- c("Bangladesh","Cambodia","Fiji","French Polynesia","Hong Kong",
                     "India","Laos","New Caledonia","Pakistan","Papua New Guinea",
                     "Samoa","Sri Lanka","Timor-Leste","Vanuatu")

no.infinity.bpp %>% 
  filter(country %in% unique(OTHER.ASIA.HEAT))-> filter.oth.asia

otherasia.plot <- ggplot(filter.oth.asia, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,2) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in Other Asia")
otherasia.plot

ggsave("other.asia.png", plot = otherasia.plot)

#-------------------------------------------------------------------------------
#Africa
africa <- MASTERcountry %>%
  filter(Figure_Region == "Africa")

AFRICA <- c("Angola","Burundi","Benin","Burkina Faso","Botswana",
            "Central African Republic","Cote dIvoire","Cameroon",
            "Democratic Republic of Congo","Congo","Comoros","Cape Verde",
            "Djibouti","Algeria","Egypt","Eritrea","Western Sahara","Ethiopia",
            "Gabon","Ghana","Guinea","Gambia","Guinea-Bissau","Equatorial Guinea",
            "Kenya","Liberia","Libya","Lesotho","Morocco","Madagascar",
            "Mali","Mozambique","Mauritania","Mauritius","Malawi","Namibia",
            "Niger","Nigeria","Reunion","Rwanda","Sudan","Senegal","Sierra Leone",
            "Somalia","South Sudan","Sao Tome and Principe","Swaziland",
            "Seychelles","Chad","Togo","Tunisia","Tanzania","Uganda","South Africa",
            "Zambia","Zimbabwe")

heatDD2024 %>%
  filter(country %in% unique(AFRICA))-> africa.heatDD

AFRICA.HEAT <- c("Angola","Burundi","Benin","Burkina Faso","Botswana",
            "Cameroon","Cape Verde","Central African Republic","Chad",
            "Comoros","Congo","Cote dIvoire","Djibouti","Egypt","Equatorial Guinea",
            "Eritrea","Ethiopia","Gabon","Ghana","Guinea","Guinea-Bissau","Kenya",
            "Liberia","Libya","Madagascar","Malawi","Mali","Mauritania","Mozambique",
            "Namibia","Niger","Nigeria","Reunion","Rwanda","Senegal","Sierra Leone",
            "Somalia","South Africa","Sudan","Swaziland","Tanzania","Togo","Uganda",
            "Western Sahara","Zambia","Zimbabwe")

no.infinity.bpp %>% 
  filter(country %in% unique(AFRICA.HEAT))-> filter.africa

africa.plot <- ggplot(filter.africa, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,2) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in Africa")
africa.plot

ggsave("africa.plot.png", plot = africa.plot)

#--------------------------------------------------------------------------------
#Eastern Europe
east.europe <- MASTERcountry %>%
  filter(Figure_Region == "Eastern Europe")

EAST.EURO <- c("Albania","Bulgaria","Bosnia and Herzegovina","Czechoslovakia",
               "Czech Republic","Croatia","Hungary","Macedonia","Poland",
               "Romania","Serbia and Montenegro","Serbia","Kosovo","Slovakia",
               "Slovenia")

heatDD2024 %>%
  filter(country %in% unique(EAST.EURO))-> east.euro.heatDD

#NO EASTERN EUROPE COUNTRIES WITH <1500 DEGREE DAYS

#-------------------------------------------------------------------------------
#Middle East
middle.east <- MASTERcountry %>%
  filter(Figure_Region == "Middle East")

MIDDLE.EAST <- c("Bahrain","Iraq","Islamic Republic of Iran","Israel","Jordan",
                 "Kuwait","Lebanon","Oman","Palestine","Qatar","Saudi Arabia",
                 "Syria","United Arab Emirates","Yemen")

heatDD2024 %>%
  filter(country %in% unique(MIDDLE.EAST))-> middle.east.heatDD

MID.EAST.HEAT <- c("Iraq","Israel","Kuwait","Oman","Qatar","Saudi Arabia",
                   "United Arab Emirates","Yemen")

no.infinity.bpp %>% 
  filter(country %in% unique(MID.EAST.HEAT))-> filter.mid.east

mid.east.plot <- ggplot(filter.mid.east, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,2) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in the Middle East")
mid.east.plot

#Giving Yemen a dotted line (consistent one)
dotted.mid.east <- ggplot(filter.mid.east, aes(x = year, y = bio.per.person, color = country, linetype = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,2) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in the Middle East")

dotted.mid.east

ggsave("mid.east.plot.png", plot = mid.east.plot)

#-------------------------------------------------------------------------------
#FSU = Former Soviet Union
FSU <- MASTERcountry %>%
  filter(Figure_Region == "FSU")

FORMERSU <- c("Armenia","Azerbaijan","Belarus","Estonia","Georgia","Kazakhstan",
              "Kyrgyzstan","Lithuania","Latvia","Moldova","Russia","Tajikistan",
              "Turkmenistan","Ukraine","Soviet Union","Uzbekistan")

heatDD2024 %>%
  filter(country %in% unique(FORMERSU))-> former.su.heatDD

#NO FORMER SOVIET UNION COUNTRIES WITH <1500 DEGREE DAYS

#-------------------------------------------------------------------------------
#South East Asia and Aust/NZ
SE.Asia.Aust.NZ <- MASTERcountry %>%
  filter(Figure_Region == "South East Asia and Aust/NZ")

SE.ASIA.AUST.NZ <- c("Australia","Brunei Darussalam","Federated States of Micronesia",
                     "Indonesia","Myanmar","Burma","Malaysia","New Zealand",
                     "Philippines","Singapore","Thailand","Vietnam")

heatDD2024 %>%
  filter(country %in% unique(SE.ASIA.AUST.NZ))-> se.asia.aust.nz.heatDD

se.ASIA.AUST.NZ.HEAT <- c("Indonesia","Malaysia","Myanmar","Philippines","Singapore",
                          "Thailand","Vietnam")

no.infinity.bpp %>% 
  filter(country %in% unique(se.ASIA.AUST.NZ.HEAT))-> filter.se.asia.aust.nz

se.asia.aust.nz.plot <- ggplot(filter.se.asia.aust.nz, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,2) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in South East Asia")
se.asia.aust.nz.plot

ggsave("se.asia.aust.nz.plot.png", plot = se.asia.aust.nz.plot)

#-------------------------------------------------------------------------------
#Western Europe
west.europe <- MASTERcountry %>%
  filter(Figure_Region == "Western Europe")

WEST.EURO <- c("Austria","Belgium","Luxembourg","Belgium-Luxembourg","Switzerland",
               "Cyprus","Germany","Denmark","Spain","Finland","France","United Kingdom",
               "Gibraltar","Greece","Greenland","Ireland","Iceland","Italy","Liechtenstein",
               "Malta","Netherlands","Norway","Portugal","Sweden","Turkey")

heatDD2024 %>%
  filter(country %in% unique(WEST.EURO))-> west.euro.heatDD

WEST.EURO.HEAT <- c("Denmark")

no.infinity.bpp %>% 
  filter(country %in% unique(WEST.EURO.HEAT))-> filter.west.euro

#DENMARK DOESN'T APPEAR IN THE BIOMASS DATA SET

#-------------------------------------------------------------------------------
#North America
north.amer <- MASTERcountry %>%
  filter(Figure_Region == "North America")

NORTH.AMER <- c("Canada","Puerto Rico","United States")

heatDD2024 %>%
  filter(country %in% unique(NORTH.AMER))-> north.amer.heatDD

NORTH.AMER.HEAT <- c("Puerto Rico")

no.infinity.bpp %>% 
  filter(country %in% unique(NORTH.AMER.HEAT))-> filter.north.amer

#PUERTO RICO DOESN'T APPEAR IN THE BIOMASS DATA SET

#-------------------------------------------------------------------------------
#China
china <- MASTERcountry %>%
  filter(Figure_Region == "China")

CHINA <- c("China")

heatDD2024 %>%
  filter(country %in% unique(CHINA))-> china.heatDD

#CHINA NOT FOUND IN HEAT DEGREE DAYS DATA SET
#-------------------------------------------------------------------------------

#FINAL RESULTS
#Graphs for:
#South East Asia
#Middle East
#Africa
#Central South America
#Other Asia

#No graphs for:
#China
#North America
#Western Europe
#Eastern Europe
#Former Soviet Union

#-----------------------------------------------------------------------------------------------------------------------------------------
#FACET BY REGION
#SIDE BY SIDE GRAPHS

heat.bpp <- no.infinity.bpp %>%
  left_join(heatDD2024, by = c("country")) %>%
  left_join(MASTERcountry, by = c("country","iso")) %>%
  select(iso, country, year, Figure_Region, bio.per.person, X2024) %>%
  filter(!(is.na(X2024))) %>%
  filter(!(is.na(Figure_Region)))

#Filter by slope
heat.slope <- heat.bpp %>% group_by(country) %>% summarise(tidy(lm(bio.per.person ~ year)))

heat.bpp.slope <- filter(heat.slope, term == "year")
heat_bpp_slope <- filter(heat.bpp.slope, abs(estimate) < 0.0035)

heatslope <- c("Bangladesh","Belize","Benin","Cambodia","Cuba","Eritrea","Ethiopia","Fiji","Ghana","Haiti",
               "Indonesia","Libya","Madagascar","Niger","Nigeria","Papua New Guinea","Philippines","Samoa","Senegal",
               "Timor-Leste","Togo","Uganda","Yemen","Zimbabwe")

heat.bpp %>% 
  filter(country %in% unique(heatslope))-> SLOPE.HEAT

#Facet by region including only countries with horizontal slopes
slope.facet.region <- ggplot(SLOPE.HEAT, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,2) +
  facet_wrap(vars(Figure_Region)) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita by Region")

slope.facet.region
ggsave("slope.facet.region.png", plot = slope.facet.region)

#Only countries with variable slopes
variable.slope <- c("Angola","Botswana","Brazil","Burkina Faso","Burundi","Cameroon",
                    "Central African Republic","Chad","Colombia","Comoros","Congo","Costa Rica",
                    "Djibouti","Dominican Republic","Ecuador","Egypt","El Salvador","Equatorial Guinea",
                    "Gabon","Guatemala","Guinea","Guinea-Bissau","Honduras","India","Iraq","Jamaica",
                    "Kenya","Liberia","Malawi","Malaysia","Mali","Mauritania","Mexico","Mozambique",
                    "Namibia","Nicaraqua","Pakistan","Paraguay","Rwanda","Sierra Leone","Somalia","South Africa",
                    "Sri Lanka","Sudan","Suriname","Thailand","Vanuatu","Zambia")

heat.bpp %>% 
  filter(country %in% unique(variable.slope))-> VAR.SLOPE

#Regional bpp
#Blue = consistent, flat slopes
#Red = inconsistent, variable slopes
red.blue <- ggplot(mapping = aes(x = year, y = bio.per.person, group = country)) + 
  geom_point(data = SLOPE.HEAT, color = "blue", size = 0.75) + geom_point(data = VAR.SLOPE, color = "red3", size = 0.00001) +
  geom_smooth(data = SLOPE.HEAT, method = loess, formula = y ~ x, color = "blue", se = FALSE) +
  geom_smooth(data = VAR.SLOPE, method = loess, formula = y ~ x, color = "red3", linewidth = 0.5, se = FALSE) +
  ylim(0,2) +
  facet_wrap(vars(Figure_Region)) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita by Region")

red.blue

ggsave("red.blue.png", plot = red.blue, height = 6.5, width = 8.7)

#Facet by region with all countries with <1500 HDD 
#includes both horizontal and variable slopes
#each country is a different color line
facet.region.bpp <- ggplot(heat.bpp, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,2) +
  facet_wrap(vars(Figure_Region)) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita by Region")
  
facet.region.bpp

ggsave("facet.region.png", plot = facet.region.bpp, height = 6, width = 13)

#----------------------------------------------------------------------------------------------------------------------------------------------
#Remove consistent countries (horizontal slopes)
#filter MASTERcountry 
minus.stable <- c("Angola","Botswana","Brazil","Burkina Faso","Burundi","Cameroon","Central African Republic",
                  "Chad","Colombia","Comoros","Congo","Costa Rica","Djibouti","Dominican Republic",
                  "Ecuador","Egypt","El Salvador","Equatorial Guinea","Gabon","Guatemala","Guinea",
                  "Guinea-Bissau","Honduras","India","Iraq","Jamaica","Kenya","Liberia","Malawi",
                  "Malaysia","Mali","Mauritania","Mexico","Mozambique","Namibia","Nicaragua","Pakistan",
                  "Paraguay","Rwanda","Sierra Leone","Somalia","South Africa","Sri Lanka","Sudan",
                  "Suriname","Thailand","Zambia")

MASTERcountry %>%
  filter(country %in% unique(minus.stable))-> minus.MASTER.country

minus.heat.bpp <- no.infinity.bpp %>%
  left_join(heatDD2024, by = c("country")) %>%
  left_join(minus.MASTER.country, by = c("country","iso")) %>%
  select(iso, country, year, Figure_Region, bio.per.person, X2024) %>%
  filter(!(is.na(X2024))) %>%
  filter(!(is.na(Figure_Region)))

minus.facet.reg.bpp <- ggplot(minus.heat.bpp, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,2) +
  facet_wrap(vars(Figure_Region)) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita by Region")

minus.facet.reg.bpp

#Remove ones with multiple years of stable data (determined before from income graphs)
minus.stable2 <- c("Angola","Botswana","Brazil","Burkina Faso","Burundi","Cameroon","Central African Republic",
                  "Chad","Colombia","Comoros","Congo","Costa Rica","Djibouti","Dominican Republic",
                  "Ecuador","Egypt","El Salvador","Equatorial Guinea","Gabon","Guatemala","Guinea",
                  "Guinea-Bissau","Honduras","India","Iraq","Jamaica","Liberia","Malawi",
                  "Malaysia","Mali","Mauritania","Mexico","Nicaragua",
                  "Paraguay","Sierra Leone","Somalia","South Africa","Sudan",
                  "Suriname","Thailand","Zambia")

MASTERcountry %>%
  filter(country %in% unique(minus.stable2))-> minus.MASTER.country2

minus.heat.bpp2 <- no.infinity.bpp %>%
  left_join(heatDD2024, by = c("country")) %>%
  left_join(minus.MASTER.country2, by = c("country","iso")) %>%
  select(iso, country, year, Figure_Region, bio.per.person, X2024) %>%
  filter(!(is.na(X2024))) %>%
  filter(!(is.na(Figure_Region)))

minus.facet.reg.bpp2 <- ggplot(minus.heat.bpp2, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  ylim(0,2) +
  facet_wrap(vars(Figure_Region)) +
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita by Region")

minus.facet.reg.bpp2
