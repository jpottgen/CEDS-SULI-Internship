#Jory Pottgen, CEDS

# Read in the various csv files
# Stonernew = Stoner et al article data
# CEDS.biomass = CEDS data
# pop.data = Urban & Rural Population data
data1 <- read.csv("Stonernew.csv")
data1
data2 <- read.csv("CEDS.biomass.csv")
data2
data3 <- read.csv("pop.data.csv")
data3
data4 <- read.csv("stoner.fueltype.csv")
data4
data5 <- read.csv("stoner.fueltype2.csv")
data5
data6 <- read.csv("stoner.fueltype3.csv")
data6

# install.packages("tidyverse")
library(tidyverse)

library(dplyr)

# Clean up data 1
Stonerdata <- data1 %>%
  rename(country = Location,
         year = Period,
         CentPopPercent = FactValueNumeric) %>%
  arrange (country, year) %>%
  select(-Dim2) %>%
  select(-DateModified) %>%
  select(-Language) %>%
  select(-FactComments) %>%
  select(-FactValueTranslationID) %>%
  select(-ValueType) %>%
  select(-Location.type) %>%
  select(-Period.type) %>%
  select(-IsLatestYear) %>%
  select(-ParentLocation) %>%
  select(-Dim2.type) %>%
  select(-Dim2ValueCode) %>%
  select(-Dim3.type) %>%
  select(-Dim3) %>%
  select(-Dim3ValueCode) %>%
  select(-FactValueUoM) %>%
  select(-DataSource) %>%
  select(-DataSourceDimValueCode) %>%
  select(-Dim1.type) %>%
  select(-FactValueNumericLowPrefix) %>%
  select(-IndicatorCode) %>%
  select(-Indicator) %>%
  select(-ParentLocationCode) %>%
  select(-Dim1ValueCode) %>%
  select(-FactValueNumericHighPrefix) %>%
  select(-FactValueNumericPrefix)

# Clean up data 2
CEDSdata <- data2 %>%
  rename(ruralpop = pop2) %>%
  arrange(iso, year) %>%
  select(-Fern_pc) %>%
  select(-ceds_pc_orig) %>%
  select(-units) %>%
  select(-ceds_pc_ext) %>%
  select(-ceds_pc_final) %>%
  select(-src)

#Clean up data 3
popdata <- data3 %>%
  select(-scenario)

#Clean up data 4
stonerfueltype <- data4 %>%
  rename(fuel = X.1,
         year = X,
         country = Indicator) %>%
  select(-Population.with.primary.reliance.on.fuels.and.technologies.for.cooking..by.fuel.type..in.millions.) %>%
  select(-Population.with.primary.reliance.on.fuels.and.technologies.for.cooking..by.fuel.type..in.millions..1) %>%
  select(-Population.with.primary.reliance.on.fuels.and.technologies.for.cooking..by.fuel.type..in.millions..2) %>%
  select(-Proportion.of.population.with.primary.reliance.on.fuels.and.technologies.for.cooking..by.fuel.type.....1)
         
#Clean up new Stoner Fuel Type data
fueltype2 <- data5

#Clean up Stoner Fuel Type data now that brackets have been separated
#Convert prop.total into usable decimals
fueltype3 <- data6 %>%
  select(country, year, fuel, prop.total) %>%
  mutate(prop.decimal = prop.total/100)

# Calculate Urban and Rural Population
# Population is in thousands of people
popdata <- data3 %>%
  mutate(urban_pop = pop*urban_share) %>%
  mutate(rural_pop = pop*(1-urban_share))

# Matching columns "year" and "iso" between popdata & CEDSdata
# Creating new column of Total Biomass/Rural Pop
popdata %>%
  left_join(CEDSdata, by = c("year","iso")) %>%
  mutate(value = if_else(is.na(rural_pop), ceds_tot_final, ceds_tot_final / rural_pop))

#Creating new column of Total Biomass/Urban Pop
popdata %>%
  left_join(CEDSdata, by = c("year","iso")) %>%
  mutate(value = if_else(is.na(urban_pop), ceds_tot_final, ceds_tot_final / urban_pop))

Stonerdata %>%
  left_join(popdata, by = c("year","country")) %>%
  mutate(value = if_else(is.na(CentPopPercent), pop, pop * CentPopPercent))

# Revised Equations
#value = ceds_tot_final / urban_pop
#Per_cap_urban_biomass = ceds_tot_final / urban_pop

#Filtering Fuel Type Data for only Biomass
filter(stonerfueltype, fuel == "Biomass")

#Revised Filtering
filter(fueltype2, fuel == "Biomass")
bio <- filter(fueltype3, fuel == "Biomass")

#Multiply proportion of pop using biomass by the total population
#This will result in the total number of people using biomass as fuel
#Error, characters will not match up with integers
popdata %>%
  left_join(fueltype2, by = c("year","country")) %>%
  mutate(value = if_else(is.na(prop.total), pop, pop * prop.total))

#Use Text to Column feature on Excel
#Separate central value from bracket range in fueltype2 data
#Use prop.total values
ppl.using.bio <- popdata %>%
  left_join(fueltype3, by = c("year","country")) %>%
  mutate(ppl.using.biomass = if_else(is.na(prop.total), pop, pop * prop.total))

#Calculating population totals for only Biomass
#biomass.population is the population of people using traditional biomass
trad.bio.pop <- popdata %>%
  left_join(bio, by = c("year","country")) %>%
  mutate(biomass.population = if_else(is.na(prop.decimal), pop, pop * prop.decimal))

#Results of last step:
#prop.decimal is Prop of pop with primary reliance on fuels and technologies for cooking, by fuel type (%) from Stoner et al
#pop is in 1000s of people from CEDS population data

#Removing NA 
#Keeping only data that has a value for prop.decimal
noNA.poptotals <- trad.bio.pop %>% filter(!(is.na(prop.decimal)))

#Practice Graphs, Sorting by one country only
Afg <- filter(noNA.poptotals, country == "Afghanistan")
ggplot(Afg, aes(x=year, y=biomass.population)) + geom_line()

#Line Graph of all countries across time
#Population of people using traditional biomass between 1990 and 2022 for every country
library(ggplot2)
bio.pop.plot <- ggplot(noNA.poptotals, aes(x = year, y = biomass.population, color = country)) + geom_line(show.legend = FALSE)
plot(bio.pop.plot)
ggsave("bio.pop.plot.png", plot = bio.pop.plot)

#use ylim to limit the upper bound to a population of 200,000
bio.pop.plot <- ggplot(noNA.poptotals, aes(x = year, y = biomass.population, color = country)) + geom_line(show.legend = FALSE) + ylim(0,2e+05)
plot(bio.pop.plot)

#use ylim to limit the upper bound to a population of 50,000
bio.pop.plot <- ggplot(noNA.poptotals, aes(x = year, y = biomass.population, color = country)) + geom_line(show.legend = FALSE) + ylim(0,50000)
plot(bio.pop.plot)

#use ylim to limit the upper bound to a population of 10,000
bio.pop.plot <- ggplot(noNA.poptotals, aes(x = year, y = biomass.population, color = country)) + geom_line(show.legend = FALSE) + ylim(0,10000)
plot(bio.pop.plot)

#use ylim to limit the upper bound to a population of 2500
bio.pop.plot <- ggplot(noNA.poptotals, aes(x = year, y = biomass.population, color = country)) + geom_line(show.legend = FALSE) + ylim(0,2500)
plot(bio.pop.plot)

#use ylim to limit the upper bound to a population of 500
bio.pop.plot <- ggplot(noNA.poptotals, aes(x = year, y = biomass.population, color = country)) + geom_line(show.legend = FALSE) + ylim(0,500)
plot(bio.pop.plot)

#Identifying charcoal data
#add the charcoal fraction (charcoal/(charcoal + all other biomass)
#from Stoner to the data and then use that for the color scale
data6 <- read.csv("stoner.fueltype3.csv")
data6

fueltype3 <- data6 %>%
  select(country, year, fuel, prop.total) %>%
  mutate(prop.decimal = prop.total/100)

charcoal <- filter(fueltype3, fuel == "Charcoal")

#pop is in thousands of people
#so charcoal.use is in thousands of people using charcoal
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

#As you start to break that apart, you can then begin to bring
#in other relevant information into the analysis.
#One of the additional statistics you could calculate is the 
#fraction of people using biomass in a country that are urban vs rural
#You'll need to combine stoner with the population statistics to 
#estimate that.  Fraction of biomass users that are rural would be
#rural_pctbio*rural_Pop/(rural_pctbio*rural_Pop + urban_pctbio*urban_Pop)
#For example, if there is a rural study with higher ppb use than our 
#implied IEA values, but that had 80% of biomass users being rural,
#then rural urban differences are not likely to play a large role.

#Fraction of people using biomass
#Highlighting Urban vs. Rural

#share data = proportion of population
#pop data = thousands of people
popdata <- data3 %>%
  mutate(urban_pop = pop*urban_share) %>%
  mutate(rural_share = 1-urban_share) %>%
  mutate(rural_pop = pop*(1-urban_share))

filtered.bio <- data6 %>%
  filter(fuel == "Biomass") %>%
  mutate(PROP.RUR = prop.rural/100) %>%
  mutate(PROP.URB = prop.urban/100)

bio.fract <- filtered.bio %>%
  left_join(popdata, by = c("year","country")) %>%
  select(-UN_code) %>%
  select(-scenario) %>%
  mutate(rural.bio.users = PROP.RUR * rural_pop) %>%
  mutate(urban.bio.users = PROP.URB * urban_pop) %>%
  mutate(tot.bio.users = rural.bio.users + urban.bio.users) %>%
  mutate(rural.fraction = rural.bio.users/tot.bio.users) %>%
  mutate(urban.fraction = urban.bio.users/tot.bio.users)

#Sample Bangladesh Graph
bangl <- bio.fract %>%
  filter(country == "Bangladesh") %>%
  pivot_longer(cols = c(rural.fraction, urban.fraction), names_to = "fraction", values_to = "value")

Bangladesh.plot <- ggplot(bangl, aes(x = year, y = value, color = fraction)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Fraction of people using biomass") +
  ggtitle("Rural vs. Urban Biomass Use in Bangladesh")
Bangladesh.plot

#Filtering for countries with inconsistent data between lit and calc
bangladesh <- biofract %>%
  filter(country == "Bangladesh")

eritrea <- biofract %>%
  filter(country == "Eritrea")

fiji <- biofract %>%
  filter(country == "Fiji")

ghana <- biofract %>%
  filter(country == "Bangladesh")