#JORY POTTGEN - SCRIPT 2
#Code Revised - 8/11/2025
#INCOME GROUP BPP GRAPHS (LOW, LOWER-MIDDLE, UPPER-MIDDLE)
#CALCULATED VS. LITERATURE BPP + AVE. % CHARCOAL

#----------------------------------------------------------------------------------------------------------------------------------
#Install packages
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)

#RUN AFTER SCRIPT 1

#Estimating the amount of biomass used per person that uses biomass
#Calculation: Residential biomass consumption / number of people using biomass
#number of people using biomass = total population * fraction of population using biomass

#total population will come from variable pop in popdata
#fraction of population using biomass will come from CentPopPercent in Stoner et al data
#ceds_tot_final column is total residential biomass in kt (kilo-tonnes)

#CentPopPercent = prop of pop with primary reliance on clean fuels for cooking
#subtract percent from 100 to get non-clean fuels (biomass)
#divide percentage by 100 to get a proportion
stoner.nonclean <- Stonerdata %>%
  mutate(nonclean.prop = (100 - CentPopPercent)/100)

#multiply pop by prop of people using non-clean biomass to get total pop using biomass
#pop = 1000s
total.pop.using.bio <- stoner.nonclean %>%
  left_join(popdata, by = c("year","country")) %>%
  mutate(ppl.biomass = if_else(is.na(nonclean.prop), pop, pop * nonclean.prop)) %>%
  rename(iso = iso.x) %>%
  mutate(iso = tolower(iso))

#biomass per person that uses biomass
#ppl.biomass = 1000s
#ceds_tot_final = kilo-tonnes (kt)
#bio.per.person = kt/1000s of people
biomass.per.cap <- CEDSdata %>%
  right_join(total.pop.using.bio, by = c("year","iso")) %>%
  mutate(bio.per.person = if_else(is.na(ceds_tot_final), ppl.biomass, ceds_tot_final / ppl.biomass)) %>%
  select(-iso.y.y, -UN_code)

#Removing NA 
#Keeping only data that has a value for bio.per.person
noNA.bio.per.person <- biomass.per.cap %>% filter(!(is.na(bio.per.person)))

TOTALS.bpp <- filter(noNA.bio.per.person, type == "Total")

#Biomass per person using biomass plot
bio.per.person.plot <- ggplot(TOTALS.bpp, aes(x = year, y = bio.per.person, color = country)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = loess, formula = y ~ x) +
  ylim(0,10) + xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Person Using Biomass in each Country")
bio.per.person.plot

#Take out infinity
#infinity bpp = high income country, low number of people using biofuels
no.infinity.bpp <- TOTALS.bpp %>% filter(!(is.infinite(bio.per.person)))

#Identify maximum and minimum bpp
max.min <- no.infinity.bpp %>%
  group_by(country) %>%
  mutate(maximum = max(bio.per.person)) %>%
  mutate(minimum = min(bio.per.person))

#Facet wrap by country
bio.per.person.plot <- ggplot(no.infinity.bpp, aes(x = year, y = bio.per.person, color = country)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~country)
plot(bio.per.person.plot)

#Facet wrap by year
bio.per.person.plot <- ggplot(no.infinity.bpp, aes(x = year, y = bio.per.person, color = country)) +
  geom_point(show.legend = FALSE) +
  ylim(0,100) +
  facet_wrap(~year)
plot(bio.per.person.plot)

#---------------------------------------------------------------------------------------------------------------------------
#REFERENCE WORLD BANK INFORMATION
#Grouping countries into lower, middle, and high-income groups
income.groups <- read.csv("WB_income_groups.csv")
income.levels <- income.groups %>%
  rename(country = country_name)

#filtering for different income levels
L.income2022 <- filter(income.levels, X2022 == "L")
LM.income2022 <- filter(income.levels, X2022 == "LM")
UM.income2022 <- filter(income.levels, X2022 == "UM")
H.income2022 <- filter(income.levels, X2022 == "H")

#creating a vector with the names of all low-income countries
Lcountry <- c("Afghanistan","Burkina Faso", "Burundi","Central African Republic","Chad",
              "Congo Democratic Republic","Eritrea","Ethiopia","Gambia","Guinea-Bissau",
              "Korea Democratic Republic","Liberia","Madagascar","Malawi","Mali","Mozambique",
              "Niger","Rwanda","Sierra Leone","Somalia","South Sudan","Sudan","Syrian Arab Republic",
              "Togo","Uganda","Yemen")

no.infinity.bpp %>% 
  filter(country %in% unique(Lcountry))-> Lowfilter

#create ggplot showing biomass per person for all Low income countries
low.plot <- ggplot(Lowfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  xlab("Year") + ylab("Biomass per one thousand people (kt/thousand)") +
  ggtitle("Biomass Per Capita in each Low-Income Country")
low.plot

ggsave("low_income_bpp.png", plot = low.plot)

#creating a vector with the names of all lower-middle income countries
LMcountry <- c("Algeria","Angola","Bangladesh","Benin","Bhutan","Bolivia","Cabo Verde","Cambodia",
               "Cameroon","Comoros","Congo Republic","Coted Ivoire","Djibouti","Egypt","Eswatini",
               "Ghana","Guinea","Haiti","Honduras","India","Iran","Jordan","Kenya","Kiribati",
               "Kyrgyz Republic","Lao PDR","Lebanon","Lesotho","Mauritania","Micronesia","Mongolia",
               "Morocco","Myanmar","Nepal","Nicaragua","Nigeria","Pakistan","Papua New Guinea",
               "Philippines","Samoa","Sao Tome and Principe","Senegal","Solomon Islands","Sri Lanka",
               "Tajikistan","Tanzania","Timor-Leste","Tunisia","Ukraine","Uzbekistan","Vanuatu","Vietnam",
               "Zambia","Zimbabwe")

no.infinity.bpp %>% 
  filter(country %in% unique(LMcountry))-> LMfilter

#create ggplot showing biomass per person for all Lower-Middle income countries
lowmiddle.plot <- ggplot(LMfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  xlab("Year") + ylab("Biomass per one thousand people (kt/thousand)") +
  ggtitle("Biomass Per Capita in each Lower-Middle Income Country")
lowmiddle.plot

ggsave("lowermiddle_income_bpp.png", plot = lowmiddle.plot)

#Bounded Lower-Middle income countries
lowmiddle.bounded.plot <- ggplot(LMfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  ylim(0,2) +
  geom_smooth(method = loess, formula = y ~ x) + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Lower-Middle Income Country (Bounded)")
lowmiddle.bounded.plot

ggsave("BOUNDED_LM_income_bpp.png", plot = lowmiddle.bounded.plot)

#creating a vector with the names of all upper-middle income countries
UMcountry <- c("Albania","Argentina","Armenia","Azerbaijan","Belarus","Belize","Bosnia and Herzegovina","Botswana",
               "Brazil","Bulgaria","China","Colombia","Costa Rica","Cuba","Dominica","Dominican Republic","Ecuador",
               "El Salvador","Equatorial Guinea","Fiji","Gabon","Georgia","Grenada","Guatemala","Indonesia",
               "Iraq","Jamaica","Kazakhstan","Kosovo","Libya","Malaysia","Maldives","Marshall Islands","Mauritius",
               "Mexico","Moldova","Montenegro","Namibia","North Macedonia","Palau","Paraguay","Peru","Russian Federation",
               "Serbia","South Africa","St. Lucia","St. Vincent and the Grenadines","Suriname","Thailand","Tonga",
               "Turkiye","Turkmenistan","Tuvalu","West Bank and Gaza")

no.infinity.bpp %>% 
  filter(country %in% unique(UMcountry))-> UMfilter

#create ggplot showing biomass per person for all upper-middle income countries
uppermiddle.plot <- ggplot(UMfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  xlab("Year") + ylab("Biomass per one thousand people (kt/thousand)") +
  ggtitle("Biomass Per Capita in each Upper-Middle Income Country")
uppermiddle.plot

ggsave("uppermiddle_income_bpp.png", plot = uppermiddle.plot)

#Bounded Upper Middle income countries
uppermiddle.bounded.plot <- ggplot(UMfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  ylim(0,2) +
  geom_smooth(method = loess, formula = y ~ x) + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Upper-Middle Income Country (Bounded)")
uppermiddle.bounded.plot

ggsave("BOUNDED_UM_income_bpp.png", plot = uppermiddle.bounded.plot)

#creating a vector with the names of all high income countries
Hcountry <- c("American Samoa","Andorra","Antigua and Barbuda","Aruba","Australia",
              "Austria","Bahamas","Bahrain","Barbados","Belgium","Bermuda","British Virgin Islands",
              "Brunei Darussalam","Canada","Cayman Islands","Channel Islands","Chile","Croatia",
              "Curacao","Cyprus","Czech Republic","Denmark","Estonia","Faeroe Islands","Finland",
              "France","French Polynesia","Germany","Gibraltar","Greece","Greenland","Guam",
              "Guyana","Hong Kong","Hungary","Iceland","Ireland","Isle of Man","Israel","Italy",
              "Japan","Korea Republic","Kuwait","Latvia","Liechtenstein","Lithuania","Luxembourg",
              "Macao","Malta","Monaco","Nauru","Netherlands","New Caledonia","New Zealand",
              "Northern Mariana Islands","Norway","Oman","Panama","Poland","Portugal","Puerto Rico",
              "Qatar","Romania","San Marino","Saudi Arabia","Seychelles","Singapore","Sint Maarten",
              "Slovak Republic","Slovenia","Spain","St. Kitts and Nevis","St. Martin","Sweden",
              "Switzerland","Taiwan","Trinidad and Tobago","Turks and Caicos","United Arab Emirates",
              "United Kingdom","United States","Uruguay","Virgin Islands")

no.infinity.bpp %>% 
  filter(country %in% unique(Hcountry))-> Highfilter

#create ggplot showing biomass per person for all Low income countries
highincome.plot <- ggplot(Highfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method = loess, formula = y ~ x) + 
  xlab("Year") + ylab("Biomass per one thousand people (kt/thousand)") +
  ggtitle("Biomass Per Capita in each High Income Country")

#ISSUE: all high income countries have "inf" for biomass per person
#CANNOT CREATE GRAPH FOR HIGH-INCOME COUNTRIES

#LINEAR FIT PRACTICE
#LOW INCOME
linear.low.plot <- ggplot(Lowfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  ylim(0,2) +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Low-Income Country")
linear.low.plot

#LOWER MIDDLE INCOME BOUNDED
linear.LM.bounded.plot <- ggplot(LMfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  ylim(0,2) +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Lower-Middle Income Country (Bounded)")
linear.LM.bounded.plot

#UPPER MIDDLE INCOME BOUNDED
linear.UM.bounded.plot <- ggplot(UMfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  ylim(0,2) +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Upper-Middle Income Country (Bounded)")
linear.UM.bounded.plot

#Identifying slope and y-int for all the separate country lines
#Filtering out slopes larger than 0.0015
#Narrow down countries into ones that have horizontal slopes (consistent data)

Lowfilter <- filter(Lowfilter, year < 2023)
LMfilter <- filter(LMfilter, year < 2023)
UMfilter <- filter(UMfilter, year < 2023)

#LOW INCOME
low.summary <- Lowfilter %>% group_by(country) %>% summarise(tidy(lm(bio.per.person ~ year)))
low.slope <- filter(low.summary, term == "year")
horiz.slope.low <- filter(low.slope, abs(estimate) < 0.0015)

#LOWER-MIDDLE INCOME
lowmiddle.summary <- LMfilter %>% group_by(country) %>% summarise(tidy(lm(bio.per.person ~ year)))
LM.slope <- filter(lowmiddle.summary, term == "year")
horiz.slope.LM <- filter(LM.slope, abs(estimate) < 0.0015)

#UPPER-MIDDLE INCOME
uppmiddle.summary <- UMfilter %>% group_by(country) %>% summarise(tidy(lm(bio.per.person ~ year)))
UM.slope <- filter(uppmiddle.summary, term == "year")
horiz.slope.UM <- filter(UM.slope, abs(estimate) < 0.0015)

#GRAPHING COUNTRIES WITH HORIZONTAL SLOPES
#LOW INCOME HORIZONTAL
zerolow <- c("Afghanistan","Niger","South Sudan","Yemen")
no.infinity.bpp %>% 
  filter(country %in% unique(zerolow)) %>%
  filter(year < 2023) -> ZERO.LOW

low.zero.plot <- ggplot(ZERO.LOW, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Low-Income Country")
low.zero.plot

ggsave("horiz.LOW.png", plot = low.zero.plot)

#LOWER MIDDLE HORIZONTAL
zeroLM <- c("Bangladesh","Benin","Cambodia","Samoa")
no.infinity.bpp %>% 
  filter(country %in% unique(zeroLM)) %>%
  filter(year < 2023) -> ZERO.LM

LM.zero.plot <- ggplot(ZERO.LM, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Lower-Middle Income Country")
LM.zero.plot

ggsave("horiz.LM.png", plot = LM.zero.plot)

#UPPER MIDDLE HORIZONTAL
zeroUM <- c("Belize","Cuba","Dominica","Fiji","Libya")
no.infinity.bpp %>% 
  filter(country %in% unique(zeroUM)) %>%
  filter(year < 2023) -> ZERO.UM

UM.zero.plot <- ggplot(ZERO.UM, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Upper-Middle Income Country")
UM.zero.plot

ggsave("horiz.UM.png", plot = UM.zero.plot)

#FINDING THE MEDIAN
#median value of per person biomass
#divide bpp by median to get fraction by year

#Low income countries median bio per person
ZERO.LOW %>%
  dplyr::group_by(country) %>%
  summarize(median = median(bio.per.person)) -> Low.Median

#Lower-middle income countries median bio per person
ZERO.LM %>%
  dplyr::group_by(country) %>%
  summarize(median = median(bio.per.person)) -> LM.Median

#Upper-middle income countries median bio per person
ZERO.UM %>%
  dplyr::group_by(country) %>%
  summarize(median = median(bio.per.person)) -> UM.Median

#Divide slope by median to get fraction by year
#Creating Relative Slopes

#Relative slopes for low-income countries
slope.low.median <- low.slope %>%
  left_join(Low.Median, by = c("country")) %>%
  mutate(slope.med.L = if_else(is.na(estimate), median, estimate / median))

relative.LOW <- slope.low.median %>% filter(!(is.na(median)))

#Relative slopes for lower-middle income countries
slope.LM.median <- LM.slope %>%
  left_join(LM.Median, by = c("country")) %>%
  mutate(slope.med.LM = if_else(is.na(estimate), median, estimate / median))

relative.LM <- slope.LM.median %>% filter(!(is.na(median)))

#Relative slopes for upper-middle income countries
slope.UM.median <- UM.slope %>%
  left_join(UM.Median, by = c("country")) %>%
  mutate(slope.med.UM = if_else(is.na(estimate), median, estimate / median))

relative.UM <- slope.UM.median %>% filter(!(is.na(median)))

#USING RELATIVE SLOPES AS BASELINE
#LOW INCOME
R.low.slope <- filter(low.summary, term == "year")
R.horiz.slope.low <- filter(R.low.slope, abs(estimate) < 0.007)

R.zerolow <- c("Afghanistan","Eritrea","Ethiopia","Madagascar","Niger", "South Sudan","Togo", "Uganda","Yemen")
no.infinity.bpp %>% 
  filter(country %in% unique(R.zerolow)) %>%
  filter(year < 2023) -> REL.ZERO.LOW

REL.low.zero.plot <- ggplot(REL.ZERO.LOW, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass Per Person (t/person)") +
  ggtitle("Biomass Per Person Using Biomass in Low Income Countries")
REL.low.zero.plot

ggsave("1REL.low.zero.png", plot = REL.low.zero.plot)

#LOWER-MIDDLE INCOME
R.LM.slope <- filter(lowmiddle.summary, term == "year")
R.horiz.slope.LM <- filter(R.LM.slope, abs(estimate) < 0.004)

R.zeroLM <- c("Bangladesh","Benin","Cambodia", "Ghana","Haiti","Kiribati",
              "Lebanon","Mongolia","Nigeria","Papua New Guinea","Philippines",
              "Samoa","Senegal","Solomon Islands","Timor-Leste","Vanuatu",
              "Zimbabwe")
no.infinity.bpp %>% 
  filter(country %in% unique(R.zeroLM))%>%
  filter(year < 2023) -> REL.ZERO.LM

REL.LM.zero.plot <- ggplot(REL.ZERO.LM, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass Per Person (t/person)") +
  ggtitle("Biomass Per Person Using Biomass in Lower-Middle Income Countries")
REL.LM.zero.plot

ggsave("1REL.LM.zero.png", plot = REL.LM.zero.plot)

#UPPER-MIDDLE INCOME
R.UM.slope <- filter(uppmiddle.summary, term == "year")
R.horiz.slope.UM <- filter(UM.slope, abs(estimate) < 0.0037)

R.zeroUM <- c("Belize","China","Cuba","Dominica","Fiji","Indonesia","Libya")
no.infinity.bpp %>% 
  filter(country %in% unique(R.zeroUM))%>%
  filter(year < 2023) -> REL.ZERO.UM

REL.UM.zero.plot <- ggplot(REL.ZERO.UM, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass Per Person (t/person)") +
  ggtitle("Biomass Per Person Using Biomass in Upper-Middle Income Countries")
REL.UM.zero.plot

ggsave("1REL.UM.zero.png", plot = REL.UM.zero.plot)

#----------------------------------------------------------------------------------------------------------------------------------
#Literature VS. Calculations Chart
lit.calc <- read.csv("calc.versus.lit.csv")

#Rename
lit.calc.names <- lit.calc %>%
  rename(literature = lit.tpp.mean,
         calculated = calc.tpp.mean)

#Convert wide format to long format
data_long <- lit.calc.names %>%
  pivot_longer(cols = c(literature, calculated), names_to = "tpp", values_to = "Value")

lit.calc.plot <- ggplot(data_long, aes(x = country, y = Value, color = tpp, group = tpp)) +
  geom_point(aes(shape = tpp, color = tpp), size = 3) +
  labs(title = "Literature vs. Calculated Biomass Estimates",
       x = "Country",
       y = "Mean Biomass Consumption (t/person/year)",
       color = "tpp")
lit.calc.plot
ggsave("circle.triangle.png", plot = lit.calc.plot, width = 13, height = 5)

#Jitter plot screened for only countries with horizontal slopes
#all three income groups combined
all.horiz <- c("Afghanistan","Bangladesh","Belize","Benin","Cambodia","China","Cuba","Dominica","Eritrea","Ethiopia","Fiji",
               "Ghana","Haiti","Indonesia","Kiribati","Lebanon","Libya","Madagascar","Mongolia","Niger","Nigeria",
               "Papua New Guinea","Philippines","Samoa","Senegal","Solomon Islands","South Sudan","Timor-Leste","Togo",
               "Uganda","Vanuatu","Yemen","Zimbabwe")
no.infinity.bpp %>% 
  filter(country %in% unique(all.horiz)) %>%
  filter(year < 2023) -> ALL.COUNTRY.HORIZ

average.biomass <- ALL.COUNTRY.HORIZ %>%
  group_by(iso) %>%
  summarise(ave.bio = mean(bio.per.person, na.rm = TRUE))

#Creating scatterplot using jitter and adding a boxplot
ALL.AVE.jitter <- ggplot(average.biomass, aes(x = year, y = ave.bio)) +
  geom_point(aes(color = ave.bio), size = 3, position = "jitter") +
  labs(title = "Per Person Cooking Biofuel Consumption",
       x = "units",
       y = "Average Biomass Consumption (t/person)",
       color = "ave.bio")
ALL.AVE.jitter
ggsave("all.average.jitter.png", plot = ALL.AVE.jitter)

#add the charcoal fraction (charcoal/(charcoal + all other biomass) 
#from Stoner to the data and then use that for the color scale
average.bio.excel <- read.csv("average.bio.csv")
average.biomass$dmy.x <- 1
NEW.all.average <- ggplot(average.bio.excel, aes(x = dmy.x, y = ave.bio)) +
  geom_boxplot() +
  geom_point(aes(color = ave.bio), size = 3, position = "jitter") +
  labs(title = "Per Person Cooking Biofuel Consumption",
       y = "Average Biomass Consumption (t/person)",
       color = "ave.bio")
NEW.all.average
ggsave("NEW.all.average.jitter.png", plot = NEW.all.average)

#-----------------------------------------------------------------------------------------------------------------------------------
#RUN AFTER SCRIPT 4
#Fraction of biomass use that comes from charcoal
charcoal.fraction <- coal.bio.total.ppl %>%
  left_join(coal.noNA, by = c("year","country")) %>%
  mutate(coal.frac = if_else(is.na(ppl.using.combined), charcoal.use, charcoal.use / ppl.using.combined)) %>%
  filter(!(is.na(ppl.using.combined))) %>%
  filter(!(is.na(coal.frac)))

average.charcoal <- charcoal.fraction %>%
  group_by(iso, UN_code) %>%
  summarise(ave.char.frac = mean(coal.frac, na.rm = TRUE))

BIO.plus.COAL <- average.charcoal %>%
  left_join(average.bio.excel, by = c("iso")) %>%
  filter(!(is.na(ave.bio)))

biofuel.w.coal <- ggplot(BIO.plus.COAL, aes(x = dmy.x, y = ave.bio)) +
  geom_boxplot() +
  geom_point(aes(color = ave.char.frac), size = 3, position = "jitter") +
  labs(title = "Per Person Cooking Biofuel Consumption",
       y = "Average Biomass Consumption (t/person)",
       color = "Average % Charcoal")
biofuel.w.coal
ggsave("biofuel.w.coal.png", plot = biofuel.w.coal)

#Remove boxplot
biofuel.w.coal.no.box <- ggplot(BIO.plus.COAL, aes(x = dmy.x, y = ave.bio)) +
  geom_point(aes(color = ave.char.frac), size = 3, position = "jitter") +
  labs(title = "Per Person Cooking Biofuel Consumption",
       y = "Average Biomass Consumption (t/person)",
       color = "Average % Charcoal")
biofuel.w.coal.no.box
ggsave("no.boxplot.biofuel.w.coal.png", plot = biofuel.w.coal.no.box)

#Adding literature data
calc.vs.lit2 <- read.csv("calc.vs.lit2.csv")

lit.plus.bio.coal <- calc.vs.lit2 %>%
  full_join(BIO.plus.COAL, by = c("iso")) %>%
  select(iso, lit.tpp.mean, ave.char.frac, ave.bio, dmy.x) %>%
  rename(literature = lit.tpp.mean,
         calculated = ave.bio)

# Convert wide format to long format
long.lit.bio.coal <- lit.plus.bio.coal %>%
  pivot_longer(cols = c(literature, calculated), names_to = "Biomass", values_to = "Value") %>% 
  mutate(ave.char.frac = if_else(Biomass == "literature", NA, ave.char.frac))

#Changing dmy.x to 1 for literature and 2 for calculated
long.lit.bio.coal$dmy.x[seq(from = 2, to = nrow(long.lit.bio.coal), by = 2)] <- 1.2

#Average Biomass Consumption (t/person)
#With Average % Charcoal
#Comparing calculated and literature values
#x-axis label removed
lit.bio.coal.PLOT <- ggplot(long.lit.bio.coal, aes(x = dmy.x, y = Value, color = Biomass, group = Biomass)) +
  geom_point(aes(shape = Biomass, color = ave.char.frac), size = 3, position = "jitter") +
  theme(axis.text.x = element_blank()) +
  labs(title = "Per Person Cooking Biofuel Consumption",
       x = element_blank(),
       y = "Average Biomass Consumption (t/person)",
       color = "Average % Charcoal")

#Jittered separately
#circle = calculated
#triangle = literature
#FINAL GRAPH
lit.bio.coal.PLOT <- ggplot(long.lit.bio.coal, aes(x = dmy.x, y = Value)) +
  geom_point(
    aes(shape = Biomass, color = ave.char.frac), 
    size = 3, 
    position = position_jitterdodge(jitter.width = 0.06, dodge.width = 0.3)) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Per Person Cooking Biofuel Consumption",
    x = NULL,
    y = "Average Biomass Consumption (t/person)",
    color = "Average % Charcoal",
    shape = "Biomass")

lit.bio.coal.PLOT

ggsave("lit.bio.and.coal.png", plot = lit.bio.coal.PLOT)

#Assigning average % charcoal to the x-axis
lbc.PLOT <- ggplot(long.lit.bio.coal, aes(x = ave.char.frac, y = Value, color = Biomass, group = Biomass)) +
  geom_point(aes(shape = Biomass, color = ave.char.frac), size = 3, position = "jitter") +
  labs(title = "Per Person Cooking Biofuel Consumption",
       y = "Average Biomass Consumption (t/person)",
       color = "Average % Charcoal")
lbc.PLOT
ggsave("lbc.plot.png", plot = lbc.PLOT)
