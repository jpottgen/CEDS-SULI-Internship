#Jory Pottgen, CEDS

#Goal: IEA residential biomass for cooking / population
#End Result: Per Capita Biomass Use for each Country or Region

#Load in CEDSdata
#ceds_tot_final is in 1000s
#Multiply decimals by 1000 to get biomass consumption for cooking in tons
#Biomass units = Kilotonnes (kt) 
total.consumpt <- CEDSdata %>%
  mutate(consumpt = ceds_tot_final * 1000)

#biomass.population is the population of people using traditional biomass
#consumpt is the total biomass consumption
#dividing the total biomass consumption by the total population results in per capita
incorrect.per.cap <- noNA.poptotals %>%
  left_join(total.consumpt, by = c("year","iso")) %>%
  mutate(percap = if_else(is.na(consumpt), biomass.population, consumpt / biomass.population))

#percap units: tons per person

#REVISED PLAN
#Estimating the amount of biomass used per person that uses biomass
#Calculation: Residential biomass consumption / number of people using biomass
#number of people using biomass = total population * fraction of population using biomass

#total population will come from variable "pop" in "popdata"
#fraction of population using biomass will come from "CentPopPercent" in Stoner et al data
#"ceds_tot_final" column is total residential biomass in kt (kilo-tonnes)

#CentPopPercent = prop of pop with primary reliance on CLEAN fuels for cooking
#subtract percent from 100 to get NON-CLEAN fuels or biomass
#divide percent by 100 to get a fraction or proportion
stoner.nonclean <- Stonerdata %>%
  mutate(nonclean.prop = (100 - CentPopPercent)/100)

#multiply pop by prop of people using non-clean biomass to get total pop using biomass
#pop = 1000s
total.pop.using.bio <- stoner.nonclean %>%
  left_join(popdata, by = c("year","country")) %>%
  mutate(ppl.biomass = if_else(is.na(nonclean.prop), pop, pop * nonclean.prop))

total.pop.using.bio

#biomass per person that uses biomass
#ppl.biomass = 1000s
#ceds_tot_final = kilo-tonnes (kt)
#bio.per.person = kt/1000s of people
biomass.per.cap <- CEDSdata %>%
  left_join(total.pop.using.bio, by = c("year","iso")) %>%
  mutate(bio.per.person = if_else(is.na(ceds_tot_final), ppl.biomass, ceds_tot_final / ppl.biomass))

#Removing NA 
#Keeping only data that has a value for bio.per.person
noNA.bio.per.person <- biomass.per.cap %>% filter(!(is.na(bio.per.person)))

TOTALS.bpp <- filter(noNA.bio.per.person, Dim1 == "Total")

#ggplot with points and bounded at max 500 bio.per.person
library(ggplot2)
bio.per.person.plot <- ggplot(TOTALS.bpp, aes(x = year, y = bio.per.person, color = country)) +
  geom_point(show.legend = FALSE) +
  ylim(0,10) + xlab("Year") + ylab("Biomass per one thousand people (kt/thousand)") +
  ggtitle("Biomass Per Capita in each Country")
bio.per.person.plot

#Focusing in on specific parts of the graph
#identifying some potential outliers
unique(df$country)
greaterten <- TOTALS.bpp %>%
  filter(bio.per.person > 10)
unique(greaterten$country)
summary(greaterten$bio.per.person)

#Results most likely due to a lack of data in these countries (ouliers)
belowpointohfive <- TOTALS.bpp %>%
  filter(bio.per.person < 0.05)
unique(belowpointohfive$country)
summary(belowpointohfive$bio.per.person)

betweenone.twopointfive <- TOTALS.bpp %>%
  filter(between (bio.per.person, 1,2.5))
unique(betweenone.twopointfive$country)
summary(betweenone.twopointfive$bio.per.person)

#Take out infinity
no.infinity.bpp <- TOTALS.bpp %>% filter(!(is.infinite(bio.per.person)))

#Practice Group By Function
max.min <- no.infinity.bpp %>%
  group_by(country) %>%
  mutate(maximum = max(bio.per.person)) %>%
  mutate(minimum = min(bio.per.person))

#NOTES
#group_by()
#mutate for max and min
#per person numbers for cooking use
#compare heating and cooking use
#figure out units and add to graph

#Filtering for five different A countries
target <- c("Angola", "Albania", "Argentina", "Armenia", "Australia")
A.countries <- filter(TOTALS.bpp, country %in% target)
A.countries

#Creating 5 graphs (one for each country)
#plotting total biomass per person between 2000 and 2022
AAAAA <- ggplot(A.countries, aes(x = year, y = bio.per.person, color = country)) +
  geom_point() +
  facet_wrap(~country) +
  geom_smooth(method = loess, formula = y ~ x)
AAAAA

#Attempt to facet wrap by country
bio.per.person.plot <- ggplot(no.infinity.bpp, aes(x = year, y = bio.per.person, color = country)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~country)
plot(bio.per.person.plot)

#Facet wrap by year
bio.per.person.plot <- ggplot(no.infinity.bpp, aes(x = year, y = bio.per.person, color = country)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~year)
plot(bio.per.person.plot)
ggsave("wrap.by.year.bpp.png", plot = bio.per.person.plot)

#Facet wrap by year, max y = 100 bio.per.person
bio.per.person.plot <- ggplot(no.infinity.bpp, aes(x = year, y = bio.per.person, color = country)) +
  geom_point(show.legend = FALSE) +
  ylim(0,100) +
  facet_wrap(~year)
plot(bio.per.person.plot)

ggsave("bio.per.person.plot.png", plot = bio.per.person.plot)


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


#ISSUE: all high income countries have "inf" for biomass per person
#CANNOT CREATE GRAPH
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
highincome.plot

ggsave("high_income_bpp.png", plot = highincome.plot)

#automatically sort ones that are consistent
#dpylr to create a linear fit for each country
#get slope and intercept as two parameters and median value of per person biomass
#slope is in absolute units - biomass per person per year
#screen for lines that have a relatively flat slope
#linear fits, screen for flat line countries, pull out for biomass per person cooking fuel
#email the steps to do that in R
#simple linear fit, get rid of non-flat ones
#use screening that is in absolute value (some range of slope) = biased
#turn that into something more robust with a relative slope
#divide slope by median to get fraction by year
#metric for relative to use as a slope
#end up with table that has one number for country (average bpp for each country that is fairly flat)
#vary with income or region
#values from the literature and come to conclusion (too high or too low)
#understand the variance between countries
#estimate cooking and subtract from IEA and get the heating data
#compare to see if heating matches up with estimates

#PRACTICING LINEAR FIT
#LOW INCOME
linear.low.plot <- ggplot(Lowfilter, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
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

#PRACTICE EXTRACTING SLOPE & Y-INT
model.L <- lm(bio.per.person ~ year, data = Lowfilter)
coef.L <- coef(model.L)
coef.L

#EXAMPLE
#This code takes my data, then groups by two columns Non.CO2 and year. 
#Now when I do operations it will do those using those groups. 
#In this case I'm generating total emissions EDGAR_total
#It will give me totals by year and Non.CO2. (Non.CO2 is the emission species in this case.)
#So in your case, if you group by iso (or region), then apply the linear model within a dplyr pipe.
dplyr::group_by(Non.CO2, year) %>%
  dplyr::summarise(EDGAR_total = sum(value, na.rm = TRUE)) -> EDGAR_Global_Em

#REDDIT EXAMPLE
library(dplyr)
library(broom)

mtcars%>%group_by(cyl)%>%summarise(tidy(lm(mpg ~ wt)))

#MY PRACTICE
#LOW-INCOME
#Identifying slope and y-int for all the separate country lines
low_income_model <- Lowfilter %>%
  dplyr::group_by(country) %>%
  summarise(slope = lm(bio.per.person ~ year)) -> low_slope

library(broom)

low.summary <- Lowfilter %>% group_by(country) %>% summarise(tidy(lm(bio.per.person ~ year)))

#Filtering out slopes larger than 0.0015
#Narrow down countries into ones that have horizontal slopes (consistent data)
#LOW INCOME
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
  filter(country %in% unique(zerolow))-> ZERO.LOW

low.zero.plot <- ggplot(ZERO.LOW, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Low-Income Country")
low.zero.plot

ggsave("horiz.LOW.png", plot = low.zero.plot)

#LOWER MIDDLE HORIZONTAL
zeroLM <- c("Bangladesh","Benin","Cambodia","Samoa")
no.infinity.bpp %>% 
  filter(country %in% unique(zeroLM))-> ZERO.LM

LM.zero.plot <- ggplot(ZERO.LM, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass per person (t/person)") +
  ggtitle("Biomass Per Capita in each Lower-Middle Income Country")
LM.zero.plot

ggsave("horiz.LM.png", plot = LM.zero.plot)

#UPPER MIDDLE HORIZONTAL
zeroUM <- c("Belize","Cuba","Dominica","Fiji","Libya")
no.infinity.bpp %>% 
  filter(country %in% unique(zeroUM))-> ZERO.UM

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
  filter(country %in% unique(R.zerolow))-> REL.ZERO.LOW

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
  filter(country %in% unique(R.zeroLM))-> REL.ZERO.LM

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
  filter(country %in% unique(R.zeroUM))-> REL.ZERO.UM

REL.UM.zero.plot <- ggplot(REL.ZERO.UM, aes(x = year, y = bio.per.person, color = country)) + geom_point() +
  geom_smooth(method="lm") + 
  xlab("Year") + ylab("Biomass Per Person (t/person)") +
  ggtitle("Biomass Per Person Using Biomass in Upper-Middle Income Countries")
REL.UM.zero.plot

ggsave("1REL.UM.zero.png", plot = REL.UM.zero.plot)

#Literature VS. Calculations Chart
lit.calc <- read.csv("calc.versus.lit.csv")

# Load necessary libraries
library(tidyr)
library(ggplot2)

#Rename
lit.calc.names <- lit.calc %>%
  rename(literature = lit.tpp.mean,
         calculated = calc.tpp.mean)

# Convert wide format to long format
data_long <- lit.calc.names %>%
  pivot_longer(cols = c(literature, calculated), names_to = "tpp", values_to = "Value")
# Print the long-format data
print(data_long)

lit.calc.df <- data.frame(
  Country = c("Belize","China","Fiji","Bangladesh","Ghana","Haiti",
  "Nigeria","Papua New Guinea","Eritrea","Ethiopia","South Sudan"),
  Value1 = c(0.1340, 0.7985, 0.7020, 0.7615, 1.1380, 0.6340, 0.2897, 1.0760, 0.0642, 0.8455, 1.6720),
  Value2 = c(0.120, 0.720, 0.345, 0.200, 0.275, 0.525, 0.320, 0.220, 0.250, 0.840, 0.385)
)

lit.calc.plot <- ggplot(data_long, aes(x = country, y = Value, color = tpp, group = tpp)) +
  geom_point(aes(shape = tpp, color = tpp), size = 3) +
  labs(title = "Literature vs. Calculated Biomass Estimates",
       x = "Country",
       y = "Mean Biomass Consumption (t/person/year)",
       color = "tpp")
lit.calc.plot
ggsave("circle.triangle.png", plot = lit.calc.plot, width = 13, height = 5)

#The big thing to do analytically related to this is to put together a summary 
#of the per person cooking biofuel consumption as we have it now. A scatter plot 
#with “jitter” would be fine (basically a plot with everything having the same 
#x axis value). (You could add into that the literature values with a different 
#symbol type.)

#no.infinity.bpp jitter graph
jitter.bpp.plot <- ggplot(no.infinity.bpp, aes(x = units, y = bio.per.person)) +
  geom_point(aes(color = year), size = 3) +
  geom_jitter(aes(color = year), size = 3) +
  labs(title = "Per Person Cooking Biofuel Consumption",
       x = "Country",
       y = "Biomass Consumption (t/person)",
       color = "iso")
jitter.bpp.plot

ggsave("all.data.jitter.png", plot = jitter.bpp.plot)

#Feedback: Yes, although you can just do the average across all years to simply.
#Then you can add the literature data as well. When those are joined you will get 
#NAs where there are no literature data and that should be fine, no points will show up.)
#Although this looks like its not screened for the iso's falling within the slope criteria?

#Jitter plot screened for only countries with horizontal slopes
#all three income groups combined
all.horiz <- c("Afghanistan","Bangladesh","Belize","Benin","Cambodia","China","Cuba","Dominica","Eritrea","Ethiopia","Fiji",
               "Ghana","Haiti","Indonesia","Kiribati","Lebanon","Libya","Madagascar","Mongolia","Niger","Nigeria",
               "Papua New Guinea","Philippines","Samoa","Senegal","Solomon Islands","South Sudan","Timor-Leste","Togo",
               "Uganda","Vanuatu","Yemen","Zimbabwe")
no.infinity.bpp %>% 
  filter(country %in% unique(all.horiz))-> ALL.COUNTRY.HORIZ
ALL.COUNTRY.HORIZ

ALLjitter.bpp.plot <- ggplot(ALL.COUNTRY.HORIZ, aes(x = units, y = bio.per.person)) +
  geom_point(aes(color = year), size = 3) +
  geom_jitter(aes(color = year), size = 3) +
  labs(title = "Per Person Cooking Biofuel Consumption",
       x = "Country",
       y = "Biomass Consumption (t/person)",
       color = "iso")
ALLjitter.bpp.plot

average.biomass <- ALL.COUNTRY.HORIZ %>%
  group_by(iso, units) %>%
  summarise(ave.bio = mean(bio.per.person, na.rm = TRUE))

#Creating scatterplot using jitter and adding a boxplot
#make x a numeric column. Just create a new column in your data
#for example, average.biomass$dmy.x <- 1, and then use dmy.x as your 
#x instead of "units" which isn't (I presume) numeric
#Don't give a label to the x axis.
#Then once you add the charcoal share, use that for the color.
ALL.AVE.jitter <- ggplot(average.biomass, aes(x = units, y = ave.bio)) +
  geom_boxplot() +
  geom_point(aes(color = ave.bio), size = 3, position = "jitter") +
  labs(title = "Per Person Cooking Biofuel Consumption",
       x = "units",
       y = "Average Biomass Consumption (t/person)",
       color = "ave.bio")
ALL.AVE.jitter
ggsave("all.average.jitter.png", plot = ALL.AVE.jitter)

#Analyzing just the boxplot
just.box <- boxplot(ave.bio~units,data=average.biomass, main="Per Person Cooking Biofuel Consumption",
        xlab="Units", ylab="Average Biomass Consumption (t/person)")

#Finding the summary statistics of the boxplot
#Minimum = 0.009696114
#Q1 = 0.215017
#Median = 0.2663594
#Q3 = 0.4273719
#Maximum = 0.8335239
#Mean = 0.3212016
#Standard Deviation = 0.2063315
#n = 33
install.packages("mosaic")
library(mosaic)
fav_stats(average.biomass$ave.bio)

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

#Same plot as biofuel.w.coal but with no boxplot
biofuel.w.coal.no.box <- ggplot(BIO.plus.COAL, aes(x = dmy.x, y = ave.bio)) +
  geom_point(aes(color = ave.char.frac), size = 3, position = "jitter") +
  labs(title = "Per Person Cooking Biofuel Consumption",
       y = "Average Biomass Consumption (t/person)",
       color = "Average % Charcoal")
biofuel.w.coal.no.box
ggsave("no.boxplot.biofuel.w.coal.png", plot = biofuel.w.coal.no.box)

#add in the literature values, they can be a different symbol 
#perhaps make that symbol open, not filled
#That’ll make those data stand out a bit more
#Since those don’t, in general, have fraction of charcoal,
#you can just leave that field NA for now.

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
# Print the long-format data
print(long.lit.bio.coal)

lit.bio.coal.PLOT <- ggplot(long.lit.bio.coal, aes(x = dmy.x, y = Value, color = Biomass, group = Biomass)) +
  geom_point(aes(shape = Biomass, color = ave.char.frac), size = 3, position = "jitter") +
  labs(title = "Per Person Cooking Biofuel Consumption",
       y = "Average Biomass Consumption (t/person)",
       color = "Average % Charcoal")

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

