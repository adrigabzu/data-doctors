
library(tidyverse)
library(skimr)

## Load Data
# https://ourworldindata.org/causes-of-death#causes-of-death-in-recent-decades
causedeath <- read_csv("./workshops/3_data_wrangling/material/annual-number-of-deaths-by-cause.csv", guess_max = 6000)

gdphealth <- read_csv("workshops/3_data_wrangling/material/total-healthcare-expenditure-as-share-of-national-gdp-by-country.csv")

publichealth <- read_csv("workshops/3_data_wrangling/material/public-expenditure-on-healthcare-as-share-of-national-gdp.csv")


## Data fame vs Tibble 
as.data.frame(causedeath)
as_tibble(causedeath)

# Explore data
glimpse(causedeath)
glimpse(gdphealth)
glimpse(publichealth)


library(skimr)
library(psych)

gdphealth %>% skim() %>% print()

## Wide to long dataset
causedeath %>% gather(Cause, Deaths, Dementia:`Terrorism (deaths)`)

# Merge different sources
left_join(causedeath, gdphealth) %>% select(Entity, Year, pct_gdp = `(% of GDP)`) %>% drop_na(pct_gdp)

myresults <- mydata %>%
  select(column1, column2, column3:column10, -column9) %>%
  filter(column1 > 10)

ToothGrowth %>%
  mutate(dose = factor(dose)) %>%
  ggplot(aes(x = dose, y = len, fill = supp)) + geom_col(position = position_dodge()) + 
  theme_minimal()