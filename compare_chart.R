library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(usmap)
library(scales)

incareration_trends <- read_csv("incarceration_trends.csv")
##
new_york_county_gender <- select(new_york_county,year, female_prison_pop_rate,
                                 male_prison_pop_rate)
new_york_county_gender <- filter(new_york_county_gender,year >= 1992)

ggplot(new_york_county_gender, aes(x=year)) +
  geom_line(aes(y=male_prison_pop_rate*0.01,color = "male"), size = 1.5) +
  geom_line(aes(y=female_prison_pop_rate*0.01,color = "female"), size = 1.5) +
  labs(title="New York County Population Rate Changing in prison of Sex (1992-2016)",
       x="Year", y="Population Rate(%)", color = "Sex")
