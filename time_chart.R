library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(usmap)
library(scales)

incareration_trends <- read_csv("incarceration_trends.csv")

###
new_york_county <- filter(incareration_trends, county_name == "New York County")
###
new_york_county_black <- select(new_york_county, year, total_jail_pop,
                                black_jail_pop,white_jail_pop)
new_york_county_black <- mutate(new_york_county_black, black_white_ratio = 
                                  black_jail_pop/white_jail_pop)
new_york_county_black <- filter(new_york_county_black,year >= 1985)
ggplot(new_york_county_black, aes(x=year, y=black_white_ratio)) +
  geom_line(aes(color = "Black White Ratio")) +
  geom_point()+
  labs(title = "New York County Race Ratio Change (1985-2018)",
       x="Year", y = "Black and White Ratio", color = "Legend")
