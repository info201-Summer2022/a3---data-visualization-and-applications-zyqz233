library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(usmap)
library(scales)

incareration_trends <- read_csv("incarceration_trends.csv")

incareration_trends_race <- select(incareration_trends,state,total_prison_pop,
                                   white_prison_pop)
incareration_trends_race <- mutate(incareration_trends_race,
                                   minority_race_prop = 
                                     (total_prison_pop-white_prison_pop)/
                                     total_prison_pop *100)
incareration_trends_race <- filter(incareration_trends_race, !is.na(incareration_trends_race$minority_race_prop))
grouped_race <- group_by(incareration_trends_race, state)
summary_race <- summarize(grouped_race, mean_minority_prop = mean(minority_race_prop))
state_map <- plot_usmap(data = summary_race, values = "mean_minority_prop") +
  labs(title="Mean Minority Proportion in Different State")+
  scale_fill_gradientn(colors = c("springgreen2","springgreen3","springgreen4"),
                       values = rescale(c(0,50,100)))

ggplotly(state_map)
