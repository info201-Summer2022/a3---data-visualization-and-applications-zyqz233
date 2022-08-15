library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(usmap)
library(scales)

#read data frame

incareration_trends <- read_csv("incarceration_trends.csv")

#delete missing info

incareration_trends_df <- filter(incareration_trends, !is.na(incareration_trends$total_jail_pop_dcrp))

general_pop_info_df <- select(incareration_trends_df, yfips,year,
                              fips,state,county_name,total_pop,total_pop_15to64,
                              female_pop_15to64,male_pop_15to64,aapi_pop_15to64,
                              black_pop_15to64,latinx_pop_15to64,
                              native_pop_15to64,white_pop_15to64,urbanicity,
                              region,division,commuting_zone,metro_area,
                              land_area,total_jail_pop,total_prison_pop)

general_pop_info_df <- mutate(general_pop_info_df,
                              jail_rate = total_jail_pop/total_pop)
general_pop_info_df <- mutate(general_pop_info_df,
                              prison_rate = total_prison_pop/total_pop)
#delete n/a
general_pop_info_df <- filter(general_pop_info_df, !is.na(general_pop_info_df$prison_rate))
general_pop_info_df <- filter(general_pop_info_df, !is.na(general_pop_info_df$jail_rate))

summary_info <- list()
summary_info <- summarize(general_pop_info_df, mean_jail_rate =mean(jail_rate))
summary_info <- summarize(general_pop_info_df, mean_prison_rate =mean(prison_rate))
prop_info_df <- select(incareration_trends,year,fips,total_jail_pop_rate,
                               female_jail_pop_rate,
                               male_jail_pop_rate,aapi_jail_pop_rate,          
                               black_jail_pop_rate,latinx_jail_pop_rate,
                               native_jail_pop_rate,white_jail_pop_rate,
                               total_jail_adm_rate,total_jail_pretrial_rate,    
                               total_prison_pop_rate,female_prison_pop_rate,
                               male_prison_pop_rate,aapi_prison_pop_rate,
                               black_prison_pop_rate,latinx_prison_pop_rate,      
                               native_prison_pop_rate,white_prison_pop_rate,
                               total_prison_adm_rate,female_prison_adm_rate,
                               male_prison_adm_rate,aapi_prison_adm_rate,        
                               black_prison_adm_rate,latinx_prison_adm_rate,
                               native_prison_adm_rate,white_prison_adm_rate)

prop_info_df <- filter(prop_info_df, !is.na(prop_info_df$total_jail_pop_rate))
prop_info_df <- filter(prop_info_df, !is.na(prop_info_df$total_prison_pop_rate))
prop_info_df <- filter(prop_info_df, !is.na(prop_info_df$male_jail_pop_rate))
prop_info_df <- filter(prop_info_df, !is.na(prop_info_df$female_jail_pop_rate))
prop_info_df <- filter(prop_info_df, !is.na(prop_info_df$white_prison_pop_rate))
prop_info_df <- filter(prop_info_df, !is.na(prop_info_df$black_prison_pop_rate))

summary_info <- summarize(prop_info_df,
                          mean_jail_rate =mean(total_jail_pop_rate) * 0.01,
                          mean_prison_rate =mean(total_prison_pop_rate)* 0.01,
                          mean_male_rate =mean(male_jail_pop_rate)* 0.01,
                          mean_female_rate =mean(female_jail_pop_rate)* 0.01,
                          mean_white_prison_rate =mean(white_prison_pop_rate)* 0.01,
                          mean_black_prison_rate =mean(black_prison_pop_rate)* 0.01)

###
new_york_county <- filter(incareration_trends, county_name == "New York County")
###
new_york_county_black <- select(new_york_county, year, total_jail_pop,
                                  black_jail_pop,white_jail_pop)
new_york_county_black <- mutate(new_york_county_black, black_white_ratio = 
                                  black_jail_pop/white_jail_pop)
new_york_county_black <- filter(new_york_county_black,year >= 1985)
time <- ggplot(new_york_county_black, aes(x=year, y=black_white_ratio)) +
  geom_line(aes(color = "Black White Ratio")) +
  geom_point()+
  labs(title = "New York County Race Ratio Change(1985-2018)",
       x="Year", y = "Black and White Ratio", color = "Legend")


##
new_york_county_gender <- select(new_york_county,year, female_prison_pop_rate,
                                 male_prison_pop_rate)
new_york_county_gender <- filter(new_york_county_gender,year >= 1992)

compare <- ggplot(new_york_county_gender, aes(x=year)) +
  geom_line(aes(y=male_prison_pop_rate*0.01,color = "male"), size = 1.5) +
  geom_line(aes(y=female_prison_pop_rate*0.01,color = "female"), size = 1.5) +
  labs(title="New York County Population Rate Changing in prison of Sex (1992-2016)",
       x="Year", y="Population Rate(%)", color = "Sex")
##

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
  labs(title="Mean Minority Propotion in Different State")+
  scale_fill_gradientn(colors = c("springgreen2","springgreen3","springgreen4"),
                       values = rescale(c(0,50,100)))

ggplotly(state_map)
