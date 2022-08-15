library(dplyr)
library(ggplot2)
library(plotly)
library(usmap)
incar_trends <- read.csv("incarceration_trends.csv")
year_pop <- read.csv("year-end-prison-2021.csv")
incar_juri <- read.csv("incarceration_trends_jail_jurisdiction.csv")


#Trends over time chart
#This chart serves as comparing prison incarceration population over different states

unorganized_states <- year_pop$state_name
states <- unorganized_states[8 : 57]
unorganized_prison_rate_2019 <- year_pop$total_prison_pop_2019
prison_rate_2019 <- unorganized_prison_rate_2019[8 : 57]
unorganized_prison_rate_2020 <- year_pop$total_prison_pop_2020
prison_rate_2020 <- unorganized_prison_rate_2020[8 : 57]
unorganized_prison_rate_2021 <- year_pop$total_prison_pop_2021
prison_rate_2021 <- unorganized_prison_rate_2021[8 : 57]
states_total <- data.frame(states, prison_rate_2019, prison_rate_2020, prison_rate_2021)

states_prison_rate <- plot_ly() %>% 
  add_trace(data = states_total, x = ~states, y = ~prison_rate_2019, name = "prison pop of 2019", type = "bar") %>% 
  add_trace(states_total, x = ~states, y = ~prison_rate_2020, name = "prison pop of 2020", type = "bar") %>% 
  add_trace(states_total, x = ~states, y = ~prison_rate_2021, name = "prison pop of 2021", type = "bar") %>% 
  layout(title = "prison population in different states between 2019-2021",
         xaxis = list(title = "states"),
         yaxis = list(title = "prison population"))

#I could calculate the average yearly prison population in one state by
ave_pop <- function(state){
  filtered_state <- filter(states_total, states == state)
  pop_2019 <- filtered_state$prison_rate_2019
  pop_2020 <- filtered_state$prison_rate_2020
  pop_2021 <- filtered_state$prison_rate_2021
  total_state_pop <- pop_2019 + pop_2020 +pop_2021
  ave <- total_state_pop/3
  return(ave)
}

#I could calculate which year has the most incarceration population
the_most_incar_year <- function(){
  total_2019 <- sum(states_total$prison_rate_2019)
  total_2020 <- sum(states_total$prison_rate_2020)
  total_2021 <- sum(states_total$prison_rate_2021)
  three_year <- c(total_2019, total_2020, total_2021)
  the_most_incar_pop <- max(three_year)
  if(the_most_incar_pop == total_2019){
    return("2019 has the most incarceration population.")
  } else if(the_most_incar_pop == total_2020){
    return("2020 has the most incarceration population.")
  } else if(the_most_incar_pop == total_2021){
    return("2021 has the most incarceration population.")
  }
}
#2019 has the most incarceration population

#I could calculate which year has the least incarceration population
the_least_incar_year <- function(){
  total_2019 <- sum(states_total$prison_rate_2019)
  total_2020 <- sum(states_total$prison_rate_2020)
  total_2021 <- sum(states_total$prison_rate_2021)
  three_year <- c(total_2019, total_2020, total_2021)
  the_least_incar_pop <- min(three_year)
  if(the_least_incar_pop == total_2019){
    return("2019 has the least incarceration population.")
  } else if(the_least_incar_pop == total_2020){
    return("2020 has the least incarceration population.")
  } else if(the_least_incar_pop == total_2021){
    return("2021 has the least incarceration population.")
  }
}

#Variable comparison chart
#This chart compares incarceration population for age group between 15-64 at the year of 2018 at different counties
state_abb <- incar_trends$state
year <- incar_trends$year
county <- incar_trends$county_name
asian <- incar_trends$aapi_pop_15to64
black <- incar_trends$black_pop_15to64
latinx <- incar_trends$latinx_pop_15to64
native <- incar_trends$native_pop_15to64
white <- incar_trends$white_pop_15to64
unorganized_total_15_to_64 <- data.frame(state_abb, year, county, asian, black, latinx, native, white)
total_15_to_64 <- filter(unorganized_total_15_to_64, year == 2018)

counties_race_incar <- plot_ly() %>% 
  add_trace(data = total_15_to_64, x = ~county, y = ~asian, name = "asian arrested pop in 2018", type = "bar") %>% 
  add_trace(data = total_15_to_64, x = ~county, y = ~black, name = "black arrested pop in 2018", type = "bar") %>% 
  add_trace(data = total_15_to_64, x = ~county, y = ~latinx, name = "latinx arrested pop in 2018", type = "bar") %>% 
  add_trace(data = total_15_to_64, x = ~county, y = ~native, name = "native arrested pop in 2018", type = "bar") %>% 
  add_trace(data = total_15_to_64, x = ~county, y = ~white, name = "white arrested pop in 2018", type = "bar") %>% 
  layout(title = "races arrested population in counties",
         xaxis = list(title = "counties"),
         yaxis = list(title = "prison population"))

#I could check how many people in total are arrested in the county during the year of 2018
arrest_c_2018 <- function(counties, state1){
  filter_county1 <- filter(total_15_to_64, county == counties)
  filter_county <-  filter(filter_county1, state_abb == state1)
  pop_asian <- filter_county$asian
  pop_black <- filter_county$black
  pop_latinx <- filter_county$latinx
  pop_native <- filter_county$native
  pop_white <- filter_county$white
  total_counties <- pop_asian + pop_black + pop_latinx + pop_native + pop_white
  return(total_counties)
}

#I could check how many people in certain race were arrested in the year of 2018
arrest_race_2018 <- function(race){
  if(race == "asian"){
    return(sum(total_15_to_64$asian))
  } else if(race == "black"){
    return(sum(total_15_to_64$black))
  } else if(race == "latinx"){
    return(sum(total_15_to_64$latinx))
  } else if(race == "native"){
    return(sum(total_15_to_64$native))
  } else if(race == "white"){
    return(sum(total_15_to_64$white))
  }
}

#Map
#This map compares the incarceration population of different races in WA at the year of 2018
un_WA <- data.frame(filter(unorganized_total_15_to_64, state_abb == "WA", year == 2018))
new_WA <- data.frame(rep("Washington", nrow(un_WA)))
newest <- cbind(un_WA, new_WA)
colnames(newest)[9] <- "state"
n_WA <- newest %>% 
  select("state", everything())

un_fips <- filter(incar_trends, year == 2018)
un_wa_fips <- filter(un_fips, state == "WA")
fips <- un_wa_fips$fips
WA <- cbind(n_WA, fips)

#Asian map
asian_map <- plot_usmap(data = WA,
                     values = "asian",
                     regions = "counties",
                     labels = TRUE,
                     label_color = "orange",
                     include = c("WA"),
                     color = "black") +
  scale_fill_continuous(low = "light blue", 
                        high = "dark blue",
                        name = "asian incarcerated pop",
                        label = scales::comma) +
  labs(title = "Asian incarcerated population in WA 2008") +
  theme(panel.background = element_rect(color = "black", fill = "white"), 
        legend.position = "right")

#Black map
black_map <- plot_usmap(data = WA,
                        values = "black",
                        regions = "counties",
                        labels = TRUE,
                        label_color = "orange",
                        include = c("WA"),
                        color = "black") +
  scale_fill_continuous(low = "light blue", 
                        high = "dark blue",
                        name = "black incarcerated pop",
                        label = scales::comma) +
  labs(title = "Black incarcerated population in WA 2008") +
  theme(panel.background = element_rect(color = "black", fill = "white"), 
        legend.position = "right")

#Latinx map
latinx_map <- plot_usmap(data = WA,
                        values = "latinx",
                        regions = "counties",
                        labels = TRUE,
                        label_color = "orange",
                        include = c("WA"),
                        color = "black") +
  scale_fill_continuous(low = "light blue", 
                        high = "dark blue",
                        name = "latinx incarcerated pop",
                        label = scales::comma) +
  labs(title = "Latinx incarcerated population in WA 2008") +
  theme(panel.background = element_rect(color = "black", fill = "white"), 
        legend.position = "right")

#Native map
native_map <- plot_usmap(data = WA,
                         values = "native",
                         regions = "counties",
                         labels = TRUE,
                         label_color = "orange",
                         include = c("WA"),
                         color = "black") +
  scale_fill_continuous(low = "light blue", 
                        high = "dark blue",
                        name = "native incarcerated pop",
                        label = scales::comma) +
  labs(title = "Native incarcerated population in WA 2008") +
  theme(panel.background = element_rect(color = "black", fill = "white"), 
        legend.position = "right")

#White map
white_map <- plot_usmap(data = WA,
                         values = "white",
                         regions = "counties",
                         labels = TRUE,
                         label_color = "orange",
                         include = c("WA"),
                         color = "black") +
  scale_fill_continuous(low = "light blue", 
                        high = "dark blue",
                        name = "white incarcerated pop",
                        label = scales::comma) +
  labs(title = "White incarcerated population in WA 2008") +
  theme(panel.background = element_rect(color = "black", fill = "white"), 
        legend.position = "right")
