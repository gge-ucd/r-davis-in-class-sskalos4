#For the homework this week, we are going to be combining a lot of the packages we have learned the past three weeks: dplyr, ggplot, lubridate, and functions.

#SPOILER: You are currently looking at an RMarkdown document! If you are curious about what RMarkdown is and what we are going to be covering next week, check out the lesson “Literate Programming with RMarkdown” on the website

################################# Part 1 ######################################

#Download a new American River data set using this piece of code:

library(tidyverse)
library(lubridate)
library(ggplot2)

am_riv <- read_csv("https://gge-ucd.github.io/R-DAVIS/data/2015_NFA_solinst_08_05.csv", skip = 13)

#should have a data frame with 35,038 obs of 5 variables

#Make a datetime column by using paste to combine the date and time columns; remember to convert it to a datetime!

am_riv$datetime <- paste(am_riv$Date, " ", am_riv$Time, sep = "")

glimpse(am_riv) # check that datetime column was added

am_riv$datetime <- ymd_hms(am_riv$datetime, tz = "America/Los_Angeles")

glimpse(am_riv) # now you see the correct format for the datetime column as "dttm"

#Calculate the weekly mean, max, and min water temperatures and plot as a point plot (all on the same graph)

am_riv_calc <- am_riv %>% 
  mutate(which_week = week(datetime)) %>% 
  group_by(which_week) %>% 
  summarise(avg_temp = mean(am_riv$Temperature), max_temp = max(am_riv$Temperature), min_temp = min   (am_riv$Temperature)) 

#this code below isn't working correctly for some reason...
am_riv_calc %>% ggplot()+
  geom_point(aes(x = am_riv_calc$which_week, y = am_riv_calc$avg_temp), size = 3, color = "green")+
  geom_point(aes(x = am_riv_calc$which_week, y = am_riv_calc$max_temp), size = 3, color = "red")+
  geom_point(aes(x = am_riv_calc$which_week, y = am_riv_calc$min_temp), size = 3, color = "blue")

#Calculate the hourly mean Level for April through June and make a line plot (y axis should be the hourly mean level, x axis should be datetime)

am_riv$month <- format(as.Date(am_riv$Date), "%m") #extracted month from datetime and created a new column for month
glimpse(am_riv)

am_riv_level <- am_riv %>% 
  filter(month < "04"| month > "07") %>% 
  mutate(which_hour = hour(datetime)) %>%
  group_by(which_hour) %>% 
  summarise(avg_level = mean(am_riv$Level))

am_riv_level %>% ggplot()+
  geom_line(aes(x = am_riv$datetime, y = am_riv_level$avg_level), color = "green")

##################### Part 2 ##########################

#Use the mloa_2001 data set (if you don’t have it, download the .rda file from the resources tab on the website). Remeber to remove the NAs (-99 and -999) and to create a datetime column (we did this in class).

load("Data/mauna_loa_met_2001_minute.rda") #must use load function for .rda files

summary(mloa_2001)

mloa_2001$datetime <- paste0(mloa_2001$year, "-", mloa_2001$month, "-", mloa_2001$day, " ", mloa_2001$hour24, ":", mloa_2001$min) #this creates a new column for datetime like before

glimpse(mloa_2001)

# now we need to change this column to the correct dttm format

mloa_2001$datetime <- ymd_hm(mloa_2001$datetime) #this changes to correct format and assigns that to the entire dataframe
glimpse(mloa_2001)

# filter out -99 and -999 like in class

new_mloa <- mloa_2001 %>% 
  filter(rel_humid != -99, rel_humid != -999) %>% 
  filter(temp_C_2m != -99, temp_C_2m != -999) %>% 
  filter(windSpeed_m_s != -99, windSpeed_m_s != -999) 

summary(new_mloa)

#Then, write a function called plot_temp that returns a graph of the temp_C_2m for a single month. The x-axis of the graph should be pulled from a datetime column (so if your data set does not already have a datetime column, you’ll need to create one!)

plot_temp <- function(monthtoplot, dat = new_mloa){
  df <- filter(new_mloa, month == monthtoplot)
  plot <- ggplot(df, aes(temp_C_2m, month))+
    geom_line()
  return(plot)
}
plot_temp('1') #this doesn't work, I don't know why!?

#Hint! Take a look at the Challenge problem at the bottom of the functions lesson 
#(https://gge-ucd.github.io/R-DAVIS/lesson_functions.html) to figure out how to feed a function a #dataframe

##Challenge from lecture  

#Write a new function that takes two arguments, the gapminder data.frame and the name of a country, and plots the change in the country’s population over time. That is, the return value from the function should be a ggplot object. - It is often easier to modify existing code than to start from scratch. Feel free to start with the calcGDP function code.
  
plotPopGrowth <- function(countrytoplot, dat = gapminder) {
    df <- filter(dat, country == countrytoplot) 
    plot <- ggplot(df, aes(year, pop)) + 
      geom_line()
    return(plot)
  }
plotPopGrowth('Canada')


