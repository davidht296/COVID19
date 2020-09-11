# File:    COVID-19 
# Author:  David Harris 
# Date:    10-08-20

# INSTALL AND LOAD PACKAGES ################################

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, dplyr, gganimate, ggthemes, gifski, lubridate, 
               reshape2, rio, tidyverse, transformr,  zoo)  

# Set Working Directory ####################################
getwd()
setwd("C:/Users/david/Desktop/David/PD & Personal/R data analysis/GitHub/COVID19") #*remember to change \ to /

# LOAD DATA ################################################
data <- import("cases_daily_state.csv") # from: https://infogram.com/1p0lp9vmnqd3n9te63x3q090ketnx57evn5?live
data[is.na(data)] <- 0

# WORK WITH DATA ###########################################
data <- data %>% 
  mutate(Date = seq.Date(as.Date("2020/01/25"), as.Date(Sys.Date()), "days")) %>% # set dates to correct format
  mutate(AUS = rowSums(data[,2:9]))

n <- nrow(data)

data.NSW <- data %>% 
  select(Date, NSW) %>%
  mutate(State = rep("NSW", n)) %>% 
  mutate(MS7 = rollsum(NSW, 7, fill = NA, align = "right")) %>% 
  mutate(Cum = cumsum(NSW)) %>%
  filter(Date >= "2020/02/25") %>% 
  .[,c(1,3,2,4,5)] %>% 
  setNames(., c("Date", "State", "New", "MS7", "Cum"))

data.VIC <- data %>% 
  select(Date, VIC) %>%
  mutate(State = rep("VIC", n)) %>% 
  mutate(MS7 = rollsum(VIC, 7, fill = NA, align = "right")) %>% 
  mutate(Cum = cumsum(VIC)) %>%
  filter(Date >= "2020/02/25") %>% 
  .[,c(1,3,2,4,5)] %>% 
  setNames(., c("Date", "State", "New", "MS7", "Cum"))

data.AUS <- data %>% 
  select(Date, AUS) %>%
  mutate(State = rep("AUS", n)) %>% 
  mutate(MS7 = rollsum(AUS, 7, fill = NA, align = "right")) %>% 
  mutate(Cum = cumsum(AUS)) %>%
  filter(Date >= "2020/02/25") %>% 
  .[,c(1,3,2,4,5)] %>% 
  setNames(., c("Date", "State", "New", "MS7", "Cum"))

data.ALL <- bind_rows(data.NSW, data.VIC, data.AUS)

### FIRST WAVE ##############################
data1 <- data.ALL %>%
  filter(Date >= "2020/02/25" & Date <= "2020/06/06")

mycolors <- c("#54A338", "#1F83B4", "#1a476f")

plot1 <- data1 %>%   
  ggplot(.,aes(x=Cum, y=MS7, color=State)) +
  geom_line(size = .8, alpha = .8) +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(colour="#D23E4E", linetype = "longdash") +
  labs(title = "COVID-19 First Wave", subtitle = "Date: {frame_along}",
       x = 'Cumulative cases (Since 25 February 2020)', 
       y = 'New cases (Past 7 days)',
       color = "State") +
  theme_few() +
  scale_color_manual(values = mycolors)
  panel_border(remove = TRUE)

ani1 <- plot1 + 
  transition_reveal(along = Date) + 
  ease_aes('linear')

n <- nrow(data1)/3
animate(ani1, nframes = n, fps = 8, renderer = gifski_renderer())
anim_save("Wave1-ani.gif", ani1, nframe = n, fps = 6, renderer = gifski_renderer())

# Static plot
data1 %>%   
  ggplot(.,aes(x=Cum, y=MS7, color=State)) +
  geom_line(size = .8, alpha = .8) +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(colour="#D23E4E", linetype = "longdash") +
  labs(title = "COVID-19 First Wave", subtitle = "Date: 6 June 2020",
       x = 'Cumulative cases (Since 25 February 2020)', 
       y = 'New cases (Past 7 days)',
       color = "State") +
  theme_few() +
  scale_color_manual(values = mycolors)

### SECOND WAVE #############################
n <- nrow(data)

data.NSW.2 <- data %>% 
  select(Date, NSW) %>%
  mutate(State = rep("NSW", n)) %>%   
  filter(Date >= "2020/06/06") %>% 
  mutate(MS7 = rollsum(NSW, 7, fill = NA, align = "right")) %>% 
  mutate(Cum = cumsum(NSW)) %>%
  .[,c(1,3,2,4,5)] %>% 
  setNames(., c("Date", "State", "New", "MS7", "Cum")) %>% 
  na.omit()

data.VIC.2 <- data %>% 
  select(Date, VIC) %>%
  mutate(State = rep("VIC", n)) %>%   
  filter(Date >= "2020/06/06") %>% 
  mutate(MS7 = rollsum(VIC, 7, fill = NA, align = "right")) %>% 
  mutate(Cum = cumsum(VIC)) %>%
  .[,c(1,3,2,4,5)] %>% 
  setNames(., c("Date", "State", "New", "MS7", "Cum")) %>% 
  na.omit()

data.AUS.2 <- data %>% 
  select(Date, AUS) %>%
  mutate(State = rep("AUS", n)) %>%   
  filter(Date >= "2020/06/06") %>% 
  mutate(MS7 = rollsum(AUS, 7, fill = NA, align = "right")) %>% 
  mutate(Cum = cumsum(AUS)) %>%
  .[,c(1,3,2,4,5)] %>% 
  setNames(., c("Date", "State", "New", "MS7", "Cum")) %>% 
  na.omit()

data2 <- bind_rows(data.NSW.2, data.VIC.2, data.AUS.2)

plot2 <- data2 %>%   
  ggplot(.,aes(x=Cum, y=MS7, color=State)) +
  geom_line(size = .8, alpha = .8) +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(colour="#D23E4E", linetype = "longdash") +
  labs(title = "COVID-19 Second Wave", subtitle = "Date: {frame_along}",
       x = 'Cumulative cases (Since 6 June 2020)', 
       y = 'New cases (Past 7 days)',
       color = "State") +
  theme_few() +
  scale_color_manual(values = mycolors)

ani2 <- plot2 + 
  transition_reveal(along = Date) + 
  ease_aes('linear')

n <- nrow(data2)/3
animate(ani2, nframes = n, fps = 6, renderer = gifski_renderer())
anim_save("Wave2-ani.gif", ani2, nframe = n, fps = 6, renderer = gifski_renderer())

# Static plot
data2 %>%   
  ggplot(.,aes(x=Cum, y=MS7, color=State)) +
  geom_line(size = .8, alpha = .8) +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(colour="#D23E4E", linetype = "longdash") +
  labs(title = "COVID-19 Second Wave", subtitle = "Date: 26 August 2020",
       x = 'Cumulative cases (Since 6 June 2020)', 
       y = 'New cases (Past 7 days)',
       color = "State") +
  theme_few() +
  scale_color_manual(values = mycolors)

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)