library(readr)
library(tidyr)
library(plyr)
library(dplyr)
library(magrittr)
library(viridis)
library(lubridate)
library(tidyverse)
library(forecast)
library(quantmod)
library(zoo)
library(ggplot2)

US_COVID_Data <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))

US_COVID_Data <- US_COVID_Data %>% 
  rename(
    COVID_Cases = cases,
    COVID_Deaths = deaths
    ) %>% 
  date = ymd(date)

# California COVID Infections
US_COVID_Data %>% 
  filter(state %in% c("Minnesota", "California")) %>% 
  as_tibble() %>% 
  ggplot(aes(x = date, y = COVID_Cases, group = state, color = state)) + 
  geom_line(aes(y = COVID_Cases)) +
  scale_x_date(date_labels="%b %d",date_breaks  ="3 month") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

# California New Cases
US_COVID_Data %>% 
  filter(state %in% "California") %>% 
  as_tibble() %>% 
  mutate(newCOVID_Cases = ifelse(COVID_Cases - lag(COVID_Cases) < 0, 0, COVID_Cases - lag(COVID_Cases))) %>% 
  ggplot(aes(x = date, y = newCOVID_Cases)) + 
  geom_line(aes(y = newCOVID_Cases), color = "green") +
  labs(x = "Date", y = "New COVID-19 Cases") + 
  scale_x_date(date_labels="%b %d",date_breaks  ="3 month") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

California <- US_COVID_Data %>% 
  filter(state %in% "California") %>% 
  
California %>% 
  as_tibble() %>% 
  ggplot(aes(x = date, y = COVID_Cases)) + 
  geom_line(aes(y = COVID_Cases, color = "green")) +
  labs(x = "Date", y = "COVID-19 Cases") +
  ggtitle("California COVID-19 Cases") 

# California New Cases
US_COVID_Data %>% 
  filter(state %in% "Arizona") %>% 
  as_tibble() %>% 
  mutate(newCOVID_Cases = ifelse(COVID_Cases - lag(COVID_Cases) < 0, 0, COVID_Cases - lag(COVID_Cases))) %>% 
  ggplot(aes(x = date, y = newCOVID_Cases)) + 
  geom_line(aes(y = newCOVID_Cases), color = "green") +
  labs(x = "Date", y = "New COVID-19 Cases") + 
  scale_x_date(date_labels="%b %d",date_breaks  ="3 month") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

# Texas COVID-19 Infection Rate
infections_Pop %>% 
  filter(state %in% "Texas") %>% 
  as_tibble() %>% 
  mutate(newCOVID_Cases = ifelse(COVID_Cases - lag(COVID_Cases) < 0, 0, COVID_Cases - lag(COVID_Cases))) %>% 
  mutate(infectionRate = newCOVID_Cases / POPESTIMATE2020) %>% 
  ggplot(aes(x = date, y = infectionRate)) +
  geom_line(aes(y = infectionRate), color = "blue") + 
  labs(x = "Date", y = "COVID-19 Infection Rate") + 
  scale_x_date(date_labels = "%b %y", date_breaks = "3 month") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

# Minnesota COVID-19 Infection Rate
infections_Pop %>% 
  filter(state %in% "Minnesota") %>% 
  as_tibble() %>% 
  mutate(newCOVID_Cases = ifelse(COVID_Cases - lag(COVID_Cases) < 0, 0, COVID_Cases - lag(COVID_Cases))) %>% 
  mutate(infectionRate = newCOVID_Cases / POPESTIMATE2020) %>% 
  ggplot(aes(x = date, y = infectionRate)) +
  geom_line(aes(y = infectionRate), color = "blue") + 
  labs(x = "Date", y = "COVID-19 Infection Rate") + 
  scale_x_date(date_labels = "%b %y", date_breaks = "3 month") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

# California COVID-19 Deaths
infections_Pop %>% 
  filter(state %in% "California") %>% 
  as_tibble() %>% 
  mutate(newDeaths = ifelse(COVID_Deaths - lag(COVID_Deaths) < 0, 0, COVID_Deaths - lag(COVID_Deaths))) %>% 
  ggplot(aes(x = date, y = newDeaths)) +
  geom_line(aes(y = newDeaths), color = "blue") + 
  labs(x = "Date", y = "COVID-19 Deaths") + 
  ggtitle("California COVID-19 New Deaths") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 month") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

# California COVID-19 Death Rate
infections_Pop %>% 
  filter(state %in% "California") %>% 
  as_tibble() %>% 
  mutate(newDeaths = ifelse(COVID_Deaths - lag(COVID_Deaths) < 0, 0, COVID_Deaths - lag(COVID_Deaths))) %>% 
  mutate(deathRate = newDeaths/POPESTIMATE2020) %>% 
  ggplot(aes(x = date, y = deathRate)) +
  geom_line(aes(y = deathRate), color = "blue") + 
  labs(x = "Date", y = "COVID-19 Death Rate") + 
  ggtitle("California COVID-19 Death Rate") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 month") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

# Minnesota COVID-19 Deaths
infections_Pop %>% 
  filter(state %in% "Minnesota") %>% 
  as_tibble() %>% 
  mutate(newDeaths = ifelse(COVID_Deaths - lag(COVID_Deaths) < 0, 0, COVID_Deaths - lag(COVID_Deaths))) %>% 
  ggplot(aes(x = date, y = newDeaths)) +
  geom_line(aes(y = newDeaths), color = "blue") + 
  labs(x = "Date", y = "COVID-19 Deaths") + 
  ggtitle("Minnesota COVID-19 New Deaths") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 month") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

# Minnesota COVID-19 Death Rate
infections_Pop %>% 
  filter(state %in% "Minnesota") %>% 
  as_tibble() %>% 
  mutate(newDeaths = ifelse(COVID_Deaths - lag(COVID_Deaths) < 0, 0, COVID_Deaths - lag(COVID_Deaths))) %>% 
  mutate(deathRate = newDeaths/POPESTIMATE2020) %>% 
  ggplot(aes(x = date, y = deathRate)) +
  geom_line(aes(y = deathRate), color = "blue") + 
  labs(x = "Date", y = "COVID-19 Death Rate") + 
  ggtitle("Minnesota COVID-19 Death Rate") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 month") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

# Texas COVID-19 Deaths
infections_Pop %>% 
  filter(state %in% "Minnesota") %>% 
  as_tibble() %>% 
  mutate(newDeaths = ifelse(COVID_Deaths - lag(COVID_Deaths) < 0, 0, COVID_Deaths - lag(COVID_Deaths))) %>% 
  ggplot(aes(x = date, y = newDeaths)) +
  geom_line(aes(y = newDeaths), color = "blue") + 
  geom_line(aes(y = rollmean(newDeaths, 7, na.pad = TRUE)), color = "green") + 
  labs(x = "Date", y = "COVID-19 Deaths") + 
  ggtitle("Texas COVID-19 New Deaths") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 month") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

# Texas COVID-19 Death Rate
infections_Pop %>% 
  filter(state %in% "Texas") %>% 
  as_tibble() %>% 
  mutate(newDeaths = ifelse(COVID_Deaths - lag(COVID_Deaths) < 0, 0, COVID_Deaths - lag(COVID_Deaths))) %>% 
  mutate(deathRate = newDeaths/POPESTIMATE2020) %>% 
  ggplot(aes(x = date, y = deathRate)) +
  geom_line(aes(y = deathRate), color = "blue") + 
  labs(x = "Date", y = "COVID-19 Death Rate") + 
  ggtitle("Texas COVID-19 Death Rate") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 month") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray27"),
        panel.background = element_blank()) 

# TS Object Practice
infections_Pop.DeathsTS <- ts(infections_Pop$COVID_Deaths, start = c(2020, 21, 1), end = c(2021, 29, 6), frequency = 365)
plot(infections_Pop.DeathsTS, xlab = "Date", ylab = "COVID-19 Deaths")
abline(h = 0.18)

acf(infections_Pop.DeathsTS)
