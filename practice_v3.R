rm(list=ls())
library(readr)
library(tidyverse)
library(gdata)
library(lubridate)
library(data.table)
options(stringsAsFactors = F)
Sys.setlocale(locale = "UTF-8")
raw <- read.csv("D:/R_programming/Journalism_homework/NTUSC/practice.csv")
raw2 <- read.csv("D:/R_programming/Journalism_homework/NTUSC/practice_v2.csv")

rawdata <- raw2 %>%
  mutate(end = ymd_hm(end), start = ymd_hm(start), taketime = as.numeric((end - start), units = "mins"), gawy = end - start, week = week(start))
rawdata %>% View()
colnames(rawdata)

rawdata %>%
  ggplot(mapping = aes(x = factor(user), y = factor(material), height =0.9)) +
  geom_tile(mapping = aes(fill = frequency, color = "white")) +
  #scale_colour_gradient(low = "white", high = "black") +
  facet_grid(week~., scale="free_y") +
  scale_fill_gradient() +
  scale_y_discrete(limits=rev(levels(factor(rawdata$material))),position = "right") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))

