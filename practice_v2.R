rm(list=ls())
library(readr)
library(tidyverse)
library(gdata)
library(lubridate)
options(stringsAsFactors = F)
Sys.setlocale(locale = "UTF-8")
raw <- read.csv("D:/R_programming/Journalism_homework/NTUSC/practice.csv")
rawdata <- raw %>%
  mutate(end = ymd_hm(end), start = ymd_hm(start), taketime = as.numeric((end - start), units = "mins"), gawy = end - start, come = ymd_hm(come), go = ymd_hm(go))
rawdata %>% View()
colnames(rawdata)

a0 <- rawdata$user
a1 <- rawdata$start
a2 <- rawdata$end
a3 <- rawdata$material
a4 <- rawdata$come
a5 <- rawdata$go
df <- data.frame(x = c(a0,a0), y = c(a1,a2), z = c(a3,a3), w = c(a4,a5))

ggplot(df, aes(x="student", y=y, color = z, group = z)) +  
  geom_point() + geom_line(aes(group = z)) +
  facet_grid(x~.) + 
  coord_flip() +
  geom_point(aes(x="student", y = w), shape = 8, col = "red")