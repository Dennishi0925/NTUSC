rm(list=ls())
library(readr)
library(tidyverse)
library(gdata)
options(stringsAsFactors = F)
Sys.setlocale(locale = "UTF-8")
#a <- read.xls(xls ="D:/R_programming/Journalism_homework/NTUSC/106-1_processed.xls", sheet = 1, verbose=FALSE)
rawdata <- read.csv("D:/R_programming/Journalism_homework/NTUSC/106-1_processed.csv")
rawdata %>% View()
#gawy <- function(x, ifinput, ifoutput) {
#  for(i in seq_along(x)) {
#    if(x[i] == ifinput){x[i] = ifoutput}
#  } 
#}
rawdata <- rawdata %>% as_tibble()
for(i in seq_along(rawdata$總體狀況)) {
  if(rawdata$總體狀況[i] == ""){
    rawdata$總體狀況[i] = "current"}
} 
rawdata$X0913狀況[rawdata$X0913狀況 == ""] <- "present"
rawdata$X0918狀況[rawdata$X0918狀況 == ""] <- "present"
rawdata$X0918特別狀況[rawdata$X0918特別狀況 == ""] <- "present"
rawdata$X0925狀況[rawdata$X0925狀況 == ""] <- "present"
rawdata$X1003狀況[rawdata$X1003狀況 == ""] <- "present"
rawdata$X1025狀況[rawdata$X1025狀況 == ""] <- "present"
rawdata$X1123狀況[rawdata$X1123狀況 == ""] <- "present"
rawdata$X1213狀況[rawdata$X1213狀況 == ""] <- "present"
rawdata$X0112狀況[rawdata$X0112狀況 == ""] <- "present"
rawdata$X0130狀況[rawdata$X0130狀況 == ""] <- "present"
colnames(rawdata)[8] <- "general"
rawdata$general[rawdata$general == "resign"] <- "resigned"
rawdata %>% View()
table(rawdata$general)
rawdata[, "X0913狀況"]
rawdata$college <- factor(rawdata$college)
rawdata$dept <- factor(rawdata$dept)
# "X0913狀況","X0130狀況"根據學代會官網不列入紀錄
data_present <- rawdata[, c("X0918狀況","X0918特別狀況","X0925狀況","X1003狀況","X1025狀況","X1123狀況","X1213狀況","X0112狀況")]
data_basic <- rawdata[,c(1:8)]
data_processed <- cbind(data_basic ,data_present)
table(data_processed$general)
data_processed %>% View()
# 來看一些基本背景統計
data_processed %>%
  group_by(college) %>%
  count(sort = T) %>%
  ggplot(aes(x = college, y = n)) +
  geom_col(fill="lightblue", colour="white", width = 0.7) +
  geom_text(aes(y = n + 0.5, label= n), colour="black")

data_processed %>%
  group_by(grade) %>%
  count(sort = T) %>%
  ggplot(aes(x = grade, y = n)) +
  geom_col(fill="lightblue", colour="white", width = 0.7) +
  geom_text(aes(y = n + 1, label= n), colour="black")

data_processed %>%
  group_by(college, grade, dept) %>%
  count(sort = T) %>%
  ggplot(aes(x = college, y = grade, color = dept, size = n)) +
  geom_jitter(show.legend = FALSE, width = 0.3, height = 0) 

data_processed %>%
  #  group_by(college, grade) %>% #, start
  #  count(sort = T) %>%
  ggplot(aes(x = college, fill = factor(grade))) +
  geom_bar(width = 0.5) 

data_processed %>%
  group_by(start, grade) %>%
  count(sort = T) %>%
  ungroup() %>%
  group_by(start) %>%
  mutate(labely = cumsum(n)) %>%
  ggplot(aes(x = start, y = n, fill = factor(grade))) +
  geom_col(width = 0.3) +
  geom_text(position = position_stack(vjust = .5), aes(label= n), size=4) + #aes(label=paste(format(n, nsmall=0), "people"))
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_brewer(palette="Pastel1")
data_processed[data_processed$general == "resigned" | data_processed$general == "disqualified",9:16]  <- NA
data_processed_long <- gather(data_processed, key = "keys", value = "values", X0112狀況:X0918狀況)
data_processed_long %>% View()
# 總體出席
data_processed_long %>%
  group_by(values) %>%
  count()
# 按照人來看出席
data_processed_long %>%
  group_by(name, values) %>%
  count() %>%
  filter(values != "present") %>%
  arrange(desc(n))
# 按照院來看出席
data_processed_long %>%
  filter(general == "current") %>%
  group_by(college) %>%
  count()
a <- data_processed_long %>%
  filter(general == "current") %>%
  group_by(college, values) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup
a$values <- factor(a$values, , levels = c("present", "leave", "skip"))
ggplot(a, aes(x = college, y = percent, fill = values)) +
  geom_bar(stat = "identity") +
  guides(fill=guide_legend())
ggplot(a, aes(x = college, y = percent, fill = values)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_brewer(palette="Pastel1")
# 按照年級來看出席
data_processed_long %>%
  filter(general == "current") %>%
  group_by(grade) %>%
  count()
b <- data_processed_long %>%
  filter(general == "current") %>%
  group_by(grade, values) %>%
  count() %>%
  ungroup %>%
  group_by(grade) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup
#a$values <- factor(a$values, , levels = c("present", "leave", "skip"))
ggplot(b, aes(x = grade, y = percent, fill = values)) +
  geom_bar(stat = "identity") +
  guides(fill=guide_legend())
ggplot(b, aes(x = grade, y = percent, fill = values)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_brewer(palette="Pastel1")
