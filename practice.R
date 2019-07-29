rm(list=ls())
library(readr)
library(tidyverse)
library(gdata)
library(lubridate)
options(stringsAsFactors = F)
Sys.setlocale(locale = "UTF-8")
#a <- read.xls(xls ="D:/R_programming/Journalism_homework/NTUSC/106-1_processed.xls", sheet = 1, verbose=FALSE)
raw <- read.csv("D:/R_programming/Journalism_homework/NTUSC/practice.csv")
rawdata %>% View()
#gawy <- function(x, ifinput, ifoutput) {
#  for(i in seq_along(x)) {
#    if(x[i] == ifinput){x[i] = ifoutput}
#  } 
#}
class(rawdata$start)
colnames(rawdata)
rawdata %>% group_by(user) %>%
  count()
a=  ymd_hm("2018/4/9 14:11") -  ymd_hm("2018/4/9 14:01")
a = parse_datetime("2018/4/9 14:11", "%Y/%m/%d %H:%M") - parse_datetime("2018/4/9 14:01", "%Y/%m/%d %H:%M")
as.numeric(a, units = "mins")
a - 9

#2018/4/9 14:11
rawdata <- raw %>%
  mutate(end = ymd_hm(end), start = ymd_hm(start), taketime = as.numeric((end - start), units = "mins"), gawy = end - start, come = ymd_hm(come), go = ymd_hm(go))
rawdata %>%
  ggplot(aes(user, taketime, fill = material)) +
  geom_col()
pd <- position_dodge(.2)
ggplot(rawdata,aes(x=user,color=material,group=material)) +
  geom_point(aes(y=start)) +
  geom_point(aes(y=end)) +
  geom_line(aes(group=user)) +
  coord_flip()
  
colnames(rawdata)  
  theme(panel.grid.major.x = element_line(colour=material, linetype="dashed"))
  
d1 <- runif(10,10,15)
d2 <- runif(10,25,30)
d3 <- rep(1:10,2)
df <- data.frame(x = d3, y = c(d1,d2))
ggplot(df, aes(x=x, y=y)) +  
    geom_point() + geom_line(aes(group = d3))
a0 <- rawdata$user
a00 <- c(a0,a0)
a1 <- rawdata$start
a2 <- rawdata$end
a3 <- rawdata$material
smin <-min(rawdata$start, na.rm=T)
smax <-max(rawdata$end, na.rm=T)
a4 <- rawdata$come
a5 <- rawdata$go
df <- data.frame(x = a00, y = c(a1,a2), z = c(a3,a3), w = c(a4,a5))
ggplot(df, aes(x=x, y=y, color = z, group = z)) +  
  geom_point() + geom_line(aes(group = x)) +
  coord_flip()
df %>% View()
str(df)
ggplot(df, aes(x="student", y=y, color = z, group = z)) +  
  geom_point() + geom_line(aes(group = z)) +
  facet_grid(x~.) + 
  coord_flip() +
  geom_point(aes(x="student", y = w), shape = 8, col = "red")

ggplot(df, aes(x="student", y=y, color = z, group = z)) +  
  geom_point() + geom_line(aes(group = z)) +
  facet_grid(x~.) + 
  coord_flip()

ggplot(rawdata,aes(x=user,y=taketime,color=material,group=material)) + 
  geom_line(stat="summary",stat_params=list(fun.y="mean"),position=pd) + 
  geom_point(stat="summary",stat_params=list(fun.y="mean"),position=pd) + 
  stat_summary(fun.data = "mean_se", geom="errorbar",position=pd) +
  facet_grid(. ~ miht.binned)

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
