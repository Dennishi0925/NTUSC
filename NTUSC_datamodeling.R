library(tidyverse)
library(rethinking)
data_model %>% View()
df_candidate <- read_rds("D:/R_programming/Journalism_homework/NTUSC/candidate")
df_college <- read_rds("D:/R_programming/Journalism_homework/NTUSC/college")
data_pornot <- read_rds("datapor")
data_model <- read_rds("model")
data_lm <- read_rds("D:/R_programming/Journalism_homework/NTUSC/datalm")
glimpse(data_lm)
colnames(data_lm)
glimpse(data_model)
View(data_model)
colnames(data_model)
data <- data_lm[ data_lm$start == "104-1" , ]
#data_lm <- write_csv(data, "D:/R_programming/Journalism_homework/NTUSC/datalm.csv")
#write_rds(data_lm, "D:/R_programming/Journalism_homework/NTUSC/datalm")
#data_lm <- read_csv("D:/R_programming/Journalism_homework/NTUSC/datalm.csv",col_types = cols(
#  college = col_character(),
#  name = col_character(),
#  dept = col_character(),
#  start = col_character(),
#  percent = col_double(),
#  sup_percent = col_double(),
#  nonobj_percent = col_double(),
#  vote_rate = col_double(),
#  competitive = col_integer(),
#  void_rate = col_double(),
#  population_rate = col_double(),
#  college工學院 = col_factor(NULL),
#  college文學院 = col_factor(NULL),
#  college生物資源暨農學院 = col_factor(NULL),
#  college法律學院 = col_factor(NULL),
#  college社會科學院 = col_factor(NULL),
#  college理學院 = col_factor(NULL),
#  college管理學院 = col_factor(NULL),
#  college醫學院 = col_factor(NULL)
#))
#data <- data_model %>%
#  filter(!is.na(sup_percent)) %>%
#  mutate(nonobj_percent = if_else(is.na(nonobj_percent), 0, nonobj_percent)) %>%
#  as.data.frame()
#data <- data[ complete.cases(data) , ]
View(data_lm)
data_lm <- data_model %>%
  filter(!is.na(sup_percent)) %>%
  mutate(nonobj_percent = if_else(is.na(nonobj_percent), 0, nonobj_percent)) %>%
  rename(term = start, Attnd_Rate = percent, support_rate = sup_percent, nonobj_Rate = nonobj_percent, college_population_rate = population_rate) %>%
  mutate(Attnd_Rate = round(Attnd_Rate, 2), support_rate = round(support_rate, 2), nonobj_Rate = round(nonobj_Rate, 2), college_population_rate = round(college_population_rate, 2), void_rate = round(void_rate, 2), vote_rate = round(vote_rate, 2))
write_rds(data_lm, "D:/R_programming/Journalism_homework/NTUSC/datalm")
getwd()
colnames(data_model)  
?rename
#mm <- model.matrix( ~ college - 1, data= data )
#colnames(mm)
#data <- cbind(data, mm)
#View(data)
#typeof(data)
#data %>% as_tibble()
m1_1 <- map(
  alist(
    percent ~ dnorm( mu , sigma ) ,
    mu <- b0 + b1*sup_percent + b2*nonobj_percent*competitive + b3*vote_rate + b4*competitive + b5*void_rate + b6*population_rate,
    b0 ~ dnorm(0,1),
    c(b1, b2, b3, b4, b5, b6) ~ dnorm(0,2),
    sigma ~ dunif(0,2)
  ) ,
  data = data)


model <- lm(formula= percent ~ sup_percent + nonobj_percent*competitive + vote_rate + competitive + void_rate + population_rate,
            data=data_lm)
summary(model)
model2 <- lm(formula= percent ~ sup_percent + vote_rate + competitive,
            data=data_lm)
summary(model2)
model3 <- lm(formula= percent ~ sup_percent + vote_rate,
             data=data_lm)
summary(model3)
model4 <- lm(formula= percent ~ vote_rate,
             data=data_lm)
summary(model4)
model5 <- lm(formula= percent ~ sup_percent + vote_rate + competitive + college工學院 + college文學院 + college生物資源暨農學院 + college法律學院 + college社會科學院 + college理學院 + college管理學院 + college醫學院,
             data=data_lm)
ggplot(data_lm, aes(vote_rate, percent)) +
  geom_point()
ggplot(data_lm, aes(void_rate, percent)) +
  geom_point()
ggplot(data_lm, aes(sup_percent, percent)) +
  geom_point()
summary(model5)
model6 <- lm(formula= percent ~ vote_rate + college工學院 + college文學院 + college生物資源暨農學院 + college法律學院 + college社會科學院 + college理學院 + college管理學院 + college醫學院,
             data=data_lm)
summary(model6)
model7 <- lm(formula= percent ~ sup_percent + nonobj_percent*competitive + vote_rate + competitive + void_rate + population_rate + college工學院 + college文學院 + college生物資源暨農學院 + college法律學院 + college社會科學院 + college理學院 + college管理學院 + college醫學院,
            data=data_lm)
summary(model7)
model8 <- lm(formula= percent ~ sup_percent + vote_rate + competitive + population_rate + college工學院 + college文學院 + college生物資源暨農學院 + college法律學院 + college社會科學院 + college理學院 + college管理學院 + college醫學院,
             data=data_lm)
summary(model8)
model9 <- lm(formula= percent ~ sup_percent + vote_rate + competitive + population_rate + college工學院 + college文學院 + college生物資源暨農學院 + college理學院 + college管理學院 + college醫學院,
             data=data_lm)
summary(model9)
model10 <- lm(formula= percent ~ sup_percent + vote_rate + competitive + population_rate + college文學院 + college理學院 + college醫學院,
             data=data_lm)
summary(model10)
model11 <- lm(formula= percent ~ sup_percent + vote_rate + population_rate + college文學院 + college理學院 + college醫學院,
              data=data_lm)
summary(model11)
model12 <- lm(formula= percent ~ sup_percent + vote_rate + population_rate + college文學院 + college理學院,
              data=data_lm)
summary(model12)
model13 <- lm(formula= percent ~ sup_percent + vote_rate + college文學院,
              data=data_lm)
summary(model13)
model14 <- lm(formula= percent ~ sup_percent + vote_rate + sup_percent*vote_rate + college文學院,
              data=data_lm)
summary(model14)
View(data_lm)
data_pornot %>% filter(term == "1041") %>%
  group_by(keys, values) %>%
  count() %>%
  ungroup() %>%
  group_by(keys) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = keys, y = n, fill = factor(values, levels = c("skip","leave","present")))) +
  geom_bar(stat = "identity") +
  #guides(fill=guide_legend(reverse=TRUE)) +
  #scale_fill_discrete() +
  coord_flip() +
  scale_fill_brewer(palette="Pastel1") +
  guides(fill=guide_legend(title="present"))

data_pornot %>% filter(term == "1041") %>%
  group_by(keys, values) %>%
  count() %>%
  ungroup() %>%
  group_by(keys) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot() +
  geom_bar(aes(x = keys, y = percent, fill = factor(values, levels = c("skip","leave","present"))), stat = "identity") +
  geom_line(aes(x = keys, y = percent, group = values)) +
  #guides(fill=guide_legend(reverse=TRUE)) +
  #scale_fill_discrete() +
  coord_flip() +
  guides(fill=guide_legend(title="present")) +
  facet_grid(.~values) +
  scale_fill_brewer(palette="Pastel1")

data_pornot %>% filter(term == "1041") %>%
  group_by(keys, values) %>%
  count() %>%
  ungroup() %>%
  group_by(keys) %>%
  mutate(percent = n/sum(n)) %>% kable(caption = "Title of the table")# %>% 

ggplot(data = data_1041_final(), aes_string(x = input$a1041, y = 'percent', fill = 'values')) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel1")


data_pornot %>% filter(term == "1041") %>%
  select(-present, -X1) %>%
  spread(key = "keys", value = "values") %>%
  View()
# 總體出席 
data_pornot %>%
  group_by(values) %>%
  count()

data_pornot %>%
  group_by(college, values) %>%
  count() %>%
  arrange(desc(n))

# 按照人來看出席
data_pornot %>%
  group_by(name, values) %>%
  count() %>%
  filter(values != "present") %>%
  arrange(desc(n))
# 按照院來看出席
data_pornot %>%
  #filter(str_detect(general, "current")) %>%
  group_by(college) %>%
  count() %>%
  arrange(desc(n))

# 抓出院的翹王
data_pornot %>%
  group_by(college, values) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "skip") %>%
  arrange(desc(percent))
# 抓出院的請假王
data_pornot %>%
  group_by(college, values) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "leave") %>%
  arrange(desc(percent))
# 抓出院的不出席王
data_pornot %>%
  group_by(college, values) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = n/sum(n)) %>%
  filter(values == "leave" |values == "skip") %>%
  summarise(not_present = sum(percent)) %>%
  ungroup %>%
  arrange(desc(not_present))

# 抓出系的翹王
data_pornot %>%
  group_by(dept, values) %>%
  count() %>%
  ungroup %>%
  group_by(dept) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "skip") %>%
  arrange(desc(percent))
# 抓出院的請假王
data_pornot %>%
  group_by(dept, values) %>%
  count() %>%
  ungroup %>%
  group_by(dept) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "leave") %>%
  arrange(desc(percent))
# 抓出院的不出席王
data_pornot %>%
  group_by(dept, values) %>%
  count() %>%
  ungroup %>%
  group_by(dept) %>%
  mutate(percent = n/sum(n)) %>%
  filter(values == "leave" |values == "skip") %>%
  summarise(not_present = sum(percent)) %>%
  ungroup %>%
  arrange(desc(not_present))

# 抓出年級的翹王
data_pornot %>%
  group_by(grade, values) %>%
  count() %>%
  ungroup %>%
  group_by(grade) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "skip") %>%
  arrange(desc(percent))
# 抓出院的請假王
data_pornot %>%
  group_by(grade, values) %>%
  count() %>%
  ungroup %>%
  group_by(grade) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "leave") %>%
  arrange(desc(percent))
# 抓出院的不出席王
data_pornot %>%
  group_by(grade, values) %>%
  count() %>%
  ungroup %>%
  group_by(grade) %>%
  mutate(percent = n/sum(n)) %>%
  filter(values == "leave" |values == "skip") %>%
  summarise(not_present = sum(percent)) %>%
  ungroup %>%
  arrange(desc(not_present))