library(readr)
library(tidyverse)
library(gdata)
options(stringsAsFactors = F)
# Sys.setlocale(locale = "UTF-8")
# Sys.setlocale(category = "LC_ALL", locale = "cht")
# rm(list=ls())

NTU_Attnd <- read_csv("all_long_new.csv")
NTU_Vote <- read_csv("vote_all.csv")

NTU_Attnd_rate <- NTU_Attnd %>%
  #mutate(Attnd_condition = if_else(Attnd == '出席', Attnd, '缺席')) %>%
  filter(general == "current" | general == "current*") %>%
  group_by(degree, college, grade, dept, name, start, end, Attnd) %>%
  count() %>%
  ungroup() %>%
  group_by(degree, college, grade, dept, name, start, end) %>%
  mutate(Attnd_Rate = n/sum(n)) %>%
  ungroup()
  
df_combine <- NTU_Attnd_rate %>%
  left_join(NTU_Vote, by = c('name' = 'name', 'start' = 'start', 'end' = 'end', 'college' = 'college'))
df_combine %>%
  filter(is.na(elected)) %>%
  select(college, name, start, end) %>%
  group_by(college, name, start, end) %>%
  count()
df_model <- df_combine %>%
  mutate(degree = if_else(degree == '大學部', 0, 1),
         grade = as.factor(grade),
         start = str_c(str_sub(start, 1, 3), str_sub(start, 5, 5)),
         vote_support_rate = vote_support/vote_object) %>%
  filter(Attnd == '出席') %>%
  select(-Attnd)
# View(df_model)
mm <- model.matrix( ~ college - 1, data = df_model )
colnames(mm)
df_model_college <- cbind(df_model, mm)
mm2 <- model.matrix( ~ grade - 1, data = df_model_college )
colnames(mm2)
df_model_grade <- cbind(df_model_college, mm2)
mm3 <- model.matrix( ~ start - 1, data = df_model_grade )
colnames(mm3)
df_model_final <- cbind(df_model_grade, mm3)

#View(df_model_final)
colnames(df_model_final)
df <- df_model_final %>%
  select(-c(college, elected, grade, dept, name, start, end, n, vote_support,
            vote_object, vote_invalid, college_population,
            college_population_total, college_vote_population,
            college_support_vote, college_vote_invalid))
#View(df)
df_colname <- df %>% colnames()
df_colname %>% str_c(collapse = ", ")
df_colname %>% str_c(collapse = "+ ")
model1 <- lm(formula= Attnd_Rate ~ degree + vote_support_rate*competitive + college_vote_rate + competitive + college_support_rate + college_population_rate,
            data=df)
summary(model1)
model2 <- lm(formula= Attnd_Rate ~ vote_support_rate*competitive + college_vote_rate + competitive + college_support_rate + college_population_rate,
             data=df)
summary(model2)
model3 <- lm(formula= Attnd_Rate ~ vote_support_rate*competitive + college_vote_rate + competitive + college_support_rate,
             data=df)
summary(model3)
model4 <- lm(formula= Attnd_Rate ~ vote_support_rate*competitive + college_vote_rate + college_support_rate,
             data=df)
summary(model4)
model5 <- lm(formula= Attnd_Rate ~ college_vote_rate + college_support_rate,
             data=df)
summary(model5)

model6 <- lm(formula= Attnd_Rate ~ college_vote_rate + college工學院 + college文學院 + college生物資源暨農學院 + college法律學院 + college社會科學院 + college理學院 + college管理學院 + college醫學院,
             data=df)
summary(model6)
model7 <- lm(formula= Attnd_Rate ~ vote_support_rate*competitive + college_vote_rate + competitive + college_support_rate + college_population_rate + college工學院 + college文學院 + college生物資源暨農學院 + college法律學院 + college社會科學院 + college理學院 + college管理學院 + college醫學院,
             data=df)
summary(model7)
model8 <- lm(formula= Attnd_Rate ~ vote_support_rate*competitive + college_vote_rate + competitive + college_support_rate + college_population_rate + college工學院 + college文學院 + college生物資源暨農學院 + college法律學院 + college社會科學院 + college理學院 + college管理學院 + college醫學院  +
               grade1+ grade2+ grade3+ grade4+ grade5,
             data=df)
summary(model8)
model9 <- lm(formula= Attnd_Rate ~ vote_support_rate*competitive + college_vote_rate + competitive + college_support_rate + college_population_rate + college工學院 + college文學院 + college生物資源暨農學院 + college法律學院 + college社會科學院 + college理學院 + college管理學院 + college醫學院  +
               grade1+ grade2+ grade3+ grade4+ grade5 + start1032+ start1041+ start1042+ start1051+ start1052+ start1061,
             data=df)
summary(model9)
model10 <- lm(formula= Attnd_Rate ~ vote_support_rate*competitive + college_vote_rate + competitive + college_support_rate + college_population_rate + college法律學院 + college社會科學院 + college管理學院 + college醫學院 +
               grade1+ grade2+ grade3+ grade4+ grade5 + start1032+ start1041+ start1042+ start1051+ start1052+ start1061,
             data=df)
summary(model10)
model11 <- lm(formula= Attnd_Rate ~ vote_support_rate*competitive + college_vote_rate + competitive + college_support_rate + college_population_rate + college法律學院 + college社會科學院 + college管理學院 + college醫學院 +
                grade1+ grade2+ grade3+ grade4+ grade5 + start1041+ start1042+ start1051+ start1061,
              data=df)
summary(model11)
model12 <- lm(formula= Attnd_Rate ~ college_vote_rate + competitive + college法律學院 + college社會科學院 + college管理學院 + college醫學院 +
                grade1+ grade2+ grade3+ grade4+ grade5 + start1051,
              data=df)
summary(model12)
model13 <- lm(formula= Attnd_Rate ~ degree + college_vote_rate + competitive + college法律學院 + college社會科學院 + college管理學院 +
                grade1+ grade2+ grade3+ grade4 + grade5 + start1051,
              data=df)
summary(model13)
model14 <- lm(formula= Attnd_Rate ~ degree + college_vote_rate + competitive + college法律學院 + college社會科學院 + college管理學院 +
                grade1+ grade2+ grade3+ grade4 + start1051,
              data=df)
summary(model14)
model15 <- lm(formula= Attnd_Rate ~ degree + college_vote_rate + competitive + college管理學院 +
                grade1+ grade2+ grade3+ grade4 + start1051,
              data=df)
summary(model15)
model16 <- lm(formula= Attnd_Rate ~ degree + competitive + college管理學院 +
                grade1+ grade2+ grade3 + start1051,
              data=df)
summary(model16)
model17 <- lm(formula= Attnd_Rate ~ degree + competitive + college管理學院 +
                start1051,
              data=df)
summary(model17)
model18 <- lm(formula= Attnd_Rate ~ degree + competitive + college管理學院,
              data=df)
summary(model18)
model19 <- lm(formula= Attnd_Rate ~ degree + competitive,
              data=df)
summary(model19)
model20 <- lm(formula= Attnd_Rate ~ competitive,
              data=df)
summary(model20)

ggplot(df, aes(college_vote_rate, Attnd_Rate)) +
  geom_point()
ggplot(df, aes(college_support_rate, Attnd_Rate)) +
  geom_point()
ggplot(df, aes(competitive, Attnd_Rate)) +
  geom_point()
ggplot(df, aes(degree, Attnd_Rate)) +
  geom_point()
ggplot(df, aes(college_population_rate, Attnd_Rate)) +
  geom_point()