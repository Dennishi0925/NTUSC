library(tidyverse)
library(knitr)
library(kableExtra)
library(formattable)
options(stringsAsFactors = F)
# Sys.setlocale(locale = "UTF-8")
# Sys.setlocale(category = "LC_ALL", locale = "cht")
# rm(list=ls())

###### NTUSC Attendance Data 台大學代出席資料 

### 資訊視覺化

### 讀入檔案
df_sc_attnd_t <- read_csv("df_sc_attnd.csv")
df_sc_attnd <- df_sc_attnd_t %>% filter(degree == '大學部')
df_vote_college <- read_csv("df_vote_college.csv")
df_college_dept_eng <- read_csv("df_college_dept_eng.csv")
df_college <- read_csv("df_college_final.csv")
df_candidate <- read_csv("df_candidate_final.csv")

### 增加可能需要欄位然後踢掉研究所
df_sc_attnd_vis <- 
  df_sc_attnd %>%
  left_join(
    df_sc_attnd %>%
      distinct(term, session) %>%
      group_by(term) %>% 
      mutate(session_id = row_number()) %>%
      ungroup(),
    by = c("term", "session")
  ) %>%
  mutate(Attnd_condition = if_else(Attnd == '出席', Attnd, '缺席')) %>%
  mutate(Attnd_condition = if_else(Attnd_condition == "出席", 0, 1)) %>%
  mutate(college = factor(college), term = factor(term), session_id = as.factor(session_id)) %>%
  filter(degree == '大學部') %>%
  mutate(college_english = if_else(str_detect(college_english, "Computer"), "EE & CS", if_else(str_detect(college_english, "Bio"), "Bio & Agr", college_english )))

#出席率密度圖_單一學期每次會議 vs.人
df_sc_attnd_vis %>%
  filter(term == "104-1") %>%
  arrange(desc(college), desc(dept)) %>%
  ggplot(mapping = aes(x = session_id, y = factor(name), height =0.9)) +
  geom_tile(mapping = aes(fill = Attnd_condition)) 

#出席率密度圖_單一學期每次會議 vs.院
df_sc_attnd_vis %>%
  filter(term == "104-1") %>%
  group_by(college, session) %>%
  mutate(Attnd_Rate = sum(Attnd_condition)/n()) %>%
  ggplot(mapping = aes(x = session_id, y = college_english, fill = Attnd_Rate, height =0.9)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", labels = scales::percent_format(accuracy = 1L))+
  theme(axis.title.y = element_text(angle = 90, color = "black"),
        axis.text.x = element_text(angle = 60, hjust = 1, color = "black"),
        axis.title.x = element_text(color = "black", face = "bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text=element_text(family="Calibri"))

#出席率長條圖_五學期 vs.院 ~ 依照院分
df_sc_attnd_vis_avg <-
  df_sc_attnd_vis %>%
  group_by(college_english, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(term) %>%
  summarise(Attnd_Rate_avg = sum(if_else(Attnd == '出席', n, as.integer(0)))/sum(n)) %>%
  left_join(distinct(df_sc_attnd_vis, term, college_english))

df_sc_attnd_vis %>%
  group_by(college_english, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(college_english, term) %>%
  mutate(Attnd_Rate = round(n/sum(n), 2)) %>%
  mutate(Attnd_Rate_label = if_else(term %in% c("104-1", "106-1"), as.character(formattable::percent(Attnd_Rate, digits = 0)), NA_character_)) %>%
  ungroup %>%  
  filter(Attnd == "出席") %>%
  ggplot(aes(term, Attnd_Rate, group = college_english)) +
  geom_line(colour = "black", size = .8) +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L)) + 
  facet_wrap(~college_english, ncol = 4) +
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text=element_text(family="Calibri")) +
  geom_text(aes(label=Attnd_Rate_label),hjust=0.5, vjust=2) +
  theme(strip.text.x = element_text(face = "bold")) + 
  labs(title = "NTU Students Congress Attendance Rate - by college from 2015H2 to 2017H1") +
  geom_line(data=df_sc_attnd_vis_avg, aes(term, Attnd_Rate_avg), colour="grey50")

#出席率長條圖_五學期 vs.所有年級
df_sc_attnd_vis_avg <-
  df_sc_attnd_vis %>%
  group_by(grade, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(term) %>%
  summarise(Attnd_Rate_avg = sum(if_else(Attnd == '出席', n, as.integer(0)))/sum(n)) %>%
  left_join(distinct(df_sc_attnd_vis, term, grade)) %>%
  mutate(grade = case_when(grade == 1 ~ "freshman",
                           grade == 2 ~ "sophomore",
                           grade == 3 ~ "junior",
                           grade == 4 ~ "senior",
                           grade == 5 ~ "Super Senior"))

df_sc_attnd_vis %>%
  group_by(grade, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(grade, term) %>%
  mutate(Attnd_Rate = round(n/sum(n), 2)) %>%
  mutate(Attnd_Rate_label = if_else(term %in% c("104-1", "106-1"), as.character(formattable::percent(Attnd_Rate, digits = 0)), NA_character_)) %>%
  ungroup %>%  
  mutate(grade = case_when(grade == 1 ~ "freshman",
                           grade == 2 ~ "sophomore",
                           grade == 3 ~ "junior",
                           grade == 4 ~ "senior",
                           grade == 5 ~ "Super Senior"
  )) %>%
  filter(Attnd == "出席") %>%
  ggplot(aes(term, Attnd_Rate, group = grade)) +
  geom_line(colour = "black", size = .8) +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L)) + 
  facet_wrap(~grade, ncol = 2) +
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text=element_text(family="Calibri")) +
  geom_text(aes(label=Attnd_Rate_label),hjust=0.5, vjust=2) +
  theme(strip.text.x = element_text(face = "bold")) + 
  labs(title = "NTU Students Congress Attendance Rate - by college from 2015H2 to 2017H1") +
  geom_line(data=df_sc_attnd_vis_avg, aes(term, Attnd_Rate_avg), colour="grey50")


#出席率長條圖_五學期 vs.所有年級
df_sc_attnd_vis %>%
  select(college, grade, dept, name, term, session, Attnd) %>%
  mutate(college = factor(college), term = factor(term)) %>%
  group_by(session, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(session, term) %>%
  mutate(Attnd_Rate = round(n/sum(n), 2), total = sum(n)) %>%
  filter(Attnd == "出席") %>%
  ungroup %>%
  ggplot(aes(session, Attnd_Rate)) +
  geom_col(width = 0.7, fill="lightblue", colour="black") +
  #geom_line(aes(term, percent, group = college)) +
  facet_wrap(~term, ncol = 5, scale="free_x") +
  theme(strip.background = element_rect(fill="pink"))

df_sc_attnd_vis %>% filter(term == "104-1") %>%
  group_by(session, Attnd) %>%
  count() %>%
  ungroup() %>%
  group_by(session) %>%
  mutate(Attnd_Rate = n/sum(n)) %>%
  ggplot(aes(x = session, y = n, fill = factor(Attnd))) +
  geom_bar(stat = "identity") +
  #guides(fill=guide_legend(reverse=TRUE)) +
  #scale_fill_discrete() +
  coord_flip() +
  scale_fill_brewer(palette="Pastel1") +
  guides(fill=guide_legend(title="present"))

df_sc_attnd_vis %>% filter(term == "104-1") %>%
  group_by(session, Attnd) %>%
  count() %>%
  ungroup() %>%
  group_by(session) %>%
  mutate(Attnd_Rate = n/sum(n)) %>%
  ggplot() +
  geom_bar(aes(x = session, y = Attnd_Rate, fill = factor(Attnd)), stat = "identity") +
  geom_line(aes(x = session, y = Attnd_Rate, group = Attnd)) +
  #guides(fill=guide_legend(reverse=TRUE)) +
  #scale_fill_discrete() +
  coord_flip() +
  guides(fill=guide_legend(title="present")) +
  facet_grid(.~Attnd) +
  scale_fill_brewer(palette="Pastel1")

df_sc_attnd_vis %>% filter(term == "104-1") %>%
  group_by(session, Attnd) %>%
  count() %>%
  ungroup() %>%
  group_by(session) %>%
  mutate(Attnd_Rate = n/sum(n)) %>%
  kable(caption = "Title of the table")

df_sc_attnd_vis %>% filter(term == "104-1") %>%
  #select(-present) %>%
  spread(key = "session", value = "Attnd")

df_sc_attnd_vis %>%
  filter(start == '104-2') %>%
  #group_by(college) %>%
  #mutate(mean_vote_support = mean(vote_support)) %>%
  ggplot() +
  geom_boxplot(aes(x = college, y = vote_support)) +
  facet_grid(.~elected)


p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot()
mpg$hwy
