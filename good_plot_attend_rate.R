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

#出席率長條圖_五學期 vs.院 ~ 依照院分
df_sc_attnd_vis_avg <-
  df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(college_english, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(term) %>%
  summarise(Attnd_Rate_avg = sum(if_else(Attnd == '出席', n, as.integer(0)))/sum(n)) %>%
  left_join(
    df_sc_attnd_vis %>%
      select(college_english, term, Attnd) %>%
      filter(term != "103-2") %>%
      group_by(college_english, term, Attnd) %>%
      summarise(n = n()) %>% 
      mutate(Attnd_yes = if_else(Attnd == "出席", n, as.integer(0))) %>%
      ungroup() %>%
      group_by(college_english) %>%
      mutate(Attnd_Rate_college_order = sum(Attnd_yes, na.rm = T)/sum(n, na.rm = T))  %>%
      ungroup %>% 
      filter(Attnd == "出席") %>%
      mutate(college_english = fct_reorder(as.factor(college_english), -Attnd_Rate_college_order)) %>%
      distinct(term, college_english)
    )

#overall出席率
df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(college_english) %>%
  count(Attnd) %>%
  mutate(Attnd_Rate = n/sum(n)) %>%
  ungroup() %>%
  filter(Attnd == "出席") %>%
  arrange(desc(Attnd_Rate))

df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(college_english, term) %>%
  count(Attnd) %>%
  mutate(Attnd_yes = if_else(Attnd == "出席", n, as.integer(0))) %>%
  mutate(Attnd_Rate = n/sum(n)) %>%
  mutate(Attnd_Rate_label = if_else(term %in% c("104-1", "106-1"), as.character(formattable::percent(Attnd_Rate, digits = 0)), NA_character_)) %>%
  mutate(Attnd_Rate_label_dot = if_else(term %in% c("104-1", "106-1"), formattable::percent(Attnd_Rate), as.double(NA))) %>%
  ungroup() %>%  
  group_by(college_english) %>%
  mutate(Attnd_Rate_college_order = sum(Attnd_yes, na.rm = T)/sum(n, na.rm = T))  %>%
  ungroup() %>% 
  filter(Attnd == "出席") %>%
  mutate(college_english = fct_reorder(as.factor(college_english), -Attnd_Rate_college_order)) %>%
  add_row(college_english = "Life Science", term = "104-1", Attnd = "出席",
          n = 0, Attnd_yes = 0, Attnd_Rate =  formattable::percent(0), 
          Attnd_Rate_label = as.character(formattable::percent(0, digits = 0)),
          Attnd_Rate_label_dot = as.double(formattable::percent(0)),
          Attnd_Rate_college_order = as.double(0)) %>%
  add_row(college_english = "Life Science", term = "104-2", Attnd = "出席",
          n = 0, Attnd_yes = 0, Attnd_Rate =  NA, 
          Attnd_Rate_label = NA_character_,
          Attnd_Rate_label_dot = as.double(NA),
          Attnd_Rate_college_order = as.double(0)) %>%
  ggplot(aes(term, Attnd_Rate, group = college_english)) +
  geom_line(colour = "#bd3037", size = .8) +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L),
                     limits = c(0, 1),
                     breaks = c(0, 0.5, 1)) + 
  facet_wrap(~college_english, ncol = 3) +
  theme(panel.spacing.y = unit(1, "lines")) + 
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) + 
  geom_text(aes(label=Attnd_Rate_label),hjust=0.5, vjust=+2.5, size=3) +
  geom_point(aes(term, Attnd_Rate_label_dot)) +
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "HP", face = "plain", size=12)) +
  labs(title = "Student Congress Members Assembly Attendance Rate Reached 50% at NTU") +
  labs(subtitle = "College of Management Congress Members Preserve Their Time for Internship instead of Assembly") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\nterm") +
  ylab("attendance rate") +
  theme(axis.title = element_text(family = "HP", face = "plain"), 
        legend.title = element_text(family = "HP", face = "plain"), 
        legend.text = element_text(family = "HP", face = "plain"), 
        plot.title = element_text(family = "HP", face = "plain"), 
        plot.subtitle = element_text(family = "HP", face = "italic"), 
        plot.caption = element_text(family = "HP", face = "plain")) +
  geom_line(data=df_sc_attnd_vis_avg, aes(term, Attnd_Rate_avg), colour="grey50")

#出席率折線圖_五學期 vs.院 ~ 依照院分
df_sc_attnd_vis_avg <-
  df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(grade, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(term) %>%
  summarise(Attnd_Rate_avg = sum(if_else(Attnd == '出席', n, as.integer(0)))/sum(n)) %>%
  left_join(distinct(df_sc_attnd_vis, term, grade)) %>%
  rename(grade_order = grade) %>%
  mutate(grade = case_when(grade_order == 1 ~ "freshman",
                           grade_order == 2 ~ "sophomore",
                           grade_order == 3 ~ "junior",
                           grade_order == 4 ~ "senior",
                           grade_order == 5 ~ "super senior")) %>%
  mutate(grade = reorder(as.factor(grade), grade_order))

df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(grade, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(grade, term) %>%
  mutate(Attnd_Rate = round(n/sum(n), 2)) %>%
  mutate(Attnd_Rate_label = if_else(term %in% c("104-1", "106-1") | grade == 1, as.character(formattable::percent(Attnd_Rate, digits = 0)), NA_character_)) %>%
  mutate(Attnd_Rate_label_dot = if_else(term %in% c("104-1", "106-1") | grade == 1, formattable::percent(Attnd_Rate), as.double(NA))) %>%
  ungroup() %>%  
  rename(grade_order = grade) %>%
  mutate(grade = case_when(grade_order == 1 ~ "freshman",
                           grade_order == 2 ~ "sophomore",
                           grade_order == 3 ~ "junior",
                           grade_order == 4 ~ "senior",
                           grade_order == 5 ~ "super senior")) %>%
  mutate(grade = reorder(as.factor(grade), grade_order)) %>%
  filter(Attnd == "出席") %>%
  # select(grade, term, Attnd, Attnd_Rate) %>%
  # filter(grade == 'j')
  ggplot(aes(term, Attnd_Rate, group = grade)) +
  geom_line(colour = "#bd3037", size = .8, group=1) +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L),
                     limits = c(0.35, 0.90),
                     breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) + 
  facet_wrap(~grade, ncol = 5) +
  theme(panel.spacing.y = unit(1, "lines")) + 
  # theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) + 
  geom_text(aes(label=Attnd_Rate_label),hjust=0.5, vjust=2, size=3) +
  geom_point(aes(term, Attnd_Rate_label_dot)) +
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "HP", face = "plain", size=12)) +
  labs(title = "Student Congress Members Attendance Rate Changed a Lot as Time Differed in Terms of Grade") + #有可能根本是缺席或出席的人年級上升了
  labs(subtitle = "Attendance Rate of Sophomores and Juniors were Relatively Stable") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\nterm") +
  ylab("attendance rate") +
  theme(axis.title = element_text(family = "HP", face = "plain"), 
        legend.title = element_text(family = "HP", face = "plain"), 
        legend.text = element_text(family = "HP", face = "plain"), 
        plot.title = element_text(family = "HP", face = "plain"), 
        plot.subtitle = element_text(family = "HP", face = "italic"), 
        plot.caption = element_text(family = "HP", face = "plain")) +
  geom_line(data=df_sc_attnd_vis_avg, aes(term, Attnd_Rate_avg), colour="grey50")

#####底下測試用 
#出席率折線圖_五學期 vs.院 ~ 依照院分
df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(term, grade) %>%
  mutate(n_sc = n_distinct(name)) %>%
  ungroup() %>%
  group_by(grade, term, Attnd, n_sc) %>%
  count() %>%
  ungroup %>%
  group_by(grade, term) %>%
  mutate(Attnd_Rate = round(n/sum(n), 2)) %>%
  mutate(Attnd_Rate_label = if_else(term %in% c("104-1", "106-1") | grade == 1, as.character(formattable::percent(Attnd_Rate, digits = 0)), NA_character_)) %>%
  mutate(Attnd_Rate_label_dot = if_else(term %in% c("104-1", "106-1") | grade == 1, formattable::percent(Attnd_Rate), as.double(NA))) %>%
  ungroup() %>%  
  rename(grade_order = grade) %>%
  mutate(grade = case_when(grade_order == 1 ~ "freshman",
                           grade_order == 2 ~ "sophomore",
                           grade_order == 3 ~ "junior",
                           grade_order == 4 ~ "senior",
                           grade_order == 5 ~ "super senior")) %>%
  mutate(grade = reorder(as.factor(grade), grade_order)) %>%
  filter(Attnd == "出席") %>%
  # select(grade, term, Attnd, Attnd_Rate) %>%
  # filter(grade == 'j')
  ggplot(aes(n_sc, Attnd_Rate, group = grade, color = term)) +
  geom_point() +
  # geom_line(colour = "#bd3037", size = .8, group=1) +
  # scale_y_continuous(labels =  scales::percent_format(accuracy = 1L),
  #                    limits = c(0.35, 0.75),
  #                    breaks = c(0.4, 0.5, 0.6, 0.7)) + 
  facet_wrap(~grade, ncol = 5) +
  theme(panel.spacing.y = unit(1, "lines")) + 
  # theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "grey", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) +
  scale_color_manual( values=c("blue2", "blue1", "purple", "red1","red2"))

df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(term, grade) %>%
  mutate(n_sc = n_distinct(name)) %>%
  ungroup() %>%
  group_by(grade, term, Attnd, n_sc) %>%
  count() %>%
  ungroup %>%
  group_by(grade, term) %>%
  mutate(Attnd_Rate = round(n/sum(n), 2)) %>%
  mutate(Attnd_Rate_label = if_else(term %in% c("104-1", "106-1") | grade == 1, as.character(formattable::percent(Attnd_Rate, digits = 0)), NA_character_)) %>%
  mutate(Attnd_Rate_label_dot = if_else(term %in% c("104-1", "106-1") | grade == 1, formattable::percent(Attnd_Rate), as.double(NA))) %>%
  ungroup() %>%  
  rename(grade_order = grade) %>%
  mutate(grade = case_when(grade_order == 1 ~ "freshman",
                           grade_order == 2 ~ "sophomore",
                           grade_order == 3 ~ "junior",
                           grade_order == 4 ~ "senior",
                           grade_order == 5 ~ "super senior")) %>%
  mutate(grade = reorder(as.factor(grade), grade_order)) %>%
  filter(Attnd == "出席") %>%
  # select(grade, term, Attnd, Attnd_Rate) %>%
  # filter(grade == 'j')
  ggplot(aes(n_sc, Attnd_Rate, group = term, color = grade)) +
  geom_point() +
  # geom_line(colour = "#bd3037", size = .8, group=1) +
  # scale_y_continuous(labels =  scales::percent_format(accuracy = 1L),
  #                    limits = c(0.35, 0.75),
  #                    breaks = c(0.4, 0.5, 0.6, 0.7)) + 
  facet_wrap(~term, ncol = 5) +
  theme(panel.spacing.y = unit(1, "lines")) + 
  # theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) #+ 
  geom_text(aes(label=Attnd_Rate_label),hjust=0.5, vjust=2, size=3) +
  # geom_point(aes(term, Attnd_Rate_label_dot)) +
    # geom_text(aes(label=college_english)) #+
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "HP", face = "plain", size=12)) +
  labs(title = "Student Congress Members Assembly Attendance Rate Reached 50% at NTU") +
  labs(subtitle = "College of Management Congress Members Preserve Their Time for Internship instead of Assembly") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\nterm") +
  ylab("attendance rate") +
  theme(axis.title = element_text(family = "HP", face = "plain"), 
        legend.title = element_text(family = "HP", face = "plain"), 
        legend.text = element_text(family = "HP", face = "plain"), 
        plot.title = element_text(family = "HP", face = "plain"), 
        plot.subtitle = element_text(family = "HP", face = "italic"), 
        plot.caption = element_text(family = "HP", face = "plain")) +
  geom_line(data=df_sc_attnd_vis_avg, aes(term, Attnd_Rate_avg), colour="grey50")

 
#出席率長條圖_五學期 vs.所有年級
df_sc_attnd_vis_avg <-
  df_sc_attnd_vis %>%
  group_by(session_id, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(session_id) %>%
  summarise(Attnd_Rate_avg = sum(if_else(Attnd == '出席', n, as.integer(0)))/sum(n)) %>%
  left_join(distinct(df_sc_attnd_vis, term, session_id))

df_sc_attnd_vis %>%
  group_by(session_id, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(session_id, term) %>%
  mutate(Attnd_Rate = round(n/sum(n), 2), total = sum(n)) %>%
  ungroup() %>%
  group_by(term) %>%
  mutate(session_min = min(as.integer(session_id)), session_max = max(as.integer(session_id))) %>%
  ungroup %>%
  mutate(term_session = str_c(term,"_", session_id),
         term_session_min = str_c(term,"_",  session_min),
         term_session_max = str_c(term,"_",  session_max)) %>%
  mutate(Attnd_Rate_label = if_else(term_session == term_session_min | term_session == term_session_max, as.character(formattable::percent(Attnd_Rate, digits = 0)), NA_character_)) %>%
  mutate(Attnd_Rate_label_dot = if_else(term_session == term_session_min | term_session == term_session_max, round(Attnd_Rate,2), as.double(NA))) %>%
  filter(Attnd == "出席") %>%
  # select(Attnd_Rate_label, Attnd_Rate_label_dot)
  ungroup %>%
  ggplot(aes(session_id, Attnd_Rate, group = term)) +
  geom_line(colour = "red2", size = .8) +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L)) + 
  facet_wrap(~term, ncol = 5, scale="free_x") +
  scale_x_discrete(expand = c(0, 0.9)) + # change additive expansion from default 0.6 to 0.5
  
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text=element_text(family="TT Courier New")) +
  geom_text(aes(label=Attnd_Rate_label),hjust=-0.1, vjust=2, size=3) +
  geom_point(aes(session_id, Attnd_Rate_label_dot)) +
  theme(strip.text.x = element_text(face = "bold")) +
  # theme(panel.spacing = unit(2, "lines")) + 
  labs(title = "NTU Students Congress Attendance Rate Data")  +
  #sub title: It declines very steep.
  geom_line(data=df_sc_attnd_vis_avg, aes(session_id, Attnd_Rate_avg), colour="grey50")

  
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

