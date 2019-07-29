library(tidyverse)
library(psych)
library(clipr)
library(showtext)
showtext_auto(enable=TRUE)

## 载入黑体
font_add(family = "LiterataBook", 
         regular = "LiterataBook.otf",
         bold = "LiterataBook-Semibold.otf",
         italic = "LiterataBook-Italic.otf",
         bolditalic = "LiterataBook-BoldItalic.otf")
font_add(family = "consola", 
         regular = "consola.ttf",
         bold = "consolab.ttf",
         italic = "consolai.ttf",
         bolditalic = "consolaz.ttf")
font_add(family = "calibri", 
         regular = "calibri.ttf",
         bold = "calibrib.ttf",
         italic = "calibrii.ttf",
         bolditalic = "calibriz.ttf")
font_add(family = "HP", 
         regular = "HPSimplified_Rg.ttf",
         bold = "HPSimplified_Bd.ttf",
         italic = "HPSimplified_It.ttf",
         bolditalic = "HPSimplified_BdIt.ttf")

### load data
df_sc_attnd_t <- read_csv("df_sc_attnd.csv")
df_sc_attnd <- df_sc_attnd_t %>% filter(degree == '大學部')
df_vote_college <- read_csv("df_vote_college.csv")
df_college_dept_eng <- read_csv("df_college_dept_eng.csv")
df_college <- read_csv("df_college_final.csv")
df_candidate <- read_csv("df_candidate_final.csv")

df_sc_attnd_t <- read_csv("df_sc_attnd.csv")
df_sc_attnd <- df_sc_attnd_t %>% filter(degree == '大學部')
df_vote_college <- read_csv("df_vote_college.csv")
df_vote_college %>% write_rds("df_vote_college")
df_sc_attnd %>% write_rds("df_sc_attnd_university")
df_college_dept_eng <- read_csv("df_college_dept_eng.csv")
df_college <- read_csv("df_college_final.csv")
df_candidate <- read_csv("df_candidate_final.csv")

###檢查區: 有些人沒出席過 如果用原本算出席率的算法去filter 會把全部缺席的人過濾掉
#檢查: 院 - 有找到一個生命科學院104-1
df_sc_attnd %>%
  mutate(Attnd = if_else(Attnd == "出席", "present", "absent")) %>%
  count(term, college_english, Attnd) %>%
  mutate(ID = str_c(term, college_english)) %>%
  select(-term, -college_english) %>%
  spread(key = Attnd, value = n) %>%
  filter(is.na(present))
#檢查: grade
df_sc_attnd %>%
  mutate(Attnd = if_else(Attnd == "出席", "present", "absent")) %>%
  count(term, grade, Attnd) %>%
  mutate(ID = str_c(term, grade)) %>%
  select(-term, -grade) %>%
  spread(key = Attnd, value = n) %>%
  filter(is.na(present))
#檢查: 人 共20人 有點慘
df_sc_attnd %>%
  mutate(Attnd = if_else(Attnd == "出席", "present", "absent")) %>%
  count(college_english, term, name, Attnd) %>%
  mutate(ID = str_c(college_english, term, name)) %>%
  select(-college_english, -term, -name) %>%
  spread(key = Attnd, value = n) %>%
  filter(is.na(present))

#檢查: 人 共20人 有點慘
df_attnd_vote <-
  df_sc_attnd %>%
  # mutate(Attnd = if_else(Attnd == "出席", "present", "absent")) %>%
  count(college_english, term, start, end, name, Attnd) %>%
  unite(ID, college_english, term, start, end, name, sep = "_", remove = T) %>%
  spread(key = Attnd, value = n) %>%
  # filter(is.na(present)) %>%
  mutate(出席 = if_else(is.na(出席), as.integer(0), 出席),
           未請假缺席 = if_else(is.na(未請假缺席), as.integer(0), 未請假缺席),
           其他原因缺席 = if_else(is.na(其他原因缺席), as.integer(0), 其他原因缺席),
           請假缺席 = if_else(is.na(請假缺席), as.integer(0), 請假缺席)) %>%
  separate(col = ID, into = c("college_english", "term", "start", "end", "name"), sep = "_") %>%
  mutate(Attnd_all = 出席 + 未請假缺席 + 其他原因缺席 + 請假缺席) %>%
  mutate(Attnd_Rate = 出席/Attnd_all) %>%
  left_join(select(df_vote_college, -college_english), by = c("start", "end", "name"))
df_attnd_vote %>% filter(is.na(competitive)) %>%
  select(start, competitive)
df_vote_college %>% count(competitive, start)
x11()


df_sc_attnd %>%
  count(college_english, start, end, name, Attnd) %>%
  filter(n >= 8)
df_sc_attnd %>% 
  filter(name == '張家豪') %>%
  count(college,term, Attnd)

# df_attnd_vote <-
#   df_sc_attnd %>% 
#   count(college_english, start, end, name, Attnd) %>%
#   mutate(Attnd_yes = if_else(Attnd == "出席", n, as.integer(0))) %>%
#   mutate(Attnd_Rate = n/sum(n)) %>%
#   filter(Attnd == "出席") %>%
#   left_join(df_vote_college)
### overview

### 候選人及當選與否overview
df_vote_college %>%
  mutate(college_english = if_else(str_detect(college_english, "Computer"), "EE & CS", if_else(str_detect(college_english, "Bio"), "Bio & Agr", college_english ))) %>%
  filter(start != "103-2") %>%
  group_by(college_english) %>%
  mutate(candidate_college_order = n()) %>%
  ungroup() %>%
  mutate(college_english = fct_reorder(as.factor(college_english), -candidate_college_order)) %>%
  mutate(elected = if_else(is.na(elected), "lost", "won")) %>%
  mutate(uncontested_election = if_else(competitive == 0, "uncontested", "contested")) %>%
  mutate(uncontested_election = as.factor(uncontested_election),
         elected = as.factor(elected)) %>%
  count(start, college_english, uncontested_election, elected) %>%
  add_row(start = "104-1", college_english = "Life Science", uncontested_election = "uncontested", elected = "lost", n = 0) %>%
  add_row(start = "105-1", college_english = "Life Science", uncontested_election = "uncontested", elected = "lost", n = 0) %>%
  add_row(start = "105-5", college_english = "Public Health", uncontested_election = "uncontested", elected = "lost", n = 0) %>%
  ggplot(aes(start, n, fill = elected, color = uncontested_election)) +
  geom_col() +
  # scale_color_manual(values=c("#56B4E9","#E69F00")) +
  scale_color_manual(values=c("black","white")) +
  # scale_fill_manual(values=c("#999999", "#E69F00")) + 
  # scale_y_continuous(labels =  scales::percent_format(accuracy = 1L),
  #                    limits = c(0, 0.2),
  #                    breaks = c(0, 0.1, 0.2)) +
  facet_wrap(~college_english, ncol = 4, scale="free_x") +
  theme(panel.spacing.y = unit(1, "lines")) + 
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) + 
  theme(legend.position = "bottom") +
  # geom_text(aes(label=vote_rate_label),hjust=+0.5, vjust=-1, size=3) +
  # geom_point(aes(term, vote_rate_label_dot)) +
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "HP", face = "plain", size=12)) +
  labs(title = "To Lose a Student Congress Election Was Not Easy at NTU") +
  labs(subtitle = "College of Law and Social Sciences Students Fought Relatively Hard for Being Elected") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\nterm") +
  ylab("candidate number") +
  theme(axis.title = element_text(family = "HP", face = "plain"), 
        legend.title = element_text(family = "HP", face = "plain"), 
        legend.text = element_text(family = "HP", face = "plain"), 
        plot.title = element_text(family = "HP", face = "plain"), 
        plot.subtitle = element_text(family = "HP", face = "italic"), 
        plot.caption = element_text(family = "HP", face = "plain")) 

###
df_attnd_vote %>%
  mutate(college_english = if_else(str_detect(college_english, "Computer"), "EE & CS", if_else(str_detect(college_english, "Bio"), "Bio & Agr", college_english ))) %>%
  filter(start != "103-2") %>%
  group_by(college_english) %>%
  mutate(candidate_college_order = n()) %>%
  ungroup() %>%
  mutate(college_english = fct_reorder(as.factor(college_english), -candidate_college_order)) %>%
  mutate(elected = if_else(is.na(elected), "lost", "won")) %>%
  mutate(uncontested_election = if_else(competitive == 0, "uncontested", "contested")) %>%
  mutate(uncontested_election = as.factor(uncontested_election),
         elected = as.factor(elected)) %>% 
  mutate(competitive = as.factor(competitive)) %>% 
  ggplot(aes(college_support_rate , Attnd_Rate, color = uncontested_election)) +
  geom_point() +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L)) + 
  scale_x_continuous(labels =  scales::percent_format(accuracy = 1L)) + 
  facet_wrap(.~uncontested_election, ncol = 4, scale="free_x") + 
  geom_smooth(method='lm',formula=y~x) +
  theme(panel.spacing.y = unit(1, "lines")) + 
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) + 
  theme(legend.position = "bottom") +
  # geom_text(aes(label=vote_rate_label),hjust=+0.5, vjust=-1, size=3) +
  # geom_point(aes(term, vote_rate_label_dot)) +
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "HP", face = "plain", size=12)) +
  labs(title = "Failed to Find Relationships Between Election Support and Congress Attendance") +
  labs(subtitle = "On Average Congress Members Elected from Contested Election Attended More") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\n(support vote / college total vote)") +
  ylab("attendance rate") +
  theme(axis.title = element_text(family = "HP", face = "plain"), 
        legend.title = element_text(family = "HP", face = "plain"), 
        legend.text = element_text(family = "HP", face = "plain"), 
        plot.title = element_text(family = "HP", face = "plain"), 
        plot.subtitle = element_text(family = "HP", face = "italic"), 
        plot.caption = element_text(family = "HP", face = "plain")) 

### 測試用
df_attnd_vote %>% 
  mutate(competitive = as.factor(competitive)) %>% 
  ggplot(aes(college_support_rate , Attnd_Rate, color = competitive)) +
  geom_point() +
  facet_wrap(.~college_english, ncol = 4, scale="free_x") + 
  geom_smooth(method='lm',formula=y~x)

df_attnd_vote %>% 
  mutate(competitive = as.factor(competitive)) %>% 
  ggplot(aes(college_support_rate , Attnd_Rate, color = competitive)) +
  geom_point() + 
  geom_smooth(method='lm',formula=y~x)


df_attnd_vote %>% 
  mutate(competitive = as.factor(competitive)) %>% 
  ggplot(aes(vote_support , Attnd_Rate, color = competitive)) +
  geom_point() +
  facet_wrap(.~college_english, ncol = 4, scale="free_x")


df_attnd_vote %>% 
  ggplot(aes(vote_support , Attnd_Rate)) +#, color = as.factor())) +
  geom_jitter() +
  facet_grid(competitive~term, scale="free_x") + 
  geom_smooth(method='lm',formula=y~x)

df_attnd_vote %>% 
  ggplot(aes(college_support_rate , Attnd_Rate)) +#, color = as.factor())) +
  geom_jitter() +
  facet_grid(competitive~term, scale="free_x") + 
  geom_smooth(method='lm',formula=y~x)
  facet_wrap(competitive~term, ncol = 5, scale="free_x")


df_attnd_vote %>%
  filter(term != "103-2") %>%
  # group_by(college_english, term, Attnd, ) %>%
  count(Attnd) %>%
  mutate(Attnd_yes = if_else(Attnd == "出席", n, as.integer(0))) %>%
  mutate(Attnd_Rate = n/sum(n)) %>%
  mutate(Attnd_Rate_label = if_else(term %in% c("104-1", "106-1"), as.character(formattable::percent(Attnd_Rate, digits = 0)), NA_character_)) %>%
  mutate(Attnd_Rate_label_dot = if_else(term %in% c("104-1", "106-1"), formattable::percent(Attnd_Rate), as.double(NA))) %>%
  ungroup() %>%
  group_by(college_english) %>%
  mutate(Attnd_Rate_college_order = sum(Attnd_yes, na.rm = T)/sum(n, na.rm = T))  %>%
  ungroup() %>%
  filter(Attnd == "出席")
df_candidate %>% 
  mutate(elected = if_else(!is.na(elected), "yes", "not")) %>%
  mutate(elected = as.factor(elected)) %>%
  ggplot(aes(support, object, color = elected)) +
  geom_jitter() +
  scale_y_continuous(limits = c(0, 300),
                     breaks = c(0, 100, 200, 300)) + 
  scale_x_continuous(limits = c(0, 300),
                     breaks = c(0, 100, 200, 300))

  
df_candidate %>% 
  mutate(elected = if_else(!is.na(elected), "yes", "not")) %>%
  mutate(elected = as.factor(elected)) %>%
  ggplot(aes(support, object, color = elected)) +
  geom_jitter() +
  scale_y_continuous(limits = c(0, 300),
                     breaks = c(0, 100, 200, 300)) + 
  scale_x_continuous(limits = c(0, 300),
                     breaks = c(0, 100, 200, 300))

NTU_Vote %>% tail()

df_sc_attnd %>%
  group_by(name, start, end, dept, college) %>%
  count(Attnd) %>%
  mutate(per = n/sum(n)) %>%
  ungroup() %>%
  filter(Attnd == '出席') %>%
  left_join(df_vote_college) %>%
  ggplot(aes(per, college_support_rate)) +
  geom_point()

df_sc_attnd %>%
  group_by(name, start, end, dept, college) %>%
  count(Attnd) %>%
  mutate(per = n/sum(n)) %>%
  ungroup() %>%
  filter(Attnd == '出席') %>%
  left_join(df_vote_college) %>%
  filter(is.na(competitive))  %>%
  ggplot(aes(per, college_support_rate, color = as.factor(competitive))) +
  geom_point()

df_vote_college %>%
  select(college_support_rate, college_vote_rate)
# NTU_Vote %>% filter(college_support_rate > .9) %>% View()
df_vote_college %>%
  count(start, competitive)
  filter(is.na(competitive)) %>%
  count(start)
  View()