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

### load date
df_sc_attnd_t <- read_csv("df_sc_attnd.csv")
df_sc_attnd <- df_sc_attnd_t %>% filter(degree == '大學部')
df_vote_college <- read_csv("df_vote_college.csv")
df_college_dept_eng <- read_csv("df_college_dept_eng.csv")
df_college <- read_csv("df_college_final.csv")
df_candidate <- read_csv("df_candidate_final.csv")

df_college_f <- df_college %>%
  left_join(distinct(df_college_dept_eng, dept_college, college_english, college_english_ab,concentration), by = c("college" = "dept_college")) %>%
  mutate(college_english = if_else(str_detect(college_english, "Computer"), "EE & CS", if_else(str_detect(college_english, "Bio"), "Bio & Agr", college_english )))

x11()
##################投票率各種嘗試

#投票率折線圖_五學期 vs.所有學院

df_college_avg <-
  df_college_f %>%
  filter(term != "103-2") %>%
  group_by(term) %>%
  summarise(vote = sum(vote, na.rm = T), all = sum(all, na.rm = T)) %>%
  mutate(vote_rate = vote/all) %>%
  left_join(
    df_college_f %>%
      filter(term != "103-2") %>%
      group_by(college_english) %>%
      mutate(vote_rate_college_order = sum(vote, na.rm = T)/sum(all, na.rm = T)) %>%
      ungroup() %>%
      mutate(college_english = fct_reorder(as.factor(college_english), -vote_rate_college_order)) %>%
      distinct(term, college_english)
  )

df_college_f %>%
  filter(term != "103-2") %>%
  group_by(college_english) %>%
  mutate(vote_rate_min = min(vote_rate, na.rm = TRUE), vote_rate_max = max(vote_rate, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(college_english_vote_rate = str_c(college_english,"_", as.character(vote_rate)),
         college_english_vote_rate_min = str_c(college_english,"_",  as.character(vote_rate_min)),
         college_english_vote_rate_max = str_c(college_english,"_",  as.character(vote_rate_max))) %>%
  group_by(college_english) %>%
  mutate(vote_rate_label = if_else(college_english_vote_rate == college_english_vote_rate_min | college_english_vote_rate == college_english_vote_rate_max, as.character(formattable::percent(vote_rate, digits = 0)), NA_character_)) %>%
  mutate(vote_rate_label_dot = if_else(college_english_vote_rate == college_english_vote_rate_min | college_english_vote_rate == college_english_vote_rate_max, vote_rate, as.double(NA))) %>%
  ungroup() %>%
  group_by(college_english) %>%
  mutate(vote_rate_college_order = sum(vote, na.rm = T)/sum(all, na.rm = T)) %>%
  ungroup() %>%
  mutate(college_english = fct_reorder(as.factor(college_english), -vote_rate_college_order)) %>%
  # distinct(college_english, vote_rate_college_order) %>% arrange(college_english)
  ggplot(aes(term, vote_rate, group = college_english)) +
  geom_line(colour = "red2", size = .8) +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L),
                     limits = c(0, 0.2),
                     breaks = c(0, 0.1, 0.2)) +
  facet_wrap(~college_english, ncol = 4, scale="free_x") +
  theme(panel.spacing.y = unit(1, "lines")) + 
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) + 
  geom_text(aes(label=vote_rate_label),hjust=+0.5, vjust=-1, size=3) +
  geom_point(aes(term, vote_rate_label_dot)) +
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "HP", face = "plain", size=12)) +
  labs(title = "Turnout Rate Diverged across Colleges in NTU Student Congress Election") +
  #Turnout from 2014 to 2018 increased in every district except two
  labs(subtitle = "College of Law and Social Sciences students were more willing to vote") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\nterm") +
  ylab("vote rate") +
  theme(axis.title = element_text(family = "HP", face = "plain"), 
        legend.title = element_text(family = "HP", face = "plain"), 
        legend.text = element_text(family = "HP", face = "plain"), 
        plot.title = element_text(family = "HP", face = "plain"), 
        plot.subtitle = element_text(family = "HP", face = "italic"), 
        plot.caption = element_text(family = "HP", face = "plain")) +
  geom_line(data=df_college_avg, aes(term, vote_rate), colour="grey50")

#投票率散點圖_五學期 vs.所有學院
#這邊不太適合用類組因為管院就有兩個類組
df_college_f %>% 
  rename(Voting_eligible_population = all) %>%
  mutate(uncontested_election = if_else(competitive == "no", "uncontested", "contested")) %>%
  filter(term != "103-2") %>%
  ggplot(aes(Voting_eligible_population, vote_rate, group = term)) +
  geom_point() +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L)) + 
  scale_x_continuous(limits = c(0, 6000),
                     breaks = c(1000, 3000, 5000)) +
  facet_wrap(~term, ncol = 5, scale="free_x") +
  # facet_grid(~term, scale="free_x") +
  theme(panel.spacing.x = unit(.5, "lines")) + 
  theme(strip.background = element_rect(fill="white"),
        strip.placement = "inside") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        panel.background = element_rect(fill = "#fafafa"),
        axis.line = element_line(colour = "black")) +
  geom_label(aes(label=college_english_ab, color = uncontested_election),hjust=+0.3, vjust=1.2, size=3, family = "HP") +
  scale_color_manual( values=c("blue","red","#8abbd0","#e3120b"))+
  theme(strip.text.x = element_text(family = "HP", face = "plain")) + 
  xlab("\nvoting-eligible population") +
  ylab("vote rate") +
  labs(title = "NTU Students Tended Not to Vote Nor Run for Student Congress") +
  labs(subtitle = "Uncontested Elections Occured more than Contested Elections ") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") +
  labs(color = "Uncontested Election") +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(family = "HP", face = "plain"), 
        legend.title = element_text(family = "HP", face = "plain"), 
        legend.text = element_text(family = "HP", face = "plain"), 
        plot.title = element_text(family = "HP", face = "plain"), 
        plot.subtitle = element_text(family = "HP", face = "italic"), 
        plot.caption = element_text(family = "HP", face = "plain"))

#投票率長條圖_五學期 vs.所有學院
#可以不用這張
df_college_f %>%
  filter(term != "103-2") %>%
  group_by(term) %>%
  arrange(term, vote_rate) %>%
  ungroup() %>%
  # 3. Add order column of row numbers
  mutate(order = row_number()) %>%
  ggplot(aes(x = order, y = vote_rate)) +
  geom_col() +
  # geom_line(colour = "red2", size = .8) +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L)) + 
  facet_wrap(~term, ncol = 5, scale="free_x") +
  geom_text(aes(label=college_english),hjust=-0.1, vjust=2, size=3) +
  theme(strip.text.x = element_text(face = "bold")) +
  labs(title = "NTU Students Congress Vote Rate")

#投票率長條圖_五學期 vs.所有學院
#跟上一張一樣只是 X Y 軸互相調換
#可以不用這張
df_college_f %>%
  filter(term != "103-2") %>%
  group_by(term) %>%
  arrange(term, vote_rate) %>%
  # mutate(college_english = reorder(college_english, -vote_rate)) %>%
  ungroup() %>%
  # 3. Add order column of row numbers
  mutate(order = row_number()) %>%
  ggplot(aes(x = college_english, y = vote_rate)) +
  geom_col() +
  facet_wrap(~term, ncol = 5, scale="free_x") +
  coord_flip()

## 关闭showtext字体设置
showtext_auto(FALSE)
# library(extrafont)
# font_import();y
# loadfonts(device = "win")