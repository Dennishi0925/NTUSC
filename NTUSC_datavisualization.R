library(tidyverse)
library(knitr)
library(kableExtra)
?sidebarLayout
df_candidate <- read_rds("D:/R_programming/Journalism_homework/NTUSC/candidate")
df_college <- read_rds("D:/R_programming/Journalism_homework/NTUSC/college")
data_pornot <- read_rds("D:/R_programming/Journalism_homework/NTUSC/datapor")
data_pornot %>%
  filter(term == "1041") %>%
  select(name, term, keys, present) %>%
  mutate(dummyp = if_else(present == "not", 0, 1)) %>%
  #group_by(name, term, keys, present) %>%
  #count() %>%
  #ungroup %>%
  #group_by(college, name, dept, term, grade) %>%
  #mutate(percent = n/sum(n)) %>%
  #ungroup %>%
  ggplot(mapping = aes(x = factor(keys), y = factor(name), height =0.9)) +
  geom_tile(mapping = aes(fill = dummyp)) 


data_pornot %>%
  filter(term == "1041") %>%
  select(college, grade, dept, name, term, keys, present) %>%
  mutate(dummyp = if_else(present == "not", 0, 1)) %>%
  group_by(college, keys) %>%
  mutate(dpc = sum(dummyp)/n()) %>%
  #count() %>%
  #ungroup %>%
  #group_by(college, name, dept, term, grade) %>%
  #mutate(percent = n/sum(n)) %>%
  #ungroup %>%
  ggplot(mapping = aes(x = keys, y = factor(college), fill = dpc, height =0.9)) +
  geom_tile() +
  #scale_colour_gradient(low = "white", high = "black") +
  #facet_grid(week~., scales="free") +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.title.y = element_text(angle = 90, family = "BL", color = "black"),
        axis.text.x = element_text(angle = 60, family = "BL", hjust = 1, color = "black"),
        axis.title.x = element_text(family = "BL", color = "black", face = "bold"))
data_pornot %>%
  select(college, grade, dept, name, term, keys, present) %>%
  mutate(college = factor(college), term = factor(term)) %>%
  group_by(college, term, present) %>%
  count() %>%
  ungroup %>%
  group_by(college, term) %>%
  mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
  filter(present == "present") %>%
  ungroup %>%
  ggplot(aes(term, percent)) +
  geom_col(width = 0.7, fill="lightblue", colour="black") +
  #geom_line(aes(term, percent, group = college)) +
  facet_wrap(~college, ncol = 4) +
  theme(strip.background = element_rect(fill="pink"))

data_pornot %>%
  select(college, grade, dept, name, term, keys, present) %>%
  mutate(grade = factor(grade), term = factor(term)) %>%
  group_by(grade, term, present) %>%
  count() %>%
  ungroup %>%
  group_by(grade, term) %>%
  mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
  filter(present == "present") %>%
  ungroup %>%
  ggplot(aes(term, percent)) +
  geom_col(width = 0.7, fill="lightblue", colour="black") +
  #geom_line(aes(term, percent, group = college)) +
  facet_wrap(~grade, ncol = 5) +
  theme(strip.background = element_rect(fill="pink"))

data_pornot %>%
  select(college, grade, dept, name, term, keys, present) %>%
  mutate(college = factor(college), term = factor(term)) %>%
  group_by(keys, term, present) %>%
  count() %>%
  ungroup %>%
  group_by(keys, term) %>%
  mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
  filter(present == "present") %>%
  ungroup %>%
  ggplot(aes(keys, percent)) +
  geom_col(width = 0.7, fill="lightblue", colour="black") +
  #geom_line(aes(term, percent, group = college)) +
  facet_wrap(~term, ncol = 5, scale="free_x") +
  theme(strip.background = element_rect(fill="pink"))



data_pornot %>%
  filter(term == "1041") %>%
  select(college, name, dept, term, grade, values) %>%
  group_by(college, name, dept, term, grade, values) %>%
  count() %>%
  ungroup %>%
  group_by(college, name, dept, term, grade) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup 




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