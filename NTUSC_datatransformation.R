library(tidyverse)
options(stringsAsFactors = T)


write_rds(df_candidate, "candidate")
write_rds(df_college, "college")

glimpse(candidate)
candidate <- read_csv("D:/R_programming/Journalism_homework/NTUSC/candidate_rate.csv")
college <- read_csv("D:/R_programming/Journalism_homework/NTUSC/college_vote_rate.csv")
#View(candidate)
#View(college)
glimpse(candidate)
glimpse(college)
total_vote <- candidate %>%
  group_by(term, college) %>%
  summarise(total_sup = sum(support))
df_candidate <- candidate %>%
  mutate(elected = if_else(elected == "V", 1, 0)) %>%
  left_join(total_vote) %>%
  mutate(sup_percent = support/total_sup, elected = factor(elected)) %>%
  mutate(total_vote = ifelse(is.na(object), NA, (support + object +invalid))) %>%
  mutate(nonobj_percent = support/total_vote) %>%
  filter(elected == 1) %>%
  select(term, college, name, elected, sup_percent, nonobj_percent)
df_candidate$term <- str_c(str_sub(df_candidate$term, 1,3),str_sub(df_candidate$term, 5,5))

college$vote_rate %>% str()
total_people <- college %>% filter(!is.na(all)) %>%
  group_by(term) %>%
  summarise(sum(all))
college <- college %>%
  mutate(vote_rate = vote/all, void_rate = void/vote) %>%
  left_join(total_people, c("term" = "term")) %>%
  rename(total = "sum(all)") %>%
  mutate(population_rate = all/total)
df_college <- college %>%
  select(term, college, vote_rate, competitive, void_rate, population_rate) %>%
  mutate(competitive = if_else(competitive == "yes", 1, 0)) %>%
  mutate(competitive = factor(competitive))
#df_college %>% View()

data <- read_csv("D:/R_programming/Journalism_homework/NTUSC/data.csv")
glimpse(data)
write_rds(data, "D:/R_programming/Journalism_homework/NTUSC/datards")
data <- read_rds("D:/R_programming/Journalism_homework/NTUSC/datards")

write_rds(data_pornot, "D:/R_programming/Journalism_homework/NTUSC/datapor")
data_pornot <- read_rds("D:/R_programming/Journalism_homework/NTUSC/datapor")
View(data_pornot)
#rm(list=ls())
View(data)

# 總體出席
data %>%
  group_by(values) %>%
  count()

data %>%
  group_by(college, values) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  filter(values == "present") %>%
  arrange(desc(percent)) %>%
  View()

  # 按照人來看出席
data %>%
  group_by(name, values) %>%
  count() %>%
  filter(values != "present") %>%
  arrange(desc(n))
# 按照院來看出席
data %>%
  #filter(str_detect(general, "current")) %>%
  group_by(college) %>%
  count() %>%
  arrange(desc(n))

# 抓出院的翹王
data %>%
  group_by(college, values) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "skip") %>%
  arrange(desc(percent))
# 抓出院的請假王
data %>%
  group_by(college, values) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "leave") %>%
  arrange(desc(percent))
# 抓出院的不出席王
data %>%
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
data %>%
  group_by(dept, values) %>%
  count() %>%
  ungroup %>%
  group_by(dept) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  ungroup %>%
  filter(values == "skip") %>%
  arrange(desc(percent)) %>%
  View()
# 抓出院的請假王
data %>%
  group_by(dept, values) %>%
  count() %>%
  ungroup %>%
  group_by(dept) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "leave") %>%
  arrange(desc(percent))
# 抓出院的不出席王
data %>%
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
data %>%
  group_by(grade, values) %>%
  count() %>%
  ungroup %>%
  group_by(grade) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "skip") %>%
  arrange(desc(percent))
# 抓出院的請假王
data %>%
  group_by(grade, values) %>%
  count() %>%
  ungroup %>%
  group_by(grade) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  filter(values == "leave") %>%
  arrange(desc(percent))
# 抓出院的不出席王
data %>%
  group_by(grade, values) %>%
  count() %>%
  ungroup %>%
  group_by(grade) %>%
  mutate(percent = n/sum(n)) %>%
  filter(values == "leave" |values == "skip") %>%
  summarise(not_present = sum(percent)) %>%
  ungroup %>%
  arrange(desc(not_present))

data_pornot <- data %>%
  mutate(present = ifelse(values == "present", "present", "not"))
data_pornot %>% View()


data_pornot %>%
  group_by(college, present) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(college, percent), y = percent, fill = present)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel1")

# 出席與不出席vs.院別
data_pornot %>%
  group_by(college, present) %>%
  count() %>%
  ungroup() %>%
  group_by(college) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  ggplot(aes(x = college, y = percent, fill = present)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel1")
data_pornot %>% filter(name == "周安履") %>% View()
data_pornot %>%
  count(dept) %>%
  arrange(n)
# 出席與不出席vs.年級
data_pornot %>%
  mutate()
  group_by(grade, present) %>%
  count() %>%
  ungroup %>%
  group_by(grade) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  ggplot(aes(x = college, y = percent, fill = present)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel1")
# 按照年級來看出席
data %>%
  filter(general == "current") %>%
  group_by(grade) %>%
  count()
b <- data %>%
  
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



raw_1041 %>%
  group_by(college) %>%
  count(sort = T) %>%
  ggplot(aes(x = college, y = n)) +
  geom_col(fill="lightblue", colour="white", width = 0.7) +
  geom_text(aes(y = n + 0.5, label= n), colour="black")

raw_1041 %>%
  group_by(grade) %>%
  count(sort = T) %>%
  ggplot(aes(x = grade, y = n)) +
  geom_col(fill="lightblue", colour="white", width = 0.7) +
  geom_text(aes(y = n + 1, label= n), colour="black")

raw_1041 %>%
  group_by(college, grade, dept) %>%
  count(sort = T) %>%
  ggplot(aes(x = college, y = grade, color = dept, size = n)) +
  geom_jitter(show.legend = FALSE, width = 0.3, height = 0) 

raw_1041 %>%
  #  group_by(college, grade) %>% #, start
  #  count(sort = T) %>%
  ggplot(aes(x = college, fill = factor(grade))) +
  geom_bar(width = 0.5) 

raw_1041 %>%
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
