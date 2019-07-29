library(readr)
library(tidyverse)
library(gdata)
options(stringsAsFactors = F)
# Sys.setlocale(locale = "UTF-8")
# Sys.setlocale(category = "LC_ALL", locale = "cht")
# rm(list=ls())

###### NTUSC Attendance Data 台大學代出席資料 

### 探索性資料分析

### 讀入檔案
df_sc_attnd_t <- read_csv("df_sc_attnd.csv")
df_sc_attnd <- df_sc_attnd_t %>% filter(degree == '大學部')
df_vote_college <- read_csv("df_vote_college.csv")

# 總體出席
df_sc_attnd %>%
  group_by(Attnd) %>%
  count()

df_sc_attnd %>%
  group_by(college, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  filter(Attnd == "出席") %>%
  arrange(desc(n), desc(percent))

# 按照人來看出席
df_sc_attnd %>%
  group_by(name, Attnd) %>%
  count() %>%
  filter(Attnd != "出席") %>%
  arrange(desc(n))

# 按照院來看出席
df_sc_attnd %>%
  group_by(college, Attnd) %>%
  count() %>%
  filter(Attnd != "出席") %>%
  arrange(desc(n))

# 抓出院的翹王
df_sc_attnd %>%
  group_by(college, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  ungroup %>%
  filter(Attnd == "未請假缺席") %>%
  arrange(desc(percent))

# 抓出院的請假王
df_sc_attnd %>%
  group_by(college, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  ungroup %>%
  filter(Attnd == "請假缺席") %>%
  arrange(desc(percent))

# 抓出院的不出席王
df_sc_attnd %>%
  group_by(college, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  filter(Attnd == "未請假缺席" |Attnd == "請假缺席") %>%
  summarise(not_present = sum(percent)) %>%
  ungroup %>%
  arrange(desc(not_present))

# 抓出系的翹王
df_sc_attnd %>%
  group_by(dept, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(dept) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  ungroup %>%
  filter(Attnd == "未請假缺席") %>%
  arrange(desc(percent))

# 抓出系的請假王
df_sc_attnd %>%
  group_by(dept, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(dept) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  ungroup %>%
  filter(Attnd == "請假缺席") %>%
  arrange(desc(percent))

# 抓出系的不出席王
df_sc_attnd %>%
  group_by(dept, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(dept) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  filter(Attnd == "未請假缺席" |Attnd == "請假缺席") %>%
  summarise(not_present = sum(percent)) %>%
  ungroup %>%
  arrange(desc(not_present))

# 抓出年級的翹王
df_sc_attnd %>%
  group_by(degree, grade, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(degree, grade) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  ungroup %>%
  filter(Attnd == "未請假缺席") %>%
  arrange(desc(percent))

# 抓出年級的請假王
df_sc_attnd %>%
  group_by(degree, grade, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(degree, grade) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  ungroup %>%
  filter(Attnd == "請假缺席") %>%
  arrange(desc(percent))

# 抓出年級的不出席王
df_sc_attnd %>%
  group_by(degree, grade, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(degree, grade) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  filter(Attnd == "未請假缺席" |Attnd == "請假缺席") %>%
  summarise(not_present = sum(percent)) %>%
  ungroup %>%
  arrange(desc(not_present))

### 增設出席與否的欄位
df_sc_attnd_present <- df_sc_attnd %>%
  mutate(present = ifelse(Attnd == "出席", "出席", "缺席"),
         degree_grade = str_c(degree, grade))

# 查看出席情況
df_sc_attnd_present %>%
  group_by(college, present) %>%
  count() %>%
  ungroup %>%
  group_by(college) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(college, percent), y = percent, fill = present)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel1")

# 出席與不出席vs.院別
df_sc_attnd_present %>%
  group_by(college, present) %>%
  count() %>%
  ungroup() %>%
  group_by(college) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  ungroup %>%
  ggplot(aes(x = college, y = percent, fill = present)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel1")

# 出席與不出席vs.年級
df_sc_attnd_present %>%
  group_by(degree_grade, present) %>%
  count() %>%
  ungroup %>%
  group_by(degree_grade) %>%
  mutate(percent = round(n/sum(n), 2)) %>%
  ungroup %>%
  ggplot(aes(x = degree_grade, y = percent, fill = present)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel1")

# 抓指標性人物來看資料有沒有錯誤
df_sc_attnd_present %>% filter(name == "周安履")

# 看各院的五學期學代人次總計
df_sc_attnd_present %>%
  distinct(college, name, term) %>%
  group_by(college) %>%
  count(sort = T) %>%
  ggplot(aes(x = college, y = n)) +
  geom_col(fill="lightblue", colour="white", width = 0.7) +
  geom_text(aes(y = n + 0.5, label= n), colour="black") +
  coord_flip()

# 看各院的五學期學代人數總計
df_sc_attnd_present %>%
  distinct(college, name) %>%
  group_by(college) %>%
  count(sort = T) %>%
  ggplot(aes(x = college, y = n)) +
  geom_col(fill="lightblue", colour="white", width = 0.7) +
  geom_text(aes(y = n + 0.5, label= n), colour="black") +
  coord_flip()

# 看各年級的五學期學代人次總計
df_sc_attnd_present %>%
  distinct(degree, grade, name, term) %>%
  group_by(degree, grade) %>%
  count(sort = T) %>%
  ggplot(aes(x = str_c(degree,grade), y = n)) +
  geom_col(fill="lightblue", colour="white", width = 0.7) +
  geom_text(aes(y = n + 0.5, label= n), colour="black") +
  coord_flip()

# 看各院的五學期學代人數總計
df_sc_attnd_present %>%
  distinct(college, name) %>%
  group_by(college) %>%
  count(sort = T) %>%
  ggplot(aes(x = college, y = n)) +
  geom_col(fill="lightblue", colour="white", width = 0.7) +
  geom_text(aes(y = n + 0.5, label= n), colour="black") +
  coord_flip()

# 看各院的五學期學代人數總計
df_sc_attnd_present %>%
  group_by(college, grade, dept) %>%
  count(sort = T) %>%
  ggplot(aes(x = college, y = grade, color = dept, size = n)) +
  geom_jitter(show.legend = FALSE, width = 0.3, height = 0) 

df_sc_attnd %>%
  #  group_by(college, grade) %>% #, start
  #  count(sort = T) %>%
  ggplot(aes(x = college, fill = factor(grade))) +
  geom_bar(width = 0.5) 

df_sc_attnd %>%
  distinct(college, name, degree) %>%
  ggplot(aes(x = college, fill = factor(degree))) +
  geom_bar(width = 0.5) 

df_sc_attnd %>%
  distinct(term, name, grade, degree) %>%
  ggplot(aes(x = term, fill = factor(str_c(degree, grade)))) +
  geom_bar(width = 0.5) 

df_sc_attnd %>%
  distinct(term, name, grade, degree) %>%
  group_by(term, grade, degree) %>%
  count() %>%
  ungroup() %>%
  mutate(labely = cumsum(n)) %>%
  ggplot(aes(x = term, y = n, fill = factor(str_c(degree, grade)))) +
  geom_col(width = 0.3) +
  geom_text(position = position_stack(vjust = .5), aes(label= n), size=4) + #aes(label=paste(format(n, nsmall=0), "people"))
  guides(fill=guide_legend(reverse=TRUE)) #+scale_fill_brewer(palette="Pastel1")
