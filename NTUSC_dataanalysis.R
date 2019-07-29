library(tidyverse)

#write_rds(df_all, "model")
#write_rds(data_pornot, "datapor")
data_pornot$start[data_pornot$start == "105_1"] <- "105-1"
data_pornot$end[data_pornot$end == "105_2"] <- "105-2"
data_pornot$start %>% unique()
df_candidate <- read_rds("D:/R_programming/Journalism_homework/NTUSC/candidate")
df_college <- read_rds("D:/R_programming/Journalism_homework/NTUSC/college")
data_pornot <- read_rds("D:/R_programming/Journalism_homework/NTUSC/datapor")
View(data_pornot)
#data_pornot %>%
#  filter(name == "劉哲銘") %>% View()
data_pornot$college %>% unique()
df_candidate$college %>% unique()
df_college$college %>% unique()
df_all <- data_pornot %>%
  #filter(term == "1041") %>%
  select(start, college, name, dept, grade, present) %>%
  group_by(college, name, dept, start, present) %>%
  count() %>%
  summarise(total_n = sum(n)) %>% 
  mutate(percent = total_n/sum(total_n)) %>%
  ungroup %>%
  #filter(name == "劉哲銘")# %>% 
  mutate(start = as.character(start)) %>%
  filter(present == "present") %>%
  select(college, name, dept, start, percent) %>% #View()
  left_join(df_candidate,
  by = c("name" = "name", "college" = "college", "start" = "term")) %>%
  arrange(college, dept, name) %>% 
  left_join(df_college, by = c("college" = "college", "start" = "term")) %>%
  select(-c(elected))

df_college$college %>% typeof()
data_pornot$college %>% typeof()
df_college$term %>% typeof()
data_pornot$start %>% typeof()
df_all %>% View()
