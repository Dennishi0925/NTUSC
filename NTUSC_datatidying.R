rm(list=ls())
library(readr)
library(tidyverse)
library(gdata)
options(stringsAsFactors = F)
Sys.setlocale(locale = "UTF-8")
#a <- read.xls(xls ="D:/R_programming/Journalism_homework/NTUSC/106-1_processed.xls", sheet = 1, verbose=FALSE)
raw_1061 <- read.csv("D:/R_programming/Journalism_homework/NTUSC/106-1_processed.csv", na.strings = c("", "NA"))
#raw_1061 %>% View()
raw_1061 <- raw_1061 %>% as_tibble()
raw_1061$general[is.na(raw_1061$general)] <- "current"
raw_1061[is.na(raw_1061)] <- "present"
glimpse(raw_1061)
colnames(raw_1061)[str_detect(colnames(raw_1061), "Reg|spe|prep")] <- str_c("1061_", str_subset(colnames(raw_1061), "Reg|spe|prep"))
raw_1061 <- raw_1061 %>%
  mutate(ID = str_c("1061_", row_number()), term = "1061")
raw_1061$general <- str_c("1061_", raw_1061$general)

raw_1052 <- read.csv("D:/R_programming/Journalism_homework/NTUSC/105-2_processed.csv", na.strings = c("", "NA"))
#raw_1052 %>% View()
raw_1052 <- raw_1052 %>% as_tibble()
raw_1052$general[is.na(raw_1052$general)] <- "current"
raw_1052[is.na(raw_1052)] <- "present"
glimpse(raw_1052)
colnames(raw_1052)[str_detect(colnames(raw_1052), "Reg|spe|prep")] <- str_c("1052_", str_subset(colnames(raw_1052), "Reg|spe|prep"))
raw_1052 <- raw_1052 %>%
  mutate(ID = str_c("1052_", row_number()), term = "1052")
raw_1052$general <- str_c("1052_", raw_1052$general)

raw_1051 <- read.csv("D:/R_programming/Journalism_homework/NTUSC/105-1_processed.csv", na.strings = c("", "NA"))
#raw_1051 %>% View()
raw_1051 <- raw_1051 %>% as_tibble()
raw_1051$general[is.na(raw_1051$general)] <- "current"
raw_1051[is.na(raw_1051)] <- "present"
glimpse(raw_1051)
colnames(raw_1051)[str_detect(colnames(raw_1051), "Reg|spe|prep")] <- str_c("1051_", str_subset(colnames(raw_1051), "Reg|spe|prep"))
raw_1051 <- raw_1051 %>%
  mutate(ID = str_c("1051_", row_number()), term = "1051")
raw_1051$general <- str_c("1051_", raw_1051$general)

raw_1042 <- read.csv("D:/R_programming/Journalism_homework/NTUSC/104-2_processed.csv", na.strings = c("", "NA"))
#raw_1042 %>% View()
raw_1042 <- raw_1042 %>% as_tibble()
raw_1042$general[is.na(raw_1042$general)] <- "current"
raw_1042[is.na(raw_1042)] <- "present"
glimpse(raw_1042)
colnames(raw_1042)[str_detect(colnames(raw_1042), "Reg|spe|prep")] <- str_c("1042_", str_subset(colnames(raw_1042), "Reg|spe|prep"))
raw_1042 <- raw_1042 %>%
  mutate(ID = str_c("1042_", row_number()), term = "1042")
raw_1042$general <- str_c("1042_", raw_1042$general)

raw_1041 <- read.csv("D:/R_programming/Journalism_homework/NTUSC/104-1_processed.csv", na.strings = c("", "NA"))
#raw_1041 %>% View()
raw_1041 <- raw_1041 %>% as_tibble()
raw_1041$general[is.na(raw_1041$general)] <- "current"
raw_1041[is.na(raw_1041)] <- "present"
glimpse(raw_1041)
colnames(raw_1041)[str_detect(colnames(raw_1041), "Reg|spe|prep")] <- str_c("1041_", str_subset(colnames(raw_1041), "Reg|spe|prep"))
raw_1041 <- raw_1041 %>%
  mutate(ID = str_c("1041_", row_number()), term = "1041")
raw_1041$general <- str_c("1041_", raw_1041$general)

raw_1041_long <- gather(raw_1041, key = "keys", value = "values", '1041_prep_0915':'1041_Reg08_0115')
raw_1042_long <- gather(raw_1042, key = "keys", value = "values", '1042_prep_0223':'1042_Reg08')
raw_1051_long <- gather(raw_1051, key = "keys", value = "values", '1051_prep_0913':'1051_Reg08_0113')
raw_1052_long <- gather(raw_1052, key = "keys", value = "values", '1052_prep_0220':'1052_special_0704')
raw_1061_long <- gather(raw_1061, key = "keys", value = "values", '1061_prep_0913':'1061_special_0130')
raw_1061_long %>% View()
all_long <- rbind(raw_1041_long,raw_1042_long,raw_1051_long,raw_1052_long,raw_1061_long) %>% as_tibble()
all_long %>% View()
all_long %>%
  group_by(college) %>%
  count(sort = T)
all_long$college[all_long$college== "電資院"] <- "電機資訊學院"
all_long$college[all_long$college== "公衛學院"] <- "公共衛生學院"
all_long$college[all_long$college== "法學院"] <- "法律學院"
all_long$college[all_long$college== "生農院"] <- "生物資源暨農學院"
all_long$college[all_long$college== "商學院"] <- "管理學院"
all_long %>% View()
all_long %>%
  group_by(college) %>%
  count(sort = T)
all_long %>%
  group_by(dept) %>%
  count(sort = T) %>%
  View()
all_long %>%
  filter(str_detect(dept, "政治"))
con <- file("D:/R_programming/Journalism_homework/NTUSC/all_long.csv",encoding="UTF-8")
write.csv(all_long,file=con)
### read after processing department
options(stringsAsFactors = F)
all_long_new <- read.csv("D:/R_programming/Journalism_homework/NTUSC/all_long_new.csv", na.strings = c("", "NA"))
all_long_new[all_long_new$name == "許皓雲" & all_long_new$start == "105-1", 'dept'] <- "物理系"
all_long_new[all_long_new$name == "許皓雲" & all_long_new$start == "105-1", 'grade'] <- "3"
all_long_new[all_long_new$name == "許皓雲" & all_long_new$term == "1052", 'grade'] <- "4"

all_long_new %>%
  group_by(college) %>%
  count(sort = T)
all_long_new %>%
  group_by(dept) %>%
  count(sort = T)
all_long_new %>%
  filter(str_detect(general, "unreport*")) %>%
  View()


all_long_new %>%
  filter(str_detect(name, "劉昱辰")) %>%
  #filter(start == "104-1") %>%
  group_by(name) %>%
  arrange(name) %>%
  View()
all_long_new %>%
  filter(str_detect(name, "周安履")) %>%
  #filter(start == "104-1") %>%
  group_by(name) %>%
  arrange(name) %>%
  View()



#all_long_new %>%
#  filter(str_detect(name, "周安履|黃泊羲|許皓雲|許軒瑋|劉哲銘|高章琛|林子期")) %>%
#  filter(general == "1051_current") %>% View()
#  mutate(grade = as.numeric(grade)) %>%
#  mutate(start = "105_1", end = "105_2", grade = as.character(grade + 1))
#View(all_long_new)
#as.character()
#all_long_new$start
change <- str_detect(all_long_new$name,"周安履|黃泊羲|許皓雲|許軒瑋|劉哲銘|高章琛|林子期") & all_long_new$term == "1051"
all_long_new[change, 'start'] <- "105_1"
all_long_new[change, 'end'] <- "105_2"
all_long_new[change, 'grade'] <- as.numeric(all_long_new[change, 'grade']) + 1
all_long_new[change, 'grade'] <- as.character(all_long_new[change, 'grade'])

change2 <- str_detect(all_long_new$name,"吳音妮|高慕曦|劉昱辰|吳政融|周允梵|顧文傑|陳育宏|李政軒|林冠亨|羅京|趙振辰|曾靖容|洪敦昱|張聿棨|薛沛宜|林後維|劉昱佑|陳彥霖|彭子淇|何智凡|吳侑霖|羅啟仁|蕭奕翎|吳振寧|高運晅") & all_long_new$term == "1051"
all_long_new[change2, 'grade'] <- as.numeric(all_long_new[change2, 'grade']) + 1
all_long_new[change2, 'grade'] <- as.character(all_long_new[change2, 'grade'])
all_long_new <- all_long_new %>% as_tibble() %>% select(-1)

con2 <- file("D:/R_programming/Journalism_homework/NTUSC/data.csv",encoding="UTF-8")
write.csv(all_long_new,file=con2)
