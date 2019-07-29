# library(readr)
library(tidyverse)
library(gdata)
options(stringsAsFactors = F)
# Sys.setlocale(locale = "UTF-8")
# Sys.setlocale(category = "LC_ALL", locale = "cht")
# rm(list=ls()); invisible(gc())
# source('D:/R_programming/Dennis.R')

##### 資料處理: NTUSC Attendance Data 台大學代出席資料 

### 讀入檔案
raw_1061 <- read.csv("106-1_processed.csv", na.strings = c("", "NA")) %>% as_tibble()
raw_1052 <- read.csv("105-2_processed.csv", na.strings = c("", "NA")) %>% as_tibble()
raw_1051 <- read.csv("105-1_processed.csv", na.strings = c("", "NA")) %>% as_tibble()
raw_1042 <- read.csv("104-2_processed.csv", na.strings = c("", "NA")) %>% as_tibble()
raw_1041 <- read.csv("104-1_processed.csv", na.strings = c("", "NA")) %>% as_tibble()

### 將每一個學期的每一次會期加上學期以便於辨認
colnames(raw_1061)[str_detect(colnames(raw_1061), "Reg|spe|prep")] <- str_c("1061_", str_subset(colnames(raw_1061), "Reg|spe|prep"))
colnames(raw_1052)[str_detect(colnames(raw_1052), "Reg|spe|prep")] <- str_c("1052_", str_subset(colnames(raw_1052), "Reg|spe|prep"))
colnames(raw_1051)[str_detect(colnames(raw_1051), "Reg|spe|prep")] <- str_c("1051_", str_subset(colnames(raw_1051), "Reg|spe|prep"))
colnames(raw_1042)[str_detect(colnames(raw_1042), "Reg|spe|prep")] <- str_c("1042_", str_subset(colnames(raw_1042), "Reg|spe|prep"))
colnames(raw_1041)[str_detect(colnames(raw_1041), "Reg|spe|prep")] <- str_c("1041_", str_subset(colnames(raw_1041), "Reg|spe|prep"))

### 因每一學期會期不同故欄位數目不同，且每一次會期召開日期不同，故將寬表格轉換成長表格以便後續分析
raw_1041_long <- gather(raw_1041, key = "session", value = "Attnd", '1041_prep_0915':'1041_Reg08_0115') %>% mutate(term = '104-1')
raw_1042_long <- gather(raw_1042, key = "session", value = "Attnd", '1042_prep_0223':'1042_Reg08') %>% mutate(term = '104-2')
raw_1051_long <- gather(raw_1051, key = "session", value = "Attnd", '1051_prep_0913':'1051_Reg08_0113') %>% mutate(term = '105-1')
raw_1052_long <- gather(raw_1052, key = "session", value = "Attnd", '1052_prep_0220':'1052_special_0704') %>% mutate(term = '105-2')
raw_1061_long <- gather(raw_1061, key = "session", value = "Attnd", '1061_prep_0913':'1061_special_0130') %>% mutate(term = '106-1')

### 合併所有常表格為單一表格，由於出席欄位中NA代表有出席，故將NA補上，並增加Attnd2將其翻譯成中文
all_long <- rbind(raw_1041_long,raw_1042_long,raw_1051_long,raw_1052_long,raw_1061_long) %>% as_tibble()
all_long$Attnd[is.na(all_long$Attnd)] <- "present"
all_long <- all_long %>%
  mutate(Attnd2 = if_else(Attnd == 'present', '出席', if_else(Attnd == 'skip', '未請假缺席', if_else(Attnd == 'leave', '請假缺席', '其他原因缺席'))))

### 處理學院名稱
all_long %>% distinct(college)
all_long$college[str_detect(all_long$college, ".*公.*衛.*") ] <- "公共衛生學院"
all_long$college[str_detect(all_long$college, ".*生.*農.*") ] <- "生物資源暨農學院"
# all_long$college[str_detect(all_long$college, ".*電.*資.*") ] <- "電機資訊學院"
# all_long$college[str_detect(all_long$college, ".*法.*") ] <- "法律學院"
# all_long$college[str_detect(all_long$college, ".*生.*農.*") ] <- "生物資源暨農學院"
# all_long$college[str_detect(all_long$college, ".*商.*") ] <- "管理學院"

### 處理系名稱不同狀況，主要有兩大問題，其一是研究所後綴詞，其二是同系異名
department_list <- all_long %>% 
  arrange(desc(college), desc(dept)) %>%
  distinct(dept) %>% pull()

### 先處理研究所後綴詞
department_list %>% str_extract(".*所|碩|博.*")
department_list %>% str_extract("博士班|所|研究所|碩.*")
all_long$dept[str_detect(all_long$dept, ".*所|碩|博.*")] <- str_replace(all_long$dept[str_detect(all_long$dept, ".*所|碩|博.*")], "博士班|所|研究所|碩.*", "研究所")
department_list_new <- all_long %>% 
  arrange(desc(college), desc(dept)) %>%
  distinct(dept) %>% pull()

### 處理同系/所異名問題
department_index_not_master <- !str_detect(all_long$dept, ".*研究所.*") 

### 怕過程搞錯先多一個變數dept2
all_long <- all_long %>% mutate(dept2 = dept)
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*法.*") ] <- "法律系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*政.*") ] <- "政治系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*工.*管.*") ] <- "工管系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*會.*計.*") ] <- "會計系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*資.*管.*") ] <- "資管系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*國.*企.*") ] <- "國企系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*財.*金.*") ] <- "財金系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*護.*") ] <- "護理系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*醫.*學.*") ] <- "醫學系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*電.*機.*") ] <- "電機系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*資.*工.*") ] <- "資工系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*物.*理.*") ] <- "物理系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*地.*理.*") ] <- "地理系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*心.*理.*") ] <- "心理系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*大.*氣.*") ] <- "大氣系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*經.*濟.*") ] <- "經濟系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*社.*會.*") ] <- "社會系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*社.*工.*") ] <- "社工系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*農.*經.*") ] <- "農經系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*農.*化.*") ] <- "農化系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*森.*林.*") ] <- "森林系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*動.*科.*") ] <- "動科系" 
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*昆.*蟲.*") ] <- "昆蟲系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*生.*機.*") ] <- "生機系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*生.*科.*") ] <- "生科系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*生.*傳.*") ] <- "生傳系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*生.*工.*") ] <- "生工系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*生.*技.*") ] <- "生技系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*戲.*劇.*") ] <- "戲劇系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*歷.*史.*") ] <- "歷史系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*圖.*資.*") ] <- "圖資系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*外.*文.*") ] <- "外文系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*日.*文.*") ] <- "日文系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*中.*文.*") ] <- "中文系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*人.*類.*") ] <- "人類系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*人.*類.*") ] <- "人類系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*公.*衛.*") ] <- "公衛系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*機.*械.*") ] <- "機械系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*化.*工.*") ] <- "化工系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*工.*海.*") ] <- "工海系"
all_long$dept2[department_index_not_master & str_detect(all_long$dept2, ".*土.*木.*") ] <- "土木系"
department_list_new_master <- all_long %>% 
  filter(!department_index_not_master) %>%
  distinct(dept2) %>% pull()

### 研究所也有同所異名問題
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*土.*木.*") ] <- "土木研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*化.*工.*") ] <- "化工研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*國.*企.*") ] <- "國企研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*電.*機.*") ] <- "電機研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*電.*信.*") ] <- "電信研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*電.*子.*") ] <- "電子研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*資.*工.*") ] <- "資工研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*法.*律.*") ] <- "法律研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*地.*理.*") ] <- "地理研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*政.*治.*") ] <- "政治研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*工.*工.*") ] <- "工工研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*公.*事.*") ] <- "公事研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*天.*文.*") ] <- "天文研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*生.*機.*") ] <- "生機研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*生.*化.*") ] <- "生化研究所"
all_long$dept2[!department_index_not_master & str_detect(all_long$dept2, ".*職.*衛.*") ] <- "職衛研究所"

### 因為大學部的大一與研究所的研一實際上不同所以增加學位欄位
all_long <- all_long %>%
  mutate(degree = if_else(str_detect(dept2, "系"), "大學部", "研究所"))

### 檢查系所和整體有沒有NA後存檔
# all_long$na_check <- apply(is.na(all_long), 1, sum) 
# View(all_long)
all_long %>% select(dept2) %>% distinct(dept2) %>% pull()
sum(is.na(all_long))

df_all_long <- 
  all_long %>% select(-c(dept, Attnd)) %>% rename(dept = dept2, Attnd = Attnd2) %>%
  select(term, degree, college, dept, start, end, name, grade, general, session, Attnd)

### 加上完整的院系資訊
### 已經有整理好的學院系所英文的dataframe
library(psych)
library(clipr)
# df_college_dept_eng <- read.clipboard.tab(header = T) %>% as_tibble()
# df_college_dept_eng %>% mutate(college_english_ab = case_when(
#   str_detect(college_english, "Computer") ~ "EE & CS",
#   str_detect(college_english, "Agr") ~ "Bio & Agri",
#   str_detect(college_english, "Man") ~ "Mgmt",
#   str_detect(college_english, "Med") ~ "Med",
#   str_detect(college_english, "Eng") ~ "Engr",
#   str_detect(college_english, "Public") ~ "PH",
#   str_detect(college_english, "Life") ~ "Life Sci",
#   str_detect(college_english, "Social ") ~ "Soc Sci",
#   str_detect(college_english, "Sci") ~ "Sci",
#   str_detect(college_english, "Lib") ~ "LA",
#   TRUE ~ college_english)) %>%
#   write_csv("df_college_dept_eng.csv")
df_college_dept_eng <- read_csv("df_college_dept_eng.csv")

### 輸出整理後的出席檔案: df_sc_attnd.csv
df_all_long %>% 
  mutate(college = case_when(
    college == '公共衛生學院' ~ "公衛學院",
    college == '法律學院' ~ "法學院",
    college == '生命科學院' ~ "生科學院",
    college == '生物資源暨農學院' ~ "生農學院",
    college == '社會科學院' ~ "社科院",
    college == '電機資訊學院' ~ "電資學院",
    TRUE ~ college)) %>%
  left_join(distinct(select(df_college_dept_eng, -dept_college)), by = c("dept" = "dept_transfer")) %>%
  write_csv("df_sc_attnd.csv")
df_sc_attnd <- read_csv("df_sc_attnd.csv")

##### 資料處理: NTUSC Vote Data 台大學代選舉資料

### 讀入檔案
candidate <- read_csv("candidate_rate.csv")
college <- read_csv("college_vote_rate.csv")

### 查看資料
glimpse(candidate)
glimpse(college)

### 增加學院總人數以及人數占比欄位
college_population_total <- college %>% 
  group_by(term) %>%
  summarise(college_population_total = sum(all, na.rm = T))
college_processed <- college %>%
  left_join(college_population_total, c("term" = "term")) %>%
  mutate(college_population_rate = all/college_population_total) %>%
  select(-vote_rate) %>%
  mutate(vote_rate = vote/all)

### 增加候選人在學院中得票占比欄位
candidate_processed <- candidate %>%
  group_by(term, college) %>%
  mutate(college_support_vote = sum(support), college_support_rate = support/sum(support)) %>%
  ungroup()

### 有學院名稱需要修改
candidate_processed$college[str_detect(candidate$college, "生物.*") ] <- "生物資源暨農學院"

### 合併以院為單位的投票資料與以候選人為單位的投票資料
# 若是同額或不足額競選則分為贊成票與反對票，差額競選則只有得票數
# 在此將贊成票與得票數都以vote_support稱之，另外同額或不足額競選的廢票/無效票是以每個候選人計算
# 差額競選則以院計算，在此分別以vote_invalid與college_invalid稱之，要如何處理個人與院的無效票是一個課題
vote_college <- candidate_processed %>%
  mutate(elected = if_else(elected == "V", 1, 0)) %>%
  left_join(college_processed, by = c("term" = "term", "college" = "college")) %>%
  mutate(competitive = if_else(competitive == "yes", 1, 0)) %>%
  rename(start = term) %>%
  mutate(end = if_else(start == "103-2", "104-1", if_else(start == "104-1", "104-2", if_else(start == "104-2", "105-1", if_else(start == "105-1", "105-2", if_else(start == "105-2", "106-1", "106-2")))))) %>%
  rename(vote_support = support, vote_object = object, vote_invalid = invalid, college_vote_rate = vote_rate,
         college_population = all, college_vote_population = vote, college_vote_invalid = void) %>%
  mutate(elected = as.character(elected)) %>%
  select(-number) %>%
  select(start, end, college, name, elected, vote_support, vote_object, vote_invalid,
         college_population, college_population_total, college_population_rate,
         college_vote_population, college_vote_rate, college_support_vote, college_support_rate, competitive, college_vote_invalid) %>%
  mutate(college = case_when(college == '公共衛生學院' ~ "公衛學院",
    college == '法律學院' ~ "法學院",
    college == '生命科學院' ~ "生科學院",
    college == '生物資源暨農學院' ~ "生農學院",
    college == '社會科學院' ~ "社科院",
    college == '電機資訊學院' ~ "電資學院",
    TRUE ~ college)) %>%
  left_join(distinct(select(df_college_dept_eng, dept_college, college_english, college_english_ab)), by = c("college" = "dept_college"))

### 輸出投票與院系資料檔案: df_vote_college.csv
vote_college %>% write_csv("df_vote_college.csv")
df_vote_college <- read_csv("df_vote_college.csv")

### 輸出投票以及院系各自的檔案: df_college_final.csv/df_candidate_final.csv

college_processed %>% write_csv("df_college_final.csv")
candidate_processed %>% write_csv("df_candidate_final.csv")