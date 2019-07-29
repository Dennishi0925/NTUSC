library(tidyverse)
library(knitr)
library(kableExtra)
options(stringsAsFactors = F)
# Sys.setlocale(locale = "UTF-8")
# Sys.setlocale(category = "LC_ALL", locale = "cht")
# rm(list=ls())
NTU_Attnd <- read_csv("all_long_new.csv")
NTU_Vote <- read_csv("vote_all.csv")
View(NTU_Vote)
###### 大架構：NTUSC Attendance Data　& NTUSC Vote Data 台大學代選舉資料

### 資料描述

- NTUSC Attendance Data
1. Data description: NTUSC attendance data of each session
2. Data range: 104-1 ~ 106-1
3. Data size: 3843 observations * 13 variables
4. Data description: 長表格形式，也可以轉換成寬表格

- NTUSC Vote Data
1. Data description: NTUSC vote data of each term
2. Data range: 104-1 ~ 106-1
3. Data size: 186 observations * 11 variables
4. Data description: 長表格形式，無須轉換成寬表格
*補充: 因為學代一學期選一次又任期為一年，故同一學期會有不同學期上任的人

### 內容呈現

- NTUSC Attendance Data 台大學代出席資料
1. 開放：原始資料下載
2. 理解：資料概覽 & 資料視覺化
3. 查詢：資料查詢

- NTUSC Vote Data 台大學代選舉資料
1. 開放：原始資料下載
2. 理解：資料概覽 & 資料視覺化
3. 查詢：資料查詢

### 版面

1. 學代出席資料分成各學期呈現，最後再加上五學期統整
 1-1. 整學期的出席資料依條件篩選（縱斷面）
 1-2. 每次會期的出席資料（橫斷面）
 1-3. 依會期資料進行的統整（橫斷面）
 1-4. 原始資料觀看

2. 學代出席與投票資料另外弄一個版面？
- NTUSC Attendance Data 台大學代出席資料
1. 開放：原始資料下載
2. 理解：資料概覽 & 資料視覺化
3. 查詢：資料查詢

- NTUSC Vote Data 台大學代選舉資料
1. 開放：原始資料下載
2. 理解：資料概覽 & 資料視覺化
3. 查詢：資料查詢
