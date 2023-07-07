#Практическое задание 3 (Практическая работа 4 в файле)

library(xlsx)
df <- read.csv2("https://raw.githubusercontent.com/qwerty29544/RpracticeBook/master/2Data/01FlatTables/GAZ.csv", sep = '\t')
colnames(df) <- c('date', 'press', 'temp', 'value', 'condense', 'water', 'id', 
                  'cluster', 'group')
#1
df$date <- as.Date(as.character(df$date), format = "%d/%m/%Y")

#2
df <- na.omit(df)

#3
df$temp_k <- df[,3] + 273
df <- df[,-3]
View(df)

#4
df$id <- as.factor(df$id)
df$cluster <- as.factor(df$cluster)
df$group <- as.factor(df$group)
View(df)

df <- subset(df, (value > 0 & condense > 0 & water > 0)) #чтобы в 5 пункте не было inf
View(df)

#5
df$gasCon <- df$value / df$condense
df$gasWat <- df$value / df$water
df$WatCon <- df$water / df$condense
View(df) 

#6
df_2018 <- subset(df, substring(date, 1,4) == '2018')
View(df_2018)

#7
df_2018_111 <- subset(df_2018, id == '111')
View(df_2018_111)

#8
print(subset(df, water <= 2)$id)  

#9
id_not <- unique(subset(df, value + water + condense < 1000)$id) #id, где опускалась ниже 1000
print(unique(subset(df, id %in% id_not == F)$id)) #id, где не опускалась ниже 1000
#не все удовлетворяют

#10 в 10-12 я не поняла, как именно их вывести, только таблички получаются(
df_2018$summa <- df_2018$value + df_2018$condense + df_2018$water
library(dplyr)
df_2018 %>%
  select(group, summa) %>%
  group_by(group) %>%
  summarise(res = sum(summa)) #Хаузак самый результативный

#11
df_2018 %>%
  select(cluster, water) %>%
  group_by(cluster) %>%
  summarise(res_w = sum(water)) #КП 1

#12 откуда нефть?) я взяла отношение газа к воде
df %>%
  select(cluster, gasWat) %>%
  group_by(cluster) %>%
  summarise(avr_gw = mean(gasWat)) #КП 242
