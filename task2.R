#Практическая работа 2 
#ср 3 часть 1
#1
info <- list(name = c("Jane", "Michael", "Mary", "George"), 
             age = c(8, 6, 28, 45), gender = c(0, 1, 0, 1))
info
info[[1]][2] #вывод "Michael" 
info$name[2] #вывод "Michael" 
info[[3]] #вывод вектора "gender"
info$name #вывод вектора "name"
#добавила еще один столбец:
info <- append(info, list(drinks = c("juice", "tea", "rum", "coffee")))
info
#добавить данные по еще одному человеку
info$name <- c(info$name, "John")
info$age <- c(info$age, 2)
info$gender <- c(info$gender, 1)
info$drinks <- c(info$drinks, "milk")
info

#2
s <- "a,b,c,d"
let <- strsplit(s, ",")
class(let)
unlist(let)

index <- "0,72;0,38;0,99;0,81;0,15;0,22;0,16;0,4;0,24"
index <- gsub(",", ".", index) #для начала заменим , на .
I <- strsplit(index, ";")
I <- as.numeric(unlist(I)) #сделали числовой вектор I
I

#часть 2
#1
install.packages("randomNames")
library(randomNames)
#2
set.seed(1234) # чтобы у всех получались одинаковые результаты
names <- randomNames(100, which.names = "first", ethnicity = 4)
names
#3
ages <- sample(16:75, 100, replace = TRUE) # replace = TRUE – с повторяющимися значениями
views <- c("right", "left", "moderate", "indifferent")
polit <- sample(views, 100, replace = TRUE) #политические взгляды респондентов

df = data.frame(names, ages, polit)
df
#4
df$ID <- 1:100 
df
#5
n <- nrow(df[df$ages >= 25 & df$ages <= 30, ])
n # кол-во людей в возрасте от 25 до 30 
share <- n / nrow(df) * 100 #доля в процентах
print(paste0("Доля респодентов в возрасте от 25 до 30 лет: ", share, "%"))
#6
df$polit_views <- factor(df$polit, labels = c(1:4)) #4 уровня
df

#Часть 3 (1)
#1
if ("car" %in% installed.packages() == F){
  install.packages("car")
}
library(car)
firms <- Ornstein
View(firms)
#2
str(firms)
#3 
nrow(firms[complete.cases(firms), ]) #сколько полностью заполненных наблюдей
nrow(firms[!complete.cases(firms), ]) #cколько пропусков
#4
a_fil <- firms[firms$assets >= 10000 & firms$assets <= 20000, ]
b_fil <- firms[firms$interlocks <= 30, ]
c_fil <- firms[firms$sector == "TRN" & firms$nation == "CAN", ]
#5
firms$log_assets <- log(firms$assets)
head(firms)
#6 нет пропущенных значений
#7 нет пропущенных значений, но если бы были: 
firms <- na.omit(firms)
#8 
library(foreign)
write.dta(firms, "Firms.dta")

#Часть 3 (2)
install.packages(c("dplyr", "readr", "stringr"))
#1
covid <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
View(covid)
#2
str(covid)
#3
if ("tidyr" %in% installed.packages() == F){
  install.packages("tidyr")
}
library(tidyr)
covid <- unite(covid, Place, c("Province.State", "Country.Region"))
View(covid)

#4-5
if ("matrixStats" %in% installed.packages() == F){
  install.packages("matrixStats")
}
install.packages("matrixStats")
library(matrixStats)
df_covid <- data.frame(region=covid[,1], Lat=covid[,2], Long=covid[,3], 
                       sumx=rowSums(covid[c(4:ncol(covid))]),
                       sdx=rowSds(as.matrix(covid[,c(4:ncol(covid))]), na.rm=TRUE),
                       meanx=rowMeans(covid[c(4:ncol(covid))]))

View(df_covid)

#6
library('dplyr')
df_new <- t(as.matrix(covid[, 4:ncol(covid)]))
colnames(df_new) <- covid$Place
View(df_new)

rownames(df_new) <- gsub('X', '', rownames(df_new))
rownames(df_new) <- gsub('\\.','-', rownames(df_new))
rownames(df_new) <- format(as.Date(rownames(df_new), '%m-%d-%y'), "%Y-%m-%d")
df_new <- as.data.frame(df_new)
View(df_new)

#7
write.csv(df_new,'data_output.csv')
write.table(df_new,'data_output.txt')

install.packages('xlsx')
library('xlsx')
write.xlsx(df_new,'data_output.xlsx', sheetName = 'sheet1')

