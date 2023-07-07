#Практическое задание 5 (Практическая работа №6.pdf)

#часть 1
#1
type_counter <- function(df){
  count_numeric <- 0
  count_character <- 0
  count_factor <- 0
  for (i in c(1:length(df))){ 
    if (is.numeric(df[, i])){
      count_numeric <- count_numeric + 1
    }
    else if (is.character(df[, i])){
      count_character <- count_character + 1
    }
    else if (is.factor(df[, i])) {
      count_factor <- count_factor + 1
    }
  }
  return (c('numeric'=count_numeric, 'character'=count_character, 'factor'=count_factor))
}

id <- 1:3
country <- c("Flatland", "Wonderland", "Sphereland")
craziness <- c(20, 15, 18)
region_type <- c("A", "B", "A")
author <- c("Abbot", "Carroll", "Burger")
size <- c(10, 100, 30)
#m <- cbind(id, country, craziness, region_type, author, size) 
#df <- as.data.frame(m) #так все данные character
#summary(df) 
df_ <- data.frame(id, country, craziness, region_type, author, size)
summary(df_)
type_counter(df_)

#2
new_numeric <- function(df){
  index_del <- c()
  for (i in c(1:length(df))){
    if (is.numeric(df[, i]) == F){
      index_del <- c(index_del, i)
    }
  }
  df_new <- df[ , -index_del]
  return (df_new)
}
new_numeric(df_)

#4
vec_median <- function(vec){
  if (is.numeric(vec)){
    return (median(vec))
  }
  else{
    print("Vector is not numeric, cannot compute the median")
  }
}
vec_median(c(10, 11, 12, 1372, 129))
vec_median(c("A", "B", "C"))

#часть 2

if ("quantmod" %in% rownames(installed.packages()) == FALSE) {
  install.packages("quantmod") }
library(quantmod)
if ("stringr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("stringr") }
#install.packages("stringr")
library(stringr)
# Мы хотим загрузить акции с данными наименованиями в yahoo
downloadable_stocks <- c("ATVI", "^IXIC")
# Функция получения фреймов с данными
quantmod::getSymbols(Symbols = downloadable_stocks,
                     src = "yahoo",
                     from = as.Date.character("1900-01-01"))
# Функция get() позволяет получить содержимое объекта по его названию-строке
# Мы можем и не знать названия акций в скрипте, но всё равно работать с ними
# при пользовательском вводе названий
df <- data.frame(get(downloadable_stocks[1]))
# Применяем регулярное выражение для поиска и удаления ненужных символов
downloadable_stocks <- stringr::str_remove(downloadable_stocks,
                                           "[:punct:\\^]")
# Удалим полученные объекты
rm(list = downloadable_stocks)
View(df)

#2 Реализовать функцию исключения тренда

out_of_trend <- function(x, dt, method='Arifm'){
  x <- x + min(x) + 1
  if (length(x) > 3 & dt <= (ceiling(length(x)/2)-1) 
       & is.numeric(x) & is.numeric(dt) == F){
    print("Error: incorrect data")
    return (-1)
  }
  if (method %in% c('Arifm', 'Geom', 'Garm') == F){
    print("Error: incorrect data")
    return (-1)
  }
  y <- vector()
  if (method == 'Arifm'){
    for (t in c((1 + dt):(length(x) - dt))){
      y <- append(y, log((x[t-dt] + x[t+dt])/(2*x[t])))
    }
    return (y)
  }
  else if (method == 'Geom'){
    for (t in c((1 + dt):(length(x) - dt))){
      y <- append(y, log((x[t-dt] * x[t+dt])/(x[t]^2)))
    }
    return (y)
  }
  else if (method == 'Garm'){
    for (t in c((1 + dt):(length(x) - dt))){
      y <- append(y, log((2 * x[t-dt] * x[t+dt])/(x[t] * (x[t-dt] + x[t+dt]))))
    }
    return (y)
  }
}
#3
t = seq(0, 10, 0.1)
x = 2 * t + 3 + sin(2*t)
xn <- out_of_trend(x, 2)
mean(xn)
#4 
AlterJohns <- function(y){
  a_t <- numeric(length(y))
  a_t[1] <- 0
  for (i in (1:(length(y)-1))){
    a_t[i + 1] <- sum((1/(length(y)-i)) * abs(y[1 : (length(y) - i)] - y[(1 + i):(length(y))]))
  }
  return (a_t)
}
#5
aj <- AlterJohns(xn)
aj
which(aj == min(aj)) #1 отвечает локальному минимуму
#6
vec <- AlterJohns(out_of_trend(df$ATVI.Open, 2))
vec
plot(vec) #первый локальный мин в точке 0, но как будто я что-то не так делаю

#часть 3
SIM <- function(A, u0, f, n_iter = 10e5, eps = 10e-7){
  alpha <- (-1) * A / diag(A)
  diag(alpha) <- 0
  beta <- f / diag(A)
  u <- u0
  for (i in c(1:n_iter)){
    if (is.na(max(abs(beta + alpha %*% u - u)) < eps)){
      print("Решения нет")
      return(u)
    } else if (max(abs(beta + alpha %*% u - u)) < eps){
      return(u)
    } else {
      u <- beta + alpha %*% u
    }
  }
  return(u)
}
