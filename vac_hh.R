# Библиотеки --------------------------------------------------------------
library(httr)
library(dplyr)
library(ggplot2)
library(GGally)
library(stopwords)

#вводим ключевое слово, по которому будем проводить анализ --------------
#name <- readline(prompt = "Enter key word: ")
#name <- "python"
name <- "data science || python"
# создаем пустой датафрейм с необходимыми столбцами -----------------------
df <- data.frame(matrix(ncol=10, nrow=0))

colnames(df) <- c("name", "requriment", "from", "to", "currency", "area", "lat",
                  "lng", "expirience", "created_at")


# парсим данные -----------------------------------------------------------
for (x in 0:10){
  response <- httr::GET(url = "https://api.hh.ru/vacancies/", 
                        query = list(text = name,
                                     page = x,
                                     per_page = 100), )
  
  content_list <- content(response)
  
  nrows <- length(content_list$items)
  salary_from <- list()
  salary_to <- list()
  names <- list()
  requirements <- list()
  currency <- list()
  created_at <- list()
  area <- list()
  address_lat <- list()
  address_lng <- list()
  expirience <- list()
  for (i in 1:nrows) {
    names[[i]] <- content_list$items[[i]]$name[1]
    requirements[[i]] <- content_list$items[[i]]$snippet$requirement[1]
    salary_from[[i]] <- content_list$items[[i]]$salary$from[1]  
    salary_to[[i]] <- content_list$items[[i]]$salary$to[1]
    currency[[i]] <- content_list$items[[i]]$salary$currency[1]
    area[[i]] <- content_list$items[[i]]$area$name[1]
    address_lat[[i]] <- content_list$items[[i]]$address$lat[1]
    address_lng[[i]] <- content_list$items[[i]]$address$lng[1]
    created_at[[i]] <- content_list$items[[i]]$created_at[1]
    expirience[[i]] <- content_list$items[[i]]$experience$name[1]
  }
  
  salary <- list(name=as.character(names),requirement=as.character(requirements),
                 from=as.character(salary_from), to=as.character(salary_to),
                 currency=as.character(currency), area=as.character(area), 
                 lat=as.character(address_lat), lng=as.character(address_lng),
                 expirience=as.character(expirience), created_at=as.character(created_at))
  
  df_ <- data.frame(do.call(cbind, salary))
  df <- rbind(df, df_)
}

# смотрим, что получили ---------------------------------------------------
View(df)
str(df)
#summary(df)

# обработка валюты --------------------------------------------------------
cur_kind <- table(df$currency)
cur_kind #табличка с количеством валют, заметили, что больше всего нулов и рублей

#проверка, что валюта не указана там, где не указана ни мин, ни макс зп
dim(df[df$from == "NULL" & df$to == "NULL", ])
unique(df[df$from == "NULL" & df$to == "NULL", ]$currency)
dim(df[(df$from == "NULL") & (df$to == "NULL") & (df$currency == "RUR"), ])

df[df$currency == "NULL", ]$to
df[df$currency == "NULL" & df$to != 'NULL', ]$to <- "NULL"

df[df$currency == "NULL", ]$from
df[df$currency == "NULL" & df$from != 'NULL', ]$from <- "NULL" 

# 1 BYR = 31 RUR
# 1 EUR = 87 RUR
# 1 KZT = 0,18 RUR
# 1 USD = 78 RUR
# 1 UZS = 0,0068 RUR

cur_trans <- c(
  "BYR" = 31, 
  "EUR" = 87, 
  "KZT" = 0.18,
  "USD" = 78, 
  "UZS" = 0.0068
)

df[df$from == "NULL", ]$from <- df[df$from == "NULL", ]$to
df[df$to == "NULL", ]$to <- df[df$to == "NULL", ]$from

df$from <- as.numeric(df$from)
df$to <- as.numeric(df$to)
View(df)

df[(df$currency == "BYR"), ]$to <- df[(df$currency == "BYR"), ]$to * 31
df[(df$currency == "BYR"), ]$from <- df[(df$currency == "BYR"), ]$from * 31
df[(df$currency == "BYR"), ]$currency <- "RUR"

df[(df$currency == "EUR"), ]$to <- df[(df$currency == "EUR"), ]$to * 87
df[(df$currency == "EUR"), ]$from <- df[(df$currency == "EUR"), ]$from * 87
df[(df$currency == "EUR"), ]$currency <- "RUR"

df[(df$currency == "KZT"), ]$to <- df[(df$currency == "KZT"), ]$to * 0.18
df[(df$currency == "KZT"), ]$from <- df[(df$currency == "KZT"), ]$from * 0.18
df[(df$currency == "KZT"), ]$currency <- "RUR"

df[(df$currency == "USD"), ]$to <- df[(df$currency == "USD"), ]$to * 78
df[(df$currency == "USD"), ]$from <- df[(df$currency == "USD"), ]$from * 78
df[(df$currency == "USD"), ]$currency <- "RUR"

df[(df$currency == "UZS"), ]$to <- df[(df$currency == "UZS"), ]$to * 0.0068
df[(df$currency == "UZS"), ]$from <- df[(df$currency == "UZS"), ]$from * 0.0068
df[(df$currency == "UZS"), ]$currency <- "RUR"

View(df)
boxplot(df$from, df$to, names=c("от", "до"), xlab='зарплата', ylab='сумма', 
        col = 'lightblue',  main = "Зарплаты")

summary(df$from)
summary(df$to)
df$avr_salary <- (df$from + df$to) / 2

#для анализа зп
df_income <- na.omit(df)
df_income <- df_income[df_income$from >= 10000, ]
View(df_income)

summary(df_income$from)
summary(df_income$to)

df_income$avr_salary <- (df_income$from + df_income$to) / 2
summary(df_income$avr_salary)

ggplot(df_income) + #как числа в читабельный вид привести((
  geom_density(aes(avr_salary), fill="lightblue", alpha=0.5) +
  labs(title='Распределение средней зарплаты', x = "сумма") +
  theme_bw()

# опыт --------------------------------------------------------------------
ggplot(df, aes(expirience)) +
  geom_bar(fill = 'lightblue') +
  labs(title="Количество вакансий в зависимости от опыта") + 
  theme_bw()

table(df$expirience)
#посчитаем в процентах какую часть каждый опыт составляет 
32 * 100 / 1100 
154 * 100 / 1100
611 * 100 / 1100
303 * 100 / 1100

# корреляция  -------------------------------------------------------------
#сделаем без опыта = 0
# 1-3 года = 1
# 3-6 лет = 2
# более 6 лет = 3
df1 <- df
df1$num_expir <- df1$expirience
df1[df1$num_expir == "Нет опыта", ]$num_expir <- 0
df1[df1$num_expir == "От 1 года до 3 лет", ]$num_expir <- 1
df1[df1$num_expir == "От 3 до 6 лет", ]$num_expir <- 2
df1[df1$num_expir == "Более 6 лет", ]$num_expir <- 3
df1$num_expir <- as.numeric(df1$num_expir) 

df1$lat <- as.numeric(df1$lat)
df1$lng <- as.numeric(df1$lng)

View(df1)
ggcorr(df1, method = c("pairwise", "pearson"),
       nbreaks = NULL, digits = 2, low = "#F21A00",
       mid = "#EEEEEE", high = "#3B9AB2",
       geom = "tile", label = T,
       label_alpha = FALSE)

area <- as.data.frame(table(df$area))
colnames(area) <- c('Город', 'Кол-во вакансий')
View(area) #добавить потом в отчет топ n

#построим график распределения кол-ва вакансий по городам
ggplot(df, aes(area, fill=area))+
  geom_bar() +
  labs(title="Количество вакансий в разных городах",
       subtitle = paste("Вакансии отобраны по ключевому слову:", name),
       x = "Города", #надо поправить подписи городов, очень плохо выглядит
       y = "Количество вакансий") +
  theme_bw()


avr_salary_area <- as.data.frame(aggregate(df$avr_salary, list(df$area), 
                                           FUN=mean, na.rm = T))

a <- as.data.frame(aggregate(df$avr_salary, list(df$area, df$expirience), FUN=mean, na.rm = T))

colnames(avr_salary_area) <-  c('Город', 'Средняя зарплата')

area <- merge(area, avr_salary_area)
area <- area[order(area$`Кол-во вакансий`, decreasing = T), ]
View(area)


# частотный анализ --------------------------------------------------------
df$requirement <- stringr::str_extract_all(df$requirement, "[a-zA-Z]+")
df$requirement <- gsub("highlighttext", "", df$requirement)
View(df)

plain_text <- df$requirement %>% 
  stringr::str_extract_all(pattern = "(([A-Za-z]){1,})") %>% 
  unlist() %>%
  tolower()

#bigrams <- paste(plain_text[-length(plain_text)], plain_text[-1])

hist_words <- plain_text %>%  
  table() %>%
  sort(decreasing = TRUE)
View(hist_words)

#hist_bigrams <- bigrams %>% 
#table() %>%
#  sort(decreasing = TRUE)
#View(hist_bigrams)

my_stopwords <- c('experience', 'power', 'google', 'etc', 'knowledge', 'server',
                  'office', 'degree', 'cd', 'yandex', 'strong', 'skill', 'hard',
                  'soft', 'confident', 'skills', 'character', 'web', 'science',
                  'computer')

freq_border <- 8
del <- c()
result <- hist_words[-which(names(hist_words) %in% stopwords::stopwords())]
for (i in (1:dim(hist_words))){
  if (nchar(names(hist_words)[i]) < 2 & names(hist_words)[i] != "r"){
    del <- c(del, names(hist_words)[i])
  }
}
del
result <- result[-which(names(result) %in% del)]
result <- result[-which(names(result) %in% my_stopwords)]
result <- result[result > freq_border]
View(result)

wordcloud::wordcloud(words = names(result), freq = result)

result <- result[-which(names(result) == "python")]
wordcloud::wordcloud(words = names(result), freq = result)

#result_bigrams <- hist_bigrams[hist_bigrams > freq_border]
#wordcloud::wordcloud(words = names(result_bigrams), freq = result_bigrams)
#result_bigrams




