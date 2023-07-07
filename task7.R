#Практическая работа 7 (Практика №10)

if ("ggplot2" %in% rownames(installed.packages()) == F){
  install.packages("ggplot2")
}
library(ggplot2)
df <- faithful
df
ggplot(data = df, mapping = aes(eruptions, waiting)) + 
  geom_point(color = kmeans(x = df, centers = 2)$cluster) +
  geom_density2d() +
  theme_bw() +
  labs(title="Время ожидания между извержениями и продолжительность извержения",
       subtitle = "Старый Верный гейзер, штат Вайоминг, США",
       x = "Продолжительность извержения, минуты",
       y = "Ожидание до следующего извержения, минуты") +
  scale_x_continuous(breaks = seq(min(df$eruptions), max(df$eruptions), 0.25)) +
  scale_y_continuous(breaks = seq(min(df$waiting), max(df$waiting), 5))


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

#открытие пнг устройства
png(filename = 'timeseries.png', width = 800, height = 800, units = 'px')
ggplot(data = df, mapping = aes(x = 1:nrow(df), y = df[[4]])) +
  geom_line(lwd = I(0.5), lty = 1) +
  geom_point(cex = I(0.5)) +
  coord_cartesian(xlim = c(1, nrow(df)), ylim = range(df[[4]]))+
  labs(title = "График цен акций компании Activision-Blizzard",
       subtitle = paste("Данные от", min(rownames(df))),
       x = "Торговые дни от начала торгов, отсчитанные с 1",
       y = "Цены акций в долл.США") +
  scale_x_continuous(breaks = seq(1, nrow(df), 365)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_bw()
dev.off() #сохранить в папке


png(filename = "histogram.png", width = 600, height = 600, units = "px")
ggplot(data = df, mapping = aes(df[[4]])) +
  geom_histogram(bins = 40, color = "black", fill = "grey") +
  geom_freqpoly(bins = 40, color = "red4", lwd = I(1.1)) +
  labs(title = "Гистограмма распределения данных статистического разреза",
       subtitle = "Временной ряд цен акций Activision Blizzard",
       x = "Данные цен акций в долл.США",
       y = "Количество измерений в промежутке") +
  scale_x_continuous(n.breaks = 15) +
  scale_y_continuous(n.breaks = 15) +
  theme_bw()
dev.off() 

png(filename = "density_histogram.png", width = 600, height = 600, units = "px")
ggplot(data = df, mapping = aes(df[[4]])) +
  geom_histogram(aes(y=..density..),
                 binwidth = density(df[[4]])$bw,
                 color = "black",
                 fill = "grey") +
  geom_density(fill = "red", alpha = 0.2) +
  labs(title = "Гистограмма и ядровая функция распределения данных",
       subtitle = "Временной ряд цен акций Activision Blizzard",
       x = "Данные цен акций в долл.США",
       y = "Количество измерений в промежутке") +
  scale_x_continuous(n.breaks = 15) +
  scale_y_continuous(n.breaks = 15) +
  theme_bw()
dev.off() 

#самостоятельная работа №10
#часть 1
#1
demograthy <- read.csv("https://raw.githubusercontent.com/allatambov/R-programming-3/master/seminars/sem8-09-02/demography.csv")
View(demograthy)
#2 
demograthy$young_share <- (demograthy$young_total / demograthy$popul_total * 100) 
demograthy$trud_share <- (demograthy$wa_total / demograthy$popul_total * 100) 
demograthy$old_share <- (demograthy$ret_total / demograthy$popul_total * 100) 
View(demograthy)
#3
ggplot(data = demograthy, mapping = aes(trud_share)) +
  geom_histogram(bins = 15, color = "black", fill = "lightblue") +
  geom_rug() +
  geom_vline(xintercept = median(demograthy$trud_share),
             color = "darkblue",
             lty = 2) +
  labs(title = "Гистограмма распределения доли трудоспособного населения",
       x = "Доля трудоспособных") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_bw()
#4
ggplot(data = demograthy, aes(x = trud_share,
                              group = region,
                              fill = region)) +
  geom_density(alpha = 0.5)

#наверное, имеет, как я поняла, различие violin от boxplot: violin позволяет увидеть различие в структуре данных
ggplot(data = demograthy, aes(x = region, y = trud_share,
                              fill = region)) +
  geom_violin()
#boxplot не нужен был, но решила оставить
ggplot(data = demograthy, aes(x = region, y = trud_share,
                              fill = region)) +
  geom_boxplot()
#5 я не заметила закономерности, что чем больше процент молодого населения, тем меньше процент пожилого
ggplot(data = demograthy) +
  geom_point(aes(x = old_share, y = young_share), color = 'blue', 
             size = 5, shape = 1) +
  labs(title = "Диаграмма рассеяния",
       subtitle = "для доли людей моложе и старше трудоспособного возраста",
       x = "старше",
       y = "моложе") +
  scale_x_continuous(n.breaks = 15) +
  scale_y_continuous(n.breaks = 15) +
  theme_bw()
#6
demograthy$male_share <- ((demograthy$wa_male + demograthy$ret_male +
                          demograthy$young_male) / demograthy$popul_total * 100)
demograthy$male <- ifelse(demograthy$male_share >= 50, 1, 0) 
#только в 1 районе, немного подозрительно, но вроде я все так сделала
View(demograthy)
#7
demograthy$male <- as.factor(demograthy$male)
ggplot(data = demograthy, aes(x = young_share, y = old_share, size = male_share, 
                              color = male)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("pink", "lightblue"))
  theme_bw()

ggplot(data = demograthy) +
  geom_bar(mapping = aes(region, color = region, fill = region)) +
  theme_bw()

#часть 2
#задание 1
View(mtcars)
ggplot(data = mtcars, aes(x = hp, y = wt)) +
  geom_point(aes(size = cyl, color = as.factor(am))) +
  labs(title = "Характеристики автомобилей", 
       x = "Число лошадиных сил", 
       y = "Вес", 
       color = "Коробка передач", 
       size = "Число цилиндров") +
  scale_color_manual(values = c("green", "red"),                    
                     labels = c("Автомат", "Механика"))

#задание 2
ggplot(data = mtcars, aes(x = hp)) + 
  geom_histogram(fill = "brown", 
                 color = "black", 
                 bins = 6) +  
  labs(title = "Gross horsepower", 
       x = "Horsepower", 
       y = "count") + 
  theme_bw() + 
  facet_grid(~am, 
             labeller = labeller(am = c("0" = "Automatic", 
                                        "1" = "Mechanic")))
#задание 3
View(sleep)
ggplot(sleep, aes(y = extra, group = group, fill = group)) +
  geom_boxplot() +
  labs(title = "Распределение переменной extra", 
       x = ":)", 
       y = "extra") + 
  scale_fill_manual(values = c("pink", "azure")) +
  theme_bw() 

#часть 3
cvd <- read.csv("data_output.csv")
View(cvd)
ggplot(cvd, aes(x = 1:nrow(cvd), y = X_Sweden)) +
  geom_line(lwd = I(0.5), lty = 1) +
  geom_point(cex = I(0.4)) +
  coord_cartesian(xlim = c(1, nrow(cvd)), ylim = range(cvd$X_Sweden)) +
  labs(title = "Данные по заболеваемости COVID в Швеции",
       subtitle = paste("от", min(cvd$X), "по", max(cvd$X)),
       x = "Дата",
       y = "Количество переболевших") +
  scale_x_continuous(n.breaks = 15) +
  scale_y_continuous(n.breaks = 15) +
  theme_bw()

ggplot(cvd, aes(X_Sweden)) +
  geom_histogram(bins = 40, color = "blue", fill = "azure") +
  geom_freqpoly(bins = 40, color = "black", lwd = I(1.1)) +
  labs(title = "Гистограмма распределения данных",
       subtitle = "Временной ряд заболеваемости ковид в Швеции",
       x = "Количество переболевших") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 20) +
  theme_bw()
