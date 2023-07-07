# Практическая работа 1

#ср1 часть 1
#Задание 1
x <- 2
y <- 5
term <- x
x <- y
y <- term
print(paste('x =', x))
sprintf("y = %d", y) #тестила разнные способы вывода

#Задание 2
x <- -3.5
y <- "2,6"
z <- 1.78
h <- TRUE

#определили тип переменных
class(x)
typeof(y)
typeof(z)
typeof(h)

as.numeric(h) #h -> целочисленной

#y -> числовой
y <- sub(',', '.', y)
as.numeric(y)

as.character(x) #x -> текстовой

#Задание 3
dohod <- 1573
dohod <- log(dohod)
dohod

#Задание 4
write(27, "file1(4).txt")
N <- readLines(con = "file1(4).txt")
print(paste("2*N - 1:", 2*as.numeric(N) - 1))

#ср1 часть 2
#Задание 1
g <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
print(g[1]) #первый элемент
print(g[length(g)]) #последний элемент
print(g[3:5]) #элементы с 3 по 5
print(g[g == 2 & is.na(g) == FALSE]) #элементы равны 2
print(g[g > 4 & is.na(g) == FALSE]) #элементы > 4
print(g[g %% 3 == 0 & is.na(g) == FALSE]) #элементы %% 3
print(g[g %% 3 == 0 & g > 4 & is.na(g) == FALSE]) #элементы > 4 и %% 3
print(g[(g < 1 | g > 5) & is.na(g) == FALSE]) #элементы < 1 или > 5
print(which(g == 0)) #индексы элементов = 0
print(which(g >= 2 & g <= 8)) #индексы элементов, которые не меньше 2 и не больше 8 (>= 2 и <= 8)
print(sort(g[-(which(g %% 10 == 2))])) #элементы по возрастанию, без чисел, которые оканчиваются на 2

#Задание 2 заменить последний элемент на NA
vec <- c(1, 2, 3, 4, 5)
replace(vec, length(vec), NA)

#Задание 3 вывести индексы пропущенных значений
vec1 <- c(1, 2, NA, 4, NA, 6, NA, NA, 9)
print(which(is.na(vec1)))

#Задание 4 количество NA
print(length(vec1[is.na(vec1)]))

#Задание 5 вектор со 100 уникальными значениями
vec_id <- round(runif(n = 100, min = 100, max = 999))
vec_id

#Задание 6 
#не очень поняла формулировку, я бы сделала так:
country <- c("France", "Italy", "Spain")
year <- seq(2017, 2021)
country
year

#но если надо прям вектор из таблицы повторить, то:
country <- c(rep("France", 5), rep("Italy", 5), rep("Spain", 5))
country
year <- rep(c(2019, 2020, 2020, 2018, 2017), 5)
year

#Задание 7
income <- c(10000, 32000, 28000, 150000, 65000, 1573)
avrg_income <- sum(income)/length(income)
income_class <- as.integer(income >= avrg_income)
income_class

#задание 8 (27 вариант N = 7, P = 1.88)
x <- sample(1:10, 7)
write(x, file = "coords.txt", sep = "\n")
x_vec <- readLines(con = "coords.txt")
x_vec <- as.integer(x_vec)
res <- sum(abs(x_vec)^1.88)^(1/1.88)
write(res, file = "result.txt")

#Задание 9
#создание файла и считывание данных в задании 8
first_diff <- diff(x_vec)
second_diff <- diff(first_diff)
second_diff[length(first_diff)] <- ""
diff_vec <- data.frame(first_diff, second_diff)
write.table(diff_vec, "diff_vectors.txt", quote = F, sep = "\t", 
            row.names = F, col.names = T)

#ср 2 часть 1
#создание и измнение матрицы
m <- matrix(3, 3, 4)
m
m[1, 3] <- 4
m[2, 1] <- 1
m[3, 2] <- NA
m[3, 4] <- 1
m

#создание матрицы из векторов
#1-2
a <- c(1, 3, 4, 9, NA)
b <- c(5, 6, 7, 0, 2)
c <- c(9, 10, 13, 1, 20)
m_c <- cbind(a, b, c) #векторы являются столбцами матрицы
m_r <- rbind(a, b, c) #векторы являются строками матрицы
rownames(m_c) <- c("r1", "r2", "r3", "r4", "r5")
colnames(m_r) <- c("col1", "col2", "col3", "col4", "col5")
m_c
m_r

#3: проверка может ли матрица состоять из элементов разных типов 
names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)
m1 <- cbind(names, ages, gender)
m1 #получилось, хотя я думала, что ошибку выдаст, 
#R сам преобразовал элементы векторов возраст и пол из чисел в строки

ages_sq = ages^2
m1 <- cbind(m1, ages_sq) #добавила столбец возраст в квадрате
m1

#4
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

#5
s <- "a,b,c,d"
let <- strsplit(s, ",")
class(let)
unlist(let)

index <- "0,72;0,38;0,99;0,81;0,15;0,22;0,16;0,4;0,24"
index <- gsub(",", ".", index) #для начала заменим , на .
I <- strsplit(index, ";")
I <- as.numeric(unlist(I)) #сделали числовой вектор I
I

#ср 2 часть 2
#1
A <- diag(c(4, 9), 2, 2)
rownames(A) <- c("eq1", "eq2")
colnames(A) <- c("x1", "x2")
A
#2
eigen(A)$values #вывод собственных значений матрицы А
#3
I <- diag(1, 2, 2)
B <- I - A
B
#4
f <- c(4, 2)
u <- c(0.2, -0.3) 
#5
u_result <- solve(A, f)
u_result
#6
#хотела по умному через цикл.. по умному не получается
u2 <- B %*% u + f
u3 <- B %*% u2 + f
u4 <- B %*% u3 + f
u5 <- B %*% u4 + f
u6 <- B %*% u5 + f
u7 <- B %*% u6 + f
u8 <- B %*% u7 + f
#7
diff_u <- u_result - u8
diff_u
#8
A1 <- A / max(A)
f1 <- f / max(A)
#9
eigen(A1)$values #вывод собственных значений матрицы А1
B1 = I - A1
u_res <- solve(A1, f1)
nu2 <- B1 %*% u + f1
nu3 <- B1 %*% nu2 + f1
nu4 <- B1 %*% nu3 + f1
nu5 <- B1 %*% nu4 + f1
nu6 <- B1 %*% nu5 + f1
nu7 <- B1 %*% nu6 + f1
nu8 <- B1 %*% nu7 + f1
diff_nu <- nu8 - u_res
#10
diff_u
diff_nu

#ср 2 часть 3
step <- 1 # Шаг сетки
dekart_begin <- -5 # Начало сетки
dekart_end <- 5 # Конец сетки
# Задание сеточной поверхности
x <- seq(from = dekart_begin, to = dekart_end, by = step)
y <- x
# Задание двумерной функции на координатной сетке
surface_matrix <- outer(X = x,
                        Y = y,
                        FUN = function(x,y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix) <- list(x, y)
surface_matrix
#Задание 1
write(paste0("numeric of matrix elements: ", length(surface_matrix), '\n',
            "number of rows: ", nrow(surface_matrix), '\n',
            "number of cols: ", ncol(surface_matrix), '\n',
            "sum of main diag elements: ", sum(diag(surface_matrix)), '\n',
            "sum of middle row elements: ", 
            sum(surface_matrix[round(nrow(surface_matrix)/2), ]), '\n',
            "sum of middle column elements: ", 
            sum(surface_matrix[ , round(nrow(surface_matrix)/2)])),
      file = "summary.txt")
write(paste("row sums:", 1:nrow(surface_matrix), ": ", rowSums(surface_matrix)),
      file = "summary.txt", append = T)
write(paste("col sums:", 1:ncol(surface_matrix), ": ", colSums(surface_matrix)),  
      file = "summary.txt", append = T)

#Задание 2
step2 <- as.numeric(readline(prompt = "Enter step: "))
dekart_begin2 <- as.numeric(readline(prompt = "Enter begin: "))
dekart_end2 <- as.numeric(readline(prompt = "Enter end: "))
# Задание сеточной поверхности
x <- seq(from = dekart_begin2, to = dekart_end2, by = step2)
y <- x
# Задание двумерной функции на координатной сетке
surface_matrix2 <- outer(X = x,
                        Y = y,
                        FUN = function(x,y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix2) <- list(x, y)
surface_matrix2

write(paste0("numeric of matrix elements: ", length(surface_matrix2), '\n',
             "number of rows: ", nrow(surface_matrix2), '\n',
             "number of cols: ", ncol(surface_matrix2), '\n',
             "sum of main diag elements: ", sum(diag(surface_matrix2)), '\n'),
      file = "summary2.txt")
write(paste("row sums:", 1:nrow(surface_matrix2), ": ", rowSums(surface_matrix2)),
      file = "summary2.txt", append = T)
write(paste("col sums:", 1:ncol(surface_matrix2), ": ", colSums(surface_matrix2)),  
      file = "summary2.txt", append = T)

#Задание 3
write(paste0(1, '\n', -5, '\n', 5, '\n', 2, '\n', -4, '\n', 4), file = "inputs.txt")
value <- as.numeric(readLines('inputs.txt'))
stepx <- value[1]
x_begin <- value[2]
x_end <- value[3]
stepy <- value[4]
y_begin <- value[5]
y_end <- value[6]
# Задание сеточной поверхности
x <- seq(from = x_begin, to = x_end, by = stepx)
y <- seq(from = y_begin, to = y_end, by = stepy)
# Задание двумерной функции на координатной сетке
surface_matrix3 <- outer(X = x,
                         Y = y,
                         FUN = function(x,y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix3) <- list(x, y)
surface_matrix3

write(paste0("numeric of matrix elements: ", length(surface_matrix3), '\n',
             "number of rows: ", nrow(surface_matrix3), '\n',
             "number of cols: ", ncol(surface_matrix3), '\n',
             "sum of main diag elements: ", sum(diag(surface_matrix3)), '\n'),
      file = "summary3.txt")
write(paste("row sums:", 1:nrow(surface_matrix3), ": ", rowSums(surface_matrix3)),
      file = "summary3.txt", append = T)
write(paste("col sums:", 1:ncol(surface_matrix3), ": ", colSums(surface_matrix3)),  
      file = "summary3.txt", append = T)

#ср 2 часть 4
cars_matrix <- as.matrix(cars)
cars_matrix
#1
cars_speed <- cbind(rep(1, 50), cars_matrix[, 1])
cars_speed
#2
cars_dist <- cars_matrix[, 2]
#3
alpha <- solve(t(cars_speed) %*% cars_speed) %*% t(cars_speed) %*% cars_dist
alpha
#4
alpha <- alpha[, 1] #получилась матрица, делаем вектор
typeof(alpha)
#5
alpha_c <- alpha[1]
alpha_x <- alpha[2]
print(paste("alpha_c =", alpha_c))
print(paste("alpha_x =", alpha_x))
#6
cars_speed_lm <- cars_matrix[, 1]
cars_speed_lm
#7
cars_dist_lm <- alpha_x + cars_speed_lm * alpha_x
cars_dist_lm
#8
dist_residuals <- cars_dist_lm - cars_dist
dist_residuals
#9
ds_mean <- mean(dist_residuals) #среднее
ds_sd <- sd(dist_residuals) #стандартное отклонение
#sum(dist_residuals - mean(dist_residuals))/length(dist_residuals)
#я не поняла, что именно просят среднее значение или среднее отклонения 
#(но это я даже не знаю, что такое, гугл тоже:)
#10
cars_dist_lm
#11
ds_mean
ds_sd

