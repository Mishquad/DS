#### загрузка обработка ####

# грузанем файлик
df <- read.csv('C:/Users/rybakconst/Documents/finam/YNDX_790101_191027dayok.csv',
                header = TRUE, stringsAsFactors = FALSE)

# переименуем колонки для удобства
colnames(df) <- c('ticker', 'period', 'date', 'time', 'price')

# можно посмотреть как выглядит макушка нашего датафрейма
head(df)
# можно взглянуть глаз на хвостик
tail(df)
# посмотреть на конкретную строку, например, 173
df[173,]
# или конкретный столбик, например, 3
df[,3]
# как вы догадались можно запросить конкретную ячейку
df[10, 3]
# можно вытаскивать несколько строк или столбцов или несколько конкретных ячеек
df[c(1,2,23,17,1000), ]
df[,c(1,3,5)]
df[c(1,2,23,17,1000), c(1,3,5)]

# также к столбцам можно обращаться несколько иначе, что зачастую удобо
df['date']
df[c('ticker', 'date', 'price')]
df[,c('ticker', 'date', 'price')]
# можно еще вот так обратиться к одному столбцу
df$price

# далее неплохо бы посмотреть в каком виде у нас загрузились столбики
# конкретнее - какой тип данных распознал Р:
# тикер - строка - сойдет
class(df$ticker)
# период - строка - нам вообще не нужен
class(df$period)
# дата - целое - хотим в формате даты
class(df$date)
# время - целое - особо тоже не нужно
class(df$time)
# цена - нумерик - сойдет
class(df$price)


# в RStudio в случае датафрейма можно класс переменной посмотреть и не вбивая вышевбитое


# а вдруг вообще нам удобно было бы иметь датафрейм чисто из даты и цены?
df[c('date', 'price')] -> yndx
rm(yndx) # удаляет конкретную переменную

# а может просто конкретный столбец не угодил? 
df$period <- NULL


# теперь займемся датами, они у нас подгрузились в виде числа
class(df$date)

as.Date(df$date) # по простому не вышло, залезем в справку

# видимо даты должны быть в формате строки чтобы применить эту функцию
as.Date(as.character(df$date)) # и снова провал, лезем в справку дальше

# видимо формат наших дат не входит в стандартные но это не беда!
as.Date(as.character(df$date), "%Y%m%d") 
class(as.Date(as.character(df$date), "%Y%m%d")) # победа
# перезапишем столбик в требуемом формате 
df$date <- as.Date(as.character(df$date), "%Y%m%d")


#### картиночка ####
# вот это не всегда срабатывает
plot(df$price) 
# вот это поточнее
plot(x = df$date, y = df$price)
# сделаем линией
plot(x = df$date, y = df$price, type = 'l')
# c цветомс
plot(x = df$date, y = df$price, type = 'l', col = 'blue')

# найдем среднее
findMean <- function(){
  sum <- 0
  for (i in 1:length(df$price)) {
    #print(i)
    sum <- sum + df$price[i]
    #print(sum)
  }
  avg <- sum/length(df$price)
  return(avg)
}
myMean <- findMean()

# нарисуем его на картиночке
# линия по формуле типа y = a + bx
abline(a = myMean, b = 0, col = 'magenta', lty = "dashed")

findDeviation <- function(){
  # ваш кодик
}

findLongestGrowth <- function(){
  # ваш кодик
}

# пометьте вертикальными линиями границы периода самого продолжительного роста =)