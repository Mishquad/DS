df <- read.csv('D:/Coding/blizz.csv',
               header = TRUE, stringsAsFactors = FALSE)
colnames(df) <- c('ticker', 'period', 'date', 'time', 'price')

df$date <- as.Date(as.character(df$date), "%Y%m%d")

#funkciya kotoraya sobiraet vektor vseh tcen bumagi, gde idet rost dohodnosti akcii
profit_func = function() {
  vec_profit = c()
    for (i in 1:(length(df$price)-1)) {
     init_price = df$price[i]
     next_price = df$price[i+1]
     profit = (next_price - init_price) / init_price
    
       vec_profit = c(vec_profit, profit)
    
  }
  return(vec_profit)
}
 proffunc = profit_func()
 
 #risuem graphic
 plot(x = df$date, y = df$price, type = 'l', col = 'blue')
 
 #srednee arifmeticheskoe tseni
 findMean <- function(){
   sum <- 0
   for (i in 1:length(df$price)) {
     sum <- sum + df$price[i]
   }
        avg <- sum/length(df$price)
   return(avg)
 }
 mean_profit <- findMean()
 
 # graphic
 abline(a = mean_profit, b = 0, col = 'red', lty = "dashed")
           
#funkcia dispersii dohodnosti
 findDeviation <- function(){
    quadsum <- 0
   dispersion <- 0
     for (i in 1:length(df$price)){
      quadsum <- quadsum + (df$price[i]) ** 2 
   }
     dispersion <- quadsum / length(df$price) - ( mean_profit** 2)
   return(dispersion)
  }
 print(findDeviation())
 deviation = findDeviation
 
 #Ishem samii prodolzhitelnii rost
 grow = function() {
   schet = 0
   indgrow = 1
   endgrowind = 1
   growthperiod = 0
   
   for (i in 2:length(df$price)) {
     if (df$price[i-1] < df$price[i]) {
       schet = schet + 1
       indgrow = i
     }
     else {
       if (schet > growthperiod) {
         growthperiod = schet
         endofgrowth = indgrow
         startofgrowth = indgrow - growthperiod
       }
       schet = 0
     }
   }
   
   startdate = df$date[startofgrowth]
   enddate = df$date[endofgrowth]
   
   wholegrowth = c(startdate,enddate)
   print("naibolee prodolzhitelnii rost s")
   print(startdate)
   print("po")
   print(enddate)
   return(wholegrowth)
   
 }
 
 
 longestgrowth = grow()
 plot(x = df$date[1:length(df$date)], y = df$price[1:length(df$price)], type = "l")
 abline(a = mean_profit, b = 0, col = 'red', lty = "dashed")
 abline(v = longestgrowth[1], b = 0, col = 'green', lty = "dashed")
 abline(v = longestgrowth[2], b = 0, col = 'green', lty = "dashed")
 
 