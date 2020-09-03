selsort <- function(chisla) {
  
N <- length(chisla)
s = 0 
  
  for (i in 1:(N-1)) {
    minindex <- i #predpolagaem chto vibran nomer min elementa
    for (j in (i + 1):N) {
      if (chisla[j] < chisla[minindex]) {
        minindex <- j 
        s = s+1
      }
    }
  temp  <- chisla[i]
  chisla[i]   <- chisla[minindex]
  chisla[minindex] <- temp
  
    
  }
cat("novii vektor", chisla, "chislo perestanovok:", s)
  otvet = c(chisla,s)
  
}
selsort(c(8,1,22,4,0,22,10))