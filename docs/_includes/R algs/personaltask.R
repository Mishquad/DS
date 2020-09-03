#Gipoteza : summa proizvedeniy maksimalna, esli maksimalno znachenie proizvedeniya

#sortiruem vektori
vectors<- function(a,b) {
  N <- length(a)
  N1 <- length(b)
  
   if (N != N1) {
      print("Nelzya umnozhat' vektora raznoi dlini,sorry")
        otvet = 0
          return(otvet)
   }
  #sortiruem vektor A
  
  for (i in 1:(N-1)) {
    minindex <- i
    for (j in (i + 1):N) {
      if (a[j] < a[minindex]) {
        minindex <- j 
      }
    }
    temp  <- a[i]
    a[i]   <- a[minindex]
    a[minindex] <- temp
    
    
    #sortiruem vektor B
    
    
    for (k in 1:(N1-1)) {
      minindex <- k
      for (v in (k + 1):N1) {
        if (b[v] < b[minindex]) {
          minindex <- v 
        }
      }
      temp  <- b[k]
      b[k]   <- b[minindex]
      b[minindex] <- temp
    
  }
  }
  otvet = sum(a*b)
  cat("Vektori odnoi dlini, summa ih proizvedeniya:", otvet)
  return(otvet)
   }

vectors(c(1,7,9,3,4), c(2,3,8,1,10))