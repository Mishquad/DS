 fmaxmin = function(znach) {
   
  N = length(znach)
  maxznach = znach[1]
  minznach = znach[1]
  maxindex = 1
  minindex = 1
  
  
    for (i in 1:N) {
      if (maxznach < znach[i]) {
        maxznach = znach[i]
        maxindex = i
      }
    
      if (minznach > znach[i]) {
        minznach = znach[i]
        minindex = i
        
      }
    }
  cat("maximalnoe znachenie massiva:", maxznach, "ego indeks:", maxindex, "minimalnoe znachenie massiva:", minznach, "ego indeks:", minindex)
  otvet = c(maxznach, maxindex, minznach, minindex)
  return(otvet)
 }
 
 fmaxmin(c(23,7,9,6,-2,40,1))