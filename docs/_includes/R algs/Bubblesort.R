
bubsort = function(znach) {
  
N = length(znach)
m = 0 #prohodi po ciklu
s = 0 #perestanovki

  for (i in 1:(N-1)) {
    m=m+1 #schetchik prohodov po ciklu
    
    for (j in 1:(N-i)){
      
      if (znach[j+1] < znach[j]){
        
        s = s+1 #schetchik perestanovok
        
        stakan = znach[j+1]
        znach[j+1] = znach[j]
        znach[j] = stakan
    }
  }
  }
cat("eto novaya posledovatelnost", znach,"chislo prohodov", m, "chislo perestanovok", s)
vektor_chisel = c(znach, m, s)
return (vektor_chisel)
}

bubsort(c(1,2,1,8,7,9))
