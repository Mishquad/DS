euclid = function(a,b) {
  
  while (a%%b != 0) {
    
    div = a %% b
    
    #zamena dlya prodolzheniya deleniya
    a = b
    b = div
    
  }
  
  
  return(b)
  

}
euclid(235,55)

