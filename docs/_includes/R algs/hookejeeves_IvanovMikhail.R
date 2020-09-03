fxy = function(x,y) {
  znach = 8*(x-1)^2+4*x*y+5*(y-2)^2
  return(znach)
}

accuracy = 0.1

#start poisk + poisk po obaztsu
example_search <- function(x,y,delta,a,b){
  if(fxy(x,y) < fxy(a,b)){
    a <- x
    b <- y
    
    funcXmin(a,b,delta,a,b)
  }else{ 
    if(delta > accuracy){
      delta <- delta/2
      funcXmin(x,y,delta,a,b)
    }else{
      cat("tochka min:",c(a,b),'\n')
      print("znachenie funkcii v nej")
      return(fxy(a,b))
      }
    }
}
#issled poisk po X
funcXmin <- function(x,y,delta,a,b){
  if(fxy(x + delta, y) < fxy(x,y)){
    x <- x + delta
    
  }else if(fxy(x - delta, y) < fxy(x,y)){
    x <- x - delta
    
  }
  
  funcYmin(x,y,delta,a,b)
}
#issled poisk po Y
funcYmin <- function(x,y,delta,a,b){
  if(fxy(x, y + delta) < fxy(x,y)){
    y <- y + delta
  }else if(fxy(x, y - delta) < fxy(x,y)){
    y <- y - delta
  }
  example_search (x,y,delta,a,b)
}


 funcXmin(-4,-4,1,-4,-4)