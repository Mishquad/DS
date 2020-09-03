    #dopustim chto nasha funkcia <=> y = x^2+6x-3
    f = function(x){
      znach = x^2+6*x-3
      return(znach)
    }
    
    #nakhodim min extr
    golden = function(a,b) {
      risunokX <- seq(a, b, 0.01)
      risunokY <- f(risunokX)
      plot(risunokX,risunokY,type = "l")
  # pogreshnost' i startovii otrezok
    accuracy = 0.001 
    
    x1_vec = c()
    x2_vec = c()
    y1_vec = c()
    y2_vec = c()
    
    while ((b-a)/2 >= accuracy) {
      
  # a_vec = c()
  # b_vec = c()
      phi_0 = (1+sqrt(5))/2 #chislo zolotogo sootnosheniya
      phi = round(phi_0, digits = 2) #okruglyaem, potom nahodim tochki iz zolotogo sootnosheniya
      x1 = b - (b-a)/phi
      x2 = a + (b-a)/phi
        
      x1_vec = c(x1_vec,x1)
      x2_vec = c(x2_vec,x2)
      y1 = f(x1)
      y2 = f(x2)
      
      y1_vec = c(y1_vec,y1)
      y2_vec = c(y2_vec,y2)
      
      
    if (y1 > y2) {
      a = x1
   
    }
     else  {
      b = x2
   
     }
    }
    x_total = c(x1_vec,x2_vec)
    y_total = c(y1_vec,y2_vec)

    
    
      x_extr = (a+b)/2
      y_extr = f(x_extr)
    
      cat("extremum raven =", y_extr, "v tochke = ", x_extr, "naiden za",length(x_total), "iteratsii" )

        points(x_total,y_total)
  
    }
    golden(-5,3)
  