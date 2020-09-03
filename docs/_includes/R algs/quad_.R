quad_equat = function(wtf, verbose = FALSE) {
  
   discr = wtf[2] * wtf[2] - 4 * wtf[1] * wtf[3] #nu tut discriminant nahodim
   
    all.equal(discr, wtf[2] * wtf[2] - 4 * wtf[1] * wtf[3] )
 
    if (discr > 0) {
  
      x_1 = (-wtf[2] + sqrt(discr)) / (2 * wtf[1])
      x_2 = (-wtf[2] - sqrt(discr)) / (2 * wtf[1])
  
        if (verbose) {
 
          cat("uravnenie imeet 2 kornya, pervii:", x_1, "vtoroi:", x_2)
          spisok_znachenii = c(x_1, x_2)
        }
  
          else { 
            
          spisok_znachenii = c(x_1, x_2) #vektor poluchivshihsya reshenii
  
          }
      
  return(spisok_znachenii)
    
  }
    else if (discr == 0) {
      
      x_1 = (-wtf[2]) / (2 * wtf[1])
    
        if (verbose) {
        cat("edinstvennoe reshenie uravneniya :", x_1)
        }
      
        else {
    
        spisok_znachenii = c(x_1) #vektor poluchivshihsya reshenii
 
         }
  return(spisok_znachenii)
    
  }
 
    else if (discr < 0) {
      
      if (verbose) {
      spisok_znachenii = ("Veshestvennih korney net t.k. discriminant < 0, sorry")
      
      }
      else {
      spisok_znachenii = ("net kornei")
      }
  return(spisok_znachenii)
  }
  
  
  
}

quad_equat(c(1,2,3), verbose = TRUE)
