euclid = function(a,b) {
  
  if (a==b) {
   return(a) 
    
  }
  else if (a>b) {
    euclid(a-b,b)
    step1 = euclid(a-b,b)
    
    return (step1)
  }
  else if (a<b) {
    euclid(a,b-a)
    step2 = euclid(a,b-a)
    
    return(step2)
  }
}

euclid(155,35)