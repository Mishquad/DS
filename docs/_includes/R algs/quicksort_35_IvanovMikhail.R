quicksort = function(A) {
  
  if (length(A) <= 1) {
    return(A)
  }
  
  key = A[1]
   leftpart = c()
   middlepart = c()
   rightpart = c()
  
   for (i in 1:length(A)) {
     if (A[i] < key) {
       leftpart = c(leftpart, A[i])
     }
     else if (A[i] == key) {
       middlepart = c(middlepart, A[i])
     }
     else {
       rightpart = c(rightpart, A[i])
     }
   }

   
 L = quicksort(leftpart)
 R = quicksort(rightpart)
  newresult = c(L,middlepart,R)
  
  return(newresult)
}
A = c(4,7,2,1,9,11,2,20,3,4)
 quicksort(A)