
mergesort = function(numbers) {

   if (length(numbers) <= 1) {
   return(numbers)
  }

  middle = length(numbers) %/% 2
  
      leftpart = mergesort(numbers[1:middle])
      rightpart = mergesort(numbers[(middle+1):length(numbers)])
  
  return(mergefunc(leftpart, rightpart))
}

mergefunc = function(leftpart, rightpart) {
  result = c()
  while (length(leftpart) > 0 && length(rightpart) > 0) {
    if (leftpart[1] <= rightpart[1]) {
      result = c(result, leftpart[1])
      leftpart = leftpart[-1]
    }
    else {
      result = c(result, rightpart[1])
      rightpart = rightpart[-1]
    }
  }
  if (length(leftpart) > 0) {
    result = c(result,leftpart)
  }
  if (length(rightpart) > 0) {
    result = c(result,rightpart) 
  }
  return(result)
}




numbers = c(1,3,2,6,5,9,11,23,15,0)
 mergesort(numbers)
