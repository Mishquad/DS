m = matrix(1:16, nrow = 4, ncol = 4, byrow = TRUE)


  for(i in 1:nrow(m)){
    for(j in 1:ncol(m)) {

    m[i] = 2*m[i]
    m[j] = 2*m[j]
  
    }
  }
print(m)

