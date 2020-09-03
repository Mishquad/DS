a <- matrix(1:12, nrow = 4, byrow = TRUE)
a

#1. summa chetnih
sumchet <- function(a, type = c()) {
        y = 0
        x = c()
        
        for (i in 1:length(a)) {
         if (a[i] %% 2 == 0) {
            x = c(x, a[i])
            
           }
        }
           sumchis = sum(x)
     
            
        return(sumchis)
 }
sumchet(a)


#2. proizvedenie otricatelnih

negative <- function(a){
    x <- a[a < 0]
    mult <- 1
    for (i in 1:length(x)){
        mult <- mult * x[i]
    }
    return(mult)
}
negative(a)

#3.srednee geom v kazhdoj stroke
sredgeom <- function(a){
    proizv <- c()
    mnozh <- 1
    for(i in 1: nrow(a)){
        for(j in 1:ncol(a)){
            mnozh <- mnozh * a[i,j]
        }
        mnozh <- mnozh ** (1/(ncol(a)))
        proizv <- c(proizv,mnozh)
        mnozh <- 1
    }
    return(proizv)
    
}
sredgeom(a)

#4.transpon matrici
trans <- function(a,type = c()){
    newdim <- dim(a)[1] * dim(a)[2]
    vec <- numeric(newdim)
    b <- matrix(vec,nrow = dim(a)[2],byrow = TRUE)
    for(i in 1:nrow(a)){
        for(j in 1:ncol(a)){
            b[j,i] <- a[i,j]
        }
    }
    return(b)
}
trans(a)