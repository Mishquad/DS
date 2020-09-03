
library(igraph)

g1 <- graph(edges = c(1,2,1,3,2,4,2,5,2,6,5,6,6,5,6,3,5,7,7,8), n = 8,directed=T)
plot(g1)


vec_mat <- rep(0,length(V(g1)))
print(vec_mat)

#V - nomer vershini, do kotoroj nado idti
v = 8
W <- c()

#rasstoyanie do vershini
pathfinder <- function(f,vec_mat){
for (i in 1:length(vec_mat)){
  if (g1[f,i] == 1){
    
    if(f == 1){
      vec_mat[i] <- vec_mat[i] + 1
    }
    if(vec_mat[i] == 0){
      vec_mat[i] <- vec_mat[f] + 1
    }
  }
  
}
  return(vec_mat)
}
#cherez matritsu smezhnosti ishem rasstoyanie do blizhaishih vershin
adjmatfunc <- function(vec_mat){
  for(j in 1:length(vec_mat)){
    f <- j
    vec_mat <- pathfinder(f,vec_mat)
  }
  return(vec_mat)
}

path <- adjmatfunc(vec_mat)
R <- path[v]
# print(R)

#ishem vershini cherez kotorie idet put'
for (k in 1:path[v]){
  for(m in 1:length(path)){
    if(path[v] - path[m] == 1){
      if(g1[m,v] == 1){
        v <- m
        W <- append(W,m)
        }
    } 
  }
}
cat("put' do nuzhnoi vershini idet cherez vershini s nomerami",rev(W),"\n")
cat("rasstoyanie ot pervoi vershini do nuzhnoj", R)