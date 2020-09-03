library(igraph)

adjlist = list()
adjlist[[1]] = c(2,3)
adjlist[[2]] = c(1,4)
adjlist[[3]] = c(1,4)
adjlist[[4]] = c(2,3,5)
adjlist[[5]] = c(4)
adjlist[[6]] = c(7)
adjlist[[7]] = c(6)


grr=graph_from_adj_list(adjlist, mode="all") 
plot(grr)


#visited - vektor poseshennikh vershin
n = length(adjlist)
visited = rep(F,n)
visited

k = 0 #schetchik proidennogo puti

dfs = function(startv) {

  cat("poseshennie vershini v sootv. s ih nomerom:", visited,"\n")
  cat("seichas mi v vershine:", startv,"\n")
  cat("dlina puti do etoi vershini ot nachalnoj ravna:",k,"\n")
  
  visited[startv] <<- TRUE
  
    for (i in adjlist[[startv]]) {
      if (visited[i] == FALSE) {
        k <<- k+1
        dfs(i)
      }
    }
}
dfs(1)