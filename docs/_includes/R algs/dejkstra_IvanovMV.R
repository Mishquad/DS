library(igraph)
#matrica vesov
M <- matrix(0,nrow=4,ncol=4)
M[1,] <- c(1,1,8,8)
M[2,] <- c(Inf,Inf,4,3)
M[3,] <- c(5,Inf,7,Inf)
M[4,] <- c(Inf,Inf,7,Inf)
M

g <- M
g[g==Inf]<-0
a <- graph.adjacency(g,mode='directed', weighted = T)
#
plot.igraph(a,edge.label = c(t(g)[t(g) != 0]), edge.arrow.size = 0.5)
plot.igraph(a,edge.label = c(t(g)[t(g) != 0]), edge.arrow.size = 0.5)

mn_f <- function(A,R){ #ishem blizhaishuyu iz neprojdennih vershin; A,R beret v funkcii @dijkstra@
  Rm <- Inf
  m <- -1
  for(i in 1:length(R)){
    if(R[i]<Rm & A[i]==0){ #iterativno snizhaem do blizh znacheniya k versine
      m <- i
      Rm <- R[i]
    }
  }
  return(m)
}

Dijkstra_f <- function(v1,v2,g){
  # if(nrow(g)!= ncol(g)){ 
  #     stop('matrica ne kvadratnaya')
  # }
  R <- g[v1,] #opredelyaem vershinu v matrice (stroku)
  A <- c(rep(0,nrow(g))) #delaem vektor iz 0 potom idem do nuzhnoj vershini
  A[v1] <- 1 #nachinaem put'ot v1, pomechaya vershinu
  P <- c(rep(0,nrow(g)))
  P[R<Inf] <- v1
  while(prod(A)==0){#poka ne budut projdeni vse vershini
    k <- mn_f(A,R) #min neprojdennaya vershina grafa
    
    for(i in which(g[k,] != Inf)){ #skladivaem vesa
      if(R[i]>R[k]+g[k,i]){
        R[i] <- R[k]+g[k,i]
        P[i] <- k
      }
    }
    A[k] <- 1
  }
# stroim put' ot nachalnoj k konechnoj
  path_f <- v2 
  while(path_f[1] != v1){
     path_f <- c(P[path_f[1]],path_f)
  }
  S <- R[v2]
  cat("Кратчайший путь из вершины",v1,"в",v2,"равен",S,",а маршрут",path_f)
}

Dijkstra_f(4,2,M)
