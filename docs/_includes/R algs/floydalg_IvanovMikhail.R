library(igraph)

mygraph <- graph(edges = c(1,2,3,1,
                           1,4,2,3,
                           1,3,2,4,
            A               4,3,4,4
                           ), n = 4,directed=T)

E(mygraph)$weight[1] <- 1
E(mygraph)$weight[2] <- 5
E(mygraph)$weight[3] <- 8
E(mygraph)$weight[4] <- 4
E(mygraph)$weight[5] <- 1
E(mygraph)$weight[6] <- 3
E(mygraph)$weight[7] <- 7
E(mygraph)$weight[8] <- 3

vec_weight <- E(mygraph)$weight

plot(mygraph, edge.width = vec_weight)

#delaem matricu smezhnosti
adjmat <- matrix(NA)
adjmat <- mygraph

#zamenim nulevie rasstoyania 
for(i in 1:length(V(mygraph))){
  for(j in 1:length(V(mygraph))){
    if(mygraph[i,j]==0){
      mygraph[i,j] = Inf
    }
  }
}

#idem po vsem vershinam i ishem bolee korotkij put'
for(k in 1:length(V(mygraph))){
  for(i in 1:length(V(mygraph))){
    for(j in 1:length(V(mygraph))){
      # novij ves rebra raven minimalnomu mezhdu starim i summoy reber (i,k) + (k,j) (esli cherez <k> projti bistree)
      mygraph[i,j] <- min(mygraph[i,j], mygraph[i,k] + mygraph[k,j])
      
    }
  }
}

print("poluchaem matricu kratchaishih putej iz vershini I v vershinu J")
print(mygraph[])
#ishodnaya matrica smezhnosti
#print(adjmat[])