#proveryaem snachala vojdet li odna otkritka v odin konvert
library(seqinr) #dlya funkcii swap
odno = function(A,B,a,b) {
  
  
  #cherez sootnosheniya posle || proverka mozhno li pomestit' otkritku pod uglom
  #obyazatelnoe uslovie sootnosheniya : chtobi pervaya velichina bila bolshe vtoroj (A>B, a>b) 
  #dlya etogo potom budem menyat' razmeri mestami (t.k. u nas pryamougolniki, to eto nichego ne menyaet)
  if ((a <= A && b <= B) || (a > A &&
                             B >= (2*a*b*A + (a*a-b*b)*sqrt(a*a+b*b-A*A)) / (a*a+b*b))) {
    cat("mozhno pomestit' otrkitku takih razmerov v konvert","\n")
    return(TRUE)
  }
  else {
    cat("nelzya pomestit' otrkitku takih razmerov v konvert","\n")
    return(FALSE)
  }
}
  #odno(125,11,20,15)

all_letters = function(){
  finishlist = list()
  vek1 = c() #zdes' hranim otkritki kotorie mozhno umestit' v dannij konvert

  #eto massiv otkritok
  listotkritok = list()
  listotkritok[[1]] = c(10,10)
  listotkritok[[2]] = c(125,10)
  listotkritok[[3]] = c(8,3)
  listotkritok[[4]] = c(11,4)
  listotkritok[[5]] = c(13,8)
  listotkritok[[6]] = c(20,15)
  listotkritok[[7]] = c(4,6)
  listotkritok[[8]] = c(7,10)
  listotkritok[[9]] = c(47,82)
  listotkritok[[10]] = c(10,3)
  #eto massiv konvertov
  listkonvert = list()
  listkonvert[[1]] = c(10,10)
  listkonvert[[2]] = c(125,11)
  listkonvert[[3]] = c(2,3)
  listkonvert[[4]] = c(7,18)
  listkonvert[[5]] = c(24,19)
  listkonvert[[6]] = c(11,3)
  listkonvert[[7]] = c(429,741)
  listkonvert[[8]] = c(5,10)
  listkonvert[[9]] = c(9,7)
  listkonvert[[10]] = c(5,6)
  
  N <<- length(listkonvert)

for (i in 1:N){
  if (listkonvert[[i]][1] < listkonvert[[i]][2]) {
    swap(listkonvert[[i]][1],listkonvert[[i]][2])
  }
  for (j in 1:N) {
    if (listotkritok[[j]][1] < listotkritok[[j]][2]) {
      swap(listotkritok[[j]][1],listotkritok[[j]][2])
    }
    #primenyaem funkciu dlya odnoj otkritki ko vsemu listu otkritok
    A1 = listkonvert[[i]][1]
    B1 = listkonvert[[i]][2]
    a1 = listotkritok[[j]][1]
    b1 = listotkritok[[j]][2]
    
    
    
rabota = odno(A1,B1,a1,b1)
#sostavlyaem itogovii list dlya postroeniya grafa (v konvert "i" mozhno polozhit "j" otkritku)
    if (rabota == TRUE) {
      vek1 = c(vek1,j)
      finishlist[[i]] = c(vek1)
      cat("\n","v konvert pod nomerom",i,"mozhno vlozhit' otkritku pod nomerom:",j,"\n")
       # finishlist[[paste("V konvert pod nomerom",sep=" ",i, "umeshayutsya otkritki pod nomerami")]] = vek1
    }

  }
  vek1 = NULL
}
   return(finishlist)
      }
 all_letters()

please = all_letters()
please

  library(igraph)
  gr = graph_from_adj_list(please)
  plot.igraph(gr)


