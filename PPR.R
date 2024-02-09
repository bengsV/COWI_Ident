

p = runif(3)

p = p/sum(p)



PPR<-function(p,delta){
  S <- c(0,0,0)
  its <- 0
  while(TRUE){
    #compare i,j
    o = sample(c(1,2,3),1,prob=p)
    if (o==1){
      S[1] <- S[1]+1
    }
    if(o==2){
      S[2] <- S[2]+1
    }
    if(o==3){
      S[3] <- S[3]+1
    } 
    or = order(S,decreasing = T)
    if (dbeta(0.5,S[or][1]+1,S[or][2]+1  ) <= delta/2 ){
      return (list(k = which(S==max(S)),its = its))
    }
    its <- its + 1
  }
}

p
PPR(p,0.01)



  
  



