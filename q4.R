BinomialTree<- function (putcall,exerciseType,S0,K,sigma,r,div,T,n) {
  # parameters of the binomial tree
  dt<-T/n
  u<-exp(sigma*sqrt(dt))
  print(u)
  
  d<-1/u
  
  p<-(exp((r-div)*dt)-d)/(u-d)
  print(p)
  q<-1-p
  
  disc<-exp(-r*dt);
  print(disc)
  #initialize matrices
  print(u);
  stockM<-matrix(0,nrow=n+1,ncol=n+1)
  
  optionM<-matrix(0,nrow=n+1,ncol=n+1)
  stockM[1,1]<-S0
  x = dim(stockM)
  for(j in 2:x[2]){
    for(i in 1:j-1){
      stockM[i,j]<-(stockM[i,j-1]*u)
    }
    stockM[i+1,j]<-stockM[i,j-1]*d
    
  }
  
  # for (i in 1:r){
  #   for (j in 2:x[2])
  #   {
  #    stockM[i,j] <- stockM[i,j-1]*u
  #   }
  #   
  #   stockM[i+1,j] <- stockM[i,j-1]*d
  # }   
  
  print(stockM)
  #set the z parameter to calculate the option payoff depending on the selected 
  #option type(call,put)
  
  if (putcall=='call'){
    z=1
  }else{
    if (putcall=='put'){
      z=-1
    }else{
      print("check option type")
    }
  }
  #insert terminal value
  optionM[,x[2]]<- max(z*(stockM[,x[2]]-K),0)
  #value call by working backward from time n-1 to time 0
  print(optionM)
  c = x[2]-1;
  if(exerciseType=='E'){
    for(j in c:-1:1){
      for(i in j:-1:1){
        optionM[i,j]<-disc*(p*optionM[i,j+1]+q*optionM[i+1,j+1])
      }
    }
  }else{
    if(exerciseType=='A'){
      for(j in c:-1:1){
        for(i in j:-1:1){
          optionM[i,j]<-max(z*(stockM[i,j]-K),disc*(p*optionM[i,j+1]+q*optionM[i+1,j+1]))
        }
      }
    }
    else{
      print("check exercise type")
    }
  }
  print(exerciseType)
  print(optionM)
}
BinomialTree('put','A',1513.2,1501,0.02240267,0.05/4,0,2,8)

