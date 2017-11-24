BinomialTreeGreeks<- function(S0,K,sigma,r,div,T,n, counter1= 1, counter2=1, counter3=1, counter4=1, returncounter=0 ) {
  # parameters of the binomial tree (Stock price, spot price, volatality, rate of interest, expiry time, number of steps)
  dt<-T/n #Total intervals
  u<-exp(sigma*sqrt(dt)) #up factor
  
  
  d<-1/u #down factor
  
  p<-(exp((r-div)*dt)-d)/(u-d) # up probability
  
  q<-1-p #down probabilty
  
  disc<-exp(-r*dt);  #discounting factor
  
  #initialize matrices
  
  stockM<-matrix(0,nrow=n+1,ncol=n+1)   #Stock price matrix
  
  optionM<-matrix(0,nrow=n+1,ncol=n+1)  # option price matrix
  stockM[1,1]<-S0   #first element is simply S0
  x = dim(stockM)  
  for(j in 2:x[2]){
    for(i in 1:j-1){
      stockM[i,j]<-(stockM[i,j-1]*u) ##looping to construct the table
    }
    stockM[i+1,j]<-stockM[i,j-1]*d
    
  }
  optionM[,x[2]]<- (stockM[,x[2]]-K) ##option tree
  for(i in 1:x[1]){
    if(optionM[i,x[2]]< 0){
      optionM[i,x[2]]<-0  ## removing negative values
    }
  }
 
  
  c = x[2]-1;
  for(j in c:-1:1){
    for(i in j:-1:1){
      optionM[i,j]<-disc*(p*optionM[i,j+1]+q*optionM[i+1,j+1]) ##discounting option tree
    }
  }
  
  #print('Stock tree')
  #print(stockM)
  
  #print('Option Tree')
  #print(optionM)
  
  value<-optionM[1,1]
  if(returncounter == 1) {
    return(value)
  }
  
  w1<-optionM[1,2]
  w2<-optionM[2,2]
  s1<-stockM[1,2]
  s2<-stockM[2,2]
  w3<-optionM[1,3]
  w4<-optionM[2,3]
  s3<-stockM[2,3]
  s4<-stockM[3,3]
  s5<-stockM[1,3]
  w5<-optionM[3,3]
  
  #######
  
  delta = (w1-w2)/(s1-s2);
  print("Delta")
  print(delta)
  
  
  ######
  deltaUp = (w3-w4)/(s5-s3);
  deltaDown = (w4-w5)/(s3-s4);
  gamma = (deltaUp-deltaDown)/((s5-s4)/2);
  print("Gamma")
  print(gamma)
  #######
  theta = (w4-value)/(2*dt);
  print("Theta")
  print(theta)

  if(counter1== 1){
    counter1 <- 0;
    vegaUp = do.call("BinomialTreeGreeks", list(S0,K,sigma + 0.01,r,div,T,n, 0, 1, 1, 1, 1)); 
    
  }
  if(counter2==1){
    counter2 <- 0;
    vegaDown = do.call("BinomialTreeGreeks",list(S0,K,sigma - 0.01,r,div,T,n, 0, 0, 1, 1, 1));
  }
  
  vega = (vegaUp - vegaDown)/(2*0.01);
  print("vega")
  print(vega)
  ##########
  if(counter3==1){
    counter3 <- 0;
    rhoUp = do.call(BinomialTreeGreeks,list(S0,K,sigma,r+0.01,div,T,n, 0, 0, 0, 1, 1));
  }
  
  if(counter4 == 1){
    counter4 <-0;
    rhoDown = do.call(BinomialTreeGreeks,list(S0,K,sigma,r-0.01,div,T,n, 0, 0, 0, 0, 1));
  }
  
  rho = (rhoUp - rhoDown)/(2*0.01);
  print("rho")
  print(rho)
  ######
}
BinomialTreeGreeks(1510.85,1515,0.02240736,0.05,0,2,8)
  