MSFT = read.csv("Data-A.csv")
APPLE = read.csv("Data-B.csv")
SBUX = read.csv("Data-C.csv")

dataSize <- 2261

#Read the Adjusted close prices
tMSFT = matrix(MSFT[,7])
tAPPLE = matrix(APPLE[,7])
tSBUX = matrix(SBUX[,7])

retM = matrix(MSFT[2:dataSize-1,7])
retA = matrix(APPLE[2:dataSize-1,7])
retS = matrix(SBUX[2:dataSize-1,7])

#Arrange Data in assending order
for(i in 1:dataSize){
  MSFT[i,6] = tMSFT[(dataSize + 1 - i),1]
  APPLE[i,6] = tAPPLE[(dataSize + 1 - i),1]
  SBUX[i,6] = tSBUX[(dataSize + 1 - i),1]
}
tMSFT = matrix(MSFT[,6])
tAPPLE = matrix(APPLE[,6])
tSBUX = matrix(SBUX[,6])

#Code to find the returns
for(i in 1:dataSize-1){
  retM[i,1] = log(tMSFT[i+1]/tMSFT[i])
  retA[i,1] = log(tAPPLE[i+1]/tAPPLE[i])
  retS[i,1] = log(tSBUX[i+1]/tSBUX[i])
}

#To find variance covariance matrix we need a single matrix so cbind
Z = cbind(retA,retM,retS)
sigmaMat = cov((Z))

avgA = mean(retA)
avgM = mean(retM)
avgS = mean(retS)

I = 0
for(repetation in 1:1000){
  #for 6 months or 125 days
  #generate future data for given sigma matrix (data in the form of returns)
  T = 125
  chole = chol(sigmaMat)
  norm = matrix(rnorm(3*T,mean=0,sd=1), 3, T)            #normal random vector
  hist(norm[1,],50)
  X = t(chole)%*%norm
  
  #generate future data i.e. future stock data with help of mu and sigma
  Y = X
  Y[1,1] = APPLE[1,7]
  Y[2,1] = MSFT[1,7]
  Y[3,1] = SBUX[1,7]
  for(i in 1:(T-1)){
    Y[1,i+1] = Y[1,i]*exp(avgA + X[1,i+1])
    Y[2,i+1] = Y[2,i]*exp(avgM + X[2,i+1])
    Y[3,i+1] = Y[3,i]*exp(avgS + X[3,i+1])
  }
  hist(Y[2,],nclass=50)
  
  #Now I calculate my Termianal wealth
  na = 100
  nm = 100 
  ns = 100
  Wi = na*Y[1,1]+nm*Y[2,1]+ns*Y[3,1]
  Wt = na*Y[1,T]+nm*Y[2,T]+ns*Y[3,T]
  if(Wt/Wi > 1.1){
    I = I + 1
  }
}
theta = I/1000
theta

