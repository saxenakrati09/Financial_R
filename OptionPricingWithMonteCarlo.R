#################################################################################
#This code implements Europian option pricing with help of Monte Carlo estimaion#
#Here I am implementing the paper "Monte Carlo simulations and option pricing"  #
#the results are verified with black_scholes formula                            #
#################################################################################

#Read Data for stocs
MSFT = read.csv("C:\\Users\\Vivek Vijay\\Desktop\\Quantitative finance lab\\APPLE_13.csv")
dataSize = length(MSFT[,5])
option_price = 0

#Read the Adjusted close prices
tMSFT = matrix(MSFT[,5])

retM = matrix(MSFT[1:dataSize-1,5])

#Arrange Data in assending order
for(i in 1:dataSize){
  MSFT[i,6] = tMSFT[(dataSize + 1 - i),1]
}
tMSFT = matrix(MSFT[,6])

#Code to find the returns
for(i in 1:dataSize-1){
  retM[i,1] = log(tMSFT[i+1]/tMSFT[i])
}

r = mean(retM)
sig = sd(retM)
K = 50  #Strike Price
no_time_ticks = 24
#Monte carlo agorithm for otion pricing
for (repetation in 1:1000){
  norm = matrix(rnorm(1*no_time_ticks,mean=0,sd=sig), 1, no_time_ticks)
  Y = norm
  Y[1] = MSFT[1,5]
  for(i in 1:(no_time_ticks - 1)){
    Y[1,i+1] = Y[1,i]*exp(r + norm[1,i+1])
  }
  option_price = option_price + max(0,(Y[no_time_ticks] - K))
}
option_price = exp(-r)*option_price/1000

#Verification using Black scholes
# Black-Scholes Option Value
# Call value is returned in values[1], put in values[2]
S = MSFT[1,5]
X = K
rf = 0.05
T = no_time_ticks/365
sigma = sig#sd(MSFT[,5])
{
  values <- c(2)
  
  d1 <- (log(S/X)+(rf+sigma^2/2)*T)/sigma*sqrt(T)
  d2 <- d1 - sigma * sqrt(T)
  
  values[1] <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
  #values[2] <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)
  
  values
}