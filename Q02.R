#Read Data for stocks
Stock_A = read.csv("D:\\Users\\Shukla\\Downloads\\LSNS\\Data-A.csv")
dataSize = 125

#Read the Adjusted close prices
tStock_A = matrix(Stock_A[1:dataSize,7])

retM_A = matrix(tStock_A[1:dataSize-1,1])

#Arrange Data in assending order
for(i in 1:dataSize){
  Stock_A[i,8] = tStock_A[(dataSize + 1 - i),1]
}
tStock_A = matrix(Stock_A[,8])


#Code to calculate the returns
for(i in 1:dataSize-1){
  retM_A[i,1] = log(tStock_A[i+1]/tStock_A[i])
}

return_Stock_A = mean(retM_A)
sigma_Stock_A = sd(retM_A)


K = 1500  #Strike Price
no_time_ticks = 125

#Monte carlo agorithm for otion pricing
option_price = 0
for (repetation in 1:1000){
  norm = matrix(rnorm(1*no_time_ticks,mean=return_Stock_A,sd=sigma_Stock_A), 1, no_time_ticks)
  Y = norm
  Y[1] = Stock_A[1,5]
  for(i in 1:(no_time_ticks - 1)){
    Y[1,i+1] = Y[1,i]*exp(return_Stock_A + norm[1,i+1])
  }
  option_price = option_price + max(0,(Y[no_time_ticks] - K))
}
option_price = exp(-return_Stock_A)*option_price/1000

print(option_price)
