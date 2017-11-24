T = 1000
theta = 0
norm = matrix(rnorm(3*T,mean=5,sd=1), 3, T)
h = norm[1,]
for (i in 1:T){
  h[i] = (norm[1,i] + norm[2,i] + norm[3,i])/3
  theta <- theta + h[i]
}
theta = theta/T