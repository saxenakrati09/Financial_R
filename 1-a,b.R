B_data = read.csv("/home/arpit/lsns/exam/Data-B.csv")
returnB = mean(matrix(B_data[1:2261,10]))
sdB = sd(B_data[1:2261,10])

C_data = read.csv("/home/arpit/lsns/exam/Data-C.csv")
returnC = mean(matrix(C_data[1:2261,10]))
sdC = sd(C_data[1:2261,10])

A_data = read.csv("/home/arpit/lsns/exam/Data-A.csv")
returnA = mean(matrix(A_data[1:2261,10]))
sdA = sd(A_data[1:2261,10])

mu.vec = c(returnA, returnB, returnC)
sigma.mat = matrix(c(sdA*sdA, sdA*sdB, sdA*sdC, sdB*sdA, sdB*sdB, sdB*sdC, sdC*sdA, sdC*sdB, sdC*sdC), 3, 3)
print (mu.vec)
print (sigma.mat)