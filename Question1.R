A = read.csv('/home/manu/Desktop/7th_Semester/LSNS/Finance/Exam/Data-A.csv')
B = read.csv('/home/manu/Desktop/7th_Semester/LSNS/Finance/Exam/Data-B.csv')
C = read.csv('/home/manu/Desktop/7th_Semester/LSNS/Finance/Exam/Data-C.csv')

data_size <- 2261

retA = A[2: 2261, 10]
retB = B[2: 2261, 10]
retC = C[2: 2261, 10]

u1 = mean(retA)
u2 = mean(retB)
u3 = mean(retC)

sd1 = sd(retA)
return_vector = c(u1, u2, u3)

sigma.mat = matrix(c(cov(retA,retA),cov(retA,retB),cov(retA,retC),cov(retB,retA),cov(retB,retB),cov(retB,retC),cov(retC,retA),cov(retC,retB),cov(retC,retC)),nrow=3, ncol=3)

top.mat = cbind(2*sigma.mat, rep(1, 3))
bot.vec = c(rep(1, 3), 0)
Am.mat = rbind(top.mat, bot.vec)
b.vec = c(rep(0, 3), 1)
z.m.mat = solve(Am.mat)%*%b.vec
m.vec = z.m.mat[1:3,1]


##Expected returns
returns = as.numeric(crossprod(m.vec, return_vector))


sig2.pz = as.numeric(t(m.vec)%*%sigma.mat%*%m.vec)
sig.pz = sqrt(sig2.pz)
