asset.names <- c("A", "B", "C")
mu.vec = c(0.000731245, 0.000247127, 0.000682476)
names(mu.vec) = asset.names
sigma.mat = matrix(c(0.0005018795, 0.0005790478, 0.0003630035,
                     0.0005790478, 0.0006680814, 0.0004188184,
                     0.0003630035, 0.0004188184, 0.0002625561),nrow=3, ncol=3)
dimnames(sigma.mat) = list(asset.names, asset.names)

################################################################

x.vec = rep(1,3)/3
names(x.vec) = asset.names
mu.p.x = crossprod(x.vec,mu.vec)
sig2.p.x = t(x.vec)%*%sigma.mat%*%x.vec
sig.p.x = sqrt(sig2.p.x)

#######################################################

y.vec = c(0.8, 0.4, -0.2)
names(x.vec) = asset.names
sig.xy = t(x.vec)%*%sigma.mat%*%y.vec

top.mat = cbind(2*sigma.mat, rep(1, 3))
bot.vec = c(rep(1, 3), 0)
Am.mat = rbind(top.mat, bot.vec)
b.vec = c(rep(0, 3), 1)
z.m.mat = solve(Am.mat)%*%b.vec
m.vec = z.m.mat[1:3,1]
m.vec