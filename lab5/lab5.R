mu <- c(0,0)  
p = 0
Sigma <- matrix(c(1,p,p, 1), 2)
n = 20
#set.seed(100)
selection <- mvrnorm(n = n, mu = mu, Sigma = Sigma ) 
require(ellipse)
confidence.ellipse <- ellipse(Sigma,centre=mu,level=0.99,npoints=100)
plot(confidence.ellipse,type="l", xlim=c(-3, 3),ylim=c(-3, 3), main = "rho = 0.9")
par(new=TRUE)
plot(selection,  axes = FALSE, ann = FALSE, col="blue", pch=19)
