x <- seq(-10,10,length.out=1000)
y <- pnorm(x)

inv.logit <- function(s,x) 1/(1+exp(-s*x))
z <- inv.logit(1.8,x)

plot(x,y)
points(x,z,col='red')
