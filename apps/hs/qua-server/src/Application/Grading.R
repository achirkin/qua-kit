n <- 10000000

ri <- rnorm(n)
rj <- rnorm(n)
v  <- rnorm(n)
delta <- sapply((ri - rj)*exp(v) + rnorm(n, sd=0.1), function(d) if (d>0) 1 else -1)
y <- delta * (ri - rj) - 1.11332601


c(mean(ri),mean(rj),mean(v),mean(delta))
c(var(ri),var(rj),var(v),var(delta))
c(cor(ri,delta), cor(rj,delta), cor(v,delta))



zi <- delta * log(1 + exp(v + delta * (rj - ri) ) )
mean(zi)
var(zi)
c(cor(ri,zi), cor(rj,zi), cor(v,zi))



zj <- delta * log(1 + exp(v + delta * (ri - rj) ) )
mean(zj)
var(zj)
c(cor(ri,zj), cor(rj,zj), cor(v,zj))


# Ey, Dy, Cy
c(mean(y), var(y), cor(y,v))
# Ez, Dz, Cz
c(mean(zi),var(zi),cor(ri,zi))
