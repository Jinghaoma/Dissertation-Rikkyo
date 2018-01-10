x = c(rnorm(50),rnorm(50,1),rnorm(50,2))
f = gl(3,50)
ao = aov(x~f)
ao = summary(ao)
ao

tt = c()
for (i in 1:10000) {
  n = 50
  t = rnorm(50,10,10)
  t1 = sd(t)^2*(n-1)
  #t1 = sum((t-mean(t))^2)
  tt = c(tt,t1)
}
mean(tt)
