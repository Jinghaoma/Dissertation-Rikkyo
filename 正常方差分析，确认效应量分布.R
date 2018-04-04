get_population <- function(n, eta2, SD=1) {
  cohen_f <- sqrt(3*eta2/((1-eta2)*2))
  x <- rnorm(n, 0, SD) + cohen_f
  y <- rnorm(n, 0, SD) - cohen_f
  z <- rnorm(n,0,SD)
  return(cbind(x,y,z))
}

a = get_population(200,0.5)
#print (summary(aov(y ~ x,data = a)))
eta2 <- function(y){
  SSa <- y[[1]]$"Sum Sq"[1]
  SSe <- y[[1]]$"Sum Sq"[2]
  SSt <- SSa + SSe
  eta <- SSa / SSt
  return(eta)
}

eta = c()
for (i in 1:500) {
  samp = get_population(200,0.5)
  samp = data.frame(y = c(samp),x = factor(rep(1:3,each=200)))
  b = summary(aov(y ~ x,data = samp))
  eta = c(eta,eta2(b))
}
print(summary(eta))