#1要因3水準、逐次検定BF
library(BayesFactor)
#繰り返し回数
turn = 20000
#毎回最大サンプル数
N = 15000

rscaleFixed_medium = data.frame()
rscaleFixed_wide = data.frame()
rscaleFixed_ultrawide = data.frame()
S_ANOVA_BF <-function(star_ss=20){
  bf_medium = c()
  bf_wide = c()
  bf_ultrawide = c()
  #star_ss = 20
  a = rnorm(star_ss)
  b = rnorm(star_ss)
  c = rnorm(star_ss)
  foc = gl(3,star_ss)
  dat = data.frame(dat = c(a,b,c),foc)
  bf_mediun = exp(anovaBF(dat~foc, data = dat,rscaleFixed = "medium", progress = FALSE)@bayesFactor["bf"])[[1]]
  bf_wide = exp(anovaBF(dat~foc, data = dat,rscaleFixed = "wide", progress = FALSE)@bayesFactor["bf"])[[1]]
  bf_ultrawide = exp(anovaBF(dat~foc, data = dat,rscaleFixed = "ultrawide", progress = FALSE)@bayesFactor["bf"])[[1]]
  for (i in 1:4980) {
    a = c(a,rnorm(1))
    b = c(b,rnorm(1))
    c = c(c,rnorm(1))
    dat = data.frame(dat = c(a,b,c),foc = gl(3,length(a)))
    bf_mediun = c(bf_mediun,exp(anovaBF(dat~foc, data = dat,rscaleFixed = "medium", progress = FALSE)@bayesFactor["bf"])[[1]])
    bf_wide = c(bf_wide,exp(anovaBF(dat~foc, data = dat,rscaleFixed = "medium", progress = FALSE)@bayesFactor["bf"])[[1]])
    bf_ultrawide = c(bf_ultrawide,exp(anovaBF(dat~foc, data = dat,rscaleFixed = "medium", progress = FALSE)@bayesFactor["bf"])[[1]])
    print(i)
  }
  return(data.frame(bf_mediun,bf_wide,bf_ultrawide))
}

timestart<-Sys.time()
a = S_ANOVA_BF()
runningtime<-Sys.time()-timestart

