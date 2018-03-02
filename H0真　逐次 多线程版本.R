S_ANOVA_BF <- function(){
  
  star_s=20
  
  a = rnorm(5000)
  b = rnorm(5000)
  c = rnorm(5000)
  
  l = list()
  fir = star_s
  for (i in 20:length(a)) {
    dat = c(a[1:fir],b[1:fir],c[1:fir])
    l = c(l,list(dat))
    fir = fir + 1
  }
  
  ANOVA_BF <- function(x){
    library(BayesFactor)
    dat = data.frame(dat = x, foc = gl(3,length(x)))
    bf_mediun = exp(anovaBF(dat~foc, data = dat,rscaleFixed = "medium", progress = FALSE)@bayesFactor["bf"])[[1]]
    bf_wide = exp(anovaBF(dat~foc, data = dat,rscaleFixed = "wide", progress = FALSE)@bayesFactor["bf"])[[1]]
    bf_ultrawide = exp(anovaBF(dat~foc, data = dat,rscaleFixed = "ultrawide", progress = FALSE)@bayesFactor["bf"])[[1]]
    return(data.frame(bf_mediun,bf_wide,bf_ultrawide))
  }
  cl <- makeCluster(6) # 初始化6核心集群
  bf <- parLapply(cl,l,ANOVA_BF) # lapply的并行版本
  bf <- do.call('rbind',bf) # 整合结果
  stopCluster(cl) # 关闭集群
  return(bf)
}

bf = list()
for (i in 1:30) {
  bf1 = S_ANOVA_BF()
  bf = c(bf,list(bf1))
}

