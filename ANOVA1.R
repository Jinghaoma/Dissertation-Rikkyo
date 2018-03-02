#1要因3水准，H0正确，使用贝叶斯因子和ANOVA，非逐次
#1要因3水準、H0が正しい、ベイズファクターとANOVA、逐次ではない

library(BayesFactor)

bf_anova <-function(level=3,evelevel_N=100,
                    rscaleFixed="ultrawide",progress = FALSE){
  #测试用
  #level=3
  #evelevel_N=100
  #rscaleFixed = "ultrawide"
  
  dat = rnorm(evelevel_N*level)
  dat = data.frame(dat,le = gl(level,evelevel_N))
  ano = summary(aov(dat~le,data = dat))
  p_v = ano[[1]][["F value"]][1]
  
  bf_result = anovaBF(dat~le,data = dat,rscaleFixed = rscaleFixed,
                      whichModels = "all",progress = progress)
  bf = exp(bf_result@bayesFactor["bf"])
  return(data.frame(p_v,bf))
}

an = data.frame()
for (i in 1:10000) {
  re = bf_anova()
  an = rbind(an,re)
  print(i)
}

