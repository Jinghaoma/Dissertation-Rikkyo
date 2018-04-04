library(GGally)
load("dat_h0.Rdata")
load("dat_h1.Rdata")
#data_h0のBFを逆数変換
a = 1/dat_h0$bf
dat = cbind(dat_h0,a)
dat <- subset(dat, select = -bf )
dat_h0 = data.frame(t.t=dat$t.t,t.p=dat$t.p,bf01=dat$a,bay_over=dat$bay_over)
rm(a,dat)
#h0の散歩図を描く
ggscatmat(dat_h0, columns = 4:2,alpha = 0.01)
#各指標の偽陽性割合
h0.p_0.05 = nrow(subset(dat_h0,t.p<0.05))/2000
h0.bf_3 = nrow(subset(dat_h0,bf01<1))/2000
h0.bayover_0.05 = nrow(subset(dat_h0,bay_over<0.05 | bay_over>0.95))/2000

#h1の散歩図を描く
ggscatmat(dat_h1, columns = 4:2,alpha = 0.01)
#各指標の偽陰性割合
h1.p_0.05 = nrow(subset(dat_h1,t.p>0.05))/2000
h1.bf_3 = nrow(subset(dat_h1,bf<3))/2000
h1.bayover_0.05 = nrow(subset(dat_h1,bay_over>0.05 & bay_over<0.95))/2000

