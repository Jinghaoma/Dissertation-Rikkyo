

batch = 1
f <- paste0("raw/final_eta_0.01_batch_", batch, ".RData")
load(f)
final_all = final

for (batch in 2:6) {
  f <- paste0("raw/final_eta_0.01_batch_", batch, ".RData")
  load(f)
  final_all = rbind(final_all,final)
}
rm(batch,final,f)
final = final_all
rm(final_all)

save(final, file=paste0("raw/final_eta_0.01_all.RData"))

#条件检索
bf3_r1 = subset(final,boundary==3 & r==1)
summary(bf3_r1)
bf5_r1 = subset(final,boundary==5 & r==1)
summary(bf5_r1)
bf15_r1 = subset(final,boundary==15 & r==1)
summary(bf15_r1)
bf30_r1 = subset(final,boundary==30 & r==1)
summary(bf30_r1)


bf30_r1_h0 = length(subset(bf30_r1,logBF<1)$logBF)/3600

bf15_r1_h0 = length(subset(bf15_r1,logBF<1)$logBF)/3600

bf3_r1_h0 = length(subset(bf3_r1,logBF<1)$logBF)/3600

a = bf5_r1$eat.emp
d<-density(a)
plot(d)
abline(v=0.01,lwd=1,col="black")


#subset(student,Gender=="F" & Age<30 ,select=c("Name","Age"))


