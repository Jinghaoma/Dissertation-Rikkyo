
# 保存数据
combinedata <- function(cor = 6){
  etas = c(0,0.005,0.01,0.03,0.06,0.1,0.14,0.3,0.5,0.8)
  batchs = c(1:cor)
  for (eta_name in etas) {
    
    finals <- data.frame()
    
    for (batch in batchs) {
      f <- paste0("raw/final_eta_",eta_name,"_batch_",batch,".RData")
      load(f)
      finals <- bind_rows(finals, final)		 
      #print(f)
    }
    save_name = paste0("raw/final_eta_",eta_name,".RData")
    save(finals,file = save_name)
    rm(finals)
    print(save_name)
  }
}
combinedata()



#条件检索
bf3_r1 = subset(finals,boundary==3 & r==1)
summary(bf3_r1)
bf5_r1 = subset(finals,boundary==5 & r==1)
summary(bf5_r1)
bf15_r1 = subset(finals,boundary==15 & r==1)
summary(bf15_r1)
bf30_r1 = subset(finals,boundary==30 & r==1)
summary(bf30_r1)




bf30_r1_h0 = length(subset(bf30_r1,logBF<1)$logBF)/3600

bf15_r1_h0 = length(subset(bf15_r1,logBF<1)$logBF)/3600

bf3_r1_h0 = length(subset(bf3_r1,logBF<1)$logBF)/3600

a = bf5_r1$eat.emp
d<-density(a)
plot(d)
abline(v=0.01,lwd=1,col="black")


#subset(student,Gender=="F" & Age<30 ,select=c("Name","Age"))


