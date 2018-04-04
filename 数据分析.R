#毕业论文分析数据

make_data = function(){
  li <- list.files("raw", pattern="final_.*\\.RData", full.names=TRUE)
  #收录母数据，10个应量，R3个，横轴为边界
  data.table.par = matrix(NA,nrow = 0,ncol = 29, dimnames=list(NULL, c("r",3:30)))
  data.table.n = matrix(NA,nrow = 0,ncol = 29, dimnames=list(NULL, c("r",3:30)))
  
  for (name in li) {
    if (substr(name,15,19)=="0.000") {
      load(name)
      
      name_eta = paste0("eta2 = ",substr(name,15,19))
      #每个效应量的仓库
      dat.boun.3 = matrix(NA,nrow = 3,ncol = 29, dimnames=list(NULL, c("r",3:30)))
      rownames(dat.boun.3) <- c(name_eta,name_eta,name_eta)
      dat.boun.3[,1] = c("r=√2/2", "r=1", "r=√2") 
      #样本量的仓库
      dat.n.3 = matrix(NA,nrow = 3,ncol = 29, dimnames=list(NULL, c("r",3:30)))
      rownames(dat.n.3) <- c(name_eta,name_eta,name_eta)
      dat.n.3[,1] = c("r=√2/2", "r=1", "r=√2") 
      
      #最小边界到最大边界循环
      for (boun in min(finals$boundary):max(finals$boundary)) {
        #每个边界，
        dat.boun = c()
        dat.n = c()
        r.roop = c(finals$r[1],finals$r[31],finals$r[61])
        for (ri in r.roop) {
          dat = subset(finals,boundary==boun & r==ri)
          erro.par = nrow(subset(dat,logBF>0))/nrow(dat)
          
          dat.boun = c(dat.boun,erro.par)
          dat.n = c(dat.n,mean(dat$n))
        }
        dat.boun.3[,boun-1] = dat.boun
        dat.n.3[,boun-1] = dat.n
        #print(boun)
      }
      data.table.par = rbind(data.table.par,dat.boun.3)
      data.table.n = rbind(data.table.n,dat.n.3)
      
    }
    else{
      load(name)
      
      name_eta = paste0("eta2 = ",substr(name,15,19))
      
      #每个效应量的仓库
      dat.boun.3 = matrix(NA,nrow = 3,ncol = 29, dimnames=list(NULL, c("r",3:30)))
      rownames(dat.boun.3) <- c(name_eta,name_eta,name_eta)
      dat.boun.3[,1] = c("r=√2/2", "r=1", "r=√2") 
      #样本量的仓库
      dat.n.3 = matrix(NA,nrow = 3,ncol = 29, dimnames=list(NULL, c("r",3:30)))
      rownames(dat.n.3) <- c(name_eta,name_eta,name_eta)
      dat.n.3[,1] = c("r=√2/2", "r=1", "r=√2") 
      
      #最小边界到最大边界循环
      for (boun in min(finals$boundary):max(finals$boundary)) {
        #每个边界，
        dat.boun = c()
        dat.n = c()
        r.roop = c(finals$r[1],finals$r[31],finals$r[61])
        for (ri in r.roop) {
          dat = subset(finals,boundary==boun & r==ri)
          erro.par = nrow(subset(dat,logBF<0))/nrow(dat)
          
          dat.boun = c(dat.boun,erro.par)
          dat.n = c(dat.n,mean(dat$n))
        }
        dat.boun.3[,boun-1] = dat.boun
        dat.n.3[,boun-1] = dat.n
        #print(boun)
      }
      data.table.par = rbind(data.table.par,dat.boun.3)
      data.table.n = rbind(data.table.n,dat.n.3)
    }
    print(name)
  }
  
  write.csv(data.table.par,file = "data_table/erro_par.csv",fileEncoding = "utf-8")
  write.csv(data.table.n,file = "data_table/n_mean.csv",fileEncoding = "utf-8")
  
  save(data.table.par,file = "data_table/erro_par.RData")
  save(data.table.n,file = "data_table/n_mean.RData")
  
}


data = read.csv("data_table/erro_par.csv",fileEncoding = "utf-8")
dat = data.frame()
for (m in 1:3) {
  i = m
  while (i<=30) {
    dat = rbind(dat,data[i,])
    i = i+3
  }
}
write.csv(dat,file = "data_table/erro_par1.csv",fileEncoding = "utf-8")

data = read.csv("data_table/n_mean.csv",fileEncoding = "utf-8")
dat = data.frame()
for (m in 1:3) {
  i = m
  while (i<=30) {
    dat = rbind(dat,data[i,])
    i = i+3
  }
}
write.csv(dat,file = "data_table/n_mean1.csv",fileEncoding = "utf-8")
