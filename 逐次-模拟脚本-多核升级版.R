
# register CPU cores for parallel processing
library(doParallel)
library(foreach)
cl <- makeCluster(6)
registerDoParallel(cl)
getDoParWorkers()


minN <- 5				# 最小样本数
maxN <- 10000			# 最大样本数
B = 3600
#B <- 50016				# bootstrap样本数，应该被核心数整除 number of bootstrap samples (should be dividable by getDoParWorkers())
maxBoundary <- log(30)	# 最大边界的停止值，取对数
boundaries <- seq(3, 30, by=1)	# 所有的边界3-30，1为间隔
etas = 0.01
#etas <- seq(0.5, 0.05, by=-.05)    #效应量大小
#etas <- seq(0, 0.5, by=.05)    #效应量大小
rs <- c(sqrt(2)/2, 1, sqrt(2))  #柯西分布的参数

c = getDoParWorkers()
#生成指定效果了大小的母集团
get_population <- function(n, eta2, SD=1) {
  cohen_f <- sqrt(eta2/(1-eta2))
  
  x <- rnorm(n, 0, SD) + cohen_f
  y <- rnorm(n, 0, SD) - cohen_f
  return(cbind(x, y))
}
#计算各种效应量的函数
eta2 <- function(y){
  SSa <- y[[1]]$"Sum Sq"[1]
  SSe <- y[[1]]$"Sum Sq"[2]
  SSt <- SSa + SSe
  eta <- SSa / SSt
  return(eta)
}
omega2 <- function(y){
  MSa <- y[[1]]$"Mean Sq"[1] 
  MSe <- y[[1]]$"Mean Sq"[2]
  dfa <- y[[1]]$Df[1] 
  SSa <- y[[1]]$"Sum Sq"[1]
  SSe <- y[[1]]$"Sum Sq"[2]
  SSt <- SSa + SSe
  omega2 <- dfa * (MSa - MSe) / (SSt + MSe)
  return(omega2)
}
epsilon2 <- function(y){
  MSa <- y[[1]]$"Mean Sq"[1] 
  MSe <- y[[1]]$"Mean Sq"[2]
  dfa <- y[[1]]$Df[1]
  SSa <- y[[1]]$"Sum Sq"[1]
  SSe <- y[[1]]$"Sum Sq"[2]
  SSt <- SSa + SSe
  epsilon2 <- dfa * (MSa - MSe) / SSt 
  return(epsilon2)
}


ns <- c(minN:149, seq(150, 995, by=5), seq(1000, 2490, by=10), seq(2500, 5000, by=20),seq(5050, maxN, by=50))	

print(start <- Sys.time())




foreach(batch=1:getDoParWorkers(),
        .combine=function(...) {}) %dopar%  {
  
  library(BayesFactor)
  library(dplyr)
  library(data.table)
  library(coda)
  
  max_b <- round(B/c)
  for (eta in etas) {
    
    # rep是小b final saves the data at all stopping points
    final <- matrix(NA, nrow=length(boundaries)*length(rs)*max_b, ncol=12, dimnames=list(NULL, c("boundary", "eta", "r", "batch", "rep", "n", "logBF", "eat.emp", " omega.emp", "epsilon.emp","f.value", "p.value")))
    final.counter <- 1
    #获得模样本，1百万，
    pop <- get_population(1000000, eta)
    print(paste0(Sys.time(), ": batch = ", batch, "; eta = ", eta))
    
    for (b in 1:max_b) {
      
      # 在每1步保存统计信息  res saves the statistics at each step
      res <- matrix(NA, nrow=length(ns)*length(rs), ncol=11, dimnames=list(NULL, c("eta", "r", "batch", "rep", "n", "logBF", "eta.emp", " omega.emp", "epsilon.emp","f.value", "p.value")))
      res.counter <- 1
      print(paste0(Sys.time(), ": batch = ", batch, "; eta = ",eta , "; Rep = ", b, "/", round(B/c)))
      
      # 生成样本
      maxsamp <- pop[sample(nrow(pop), maxN), ]
      
      for (r in rs) {				
        
        # res0 keeps the accumulating sample variables from this specific run
        res0 <- matrix(NA, nrow=length(ns), ncol=ncol(res), dimnames=dimnames(res))
        
        # increase sample size until the final boundary is hit
        finalhit <- FALSE	# tracks whether a trajectory does not hit a boundary beforemax.n is reached
        
        for (n in ns) {
          # 生成样本
          samp <- maxsamp[1:n, ]
          N <- nrow(samp)
          samp = data.frame(y = c(samp),x = factor(rep(1:2,each=n)))
          
          # 计算分散分析和BF
          f0 <- summary(aov(y~x,data = samp))
          f.v = c(f0[[1]]$`F value`[1])
          BF0 = anovaBF(y~x,data = samp,rscaleEffects = r,progress=F)@bayesFactor$bf
          
          #BF0 = oneWayAOV.Fstat(f.v, n, 2, rscale = r, simple = TRUE)
          
          res0[which(ns == n), ]<- c(
            eta	= eta, 
            r 	= r, 
            batch = batch, 
            rep	= b, 
            n	= N, 
            BF	= BF0, 
            eta.emp = eta2(f0),
            omega.emp = omega2(f0),
            epsilon.emp = epsilon2(f0),
            f.value	= f.v, 
            p.value	= f0[[1]]$`Pr(>F)`[1])
          
          # When last boundary is hit: compute posteriors, stopping-n's. etc.
          if (abs(BF0)>maxBoundary) {
            
            finalhit <- TRUE
            #################
            #print(paste0(Sys.time(), ": batch=", batch, "; eta=",eta, " r=", r, "; Rep=", b, "/", round(B/getDoParWorkers()), "--- final: n=", n, "; BF=", round(BF0, 2)))
            
            res0 <- res0[1:which(ns == n),, drop=FALSE]
            final0 <- matrix(NA, nrow=length(boundaries), ncol=ncol(final))
            
            # 遍历边界 loop through all boundaries
            old_hit_row <- 0
            for (l in 1:length(boundaries)) {
              # find the n where the boundary was exceeded
              hit_row <- which(abs(res0[, "logBF"]) > log(boundaries[l]))[1]
              
              if (hit_row == old_hit_row) {
                # short-cut: it's the same result as before
                final0[l, ] <- final0[l-1, ]
                final0[l, 1] <- boundaries[l]
                next;
              }
              old_hit_row <- hit_row
              
              if (!is.na(hit_row)) {
                
                final0[l, 1] <- boundaries[l]
                final0[l, 2:12]  <- res0[hit_row, ]
              }
            }
            
            break;
          }
        } 	# of n
        
        if (finalhit == TRUE) {
          # Add this single accumulation to the overall results matrix
          res[res.counter:(res.counter+nrow(res0)-1), ] <- res0
          res.counter <- res.counter + nrow(res0)
          
          final[final.counter:(final.counter+nrow(final0)-1), ] <- final0
          final.counter <- final.counter + nrow(final0)						
        } else {
          print(paste0("Skipping replication ", b, " at r = ", r, " because it did not reach a boundary!"))
        }
      } # of r's
      
      
      res <- res[1:res.counter-1, ]				
      
      res <- round(res, 5)
      if (b==1) {
        write.table(res, file=paste0("raw/eta_", eta, "_batch_", batch, ".tab"), append = FALSE, row.names = FALSE, col.names = TRUE)
      } else {
        write.table(res, file=paste0("raw/eta_", eta, "_batch_", batch, ".tab"), append = TRUE, row.names = FALSE, col.names = FALSE)
      }
      gc(reset=TRUE)
    }  # of b's		
    
    # convert file to .RData file
    # fast reading of large tables: fread
    f <- paste0("raw/eta_", eta, "_batch_", batch, ".tab")
    print(paste(Sys.time(), ": Converting", f, "..."))
    res <- fread(f)
    save(res, file=gsub(".tab", ".RData", f, fixed=TRUE))
    rm(res)
    
    final <- final[1:final.counter-1, ]	
    final <- mutate(as.data.frame(final))
    save(final, file=paste0("raw/final_eta_", eta, "_batch_", batch, ".RData"))
    
    print(paste0("finished eta=", eta, "; batch=", batch))
    
  }
}


end <- Sys.time()
print(Sys.time())
print(end - start)
