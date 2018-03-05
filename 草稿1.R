######
a = 0
sim <- foreach(batch=1:getDoParWorkers(), .combine=function(...) {}) %do% {
  a = a+1
  print(batch)
}

######-------------------------------------------

for (r in rs) {				
  
  # res0 keeps the accumulating sample variables from this specific run
  res0 <- matrix(NA, nrow=length(ns), ncol=ncol(res), dimnames=dimnames(res))
  
  # increase sample size until the final boundary is hit
  finalhit <- FALSE	# tracks whether a trajectory does not hit a boundary beforemax.n is reached
  
  for (n in ns) {
    # 生成样本
    samp <- maxsamp[1:n, ]
    N <- nrow(samp)
    samp = data.frame(c(samp),factor(rep(1:2,each=n)))
    names(samp) = c("y","x")
    
    # 计算分散分析和BF
    f0 <- summary(aov(y~x,data = samp))
    f.v = c(f0[[1]]$`F value`[1])
    BF0 = oneWayAOV.Fstat(f.v, n, 2, rscale = r, simple = TRUE)
    
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
      print(paste0(Sys.time(), ": batch=", batch, "; eta=",eta, " r=", r, "; Rep=", b, "/", round(B/getDoParWorkers()), "--- final: n=", n, "; BF=", round(BF0, 2)))
      
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
