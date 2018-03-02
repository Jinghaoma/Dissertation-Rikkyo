library(BayesFactor)
library(dplyr)
library(data.table)
library(coda)

# register CPU cores for parallel processing
library(doParallel)
library(foreach)
cl <- makeCluster(6)
registerDoParallel(cl)
getDoParWorkers()

#library(doMC)
#registerDoMC(16)
#getDoParWorkers()


## ======================================================================
## SETTINGS
## ======================================================================

minN <- 10				# 最小样本数
maxN <- 45000			# 最大样本数
iterations <- 5000		# iterations for posterior draws（迭代后抽签）
B <- 50016				# bootstrap样本数，应该被核心数整除 number of bootstrap samples (should be dividable by getDoParWorkers())
maxBoundary <- log(30)	# 最大边界的停止值，取对数
boundaries <- seq(3, 30, by=1)	# 所有的边界3-30，1为间隔
ds <- seq(1.5, 0, by=-.1)    #效应量大小
rs <- c(sqrt(2)/2, 1, sqrt(2))  #柯西分布的参数


## ======================================================================
## THE SIMULATION
## ======================================================================

#生成指定效果了大小的母集团
get_population <- function(n, d, SD=1) {
	x <- rnorm(n, 0, SD)
	y <- rnorm(n, 0, SD) - d
	return(cbind(x, y))
}

# 计算模拟样本的大小，增大步长，最小到99为止是1，100到995是5 compute sample sizes that are simulated: increasing step size for large n's
ns <- c(minN:99, seq(100, 995, by=5), seq(1000, 2490, by=10), seq(2500, 5000, by=20),seq(5050, maxN, by=50))	

print(start <- Sys.time())


# 不合并结果，每个样本量保存到分别的文件中 do not combine the results, each fork saves the result to a file.
sim <- foreach(batch=1:getDoParWorkers(), .combine=function(...) {}) %do% {    
	
	max_b <- round(B/getDoParWorkers())
	# 每个效应量循环
	for (d in ds) {
		
		# final saves the data at all stopping points
		final <- matrix(NA, nrow=length(boundaries)*length(rs)*max_b, ncol=14, dimnames=list(NULL, c("boundary", "d", "r", "batch", "rep", "n", "logBF", "d.emp", "t.value", "p.value", "HPD.mean", "HPD.median", "HPD.lower", "HPD.upper")))
		final.counter <- 1
    #获得模样本，1百万，
		pop <- get_population(1000000, d)		
		print(paste0(Sys.time(), ": batch = ", batch, "; d = ", d))
		
		for (b in 1:max_b) {
			
			# 在每1步保存统计信息  res saves the statistics at each step
			res <- matrix(NA, nrow=length(ns)*length(rs), ncol=9, dimnames=list(NULL, c("d", "r", "batch", "rep", "n", "logBF", "d.emp", "t.value", "p.value")))
			res.counter <- 1
			print(paste0(Sys.time(), ": batch = ", batch, "; d = ",d, "; Rep = ", b, "/", round(B/getDoParWorkers())))
			
			#
			maxsamp <- pop[sample(nrow(pop), maxN), ]
			
			for (r in rs) {				
				
				# res0 keeps the accumulating sample variables from this specific run
				res0 <- matrix(NA, nrow=length(ns), ncol=ncol(res), dimnames=dimnames(res))
				
				# increase sample size until the final boundary is hit
				finalhit <- FALSE	# tracks whether a trajectory does not hit a boundary beforemax.n is reached
				for (n in ns) {
					samp <- maxsamp[1:n, ]
					N <- nrow(samp)
					t0 <- t.test(samp[, 1], samp[, 2])
				
					BF0 <- ttest.tstat(t0$statistic, N, N, rscale=r)$bf
					res0[which(ns == n), ]<- c(
						d	= d, 
						r 	= r, 
						batch = batch, 
						rep	= b, 
						n	= N, 
						BF	= BF0, 
						d.emp	= 2*t0$statistic / sqrt(2*N-2),
						t.value	= t0$statistic, 
						p.value	= t0$p.value)
					
					# When last boundary is hit: compute posteriors, stopping-n's. etc.
					if (abs(BF0)>maxBoundary) {
						
						finalhit <- TRUE
						print(paste0(Sys.time(), ": batch=", batch, "; d=",d, " r=", r, "; Rep=", b, "/", round(B/getDoParWorkers()), "--- final: n=", n, "; BF=", round(BF0, 2)))
						
						res0 <- res0[1:which(ns == n),, drop=FALSE]
						final0 <- matrix(NA, nrow=length(boundaries), ncol=ncol(final))

						# loop through all boundaries
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
								
								# compute posterior
								post <- ttestBF(samp[1:res0[hit_row, "n"], 1], samp[1:res0[hit_row, "n"], 2], rscale=r, posterior=TRUE, iterations = iterations, progress=FALSE)		
								HPD <- HPDinterval(post)
								
								final0[l, 1] <- boundaries[l]
								final0[l, 2:10]  <- res0[hit_row, ]
								final0[l, 11] <- mean(post[, "delta"])
								final0[l, 12] <- median(post[, "delta"])
								final0[l, 13] <- HPD["delta", "lower"]
								final0[l, 14] <- HPD["delta", "upper"]
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
				write.table(res, file=paste0("raw/d_", d, "_batch_", batch, ".tab"), append = FALSE, row.names = FALSE, col.names = TRUE)
			} else {
				write.table(res, file=paste0("raw/d_", d, "_batch_", batch, ".tab"), append = TRUE, row.names = FALSE, col.names = FALSE)
			}
			gc(reset=TRUE)
		}  # of b's		
		
		# convert file to .RData file
		# fast reading of large tables: fread
		f <- paste0("raw/d_", d, "_batch_", batch, ".tab")
		print(paste(Sys.time(), ": Converting", f, "..."))
		res <- fread(f)
		save(res, file=gsub(".tab", ".RData", f, fixed=TRUE))
		rm(res)
		
		final <- final[1:final.counter-1, ]	
		final <- mutate(as.data.frame(final), g.emp=d.emp * (1 - (3 / (4*n*2 - 9))))
		save(final, file=paste0("raw/final_d_", d, "_batch_", batch, ".RData"))
				
		print(paste0("finished d=", d, "; batch=", batch))
		
	} # of d's
}

end <- Sys.time()
print(Sys.time())
print(end - start)
