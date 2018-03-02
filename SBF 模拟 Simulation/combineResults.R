# source("combineResults.R", verbose=TRUE)
library(dplyr)
library(data.table)

print(Sys.time())
li <- list.files("raw_final", pattern="final_.*\\.RData", full.names=TRUE)

finals <- data.frame()
for (f in rev(li)) {
	print(paste0(Sys.time(), ": ", f, ": ", which(li == f), "/", length(li)))
	load(f)
	finals <- bind_rows(finals, final)		
	print(nrow(finals))
}

final <- finals
rm(finals)
gc(reset=TRUE)
str(final)

final <- final %>% select(-batch, -rep)

final <- mutate(final, 
	s 		= sign(logBF),
	wrong 	= (s==1 & d==0) | (s==-1 & d > 0),
	hit 	= factor(s, labels=c("H0_hit", "H1_hit"))
)

final$boundary <- factor(final$boundary)

save(final, file="final.RData")


# show summary of runs - did all runs finish?

final %>% group_by(d) %>% dplyr::summarise(n=n())

final %>% group_by(d, r) %>% dplyr::summarise(n=n())

final %>% group_by(d, r) %>% dplyr::summarise(n=n()) %>% ungroup() %>% select(n) %>% table(.)

print(Sys.time())