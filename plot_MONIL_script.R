 MONIL <-read.csv(file.choose(), header=TRUE)
 MONIL$pct<-as.numeric(paste(MONIL$dauer/(MONIL$dauer+MONIL$ar+MONIL$pd+MONIL$non)))
 MONIL$genotype<-factor(MONIL$genotype, levels=c("N2", "NIL19", "NIL59", "NIL76", "NIL78"))
 boxplot(pct~genotype, data=MONIL, col=c("lightgreen", "magenta", "magenta", "lightgreen", "lightgreen"), ylim=c(0,0.7))
 
