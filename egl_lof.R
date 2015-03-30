## egl-3, egl-21##
egl<-read.csv(file.choose(), header=TRUE)
egl$pct<-as.numeric(paste(egl$dauer/(egl$dauer+egl$ar+egl$pd+egl$non)))
egl$genotype<-factor(egl$genotype, levels=c("N2", "egl-3", "egl-21"))
 
 par(mar=c(8,5,1,1))
 
 library(ggplot2)
 p<-ggplot(egl, aes(x=genotype, y=pct, group=genotype))
 
 p + geom_boxplot() +
 geom_point(aes(colour = genotype), position = position_jitter(width = .1, height = 0)) +
 theme(axis.text.x=element_text(angle=45, hjust=1)) +
 theme(panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank())

par(mar=c(8,5,1,1))
 boxplot(pct~genotype, data=egl, col="lightgreen", ylim=c(0,1), las=2)