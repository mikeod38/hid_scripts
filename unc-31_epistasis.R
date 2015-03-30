## unc-31 epistasis##
unc_epi<-read.csv(file.choose(), header=TRUE)
unc_epi$pct<-as.numeric(paste(unc_epi$dauer/(unc_epi$dauer+unc_epi$ar+unc_epi$pd+unc_epi$non)))
unc_epi$genotype<-factor(unc_epi$genotype, levels=c("N2", "NIL59", "egl-21", "unc-31", "NIL59;unc-31", "egl-21;unc-31"))
 
 par(mar=c(8,5,1,1))
 
 library(ggplot2)
 p<-ggplot(unc_epi, aes(x=genotype, y=pct, group=genotype))
 
 p + geom_boxplot() +
 geom_point(aes(colour = genotype), position = position_jitter(width = .1, height = 0)) +
 theme(axis.text.x=element_text(angle=45, hjust=1, size=12)) +
 scale_x_discrete(limits=c("N2", "NIL59", "egl-21", "unc-31", "NIL59;unc-31", "egl-21;unc-31")) +
 ylab("% dauer") +
 theme(panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank())

  par(mar=c(8,5,1,1))
 boxplot(pct~genotype, data=unc_epi, col="lightgreen", ylim=c(0,1), las=2)