 egl_supp<-read.csv(file.choose(), header=TRUE)
 egl_supp$pct<-as.numeric(paste(egl_supp$dauer/(egl_supp$dauer+egl_supp$ar+egl_supp$pd+egl_supp$non)*100))
 egl_supp$genotype<-factor(egl_supp$genotype, levels=c("N2", "rict-1", "rict-1;egl-21"))
 egl_supp$food<-ordered(egl_supp$food, levels=c("op50", "hb101"))
 par(mar=c(8,5,1,1))
 
 library(ggplot2)

p <- ggplot(egl_supp, aes(x=food, y=pct, fill=food))
 
p + geom_boxplot(aes(group=food), outlier.shape = NA) + geom_point(aes(colour=food), position = position_jitter(width = .125, height = 0), size=1.25) + facet_grid(.~genotype) + theme(axis.text.x=element_text(angle=45, hjust=1)) + scale_fill_manual(values=c("white", "grey70", "grey70", "white", "grey70", "grey70", "grey70", "grey70", "grey70")) + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())
 
p + geom_boxplot(aes(group=food)) + facet_grid(.~genotype) +
   ylab("pct dauer") + 
   xlab("genotype") +
   theme(axis.text.x=element_blank()) + 
   theme(axis.text.y=element_text(size=10)) + 
   scale_fill_manual(values=c("white", "grey70")) + 
   theme(axis.ticks.x=element_blank()) +
   theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())


