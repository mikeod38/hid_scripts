 gcy35<-read.csv(file.choose(), header=TRUE)
 gcy35$pct<-as.numeric(paste(gcy35$dauer/(gcy35$dauer+gcy35$ar+gcy35$pd+gcy35$non)*100))
 gcy35$genotype<-ordered(gcy35$genotype, levels=c("N2", "mg360", "rict-1p","gpa-4p", "flp-8M1p", "flp-21p", "gcy-32p", "ges-1p", "odr-4p"))
 gcy35$food<-ordered(gcy35$food, levels=c("op50", "hb101"))
 par(mar=c(8,5,1,1))
 
 library(ggplot2)

p <- ggplot(gcy35, aes(x=food, y=pct, fill=food))
 
p + geom_boxplot(aes(group=food), outlier.shape = NA) + geom_point(aes(colour=food), position = position_jitter(width = .125, height = 0), size=1.25) + facet_grid(.~genotype) + theme(axis.text.x=element_text(angle=45, hjust=1)) + scale_fill_manual(values=c("white", "grey70", "grey70", "white", "grey70", "grey70", "grey70", "grey70", "grey70")) + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())
 
p + geom_boxplot(aes(group=food)) + facet_grid(.~genotype) +
   ylab("pct dauer") + 
   xlab("genotype") +
   theme(axis.text.x=element_blank()) + 
   theme(axis.text.y=element_text(size=10)) + 
   scale_fill_manual(values=c("white", "grey70")) + 
   theme(axis.ticks.x=element_blank()) +
   theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())

