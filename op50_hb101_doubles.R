 doubles<-read.csv(file.choose(), header=TRUE)
 doubles$pct<-as.numeric(paste(doubles$dauer/(doubles$dauer+doubles$ar+doubles$pd+doubles$non)*100))
 doubles$genotype<-ordered(doubles$genotype, levels=c("N2", "ft15", "tm2308","daf-5", "tm4241", "mg360", "mg360;mu86", "mg360;ft15", "mg360;n476"))
 doubles$food<-ordered(doubles$food, levels=c("op50", "hb101"))
 par(mar=c(8,5,1,1))
 
 library(ggplot2)

p <- ggplot(doubles, aes(x=food, y=pct, group=genotype))
 
 p + geom_boxplot(aes(group=food), outlier.shape = NA) + geom_point(aes(colour=food), position = position_jitter(width = .125, height = 0), size=1.25) + facet_grid(.~genotype) + theme(axis.text.x=element_text(angle=45, hjust=1)) + scale_fill_manual(values=c("white", "grey70", "grey70", "white", "grey70", "grey70", "grey70", "grey70", "grey70")) + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())

