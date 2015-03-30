 rict_resc<-read.csv(file.choose(), header=TRUE)
 rict_resc$pct<-as.numeric(paste(rict_resc$dauer/(rict_resc$dauer+rict_resc$ar+rict_resc$pd+rict_resc$non)*100))
 rict_resc$genotype<-ordered(rict_resc$genotype, levels=c("N2", "mg360", "rict-1p_N2", "rict-1p_HW", "odr-4p", "flp-8M1p", "gpa-4p", "NIL59"))
 par(mar=c(8,5,1,1))
 
 library(ggplot2)

p <- ggplot(rict_resc, aes(x=genotype, y=pct))
 
 p + geom_boxplot(aes(group=genotype), outlier.shape = NA) + geom_point(, position = position_jitter(width = .1, height = 0)) + theme(axis.text.x=element_text(angle=45, hjust=1)) + scale_colour_manual(values=c("red3", "royalblue4", "royalblue1", "springgreen3", "magenta", "black")) +  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())

