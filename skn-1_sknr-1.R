 skn1<-read.csv(file.choose(), header=TRUE)
 skn1$pct<-as.numeric(paste(skn1$dauer/(skn1$dauer+skn1$ar+skn1$pd+skn1$non)*100))
 skn1$genotype<-ordered(skn1$genotype, levels=c("N2", "tm4241", "ok1216", "tm2386", "mg360", "tm2386;mg360", "gpa-4::skn-1b"))
 par(mar=c(8,5,1,1))
 
 library(ggplot2)

p <- ggplot(skn1, aes(x=genotype, y=pct))
 
 p + geom_boxplot(aes(group=genotype), outlier.shape = NA) + geom_point(, position = position_jitter(width = .1, height = 0)) + theme_bw() + theme(axis.text.x=element_text(angle=45, hjust=1)) + scale_colour_manual(values=c("red3", "royalblue4", "royalblue1", "springgreen3", "magenta", "black")) + 
 theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())

