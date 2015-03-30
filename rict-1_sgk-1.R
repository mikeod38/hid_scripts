sgk_1<-read.csv(file.choose(), header=TRUE)
sgk_1$pct<-as.numeric(paste(sgk_1$dauer/(sgk_1$dauer+sgk_1$ar+sgk_1$pd+sgk_1$non)))
sgk_1$genotype<-factor(sgk_1$genotype, levels=c("N2", "mg360", "ft7", "sgk-1", "sgk-1 (ft15)"))
 
 par(mar=c(8,5,1,1))
 
 library(ggplot2)
 p<-ggplot(sgk_1, aes(x=genotype, y=pct))
 
 p + geom_boxplot() +
 theme(axis.text.x=element_text(angle=45, hjust=1, size=12)) + geom_point(aes(colour = genotype), position = position_jitter(width = .1, height = 0)) +
 scale_x_discrete(limits=c("N2", "mg360", "ft7", "sgk-1", "sgk-1 (ft15)")) + ylab("% dauer") +
 theme(panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank()) + ylim(c(0,1))