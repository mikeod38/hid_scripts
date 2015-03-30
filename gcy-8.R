## gcy8##
gcy8<-read.csv(file.choose(), header=TRUE)
gcy8$pct<-as.numeric(paste(gcy8$dauer/(gcy8$dauer+gcy8$ar+gcy8$pd+gcy8$non)*100))
gcy8$day<-factor(gcy8$day)
 
 par(mar=c(8,10,1,1))
 
 library(ggplot2)
 p<-ggplot(gcy8, aes(x=genotype, y=pct, group=genotype))
 
 p + geom_boxplot(outlier.shape=NA) +
 geom_point(position = position_jitter(width = .1, height = 0)) +
 theme(axis.text.x=element_text(angle=45, hjust=1, size=14)) +
 scale_x_discrete(limits=c("N2", "gcy_triple", "gcy-8")) +
 ylab("% dauer") +
 theme(panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank())+
 ylim(0,100) +
 theme(axis.title.x = element_text(vjust=-0.5, size=14))