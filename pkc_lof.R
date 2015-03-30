## pkc##
pkc<-read.csv(file.choose(), header=TRUE)
pkc$pct<-as.numeric(paste(pkc$dauer/(pkc$dauer+pkc$ar+pkc$pd+pkc$non)))
pkc$day<-factor(pkc$day)
 
 par(mar=c(8,10,1,1))
 
 library(ggplot2)
 p<-ggplot(pkc, aes(x=genotype, y=pct, group=genotype))
 
 p + geom_boxplot(outlier.shape=NA) +
 geom_point(position = position_jitter(width = .1, height = 0)) +
 theme(axis.text.x=element_text(angle=45, hjust=1, size=14)) +
 scale_x_discrete(limits=c("N2", "pkc-1", "pkc-2")) +
 ylab("% dauer") +
 theme(panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank())+
 ylim(0,100) +
 theme(axis.title.x = element_text(vjust=-0.5, size=14))
 
  par(mar=c(8,5,1,1))
 boxplot(pct~genotype, data=pkc, col="lightgreen", ylim=c(0,1), las=2)