## daf-5##
daf5<-read.csv(file.choose(), header=TRUE)
daf5$pct<-as.numeric(paste(daf5$dauer/(daf5$dauer+daf5$ar+daf5$pd+daf5$non)*100))
daf5$genotype<-factor(daf5$genotype, levels=c("N2", "daf-5"))
daf5$day<-factor(daf5$day)
 
 par(mar=c(8,5,1,1))
 
 library(ggplot2)
 p<-ggplot(daf5, aes(x=genotype, y=pct, group=genotype))
 
 p + geom_boxplot(outlier.shape = NA) +
 geom_point(aes(colour = day), position = position_jitter(width = .1, height = .05)) +
 theme(axis.text.x=element_text(angle=45, hjust=1, size=12)) +
 theme(panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank())
