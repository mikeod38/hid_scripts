## rict-1 NK rescue#
NK_resc<-read.csv(file.choose(), header=TRUE)
NK_resc$pct<-as.numeric(paste(NK_resc$dauer/(NK_resc$dauer+NK_resc$ar+NK_resc$pd+NK_resc$non)))
NK_resc$genotype<-factor(NK_resc$genotype, levels=c("N2", "NIL59", "mg360", "mg360; ex[N2]", "mg360; ex[HW]", "mg360; ex[odr-4p::N2]"))

NK_resc$dose<-interaction(NK_resc$genotype, NK_resc$TG_dose)
 
 par(mar=c(8,5,1,1))
 
 library(ggplot2)
 p<-ggplot(NK_resc, aes(x=dose, y=pct))
 
 p + geom_boxplot() +
 geom_point(aes(colour = genotype), position = position_jitter(width = .1, height = 0)) + theme_bw()+
 theme(axis.text.x=element_text(angle=45, hjust=1, size=12))
 scale_x_discrete(limits=c("N2", "NIL59", "mg360", "mg360; ex[N2]", "mg360; ex[HW]", "mg360; ex[odr-4p::N2]")) +
 ylab("% dauer") +
 theme(panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank())

  par(mar=c(8,5,1,1))
 boxplot(pct~dose, data=NK_resc, col="lightgreen", ylim=c(0,1), las=2)
 
### for NK rescue without dose ####  

NK_resc_simple<-read.csv(file.choose(), header=TRUE)
NK_resc_simple$pct<-as.numeric(paste(NK_resc$dauer/(NK_resc$dauer+NK_resc$ar+NK_resc$pd+NK_resc$non)))
NK_resc_simple$genotype<-factor(NK_resc_simple$genotype, levels=c("N2", "NIL59", "mg360", "mg360; ex[N2]-1", "mg360; ex[HW]-1","mg360; ex[N2]-5", "mg360; ex[HW]-5", "mg360; ex[odr-4p::N2]"))
 
 par(mar=c(8,5,1,1))
 
 library(ggplot2)
 p<-ggplot(NK_resc_simple, aes(x=genotype, y=pct))
 
 p + geom_boxplot() + theme_bw()+
 theme(axis.text.x=element_text(angle=45, hjust=1, size=12))
 scale_x_discrete(limits=c("N2", "NIL59", "mg360", "mg360; ex[N2]-1", "mg360; ex[HW]-1","mg360; ex[N2]-5", "mg360; ex[HW]-5", "mg360; ex[odr-4p::N2]")) +
 ylab("% dauer") +
 theme(panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank())