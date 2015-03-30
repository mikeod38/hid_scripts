###op50 vs hb101#1
 food1 <-read.csv(file.choose(), header=TRUE)
 food1$pct<-as.numeric(paste(food1$dauer/(food1$dauer+food1$ar+food1$pd+food1$non)))
 food1$genotype<-ordered(food1$genotype, levels=c("N2", "ft7", "mg360"))
 food1$food<-ordered(food1$food, levels=c("op50", "hb101"))
 par(mar=c(8,5,1,1))
 boxplot(pct~food+genotype, data=food1, col="lightgreen", ylim=c(0,1), las=2)
 
 ### for combined
 food2<-read.csv(file.choose(), header=TRUE)
 food2$pct<-as.numeric(paste(food2$dauer/(food2$dauer+food2$ar+food2$pd+food2$non)*100))
 food2$genotype<-ordered(food2$genotype, levels=c("N2", "ft7", "mg360", "sgk-1", "AWC_killed", "ASK_killed"))
 food2$food<-ordered(food2$food, levels=c("op50", "hb101"))
 par(mar=c(8,5,1,1))

### for rict-1 rescue
 food3<-read.csv(file.choose(), header=TRUE)
 food3$pct<-as.numeric(paste(food3$dauer/(food3$dauer+food3$ar+food3$pd+food3$non)*100))
 food3$genotype<-ordered(food3$genotype, levels=c("N2", "mg360", "flp8M1p", "gpa4p", "ASK_killed"))
 food3$food<-ordered(food3$food, levels=c("op50", "hb101"))
 par(mar=c(8,5,1,1))

 #boxplot(pct~food+genotype, data=food2, col=c("grey", "light blue","grey", "light blue","grey", "light blue","grey", "light blue","grey", "light blue","grey", "light blue","grey", "light blue"), ylim=c(0,1), las=2)
 #legend(12,1, c("OP50", "Hb101"), col=c("grey50","sky blue"), lty = 1, pch=20, merge = TRUE, bg = "gray100")
 
library(ggplot2)

p <- ggplot(food3, aes(x=food, y=pct, group=genotype))
 
 p + geom_boxplot(aes(group=food), outlier.shape = NA) + geom_point(aes(colour = food), position = position_jitter(width = .1, height = 0)) + facet_grid(. ~ genotype) + theme(axis.text.x=element_text(angle=45, hjust=1)) + scale_colour_manual(values=c("red3", "royalblue4", "royalblue1", "springgreen3", "magenta", "black")) +  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())
