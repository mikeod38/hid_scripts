 CS_resc_food<-read.csv(file.choose(), header=TRUE)
 CS_resc_food$pct<-as.numeric(paste(CS_resc_food$dauer/(CS_resc_food$dauer+CS_resc_food$ar+CS_resc_food$pd+CS_resc_food$non)*100))
 CS_resc_food$genotype<-ordered(CS_resc_food$genotype, levels=c("N2", "mg360", "rict-1p","gpa-4p", "flp-8M1p", "gcy-32p", "flp-21p", "ges-1p"))
 CS_resc_food$food<-ordered(CS_resc_food$food, levels=c("op50", "hb101"))
 par(mar=c(8,5,1,1))
 
 library(ggplot2)

p <- ggplot(CS_resc_food, aes(x=food, y=pct, group=genotype))
 
 p + geom_boxplot(aes(group=food), outlier.shape = NA) + geom_point(aes(colour=food), position = position_jitter(width = .125, height = 0), size=1.25) + facet_grid(.~genotype) + theme(axis.text.x=element_text(angle=45, hjust=1)) + scale_fill_manual(values=c("white", "grey70", "grey70", "white", "grey70", "grey70", "grey70", "grey70", "grey70")) + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())

