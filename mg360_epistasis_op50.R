 epi<-read.csv(file.choose(), header=TRUE)
 epi$pct<-as.numeric(paste(epi$dauer/(epi$dauer+epi$ar+epi$pd+epi$non)*100))
 epi$genotype<-ordered(epi$genotype, levels=c("N2", "ft15", "pkc-2", "n476", "mg360", "mu86;mg360", "ft15;mg360", "pkc-2;mg360", "n476;mg360"))
 par(mar=c(8,5,1,1))
 
 library(ggplot2)

p <- ggplot(epi, aes(x=genotype, y=pct))
 
 p + geom_boxplot(aes(fill=genotype), outlier.shape = NA) + geom_point(position = position_jitter(width = .125, height = 0), size=1.25) + theme_bw() + theme(axis.text.x=element_text(angle=45, hjust=1)) + scale_fill_manual(values=c("white", "grey70", "grey70", "white", "grey70", "grey70", "grey70", "grey70", "grey70")) + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())

