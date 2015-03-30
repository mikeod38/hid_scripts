 egl_lof<-read.csv(file.choose(), header=TRUE)
 egl_lof$pct<-as.numeric(paste(egl_lof$dauer/(egl_lof$dauer+egl_lof$ar+egl_lof$pd+egl_lof$non)*100))
 egl_lof$pct_arrest<-as.numeric(paste((egl_lof$dauer+egl_lof$pd)/(egl_lof$dauer+egl_lof$ar+egl_lof$pd+egl_lof$non)*100))
 egl_lof$genotype<-factor(egl_lof$genotype, levels=c("N2", "egl-3", "egl-21"))
 #egl_lof$allele<-ordered(egl_lof$allele, levels=c("","mg360", "mg360;ft15", "mg360;ok238"))
 egl_lof$food<-ordered(egl_lof$food, levels=c("op50", "hb101"))
 par(mar=c(8,5,1,1))
 
 library(ggplot2)

p1 <- ggplot(egl_lof, aes(x=genotype, y=pct_arrest, fill=food))
 
p1 + geom_boxplot(aes(group=food)) +
   ylab("pct arrest") + 
   xlab("genotype") +
   theme(axis.text.x=element_blank()) + 
   theme(axis.text.y=element_text(size=10)) + 
   scale_fill_manual(values=c("white", "grey70")) + 
   theme(axis.ticks.x=element_blank()) +
   theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank())

 
 p2 <- ggplot(egl_lof, aes(x=genotype, y=pct, fill=food))
 
 p2 + geom_boxplot() +
   ylab("pct dauer") + 
   xlab("genotype") + 
   theme(axis.text.x=element_text(angle=45, hjust=1, size=15)) +
   theme(axis.text.y=element_text(size=10)) + 
   scale_fill_manual(values=c("white", "grey70")) + 
   theme(axis.ticks.x=element_blank()) +
   theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),panel.grid.major.x=element_blank()) + 
   coord_cartesian(ylim=(0,100))
 
lm1<-lm(pct~genotype, data=egl_lof)
summary(lm1)
glm1<-glm(cbind(dauer,non)~genotype, data=egl_lof, family=binomial(link="logit"))
summary(glm1)
#library(pscl)
#glm2<-zeroinfl(c(dauer,non)~genotype, data=egl_lof, family=binomial(link="logit"))
