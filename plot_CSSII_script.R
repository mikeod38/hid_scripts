 ### C3 response at 25
 CSSII_C3 <-read.csv(file.choose(), header=TRUE)
 CSSII_C3$pctdauer<-(CSSII_C3$dauer/CSSII_C3$n)
 CSSII_C3$genotype<-ordered(CSSII_C3$genotype, levels=c("N2", "CSSII"))
 boxplot(pctdauer~genotype*treat, data=CSSII_C3, col="lightgreen")
 
 #### CSS map
 CSSII <-read.csv(file.choose(), header=TRUE)
 CSSII$pctdauer<-(CSSII_C3$dauer/CSSII_C3$n)
 CSSII$genotype<-ordered(CSSII_C3$genotype, levels=c("N2", "CSSII"))
 boxplot(pctdauer~genotype*treat, data=CSSII_C3, col="lightgreen")