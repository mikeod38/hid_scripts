 unc64resc <-read.csv(file.choose(), header=TRUE)
 unc64resc$pctdauer<-(unc64resc$dauer/unc64resc$n)
 unc64resc$genotype<-ordered(unc64resc$genotype, levels=c("N2", "unc-64", "tax2p::unc-64"))
 boxplot(pctdauer~genotype*temp, data=unc64resc, col="lightgreen")
 
 