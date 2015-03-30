 N2wild <-read.csv(file.choose(), header=TRUE)
 N2wild$pctdauer<-(N2wild$dauer/N2wild$n)
 N2wild$genotype<-ordered(N2wild$genotype, levels=c("N2", "HW","JU561", "MY14",  "CB4852","DL238","QX1211","JU322", "JU1400", "JU775", "AB1","AB3","ED3072",  "JU362","ED3040", "JU345", "PX178",  "CB3198"))
 boxplot(pctdauer~genotype, data=N2wild, col="lightgreen", las=2)
 
 