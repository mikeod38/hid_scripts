 uncegl <-read.csv(file.choose(), header=TRUE)
 uncegl$pctdauer<-(uncegl$dauer/uncegl$n)
 uncegl$genotype<-ordered(uncegl$genotype, levels=c("N2", "unc-13", "unc-64 (e246)", "unc-31 (e169)", "unc-31 (e928)"))
 par(mar=c(7,5,1,1))
 boxplot(pctdauer~genotype, data=uncegl, col=c("grey") , ylim=c(0,1), las=2)
 
 ##egl
 uncegl <-read.csv(file.choose(), header=TRUE)
 uncegl$pctdauer<-(uncegl$dauer/uncegl$n)
 uncegl$genotype<-ordered(uncegl$genotype, levels=c("N2", "egl-3", "egl-21", "bli-4", "daf-28", "daf-16"))
 par(mar=c(7,5,1,1))
 boxplot(pctdauer~genotype, data=uncegl, col="grey", ylim=c(0,1), las=2)
 
 ###egl no daf
 uncegl <-read.csv(file.choose(), header=TRUE)
 uncegl$pctdauer<-(uncegl$dauer/uncegl$n)
 uncegl$genotype<-ordered(uncegl$genotype, levels=c("N2", "egl-3", "egl-21", "bli-4"))
 par(mar=c(7,5,1,1))
 boxplot(pctdauer~genotype, data=uncegl, col="lightgreen", ylim=c(0,1), las=2)

###uncrescue
 uncresc <-read.csv(file.choose(), header=TRUE)
 uncresc$pctdauer<-(uncresc$dauer/uncresc$n)
 uncresc$genotype<-ordered(uncresc$genotype, levels=c("N2", "unc-64", "tax2p::unc-64"))
 par(mar=c(8,5,1,1))
 boxplot(pctdauer~genotype+temp, data=uncresc, col="lightgreen", ylim=c(0,1), las=2)
 
 ###uncrescue combined @ 27
uncresccom <-read.csv(file.choose(), header=TRUE)
 uncresccom$pctdauer<-(uncresccom$dauer/uncresccom$n)
 uncresccom$genotype<-ordered(uncresccom$genotype, levels=c("N2", "unc-13", "unc-31 (e928)", "unc-64 (e246)", "tax2p::unc-64"))
 par(mar=c(8,5,1,1))
 boxplot(pctdauer~genotype, data=uncresccom, col="lightgreen", ylim=c(0,1), las=2)