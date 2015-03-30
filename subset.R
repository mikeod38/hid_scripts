N2HwII_count_C5<-subset(N2HwII_count,N2HwII_count$treat=="C5")
plot(N2HwII_count_C5$genotype,N2HwII_count_C5$dauer/N2HwII_count_C5$n,col="darkgreen")
