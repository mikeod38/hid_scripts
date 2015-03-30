 KammengaNIL <-read.csv(file.choose(), header=TRUE)
 KammengaNIL<-subset(KammengaNIL, select = c(genotype, date, day, treat, plate, n, dauer))
 KammengaNIL$pctdauer<-(KammengaNIL$dauer/KammengaNIL$n)
 KammengaNIL$genotype<-factor(KammengaNIL$genotype, levels=c("N2", "HW", "CSSII", "ewir19", "ewir20", "ewir21", "ewir22", "ewir23", "ewir24", "ewir25", "ewir26", "ewir27"))
 boxplot(pctdauer~genotype, data=KammengaNIL, col=c("lightgreen", "magenta", "magenta", "lightgreen", "lightgreen", "lightgreen", "lightgreen", "lightgreen", "lightgreen", "magenta", "magenta", "lightgreen"), ylim = c(0,1))
 
 