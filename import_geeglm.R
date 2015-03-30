N2HwII <- read.csv(file.choose(), header=TRUE) #import CSV file
library(reshape);
N2HwII <- melt(N2HwII, id = c("genotype","id", "day", "plate", "treat", "n"))
N2HwII <- N2HwII[order(N2HwII$id, N2HwII$day, N2HwII$treat, N2HwII$plate),]
N2HwII$wave <- as.numeric(factor(paste(N2HwII$variable)))
N2HwII <- N2HwII[complete.cases(N2HwII),]
library(geepack)
N2HwII.geeglm.1 <- geeglm(value ~ id, id = plate, family = binomial, corstr="independence", data=N2HwII, scale.fix=T, waves=wave)
N2HwII.geeglm.2 <- geeglm(value ~ id + treat, id = plate, family = binomial, corstr="independence", data=N2HwII, scale.fix=T, waves=wave)
N2HwII.geeglm.3 <- geeglm(value ~ id + treat + n, id = plate, family = binomial, corstr="independence", data=N2HwII, scale.fix=T, waves=wave)
N2HwII.geeglm.day <- geeglm(value ~ id + treat + n, id = day, family = binomial, corstr="independence", data=N2HwII, scale.fix=T, waves=wave)

#N2Hwcoef<-coef(N2HwII.geeglm.day)
#N2HwEst<-c(inv.logit(N2Hwcoef[1:1]), inv.logit(N2Hwcoef[1:1])*exp(N2Hwcoef[2:8]))

#