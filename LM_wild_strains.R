##### LM for wild strains ####
N2wild$non<-(N2wild$n-N2wild$dauer)
N2wild$genotype<-factor(N2wild$genotype, levels=c("N2", "HW", "JU561", "MY14", "CB4852", "DL238", "QX1211", "JU322", "JU1400", "JU775", "AB1", "AB3", "ED3072", "JU362", "ED3040", "JU345", "PX178", "CB3198"), ordered=FALSE)
N2wild <- N2wild[order(N2wild$day, N2wild$genotype, N2wild$plate),]
tapply(N2wild$dauer/N2wild$n, N2wild$genotype, mean, na.rm=TRUE)
library(glmmML)
N2wild.mod1<-glmmML(c(dauer,non)~genotype, family = binomial, cluster=day:genotype, data = N2wild) ### need same number observations) ###



library(lme4)
N2wild.lmer1<-lmer(cbind(dauer,non) ~ genotype + (1|n) , family=binomial(link="logit"), data=N2wild, na.action="na.exclude")


N2wild.glm1<-glm(cbind(dauer,non) ~ genotype, family=binomial(link="logit"), data=N2wild)
N2wild.glm0<-glm(cbind(dauer,non) ~ 1, family=binomial(link="logit"), data=N2wild)
N2wild.glm2<-glm(cbind(dauer,non) ~ genotype + day, family=binomial(link="logit"), data=N2wild)
N2wild.glm3<-glm(cbind(dauer,non) ~ genotype + day + n, family=binomial(link="logit"), data=N2wild) ### no extra explanatory power###
N2wild.glm4<-glm(cbind(dauer,non) ~ genotype + n, family=binomial(link="logit"), data=N2wild)
anova(N2wild.glm1, N2wild.glm2)

library(geepack) #### cluster effect can be done in gee but only when number of clusters is large ###3
N2wild.geeglm.0 <- geeglm(cbind(dauer,non) ~ 1, id = genotype, family = binomial, corstr="independence", data=N2wild, scale.fix=T)
N2wild.geeglm.1 <- geeglm(cbind(dauer,non) ~ genotype, id = genotype, family = binomial, corstr="independence", data=N2wild, scale.fix=T)
N2wild.geeglm.2 <- geeglm(cbind(dauer,non) ~ genotype, id = day, family = binomial, corstr="independence", data=N2wild, scale.fix=T)
N2wild.geeglm.3 <- geeglm(cbind(dauer,non) ~ genotype, id = day:genotype:plate, family = binomial, corstr="independence", data=N2wild, scale.fix=T) 
N2wild.geeglm.3 <- geeglm(cbind(dauer,non) ~ genotype , id = day:genotype:plate, family = binomial, corstr="independence", data=N2wild, scale.fix=T) 


##### check QIC, CIC ###
QIC.binom.geeglm <- function(model.geeglm, model.independence)
{
#calculates binomial QAIC of Pan (2001)
#obtain trace term of QAIC
AIinverse <- solve(model.independence$geese$vbeta.naiv)
V.msR <- model.geeglm$geese$vbeta
trace.term <- sum(diag(AIinverse%*%V.msR))
#estimated mean and observed values
mu.R <- model.geeglm$fitted.values
y <- model.geeglm$y
#scale for binary data
scale <- 1
#quasilikelihood for binomial model
quasi.R <- sum(y*log(mu.R/(1-mu.R))+log(1-mu.R))/scale
QIC <- (-2)*quasi.R + 2*trace.term
output <- c(QIC,trace.term)
names(output) <- c('QIC','CIC')
output
}

sapply(list(N2wild.geeglm.0, N2wild.geeglm.1, N2wild.geeglm.2, N2wild.geeglm.3),function(x) QIC.binom.geeglm(x,N2wild.geeglm.0))
