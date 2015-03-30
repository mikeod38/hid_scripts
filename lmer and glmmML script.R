###for uncegl data
uncegl <-read.csv(file.choose(), header=TRUE)
uncegl$non<-(uncegl$n-uncegl$dauer)
uncegl$pctdauer<-(uncegl$dauer/uncegl$n)
library(glmmML)
uncegl.mod1<-glmmML(c(dauer,non)~id + (1|n), family = binomial, cluster=day:id, subset = treat == "C", data = uncegl)



library(lme4)
uncegl.lmer1<-lmer(cbind(dauer,non) ~ id + (1|n), family=binomial(link="logit"), data=uncegl, na.action="na.exclude")
uncegl.lmer2<-lmer(cbind(dauer,non) ~ id + (1|n) + (1|day:id), family=binomial(link="logit"), data=uncegl, na.action="na.exclude")
uncegl.lmer3<-lmer(cbind(dauer,non) ~ id + n + (1|day:id), family=binomial(link="logit"), data=uncegl, na.action="na.exclude")
uncegl.lmer4<-lmer(cbind(dauer,non) ~ id + n + (1|day), family=binomial(link="logit"), data=uncegl, na.action="na.exclude")

###plot
f <- fitted(uncegl.lmer2) 
r <- residuals(uncegl.lmer2) 
plot(f,r) 
sm <- loess(r~f) 
v <- seq(min(f),max(f),length=101) 
lines(v,predict(sm,data.frame(f=v)),col=2) 

f <- fitted(uncegl.lmer1) 
r <- residuals(uncegl.lmer1) 
plot(f,r) 
sm <- loess(r~f) 
v <- seq(min(f),max(f),length=101) 
lines(v,predict(sm,data.frame(f=v)),col=2) 

f <- fitted(uncegl.lmer3) 
r <- residuals(uncegl.lmer3) 
plot(f,r) 
sm <- loess(r~f) 
v <- seq(min(f),max(f),length=101) 
lines(v,predict(sm,data.frame(f=v)),col=2) 

fix<-fixef(uncegl.lmer3)
plot(fix)
VarCorr(uncegl.lmer3)
#####-----------------------------------------------######
library(glmmML)
unceglsub<-uncegl[uncegl$id %in% c('A', 'C', 'E', 'F', 'G'),] 
unceglsub.glmmML1<-glmmML(cbind(dauer,non) ~ id, cluster=day:id, family=binomial(link="logit"), data=unceglsub, na.action="na.exclude")




###for simple glm
uncegl.glm1<-glm(pctdauer ~ id, weights=n, family=binomial(link="logit"), data=uncegl)
uncegl.glm2<-glm(pctdauer ~ id + n, weights=n, family=binomial(link="logit"), data=uncegl)
