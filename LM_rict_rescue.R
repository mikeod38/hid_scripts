##### LM for rict-1 NK rescue ####

NK_resc.glm0<-glm(cbind(dauer,non) ~ 1, family=binomial(link="logit"), data=NK_resc, na.action="na.exclude")

NK_resc.glm1<-glm(cbind(dauer,non) ~ genotype, family=binomial(link="logit"), data=NK_resc, na.action="na.exclude")

NK_resc.glm2<-glm(cbind(dauer,non) ~ genotype + factor(TG_dose), family=binomial(link="logit"), data=NK_resc, na.action="na.exclude")

NK_resc.glm3<-glm(cbind(dauer,non) ~ genotype + factor(TG_dose) + day, family=binomial(link="logit"), data=NK_resc, na.action="na.exclude") ### no explanatory value ####

NK_resc.glm2a<-glm(cbind(dauer,non) ~ genotype + day, family=binomial(link="logit"), data=NK_resc, na.action="na.exclude") ### no explanatory value ####

NK_resc.glm2b<-glm(cbind(dauer,non) ~ genotype + genotype*factor(TG_dose), family=binomial(link="logit"), data=NK_resc, na.action="na.exclude")

anova(NK_resc.glm1, NK_resc.glm2)

anova(NK_resc.glm2b, test="Chisq") ### all effects are significant ####

library(multcomp)
genotype.comp <- glht(NK_resc.glm2b)
summary(genotype.comp)


#### now do it without TG dose, just genotypes ####

NK_resc_simp.glm0<-glm(cbind(dauer,non) ~ 1, family=binomial(link="logit"), data=NK_resc_simple, na.action="na.exclude")
NK_resc_simp.glm1<-glm(cbind(dauer,non) ~ genotype, family=binomial(link="logit"), data=NK_resc_simple, na.action="na.exclude")
NK_resc_simp.glm2<-glm(cbind(dauer,non) ~ genotype + day, family=binomial(link="logit"), data=NK_resc_simple, na.action="na.exclude") ### not significant ####

tab<-as.table(NK_resc_simple$dauer, NK_resc_simple$non)


#### run zero-inflated model (doesn't work yet) ####
library(pscl)
NK_resc_zinf.glm1<-zeroinfl(c(dauer,non) ~ genotype, family=binomial(link="logit"), data=NK_resc_simple, na.action="na.exclude")


#### the simpler model, genotype fits just as well as the interaction model (because it's essentially the same) #####
#library(agricolae)
#comp1 <- HSD.test(NK_resc_simp.glm1, "genotype", group=FALSE)





library(multcomp)
genotype.comp <- glht(NK_resc_simp.glm1, mcp(genotype="Tukey"))
summary(genotype.comp)


	 Simultaneous Tests for General Linear Hypotheses

Multiple Comparisons of Means: Tukey Contrasts


Fit: glm(formula = cbind(dauer, non) ~ genotype, family = binomial(link = "logit"), 
    data = NK_resc_simple, na.action = "na.exclude")

Linear Hypotheses:
                                              Estimate Std. Error z value Pr(>|z|)    
NIL59 - N2 == 0                               -20.1476  2108.9882  -0.010    1.000    
mg360 - N2 == 0                                 6.7243     1.0249   6.561   <0.001 ***
mg360; ex[N2]-1 - N2 == 0                       2.0220     0.2582   7.832   <0.001 ***
mg360; ex[HW]-1 - N2 == 0                      -0.4662     0.3146  -1.482    0.762    
mg360; ex[N2]-5 - N2 == 0                       2.2584     0.4704   4.801   <0.001 ***
mg360; ex[HW]-5 - N2 == 0                       1.6364     0.3936   4.157   <0.001 ***
mg360; ex[odr-4p::N2] - N2 == 0                -0.2359     0.2669  -0.884    0.981    
mg360 - NIL59 == 0                             26.8719  2108.9884   0.013    1.000    
mg360; ex[N2]-1 - NIL59 == 0                   22.1696  2108.9881   0.011    1.000    
mg360; ex[HW]-1 - NIL59 == 0                   19.6814  2108.9882   0.009    1.000    
mg360; ex[N2]-5 - NIL59 == 0                   22.4060  2108.9882   0.011    1.000    
mg360; ex[HW]-5 - NIL59 == 0                   21.7840  2108.9882   0.010    1.000    
mg360; ex[odr-4p::N2] - NIL59 == 0             19.9117  2108.9881   0.009    1.000    
mg360; ex[N2]-1 - mg360 == 0                   -4.7023     1.0149  -4.633   <0.001 ***
mg360; ex[HW]-1 - mg360 == 0                   -7.1905     1.0307  -6.976   <0.001 ***
mg360; ex[N2]-5 - mg360 == 0                   -4.4659     1.0884  -4.103   <0.001 ***
mg360; ex[HW]-5 - mg360 == 0                   -5.0880     1.0575  -4.811   <0.001 ***
mg360; ex[odr-4p::N2] - mg360 == 0             -6.9603     1.0172  -6.843   <0.001 ***
mg360; ex[HW]-1 - mg360; ex[N2]-1 == 0         -2.4882     0.2804  -8.874   <0.001 ***
mg360; ex[N2]-5 - mg360; ex[N2]-1 == 0          0.2364     0.4483   0.527    0.999    
mg360; ex[HW]-5 - mg360; ex[N2]-1 == 0         -0.3857     0.3669  -1.051    0.951    
mg360; ex[odr-4p::N2] - mg360; ex[N2]-1 == 0   -2.2580     0.2256 -10.008   <0.001 ***
mg360; ex[N2]-5 - mg360; ex[HW]-1 == 0          2.7246     0.4830   5.642   <0.001 ***
mg360; ex[HW]-5 - mg360; ex[HW]-1 == 0          2.1025     0.4086   5.146   <0.001 ***
mg360; ex[odr-4p::N2] - mg360; ex[HW]-1 == 0    0.2302     0.2885   0.798    0.990    
mg360; ex[HW]-5 - mg360; ex[N2]-5 == 0         -0.6221     0.5378  -1.157    0.921    
mg360; ex[odr-4p::N2] - mg360; ex[N2]-5 == 0   -2.4943     0.4533  -5.502   <0.001 ***
mg360; ex[odr-4p::N2] - mg360; ex[HW]-5 == 0   -1.8723     0.3731  -5.018   <0.001 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Adjusted p values reported -- single-step method)

genotype.comp <- glht(NK_resc_simp.glm1, mcp(genotype="Bonferroni"))