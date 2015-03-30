> N2HwII_C.geeglm.2 <- geeglm(value ~ id + n, id = plate, family = binomial, corstr="independence", data=N2HwII_count_C_long, scale.fix=T, waves=wave)
> N2HwII_C.geeglm.3 <- geeglm(value ~ id + n + day, id = plate, family = binomial, corstr="independence", data=N2HwII_count_C_long, scale.fix=T, waves=wave)
> N2HwII_C.geeglm.day <- geeglm(value ~ id + n, id = day, family = binomial, corstr="independence", data=N2HwII_count_C_long, scale.fix=T, waves=wave)

var.crit <- function(mymodel) sum(abs(mymodel[["geese"]]$vbeta.naiv-mymodel[["geese"]]$vbeta))
QIC.binom.geeglm <- function(model.geeglm, model.independence)
+ {
+ #calculates binomial QAIC of Pan (2001)
+ #obtain trace term of QAIC
+ AIinverse <- solve(model.independence$geese$vbeta.naiv)
+ V.msR <- model.geeglm$geese$vbeta
+ trace.term <- sum(diag(AIinverse%*%V.msR))
+ #estimated mean and observed values
+ mu.R <- model.geeglm$fitted.values
+ y <- model.geeglm$y
+ #scale for binary data
+ scale <- 1
+ #quasilikelihood for binomial model
+ quasi.R <- sum(y*log(mu.R/(1-mu.R))+log(1-mu.R))/scale
+ QIC <- (-2)*quasi.R + 2*trace.term
+ output <- c(QIC,trace.term)
+ names(output) <- c('QIC','CIC')
+ output
+ } #### calculate QAIC, CIC and var.crit

sapply(list(N2HwII_C.geeglm.2, N2HwII_C.geeglm.3, N2HwII_C.geeglm.day), var.crit)
#[1]  5.69  8.09 10.97

sapply(list(N2HwII_C.geeglm.day), function(x) QIC.binom.geeglm(x,N2HwII_C.geeglm.day))
#      [,1]
#QIC 2229.4
#CIC   50.9
> sapply(list(N2HwII_C.geeglm.2), function(x) QIC.binom.geeglm(x,N2HwII_C.geeglm.2))
#      [,1]
#QIC 2199.3
#CIC   35.8
> sapply(list(N2HwII_C.geeglm.3), function(x) QIC.binom.geeglm(x,N2HwII_C.geeglm.3))
#      [,1]
#QIC 2188.7
#CIC   44.2

### by all three criteria, id and n is best, I'll check correlation structure. 
sapply(list(N2HwII_C.geeglm.2,N2HwII_C.geeglm.2_ex), function(x) QIC.binom.geeglm(x,N2HwII_C.geeglm.2))
#      [,1]   [,2]
#QIC 2199.3 2211.8
#CIC   35.8   39.1
> sapply(list(N2HwII_C.geeglm.2, N2HwII_C.geeglm.2_ex), var.crit)
[1] 5.69 2.65
### two of three criteria fit better with independence structure

summary(N2HwII_C.geeglm.2)

#Call:
#geeglm(formula = value ~ id + n, family = binomial, data = N2HwII_count_C_long, 
#    id = plate, waves = wave, corstr = "independence", scale.fix = T)
#
# Coefficients:
#            Estimate Std.err  Wald Pr(>|W|)    
#(Intercept)  -1.4924  0.5056  8.71   0.0032 ** 
#idB          -2.6938  0.6818 15.61  7.8e-05 ***
#idC           0.7082  0.3370  4.42   0.0356 *  
#idD          -1.4509  0.4463 10.57   0.0012 ** 
#idE          -0.1525  0.3393  0.20   0.6532    
#idF          -0.0883  0.4255  0.04   0.8356    
#idG           0.6058  0.6476  0.87   0.3496    
#idH          -0.0711  0.4044  0.03   0.8605    
#n             0.0109  0.0092  1.41   0.2347    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#
#Scale is fixed.
#
#Correlation: Structure = independenceNumber of clusters:   58   Maximum cluster size:81
# 

##### generate table to adjust p-values:

> N2HwII_Csum<-geese.mysummary(N2HwII_C.geeglm.2$geese)
> N2HwII_Csum
            estimate robust.se Odds.Ratio OR.lower OR.upper        p
(Intercept)   -1.492     0.506      0.225    0.083    0.606 3.16e-03
idB           -2.694     0.682      0.068    0.018    0.257 7.79e-05
idC            0.708     0.337      2.030    1.049    3.930 3.56e-02
idD           -1.451     0.446      0.234    0.098    0.562 1.15e-03
idE           -0.152     0.339      0.859    0.441    1.670 6.53e-01
idF           -0.088     0.425      0.915    0.398    2.108 8.36e-01
idG            0.606     0.648      1.833    0.515    6.522 3.50e-01
idH           -0.071     0.404      0.931    0.422    2.058 8.61e-01
n              0.011     0.009      1.011    0.993    1.029 2.35e-01

N2HwII_Csum<-data.frame(N2HwII_Csum)
> N2HwII_Csum
            estimate robust.se Odds.Ratio OR.lower OR.upper        p
(Intercept)   -1.492     0.506      0.225    0.083    0.606 3.16e-03
idB           -2.694     0.682      0.068    0.018    0.257 7.79e-05
idC            0.708     0.337      2.030    1.049    3.930 3.56e-02
idD           -1.451     0.446      0.234    0.098    0.562 1.15e-03
idE           -0.152     0.339      0.859    0.441    1.670 6.53e-01
idF           -0.088     0.425      0.915    0.398    2.108 8.36e-01
idG            0.606     0.648      1.833    0.515    6.522 3.50e-01
idH           -0.071     0.404      0.931    0.422    2.058 8.61e-01
n              0.011     0.009      1.011    0.993    1.029 2.35e-01

p.adjust(N2HwII_Csum$p, method="bonf")
[1] 0.028413 0.000701 0.320310 0.010359 1.000000 1.000000 1.000000 1.000000 1.000000
> p.adjust(N2HwII_Csum$p, method="holm")
[1] 0.022099 0.000701 0.213540 0.009208 1.000000 1.000000 1.000000 1.000000 1.000000


####alternatively, I'd like to consider that day matters

N2HwII_Csum<-geese.mysummary(N2HwII_C.geeglm.2$geese)
> N2HwII_Csum<-geese.mysummary(N2HwII_C.geeglm.day$geese)
> N2HwII_Csum<-data.frame(N2HwII_Csum)
> N2HwII_Csum
            estimate robust.se Odds.Ratio OR.lower OR.upper       p
(Intercept)   -1.492     0.553      0.225    0.076    0.665 0.00697
idB           -2.694     0.873      0.068    0.012    0.374 0.00202
idC            0.708     0.460      2.030    0.824    5.004 0.12390
idD           -1.451     0.544      0.234    0.081    0.681 0.00764
idE           -0.152     0.416      0.859    0.380    1.940 0.71390
idF           -0.088     0.508      0.915    0.338    2.477 0.86200
idG            0.606     0.807      1.833    0.377    8.915 0.45300
idH           -0.071     0.536      0.931    0.326    2.663 0.89460
n              0.011     0.008      1.011    0.996    1.027 0.16050
p.adjust(N2HwII_Csum$p[2:8], method="holm")
[1] 0.0142 0.6195 0.0458 1.0000 1.0000 1.0000 1.0000

more conservative, HW and HWII are significant

###for C5
summary(N2HwII_C5.geeglm.day)

Call:
geeglm(formula = value ~ id + n, family = binomial, data = N2HwII_count_C5_long, 
    id = day, waves = wave, corstr = "independence", scale.fix = T)

 Coefficients:
            Estimate  Std.err  Wald Pr(>|W|)    
(Intercept)  0.23295  0.79904  0.08  0.77064    
idB         -3.16639  0.89768 12.44  0.00042 ***
idC          0.57084  0.86148  0.44  0.50757    
idD         -1.58756  0.77000  4.25  0.03923 *  
idE         -0.00551  0.73710  0.00  0.99403    
idF         -0.20085  0.70583  0.08  0.77599    
idG          1.51237  0.73787  4.20  0.04040 *  
idH         -0.41969  0.77259  0.30  0.58697    
n            0.01352  0.00947  2.04  0.15335    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Scale is fixed.

Correlation: Structure = independenceNumber of clusters:   23   Maximum cluster size: 123 
p.adjust(c(0.00042, 0.50757, 0.03923, 0.99403, 0.77599, 0.04040, 0.05697), method="holm")
[1] 0.00294 1.00000 0.23538 1.00000 1.00000 0.23538 0.23538
###only Hw is sig. different. 

### how about with glmmML? 

