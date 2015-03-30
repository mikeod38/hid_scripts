 curve(logit.ex)
> logit.Hw <- function(x) {lin.model <- coef(N2HwII_treat.geeglm)[1]+ +coef(N2HwII_treat.geeglm)[2] + coef(N2HwII_treat.geeglm)[9]*x
+ exp(lin.model)/(1+exp(lin.model))}
> curve(logit.Hw, add=T, col="red")
> logit.Hw <- function(x) {lin.model <- coef(N2HwII_treat.geeglm)[1]+ coef(N2HwII_treat.geeglm)[2] + coef(N2HwII_treat.geeglm)[9]*x
+ exp(lin.model)/(1+exp(lin.model))}
> curve(logit.Hw, add=T, col="red")
> curve(logit.Hw, col="red")
> logit.N2 <- function(x) {lin.model <- coef(N2HwII_treat.geeglm)[1] + coef(N2HwII_treat.geeglm)[9]*x
+ exp(lin.model)/(1+exp(lin.model))}
> curve(logit.N2, add=T, col="red")
> curve(logit.N2, add=F, col="red")
> curve(logit.Hw, add=T)
> curve(logit.Hw)
> curve(logit.N2)
> curve(logit.Hw, from=0, to=1, add=TRUE)
> curve(logit.N2, ylim=(0,1)
Error: unexpected ',' in "curve(logit.N2, ylim=(0,"
> plot(ylim=(0,1))
Error: unexpected ',' in "plot(ylim=(0,"
> plot(logit.N2, ylim=(0,1)
Error: unexpected ',' in "plot(logit.N2, ylim=(0,"
> plot(logit.N2, ylim=(0,1))
Error: unexpected ',' in "plot(logit.N2, ylim=(0,"
> plot(logit.N2, ylim=(0 1))
Error: unexpected numeric constant in "plot(logit.N2, ylim=(0 1"
> plot(logit.N2, ylim=(0:1))
> curve(logit.Hw, from=0, to=1, add=TRUE, col="red")
> logit.HwII <- function(x) {lin.model <- coef(N2HwII_treat.geeglm)[1]+ +coef(N2HwII_treat.geeglm)[4] + coef(N2HwII_treat.geeglm)[9]*x
+ exp(lin.model)/(1+exp(lin.model))}
> curve(logit.HwII, from=0, to=1, add=TRUE, col="blue")
> logit.HwV <- function(x) {lin.model <- coef(N2HwII_treat.geeglm)[1]+ +coef(N2HwII_treat.geeglm)[7] + coef(N2HwII_treat.geeglm)[9]*x
+ exp(lin.model)/(1+exp(lin.model))}
> curve(logit.HwIII, from=0, to=1, add=TRUE, col="green")
Error in eval(expr, envir, enclos) : 
  could not find function "logit.HwIII"
> curve(logit.HwV, from=0, to=1, add=TRUE, col="green")
> ogit.HwIII <- function(x) {lin.model <- coef(N2HwII_treat.geeglm)[1]+ +coef(N2HwII_treat.geeglm)[5] + coef(N2HwII_treat.geeglm)[9]*x
+ + exp(lin.model)/(1+exp(lin.model))}
> logit.HwIII <- function(x) {lin.model <- coef(N2HwII_treat.geeglm)[1]+ +coef(N2HwII_treat.geeglm)[5] + coef(N2HwII_treat.geeglm)[9]*x
+ + exp(lin.model)/(1+exp(lin.model))}
> logit.HwIV <- function(x) {lin.model <- coef(N2HwII_treat.geeglm)[1]+ +coef(N2HwII_treat.geeglm)[6] + coef(N2HwII_treat.geeglm)[9]*x
+ + exp(lin.model)/(1+exp(lin.model))}
> logit.HwX <- function(x) {lin.model <- coef(N2HwII_treat.geeglm)[1]+ +coef(N2HwII_treat.geeglm)[8] + coef(N2HwII_treat.geeglm)[9]*x
+ + exp(lin.model)/(1+exp(lin.model))}
> curve(logit.HwIII, from=0, to=1, add=TRUE, col="grey")
> curve(logit.HwIV, from=0, to=1, add=TRUE, col="grey")
> curve(logit.HwX, from=0, to=1, add=TRUE, col="grey")