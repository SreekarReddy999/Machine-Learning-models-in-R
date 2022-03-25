library(boot)
> set.seed(312)
> glm.fit = glm(default ~ income + balance, data = defaultData, family = "binomial")
> summary(glm.fit)

Call:
  glm(formula = default ~ income + balance, family = "binomial", 
      data = defaultData)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.4725  -0.1444  -0.0574  -0.0211   3.7245  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.154e+01  4.348e-01 -26.545  < 2e-16 ***
  income       2.081e-05  4.985e-06   4.174 2.99e-05 ***
  balance      5.647e-03  2.274e-04  24.836  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2920.6  on 9999  degrees of freedom
Residual deviance: 1579.0  on 9997  degrees of freedom
AIC: 1585

Number of Fisher Scoring iterations: 8

> boot.fn = function(data, index){
  +     coefs = coef(glm(default ~ income + balance, data = data, subset = index, 
                         +                      family = "binomial"))[c("income", "balance")]
  +     return(coefs)
  + }
> boot(defaultData, boot.fn, 1000)

ORDINARY NONPARAMETRIC BOOTSTRAP


Call:
  boot(data = defaultData, statistic = boot.fn, R = 1000)


Bootstrap Statistics :
  original       bias     std. error
t1* 2.080898e-05 1.947180e-08 4.668888e-06
t2* 5.647103e-03 1.362778e-05 2.323593e-04
> 
  