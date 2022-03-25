> defaultData=read.csv("D:/Default.csv",header=TRUE,stringsAsFactors=TRUE)
> head(defaultData)
default student   balance    income
1      No      No  729.5265 44361.625
2      No     Yes  817.1804 12106.135
3      No      No 1073.5492 31767.139
4      No      No  529.2506 35704.494
5      No      No  785.6559 38463.496
6      No     Yes  919.5885  7491.559
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

> train = sample(dim(defaultData)[1], 0.75*dim(defaultData)[1])
> glm.fit = glm(default ~ income + balance, data = defaultData, subset = train, family = "binomial")
> glm.probs = predict(glm.fit, defaultData[-train, ], type = "response")
> glm.preds = rep("No", dim(defaultData)[1])
> glm.preds[glm.probs > 0.5] = "Yes"
> mean(glm.preds != defaultData[-train, "default"])
[1] 0.0224
> train = sample(dim(defaultData)[1], 0.75*dim(defaultData)[1])
> glm.fit = glm(default ~ income + balance, data = defaultData, subset = train, family = "binomial")
> glm.probs = predict(glm.fit, defaultData[-train, ], type = "response")
> glm.preds = rep("No", dim(defaultData)[1])
> glm.preds[glm.probs > 0.5] = "Yes"
> mean(glm.preds != defaultData[-train, "default"])
[1] 0.0272
> train = sample(dim(defaultData)[1], 0.75*dim(defaultData)[1])
> glm.fit = glm(default ~ income + balance, data = defaultData, subset = train, family = "binomial")
> glm.probs = predict(glm.fit, defaultData[-train, ], type = "response")
> glm.preds = rep("No", dim(defaultData)[1])
> glm.preds[glm.probs > 0.5] = "Yes"
> mean(glm.preds != defaultData[-train, "default"])
[1] 0.024
> train = sample(dim(defaultData)[1], 0.75*dim(defaultData)[1])
> glm.fit = glm(default ~ income + balance, data = defaultData, subset = train, family = "binomial")
> glm.probs = predict(glm.fit, defaultData[-train, ], type = "response")
> glm.preds = rep("No", dim(defaultData)[1])
> glm.preds[glm.probs > 0.5] = "Yes"
> mean(glm.preds != defaultData[-train, "default"])
[1] 0.0296
> 1 - mean(defaultData[-train, "default"] == "No")
[1] 0.0376
> with.student = rep(0, 50)
> without.student = rep(0, 50)
> for (i in 1:50){
  +     train = sample(dim(defaultData)[1], 0.75*dim(defaultData)[1])
  +     with.student.fit = glm(default ~ ., data = defaultData, subset = train, family = "binomial")
  +     without.student.fit = glm(default ~ income + balance, data = defaultData, subset = train, family = "binomial")
  +     with.student.probs = predict(with.student.fit, defaultData[-train, ], type = "response")
  +     without.student.probs = predict(without.student.fit, defaultData[-train, ], type = "response")
  +     with.student.preds = rep("No", dim(defaultData)[1])
  +     without.student.preds = rep("No", dim(defaultData)[1])
  +     with.student.preds[with.student.probs > 0.5] = "Yes"
  +     without.student.preds[without.student.probs > 0.5] = "Yes"
  +     with.student[i] = mean(with.student.preds != defaultData[-train, "default"])
  +     without.student[i] = mean(without.student.preds != defaultData[-train, "default"])
  + }
> difference = with.student - without.student
> errors = data.frame(with.student, without.student, difference)
> mean(errors$difference)
[1] 0.000304