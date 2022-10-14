---
title: "Examples for R package twopartm"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
library(twopartm)
```

## 1. Continuous health cost data example: MEPS


### Data
```{r}
##data about health expenditures, i.e., non-negative continuous response
data(meps,package = "twopartm")

##Data information
?meps
```


### Fit two-part model:

Fit two-part model with the same regressors in both parts, with logistic regression model for the first part, and glm with Gamma family with log link for the second-part model
```{r}
tpmodel = tpm(exp_tot~female+age, data = meps,link_part1 = "logit",family_part2 = Gamma(link = "log"))

tpmodel
```

fit two-part model with different regressors in both parts
```{r}
tpmodel = tpm(formula_part1 = exp_tot~female+age, formula_part2 = exp_tot~female+age+ed_colplus,data = meps,link_part1 = "logit",family_part2 = Gamma(link = "log"))

tpmodel
```

fit two-part model with transformed regressors and randomly assigned weights
```{r}
set.seed(100)
meps$weights = sample(1:30,nrow(meps),replace = T)

tpmodel = tpm(formula_part1 = exp_tot~female+age, formula_part2 = exp_tot~female+I(age^2)+ed_colplus,data = meps,link_part1 = "logit",family_part2 = Gamma(link = "log"),weights = meps$weights)

tpmodel

```

### Model object

```{r}
##fit two-part model with the same regressors in both parts
tpmodel = tpm(exp_tot~female+age, data = meps,link_part1 = "logit",family_part2 = Gamma(link = "log"))

tpmodel
```

Get the formula specified for the first-part model
```{r}
tpmodel@formula_part1
```

Get the formula specified for the second-part model
```{r}
tpmodel@formula_part2
```

Get the log-likelihood for the fitted two-part model
```{r}
tpmodel@loglik
```

Get the the fitted glm model for the first part
```{r}
tpmodel@model_part1
```

Get the the fitted glm model for the second part
```{r}
tpmodel@model_part2
```


### Methods about two-part model object

```{r}
##information about fitted two-part model
print(tpmodel)

##summary information
summary(tpmodel)

##estimated coefficients for both parts
coef(tpmodel)

##estimated coefficients for the first-part model
coef(tpmodel,model = "model1")

##response residues from the full two-part model
res = residuals(tpmodel)

##response residues from the first-part model
res1 = residuals(tpmodel,model = "model1")

##deviance residues from the second-part model
res2 = residuals(tpmodel,model = "model2",type = "deviance")

##log-likehood
logLik(tpmodel)

##plots for two-part model
plot(tpmodel)
```

### Prediction

Get prediction results with standard errors for the first 10 observations in the dataset
```{r}
predict(tpmodel,newdata = meps[1:10,],se.fit = T)
```

### Average Marginal Effect (AME)

Fit two-part model with different regressors in both parts
```{r}
##fit two-part model with different regressors in both parts
tpmodel = tpm(formula_part1 = exp_tot~female+age, formula_part2 = exp_tot~female+age+ed_colplus,data = meps,link_part1 = "logit",family_part2 = Gamma(link = "log"))

tpmodel
```

AMEs for all variables with standard errors and CIs
```{r}
AME(tpmodel)
```

AMEs for variable "female"
```{r}
AME(tpmodel,term = "female")
```

AMEs for variables "female" and "age" with standard errors by bootstrap methods, CIs by bootstrap quantiles at level 0.9
```{r}
AME(tpmodel,term = c("female","age"),se.method = "bootstrap", CI.boots = T, level = 0.9)
```

AMEs for all variables with standard errors and CIs at age 20,40,60,80 respectively
```{r}
AME(tpmodel,at = list(age = c(20,40,60,80)))
```

AMEs for all variables with standard errors and CIs at age 20,40,60,80, and education level is more than college
```{r}
AME(tpmodel,at = list(age = c(20,40,60,80),ed_colplus = 1),term = "female",se.method = "bootstrap")
```


### Predictive Margins with Ratios

Predictive margins and corresponding ratios for all variables with standard errors and CIs. For factor or logical variables, predictive margins at all the levels are calculated, and for numeric (and integer) variables, predictive margins at the mean values among observations are calculated.
```{r}
margin(tpmodel)
```

Predictive margins and corresponding ratios for variable "age" at 20,40,60,80, with standard errors and CIs.
```{r}
margin(tpmodel,term = "age",value = list(age = c(20,40,60,80)))
```

Predictive margins and corresponding ratios for female, age at 20,40,60,80, and more than college education level, respectively
```{r}
margin(tpmodel,value = list(female = 1,age = c(50,70),ed_colplus = 1))
```

Predictive margins and corresponding ratios for variable "ed_colplus" with standard errors by bootstrap methods, and CIs by bootstrap quantiles at level 0.99
```{r}
margin(tpmodel,term = "ed_colplus",se.method = "bootstrap",CI.boots = T, level = 0.99)
```

Predictive margins and corresponding ratios for all variables with standard errors and CIs calculated on the first 500 observations
```{r}
margin(tpmodel,newdata = meps[1:500,])
```



## 2. Count data example: bioChemistry


### Data
```{r}
data("bioChemists",package = "twopartm")

##Data information
?bioChemists
```


### Fit two-part model:

Fit two-part model with the same regressors in both parts, with logistic regression model  for the first part, and Poisson regression model with default log link for the second-part model
```{r}
tpmodel = tpm(art ~ .,data = bioChemists,link_part1 = "logit",family_part2 = poisson)

tpmodel

summary(tpmodel)
```

```{r}
##estimated coefficients for both parts
coef(tpmodel)

##log-likehood
logLik(tpmodel)

##plots for two-part model
plot(tpmodel)

```


### Average Marginal Effect (AME)

AMEs for all variables with standard errors and CIs
```{r}
AME(tpmodel)
```

AMEs for variables "fem" and "kid5" with standard errors
```{r}
AME(tpmodel,term = c("fem","kid5"))
```

AMEs for all variables if all are women
```{r}
AME(tpmodel,at = list(fem = "Women"))
```

AMEs for variable "ment" when all are women and the numbers of children aged 5 or younger are 0,1,3,5, with standard errors by bootstrap methods, and CIs by bootstrap quantiles
```{r}
AME(tpmodel,term = "ment",at = list(fem = "Women",kid5 = c(0,1,3,5)),se.method = "bootstrap",CI.boots = T)
```


### Predictive Margins with Ratios

Predictive margins and corresponding ratios for all variables with standard errors and CIs.
```{r}
margin(tpmodel)
```

Predictive margins and corresponding ratios for variable "kid5" at 1,2,3,5, with standard errors by bootstrap methods, and CIs by bootstrap quantiles
```{r}
margin(tpmodel,term = "kid5",value = list(kid5 = c(1,2,3,5)),se.method = "bootstrap",CI.boots = T)
```

Predictive margins and corresponding ratios for variable "ment" at 6,7,8, without standard errors and CIs
```{r}
margin(tpmodel,term = "ment",value = list(ment = c(6,7,8)),se = F)
```

Predictive margins and corresponding ratios for women and all the levels of variable "mar",with standard errors by bootstrap methods, and normal-based CIs
```{r}
margin(tpmodel,term = c("fem","mar"),value = list(fem = "Women"),se.method = "bootstrap")
```


Predictive margins and corresponding ratios for all the levels of variable "mar", and for variable "phd" at 2.5,3,3.2, calculated on the first 500 observations, with standard errors and CIs
```{r}
margin(tpmodel,newdata = bioChemists[1:500,],term = c("phd","mar"),value = list(phd = c(2.5,3,3.2)))
```
