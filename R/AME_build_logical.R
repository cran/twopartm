

#calculate AME for logical variables
AME_build.logical <- function(object, newdata = NULL, term, se = TRUE,se.method = "delta", CI = TRUE, CI.boots = FALSE,level = 0.95,eps = 1e-7,na.action = na.pass, iter = 50){

  if(is.null(newdata)){
    newdata = object@data
  }

  #obtain glm models from two parts
  m1 = object@model_part1
  m2 = object@model_part2

  #construct datasets to calculate AME
  data_premarg1 = data_premarg2 = newdata
  data_premarg1[,term] = FALSE
  data_premarg1$pred = stats::predict(m1,newdata = data_premarg1, type = "response",na.action = na.action)*stats::predict(m2,newdata = data_premarg1, type = "response",na.action = na.action)

  data_premarg2[,term] = TRUE
  data_premarg2$pred = stats::predict(m1,newdata = data_premarg2, type = "response",na.action = na.action)*stats::predict(m2,newdata = data_premarg2, type = "response",na.action = na.action)

  #calculate AME
  meanME = mean(data_premarg2$pred)-mean(data_premarg1$pred)

  res = data.frame(Variable = paste(term,"TRUE",sep = ""),
                   dydx = meanME)

  if(se == TRUE && se.method == "delta"){

    #calculate average Jacobian matrix to be used for variance of marginal effects
    J0_ave = J_ave(data_premarg1,m1,m2,term,eps,na.action = na.action,ame_cont = FALSE)
    J1_ave = J_ave(data_premarg2,m1,m2,term,eps,na.action = na.action,ame_cont = FALSE)

    #get variance matrix of estimated parameters for two-part model
    var = var_2part(m1,m2)

    #obtain standard errors of AME by delta method
    SD_AME = as.numeric(sqrt(t(J1_ave-J0_ave)%*%var%*%((J1_ave-J0_ave))))

    #calculate test statistic and corresponding p-value
    z_stat_AME = meanME /SD_AME
    pvalue_AME = 2*(1-pnorm(abs(z_stat_AME)))

    res$Std.Err = SD_AME
    res$z = z_stat_AME
    res$pvalue = pvalue_AME

    #obtain CI
    if(CI == TRUE){

      res$CI.L = meanME-qnorm(1-((1-level)/2))*SD_AME
      res$CI.U = meanME+qnorm(1-((1-level)/2))*SD_AME

    }
  }

  if(se == TRUE && se.method == "bootstrap"){

    #bootstrap function for logical variables
    boots <- function(){
      models_boots <- boots_model(object)
      model1 = models_boots$model1
      model2 = models_boots$model2

      data_premarg1 = data_premarg2 = newdata
      data_premarg1[,term] = FALSE
      data_premarg1$pred = stats::predict(model1,newdata = data_premarg1, type = "response",na.action = na.action)*stats::predict(model2,newdata = data_premarg1, type = "response",na.action = na.action)

      data_premarg2[,term] = TRUE
      data_premarg2$pred = stats::predict(model1,newdata = data_premarg2, type = "response",na.action = na.action)*stats::predict(model2,newdata = data_premarg2, type = "response",na.action = na.action)


      mean(data_premarg2$pred)-mean(data_premarg1$pred)

    }

    #obtain bootstrap samples of AME
    bootsample = replicate(iter,boots())

    #obtain standard errors of AME from bootstrap samples
    SD_AME = sqrt(var(bootsample))

    #calculate test statistic and correpsonding p-value
    z_stat_AME = meanME /SD_AME
    pvalue_AME = 2*(1-pnorm(abs(z_stat_AME)))

    res$Std.Err = SD_AME
    res$z = z_stat_AME
    res$pvalue = pvalue_AME

    #obtain CI
    if(CI == TRUE){

      if(CI.boots == TRUE){
        #converted CI by quantiles of bootstrap samples
        res$bootsCI.L = 2*meanME-quantile(bootsample,probs = (1-((1-level)/2)))
        res$bootsCI.U = 2*meanME-quantile(bootsample,probs = (((1-level)/2)))
      }else{
        #CI by assuming normal distribution
        res$CI.L = meanME-qnorm(1-((1-level)/2))*SD_AME
        res$CI.U = meanME+qnorm(1-((1-level)/2))*SD_AME
      }

    }
  }


  #return results with AME and their standard errors
  res

}
