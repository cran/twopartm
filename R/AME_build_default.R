
#calculate AME for numerical variables
AME_build.default <- function(object, newdata = NULL, term, se = TRUE,se.method = "delta", CI = TRUE, CI.boots = FALSE,level = 0.95,eps = 1e-7,na.action = na.pass, iter = 50){


  if(is.null(newdata)){
    newdata = object@data
  }

  #obtain glm models from two parts
  m1 = object@model_part1
  m2 = object@model_part2

  #calculate AME
  meanME = mean(dydx.cont(m1,m2,newdata,term,eps,na.action = na.action))

  res = data.frame(Variable = term,
                   dydx = meanME)

  if(se == TRUE && se.method == "delta"){

    #calculate average Hessian matrix to be used for variance of marginal effects
    J_con_ave = J_ave(newdata,m1,m2,term,eps,na.action = na.action,ame_cont = TRUE)

    #get variance matrix of estimated parameters for two-part model
    var = var_2part(m1,m2)

    #obtain standard errors of AME by delta method
    SD_AME = as.numeric(sqrt(t(J_con_ave)%*%var%*%(J_con_ave)))

    #calculate test statistic and correpsonding p-value
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

    #bootstrap function for numerical variables
    boots <- function(){
      models_boots <- boots_model(object)
      model1 = models_boots$model1
      model2 = models_boots$model2

      mean(dydx.cont(model1,model2,newdata,term,eps,na.action = na.action))

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
