

#calculate predictive margins for numerical variables
margin_build.default <- function(object, newdata = NULL, term, value = NULL, se = TRUE,se.method = "delta", CI = TRUE, CI.boots = FALSE,level = 0.95,eps = 1e-7,na.action = na.pass,iter = 50){

  if(is.null(newdata)){
    newdata = object@data
  }

  #obtain glm models from two parts
  m1 = object@model_part1
  m2 = object@model_part2

  #if no values of the numerical variable is specified, predictive margin at the mean value would be calculated.
  if(is.null(value)){
    value = mean(newdata[,term])
  }

  out_pred = NULL
  Jac = NULL

  for (i in value) {

    #construct datasets to calculate margins
    data_premarg1 = newdata
    data_premarg1[,term] = i

    data_premarg1$pred = stats::predict(m1,newdata = data_premarg1, type = "response",na.action = na.action)*stats::predict(m2,newdata = data_premarg1, type = "response",na.action = na.action)

    #get results for predictive margins
    marg = mean(data_premarg1$pred)

    res = data.frame(Variable = term,
                     Value = i,
                     Margin = marg)

    if(se == TRUE && se.method == "delta"){

      #calculate average Jacobian matrix to be used for variance of margins
      J0_ave = J_ave(data_premarg1,m1,m2,term,eps,na.action,ame_cont = FALSE)
      Jac = rbind(Jac,J0_ave)

      #get variance matrix of estimated parameters for two-part model
      var = var_2part(m1,m2)

      #obtain standard errors of margins by delta method
      SD_pred = sqrt(t(J0_ave)%*%var%*%(J0_ave))

      #calculate test statistic and corresponding p-value
      z_stat_pred = res$Margin/SD_pred
      pvalue_pred = 2*(1-pnorm(abs(z_stat_pred)))

      res$Std.Err = SD_pred
      res$z = z_stat_pred
      res$pvalue = pvalue_pred

      #obtain CI
      if(CI == TRUE){

        res$CI.L = res$Margin-qnorm(1-((1-level)/2))*SD_pred
        res$CI.U = res$Margin+qnorm(1-((1-level)/2))*SD_pred

      }

    }

    out_pred = rbind(out_pred,res)

  }

  if(se == TRUE && se.method == "delta" && length(value) >1){

    #obtain covariance matrix of margins by delta method if multiple margins are calculated
    var = var_2part(m1,m2)
    SD_pre = Jac%*%var%*%t(Jac)
  }

  if(se == TRUE && se.method == "bootstrap"){

    #bootstrap function for numerical variables
    boots <- function(){
      models_boots <- boots_model(object)
      model1 = models_boots$model1
      model2 = models_boots$model2

      boot_res = NULL

      for (i in value) {
        data_premarg1 = newdata
        data_premarg1[,term] = i
        data_premarg1$p = stats::predict(model1,newdata = data_premarg1, type = "response",na.action = na.action)
        data_premarg1$mu = stats::predict(model2,newdata = data_premarg1, type = "response",na.action = na.action)
        data_premarg1$pred = data_premarg1$p*data_premarg1$mu

        boot_res = c(boot_res,mean(data_premarg1$pred))

      }

      boot_res

    }

    #obtain bootstrap samples of margins and correspondeing standard errors
    if(length(value) == 1){

      bootsample = replicate(iter,boots())

      SD_pred = sqrt(var(bootsample))

    }else{

      bootsample = t(replicate(iter,boots()))

      #obtain covariance matrix of margins from bootstrap samples if multiple margins are calculated
      SD_pre = cov(bootsample)

      SD_pred = sqrt(diag(SD_pre))
    }

    #calculate test statistic and corresponding p-value
    z_stat_pred = out_pred$Margin/SD_pred
    pvalue_pred = 2*(1-pnorm(abs(z_stat_pred)))

    out_pred$Std.Err = SD_pred
    out_pred$z = z_stat_pred
    out_pred$pvalue = pvalue_pred

    #obtain CI
    if(CI == TRUE){

      if(CI.boots == TRUE){

        #converted CI by quantiles of bootstrap samples

        if(length(value) == 1){
          out_pred$bootsCI.L = 2*out_pred$Margin-quantile(bootsample,probs = (1-((1-level)/2)))
          out_pred$bootsCI.U = 2*out_pred$Margin-quantile(bootsample,probs = (((1-level)/2)))
        }else{
          out_pred$bootsCI.L = 2*out_pred$Margin-apply(bootsample, 2, function(t){quantile(t,probs = (1-((1-level)/2)))})
          out_pred$bootsCI.U = 2*out_pred$Margin-apply(bootsample, 2, function(t){quantile(t,probs = (((1-level)/2)))})
        }
      }else{
        #CI by assuming normal distribution
        out_pred$CI.L = out_pred$Margin-qnorm(1-((1-level)/2))*SD_pred
        out_pred$CI.U = out_pred$Margin+qnorm(1-((1-level)/2))*SD_pred
      }

    }

  }


  #return margins with CIs
  if(length(value) > 1){

    out_ratio = get_ratioCI(value,out_pred,SD_pre,level,se,CI)
    list(Pred_marg = out_pred, Ratio = out_ratio)

  }else{

    out_pred

  }


}
