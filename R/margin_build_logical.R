
#calculate predictive margins for logical variables
margin_build.logical <- function(object, newdata = NULL, term, value = NULL, se = TRUE,se.method = "delta", CI = TRUE, CI.boots = FALSE,level = 0.95,eps = 1e-7,na.action = na.pass,iter = 50){

  levs <- c(FALSE, TRUE)

  if(is.null(newdata)){
    newdata = object@data
  }

  if(is.null(value)){
    value = levs
  }

  #obtain glm models from two parts
  m1 = object@model_part1
  m2 = object@model_part2

  data_premarg= newdata

  out_pred = NULL
  Jac = NULL


  for(i in value){

    #construct datasets to calculate margins
    data_premarg[,term] = i
    data_premarg$pred = stats::predict(m1,newdata = data_premarg, type = "response",na.action = na.action)*stats::predict(m2,newdata = data_premarg, type = "response",na.action = na.action)

    #get results for predictive margins
    marg = mean(data_premarg$pred)

    res_pred = data.frame(V= i,
                          Margin = marg)
    colnames(res_pred)[1] = term

    out_pred = rbind(out_pred,res_pred)

    if(se == TRUE && se.method =="delta"){
      #calculate average Jacobian matrix to be used for variance of margins
      J0_ave = J_ave(data_premarg,m1,m2,term,eps,na.action,ame_cont = FALSE)
      Jac = rbind(Jac,J0_ave)
    }
  }

  if(se == TRUE && se.method == "delta"){

    #get variance matrix of estimated parameters for two-part model
    var = var_2part(m1,m2)

    #obtain covariance matrix of margins by delta method
    SD_pre = Jac%*%var%*%t(Jac)

    #obtain standard errors of margins by delta method
    SD_pred = sqrt(diag(SD_pre))

    out_pred$Std.Err = SD_pred

    #calculate test statistic and corresponding p-value
    z_stat_pred = out_pred$Margin/SD_pred
    pvalue_pred = 2*(1-pnorm(abs(z_stat_pred)))

    out_pred$z = z_stat_pred
    out_pred$pvalue = pvalue_pred

    #obtain CI
    if(CI == TRUE){

      out_pred$CI.L = out_pred$Margin-qnorm(1-((1-level)/2))*SD_pred
      out_pred$CI.U = out_pred$Margin+qnorm(1-((1-level)/2))*SD_pred
    }

  }

  if(se == TRUE && se.method == "bootstrap"){

    #bootstrap function for logical variables
    boots <- function(){
      models_boots <- boots_model(object)
      model1 = models_boots$model1
      model2 = models_boots$model2


      boot_res = NULL

      for(i in value){
        data_premarg= newdata
        data_premarg[,term] = i
        data_premarg$pred = stats::predict(model1,newdata = data_premarg, type = "response",na.action = na.action)*stats::predict(model2,newdata = data_premarg, type = "response",na.action = na.action)

        boot_res = c(boot_res, mean(data_premarg$pred))
      }
      boot_res


    }

    #obtain bootstrap samples of margins and correspondeing standard errors
    if(length(value) == 1){

      bootsample = replicate(iter,boots())

      SD_pred = sqrt(var(bootsample))

      out_pred$Std.Err = SD_pred

    }else{

      bootsample = t(replicate(iter,boots()))

      #obtain covariance matrix of margins from bootstrap samples if multiple margins are calculated
      SD_pre = cov(bootsample)

      SD_pred = sqrt(diag(SD_pre))

      out_pred$Std.Err = SD_pred

    }

    #calculate test statistic and corresponding p-value
    z_stat_pred = out_pred$Margin/SD_pred
    pvalue_pred = 2*(1-pnorm(abs(z_stat_pred)))

    out_pred$z = z_stat_pred
    out_pred$pvalue = pvalue_pred

    #obtain CI
    if(CI == TRUE){

      if(CI.boots == TRUE){
        #obtain covariance matrix of margins from bootstrap samples if multiple margins are calculated
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

