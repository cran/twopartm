
#function to calculate Jacobian matrix used in delta method
Jacobian <- function(model1,model2,data,term, eps = 1e-7,na.action = na.pass, ame_cont = FALSE){

  #get coefficients from two models
  coef =  c(model1[["coefficients"]],model2[["coefficients"]])
  m1 = m3 = model1
  m2 = m4 = model2
  out = matrix(NA_real_, nrow = nrow(data), ncol = length(coef))

  for (i in 1:length(coef)) {

    #change coefficients in the two-part models by a very small value
    coeftemp1 <- coef
    coeftemp1[i] <- coeftemp1[i] + eps
    m1[["coefficients"]] = coeftemp1[1:length(model1[["coefficients"]])]
    m2[["coefficients"]] = coeftemp1[(length(model1[["coefficients"]])+1):(length(coef))]

    coeftemp2 <- coef
    coeftemp2[i] <- coeftemp2[i] - eps
    m3[["coefficients"]] = coeftemp2[1:length(model1[["coefficients"]])]
    m4[["coefficients"]] = coeftemp2[(length(model1[["coefficients"]])+1):(length(coef))]

    #calculate derivatives by numerical differentiation
    if(ame_cont == FALSE){
      pre1 = stats::predict(m1,newdata = data,type = "response", na.action = na.action)*stats::predict(m2,newdata = data,type = "response", na.action = na.action)
      pre2 = stats::predict(m3,newdata = data,type = "response", na.action = na.action)*stats::predict(m4,newdata = data,type = "response", na.action = na.action)
      out[,i] = (pre2-pre1)/(2*eps)
    }else{
      dydx1 = dydx.cont(m1,m2,data,term,eps = eps,na.action = na.action)
      dydx2 = dydx.cont(m3,m4,data,term,eps = eps,na.action = na.action)
      out[,i] = (dydx2-dydx1)/(2*eps)
    }

  }
  out
}



#function to calculate average Jacobian matrix
J_ave <- function(data,model1,model2,term,eps = 1e-7,na.action = na.pass,ame_cont = FALSE){
  J = Jacobian(model1,model2,data, term, eps = eps,na.action, ame_cont)
  #return the average Jacobian matrix among the data set
  return(colMeans(J))
}


#set step for numerical differentiation
setstep <- function(x,eps) {
  max(abs(x), 1, na.rm = TRUE) * sqrt(eps)
}

#function to calculate margins for continuous variable
dydx.cont <- function(model1,model2,data,term,eps = 1e-7,na.action = na.pass){

  #change variable values by a very small value
  d0 <- d1 <- data
  d0[[term]] <- d0[[term]] - setstep(d0[[term]],eps)
  d1[[term]] <- d1[[term]] + setstep(d1[[term]],eps)

  pred0 <- stats::predict(model1,newdata = d0,type = "response",na.action = na.action)*stats::predict(model2,newdata = d0,type = "response",na.action = na.action)
  pred1 <- stats::predict(model1,newdata = d1,type = "response",na.action = na.action)*stats::predict(model2,newdata = d1,type = "response",na.action = na.action)

  #calculate marginal effects by numerical differentiation
  out <- (pred1 - pred0) / (d1[[term]] - d0[[term]])
  out
}


#function to get variance matrix of estimated parameters for two-part model assuming independence of two models
var_2part <- function(model1,model2){

  #obtain covariance matrices from two-part models
  cov1 = summary(model1)$cov.scaled
  cov2 = summary(model2)$cov.scaled

  #combine the covariance matrices assuming independence between two-part models
  var = rbind(cbind(cov1,matrix(0,nrow=nrow(cov1),ncol=ncol(cov2))),cbind(matrix(0,nrow=nrow(cov2),ncol=ncol(cov1)),cov2))

  var
}

