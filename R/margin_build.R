
#calculate predictive margins for different kinds of variables
margin_build <- function(object, newdata = NULL, term = NULL, value = NULL,se = TRUE,se.method = "delta", CI = TRUE, CI.boots = FALSE,level = 0.95,eps = 1e-7,na.action = na.pass,iter = 50){

  if(is.null(newdata)){
    newdata = object@data
  }

  UseMethod("margin_build",newdata[,term])
}

