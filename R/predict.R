#Method "predict"----
#prediction function for two-part models
#' @importFrom stats predict glm
#' @export
setMethod(f = "predict",
          signature = signature(object = "twopartm"),
          definition = function(object,newdata = NULL, se.fit = FALSE,dispersion_part1 = NULL,dispersion_part2 = NULL,na.action = na.pass){

            #check inputs
            if(class(object)[1] != "twopartm"){
              stop("object should be a two-part model object.")
            }
            if(!is.null(newdata) & !is.data.frame(newdata)){
              stop("newdata should be a data frame.")
            }
            if(is.data.frame(newdata) &
               FALSE %in% (c(attr(object@model_part1$terms,"term.labels"),
                             attr(object@model_part2$terms,"term.labels")) %in% colnames(newdata))){
              stop("some of independent variables of the two-part model are not available in the newdata set. ")

            }


            if(is.null(newdata)){
              data = object@data
            }else{
              data = as.data.frame(newdata)
            }

            #obtain glm models from two parts
            model1 = object@model_part1
            model2 = object@model_part2


            predict1 = stats::predict(model1,newdata = data,type = "response",se.fit = T, dispersion = dispersion_part1, na.action = na.action)
            predict2 = stats::predict(model2,newdata = data,type = "response",se.fit = T, dispersion = dispersion_part2, na.action = na.action)

            #get fitted values
            fit = predict1$fit*predict2$fit

            if(se.fit == FALSE){

              #return predictive values
              fit

            }else{

              #standard errors of predictive values for two-part models
              se.fit = (predict1$se.fit^2*predict2$fit^2+predict2$se.fit^2*predict1$fit^2)^0.5

              #update residual.scales for two-parts with dispersion parameters
              if(is.null(dispersion_part1)){
                residual.scale_part1 = predict1$residual.scale
              }else{
                residual.scale_part1 = sqrt(dispersion_part1)
              }
              if(is.null(dispersion_part2)){
                residual.scale_part2 = predict2$residual.scale
              }else{
                residual.scale_part2 = sqrt(dispersion_part2)
              }

              #return a list of prediction results
              list(fit = fit,
                   se.fit = se.fit,
                   residual.scale_part1 = residual.scale_part1,
                   residual.scale_part2 = residual.scale_part2)
            }


          }
)
