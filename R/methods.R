
#Method "show"----
#' @export
setMethod("show", signature = signature(object = "twopartm"),
          definition = function(object){
            cat("\nTwo-Part Model\n")
            cat("1. First-part model:\n")
            base::print(object@model_part1)
            cat("\n2. Second-part model:\n")
            base::print(object@model_part2)
            cat("\n")
          })

#Method "summary"----
#' @importFrom stats summary.glm
#' @export
setMethod(f = "summary",
          signature = signature(object = "twopartm"),
          definition = function(object,...){
            return(list(Firstpart.model = stats::summary.glm(object@model_part1,...),
                        Secondpart.model = stats::summary.glm(object@model_part2,...)))
          }
)




#Method "residuals"----
#' @importFrom stats residuals
#' @export
setMethod(f = "residuals",
          signature = signature(object = "twopartm"),
          definition = function(object,model = c("tpm","model1","model2"),
                                type = c("deviance", "pearson", "working","response", "partial")){

            model <- match.arg(model)
            type <- match.arg(type)

            #obtain residuals for the whole two-part model, or separate two-part models
            if(model == "tpm"){
              object@residuals
            }else if (model == "model1"){
              stats::residuals(object@model_part1,type)
            }else if(model == "model2"){
              stats::residuals(object@model_part2,type)
            }
          }
)

#Method "logLik"----
#' @importFrom stats logLik
#' @export
setMethod(f = "logLik",
          signature = signature(object = "twopartm"),
          definition = function(object,...){

            #calculate the log likelihood value of the two-part model
            loglik = stats::logLik(object@model_part1,...)[1]+stats::logLik(object@model_part2,...)[1]

            #obtain the degree of freedom of two-part model
            df = (object@n_part1 - object@model_part1$df.residual)+(object@n_part2 - object@model_part2$df.residual)

            #return a loglik object
            structure(loglik, df = df, nobs = object@n, class = "logLik")
          }
)


#Method "plot"----
#' @importFrom graphics plot
#' @export
setMethod(f = "plot",
          signature = signature(x="twopartm", y="missing"),
          definition = function(x,y,...){

            cat("\nTwo-part model:\n")
            xr = x@fitted
            yr = x@residuals
            graphics::plot.default(xr,yr,xlab = "fitted values" ,ylab = "Residues", main = "Two-part model")
            cat("\nFirst-part model:\n")
            plot(x@model_part1,main = "First-part Model",...)
            cat("\nSecond-part model:\n")
            plot(x@model_part2,main = "Second-part Model",...)


          }
)

#Method "coef"----
#' @export
setMethod(f = "coef",
          signature = signature(object="twopartm"),
          definition = function(object,model = c("tpm","model1","model2"),...){

            model <- match.arg(model)

            #obtain coefficients for the whole two-part model, or separate two-part models
            if(model == "tpm"){
              list(first_part = coef(object@model_part1,...),
                   second_part = coef(object@model_part2),...)
            }else if (model == "model1"){
              coef(object@model_part1,...)
            }else if(model == "model2"){
              coef(object@model_part2,...)
            }

          }
)

