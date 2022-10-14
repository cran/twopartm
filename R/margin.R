#Method "margin"----
#calculate predictive margins
setGeneric(name = "margin",
           def = function(object,newdata = NULL,term = NULL, value = NULL, se = TRUE, se.method = c("delta","bootstrap"), CI = TRUE, CI.boots = FALSE,level = 0.95,eps = 1e-7,na.action = na.pass, iter = 50) standardGeneric("margin"))


#' @importFrom stats model.frame get_all_vars predict glm cov quantile pnorm qnorm
#' @export
setMethod(f = "margin",
          signature = signature(object = "twopartm"),
          definition = function(object, newdata = NULL, term = NULL, value = NULL, se = TRUE, se.method = c("delta","bootstrap"), CI = TRUE, CI.boots = FALSE,level = 0.95,eps = 1e-7,na.action = na.pass, iter = 50){

            se.method = match.arg(se.method)

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
            if(!is.null(term) &
               FALSE %in% (term %in% c(attr(object@model_part1$terms,"term.labels"),
                                       attr(object@model_part2$terms,"term.labels")))){
              stop("the term or parts of the term are not considered in the two-part model.")
            }
            if(!is.null(value) & !is.list(value)){
              stop("value should be a list of one or more named vectors.")
            }
            if(is.list(value) &
               FALSE %in% (names(value) %in% c(attr(object@model_part1$terms,"term.labels"),
                                            attr(object@model_part2$terms,"term.labels")))){
              stop("the variable(s) or parts of the variables in the value list are not considered in the two-part model.")
            }
            if(!is.numeric(level) | level > 1 | level < 0){
              stop("level should be a number between 0 and 1")
            }
            if(!is.numeric(iter) | iter <= 0){
              stop("iter should be a positive integer.")
            }


            #obtain glm models from two parts
            m1 = object@model_part1
            m2 = object@model_part2

            if(is.null(newdata)){
              newdata = object@data
            }

            #if term is NULL, ouput predictive margins for all the terms in two-part models
            if(is.null(term)){
              term = unique(c(all.vars(terms(m1))[-1],all.vars(terms(m2))[-1]))
            }

            if(length(term) == 1){
              #if one variable is considered, only input one data frame of margins for that variable
              margin_build(object, newdata, term, value[[term]],se,se.method, CI, CI.boots,level,eps,na.action,iter)

            }else{
              out = list()
              #if muliple variables are considered, return a list of data frames of margins
              for (i in 1:length(term)) {
                res = margin_build(object, newdata, term[i], value[[term[i]]],se,se.method, CI, CI.boots,level,eps,na.action,iter)
                out = append(out, list(res))

              }

              names(out) = term
              out

            }


          }
)
