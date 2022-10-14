#Method "AME"----
setGeneric(name = "AME",
           def = function(object,newdata = NULL,term = NULL, at = NULL, se = TRUE,se.method = c("delta","bootstrap"),CI = TRUE,CI.boots = FALSE,level = 0.95,eps = 1e-7,na.action = na.pass, iter = 50) standardGeneric("AME"))

#' @importFrom stats model.frame get_all_vars predict glm cov quantile pnorm qnorm
#' @export
setMethod(f = "AME",
          signature = signature(object = "twopartm"),
          definition = function(object, newdata = NULL, term = NULL, at = NULL, se = TRUE,se.method = c("delta","bootstrap"),CI = TRUE,CI.boots = FALSE,level = 0.95,eps = 1e-7,na.action = na.pass, iter = 50){

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
            if(!is.null(at) & !is.list(at)){
              stop("at should be a list of one or more named vectors.")
            }
            if(is.list(at) &
               FALSE %in% (names(at) %in% c(attr(object@model_part1$terms,"term.labels"),
                                                     attr(object@model_part2$terms,"term.labels")))){
              stop("the variable(s) or parts of the variables in the at list are not considered in the two-part model.")
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

            #if term is NULL, ouput AMEs for all the terms in two-part models
            if(is.null(term)){
              term = unique(c(all.vars(terms(m1))[-1],all.vars(terms(m2))[-1]))
            }

            if(!is.null(at)){

              #change variables in the at list which are factors in two-part model
              #to factors with the same levels in the model
              for (a in 1:length(at)) {
                if(is.factor(newdata[[names(at[a])]])){
                  at[[a]] = factor(at[[a]],levels = levels(newdata[[names(at[a])]]))
                }
              }

              #get all combinations of at list
              at_specification <- expand.grid(at)

              #get a list of new data sets for all combinations
              data_list <- list()
              for (c in 1:nrow(at_specification)) {
                data_at = newdata
                data_at[colnames(at_specification)] = at_specification[c,]
                data_list = append(data_list,list(data_at))
              }
              colnames(at_specification) <- as.vector(sapply(colnames(at_specification), function(x){paste("at(",x,")",sep = "")}))


              out = list()


              #get AMEs based on constructed new data sets
              for (i in 1:length(term)) {

                res = NULL

                for (j in seq_along(data_list)) {
                  resat =  AME_build(object, data_list[[j]], term[i], se,se.method, CI, CI.boots,level,eps,na.action,iter)
                  res = rbind(res,resat)
                }

                # for factor variables with more than 2 levels, calculate AMEs for each indicator variable
                if(is.factor(newdata[[term[i]]]) & length(levels(newdata[[term[i]]])) > 2){

                  atcol = at_specification[rep(seq_len(nrow(at_specification)), each = length(levels(newdata[[term[i]]]))-1), ]
                  res_final = cbind(res[1],atcol,res[-1])

                  colnames(res_final) = c("Variable", colnames(at_specification),colnames(res[-1]))
                }else{

                  res_final = cbind(res[1],at_specification,res[-1])

                }

                out = append(out, list(res_final))

              }

              names(out) = term

              # return AME
              if(length(term) == 1){
                out[[1]]
              }else{
                out
              }

            }else{
              # if at is NULL, calculate AME on the original data set
              out = NULL
              for (i in 1:length(term)) {

                res = AME_build(object, newdata, term[i], se,se.method, CI, CI.boots,level,eps,na.action,iter)
                out = rbind(out, res)

              }
              # return AME
              out

            }

          }
)
