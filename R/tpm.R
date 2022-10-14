
#Method "tpm"----
#fit two-part models
setGeneric(name = "tpm",
           def = function(formula_part1,formula_part2 = NULL,data,link_part1 = c("logit", "probit", "cloglog", "cauchit", "log"),family_part2 = gaussian(),weights = NULL,...) standardGeneric("tpm"))

#' @import methods
#' @import data.table
#' @importFrom stats model.frame get_all_vars glm predict logLik as.formula binomial gaussian na.pass terms
#' @export
setMethod(f = "tpm",
          signature = signature(formula_part1 = "formula"),
          definition = function(formula_part1,formula_part2 = NULL,data,link_part1 = c("logit", "probit", "cloglog", "cauchit", "log"), family_part2 = gaussian(), weights = NULL,...){


            #check inputs
            if(is.null(formula_part2)){
              formula_part2 = formula_part1
            }else if(as.character(formula_part1)[2] != as.character(formula_part2)[2]){
              stop("Dependent variables in two formulas should be the same.")
            }


            if (is(weights, "formula")) {
              weights <- model.frame(weights, data = data, na.action = NULL)[,1]
            }

            link_part1 <- match.arg(link_part1)

            #obtain data sets to fit two-part models
            data = as.data.frame(data)
            data_model1 = model.frame(formula_part1,  data = data)
            data_model1[["weights"]] = weights
            idx_1 = which(colnames(get_all_vars(formula_part1,  data = data))%in% colnames(model.frame(formula_part1,  data = data)) == FALSE)
            colname_1 = colnames(data_model1)
            data_model1 = cbind(data_model1,get_all_vars(formula_part1,  data = data)[,idx_1])
            colnames(data_model1) = c(colname_1,colnames(get_all_vars(formula_part1,  data = data))[idx_1])

            data_model2_all = model.frame(formula_part2,  data = data)
            data_model2_all[["weights"]] = weights
            idx_2 = which(colnames(get_all_vars(formula_part2,  data = data))%in% colnames(model.frame(formula_part2,  data = data)) == FALSE)
            colname_2 = colnames(data_model2_all)
            data_model2_all = cbind(data_model2_all,get_all_vars(formula_part2,  data = data)[,idx_2])
            colnames(data_model2_all) = c(colname_2,colnames(get_all_vars(formula_part2,  data = data))[idx_2])

            #obtain the dependent variable's name
            y = as.character(formula_part1)[2]

            #obtain the indicator variable about zero or non-zero as the dependent variable of the first-part model
            data_model1$nonzero = as.numeric(data_model1[,y] != 0)
            data_model1 = data_model1[,!names(data_model1) %in% y]

            #update the model formula for part-1
            formula1_update = as.formula(paste("nonzero~",as.character(formula_part1)[3]))

            #fit the first-part model
            if(is.null(weights)){
              model1 <- glm(formula1_update,data = data_model1, family = binomial(link = link_part1),...)
            }else{
              model1 <- glm(formula1_update,data = data_model1, family = binomial(link = link_part1), weights = weights,...)
            }

            #get the family function if the user input is a function class.
            if(is.function(family_part2)){
              family_part2 = family_part2()
            }

            #obtain the subset of data for which reponses are non-zero
            data_model2 = subset(data_model2_all,data_model1$nonzero == 1)

            #fit the second-part model
            if(is.null(weights)){
              model2 <- glm(formula_part2, data = data_model2, family = family_part2,...)
            }else{
              model2 <- glm(formula_part2, data = data_model2, family = family_part2, weights = weights,...)
            }

            #change the models' call
            model1$call$formula = formula1_update
            model1$call$family = call("binomial",link = link_part1)
            model1$call$data = match.call()$data
            model2$call$formula = formula_part2
            model2$call$family = call(family_part2$family,link = family_part2$link)
            model2$call$data = match.call()$data

            #obtain the fitted values and residuals
            fitted = stats::predict(model1,type = "response")*stats::predict(model2, newdata = data_model2_all, type = "response")
            residuals = as.numeric(data_model2_all[,y] - fitted)

            #save glm models for two-parts
            data_model1 = model1$model
            data_model2 = model2$model

            #obtain log-likelihood value of the model
            loglik = stats::logLik(model1)[1]+stats::logLik(model2)[1]

            #obtain samples sizes for the whole model, and two-part models seperately
            n = if(is.null(weights)){nrow(data)}else{nrow(data[weights>0,])}
            n_part1 = if(is.null(weights)){nrow(data_model1)}else{nrow(data_model1[data_model1$`(weights)`>0,])}
            n_part2 = if(is.null(weights)){nrow(data_model2)}else{nrow(data_model2[data_model2$`(weights)`>0,])}


            #build a new two-part model object with fitted models
            tpmodel = new("twopartm",formula_part1 = formula_part1, formula_part2 = formula_part2,
                          data = data, n = n, n_part1 = n_part1, n_part2 = n_part2,
                          data_model1 = data_model1, data_model2 = data_model2,
                          model_part1 = model1, model_part2 = model2,
                          link_part1 = link_part1, family_part2 = family_part2, weights = weights,
                          fitted = fitted, residuals = residuals, loglik = loglik, y = data_model2_all[,y])

            #return the two-part model object
            tpmodel
          }
)
