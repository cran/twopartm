
#obtain bootstrapped models for two-part model
boots_model <- function(object){

  #get bootstrap sample data
  samp <- sample(seq_len(nrow(object@data)), nrow(object@data), TRUE)
  bootdata<- object@data[samp,]
  bdata = as.data.frame(bootdata)

  #refit the two-part models for bootstrap data

  #obtain data sets to fit two-part models
  data_model1 = model.frame(object@formula_part1,  data = bdata)
  data_model1[["weights"]] = object@weights[samp]
  colname_1 = colnames(data_model1)
  idx_1 = which(colnames(get_all_vars(object@formula_part1,  data = bdata))%in% colnames(model.frame(object@formula_part1,  data = bdata)) == FALSE)
  data_model1 = cbind(data_model1,get_all_vars(object@formula_part1,  data = bdata)[,idx_1])
  colnames(data_model1) = c(colname_1,colnames(get_all_vars(object@formula_part1,  data = bdata))[idx_1])

  data_model2_all = model.frame(object@formula_part2,  data = bdata)
  data_model2_all[["weights"]] = object@weights[samp]
  idx_2 = which(colnames(get_all_vars(object@formula_part2,  data = bdata))%in% colnames(model.frame(object@formula_part2,  data = bdata)) == FALSE)
  colname_2 = colnames(data_model2_all)
  data_model2_all = cbind(data_model2_all,get_all_vars(object@formula_part2,  data = bdata)[,idx_2])
  colnames(data_model2_all) = c(colname_2,colnames(get_all_vars(object@formula_part2,  data = bdata))[idx_2])

  #obtain the dependent variable's name
  y = as.character(object@formula_part1)[2]

  #obtain the indicator variable about zero or non-zero as the dependent variable of the first-part model
  data_model1$nonzero = as.numeric(data_model1[,y] != 0)
  data_model1 = data_model1[,!names(data_model1) %in% y]

  #update the model formula for part-1
  formula1_update = as.formula(paste("nonzero~",as.character(object@formula_part1)[3]))

  #fit the first-part model
  if(is.null(object@weights)){
    model1 <- glm(formula1_update,data = data_model1, family = binomial(link = object@link_part1))
  }else{
    model1 <- glm(formula1_update,data = data_model1, family = binomial(link = object@link_part1), weights = object@weights)
  }

  #obtain the subset of data for which reponses are non-zero
  data_model2 = subset(data_model2_all,data_model1$nonzero == 1)

  #fit the second-part model
  if(is.null(object@weights)){
    model2 <- glm(object@formula_part2, data = data_model2, family = object@family_part2)
  }else{
    model2 <- glm(object@formula_part2, data = data_model2, family = object@family_part2, weights = object@weights)
  }

  #return the bootrapped models
  return(list(model1 = model1,model2 = model2))

}
