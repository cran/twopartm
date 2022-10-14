
# model object----
setOldClass(c("glm", "lm"))
setOldClass("family")
setClassUnion("glm.model", c("glm", "lm"))

#class of two-part model
#' @export
setClass(Class = "twopartm",
         slots = c(formula_part1 = "formula",
                      formula_part2 = "formula",
                      data = "data.frame",
                      n = "numeric",
                      n_part1 = "numeric",
                      n_part2 = "numeric",
                      data_model1 = "data.frame",
                      data_model2 = "data.frame",
                      model_part1 = "glm.model",
                      model_part2 = "glm.model",
                      link_part1 = "character",
                      family_part2 = "family",
                      weights = "ANY",
                      fitted = "numeric",
                      residuals = "numeric",
                      loglik = "numeric",
                      y = "ANY"),

         prototype = list(formula_part1 = .~.,
                          formula_part2 = .~.,
                          data = data.frame(),
                          n = numeric(),
                          n_part1 = numeric(),
                          n_part2 = numeric(),
                          data_model1 = data.frame(),
                          data_model2 = data.frame(),
                          link_part1 = character(),
                          weights = numeric(),
                          fitted = numeric(),
                          residuals = numeric(),
                          loglik = numeric())
         )





