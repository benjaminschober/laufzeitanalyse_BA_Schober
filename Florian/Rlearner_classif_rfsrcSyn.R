#' @export
makeRLearner.classif.rfsrcSyn = function() {
  makeRLearnerClassif(
    cl = "classif.rfsrcSyn",
    package = "randomForestSRC",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", lower = 1L, default = 1L),
      makeIntegerLearnerParam(id = "nsplit", default = 0L),
      makeDiscreteLearnerParam(id = "na.action", default = "na.impute",
                               values = c("na.omit", "na.impute"), when = "both"),
      makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "min.node", lower = 1L, default = 3L),
      makeLogicalLearnerParam(id = "use.org.features", default = TRUE),
      makeIntegerVectorLearnerParam(id = "mtryseq", lower = 1L, default = c(1:10,20,30,50,100)),
      makeLogicalLearnerParam(id = "forest", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "membership", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "statistics", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "fast.restore", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(na.action = "na.impute"),
    properties = c("missings", "numerics", "factors", "prob", "twoclass", "multiclass"),
    name = "Random Forest",
    short.name = "rfsrcSyn",
    note = "'na.action' has been set to 'na.impute' by default to allow missing data support"
  )
}

#' @export
trainLearner.classif.rfsrcSyn = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  randomForestSRC::rfsrcSyn(f, data = getTaskData(.task, .subset, recode.target = "drop.levels"),                             importance = "none", proximity = FALSE, forest = TRUE, ...)
}

#' @export
predictLearner.classif.rfsrcSyn = function(.learner, .model, .newdata, ...) {
  p = rfsrcSyn(object = .model$learner.model, newdata = .newdata, ...)
  if(.learner$predict.type == "prob"){
    return(p$predicted)  
  } else {
    return(factor(colnames(p$predicted)[apply(p$predicted, 1, which.max)]))
  }
 
}
