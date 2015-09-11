#' @export
makeRLearner.classif.rfsrcSyn = function() {
  makeRLearnerClassif(
    cl = "classif.rfsrcSyn",
    package = "randomForestSRC",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerVectorLearnerParam(id = "mtrySeq"),
      makeIntegerLearnerParam(id = "nodesize", default = 5L, lower = 1L),
      makeIntegerVectorLearnerParam(id = "nodesizeSeq", default = c(1:10, 20, 30, 50, 100)),
      makeNumericLearnerParam(id = "nsplit", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "min.node", default = 3L, ),
      makeLogicalLearnerParam(id = "use.org.features", default = TRUE),
      makeDiscreteLearnerParam(id = "na.action", default = "na.impute",
        values = c("na.omit", "na.impute"), when = "both"),
      makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
      makeNumericVectorLearnerParam(id = "xwar.wt", lower = 0),
      makeLogicalLearnerParam(id = "forest", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "membership", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "statistics", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "fast.restore", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "ordered", "prob"),
    name = "Synthetic Random Forest",
    short.name = "rfsrcSyn",
    note = "na.action' has been set to 'na.impute' by default to allow missing data support"
    )
}

#' @export
trainLearner.classif.rfsrcSyn = function(.learner, .task, .subset, .weights = NULL, ...) {
  df = getTaskData(.task, .subset)
  f = getTaskFormula(.task)
  c(list(formula = f, data = df), list(...))
}

#' @export
predictLearner.classif.rfsrcSyn = function(.learner, .model, .newdata, ...) {
  args = .model$learner.model
  args$newdata = .newdata
  args$verbose = FALSE
  p = do.call(randomForestSRC::rfsrcSyn, args)$predicted
  if(.learner$predict.type == "response"){
    max.id = apply(p, MAR = 1, which.max)
    p = factor(colnames(p)[max.id])
  }
  return(p)
}