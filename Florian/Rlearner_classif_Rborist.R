#' @export
makeRLearner.classif.Rborist = function() {
  makeRLearnerClassif(
    cl = "classif.Rborist",
    package = "Rborist",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "nTree", default = 500L, lower = 1L),
      makeLogicalLearnerParam(id = "withRepl", default = TRUE),
      makeNumericLearnerParam(id = "predProb", upper = 1L),
      makeNumericVectorLearnerParam(id = "predWeight",lower = 0),
      makeIntegerLearnerParam(id = "nSamp", lower = 1L),
      makeIntegerLearnerParam(id = "minNode", lower = 1L, default = 2L),
      makeIntegerLearnerParam(id = "nLevel", lower = 0, default = 0),
      makeNumericLearnerParam(id = "minInfo", lower = 0, default = 0.01),
      makeNumericVectorLearnerParam(id = "sampleWeight", lower = 0L),
      makeLogicalLearnerParam(id = "quantiles", requires = expression(!is.null(quantVec))),
      makeNumericVectorLearnerParam(id = "quantVec", lower = 0L)
    ),
    properties = c("missings", "numerics", "factors", "prob", "twoclass", "multiclass"),
    name = "Random Forest",
    short.name = "Rborist",
    note = ""
  )
}

#' @export
trainLearner.classif.Rborist= function(.learner, .task, .subset, .weights = NULL,  ...) {
  df = getTaskData(.task, .subset, target.extra = TRUE)
  Rborist::Rborist(x = df$data,y = df$target, ...)
}

#' @export
# FIXME: predict not working yet
predictLearner.classif.Rborist = function(.learner, .model, .newdata, ...) {
  df = .newdata[, !names(.newdata) %in% c(.model$task.desc$target)]
  p = as.factor(predict(object = .model$learner.model, x = df, ...)$yPred)
}