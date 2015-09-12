#' @export
makeRLearner.classif.wsrf = function() {
  makeRLearnerClassif(
    cl = "classif.wsrf",
    package = "wsrf",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntrees", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeLogicalLearnerParam(id = "weights", default = TRUE),
      makeFunctionLearnerParam(id = "na.action", default = na.fail),
      makeLogicalLearnerParam(id = "parallel", default = TRUE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "ordered", "prob"),
    name = "Forest of Weighted Subspace Decision Trees",
    short.name = "wsrf"
    )
}

#' @export
trainLearner.classif.wsrf = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  data = getTaskData(.task, .subset)
  wsrf::wsrf(formula = f, data = data, ...)

}

#' @export
predictLearner.classif.wsrf = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "response", "prob")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}