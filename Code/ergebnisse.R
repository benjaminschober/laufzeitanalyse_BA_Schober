library(BatchExperiments)

# definiere ids die reduced werden sollen
a = seq(1,4000,16)

reg = makeExperimentRegistry("mlr_benchmark", packages = c("mlr","OpenML"))
res = reduceResultsExperiments(reg, ids = a,
                               fun = function(job, res) {
                                 r1 = as.list(res$resample.res$aggr)
                                 res$resample.res = NULL
                                 return(c(r1, res))
                               })
res
# -- New from Bernd
# Get Aggregated Results
if (FALSE) {
  res = reduceResultsExperiments(reg, ids = 1,
                                 fun = function(job, res) {
                                   r1 = res$resample.res$aggr
                                   res$resample.res = NULL
                                   return(r1)
                                 })
  res = reduceResults(reg, ids = getJobIds(reg), init = data.frame(), fun = function(aggr, job, res) {
    exp.settings = job[c("id", "prob.id", "algo.id", "repl")]
    exp.settings = as.data.frame(exp.settings)
    mt = res$resample.res$measures.test
    a = cbind(exp.settings, mt)
    aggr = rbind(aggr, a)
    return(aggr)
  })
  res
}