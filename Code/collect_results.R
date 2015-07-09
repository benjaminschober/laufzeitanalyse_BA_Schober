library(BatchExperiments)
library(OpenML)
library(mlr)


reg = loadRegistry("time_benchmark-files")

ids = getJobIds(reg)

res = reduceResultsExperiments(reg, ids = ids,
  fun = function(job, res) {
    r1 = as.list(res$resample.res$aggr)
    res$resample.res = NULL
    c(r1, res)
  }

)

res
