#hallo
batchmark = function(reg, learners, oml.task.id, resamplings, measures = NULL, repls = 1L, save.models = FALSE, overwrite = FALSE, pm.opts = list()) {
 
  BatchExperiments:::checkExperimentRegistry(reg)
  if ("mlr" %nin% names(reg$packages))
    stop("mlr is required on the slaves, please add mlr via 'addRegistryPackages'")
  
  learners = ensureVector(learners, 1L, cl = "Learner")
  assertList(learners, types = "Learner", min.len = 1L)
  learner.ids = vcapply(learners, "[[", "id")
  if (anyDuplicated(learner.ids))
    stop("Duplicated learner ids found")
  
  resamplings = ensureVector(resamplings, length(tasks), "ResampleDesc")
  if (length(resamplings) == 1)
    resamplings=  rep(resamplings,length(tasks))
  assertList(resamplings, "ResampleDesc", len = length(tasks))
  
  if (is.null(measures)) {
    measures = default.measures(tasks[[1L]])
  } else {
    measures = ensureVector(measures, 1L, "Measure")
    assertList(measures, types = "Measure", min.len = 1L)
  }
  
  assertCount(repls)
  assertFlag(save.models)
  assertFlag(overwrite)
  assertList(pm.opts, names = "named")

  
  # generate problems
  pdes = Map(
    function(id, oml.task.id, rdesc, seed) {
      task = getTaskFun(oml.task.id)
      static = list(rdesc = rdesc, task = task)
      addProblem(reg, id, static = static, overwrite = overwrite, seed = seed)
      makeDesign(id)
    }
    ,id = paste0("t",oml.task.id), oml.task.id = oml.task.id, rdesc = resamplings,
    seed = reg$seed +  seq_along(tasks)
  )
  
  # generate algos
  ades = Map(function(id, learner) {
    apply.fun = getAlgoFun(learner, measures, save.models, pm.opts)
    addAlgorithm(reg, id, apply.fun, overwrite = overwrite)
    makeDesign(id)
  }, id = learner.ids, learner = learners)
  
  # add experiments
  addExperiments(reg, prob.designs = pdes, algo.designs = ades, repls = repls, skip.defined = overwrite)
}

getAlgoFun = function(lrn, measures, save.models, pm.opts) {
  force(lrn)
  force(measures)
  force(save.models)
  force(pm.opts)
  
  function(job, static, dynamic) {
    if (length(pm.opts) > 0L) {
      do.call(parallelStart, pm.opts)
      on.exit(parallelStop())
    }
#   rdesc = makeResampleDesc("CV",iters=10)

    #runTaskMlr(task = oml.task, learner = lrn, remove.const.feats = TRUE)
    res = list(resample.res = resample(learner = lrn, task = static$task, static$rdesc,
                                       measures =    measures))
    res = c(res, n = static$task$task.desc$size)
    if (save.models) c(list(resample = resample)) else res
  }
}

getTaskFun = function(oml.task.id) {
  configureMlr(on.learner.error = "warn") # Configures the behavior of the package
  oml.task = getOMLTask(task.id = oml.task.id)
  z = toMlr(oml.task) # convert oml task to mlr task
  task = z$mlr.task # task
  target = getTaskTargetNames(task) #get the name(s) of the target column(s)
  
  # solve errors
  
  # impute missing values
  i = impute(data = getTaskData(task), 
             target = target,
             classes = list(numeric = imputeMedian(), 
                            factor = imputeConstant("_missing_")
             )
  )
  # remove constant features
  task = makeClassifTask(data = i$data, target = target) # Create classification task
  task =  removeConstantFeatures(task, perc = 0.1) 
  return(task)
}

if (FALSE) {
  library(checkmate)
  library(mlr)
  library(BatchExperiments)
  library(OpenML)
  unlink("mlr_benchmark-files", recursive = TRUE)
  reg = makeExperimentRegistry("mlr_benchmark", packages = "mlr")
}



### For OpenMl

if (FALSE) {
  tasks = c(4,5)  
#   2nd learner
#   ps = getLearnerParam("classif.rpart", 5)
#   ctrl = makeTuneControlGrid()
#   inner = makeResampleDesc("CV", iters = 3L)
#   lrn = makeTuneWrapper(makeLearner("classif.rpart"),
#                         inner, par.set = ps, 
#                         control = ctrl, show.info = FALSE)
#   
  learners = list(makeLearner("classif.rpart"))
  resamplings = list(makeResampleDesc("CV", iters = 10))
  batchmark(reg, learners, tasks, resamplings, measures = list(mmce, timetrain), overwrite = TRUE, repls = 1L)
  submitJobs(reg)
}

# -- Old:
# Get Aggregated Results
if (FALSE) {
  res = reduceResultsExperiments(reg, ids = getJobIds(reg),
                                 fun = function(job, res) {
                                   r1 = res$resample.res$aggr
                                   res$resample.res = NULL
                                   return(r1)
                                 })
}

# -- New from Bernd
# Get Aggregated Results
if (FALSE) {
  res = reduceResultsExperiments(reg, ids = getJobIds(reg),
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



