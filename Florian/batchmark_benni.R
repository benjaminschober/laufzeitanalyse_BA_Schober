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
  
  #problem.ids = paste0("t", oml.task.id)
  
  # generate problems
  pdes = Map(
    function(id, oml.task.id, rdesc, seed) {
      static = list(rdesc = rdesc, oml.task.id = oml.task.id)
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
    configureMlr(on.learner.error = "warn") # Configures the behavior of the package
    oml.task = getOMLTask(task.id = static$oml.task.id)
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
    rdesc = makeResampleDesc("CV",iters=10)

    #runTaskMlr(task = oml.task, learner = lrn, remove.const.feats = TRUE)
    res = list(resample.res = resample(learner = lrn, task = static$task, static$rdesc,
                                       measures =    measures))
    res = c(res, n = static$task$task.desc$size)
    if (save.models) c(list(resample = resample)) else res
  }
}

if (FALSE) {
  #For MLR
  library(checkmate)
  library(mlr)
  library(BatchExperiments)
  library(OpenML)
  unlink("mlr_benchmark-files", recursive = TRUE)
  reg = makeExperimentRegistry("mlr_benchmark", packages = "mlr")
  tasks = list(iris.task, sonar.task)
  learners = list(makeLearner("classif.rpart"), makeLearner("classif.randomForest"))
  resamplings = list(makeResampleDesc("CV", iters = 10))
  
  batchmark(reg, learners, tasks, resamplings, measures = list(mmce, timetrain), overwrite = TRUE, repls = 1L)
  submitJobs(reg, getJobIds(reg))
  testJob(reg, 1, external = FALSE)
  reduceResultsExperiments(reg)
}



### For OpenMl

if (FALSE) {
  tasks = c(4,5)
  
  # learners
  ps = getLearnerParam("classif.rpart", 5)
  ctrl = makeTuneControlGrid()
  inner = makeResampleDesc("CV", iters = 3L)
  lrn = makeTuneWrapper(makeLearner("classif.rpart"),
                        inner, par.set = ps, 
                        control = ctrl, show.info = FALSE)
  
  learners = list(makeLearner("classif.rpart"))
  resamplings = list(makeResampleDesc("CV", iters = 10))
  tasks = c(4,5)
  batchmark(reg, learners, tasks, resamplings, measures = list(mmce, timetrain), overwrite = TRUE, repls = 1L)
  submitJobs(reg)
}


# Get Aggregated Results
if (FALSE) {
  res = reduceResultsExperiments(reg, ids = getJobIds(reg),
                                 fun = function(job, res) {
                                   r1 = res$resample.res$aggr
                                   res$resample.res = NULL
                                   return(r1)
                                 })
}



