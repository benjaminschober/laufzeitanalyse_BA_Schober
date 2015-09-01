batchmark = function(reg, learners, oml.task.id, measures = NULL, repls = 1L, save.models = FALSE, overwrite = FALSE, pm.opts = list()) {
  
  
  
  task.list = lapply(as.list(oml.task.id),getOMLTask)
  resamplings = lapply(task.list, toMlr)
  for(i in 1: length(oml.task.id)){
    resamplings[[i]] <- resamplings[[i]]$mlr.rin$desc
  }
  
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
                                       measures = measures))
    res = c(res, n = static$task$task.desc$size, p = sum(static$task$task.desc$n.feat),
            classes = length(static$task$task.desc$class.levels), static$rdesc )
    res = c(res, static$task$task.desc$n.feat)
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
  task =  removeConstantFeatures(task, perc = 0.01)
  return(task)
}


GetLearnerList = function(learner, tune = FALSE){
  
  learner.list = lapply(as.list(learner), makeLearner)
  
  if(tune == TRUE){
    learner.list.tuned = lapply(as.list(learner), GettunedLearner)
    learner.list = c(learner.list, learner.list.tuned)
  }
  return(learner.list)
}

GettunedLearner = function(learner){
  ps = getLearnerParam(learner, 5)
  ctrl = makeTuneControlGrid()
  inner = makeResampleDesc("CV", iters = 3L)
  lrn = makeTuneWrapper(makeLearner(learner),
                        inner, par.set = ps,
                        control = ctrl, show.info = FALSE)
  return(lrn)
}
library(checkmate)
library(mlr)
library(BatchExperiments)
library(OpenML)

#unlink("mlr_benchmark-files", recursive = TRUE)
#reg = makeExperimentRegistry("mlr_benchmark", packages = c("mlr","OpenML","farff","rpart"))

# get the taskids i want to run

class.tasks = listOMLTasks(type = 1)

sel.tasks = subset(class.tasks, NumberOfInstances <= 100000 &
                     NumberOfInstances >= 200 &
                     NumberOfFeatures <= 500 &
                     NumberOfClasses <= 50)
sel.tasks = sel.tasks[order(sel.tasks$NumberOfFeatures * sel.tasks$NumberOfInstances, decreasing = FALSE),]

#remove error datasets
sel.tasks = sel.tasks[-which(sel.tasks$did == 292),]

tasks = sel.tasks$task_id[361:370]

# create the learner

learners = list(makeLearner("classif.randomForest"),
                makeLearner("classif.cforest"),
                #makeLearner("classif.boosting"),
                makeLearner("classif.gbm"),
                makeLearner("classif.lda"),
                makeLearner("classif.mda"),
                makeLearner("classif.rda"),
                makeLearner("classif.qda"),
                makeLearner("classif.ctree"),
                makeLearner("classif.multinom"),
                makeLearner("classif.rpart"),
                makeLearner("classif.kknn"),
                makeLearner("classif.nnet"),
                makeLearner("classif.randomForestSRC"),
                makeLearner("classif.ksvm"),
                makeLearner("classif.lssvm"),
                makeLearner("classif.svm"))

# write in registry

batchmark(reg, learners, tasks,
          measures = list(mmce, ber, timetrain, timepredict, timeboth),
          overwrite = TRUE, repls = 1L)



