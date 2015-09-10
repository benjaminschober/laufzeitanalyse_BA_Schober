# define functions needed for batchmark


batchmark = function(reg, learners, oml.task.id, measures = NULL, repls = 1L, save.models = FALSE, overwrite = FALSE, pm.opts = list()) {
  
  
  BatchExperiments:::checkExperimentRegistry(reg)
  if ("mlr" %nin% names(reg$packages))
    stop("mlr is required on the slaves, please add mlr via 'addRegistryPackages'")
  
  learners = ensureVector(learners, 1L, cl = "Learner")
  assertList(learners, types = "Learner", min.len = 1L)
  learner.ids = vcapply(learners, "[[", "id")
  if (anyDuplicated(learner.ids))
    stop("Duplicated learner ids found")
  
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
    function(id, oml.task.id, seed) {
      task = getTaskFun(oml.task.id)
      static = list(task = task)
      addProblem(reg, id, static = static, overwrite = overwrite, seed = seed)
      makeDesign(id)
    }
    ,id = paste0("t", oml.task.id), oml.task.id = oml.task.id,
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

    res = list(resample.res = resample(learner = lrn, task = static$task$task, static$task$rin,
                                       measures = measures))
    res = c(res, n = static$task$task$task.desc$size, p = sum(static$task$task$task.desc$n.feat),
            classes = length(static$task$task$task.desc$class.levels))
    res = c(res, static$task$task$task.desc$n.feat)
    if (save.models) c(list(resample = resample)) else res
  }
}

getTaskFun = function(oml.task.id) {
  configureMlr(on.learner.error = "warn") # Configures the behavior of the package
  oml.task = getOMLTask(task.id = oml.task.id)
  z = toMlr(oml.task) # convert oml task to mlr task
  task = z$mlr.task # task
  task$task.desc$id = paste0("t", oml.task.id)
  rin = z$mlr.rin
  return(list(task = task, rin = rin))
}


GetLearnerList = function(learner){
  learner.list = lapply(as.list(learner), makeLearner)
  return(learner.list)
}


library(checkmate)
library(mlr)
library(BatchExperiments)
library(OpenML)

# Create registry / delete registry
unlink("mlr_benchmark-files", recursive = TRUE)
reg = makeExperimentRegistry("UseCase_benchmark", packages = c("mlr","OpenML"))

# get the taskids i want to run 
class.tasks = listOMLTasks(type = 1)

# Subset according to criteria
sel.tasks = subset(class.tasks,
                   NumberOfInstances >= 200 & NumberOfInstances <= 100000 &
                   NumberOfFeatures <= 500 &
                   NumberOfClasses <= 50 &
                   NumberOfMissingValues == 0 &
                   estimation_procedure == "10-fold Crossvalidation" &
                   evaluation_measures == "predictive_accuracy"
                   )

# order by size:
sel.tasks$dims = sel.tasks$NumberOfInstances * sel.tasks$NumberOfFeatures
sel.tasks = sel.tasks[order(sel.tasks$dims,
                            decreasing = FALSE),]

# remove duplicates:
sel.tasks = sel.tasks[!duplicated(sel.tasks$name),]

#remove error datasets
sel.tasks = sel.tasks[-which(sel.tasks$did == 292),]
sel.tasks = sel.tasks[-which(sel.tasks$did == 1004),]
sel.tasks = sel.tasks[-which(sel.tasks$did == 183),]
sel.tasks = sel.tasks[-which(sel.tasks$did == 373),]
# Too many factors:
# additionally remove: 1047, 825

#remove duplicate artificial data
rm = setdiff(grep("fri_c", sel.tasks$name),grep("fri_c[1-9]_1000_", sel.tasks$name))
rm = c(rm)
sel.tasks = sel.tasks[-rm,]

# remove big datasets 
sel.tasks = sel.tasks[sel.tasks$dims < 10^6,]

# remove datasets with mmce < threshold (0.01?) on classif.randomForest

# finally task ids
tasks = sel.tasks$task_id[c(1,2)]

# create the learner(s) + file learner.R with all learners
learners = list(makeBaggingWrapper(makeLearner("classif.rpart"), bw.iters = 500L),
                makeLearner("classif.randomForest"),
                makeLearner("classif.rFerns"),
                makeLearner("classif.cforest"),
                makeLearner("classif.randomForestSRC")
)

# write into registry
batchmark(reg, learners, tasks,
          measures = list(mmce, ber, timetrain, timepredict),
          overwrite = TRUE, repls = 1L)


if (FALSE) {
 
  # execute
  submitJobs(reg,1:5)
  
  # Aggregated performance getter with additional info
  res_agg = reduceResultsExperiments(reg,
                                 fun = function(job, res) {
                                   r1 = as.list(res$resample.res$aggr)
                                   res$resample.res = NULL
                                   return(c(r1, res))
                                 })
  
  # Unaggregated performance getter
  # => Das brauche ich!
    res_all = reduceResults(reg, ids = getJobIds(reg), init = data.frame(), fun = function(aggr, job,         res) {
      exp.settings = job[c("id", "prob.id", "algo.id", "repl")]
      exp.settings = as.data.frame(exp.settings)
      mt = res$resample.res$measures.test
      a = cbind(exp.settings, mt)
      aggr = rbind(aggr, a)
      return(aggr)
    })
    
    result = list(agg = res_agg, all = res_all)
    save(result, file = "UseCase_Results.RData")
}

