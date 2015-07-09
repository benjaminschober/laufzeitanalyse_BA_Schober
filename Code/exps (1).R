library(BatchExperiments)
library(OpenML)
library(mlr)
setOMLConfig(conf = list(cachedir="~/.openml/cache"))

# list all classification tasks
tasks = listOMLTasks(type = 1)
# subset row number 

sel.tasks = subset(tasks, NumberOfInstances <= 1000 & NumberOfSymbolicFeatures > 1)
# only take the first 5
sel.tasks = head(sel.tasks,5L)

oml.task.ids = sel.tasks$task_id

# paste problem ids
problem.ids = paste0("t", oml.task.ids)

# list of measures we want to use
measures = list(mmce, ber, timetrain, timepredict)

# vector of learners we want to use
learners = c("classif.randomForest")

# delete the file . ATTENTION
unlink("time_benchmark-files", recursive = TRUE)


reg =  makeExperimentRegistry(id = "time_benchmark",
                               packages = c("mlr", "OpenML") # packages we need
)

for (i in seq_along(oml.task.ids)) { # for all task ids we choose
  addProblem(reg, id = problem.ids[i], # add an algorithm to this problem and store it
    static = list(
      oml.task.id = oml.task.ids[i]  # is not dependent on parameters
    )
  )
}




addAlgorithm(reg, # registry made 
             "run-learner", # name of algorithm
             fun = function(static, dynamic, learner) {
                library(mlr)
                configureMlr(on.learner.error = "warn") # Configures the behavior of the package
                oml.task = getOMLTask(task.id = static$oml.task.id) # fetches oml task
                z = toMlr(oml.task) # convert oml task to mlr task
                task = z$mlr.task # task
                
                
                #rin = z$mlr.rin # resampling strategy
                rin = makeResampleDesc("CV", iters=5)
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
                task =  removeConstantFeatures(task, perc = 0.1) # perc= the percentage of a feature value 
                                                                   # that must differ from the mode value 
                
                tdesc = task$task.desc
        
                # parameter tuning
                ps = getLearnerParam(learner, 3)
                ctrl = makeTuneControlGrid()
                inner = makeResampleDesc("CV", iters = 3L)
                outer = rin
                lrn = makeTuneWrapper(learner, inner, par.set = ps, 
                                      control = ctrl, show.info = FALSE)
                r = resample(lrn, task = task, resampling = outer, extract = getTuneResult,
                             measures = measures, show.info = FALSE)
                res = list(resample.res = r, n = tdesc$size, p = sum(tdesc$n.feat))
                res = c(res, tdesc$n.feat)
                return(res)
})



ades = makeDesign("run-learner", exhaustive = list(learner = learners))

addExperiments(reg, algo.des = ades)

batchExport(reg, measures = measures)

# x = testJob(reg, 1L, external = FALSE)

submitJobs(reg)


getLearnerParam("classif.lda")
getLearnerParam("classif.cforest")
getParamSet(makeLearner("classif.IBk"))


