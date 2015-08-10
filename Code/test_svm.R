

# funktioniert nicht :


oml.task = getOMLTask(task.id = 21) 
z = toMlr(oml.task) # convert oml task to mlr task
task = z$mlr.task # task
target = getTaskTargetNames(task) #get the name(s) of the target column(s)
i = impute(data = getTaskData(task), 
           target = target,
           classes = list(numeric = imputeMedian(), 
                          factor = imputeConstant("_missing_")
           )
)
task = makeClassifTask(data = i$data, target = target) # Create classification task
task =  removeConstantFeatures(task, perc = 0.01)
lrn = makeLearner("classif.svm")
rdesc = makeResampleDesc("CV", iters = 10L)
rinst = makeResampleInstance(desc = rdesc, task = task)
resample(learner = lrn, task = task, resampling = rinst)

# funktioniert 

oml.task = getOMLTask(task.id = 18) 
z = toMlr(oml.task) # convert oml task to mlr task
task = z$mlr.task # task
target = getTaskTargetNames(task) #get the name(s) of the target column(s)
i = impute(data = getTaskData(task), 
           target = target,
           classes = list(numeric = imputeMedian(), 
                          factor = imputeConstant("_missing_")
           )
)
task = makeClassifTask(data = i$data, target = target) # Create classification task
task =  removeConstantFeatures(task, perc = 0.01)
lrn = makeLearner("classif.svm")
rdesc = makeResampleDesc("CV", iters = 10L)
rinst = makeResampleInstance(desc = rdesc, task = task)
resample(learner = lrn, task = task, resampling = rinst)



