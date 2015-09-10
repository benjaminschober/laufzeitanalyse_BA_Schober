# Coerce a Registry as Used in UseCase_RandomForests to a BenchmarkResult(as in mlr)

# List of learners in the UseCase
lrns = list(makeBaggingWrapper(makeLearner("classif.rpart"), bw.iters = 500L),
            makeLearner("classif.randomForest"),
            makeLearner("classif.rFerns"),
            makeLearner("classif.cforest"),
            makeLearner("classif.randomForestSRC")
)

# List of measures in the UseCase
measures = list(mmce, ber, timetrain, timepredict)

RegistryToBMR = function(reg, learners, measures){

res = reduceResultsList(reg, fun = function(job, res){
  tsk = res$resample.res$task.id
  lrn = res$resample.res$learner.id
  dat = res$resample.res
  lst = eval(parse(text = paste0("list(" , tsk, "= list(", lrn, "= dat))")))
})

# Coerce to format list of tasks that contain all learners for the task. 
r = list()
for(i in 1:length(res)){
  tnm = names(res[[i]])
  lnm = names(res[[i]][[tnm]])
  r[[tnm]][[lnm]] = res[[i]][[tnm]][[lnm]]
}

# Coerce to BMR
newBMR = list(results = r, measures = meas, learners = lrns)
class(newBMR) = "BenchmarkResult"

# Return BMR
return(newBMR)
}

# Call:
# RegistryToBMR(reg, lrn, measures)



## Old Stuff for debugging.
if(FALSE){
  # Create BMR to see Structure
  tsk = list(iris.task, sonar.task)
  lrns = list(makeLearner("classif.rpart"),
            makeLearner("classif.nnet"))
  rdesc = makeResampleDesc("CV", iters = 10)
  meas = list(mmce, ber, timetrain, timepredict)
  bmr = benchmark(lrns, tsk, rdesc, meas)

  # Inspect structure
  str(bmr, max.level = 1)
  bmr$measures
  bmr$learners
  str(bmr$results, max.level = 1)
  str(bmr$results[[1]], max.level = 1)
}
