library(mlr)
library(Rborist)

source("./tests/testthat/helper_helpers.R")
source("./tests/testthat/helper_objects.R")
source("./tests/testthat/helper_zzz.R")


i = sample(c(1,1,2), 150, replace = TRUE)
test_lrn = function(lrn){
  lrn = makeLearner("classif.rfsrcSyn")
  measure = acc
  tasks = list(iris.task,pid.task,sonar.task)
  mod = train(lrn, task = tasks[[1]], subset = i == 1)
  predict(mod, task = tasks[[1]], subset = i == 2)

  rdesc = makeResampleDesc("CV", iter = 5)
  benchmark(lrn, tasks, rdesc, measure)
}

test_lrn("classif.ranger")
test_lrn("classif.rrf") # works
test_lrn("classif.rfsrcSyn")
test_lrn("classif.rfsrc")




a = RRF(getTaskFormula(iris.task), data = getTaskData(iris.task), keep.forest = TRUE)
library(randomForestSRC)
b = rfsrc(getTaskFormula(iris.task), data = getTaskData(iris.task), forest = TRUE)
  c = predict(a, iris, type = "prob")
library(randomForest)
d = randomForest::randomForest(getTaskFormula(iris.task), data = getTaskData(iris.task))
str(c)



