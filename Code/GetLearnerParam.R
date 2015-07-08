getLearnerParam = function(learner, tune.length){
  
  # classifier 
  
  if(learner == "classif.boosting"){
    ps = makeParamSet(
      makeDiscreteParam("mfinal",
                       floor((1L:tune.length) * 50L)),
      makeIntegerParam("maxdepth",
                        lower = 1L,
                        upper = tune.length))
  }
  
  if(learner == "classif.cforest"){
    ps = makeParamSet(
      makeIntegerParam("mtry",          
                       lower = ceiling(0.03*sum(tdesc$n.feat)),
                       upper = floor(0.3*sum(tdesc$n.feat)))) 
  }
  
  if(learner == "classif.ctree"){
    ps = makeParamSet(
      makeDiscreteParam("mincriterion",          
                        seq(from = 0.99, to = 0.01, length = tune.length)))
  }
  
  if(learner == "classif.gbm"){
    ps = makeParamSet(
      makeIntegerParam("interaction.depth",          
                       lower = 1L,
                       upper = tune.length),
      makeDiscreteParam("n.trees",          
                        floor((1L:tune.length) * 50L)))

  }
  
  if(learner == "classif.glmnet"){
    ps = makeParamSet(
      makeDiscreteParam("alpha",
                        seq(0.1, 1, length = tune.length)),
      makeDicreteParam("lambda",
                       ))
    }
  
  if(learner == "classif.IBk"){  # nicht in caret
    ps = makeParamSet(
      makeDiscreteParam("K",
                        lower = 1L,
                        upper = tune.length))
    }
  
  if(learner == "classif.J48"){
    ps = makeParamSet(
      makeDiscreteParam("C",
                        0.25))
  }
  
  if(learner == "classif.JRip"){
     ps = makeParamSet(
      makeIntegerParam("N",
                       lower = 1,
                       upper = tune.length))
  }
  
  if(learner == "classif.kknn"){
     ps = makeParamSet(
       makeDiscreteParam("k",
                         (5:((2 * tune.length) + 4))[(5:((2 * tune.length) + 4))%%2 > 0]))
  
  }  
  
  if(learner == "classif.ksvm"){
    ps = makeParamSet(
      makeDiscreteParam("kernel",
                        c("polydot")),
      makeDiscreteParam("degree",
                        seq(1, min(tune.length, 3))),
      makeDiscreteParam("scale",
                        10^((1:tune.length) - 4)))
  }  
  
  if(learner == "classif.lda"){
    ps = makeParamSet(
      makeDiscreteParam("tol",
                        c(0.0001)))
  }  
  
  if(learner == "classif.lssvm"){
    ps = makeParamSet(
      makeDiscreteParam("kernel",
                        c("polydot")),
      makeDiscreteParam("degree",
                        seq(1, min(tune.length, 3))),
      makeDiscreteParam("scale",
                        10^((1:tune.length) - 4)))  
  }  
  
  if(learner == "classif.mda"){
    ps = makeParamSet(
      makeDiscreteParam("subcalsses",
                        (1:tune.length) + 1))
  }  
  
  if(learner == "classif.multinom"){
    ps = makeParamSet(
      makeDiscreteParam("decay",
                        c(0, 10^seq(-1, -4, length = tune.length - 1))))
  }  
  
  if(learner == "classif.naiveBayes"){
    ps = makeParamSet(
      makeDiscreteParam("laplace",
                        0))
  }  
  
  if(learner == "classif.nnet"){
    ps = makeParamSet(
      makeDiscreteParam("size",
                        ((1:tune.length) * 2) - 1),
      makeDiscreteParam("decay",
                        c(0, 10^seq(-1,-4))))
  }  
  
  if(learner == "classif.OneR"){
    ps = makeParamSet(
      makeDiscreteParam("B",
                        6))
  }  
  
  if(learner == "classif.PART"){
    ps = makeParamSet(
      makeDiscreteParam("C",
                        0.25))
  }  
  
  if(learner == "classif.qda"){
    ps = makeParamSet(
      makeDiscreteParam("nu",
                        5))
  }  
  
  if(learner == "classif.randomForest"){
    ps = makeParamSet(
      makeDiscreteParam("mtry",
                        5L))
  } 
  
  if(learner == "classif.randomForestSRC"){
    ps = makeParamSet(
      makeDiscreteParam("mtry",
                        5L))
  } 
  
  if(learner == "classif.rda"){
    ps = makeParamSet(
      makeDiscreteParam("gamma",
                        seq(0, 1, length = tune.length)),
      makeDiscreteParam("lamba",
                        seq(0, 1, length = tune.length)))
  }
  
  if(learner == "classif.rpart"){
    ps = makeParamSet(
      makeDiscreteParam("cp",
                        seq(0, 1, length = tune.length)))
  }
  
  
  if(learner == "classif.svm"){
    ps = makeParamSet(
      makeDiscreteParam("cost",
                        seq(1L, tune.length)))
  }
  return(ps)
}