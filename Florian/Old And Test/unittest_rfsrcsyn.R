context("classif_rfsrcSyn")

test_that("classif_rfsrcSyn", {
  requirePackages("rfsrcSyn", default.method = "load")
  
  parset.list = list(
    list(ntree=5, mtry=2, nodesize = 3)
  )
  type = c("response", "prob")
  
  #randomUniformForest ist not reproducible with set.seed, so we just check for createability
  for(i in 1:length(parset.list)){
    for(j in 1:length(type)){
      parset = parset.list[[i]]
      ruf.classif.lrn = try(makeLearner("classif.rfsrcSyn", par.vals = parset,
                                        predict.type = type[j]))
      expect_is(ruf.classif.lrn, "classif.rfsrcSyn")
      ruf.classif.m = try(train(ruf.classif.lrn, binaryclass.task))
      expect_is(ruf.classif.m, "WrappedModel")
      ruf.classif.p = try(predict(ruf.classif.m, newdata = binaryclass.test))
      expect_is(ruf.classif.p, c("PredictionClassif", "Prediction"))
    }
  }
})
