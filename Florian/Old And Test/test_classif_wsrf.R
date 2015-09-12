context("classif_wsrf")

test_that("classif_wsrf", {
  requirePackages("wsrf", default.method = "load")
  parset.list = list(
    list(),
    list(ntrees = 5, mtry=2),
    list(ntrees = 5, mtry=4),
    list(ntrees = 10, mtry=2),
    list(ntrees = 10, mtry=4)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(wsrf::wsrf, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = multiclass.test, type = "response")
    set.seed(getOption("mlr.debug.seed"))
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.wsrf", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.wsrf", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)

  tt = wsrf::wsrf

  testCVParsets("classif.wsrf", multiclass.df, multiclass.target, tune.train = tt, parset.list = parset.list)
})