
unlink("EasyTest-files", recursive = TRUE)

reg = BatchExperiments::makeExperimentRegistry("EasyTest", packages = c("OpenML", "mlr"))

tasks = listOMLTasks(type = 1)
sel.tasks = subset(tasks, NumberOfInstances <= 1000 & NumberOfSymbolicFeatures > 1)
sel.tasks = head(sel.tasks,5L)

oml.task.ids = sel.tasks$task_id
oml.task.ids
problem.ids = paste0("t", oml.task.ids)


task = list(getOMLTask(task.id = 8), getOMLTask(task.id = 2))
rdesc = list(makeResampleDesc("CV", iters = 3L))


str(rdesc)

 
pdes = Map(
  function(id, task, rdesc, seed) {
  static = list(rdesc = rdesc, task = task)
  addProblem(reg, id, static = static, dynamic = NULL, overwrite = TRUE, seed = seed)
  makeDesign(id, design = data.frame(i = seq_len(3)))
}, 
id = c("t2","t8"),
rdesc = rdesc,
task = task,
seed = reg$seed
)

pdes
reg

