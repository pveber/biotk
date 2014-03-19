let main experiment_description_file output =
  let ed = Experiment_description.load experiment_description_file in
  ignore ed
