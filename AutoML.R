
## Run AutoML 

# Run AutoML, stopping after 10 models.  The `max_models` argument specifies the number of individual
# (or "base") models, and does not include the two ensemble models that are trained at the end.

aml <- h2o.automl(y = "TenYearCHD",
                  training_frame = train.hex,
                  max_models = 10,
                  seed = 1234)



## Leaderboard

# Next, we will view the AutoML Leaderboard.  Since we did not specify a `leaderboard_frame` in the `h2o.automl()`
# function for scoring and ranking the models, the AutoML leaderboard uses cross-validation metrics to rank the models.  

# A default performance metric for each machine learning task (binary classification, multiclass classification, regression)
# is specified internally and the leaderboard will be sorted by that metric.  In the case of binary classification,
# the default ranking metric is Area Under the ROC Curve (AUC).  In the future, the user will be able to specify any of the
# H2O metrics so that different metrics can be used to generate rankings on the leaderboard.

# The leader model is stored at `aml@leader` and the leaderboard is stored at `aml@leaderboard`.

lb <- aml@leaderboard


# Now we will view a snapshot of the top models.  Here we should see the two Stacked Ensembles at or near the top of
# the leaderboard.  Stacked Ensembles can almost always outperform a single model.

print(lb)


# To view the entire leaderboard, specify the `n` argument of the `print.H2OFrame()` function as the total number of rows:

print(lb, n = nrow(lb))



## Ensemble Exploration

# To understand how the ensemble works, let's take a peek inside the Stacked Ensemble "All Models" model.
# The "All Models" ensemble is an ensemble of all of the individual models in the AutoML run.  This is often the top performing
# model on the leaderboard.

# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
# Get the "All Models" Stacked Ensemble model
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
# Get the Stacked Ensemble metalearner model
metalearner <- h2o.getModel(se@model$metalearner$name)


# Examine the variable importance of the metalearner (combiner) algorithm in the ensemble.
# This shows us how much each base learner is contributing to the ensemble. The AutoML Stacked Ensembles use the default
# metalearner algorithm (GLM with non-negative weights), so the variable importance of the metalearner is actually the 
# standardized coefficient magnitudes of the GLM. 

h2o.varimp(metalearner)


# We can also plot the base learner contributions to the ensemble.

h2o.varimp_plot(metalearner)


## Save Leader Model

'There are two ways to save the leader model -- binary format and MOJO format.  If you are taking your leader model to production, then we'd suggest the MOJO format since it's optimized for production use.
```{r}
h2o.saveModel(aml@leader, path = "./product_backorders_model_bin")
```

```{r}
h2o.download_mojo(aml@leader, path = "./")
```
