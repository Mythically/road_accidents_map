library(h2o)
h2o.init()

data <- read.csv("../resources/merge_data_table_both_index_ml.csv")
data_h2o <- as.h2o(data)

splits <- h2o.splitFrame(data_h2o, ratios = 0.8, seed = 123)
train <- splits[[1]]
test <- splits[[2]]

nrow(data)
nrow(train)
nrow(test)


y <- "accident_severity"
x <- setdiff(names(data), y)

aml <- h2o.automl(x = x, y = y, training_frame = train, max_models = 10, seed = 1)

best_model <- aml@leader

h2o.varimp_plot(best_model)

# Get the leaderboard
lb <- aml@leaderboard
lb
# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(lb[,"model_id"])[,"model_id"]
model_ids
# Get variable importance for each model
for (model_id in model_ids) {
  model <- h2o.getModel(model_id)
  print(paste("Variable importance for model", model_id))
  print(h2o.varimp(model))
}
# compare to a baseline mean model
# Compute the mean value of the target variable in the training data
mean_value <- mean(train[,y])

# Create a baseline model that always predicts the mean value
baseline_predictions <- rep(mean_value, nrow(test))

# Compute the RMSE of the baseline model
baseline_rmse <- sqrt(mean((test[,y] - baseline_predictions)^2))

# Compute the RMSE of the best H2O AutoML model
best_model_rmse <- h2o.rmse(best_model)

# Compare the RMSE values
cat("RMSE of baseline model:", baseline_rmse, "\n")
cat("RMSE of best H2O AutoML model:", best_model_rmse, "\n")
