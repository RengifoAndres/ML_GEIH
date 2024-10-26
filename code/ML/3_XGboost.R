# Load necessary libraries
library(pacman)

p_load(tidyverse, xgboost, pROC, foreach, doParallel, PRROC)

# Clear workspace
rm(list=ls())

# Paths
if (getwd() == "/export/home/rcsguest/rcs_arengifojaramill/Documents/GitHub/ML_GEIH/code/ML") {
  root <- "/export/home/rcsguest/rcs_arengifojaramill/Documents/GitHub/ML_GEIH"
} else {
  root <- "C:/Users/Andres Felipe/OneDrive - Universidad de los Andes/Research Proyects/ML_GEIH"
}

data <- paste0(root, "/data")
raw <- paste0(data, "/raw")
geih2012 <- paste0(raw, "/GEIH/2012")
clean <- paste0(data, "/clean_prediction/2012")

# Import and Prepare Data
workers <- readRDS(paste0(clean, "/employees_analysis.rds"))
discart <- c("house", "household", "person", "Hogar", "P6016", "oci", "job_type", "unionized", 
             "student", "monthly_mw50", "monthly_mw75","monthly_mw125","inglabo",
             "mw_worker125", "mw_worker50", "maxgrade", "schyears", "maxedulevel")

workers_final <- workers %>% select(-all_of(discart))

# Random Forest Parameters
param_combinations <- expand.grid(eta = c(0.01, 0.05, 0.3), 
                                  max_depth = c(2, 3, 10), 
                                  subsample = c(0.7, 0.9),
                                  nrounds = 1000
)

table(workers_final$fold)

# Set Parallelization
num_cores <- 3
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Set Folds
nfolds <- 5

start_time <- Sys.time()

cv_results_xgb <- foreach(i = 1:nfolds, .combine = rbind, .packages = c("tidyverse", "xgboost", "PRROC")) %dopar% {
 
  
  train <- workers_final %>% filter(fold != i)
  test <- workers_final %>% filter(fold == i)
  
  y <- as.numeric(train$mw_worker75) - 1  # Convert factor to numeric
  X <- model.matrix(mw_worker75 ~ . - fold, train)[, -1]
  X_test <- model.matrix(mw_worker75 ~ . - fold, test)[, -1]
  y_test <- as.numeric(test$mw_worker75) - 1  # Convert factor to numeric
  
  dtrain <- xgb.DMatrix(data = X, label = y)
  dtest <- xgb.DMatrix(data = X_test, label = y_test)
  
  # Test error 
  fold_results <- data.frame()
  for (l in 1:nrow(param_combinations)) {
    params <- list(
      objective = "binary:logistic",
      eval_metric = "aucpr",  # Use AUC-PR as evaluation metric
      eta = param_combinations$eta[l],
      max_depth = param_combinations$max_depth[l],
      subsample = param_combinations$subsample[l]
    )
    
    bst_model <- xgb.train(params = params, 
                           data = dtrain, 
                           nrounds = param_combinations$nrounds[l], 
                           verbose = 0)  # Suppress output
    
    predictions <- predict(bst_model, dtest)
    pr <- pr.curve(scores.class0 = predictions[y_test == 1], scores.class1 = predictions[y_test == 0], curve = TRUE)
    areauPR <- pr$auc.integral
    
    fold_results <- rbind(fold_results, data.frame(model = l, fold = i, auPR = areauPR))
  }
  
  fold_results
}

stopCluster(cl)

end_time <- Sys.time()

# Display the time difference
execution_time <- end_time - start_time
print(execution_time)

# Display cv_results_xgb
cv_results_xgb