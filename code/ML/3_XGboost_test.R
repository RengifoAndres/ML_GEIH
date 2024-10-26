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


train <- workers_final %>% filter(fold != 1)
test <- workers_final %>% filter(fold == 1)

y <- as.numeric(train$mw_worker75) - 1  # Convert factor to numeric
X <- model.matrix(mw_worker75 ~ . - fold, train)[, -1]
X_test <- model.matrix(mw_worker75 ~ . - fold, test)[, -1]
y_test <- as.numeric(test$mw_worker75) - 1  # Convert factor to numeric

dtrain <- xgb.DMatrix(data = X, label = y)
dtest <- xgb.DMatrix(data = X_test, label = y_test)


### set parameters 
###
  params <- list(
    objective = "binary:logistic",
    eval_metric = "aucpr",  # Use AUC-PR as evaluation metric
    eta = 0.01,
    max_depth = 2,
    subsample = 0.8
  )
  
  bst_model <- xgb.train(params = params, 
                         data = dtrain, 
                         nrounds = 1000, 
                         verbose = 1) 

  
  
  predictions <- predict(bst_model, dtest)
  pr <- pr.curve(scores.class0 = predictions[y_test == 1], scores.class1 = predictions[y_test == 0], curve = TRUE)
  areauPR <- pr$auc.integral
  areauPR