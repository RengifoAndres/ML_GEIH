# Load necessary libraries
library(pacman)

p_load(tidyverse, nnet, pROC, foreach, doParallel, PRROC)

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

# Neural Network Parameters
param_combinations <- expand.grid(size = c(5, 10),   # Number of units in hidden layer
                                  decay = c(0.1, 0.001))  # Weight decay (regularization)

# Set Parallelization
num_cores <- 3
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Set Folds
nfolds <- 5

start_time <- Sys.time()

cv_results_nnet <- foreach(i = 1:nfolds, .combine = rbind, .packages = c("tidyverse", "nnet", "PRROC")) %dopar% {
  train <- workers_final %>% filter(fold != i)
  test <- workers_final %>% filter(fold == i)
  
  # Normalize predictors
  train_mean <- colMeans(train[-which(names(train) %in% c("mw_worker75", "fold"))])
  train_sd <- apply(train[-which(names(train) %in% c("mw_worker75", "fold"))], 2, sd)
  
  X <- scale(select(train, -c(mw_worker75, fold)), center = train_mean, scale = train_sd)
  X_test <- scale(select(test, -c(mw_worker75, fold)), center = train_mean, scale = train_sd)
  
  y <- train$mw_worker75
  y_test <- test$mw_worker75
  
  # Convert response to binary (0/1)
  y <- as.numeric(as.factor(y)) - 1
  y_test <- as.numeric(as.factor(y_test)) - 1
  
  # Test error
  fold_results <- data.frame()
  
  for (l in 1:nrow(param_combinations)) {
    nn_model <- nnet(x = X, 
                     y = y, 
                     size = param_combinations$size[l], 
                     decay = param_combinations$decay[l], 
                     maxit = 200, 
                     trace = FALSE, 
                     linout = FALSE)  # Setting linout = FALSE for classification
    
    predictions <- predict(nn_model, X_test, type = "raw")
    
    # Compute the precision-recall curve
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

# Display cv_results_nnet
cv_results_nnet