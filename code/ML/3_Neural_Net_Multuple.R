# Load necessary libraries
library(pacman)

p_load(tidyverse, neuralnet, foreach, doParallel, PRROC)

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

# Normalize the data: Simplified for demonstration; consider a robust normalization strategy
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
workers_final <- workers_final %>% 
  mutate(across(-c(mw_worker75, fold), normalize))

# Neural Network Parameters
hidden_layer_sizes <- list(c(5), c(10), c(5, 5))  # Different combinations of hidden layers

# Set Parallelization
num_cores <- 5
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Set Folds
nfolds <- 5

start_time <- Sys.time()

cv_results_nn <- foreach(i = 1:nfolds, .combine = rbind, .packages = c("tidyverse", "neuralnet", "PRROC")) %dopar% {
  train <- workers_final %>% filter(fold != i)
  test <- workers_final %>% filter(fold == i)
  
  # Prepare input for neuralnet
  formula <- as.formula(paste("mw_worker75 ~", paste(names(train)[!names(train) %in% c("mw_worker75", "fold")], collapse = " + ")))
  
  # Convert response to binary (0/1)
  train$mw_worker75 <- as.numeric(as.factor(train$mw_worker75)) - 1
  test$mw_worker75 <- as.numeric(as.factor(test$mw_worker75)) - 1
  
  fold_results <- data.frame()
  
  for (hidden in hidden_layer_sizes) {
    nn_model <- neuralnet(formula, data = train, hidden = hidden, linear.output = FALSE)
    
    # Make predictions
    nn_results <- compute(nn_model, test %>% select(-c(mw_worker75, fold)))
    predictions <- nn_results$net.result[, 1]
    
    # Compute the precision-recall curve
    pr <- pr.curve(scores.class0 = predictions[test$mw_worker75 == 1], scores.class1 = predictions[test$mw_worker75 == 0], curve = TRUE)
    areauPR <- pr$auc.integral
    
    fold_results <- rbind(fold_results, data.frame(layers = paste(hidden, collapse = "-"), fold = i, auPR = areauPR))
  }
  
  fold_results
}

stopCluster(cl)

end_time <- Sys.time()

# Display the time difference
execution_time <- end_time - start_time
print(execution_time)

# Display cv_results_nn
cv_results_nn