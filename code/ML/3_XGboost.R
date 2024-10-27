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
param_combinations <- expand.grid(eta = c(0.01, 0.05, 0.1), 
                                  max_depth = c(2, 3, 5), 
                                  subsample = c(0.7),
                                  nrounds = c(1000, 1500)
)


# Set Parallelization
num_cores <- 5
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


###### Merge model to parameters 

param_combinations<- param_combinations %>%
  mutate(model=row_number())

cv_results_xgb<- cv_results_xgb %>%
  full_join(param_combinations)

############## create model-performance dataset

summary <- cv_results_xgb %>%
  group_by(model) %>%
  summarise(
    mean_auPR = mean(auPR),
    se_auPR = sd(auPR) / sqrt(n()),
    model = unique(model)  # Ensure we have the same iteration numbers
  )


summary<- summary %>%
  full_join(param_combinations)

best_param<-summary %>% select(any_of(colnames(param_combinations))) %>%
  filter(row_number()==which.max(summary$mean_auPR))
max_val<-summary %>% select(mean_auPR) %>%
  filter(row_number()==which.max(summary$mean_auPR))

max_val
best_param




# Combined plot with iteration number on x-axis and lambda as color scale
ggplot(cv_results_xgb) +
  geom_boxplot( aes(x = factor(model), y = auPR, fill=eta ), outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_point(data = summary, aes(x = factor(model), y = mean_auPR), size = 1, color = "black") +
  geom_errorbar(data = summary, aes(x = factor(model), ymin = mean_auPR - se_auPR, ymax = mean_auPR + se_auPR), width = 0.2, color = "blue") +
  labs(title = "",
       x = "Model",
       y = "AUC", 
       fill= "eta") +
  scale_fill_gradient(low = "#FFDDC1", high = "#FF5500") +  # Scale color for the fill based on lambda
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",  # Move legend to the right for better space utilization
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),  # Rotate x-axis labels and adjust size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title = element_text(size = 14)
  )


############### Estimating final model. 
test_out<-  readRDS(paste0(clean, "/employees_test.rds"))
test_out<-  test_out %>% select(-all_of(discart))

y_full <- as.numeric(workers_final$mw_worker75) - 1  # Convert factor to numeric
X_full   <- model.matrix(mw_worker75 ~ . - fold, workers_final)[, -1]
dfull <- xgb.DMatrix(data = X_full, label = y_full)


y_out <- test_out$mw_worker75  # Not Convert factor to numeric
X_out   <- model.matrix(mw_worker75 ~ . , test_out)[, -1] ## does not have fold


params_final <- list(
  objective = "binary:logistic",
  eval_metric = "aucpr",  # Use AUC-PR as evaluation metric
  eta = best_param$eta[1],
  max_depth = best_param$max_depth[1],
  subsample = best_param$subsample[1]
)

xgboost_final <- xgb.train(params = params_final, 
                         data = dfull, 
                        nrounds = best_param$nrounds[1], 
                         verbose = 0)  # Suppress output

xgboost_final_predictions <- predict(xgboost_final, X_out)

# confusion matri
p_load(caret)
confusionMatrix(data = xgboost_final_predictions, reference = y_out, positive="Yes")

auc<-roc(y_out,xgboost_final_predictions )
plot(auc)
auc$auc


######### 