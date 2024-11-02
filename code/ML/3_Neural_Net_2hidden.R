# Load necessary libraries
library(pacman)

p_load(tidyverse,
       foreach,
       doParallel,
       PRROC, 
       caret, 
       recipes,
       brulee)

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
             "mw_worker125", "mw_worker50", "maxgrade", "schyears", "maxedulevel", "h_energy_supp",
             "h_aqueduct_supp")



workers_final <- workers %>% select(-all_of(discart))

# Random Forest Parameters
param_combinations <- expand.grid( penalty = c(0.01, 0.1), 
                                   learn_rate  = c(0.005, 0.01), 
                                   dropout = c(0.1, 0.3),
                                   h_units1 = c( 50,100 ) ,
                                   h_units2 = c( 50,100 )
                                  )

# Set Parallelization
num_cores <- 5
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Set Folds
nfolds <- 5

start_time <- Sys.time()

cv_neural_net <- foreach(i = 1:nfolds, .combine = rbind, 
                          .packages = c("tidyverse", "recipes", "brulee" , "PRROC")) %dopar% {
  
train <- workers_final %>% filter(fold != i)  %>%
         select(-fold)
test <- workers_final %>% filter(fold == i) %>%
        select(-fold)


#### prepare the recipe 
train_rec <-
  recipe(mw_worker75 ~ ., data = train)  %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) 

#train_rec<- prep(train_rec, training = train)
#pr_train<- bake(train_rec, train)


fold_results <- data.frame()
for (l in 1:nrow(param_combinations)) {
  
set.seed(267487)
neural_net <- brulee_mlp(train_rec, 
                  data = train, 
                  epochs = 100, 
                  hidden_units = c(param_combinations$h_units1[l], 
                                   param_combinations$h_units2[l]),
                  activation = c("relu", "relu"),
                  learn_rate = param_combinations$learn_rate[l],
                  penalty = param_combinations$penalty[l], ## l2 penalty
                  dropout= param_combinations$dropout[l], 
                  stop_iter= 1000, 
                  validation=0.2)

predictions<- predict(neural_net, test, type = "prob",epoch= 37 )
predictions<- predictions$.pred_Yes
y_test<- test$mw_worker75
fg<-predictions[y_test=="Yes"]
bg<-predictions[y_test=="No"]
pr <- pr.curve(scores.class0 = fg, scores.class1 =bg, curve = TRUE)
areauPR <- pr$auc.integral
areauPR

fold_results <- rbind(fold_results, data.frame(model = l, fold = i, auPR = areauPR))

}

fold_results

}


stopCluster(cl)

end_time <- Sys.time()

# Display the time difference
execution_time <- end_time - start_time
print(execution_time)








##### now get the conf. mat


predictions<- predict(neural_net, test)
confusionMatrix(data = predictions$.pred_class, reference = y_test, positive="Yes")



