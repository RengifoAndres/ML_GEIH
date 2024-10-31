# Load necessary libraries
library(pacman)

p_load(tidyverse,
       neuralnet,
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

train <- workers_final %>% filter(fold != 1)  %>%
         select(-fold)
test <- workers_final %>% filter(fold == 1) %>%
        select(-fold)
#### prepare the recipe 


train_rec <-
  recipe(mw_worker75 ~ ., data = train)  %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) 

#train_rec<- prep(train_rec, training = train)
#pr_train<- bake(train_rec, train)

set.seed(2)
neural_net <- brulee_mlp(train_rec, 
                  data = train, 
                  epochs = 20, 
                  hidden_units = c(25, 50),
                  learn_rate = 0.01,
                  penalty = 0.01)

autoplot(neural_net)
predictions<- predict(fit, test, type = "prob")
predictions<- predictions$.pred_Yes
y_test<- test$mw_worker75
fg<-predictions[y_test=="Yes"]
bg<-predictions[y_test=="No"]
pr <- pr.curve(scores.class0 = fg, scores.class1 =bg, curve = TRUE)
areauPR <- pr$auc.integral
areauPR
##### now get the conf. mat


predictions<- predict(fit, test)
confusionMatrix(data = predictions$.pred_class, reference = y_test, positive="Yes")



