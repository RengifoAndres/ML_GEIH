###################
##  Randon Forest
###################

########### packages
library(pacman)

p_load(tidyverse, 
       randomForest, 
       pROC, 
       foreach,
       doParallel, 
       PRROC)

########## clean all 
rm(list=ls())

########## paths

if (getwd()=="/export/home/rcsguest/rcs_arengifojaramill/Documents/GitHub/ML_GEIH/code/ML") {
  root<- "/export/home/rcsguest/rcs_arengifojaramill/Documents/GitHub/ML_GEIH"
  
}  else {
  root<- "C:/Users/Andres Felipe/OneDrive - Universidad de los Andes/Research Proyects/ML_GEIH"
}


data<- paste0(root, "/data")
raw<- paste0(data, "/raw")
geih2012<- paste0(raw, "/GEIH/2012")
clean<-  paste0(data, "/clean_prediction/2012")



############
######## Import and prepare data.
############

workers<- readRDS(paste0(clean, "/employees_analysis.rds"))
discart<- c("house" , "household","person",   "Hogar", "P6016",  
            "oci", "job_type", "unionized" , "student", "monthly_mw50", "monthly_mw75",
            "monthly_mw125","inglabo", "mw_worker125" , "mw_worker50", "maxgrade", "schyears", "maxedulevel" )

workers_final<-workers%>% select(-all_of(discart))
table(workers_final$mw_worker75)[2]/nrow(workers_final)


######## random forest Set Parameters
param_combinations<- expand.grid(mtry= seq(40, 80 ,10) ,
                                 maxnodes= c(5, 40)    #  seq(3, 40,5)
                                 )

### set parallel 
num_cores <-  8
cl <- makeCluster(num_cores)    # Create a cluster with available cores
registerDoParallel(cl)          # Register the parallel backend


## set folds 
nfolds<- 5

start_time <- Sys.time() ## set time to count

cv_results_rf<- foreach(i= 1:nfolds, .combine = rbind,  .packages = c( "tidyverse", "randomForest","PRROC") ) %dopar% {
  
  train<- workers_final %>%
    filter(fold!=i )
  test<- workers_final %>%
    filter(fold==i )
  
  y<- train$mw_worker75
  X<- model.matrix(mw_worker75~. -fold, train )
  X<- X[, 2:ncol(X)]
  X_test<- model.matrix(mw_worker75~. -fold, test )
  X_test<- X[, 2:ncol(X_test)]
  y_test<-  test$mw_worker75
  
  
  ### Test error 
  fold_results <- data.frame()
  for (l in 1:nrow(param_combinations)){
    
  rf<- randomForest(x=X, 
                      y=y, 
                      ntree= 300, 
                      mtry= param_combinations$mtry[l], 
                      maxnodes= param_combinations$maxnodes[l],
                      importance= FALSE, 
                      nodesize= 50)
  
 
  predictions<- predict(rf, X_test, type= "prob")
  
  fg<-predictions[y_test=="Yes"]
  bg<-predictions[y_test=="No"]
  pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  areauPR<- pr$auc.integral
  
    
    fold_results <- rbind(fold_results, data.frame(model = l, fold = i, auPR = areauPR))
  }
  
  fold_results
}

stopCluster(cl)

end_time <- Sys.time()

# Display the time difference
execution_time <- end_time - start_time
execution_time


################ PLOTS ################

##### preparing the results for plot

param_combinations<- param_combinations %>%
  mutate(model=row_number())

cv_results_rf<- cv_results_rf %>%
  full_join(param_combinations)

#### Plot by model

summary <- cv_results_rf %>%
  group_by(model) %>%
  summarise(
    mean_auPR = mean(auPR),
    se_auPR = sd(auPR) / sqrt(n()),
    model = unique(model)  # Ensure we have the same iteration numbers
  )


# Combined plot with iteration number on x-axis and lambda as color scale
ggplot(cv_results_rf) +
  geom_boxplot( aes(x = factor(model), y = auPR, fill = mtry), outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_point(data = summary, aes(x = factor(model), y = mean_auPR), size = 1, color = "black") +
  geom_errorbar(data = summary, aes(x = factor(model), ymin = mean_auPR - se_auPR, ymax = mean_auPR + se_auPR), width = 0.2, color = "blue") +
  labs(title = "",
       x = "Model",
       y = "AUC",
       fill = "mtry") +
  scale_fill_gradient(low = "#FFDDC1", high = "#FF5500") +  # Scale color for the fill based on lambda
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",  # Move legend to the right for better space utilization
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),  # Rotate x-axis labels and adjust size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title = element_text(size = 14)
  )




summary2 <- cv_results_rf %>%
  group_by(mtry) %>%
  summarise(
    mean_auPR = mean(auPR),
    se_auPR = sd(auPR) / sqrt(n()),
  )

ggplot(summary2) +
  geom_errorbar( aes(x = factor(mtry), ymin = mean_auPR - se_auPR, ymax = mean_auPR + se_auPR), width = 0.2, color = "blue") +
  theme_classic(base_size = 15) 

#####

summary2 <- cv_results_rf %>%
  group_by(maxnodes) %>%
  summarise(
    mean_auPR = mean(auPR),
    se_auPR = sd(auPR) / sqrt(n()),
  )

ggplot(summary2) +
  geom_errorbar( aes(x = factor(maxnodes), ymin = mean_auPR - se_auPR, ymax = mean_auPR + se_auPR), width = 0.2, color = "blue") +
  theme_classic(base_size = 15) 


#########################
## test 
#########################


train<- workers_final %>%
  filter(fold>2 )
test<- workers_final %>%
  filter(fold<=2 )

y<- train$mw_worker75
X<- model.matrix(mw_worker75~. -fold, train )
X<- X[, 2:ncol(X)]

X_test<- model.matrix(mw_worker75~. -fold, test )
X_test<- X_test[, 2:ncol(X_test)]

y_test<-  test$mw_worker75


rf<- randomForest(x=X, 
                  y=y, 
                  ntree= 300, 
                  mtry= 130, 
                  maxnodes= 5,
                  importance= FALSE, 
                  nodesize= 25)


getTree(rf, k=1, labelVar=TRUE)
rf


predictions<- predict(rf, X_test, type= "prob")

fg<-predictions[y_test=="Yes"]
bg<-predictions[y_test=="No"]
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
areauPR<- pr$auc.integral
areauPR



predictions<- predict(rf, X_test ,type= "response") 
p_load(caret)
confusionMatrix(data = predictions, reference = y_test, positive="Yes")

predictions<- predict(rf, X_test ,type= "prob")
auc<-roc(y_test,predictions[,2] )
plot(auc)
auc$auc



getTree(rf, k=23, labelVar=TRUE)



auc_value <- roc(y_test, predictions[,2])$auc
auc_value

###### appendix tree
