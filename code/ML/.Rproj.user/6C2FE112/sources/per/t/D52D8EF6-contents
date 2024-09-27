###################
##  Randon Forest
###################

########### packages
library(pacman)

p_load(tidyverse, 
     #  rio, 
      # rpart, 
      # rpart.plot, 
       randomForest, 
      # caret, 
       pROC, 
       foreach,
       doParallel, 
       PRROC)

########## clean all 
rm(list=ls())

########## paths

root<- "C:\\Users\\Andres Felipe\\OneDrive - Universidad de los Andes\\Research Proyects\\ML_GEIH"
data<- paste0(root, "\\data")
raw<- paste0(data, "\\raw")
geih2012<- paste0(raw, "\\GEIH\\2012")
clean<-  paste0(data, "\\clean_prediction\\2012")



############
######## Import and prepare data.
############

workers<- readRDS(paste0(clean, "\\employees_analysis.rds"))

discart<- c("house" , "household","person",   "Hogar", "P6016",  
            "oci", "job_type", "unionized" , "student", "monthly_mw50", "monthly_mw75",
            "monthly_mw125","inglabo", "mw_worker125" , "mw_worker50" )

workers_final<-workers%>% 
  select(-all_of(discart))

table(workers_final$mw_worker75)[2]/nrow(workers_final)



######## random forest 
### set parallel 

param_combinations<- expand.grid(mtry= seq(1, 22 ,3) ,
                                 maxnodes=seq(20, 50,5))

num_cores <-  5
cl <- makeCluster(num_cores)    # Create a cluster with available cores
registerDoParallel(cl)          # Register the parallel backend


cv_results_rf<- foreach(i= 1:10, .combine = rbind,  .packages = c( "tidyverse", "randomForest","PRROC") ) %dopar% {
  
  train<- workers_final %>%
    filter(fold!=i )
  test<- workers_final %>%
    filter(fold==i )
  
  y<- train$mw_worker75
  X<- model.matrix(mw_worker75~.-1 -fold, train )
  
  X_test<- model.matrix(mw_worker75~.-1 -fold, test )
  y_test<-  test$mw_worker75
  
  
  ### Test error 
  fold_results <- data.frame()
  for (l in 1:nrow(param_combinations)){
    
  rf<- randomForest(x=X, 
                      y=y, 
                      ntree= 100, 
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

#########################
## test 
#########################


train<- workers_final %>%
  filter(fold!=1 )
test<- workers_final %>%
  filter(fold==1 )

y<- train$mw_worker75
X<- model.matrix(mw_worker75~.-1 -fold, train )

X_test<- model.matrix(mw_worker75~.-1 -fold, test )
y_test<-  test$mw_worker75


rf<- randomForest(x=X, 
                  y=y, 
                  ntree= 50, 
                  mtry= 5, 
                  maxnodes= 50,
                  importance= FALSE, 
                  nodesize= 50)


getTree(rf, k=1, labelVar=TRUE)
rf


predictions<- predict(rf, X_test, type= "prob")

fg<-predictions[y_test=="Yes"]
bg<-predictions[y_test=="No"]
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
areauPR<- pr$auc.integral
areauPR


auc_value <- roc(y_test, predictions[,2])$auc
auc_value



for (l in 1:length(param_combinations)){
print(l)
  }

predictions<- predict(myrf, X_test ,type= "response")
confusionMatrix(data = predictions, reference = y_test, positive="Yes")

predictions<- predict(myrf, X_test ,type= "prob")
auc<-roc(y_test,predictions[,2] )
plot(auc)




getTree(myrf, k=34, labelVar=TRUE)



###### appendix tree
