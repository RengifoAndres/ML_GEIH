###################
##  Lasso Ridge and
##   Elastic Net
###################

########### packages

library(pacman)

p_load(tidyverse, 
       rio, 
       glmnet, 
       doParallel, 
       caret, 
       pROC, 
       foreach, 
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

workers_final<-workers%>% select(-all_of(discart))
table(workers_final$mw_worker75)[2]/nrow(workers_final)



#### Run the model once to get the lambda grid

y<- workers_final$mw_worker75
X<- model.matrix(mw_worker75 ~ .-1 -fold, workers_final )


net_logit0<- glmnet(x=X,
                    y=y, 
                    family="binomial", 
                    alpha=1, 
                    type.measure = "class"
)

## 
#plot(net_logit0)
#coef(net_logit0, s=0.0592863106   )
# the solution is probably in the middle 

lambda_search<-net_logit0$lambda[20:(length(net_logit0$lambda))]


### set parallel 
num_cores <-  5
cl <- makeCluster(num_cores)    # Create a cluster with available cores
registerDoParallel(cl)          # Register the parallel backend


  #for (i in 1:10) {
 cv_results_lasso_logit<- foreach(i= 1:10, .combine = rbind,  .packages = c( "tidyverse", "glmnet", "PRROC") ) %dopar% {
    
    train<- workers_final %>%
      filter(fold!=i )
    test<- workers_final %>%
      filter(fold==i )
   
    y<- train$mw_worker75
    X<- model.matrix(mw_worker75~.-1 -fold, train )
    
    X_test<- model.matrix(mw_worker75~.-1 -fold, test )
    y_test<-  test$mw_worker75
    
    lasso_logit<- glmnet(x=X,
                        y=y, 
                        family="binomial", 
                        alpha=1, 
                        lambda= lambda_search,
                        type.measure = "class"
                        )
    
    ### Test error 
    fold_results <- data.frame()
    for (l in lambda_search){
    predictions<- predict(lasso_logit, X_test,  s=l, type= "response")
   
    fg<-predictions[y_test=="Yes"]
    bg<-predictions[y_test=="No"]
    pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
    areauPR<- pr$auc.integral
    
   
    fold_results <- rbind(fold_results, data.frame(param = l, fold = i, auPR = areauPR))
    }
    
    fold_results
  }

stopCluster(cl)


###### Plot results

param_order <- unique(cv_results_lasso_logit$param)[order(unique(cv_results_lasso_logit$param))]

# Create the iteration numbers for both datasets
cv_results_lasso_logit <- cv_results_lasso_logit %>%
  mutate(iteration = factor(match(param, param_order)))

summary_df <- cv_results_lasso_logit %>%
  group_by(param) %>%
  summarise(
    mean_auPR = mean(auPR),
    se_auPR = sd(auPR) / sqrt(n()),
    iteration = unique(iteration)  # Ensure we have the same iteration numbers
  )

# Combined plot with iteration number on x-axis and lambda as color scale
ggplot(cv_results_lasso_logit) +
  geom_boxplot( aes(x = iteration, y = auPR, fill = param), outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_point(data = summary_df, aes(x = iteration, y = mean_auPR), size = 1, color = "black") +
  geom_errorbar(data = summary_df, aes(x = iteration, ymin = mean_auPR - se_auPR, ymax = mean_auPR + se_auPR), width = 0.2, color = "blue") +
  labs(title = "",
       x = "Model",
       y = "AUC",
       fill = "Lambda") +
  scale_fill_gradient(low = "#FFDDC1", high = "#FF5500") +  # Scale color for the fill based on lambda
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",  # Move legend to the right for better space utilization
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),  # Rotate x-axis labels and adjust size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title = element_text(size = 14)
  )



















########### Appendix

#### sanity checks

train<- workers_final %>%
        filter(fold==5 )
test<- workers_final %>%
       filter(fold==1 )

y<- train$mw_worker75
X<- model.matrix(mw_worker75~.-1 -fold, train )

X_test<- model.matrix(mw_worker75~.-1 -fold, test )
y_test<-  test$mw_worker75

enet_logit<- glmnet(x=X,
                    y=y, 
                    family="binomial", 
                    alpha=1, 
                    lambda= 0.06,
                    type.measure = "class"
)


predictions<- predict(enet_logit, X_test,  s=0.06, type= "response")
fg<-predictions[y_test=="Yes"]
bg<-predictions[y_test=="No"]

pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
pr$auc.integral

plot(pr)
 a<-roc(y_test,predictions[,1])

 plot(a$sensitivities, a$specificities)

detectCores()
num_cores <-  6
cl <- makeCluster(num_cores)    # Create a cluster with available cores
registerDoParallel(cl)          # Register the parallel backend


# How many times will the loop run
n_iterations <- 1000
# To save the results
results <- list()

# Use foreach and %dopar% to run the loop in parallel
results <- foreach(i = 1:n_iterations) %dopar% {
  # Store the results
  results[i] <- i^2
}

# Don't fotget to stop the cluster
stopCluster(cl = cluster)



predictions<-as.factor( predict(enet_logit, X_test,  s=0.1, type= "class"))
confusionMatrix(data = predictions , reference = y_test, positive="Yes")

comdta<-data.frame(y= y_test, 
                   y_hat=as.factor(as.vector(predictions)))
with(comdta,table(y,y_hat))



stopCluster(cl)


data.frame(lambda= log(enet_logit$lambda), 
           miss= enet_logit$cvm, 
           missup= enet_logit$cvup, 
           misslo= enet_logit$cvlo) %>% 
    filter(lambda<=0) %>%
    ggplot( aes(x =lambda , y =miss )) +
      #geom_line(color="red")  +  
      geom_point(color="#FFD700", size= 2.5, shape= 23, fill="#FFD700")  + # Line
      geom_errorbar(aes(ymin =misslo , ymax =missup),       # Error bars
                    width = 0.07, color="black", size=0.5) +   
      labs(title = "", x = "Log(Penalty)", y = "Classification Error") +
       geom_vline(xintercept = log(enet_logit$lambda.min) , color = "red", linetype = "dashed")+
  geom_vline(xintercept = log(enet_logit$lambda.1se) , color = "red", linetype = "dashed")+
     theme_bw() 

data.frame(lambda= log(enet_logit$lambda), 
           miss= enet_logit$cvm, 
           missup= enet_logit$cvup, 
           misslo= enet_logit$cvlo) %>% 
  filter(lambda<=-2.5) %>%
   ggplot(aes(as.factor(lambda)))+
  geom_boxplot(aes(lower=misslo,
                   upper=missup,
                   middle=miss,
                   ymin=misslo-.001,
                   ymax=missup+0.001),
               stat="identity",position = position_dodge(width=0.4),width=0.3)+
  labs(title = "", x = "Log(Penalty)", y = "Classification Error") +
  theme_bw()





plot(enet_logit)

plot(enet_logit, xvar = "lambda")

coef(enet_logit$glmnet.fit, s = "lambda.1se")


 