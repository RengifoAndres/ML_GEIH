
### Intermediate analysis.

library(pacman)

p_load(tidyverse, 
       rio)


## clean all. 
rm(list=ls())


root<- "C:\\Users\\Andres Felipe\\OneDrive - Universidad de los Andes\\Research Proyects\\ML_GEIH"

data<- paste0(root, "\\data")
raw<- paste0(data, "\\raw")
geih2012<- paste0(raw, "\\GEIH\\2012")
clean<-  paste0(data, "\\clean_prediction\\2012")

####### Looking a missing values


miss_graph<- function(data) {
  
  df_miss <- skimr:: skim(data) %>% select( skim_variable, n_missing)
  Nobs= nrow(data) 
  df_miss<- df_miss %>% mutate(p_missing= n_missing/Nobs)
  
  df_miss%>% 
    filter(p_missing>0)%>%
    ggplot(aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    coord_flip() +
    labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings")+ 
    theme(axis.text = element_text(size = 5)) +
    theme_bw() 
  
  
}
