###################
## Import Persons General 
## characteristic data
###################

library(pacman)

p_load(tidyverse, 
       rio)

###### clean all.


rm(list=ls())


root<- "C:\\Users\\Andres Felipe\\OneDrive - Universidad de los Andes\\Research Proyects\\ML_GEIH"

data<- paste0(root, "\\data")
raw<- paste0(data, "\\raw")
geih2012<- paste0(raw, "\\GEIH\\2012")




###################
## Importing Data
###################


df01<- import(paste0(geih2012,"\\Enero/01/Cabecera - Vivienda y Hogares.sav") )
df02<- import(paste0(geih2012,"\\Febrero/02/Cabecera - Vivienda y Hogares.sav") )
df03<- import(paste0(geih2012,"\\Marzo/03/Cabecera - Vivienda y Hogares.sav") )
df04<- import(paste0(geih2012,"\\Abril/04/Cabecera - Vivienda y Hogares.sav") )
df05<- import(paste0(geih2012,"\\Mayo/05/Cabecera - Vivienda y Hogares.sav") )
df06<- import(paste0(geih2012,"\\Junio/06/Cabecera - Vivienda y Hogares.sav") )
df07<- import(paste0(geih2012,"\\Julio/07/Cabecera - Vivienda y Hogares.sav") )
df08<- import(paste0(geih2012,"\\Agosto/08/Cabecera - Vivienda y Hogares.sav") )
df09<- import(paste0(geih2012,"\\Septiembre/09/Cabecera - Vivienda y Hogares.sav") )
df10<- import(paste0(geih2012,"\\Octubre/10/Cabecera - Vivienda y Hogares.sav") )
df11<- import(paste0(geih2012,"\\Noviembre/11/Cabecera - Vivienda y Hogares.sav") )
df12<- import(paste0(geih2012,"\\Diciembre/12/Cabecera - Vivienda y Hogares.sav") )

#########################
#        keep only 
#   variables in common
#########################

## names to lowercase

colnames(df01) <- tolower(colnames(df01))
colnames(df02) <- tolower(colnames(df02))
colnames(df03) <- tolower(colnames(df03))
colnames(df04) <- tolower(colnames(df04))
colnames(df05) <- tolower(colnames(df05))
colnames(df06) <- tolower(colnames(df06))
colnames(df07) <- tolower(colnames(df07))
colnames(df08) <- tolower(colnames(df08))
colnames(df09) <- tolower(colnames(df09))
colnames(df10) <- tolower(colnames(df10))
colnames(df11) <- tolower(colnames(df11))
colnames(df12) <- tolower(colnames(df12))


## get the variable names 

colnames01<- colnames(df01)
colnames02<- colnames(df02)
colnames03<- colnames(df03)
colnames04<- colnames(df04)
colnames05<- colnames(df05)
colnames06<- colnames(df06)
colnames07<- colnames(df07)
colnames08<- colnames(df08)
colnames09<- colnames(df09)
colnames10<- colnames(df10)
colnames11<- colnames(df11)
colnames12<- colnames(df12)

############ get the list of common variables

common<- Reduce(intersect, list(colnames01, 
                                colnames02,
                                colnames03,
                                colnames04,
                                colnames05,
                                colnames06,
                                colnames07,
                                colnames08,
                                colnames09,
                                colnames10,
                                colnames11,
                                colnames12))

df01<- df01 %>%  
  select(all_of(common))

df02<- df02 %>%  
   select(all_of(common))

df03<- df03 %>%  
   select(all_of(common))

df04<- df04 %>%  
   select(all_of(common))

df05<- df05 %>%  
   select(all_of(common))

df06<- df06 %>%  
   select(all_of(common))

df07<- df07 %>%  
   select(all_of(common))

df08<- df08 %>%  
   select(all_of(common))

df09<- df09 %>%  
   select(all_of(common))

df10<- df10 %>%  
   select(all_of(common))

df11<- df11 %>%  
   select(all_of(common))

df12<- df12 %>%  
   select(all_of(common))


##
df<- data.frame()

df<- df %>%
  rbind(df01)%>%
  rbind(df02)%>%
  rbind(df03)%>%
  rbind(df04)%>%
  rbind(df05)%>%
  rbind(df06)%>%
  rbind(df07)%>%
  rbind(df08)%>%
  rbind(df09)%>%
  rbind(df10)%>%
  rbind(df11)%>%
  rbind(df12)

rm(df01,
   df02,
   df03,
   df04, 
   df05,
   df06,
   df07, 
   df08,
   df09,
   df10,
   df11,
   df12)


##################
##  Selecting and renaming vars
##################

  

  house_ch<- df %>% 
  rename(nrooms=p5000, 
         h_usedrooms=p5010, 
         h_bathroom_type=p5020, 
         h_bathroom_shared=p5030,
         h_garbage_disposal=p5040,
         h_water_supp=p5050,
         h_household_ownership=p5090,
         h_imputed_rent01= p5130,
         h_imputed_rent02=p5140,
         h_size=p6008,
         h_type=p4000,
         h_walls=p4010,
         h_floors=p4020,
         h_energy_supp=p4030s1,
         h_gas_supp=p4030s2,
         h_sewerage_supp=p4030s3,
         h_aqueduct_supp=p4030s5,
         h_stratum=p4030s1a1,
         h_const_water_supp=p4040)
  

    ##### generate amenities 

    amenities<- c("p5210s1",  "p5210s10", "p5210s11",  "p5210s14" ,"p5210s15", "p5210s16",
               "p5210s17", "p5210s18", "p5210s19", "p5210s20", "p5210s21", "p5210s22",
               "p5210s24", "p5210s4", "p5210s5", "p5210s6", "p5210s7", "p5210s8", "p5210s9")  
    ## replace values for amenities
    house_ch<- house_ch %>% 
      mutate(across(all_of(amenities), ~ ifelse(. == 2, 0, .)))
    ## create sum of amenities 
    house_ch <- house_ch %>%
      mutate(h_amenities = rowSums(across(all_of(amenities))))
    ## rename amenities variables as h_amenities01..
    house_ch <- house_ch %>%
      rename_with(.cols = all_of(amenities), 
                  .fn = ~ str_c("h_amenities", str_pad(seq_along(.), 2, pad = "0")))
    ## select variables 
    house_ch <- house_ch %>%
      select(directorio, secuencia_p, starts_with("h_"))
    
    ## create   rent variable from imputed and actual.
    house_ch <- house_ch %>%
      mutate(h_imputed_rent=h_imputed_rent01) %>%
      mutate(h_imputed_rent= if_else(is.na(h_imputed_rent01)==TRUE,
                                           h_imputed_rent02, h_imputed_rent01 ))
    
    ## renaming the id variables.
    house_ch <- house_ch %>%
      rename( house=directorio,
              household=secuencia_p )
     
   
    ### WE STILL HAVE TO CONVERT VARIABLES TO FACTORS, 
    ## AND RECODE VARIABLES WITH VALUES 1 AND 2. 
    
    house_ch<- house_ch %>% 
      mutate(h_bathroom_type=factor(h_bathroom_type), 
             h_garbage_disposal=factor(h_garbage_disposal),
             h_water_supp=factor(h_water_supp),
             h_household_ownership=factor(h_household_ownership),
             h_type=factor(h_type),
             h_walls=factor(h_walls),
             h_floors=factor(h_floors),
             h_stratum=factor(h_stratum))
    
    dummy<- c("h_bathroom_shared", "h_energy_supp", "h_gas_supp", "h_sewerage_supp", 
                "h_aqueduct_supp", "h_const_water_supp" )
 
    ## replace values for other dummy variables
    house_ch<- house_ch %>% 
      mutate(across(all_of(dummy), ~ ifelse(. == 2, 0, .)))
    
    ## drop variables 
    house_ch<- house_ch %>% 
      select(-h_imputed_rent01, -h_imputed_rent02)
    
    
    ### save data
    
    out_path<- paste0(data, "\\clean_prediction\\2012")
    saveRDS( house_ch,  file= paste0(out_path, "\\house_general_characteristics_clean.rds"))
    