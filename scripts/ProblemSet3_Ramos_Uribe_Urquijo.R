# Paula Ramos, Karen Uribe y Juan D. Urquijo
# update: 17-07-2022
###----------------- Project Set 3----------###

##### ---Limpiar Ambiente --- ######

rm(list = ls())

##### ---Cargar paquetes --- ######
require(pacman)
p_load(rio) # Librer?a para importar datos 
p_load(tidyverse) # Librer?a para limpiar datos
p_load(e1071) # Tiene la funci?n para calcular skewness
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librer?a para visualizar datos
p_load(scales) # Formato de los ejes en las gr?ficas
p_load(ggpubr) # Combinar gr?ficas
p_load(skimr, # summary data
       caret, # Classification And REgression Training
       rvest,
       stringr,
       dplyr,
       robotstxt
)


##############################Cargar los datos#################################
train<-readRDS("C:/Users/pau_9/Documents/GitHub/ProblemSet3_Ramos_Uribe_Urquijo/dataPS3/train.Rds")
test<-readRDS("C:/Users/pau_9/Documents/GitHub/ProblemSet3_Ramos_Uribe_Urquijo/dataPS3/test.Rds")

######Data######
###Creación de 4 variables extra


###Estadpisticas descriptivas y mapas


#####Modelos######



