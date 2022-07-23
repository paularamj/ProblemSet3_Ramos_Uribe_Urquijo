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
       robotstxt,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, #visualizción
       tmaptools, #geocode
       osmdata # Get OSM data
)

##############################Cargar los datos#################################
#setwd("C:/Users/pau_9/Documents/GitHub/ProblemSet3_Ramos_Uribe_Urquijo")
#setwd("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 3/ProblemSet3_Ramos_Uribe_Urquijo")
setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet3_Ramos_Uribe_Urquijo")

train<-readRDS("dataPS3/train.Rds")
test<-readRDS("dataPS3/test.Rds")

##########Explorción de los datos########
skim(train)
##Variables con mayor porcentaje de missing values (surface_covered, surface_total)
table(is.na(train$surface_covered))
table(is.na(train$surface_total))
##Datos de área en formato texto en la descripción

browseURL("https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf") #guia strings (tidyverse)

#Nueva variable de surface (rescatar mt2 en la descripcion)

train$description <- str_to_lower(train$description)
x1 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+metros" 
x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt2"
x3 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts2"
x4 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2" 
x5 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt"
x6 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts"

y1 <- "[:space:]+[:digit:]+[:space:]+metros" 
y2 <- "[:space:]+[:digit:]+[:space:]+mt2"
y3 <- "[:space:]+[:digit:]+[:space:]+mts2"
y4 <- "[:space:]+[:digit:]+[:space:]+m2" 
y5 <- "[:space:]+[:digit:]+[:space:]+mt"
y6 <- "[:space:]+[:digit:]+[:space:]+mts"

z1 <- "[:space:]+[:digit:]+metros" 
z2 <- "[:space:]+[:digit:]+mt2"
z3 <- "[:space:]+[:digit:]+mts2"
z4 <- "[:space:]+[:digit:]+m2" 
z5 <- "[:space:]+[:digit:]+mt"
z6 <- "[:space:]+[:digit:]+mts"

train =train %>% mutate(new_surface = str_extract(string = train$description,
                                 pattern =  paste0(x1,"|",x2,"|",x3,"|",x4,"|",x5,"|",x6,"|",
                                                   y1,"|",y2,"|",y3,"|",y4,"|",y5,"|",y6,"|",
                                                   z1,"|",z2,"|",z3,"|",z4,"|",z5,"|",z6)))

sum(table(train$new_surface)) ##Rescata 44732 obs con los tres patrones


####Creacion de Variables de la columna description (Minimo 2)

##Piso
x_1 <- "[:space:]+[:digit:]+[:space:]+piso" ##Patron 1 (Piso - Intuicion un piso mas alto cuesta mas)
train =train %>% mutate(piso = str_extract(string = train$description, pattern = x_1 ))
table(train$piso)
y_1 <- "[:space:]+[:digit:]+piso"
z_1 <- "[:space:]+piso+[:space:]+[:digit:]" 
y_2 <- "piso+[:space:]+[:digit:]"
z_2 <- "[:space:]+piso+[:space:]+[:digit:]+[:punct]"
train =train %>% mutate(piso= ifelse(is.na(piso)==T, 
                                 str_extract(string = train$description,
                                             pattern = paste0(y_1,"|",z_1, "|",y_2,"|",z_2)),piso))

sum(table(train$piso)) ##Rescata 15792 obs con los dos patrones

##Estrato
w1 <- "[:space:]+estrato+[:space:]+[:digit:]" ##Patron 1 (Estrato)
w2 <- "[:space:]+estrato+[:space:]+[:space:]+[:digit:]"
w3 <- "[:space:]+estrato+[:digit:]"
w4 <- "estrato+[:space:]+[:digit:]"


train =train %>% mutate(estrato = str_extract(string = train$description, pattern = paste0(w1,"|", w2,"|", w3,"|",w4)))
sum(table(train$estrato)) ##Solo se recuperan 9382

##########Data - Spatial########
db<- st_as_sf(x=train,coords=c("lon","lat"),crs=4326) ##Lectura de datos espaciales
leaflet() %>% addTiles() %>% addCircles(data=db)
class(db)

##Caja de coordenada que contiene el poligono de Chapinero - Bogotá
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)
train_chapinero <- st_crop(db, chapinero)

##Caja de coordenada que contiene el polígono de Poblado - Medellín
poblado <- getbb(place_name = "Comuna 14 - El Poblado", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") 
leaflet() %>% addTiles() %>% addPolygons(data=poblado)
train_poblado <- st_crop (db, poblado)

######Variables de OSM######
available_features() #Escoger features

###Variable: Bares##
#Chapinero
bar_chap <- opq(bbox = st_bbox(chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bar")
osm_bar_chap<- bar_chap %>% osmdata_sf()
bares_chapinero <- osm_bar_chap$osm_points %>% select(osm_id,amenity) ##points
#Poblado
bar_pob <- opq(bbox = st_bbox(poblado)) %>%
  add_osm_feature(key = "amenity", value = "bar")
osm_bar_pob<- bar_pob %>% osmdata_sf()
bares_poblado <- osm_bar_pob$osm_points %>% select(osm_id,amenity)  ##points

###Variable: Estaciones de bus
#Chapinero
est_bus_chap <- opq(bbox = st_bbox(chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bus_station")
#lista de los objetos
osm_eb_chap<- est_bus_chap%>% osmdata_sf()
estaciones_chapinero <- osm_eb_chap$osm_points %>% select(osm_id,amenity) #points

#Poblado
est_bus_pob <- opq(bbox = st_bbox(poblado)) %>%
  add_osm_feature(key = "amenity", value = "bus_station")
osm_eb_pob<- est_bus_pob%>% osmdata_sf()
estaciones_poblado <- osm_eb_pob$osm_points %>% select(osm_id,amenity) #points

###Variable: Parques
#Chapinero
parques_chap <- opq(bbox = st_bbox(chapinero)) %>%
  add_osm_feature(key = "leisure", value = "park")
osm_park_cha<- parques_chap %>% osmdata_sf()
parques_chapinero<- osm_park_cha$osm_polygons %>% select(osm_id,leisure) #poligonos 

#Poblado
parques_pob <- opq(bbox = st_bbox(poblado)) %>%
  add_osm_feature(key = "leisure", value = "park")
osm_park_pob<- parques_pob %>% osmdata_sf() #lista de elementos (points, lines, poligonos)
parques_poblado <- osm_park_pob$osm_polygons %>% select(osm_id,leisure) #poligonos
 
###Variable: Bancos
#Chapinero
ban_chp = opq(bbox = st_bbox(chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bank")
osm_ban_chap<- ban_chp  %>% osmdata_sf() #lista de elementos (points, lines, poligonos)
bancos_chapinero <- osm_ban_chap$osm_points %>% select(osm_id,amenity) #points

#Poblado
ban_pob = opq(bbox = st_bbox(poblado)) %>%
  add_osm_feature(key = "amenity", value = "bank")
osm_ban_pob<- ban_pob %>% osmdata_sf() #lista de elementos (points, lines, poligonos)
bancos_poblado <- osm_ban_pob$osm_points %>% select(osm_id,amenity)  #points

##Inspeccion grafica
library(sf)

leaflet() %>% addTiles() %>% 
  addCircleMarkers(data=db , col="red", weight=2)%>% #datos
  addPolygons(data=parques_chapinero , col="green") %>%  #parques
  addCircles(data=bares_chapinero , col="blue") %>%  #bares
  addCircles(data=bancos_chapinero , col="black" , weight=2)%>% # bancos
  addCircles(data=estaciones_chapinero , col="yellow") %>%   #estaciones de bus
  addPolygons(data=parques_poblado , col="green") %>%  #parques
  addCircles(data=bares_poblado , col="blue") %>%  #bares
  addCircles(data=bancos_poblado , col="black" , weight=2)%>% # bancos
  addCircles(data=estaciones_poblado , col="yellow")  #estaciones de bus

##Lectura de archivos shp del DANE

library(sf)

Bogota_mzn<-  st_read("dataPS3/Manzana Bog/MGN_URB_MANZANA.shp")
Antioquia_mzn <- st_read("dataPS3/Manzana Antioquia/MGN_URB_MANZANA.shp")

Medellin_mzn<- Antioquia_mzn[Antioquia_mzn$MPIO_CCDGO == "05001", ]
class(Medellin_mzn)

#Creacion de variables OSM 

##Afinar las transformaciones
st_crs(Bogota_mzn) == st_crs(train_chapinero)
st_crs(Medellin_mzn) == st_crs(train_poblado)
#Esto lo que hace es recuperar el sistema de referencia de coordenadas del objeto train_chapinero y del objeto train_poblado

##Unir dos conjuntos de datos basados en la geometria
housing_chapinero <- st_join(x=train_chapinero , y=Bogota_mzn) #Validación se mantienen las 15165 obs
housing_poblado <- st_join(x=train_poblado , y=Medellin_mzn) #Validación se mantienen las 1677 obs

###Variable: Distancia a bares###
##Chapinero
dist_bar_chp <- st_distance(x=housing_chapinero, y=bares_chapinero)
dist_bar_chp
min_dist_c <- apply(dist_bar_chp , 1 , min)
min_dist
housing_chapinero$dist_bar <- min_dist_c

##Poblado
dist_bar_pob <- st_distance(x=housing_poblado, y=bares_poblado)
dist_bar_pob
min_dist_p <- apply(dist_bar_pob , 1 , min) #distancia mínima a cada bar
min_dist_p
housing_poblado$dist_bar <- min_dist_p

###Variable: Distancia a parques###
##Chapinero
dist_pq_chp <- st_distance(x=housing_chapinero, y=parques_chapinero)
dist_pq_chp
min_dist_p_c <- apply(dist_pq_chp , 1 , min)
min_dist_p_c
housing_chapinero$dist_parque <- min_dist_p_c

##Poblado
dist_pq_pob <- st_distance(x=housing_poblado, y=parques_poblado)
dist_pq_pob
min_dist_p_p <- apply(dist_pq_pob , 1 , min)
min_dist_p_p
housing_poblado$dist_parque <- min_dist_p_p

##Variable: Distancia a bancos###
##Chapinero
dist_bc_chp <- st_distance(x=housing_chapinero, y=bancos_chapinero)
dist_bc_chp
min_dist_b_c <- apply(dist_bc_chp , 1 , min)
min_dist_b_c
housing_chapinero$dist_banco <- min_dist_b_c

##Poblado
dist_bc_pob <- st_distance(x=housing_poblado, y=parques_poblado)
dist_bc_pob
min_dist_p_p <- apply(dist_pq_pob , 1 , min)
min_dist_p_p
housing_poblado$dist_banco <- min_dist_p_p

##Variable: Distancia a estaciones de bus###
##Chapinero
dist_eb_chp <- st_distance(x=housing_chapinero, y=estaciones_chapinero)
dist_eb_chp
min_dist_eb_c <- apply(dist_eb_chp , 1 , min)
min_dist_eb_c
housing_chapinero$dist_estacionbus <- min_dist_eb_c

##Poblado
dist_eb_pob <- st_distance(x=housing_poblado, y=estaciones_poblado)
dist_eb_pob
min_dist_eb_p <- apply(dist_eb_pob , 1 , min)
min_dist_eb_p
housing_poblado$dist_estacionbus<- min_dist_eb_p

# Crear variable new_surface con informacion extraida de la descripcion

## Chapinero

housing_chapinero$ns<-housing_chapinero$new_surface
housing_chapinero$st<-housing_chapinero$surface_total

housing_chapinero$new_surface_2<-str_remove_all(housing_chapinero$new_surface,"metros")
housing_chapinero$new_surface_2<-str_remove_all(housing_chapinero$new_surface_2,"mt2")
housing_chapinero$new_surface_2<-str_remove_all(housing_chapinero$new_surface_2,"mts2")
housing_chapinero$new_surface_2<-str_remove_all(housing_chapinero$new_surface_2,"m2")
housing_chapinero$new_surface_2<-str_remove_all(housing_chapinero$new_surface_2,"mt")
housing_chapinero$new_surface_2<-str_remove_all(housing_chapinero$new_surface_2,"mts")

housing_chapinero$new_surface_2<-str_remove_all(housing_chapinero$new_surface_2, "[\n]")
housing_chapinero$new_surface_2<-str_remove_all(housing_chapinero$new_surface_2, "[ ]")
housing_chapinero$new_surface_2<-str_replace_all(housing_chapinero$new_surface_2, ",", ".")

#Lidiamos con . y , en la descipcion

housing_chapinero$new_surface_2<-str_replace_all(housing_chapinero$new_surface_2, "[:digit:]+[.]+[:digit:]+[:digit:]+[:digit:]",
                                                 str_remove_all(housing_chapinero$new_surface_2, "[.]"))



housing_chapinero$new_surface_2<-as.numeric(housing_chapinero$new_surface_2)

summary(housing_chapinero$new_surface_2)

## Poblado

housing_poblado$ns<-housing_poblado$new_surface
housing_poblado$st<-housing_poblado$surface_total

#Elimino todos los patrones de letras para poder extraer el numero de metros unicamente

housing_poblado$new_surface_2<-str_remove_all(housing_poblado$new_surface,"metros")
housing_poblado$new_surface_2<-str_remove_all(housing_poblado$new_surface_2,"mt2")
housing_poblado$new_surface_2<-str_remove_all(housing_poblado$new_surface_2,"mts2")
housing_poblado$new_surface_2<-str_remove_all(housing_poblado$new_surface_2,"m2")
housing_poblado$new_surface_2<-str_remove_all(housing_poblado$new_surface_2,"mt")
housing_poblado$new_surface_2<-str_remove_all(housing_poblado$new_surface_2,"mts")

#Elimina espacio y caracteres especiales
housing_poblado$new_surface_2<-str_remove_all(housing_poblado$new_surface_2, "[\n]")
housing_poblado$new_surface_2<-str_remove_all(housing_poblado$new_surface_2, "[ ]")
housing_poblado$new_surface_2<-str_replace_all(housing_poblado$new_surface_2, ",", ".")

#Lidiamos con . y , en la descipcion

housing_poblado$new_surface_2<-str_replace_all(housing_poblado$new_surface_2, "[:digit:]+[.]+[:digit:]+[:digit:]+[:digit:]",
                                               str_remove_all(housing_poblado$new_surface_2, "[.]"))

#Convertir en numero
housing_poblado$new_surface_2<-as.numeric(housing_poblado$new_surface_2)

summary(housing_poblado$new_surface_2)


#Imputar Variables rescatadas de la descripci�n a surface_total

# Chapinero


housing_chapinero = housing_chapinero %>% group_by(MANZ_CCNCT) %>% 
  mutate(surface_total2 = ifelse(is.na(surface_total),
                            yes = new_surface_2,
                            no = surface_total))



# Poblado


housing_poblado = housing_poblado %>% group_by(MANZ_CCNCT) %>% 
  mutate(surface_total2 = ifelse(is.na(surface_total),
                                 yes = new_surface_2,
                                 no = surface_total))




## ===Medianas manzanas cercanas la mediana del area ===##

## load data

colnames(housing_chapinero)
colnames(housing_poblado)

## Mediana de la Manzana

# Chapinero
housing_chapinero = housing_chapinero %>%
  group_by(MANZ_CCNCT) %>%
  mutate(surface_mediana=median(surface_total,na.rm=T))

table(is.na(housing_chapinero$surface_total)) ## Tenemos 11.851 NAs

table(is.na(housing_chapinero$surface_total),
      is.na(housing_chapinero$surface_mediana)) # logramos recuperar 1.389


# Poblado
housing_poblado = housing_poblado %>%
  group_by(MANZ_CCNCT) %>%
  mutate(surface_mediana=median(surface_total,na.rm=T))

table(is.na(housing_poblado$surface_total)) ## Tenemos 925 NAs

table(is.na(housing_poblado$surface_total),
      is.na(housing_poblado$surface_mediana)) # logramos recuperar 108


#Imputar Medianas Manzanas

# Chapinero


housing_chapinero = housing_chapinero %>% group_by(MANZ_CCNCT) %>% 
  mutate(surface_total2 = ifelse(is.na(surface_total2),
                                 yes = surface_mediana,
                                 no = surface_total2))



# Poblado


housing_poblado = housing_poblado %>% group_by(MANZ_CCNCT) %>% 
  mutate(surface_total2 = ifelse(is.na(surface_total2),
                                 yes = surface_mediana,
                                 no = surface_total2))


#-----Volvemos a validar NAs

#Chapinero

table(is.na(housing_chapinero$surface_total2))

#Poblado

table(is.na(housing_poblado$surface_total2))

#Eliminamos NAs que no pudimos capturar

#Chapinero

housing_chapinero <- housing_chapinero[!is.na(housing_chapinero$surface_total2),]

#Poblado

housing_poblado <- housing_poblado[!is.na(housing_poblado$surface_total2),]


## ======Con criterio experto, revisamos los datos que tengan sentido respecto al area total ===== ##

#Chapinero

quantile(housing_chapinero$surface_total2, 0.9997)

summary(housing_chapinero$surface_total2)

#Eliminamos los outliers, quitando los aptos mayores a 2000

housing_chapinero<- housing_chapinero %>% 
  filter(surface_total2 <=2000)

#Eliminamos los outliers, esta vez dejando unicamente los aptos con 15 mts2 o mas 

housing_chapinero<- housing_chapinero %>% 
  filter(surface_total2 >= 15)

ggplot(housing_chapinero, aes(x=surface_total2)) +
  geom_boxplot(fill= "darkblue", alpha=0.4)

summary(housing_chapinero$surface_total2)

#Poblado

quantile(housing_poblado$surface_total2, 0.99)

summary(housing_poblado$surface_total2)

#Eliminamos los outliers, quitando los aptos mayores a 2000

housing_poblado<- housing_poblado %>% 
  filter(surface_total2 <=2000)

#Eliminamos los outliers, esta vez dejando unicamente los aptos con 10 mts2 o mas 

housing_poblado<- housing_poblado %>% 
  filter(surface_total2 >= 15)

ggplot(housing_poblado, aes(x=surface_total2)) +
  geom_boxplot(fill= "darkblue", alpha=0.4)

summary(housing_poblado$surface_total2)



#####Modelos######
#Predicción del precio


##Arboles de decisión
# pload(rpart)
# 
# cp_alpha<-seq(from = 0, to = 0.1, length = 10)
# fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
# ctrl<- trainControl(method = "cv",
#                     number = 5,
#                     summaryFunction = fiveStats,
#                     classProbs = TRUE,
#                     verbose=FALSE,
#                     savePredictions = T)
# View(housing_chapinero)
# set.seed(123)
# housing_chapinero$property_type  <- as.factor(housing_chapinero$property_type)
# tree <- train( price ~ property_type,
#                data = housing_chapinero,
#                method = "rpart",
#                trControl = ctrl,
#                parms=list(split='Gini'),
#                #tuneGrid = expand.grid(cp = cp_alpha)#,
#                tuneLength=200,
# )


