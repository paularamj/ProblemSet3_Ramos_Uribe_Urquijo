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
       osmdata,
       expss # Get OSM data
)

##############################Cargar los datos#################################
#setwd("C:/Users/pau_9/Documents/GitHub/ProblemSet3_Ramos_Uribe_Urquijo")
#setwd("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 3/ProblemSet3_Ramos_Uribe_Urquijo")
setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet3_Ramos_Uribe_Urquijo")

train<-readRDS("dataPS3/train.Rds")
test<-readRDS("dataPS3/test.Rds")

################--- Unir bases----#########################
train$base <- "Train"
test$base <- "Test"
BASE <- bind_rows(train,test)


##########Explorción de los datos########
skim(BASE)
##Variables con mayor porcentaje de missing values (surface_covered, surface_total)
table(is.na(BASE$surface_covered))
table(is.na(BASE$surface_total))
##Datos de área en formato texto en la descripción

browseURL("https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf") #guia strings (tidyverse)

#Nueva variable de surface (rescatar mt2 en la descripcion)

BASE$description <- str_to_lower(BASE$description)
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

BASE =BASE %>% mutate(new_surface = str_extract(string = BASE$description,
                                 pattern =  paste0(x1,"|",x2,"|",x3,"|",x4,"|",x5,"|",x6,"|",
                                                   y1,"|",y2,"|",y3,"|",y4,"|",y5,"|",y6,"|",
                                                   z1,"|",z2,"|",z3,"|",z4,"|",z5,"|",z6)))

sum(table(BASE$new_surface)) ##Rescata 44732 obs con los tres patrones


####Creacion de Variables de la columna description (Minimo 2)

##Piso
x_1 <- "[:space:]+[:digit:]+[:space:]+piso" ##Patron 1 (Piso - Intuicion un piso mas alto cuesta mas)
BASE=BASE %>% mutate(piso = str_extract(string = BASE$description, pattern = x_1 ))
table(BASE$piso)
y_1 <- "[:space:]+[:digit:]+piso"
z_1 <- "[:space:]+piso+[:space:]+[:digit:]" 
y_2 <- "piso+[:space:]+[:digit:]"
z_2 <- "[:space:]+piso+[:space:]+[:digit:]+[:punct]"
BASE =BASE %>% mutate(piso= ifelse(is.na(piso)==T, 
                                 str_extract(string = BASE$description,
                                             pattern = paste0(y_1,"|",z_1, "|",y_2,"|",z_2)),piso))

sum(table(BASE$piso)) ##Rescata 15792 obs con los dos patrones

##Estrato
w1 <- "[:space:]+estrato+[:space:]+[:digit:]" ##Patron 1 (Estrato)
w2 <- "[:space:]+estrato+[:space:]+[:space:]+[:digit:]"
w3 <- "[:space:]+estrato+[:digit:]"
w4 <- "estrato+[:space:]+[:digit:]"


BASE =BASE %>% mutate(estrato = str_extract(string = BASE$description, pattern = paste0(w1,"|", w2,"|", w3,"|",w4)))
sum(table(BASE$estrato)) ##Solo se recuperan 9382

##########Data - Spatial########
db<- st_as_sf(x=BASE,coords=c("lon","lat"),crs=4326) ##Lectura de datos espaciales
leaflet() %>% addTiles() %>% addCircles(data=db)
class(db)

##Caja de coordenadas que contiene el poligono de Chapinero - Bogotá
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)
BASE_chapinero <- st_crop(db, chapinero)

##Caja de coordenadas que contiene el poligono de Poblado - Medellin
poblado <- getbb(place_name = "Comuna 14 - El Poblado", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") 
leaflet() %>% addTiles() %>% addPolygons(data=poblado)
BASE_poblado <- st_crop (db, poblado)

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

###Variable: Seguridad (Policia)
#Chapinero
police_chp = opq(bbox = st_bbox(chapinero)) %>%
  add_osm_feature(key = "amenity", value = "police")
osm_police_chap<- police_chp  %>% osmdata_sf() #lista de elementos (points, lines, poligonos)
police_chapinero <- osm_police_chap$osm_points %>% select(osm_id,amenity) #points

#Poblado
police_pob = opq(bbox = st_bbox(poblado)) %>%
  add_osm_feature(key = "amenity", value = "police")
osm_police_pob<- police_pob %>% osmdata_sf() #lista de elementos (points, lines, poligonos)
police_poblado <- osm_police_pob$osm_points %>% select(osm_id,amenity)  #points

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
st_crs(Bogota_mzn) == st_crs(BASE_chapinero)
st_crs(Medellin_mzn) == st_crs(BASE_poblado)
#Esto lo que hace es recuperar el sistema de referencia de coordenadas del objeto BASE_chapinero y del objeto BASE_poblado

##Unir dos conjuntos de datos basados en la geometria
housing_chapinero <- st_join(x=BASE_chapinero , y=Bogota_mzn) #Validación se mantienen las 15165 obs
housing_poblado <- st_join(x=BASE_poblado , y=Medellin_mzn) #Validación se mantienen las 1677 obs

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

##Variable: Distancia a estaciones de Policia###
##Chapinero
dist_police_chp <- st_distance(x=housing_chapinero, y=police_chapinero)
dist_police_chp
min_dist_police_c <- apply(dist_police_chp , 1 , min)
min_dist_police_c
housing_chapinero$dist_police <- min_dist_police_c

##Poblado
dist_police_pob <- st_distance(x=housing_poblado, y=police_poblado)
dist_police_pob
min_dist_police_p <- apply(dist_police_pob , 1 , min)
min_dist_police_p
housing_poblado$dist_police<- min_dist_police_p


#=========================== DATA CLEANING ===========================================#


########## Revisamos variable AREA #############

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


#Imputar Variables rescatadas de la descripci?n a surface_total

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


########## Revisamos variable PISO #############

#================== Crear variable Piso con la informaci?n extraida de la descripcion ====================#

## Chapinero

housing_chapinero$new_piso<-str_remove_all(housing_chapinero$piso,"piso")
housing_chapinero$new_piso<-str_remove_all(housing_chapinero$new_piso, "[\n]")

housing_chapinero$new_piso<-as.numeric(housing_chapinero$new_piso)

summary(housing_chapinero$new_piso)

## Poblado

housing_poblado$new_piso<-str_remove_all(housing_poblado$piso,"piso")
housing_poblado$new_piso<-str_remove_all(housing_poblado$new_piso, "[\n]")

housing_poblado$new_piso<-as.numeric(housing_poblado$new_piso)

summary(housing_poblado$new_piso)

# ===Medianas manzanas cercanas mediana del piso ===##

## load data

colnames(housing_chapinero)
colnames(housing_poblado)

## Mediana de la Manzana

# Chapinero
housing_chapinero = housing_chapinero %>%
  group_by(MANZ_CCNCT) %>%
  mutate(piso_mediana=median(new_piso,na.rm=T))

table(is.na(housing_chapinero$new_piso)) ## Tenemos 12.379 NAs

table(is.na(housing_chapinero$new_piso),
      is.na(housing_chapinero$piso_mediana)) # logramos recuperar 1.830


# Poblado
housing_poblado = housing_poblado %>%
  group_by(MANZ_CCNCT) %>%
  mutate(piso_mediana=median(new_piso,na.rm=T))

table(is.na(housing_poblado$new_piso)) ## Tenemos 1.146 NAs

table(is.na(housing_poblado$new_piso),
      is.na(housing_poblado$piso_mediana)) # logramos recuperar 210


#Imputar Medianas Manzanas

# Chapinero

housing_chapinero = housing_chapinero %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_piso_vf = ifelse(is.na(new_piso),
                                 yes = piso_mediana,
                                 no = new_piso))

# Poblado

housing_poblado = housing_poblado %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_piso_vf = ifelse(is.na(new_piso),
                              yes = piso_mediana,
                              no = new_piso))

#-----Volvemos a validar NAs

#Chapinero

table(is.na(housing_chapinero$new_piso_vf))

#Poblado

table(is.na(housing_poblado$new_piso_vf))


#Eliminamos NAs que no pudimos capturar

#Chapinero

housing_chapinero <- housing_chapinero[!is.na(housing_chapinero$new_piso_vf),]

#Poblado

housing_poblado <- housing_poblado[!is.na(housing_poblado$new_piso_vf),]

## ======Con criterio experto, revisamos los datos que tengan sentido del piso ===== ##

#Chapinero

quantile(housing_chapinero$new_piso_vf, 0.99)

summary(housing_chapinero$new_piso_vf)

#Eliminamos los outliers, quitando los pisos mayores a 13

housing_chapinero<- housing_chapinero %>% 
  filter(new_piso_vf <=13)

ggplot(housing_chapinero, aes(x=new_piso_vf)) +
  geom_boxplot(fill= "darkblue", alpha=0.4)

summary(housing_chapinero$new_piso_vf)


#Poblado

quantile(housing_poblado$new_piso_vf, 0.99)

summary(housing_poblado$new_piso_vf)

#Eliminamos los outliers, quitando los pisos mayores a 26

housing_poblado<- housing_poblado %>% 
  filter(new_piso_vf <=26)

ggplot(housing_poblado, aes(x=new_piso_vf)) +
  geom_boxplot(fill= "darkblue", alpha=0.4)

summary(housing_poblado$new_piso_vf)


########## Revisamos variable ESTRATO #############

#================== Crear variable Estrato con la informaci?n extraida de la descripcion ====================#

## Chapinero

housing_chapinero$new_estrato<-str_remove_all(housing_chapinero$estrato,"estrato")
housing_chapinero$new_estrato<-str_remove_all(housing_chapinero$new_estrato, "[\n]")
housing_chapinero$new_estrato<-as.numeric(housing_chapinero$new_estrato)

summary(housing_chapinero$new_estrato)

## Poblado

housing_poblado$new_estrato<-str_remove_all(housing_poblado$estrato,"estrato")
housing_poblado$new_estrato<-str_remove_all(housing_poblado$new_estrato, "[\n]")
housing_poblado$new_estrato<-as.numeric(housing_poblado$new_estrato)

summary(housing_poblado$new_estrato)

### Decidimos realizar la imputaci?n con la info del CENSO del DANE

##=== Cargar info de CENSO data para imputar estrato ===##

## load data
mnz_censo = import("http://eduard-martinez.github.io/data/fill-gis-vars/mnz_censo.rds")

## about data
browseURL("https://eduard-martinez.github.io/teaching/meca-4107/7-censo.txt")

#PARA BOGOTA

## load data bog
mgn_bog = import("dataPS3/Censo Bog/CNPV2018_MGN_A2_11.CSV")
colnames(mgn_bog)
distinct_all(mgn_bog[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

hog_bog = import("dataPS3/Censo Bog/CNPV2018_2HOG_A2_11.CSV")
colnames(hog_bog)
distinct_all(hog_bog[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA","H_NROHOG")]) %>% nrow()

viv_bog = import("dataPS3/Censo Bog/CNPV2018_1VIV_A2_11.CSV") 
colnames(viv_bog)
distinct_all(viv_bog[,c("COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

## join data
viv_hog_bog = left_join(hog_bog,viv_bog,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))
table(is.na(viv_hog_bog$VA1_ESTRATO))

data_bog = left_join(viv_hog_bog,mgn_bog,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
table(is.na(data_bog$VA1_ESTRATO))

## select vars
H_NRO_CUARTOS = "N?mero de cuartos en total"
HA_TOT_PER = "Total personas en el hogar"
V_TOT_HOG = "Total de hogares en la vivienda"
VA1_ESTRATO = "Estrato de la vivienda (seg?n servicio de energ?a)"
COD_DANE_ANM = "Codigo DANE de manzana"

db_bog = data_bog %>% select(COD_DANE_ANM,H_NRO_CUARTOS,HA_TOT_PER,V_TOT_HOG,VA1_ESTRATO)

## summary data
df_bog = db_bog %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(med_H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), 
            sum_HA_TOT_PER=sum(HA_TOT_PER,na.rm=T), 
            med_V_TOT_HOG=median(V_TOT_HOG,na.rm=T),
            med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

## export data
export(df_bog,"dataPS3/Censo Bog/mnz_censo_bog.rds")


## Unimos las bases de Housing Chapinero y la del Censo de Bogota
colnames(df_bog)
colnames(housing_chapinero)

df_bog<-rename(df_bog, MANZ_CCNCT = COD_DANE_ANM)

housing_chapinero = left_join(housing_chapinero,df_bog,by=c("MANZ_CCNCT"))

#PARA ANTIOQUIA

## load data ant
mgn_ant = import("dataPS3/Censo Antioquia/CNPV2018_MGN_A2_05.CSV")
colnames(mgn_ant)
distinct_all(mgn_ant[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

hog_ant = import("dataPS3/Censo Antioquia/CNPV2018_2HOG_A2_05.CSV")
colnames(hog_ant)
distinct_all(hog_ant[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA","H_NROHOG")]) %>% nrow()

viv_ant = import("dataPS3/Censo Antioquia/CNPV2018_1VIV_A2_05.CSV") 
colnames(viv_ant)
distinct_all(viv_ant[,c("COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

## join data
viv_hog_ant = left_join(hog_ant,viv_ant,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))
table(is.na(viv_hog_ant$VA1_ESTRATO))

data_ant = left_join(viv_hog_ant,mgn_ant,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
table(is.na(data_ant$VA1_ESTRATO))

## select vars
H_NRO_CUARTOS = "N?mero de cuartos en total"
HA_TOT_PER = "Total personas en el hogar"
V_TOT_HOG = "Total de hogares en la vivienda"
VA1_ESTRATO = "Estrato de la vivienda (seg?n servicio de energ?a)"
COD_DANE_ANM = "Codigo DANE de manzana"

db_ant = data_ant %>% select(COD_DANE_ANM,H_NRO_CUARTOS,HA_TOT_PER,V_TOT_HOG,VA1_ESTRATO)

## summary data
df_ant = db_ant %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(med_H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), 
            sum_HA_TOT_PER=sum(HA_TOT_PER,na.rm=T), 
            med_V_TOT_HOG=median(V_TOT_HOG,na.rm=T),
            med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

## export data
export(df_ant,"dataPS3/Censo Antioquia/mnz_censo_ant.rds")


## Unimos las bases de Housing Poblado y la del Censo de Antioquia
colnames(df_ant)
colnames(housing_poblado)

df_ant<-rename(df_ant, MANZ_CCNCT = COD_DANE_ANM)

housing_poblado = left_join(housing_poblado,df_ant,by=c("MANZ_CCNCT"))


# ===Medianas manzanas cercanas mediana del estrato ===##

## load data

colnames(housing_chapinero)
colnames(housing_poblado)

## Mediana de la Manzana

# Chapinero
housing_chapinero = housing_chapinero %>%
  group_by(MANZ_CCNCT) %>%
  mutate(estrato_mediana=median(new_estrato,na.rm=T))

table(is.na(housing_chapinero$new_estrato)) ## Tenemos 11.576 NAs

table(is.na(housing_chapinero$new_estrato),
      is.na(housing_chapinero$estrato_mediana)) # logramos recuperar 2.329


# Poblado
housing_poblado = housing_poblado %>%
  group_by(MANZ_CCNCT) %>%
  mutate(estrato_mediana=median(new_estrato,na.rm=T))

table(is.na(housing_poblado$new_estrato)) ## Tenemos 958 NAs

table(is.na(housing_poblado$new_estrato),
      is.na(housing_poblado$estrato_mediana)) # logramos recuperar 48


#Imputar Medianas Manzanas del estrato con info del censo

# Chapinero


housing_chapinero = housing_chapinero %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_estrato_vf = ifelse(is.na(new_estrato),
                              yes = med_VA1_ESTRATO,
                              no = new_estrato))


# Poblado

housing_poblado = housing_poblado %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_estrato_vf = ifelse(is.na(new_estrato),
                                 yes = med_VA1_ESTRATO,
                                 no = new_estrato))

#Imputar Medianas Manzanas del estrato con info de las manzanas del DANE

# Chapinero

housing_chapinero = housing_chapinero %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_estrato_vf = ifelse(is.na(new_estrato_vf),
                                 yes = estrato_mediana,
                                 no = new_estrato_vf))

# Poblado

housing_poblado = housing_poblado %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_estrato_vf = ifelse(is.na(new_estrato_vf),
                                 yes = estrato_mediana,
                                 no = new_estrato_vf))

#-----Volvemos a validar NAs

#Chapinero

table(is.na(housing_chapinero$new_estrato_vf))

#Poblado

table(is.na(housing_poblado$new_estrato_vf))


#Eliminamos NAs que no pudimos capturar

#Chapinero

housing_chapinero <- housing_chapinero[!is.na(housing_chapinero$new_estrato_vf),]

#Poblado

housing_poblado <- housing_poblado[!is.na(housing_poblado$new_estrato_vf),]

########## Revisamos variable CUARTOS #############

#================== Imputar NAs cuartos con info del censo ====================#


#Imputar Medianas Manzanas de los cuartos con info del censo

# Chapinero

housing_chapinero = housing_chapinero %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_cuartos_vf = ifelse(is.na(rooms),
                                 yes = med_H_NRO_CUARTOS,
                                 no = rooms))

# Poblado

housing_poblado = housing_poblado %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_cuartos_vf = ifelse(is.na(rooms),
                                 yes = med_H_NRO_CUARTOS,
                                 no = rooms))

## Mediana de la Manzana

# Chapinero
housing_chapinero = housing_chapinero %>%
  group_by(MANZ_CCNCT) %>%
  mutate(rooms_mediana=median(rooms,na.rm=T))

table(is.na(housing_chapinero$rooms)) ## Tenemos 5572 NAs

table(is.na(housing_chapinero$rooms),
      is.na(housing_chapinero$rooms_mediana)) # logramos recuperar 66


# Poblado
housing_poblado = housing_poblado %>%
  group_by(MANZ_CCNCT) %>%
  mutate(rooms_mediana=median(rooms,na.rm=T))

table(is.na(housing_poblado$rooms)) ## Tenemos 915 NAs

table(is.na(housing_poblado$rooms),
      is.na(housing_poblado$rooms_mediana)) # logramos recuperar 10

#Imputar Medianas Manzanas del # de cuartos con info de las manzanas del DANE

# Chapinero


housing_chapinero = housing_chapinero %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_cuartos_vf = ifelse(is.na(new_cuartos_vf),
                                 yes = rooms_mediana,
                                 no = new_cuartos_vf))

# Poblado


housing_poblado = housing_poblado %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_cuartos_vf = ifelse(is.na(new_cuartos_vf),
                                 yes = rooms_mediana,
                                 no = new_cuartos_vf))


########## Revisamos variable BA?OS #############

#================== Imputar NAs ba?os con info manzanas del DANE ====================#

## Mediana de la Manzana

# Chapinero
housing_chapinero = housing_chapinero %>%
  group_by(MANZ_CCNCT) %>%
  mutate(banos_mediana=median(bathrooms,na.rm=T))

table(is.na(housing_chapinero$bathrooms)) ## Tenemos 2641 NAs

table(is.na(housing_chapinero$bathrooms),
      is.na(housing_chapinero$banos_mediana)) # logramos recuperar 117


# Poblado
housing_poblado = housing_poblado %>%
  group_by(MANZ_CCNCT) %>%
  mutate(banos_mediana=median(bathrooms,na.rm=T))

table(is.na(housing_poblado$bathrooms)) ## Tenemos 375 NAs

table(is.na(housing_poblado$bathrooms),
      is.na(housing_poblado$banos_mediana)) # logramos recuperar 4

#Imputar Medianas Manzanas del # de ba?os con info de las manzanas del DANE

# Chapinero


housing_chapinero = housing_chapinero %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_banos_vf = ifelse(is.na(bathrooms),
                                 yes = banos_mediana,
                                 no = bathrooms))

# Poblado

housing_poblado = housing_poblado %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_banos_vf = ifelse(is.na(bathrooms),
                                 yes = banos_mediana,
                                 no = bathrooms))

#-----Volvemos a validar NAs

#Chapinero

table(is.na(housing_chapinero$new_banos_vf))

#Poblado

table(is.na(housing_poblado$new_banos_vf))

#Eliminamos NAs que no pudimos capturar

#Chapinero

housing_chapinero <- housing_chapinero[!is.na(housing_chapinero$new_banos_vf),]

#Poblado

housing_poblado <- housing_poblado[!is.na(housing_poblado$new_banos_vf),]


#======================= SELECCION DE VARIABLES ======================#

#Chapinero
colnames(housing_chapinero)
colnames(housing_poblado)

#variables: new_piso_vf, new_estrato_vf, new_cuartos_vf, surface_total2, dist_bar, dist_parque, dist_banco,
          #  dist_estacionbus, dist_police, price, bathrooms, property_type

#VALIDAMOS QUE NO TENEMOS NAs en las variables seleccionadas

sum(is.na(housing_chapinero$new_piso_vf))
sum(is.na(housing_chapinero$new_estrato_vf))
sum(is.na(housing_chapinero$new_cuartos_vf))
sum(is.na(housing_chapinero$surface_total2))
sum(is.na(housing_chapinero$dist_bar))
sum(is.na(housing_chapinero$dist_parque))
sum(is.na(housing_chapinero$dist_banco))
sum(is.na(housing_chapinero$dist_estacionbus))
sum(is.na(housing_chapinero$dist_police))
sum(is.na(housing_chapinero$price))
sum(is.na(housing_chapinero$new_banos_vf))
sum(is.na(housing_chapinero$property_type))


sum(is.na(housing_poblado$new_piso_vf))
sum(is.na(housing_poblado$new_estrato_vf))
sum(is.na(housing_poblado$new_cuartos_vf))
sum(is.na(housing_poblado$surface_total2))
sum(is.na(housing_poblado$dist_bar))
sum(is.na(housing_poblado$dist_parque))
sum(is.na(housing_poblado$dist_banco))
sum(is.na(housing_poblado$dist_estacionbus))
sum(is.na(housing_poblado$dist_police))
sum(is.na(housing_poblado$price))
sum(is.na(housing_poblado$new_banos_vf))
sum(is.na(housing_poblado$property_type))
############

############ ---- Estrato as.factor -----############
housing_chapinero$new_estrato_vf<-ceiling(housing_chapinero$new_estrato_vf)
housing_chapinero$new_estrato_vf<-as.factor(housing_chapinero$new_estrato_vf)

housing_poblado$new_estrato_vf<-ceiling(housing_poblado$new_estrato_vf)
housing_poblado$new_estrato_vf<-as.factor(housing_poblado$new_estrato_vf)

############ ---- Ba?os como valor  -----############
housing_chapinero$new_banos_vf<-ceiling(housing_chapinero$new_banos_vf)
housing_poblado$new_banos_vf<-ceiling(housing_poblado$new_banos_vf)

############ ---- property_type as.factor -----############
table(BASE$property_type)
table(housing_chapinero$property_type)
housing_chapinero$apto<-ifelse(housing_chapinero$property_type=="Apartamento",1,0 )
housing_chapinero$apto<-as.factor(housing_chapinero$apto)


table(housing_poblado$apto)
housing_poblado$apto<-ifelse(housing_poblado$property_type=="Apartamento",1,0 )
housing_poblado$apto<-as.factor(housing_poblado$apto)

################################################################################
###########################---Separar bases train y test - ######################
################################################################################
colnames(housing_chapinero)

train_chap_vf <- housing_chapinero %>%  filter(base == "Train") %>%  select(-base)
test_chap_vf <- housing_chapinero %>%  filter(base == "Test") %>%  select(-base)

train_pob_vf <- housing_poblado %>%  filter(base == "Train") %>%  select(-base)
test_pob_vf <- housing_poblado %>%  filter(base == "Test") %>%  select(-base)

###########################################################################################
########## --------- Modelos de Predicción de precios ----------- ##########################
############################################################################################

p_load(stargazer)
p_load(EnvStats)

#### Posibles transformaciones para normalizar la variable precio
ggplot(train_chap_vf, aes(x=price))+
  geom_histogram(fill="darkblue", alpha = 0.4)

ggplot(train_chap_vf, aes(x=log(price)))+
  geom_histogram(fill="darkblue", alpha = 0.4)

ggplot(train_chap_vf, aes(x=sqrt(price)))+
  geom_histogram(fill="darkblue", alpha = 0.4)

########## Box cox para precios ##########
lambda_price<-boxcox(train_chap_vf$price, objective.name = "Log-Likelihood", optimize = T)$lambda
#Transformamos la variable
train_chap_vf$price_boxcox<-boxcoxTransform(train_chap_vf$price, lambda_price)

ggplot(train_chap_vf, aes(x=price_boxcox))+
  geom_histogram(fill="darkblue", alpha = 0.4)


####--- OLS Chapinero ----########
modelo_lm_chap<-lm(sqrt(price) ~ new_piso_vf+new_estrato_vf+new_cuartos_vf
                           +surface_total2+dist_bar+dist_parque+dist_banco
                           +dist_estacionbus+dist_police+new_banos_vf
                           + apto, data=train_chap_vf)

stargazer(modelo_lm_chap, type = "text")
train_chap_vf$predict_lm<-(predict(modelo_lm_chap, newdata = train_chap_vf))^2
summary(train_chap_vf$predict_lm)

# MSE de entrenamiento
# ==============================================================================
mse_ols_chap <- mean((train_chap_vf$predict_lm - train_chap_vf$price)^2)
paste("Error (mse) de ols:", mse_ols_chap)
# MAE de entrenamiento
# ==============================================================================
mae_ols_chap <- mean(abs((train_chap_vf$predict_lm - train_chap_vf$price)))
paste("Error (mae) de ols:", mae_ols_chap)
mean(train_chap_vf$price)-mae_ols_chap #el error entre medias es de 33989

ggplot(train_chap_vf, aes(x=predict_lm))+
  geom_histogram(fill="darkblue", alpha = 0.4)
ggplot(train_chap_vf, aes(x=price))+
  geom_histogram(fill="darkblue", alpha = 0.4)


####--- OLS Poblado----########
modelo_lm_pob<-lm(sqrt(price) ~ new_piso_vf+new_estrato_vf+new_cuartos_vf
              +surface_total2+dist_bar+dist_parque+dist_banco
              +dist_estacionbus+dist_police+new_banos_vf
              + apto, data=train_pob_vf)

stargazer(modelo_lm_pob, type = "text")
train_pob_vf$predict_lm<-(predict(modelo_lm_pob, newdata = train_pob_vf))^2
summary(train_pob_vf$predict_lm)

# MSE de entrenamiento
# ==============================================================================
mse_ols_pob <- mean((train_pob_vf$predict_lm - train_pob_vf$price)^2)
paste("Error (mse) de ols:", mse_ols_pob)
# MAE de entrenamiento
# ==============================================================================
mae_ols_pob <- mean(abs((train_pob_vf$predict_lm - train_pob_vf$price)))
paste("Error (mae) de ols:", mae_ols_pob)
mean(train_pob_vf$price)-mae_ols_pob 

ggplot(train_pob_vf, aes(x=predict_lm))+
  geom_histogram(fill="darkblue", alpha = 0.4)
ggplot(train_pob_vf, aes(x=price))+
  geom_histogram(fill="darkblue", alpha = 0.4)

###########################################################################################
################# -----Ridge y Lasso--------- #############################################
###########################################################################################

# Modelado
# ==============================================================================
p_load(glmnet)
p_load(pls)

#### CHAPINERO ###

# Matrices de entrenamiento y test
# ==============================================================================
matriz_chap<-as.data.frame(cbind(train_chap_vf$price,train_chap_vf$new_piso_vf,
                                 train_chap_vf$new_estrato_vf,train_chap_vf$new_cuartos_vf,
                                 train_chap_vf$surface_total2, train_chap_vf$dist_bar,
                                 train_chap_vf$dist_parque, train_chap_vf$dist_banco,
                                 train_chap_vf$dist_estacionbus,train_chap_vf$dist_police,
                                 train_chap_vf$new_banos_vf,train_chap_vf$apto))
colnames(matriz_chap)<-c("price","new_piso_vf",
                         "new_estrato_vf","new_cuartos_vf",
                         "surface_total2","dist_bar",
                         "dist_parque","dist_banco",
                         "dist_estacionbus","dist_police",
                         "new_banos_vf","apto")

matriz_chap<- as.data.frame(scale(matriz_chap, center = TRUE, scale = TRUE))

x_train <- model.matrix(price ~ new_piso_vf+new_estrato_vf+new_cuartos_vf
                        +surface_total2+dist_bar+dist_parque+dist_banco
                        +dist_estacionbus+dist_police+new_banos_vf
                        + apto, data = matriz_chap)[, -1]

y_train <- train_chap_vf$price

#### POBLADO ###

# Matrices de entrenamiento y test
# ==============================================================================
matriz_pob<-as.data.frame(cbind(train_pob_vf$price,train_pob_vf$new_piso_vf,
                                train_pob_vf$new_estrato_vf,train_pob_vf$new_cuartos_vf,
                                train_pob_vf$surface_total2, train_pob_vf$dist_bar,
                                train_pob_vf$dist_parque, train_pob_vf$dist_banco,
                                train_pob_vf$dist_estacionbus,train_pob_vf$dist_police,
                                train_pob_vf$new_banos_vf,train_pob_vf$apto))

colnames(matriz_pob)<-c("price","new_piso_vf",
                         "new_estrato_vf","new_cuartos_vf",
                         "surface_total2","dist_bar",
                         "dist_parque","dist_banco",
                         "dist_estacionbus","dist_police",
                         "new_banos_vf","apto")

matriz_pob<- as.data.frame(scale(matriz_pob, center = TRUE, scale = TRUE))

x_train_p <- model.matrix(price ~ new_piso_vf+new_estrato_vf+new_cuartos_vf
                        +surface_total2+dist_bar+dist_parque+dist_banco
                        +dist_estacionbus+dist_police+new_banos_vf
                        + apto, data = matriz_pob)[, -1]

y_train_p <- train_pob_vf$price


#### ---- Ridge -----######

#CHAPINERO

# Evolución del error en función de lambda
# ==============================================================================
set.seed(123)
cv_error_ridge <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

cv_error_ridge
plot(cv_error_ridge)

# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error_ridge$lambda.1se)

# Mejor modelo lambda óptimo + 1sd
modelo_ridge <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error_ridge$lambda.1se,
  standardize = TRUE
)

modelo_ridge

# Coeficientes del modelo
# ==============================================================================
df_coeficientes_ridge <- coef(modelo_ridge) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train_ridge <- predict(modelo_ridge, newx = x_train)

# MSE de entrenamiento
# ==============================================================================
mse_ridge_chap <- mean((predicciones_train_ridge - y_train)^2)
paste("Error (mse) de ols:", mse_ridge_chap)

# MAE de entrenamiento
# ==============================================================================
mae_ridge_chap <- mean(abs(predicciones_train_ridge - y_train))
paste("Error (mae) de ridge", mae_ridge_chap)

#POBLADO

# Evolución del error en función de lambda
# ==============================================================================
set.seed(123)
cv_error_ridge_pob <- cv.glmnet(
  x      = x_train_p,
  y      = y_train_p,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

cv_error_ridge_pob
plot(cv_error_ridge_pob)

# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error_ridge_pob$lambda.1se)

# Mejor modelo lambda óptimo + 1sd
modelo_ridge_pob <- glmnet(
  x           = x_train_p,
  y           = y_train_p,
  alpha       = 0,
  lambda      = cv_error_ridge_pob$lambda.1se,
  standardize = TRUE
)

modelo_ridge_pob

# Coeficientes del modelo
# ==============================================================================
df_coeficientes_ridge_pob <- coef(modelo_ridge_pob) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train_ridge_pob <- predict(modelo_ridge_pob, newx = x_train_p)

# MSE de entrenamiento
# ==============================================================================
mse_ridge_pob <- mean((predicciones_train_ridge_pob - y_train_p)^2)
paste("Error (mse) de ols:", mse_ridge_pob)

# MAE de entrenamiento
# ==============================================================================
mae_ridge_pob <- mean(abs(predicciones_train_ridge_pob - y_train_p))
paste("Error (mae) de ridge", mae_ridge_pob)


#### ---- Lasso -----######

#CHAPINERO

# Creacion y entrenamiento del modelo
# ==============================================================================
# Para obtener un ajuste con regularización Lasso se indica argumento alpha=1.
# Si no se especifica valor de lambda, se selecciona un rango automático.
modelo_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

# Evolución de los coeficientes en función de lambda
# ==============================================================================
regularizacion_lasso <- modelo_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_lasso$lambda)

regularizacion_lasso <- regularizacion_lasso %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion_lasso %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

# Evolución del error en función de lambda
# ==============================================================================
set.seed(123)
cv_error_lasso <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error_lasso)
# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error_lasso$lambda.1se)

# Mejor modelo lambda óptimo + 1sd
# ==============================================================================
modelo_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error_lasso$lambda.1se,
  standardize = TRUE
)

# Coeficientes del modelo
# ==============================================================================
df_coeficientes_lasso <- coef(modelo_lasso) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes_lasso %>%
  filter(
    predictor != "(Intercept)",
    coeficiente != 0
  ) 

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train_lasso <- predict(modelo_lasso, newx = x_train)

# MSE de entrenamiento
# ==============================================================================
mse_lasso_chap <- mean((predicciones_train_lasso - y_train)^2)
paste("Error (mse) de ols:", mse_lasso_chap)

# MAE de entrenamiento
# ==============================================================================
mae_lasso_chap <- mean(abs(predicciones_train_lasso - y_train))
print(paste("Error (mae) de lasso", mae_lasso_chap))

#POBLADO

# Creacion y entrenamiento del modelo
# ==============================================================================
# Para obtener un ajuste con regularización Lasso se indica argumento alpha=1.
# Si no se especifica valor de lambda, se selecciona un rango automático.
modelo_lasso_pob <- glmnet(
  x           = x_train_p,
  y           = y_train_p,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

# Evolución de los coeficientes en función de lambda
# ==============================================================================
regularizacion_lasso_pob <- modelo_lasso_pob$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_lasso_pob$lambda)

regularizacion_lasso_pob <- regularizacion_lasso_pob %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion_lasso_pob %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en funcion de la regularizacion") +
  theme_bw() +
  theme(legend.position = "none")

# Evolucion del error en funcion de lambda
# ==============================================================================
set.seed(123)
cv_error_lasso_pob <- cv.glmnet(
  x      = x_train_p,
  y      = y_train_p,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error_lasso_pob)
# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error_lasso_pob$lambda.1se)

# Mejor modelo lambda óptimo + 1sd
# ==============================================================================
modelo_lasso_pob <- glmnet(
  x           = x_train_p,
  y           = y_train_p,
  alpha       = 1,
  lambda      = cv_error_lasso_pob$lambda.1se,
  standardize = TRUE
)

# Coeficientes del modelo
# ==============================================================================
df_coeficientes_lasso_pob <- coef(modelo_lasso_pob) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes_lasso_pob %>%
  filter(
    predictor != "(Intercept)",
    coeficiente != 0
  ) 

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train_lasso_pob <- predict(modelo_lasso_pob, newx = x_train_p)

# MSE de entrenamiento
# ==============================================================================
mse_lasso_pob <- mean((predicciones_train_lasso_pob - y_train_p)^2)
paste("Error (mse) de ols:", mse_lasso_pob)

# MAE de entrenamiento
# ==============================================================================
mae_lasso_pob <- mean(abs(predicciones_train_lasso_pob - y_train_p))
print(paste("Error (mae) de lasso", mae_lasso_pob))

###########################################################################################
###########################################################################################
###########################################################################################

# ##Arboles de decision
p_load(rpart)
p_load(rsample)     # data splitting 
p_load(dplyr)       # data wrangling
p_load(rpart)       # performing regression trees
p_load(rpart.plot)  # plotting regression trees
p_load(ipred)       # bagging
p_load(caret)       # bagging


# Veamos la cantidad de valores unicos para cada variable categorica y su distribucion
#Chapinero
glimpse(train_chap_vf)


final_chap <- train_chap_vf %>% 
                  select(property_id,price,new_piso_vf,
                         new_estrato_vf,new_cuartos_vf,
                         surface_total2,dist_bar,
                         dist_parque,dist_banco,
                         dist_estacionbus,dist_police,
                         new_banos_vf,apto)


glimpse(final_chap)

#Poblado
glimpse(train_pob_vf)


final_pob <- train_pob_vf %>% 
  select(property_id,price,new_piso_vf,
         new_estrato_vf,new_cuartos_vf,
         surface_total2,dist_bar,
         dist_parque,dist_banco,
         dist_estacionbus,dist_police,
         new_banos_vf,apto)


glimpse(final_pob)




# Creamos el primer modelo

#Chapinero
arbol1_chap <- rpart(
  formula = price ~ new_piso_vf + new_estrato_vf + new_cuartos_vf +
                    surface_total2 + dist_bar + dist_parque +
                    dist_banco + dist_estacionbus +dist_police +
                    new_banos_vf + apto, # Ecuaci?n var dependiente vs. independientes
  data    = final_chap, # Dataset
  method  = "anova" # Anova para especificar que es un arbol de regresi?n
)

rpart.plot(arbol1_chap)

plotcp(arbol1_chap)

######## ERRORES ###########
yhat_chap <- predict(arbol1_chap, newdata = final_chap)
actual_chap <- final_chap$price
plot(yhat_chap, actual_chap)
abline(0,1)

mse_arbol_chap <- mean((yhat_chap-actual_chap)^2)
mae_arbol_chap <-mean(abs(yhat_chap-actual_chap))
######################
#Poblado
arbol1_pob <- rpart(
  formula = price ~ new_piso_vf + new_estrato_vf + new_cuartos_vf +
    surface_total2 + dist_bar + dist_parque +
    dist_banco + dist_estacionbus +dist_police +
    new_banos_vf + apto, # Ecuaci?n var dependiente vs. independientes
  data    = final_pob, # Dataset
  method  = "anova" # Anova para especificar que es un arbol de regresi?n
)

rpart.plot(arbol1_pob)

plotcp(arbol1_pob)


######## ERRORES ###########
yhat_pob <- predict(arbol1_pob, newdata = final_pob)
actual_pob <- final_pob$price
plot(yhat_pob, actual_pob)
abline(0,1)

mse_arbol_pob <- mean((yhat_pob-actual_pob)^2)
mae_arbol_pob <-mean(abs(yhat_pob-actual_pob))

############ BAGGING ##############

# CHAPINERO
# Semilla
set.seed(123)

# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_cv_chap <- train(
  price ~ new_piso_vf + new_estrato_vf + new_cuartos_vf +
    surface_total2 + dist_bar + dist_parque +
    dist_banco + dist_estacionbus +dist_police +
    new_banos_vf + apto,
  data = final_chap,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_cv_chap
#####----MAE---######
bag_chap<-bagged_cv_chap$results
mae_bag_chap<-bag_chap$MAE
#####----MSE---######
mse_bag_chap<-(bag_chap$RMSE)^2

# plot most important variables
plot(varImp(bagged_cv_chap), 20)  

#Predicciones modelo
predict_bag_chap <- predict(bagged_cv_chap, final_chap)

# POBLADO
# Semilla
set.seed(123)

# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_cv_pob <- train(
  price ~ new_piso_vf + new_estrato_vf + new_cuartos_vf +
    surface_total2 + dist_bar + dist_parque +
    dist_banco + dist_estacionbus +dist_police +
    new_banos_vf + apto,
  data = final_pob,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_cv_pob

#####----MAE---######
bag_pob<-bagged_cv_pob$results
mae_bag_pob<-bag_pob$MAE
#####----MSE---######
mse_bag_pob<-(bag_pob$RMSE)^2


# plot most important variables
plot(varImp(bagged_cv_pob), 20)  

#Predicciones modelo
predict_bag_pob <- predict(bagged_cv_pob, final_pob)

############ XGBOOST ##############

#Paquetes

p_load(xgboost)
p_load(caret)

# CHAPINERO
# Semilla
set.seed(123)

require("xgboost")

grid_price<- expand.grid(nrounds = c(250,500),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))
set.seed(1410)

xgboost_chap <- train(
  price ~ new_piso_vf + new_estrato_vf + new_cuartos_vf +
    surface_total2 + dist_bar + dist_parque +
    dist_banco + dist_estacionbus +dist_police +
    new_banos_vf + apto,
  data = final_chap,
  method = "xgbTree",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = grid_price,
  preProcess = c("center", "scale")
)

xgboost_chap$bestTune
xgboost_chap_Results <- xgboost_chap$results

#---Predicciones
pred_xgb_chap<-predict(xgboost_chap, final_chap)

mae_xgb_chap<-291622240
mse_xgb_chap<-(486091477)^2

# POBLADO
# Semilla
set.seed(123)

require("xgboost")

grid_price<- expand.grid(nrounds = c(250,500),
                         max_depth = c(4,6,8),
                         eta = c(0.01,0.3,0.5),
                         gamma = c(0,1),
                         min_child_weight = c(10, 25,50),
                         colsample_bytree = c(0.7),
                         subsample = c(0.6))
set.seed(1410)

xgboost_pob <- train(
  price ~ new_piso_vf + new_estrato_vf + new_cuartos_vf +
    surface_total2 + dist_bar + dist_parque +
    dist_banco + dist_estacionbus +dist_police +
    new_banos_vf + apto,
  data = final_pob,
  method = "xgbTree",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = grid_price,
  preProcess = c("center", "scale")
)

xgboost_pob$bestTune
xgboost_Pob_Results <- xgboost_pob$results
var_imp<- varImp(xgboost_pob, scale = FALSE)
plot(var_imp)

var_imp_xgb_pob <- var_imp$importance
var_imp_xgb_pob<- as.data.frame(var_imp_xgb_pob)
class(var_imp_xgb_pob)


graph_xgb_pob <- as.data.frame(graph_xgb_pob)
graph_xgb_pob$varnames <- row.names(graph_xgb_pob$V1)

rownames(var_imp_xgb_pob) = c("Superficie Total", "Distancia Bares", "Distancia Estacion Bus", 
                         "No. Banos", "Tipo Propiedad", "Distancia Parque", "Distancia Polic�a",
                         "No. Cuartos", "Distancia Bancos", "Estrato 6", "Piso", "Estrato 3",
                         "Estrato 5", "Estrato 4")
var_imp_xgb_pob$varnames<- rownames(var_imp_xgb_pob)


ggplot(var_imp_xgb_pob, aes(x=reorder(varnames, Overall), y=Overall)) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=Overall), color = "#56B4E9") +
    ylab("Importance") +
  xlab("Variable Name") +
  coord_flip()

#---Predicciones
pred_xgb_pob<-predict(xgboost_pob, final_pob)

mae_xgb_pob<-177684837
mse_xgb_pob<-(358691318)^2

#################################################################################
########################------Criterios mejor modelo--------####################
#################################################################################
#### -------- MAE y MSE Chapinero -------####
results_chap<-as.data.frame(rbind(mae_ols_chap,mae_ridge_chap,mae_lasso_chap,
                                  mae_arbol_chap,mae_bag_chap, mae_xgb_chap))
colnames(results_chap)<-c("MAE")
rownames(results_chap)<-c("modelo_ols_chap","modelo_ridge_chap","modelo_lasso_chap",
                          "modelo_arbol_chap","modelo_bag_chap", "modelo_xgb_chap")

results_chap=results_chap %>% mutate(MSE =  c(mse_ols_chap,mse_ridge_chap,mse_lasso_chap,
                                              mse_arbol_chap,mse_bag_chap, mse_xgb_chap))

results_chap$MAE <-(results_chap$MAE)/1000000
results_chap$RMSE <- sqrt(results_chap$MSE)/1000000

results_chap<- results_chap[,-2]


#### -------- MAE y MSE Poblado -------#### 
results_pob<-as.data.frame(rbind(mae_ols_pob,mae_ridge_pob,mae_lasso_pob,
                                 mae_arbol_pob,mae_bag_pob, mae_xgb_pob))
colnames(results_pob)<-c("MAE")
rownames(results_pob)<-c("modelo_ols_pob","modelo_ridge_pob","modelo_lasso_pob",
                         "modelo_arbol_pob","modelo_bag_pob", "modelo_xgb_pob")

results_pob=results_pob %>% mutate(MSE =  c(mse_ols_pob,mse_ridge_pob,mse_lasso_pob,
                                            mse_arbol_pob,mse_bag_pob, mse_xgb_pob))
results_pob$MAE <-(results_pob$MAE)/1000000
results_pob$RMSE <- sqrt(results_pob$MSE)/1000000
results_pob<- results_pob[,-2]

######### ----- Número y valor de propiedades -----#############################

###---OLS Chapinero---####
ols_pred_chap<-as.data.frame(cbind(train_chap_vf$property_id, train_chap_vf$price ,train_chap_vf$predict_lm))
colnames(ols_pred_chap)<-c("property_id","price","predict_lm")
ols_pred_chap$price<-as.numeric(ols_pred_chap$price)
ols_pred_chap$predict_lm<-as.numeric(ols_pred_chap$predict_lm)
ols_pred_chap$dif<-(ols_pred_chap$price - ols_pred_chap$predict_lm)

ols_pred_chap$mayor<-ifelse(ols_pred_chap$predict_lm>=ols_pred_chap$price,1,0)
ols_pred_chap$menor<-ifelse(ols_pred_chap$predict_lm<ols_pred_chap$price & ols_pred_chap$dif<=40000000
                            ,1,0)
ols_pred_chap$compra<-ifelse(ols_pred_chap$mayor==1|ols_pred_chap$menor==1,1,0)

ols_pred_chap <- ols_pred_chap %>%  filter(compra == 1) 
gasto_ols_chap<-sum(ols_pred_chap$predict_lm)
compras_ols_chap<-sum(ols_pred_chap$compra)

ratio_ols_chap<-gasto_ols_chap/compras_ols_chap

###---OLS Poblado---####
ols_pred_pob<-as.data.frame(cbind(train_pob_vf$property_id, train_pob_vf$price ,train_pob_vf$predict_lm))
colnames(ols_pred_pob)<-c("property_id","price","predict_lm")
ols_pred_pob$price<-as.numeric(ols_pred_pob$price)
ols_pred_pob$predict_lm<-as.numeric(ols_pred_pob$predict_lm)
ols_pred_pob$dif<-(ols_pred_pob$price - ols_pred_pob$predict_lm)

ols_pred_pob$mayor<-ifelse(ols_pred_pob$predict_lm>=ols_pred_pob$price,1,0)
ols_pred_pob$menor<-ifelse(ols_pred_pob$predict_lm<ols_pred_pob$price & ols_pred_pob$dif<=40000000
                           ,1,0)
ols_pred_pob$compra<-ifelse(ols_pred_pob$mayor==1|ols_pred_pob$menor==1,1,0)

ols_pred_pob <- ols_pred_pob %>%  filter(compra == 1) 
gasto_ols_pob<-sum(ols_pred_pob$predict_lm)
compras_ols_pob<-sum(ols_pred_pob$compra)

ratio_ols_pob<-gasto_ols_pob/compras_ols_pob


###---Ridge Chapinero---####
ridge_pred_chap<-as.data.frame(cbind(train_chap_vf$property_id, train_chap_vf$price ,predicciones_train_ridge))
colnames(ridge_pred_chap)<-c("property_id","price","predict_lm")
ridge_pred_chap$price<-as.numeric(ridge_pred_chap$price)
ridge_pred_chap$predict_lm<-as.numeric(ridge_pred_chap$predict_lm)
ridge_pred_chap$dif<-(ridge_pred_chap$price - ridge_pred_chap$predict_lm)

ridge_pred_chap$mayor<-ifelse(ridge_pred_chap$predict_lm>=ridge_pred_chap$price,1,0)
ridge_pred_chap$menor<-ifelse(ridge_pred_chap$predict_lm<ridge_pred_chap$price & ridge_pred_chap$dif<=40000000
                              ,1,0)
ridge_pred_chap$compra<-ifelse(ridge_pred_chap$mayor==1|ridge_pred_chap$menor==1,1,0)

ridge_pred_chap <- ridge_pred_chap %>%  filter(compra == 1) 
gasto_ridge_chap<-sum(ridge_pred_chap$predict_lm)
compras_ridge_chap<-sum(ridge_pred_chap$compra)

ratio_ridge_chap<-gasto_ridge_chap/compras_ridge_chap

mean(train_chap_vf$price)


###---Ridge Poblado---####
ridge_pred_pob<-as.data.frame(cbind(train_pob_vf$property_id, train_pob_vf$price ,predicciones_train_ridge_pob))
colnames(ridge_pred_pob)<-c("property_id","price","predict_lm")
ridge_pred_pob$price<-as.numeric(ridge_pred_pob$price)
ridge_pred_pob$predict_lm<-as.numeric(ridge_pred_pob$predict_lm)
ridge_pred_pob$dif<-(ridge_pred_pob$price - ridge_pred_pob$predict_lm)

ridge_pred_pob$mayor<-ifelse(ridge_pred_pob$predict_lm>=ridge_pred_pob$price,1,0)
ridge_pred_pob$menor<-ifelse(ridge_pred_pob$predict_lm<ridge_pred_pob$price & ridge_pred_pob$dif<=40000000
                             ,1,0)
ridge_pred_pob$compra<-ifelse(ridge_pred_pob$mayor==1|ridge_pred_pob$menor==1,1,0)

ridge_pred_pob <- ridge_pred_pob %>%  filter(compra == 1) 
gasto_ridge_pob<-sum(ridge_pred_pob$predict_lm)
compras_ridge_pob<-sum(ridge_pred_pob$compra)

ratio_ridge_pob<-gasto_ridge_pob/compras_ridge_pob

mean(train_pob_vf$price)

###---lasso Chapinero---####
lasso_pred_chap<-as.data.frame(cbind(train_chap_vf$property_id, train_chap_vf$price ,predicciones_train_lasso))
colnames(lasso_pred_chap)<-c("property_id","price","predict_lm")
lasso_pred_chap$price<-as.numeric(lasso_pred_chap$price)
lasso_pred_chap$predict_lm<-as.numeric(lasso_pred_chap$predict_lm)
lasso_pred_chap$dif<-(lasso_pred_chap$price - lasso_pred_chap$predict_lm)

lasso_pred_chap$mayor<-ifelse(lasso_pred_chap$predict_lm>=lasso_pred_chap$price,1,0)
lasso_pred_chap$menor<-ifelse(lasso_pred_chap$predict_lm<lasso_pred_chap$price & lasso_pred_chap$dif<=40000000
                              ,1,0)
lasso_pred_chap$compra<-ifelse(lasso_pred_chap$mayor==1|lasso_pred_chap$menor==1,1,0)

lasso_pred_chap <- lasso_pred_chap %>%  filter(compra == 1) 
gasto_lasso_chap<-sum(lasso_pred_chap$predict_lm)
compras_lasso_chap<-sum(lasso_pred_chap$compra)

ratio_lasso_chap<-gasto_lasso_chap/compras_lasso_chap

mean(train_chap_vf$price)


###---lasso Poblado---####
lasso_pred_pob<-as.data.frame(cbind(train_pob_vf$property_id, train_pob_vf$price ,predicciones_train_lasso_pob))
colnames(lasso_pred_pob)<-c("property_id","price","predict_lm")
lasso_pred_pob$price<-as.numeric(lasso_pred_pob$price)
lasso_pred_pob$predict_lm<-as.numeric(lasso_pred_pob$predict_lm)
lasso_pred_pob$dif<-(lasso_pred_pob$price - lasso_pred_pob$predict_lm)

lasso_pred_pob$mayor<-ifelse(lasso_pred_pob$predict_lm>=lasso_pred_pob$price,1,0)
lasso_pred_pob$menor<-ifelse(lasso_pred_pob$predict_lm<lasso_pred_pob$price & lasso_pred_pob$dif<=40000000
                             ,1,0)
lasso_pred_pob$compra<-ifelse(lasso_pred_pob$mayor==1|lasso_pred_pob$menor==1,1,0)

lasso_pred_pob <- lasso_pred_pob %>%  filter(compra == 1) 
gasto_lasso_pob<-sum(lasso_pred_pob$predict_lm)
compras_lasso_pob<-sum(lasso_pred_pob$compra)

ratio_lasso_pob<-gasto_lasso_pob/compras_lasso_pob

mean(train_pob_vf$price)

###---Arboles Chapinero---####
arb_pred_chap<-as.data.frame(cbind(train_chap_vf$property_id, train_chap_vf$price ,yhat_chap))
colnames(arb_pred_chap)<-c("property_id","price","predict_lm")
arb_pred_chap$price<-as.numeric(arb_pred_chap$price)
arb_pred_chap$predict_lm<-as.numeric(arb_pred_chap$predict_lm)
arb_pred_chap$dif<-(arb_pred_chap$price - arb_pred_chap$predict_lm)

arb_pred_chap$mayor<-ifelse(arb_pred_chap$predict_lm>=arb_pred_chap$price,1,0)
arb_pred_chap$menor<-ifelse(arb_pred_chap$predict_lm<arb_pred_chap$price & arb_pred_chap$dif<=40000000
                            ,1,0)
arb_pred_chap$compra<-ifelse(arb_pred_chap$mayor==1|arb_pred_chap$menor==1,1,0)

arb_pred_chap <- arb_pred_chap %>%  filter(compra == 1) 
gasto_arb_chap<-sum(arb_pred_chap$predict_lm)
compras_arb_chap<-sum(arb_pred_chap$compra)

ratio_arb_chap<-gasto_arb_chap/compras_arb_chap

mean(train_chap_vf$price)

###---Arboles Poblado---####
arb_pred_pob<-as.data.frame(cbind(train_pob_vf$property_id, train_pob_vf$price ,yhat_pob))
colnames(arb_pred_pob)<-c("property_id","price","predict_lm")
arb_pred_pob$price<-as.numeric(arb_pred_pob$price)
arb_pred_pob$predict_lm<-as.numeric(arb_pred_pob$predict_lm)
arb_pred_pob$dif<-(arb_pred_pob$price - arb_pred_pob$predict_lm)

arb_pred_pob$mayor<-ifelse(arb_pred_pob$predict_lm>=arb_pred_pob$price,1,0)
arb_pred_pob$menor<-ifelse(arb_pred_pob$predict_lm<arb_pred_pob$price & arb_pred_pob$dif<=40000000
                           ,1,0)
arb_pred_pob$compra<-ifelse(arb_pred_pob$mayor==1|arb_pred_pob$menor==1,1,0)

arb_pred_pob <- arb_pred_pob %>%  filter(compra == 1) 
gasto_arb_pob<-sum(arb_pred_pob$predict_lm)
compras_arb_pob<-sum(arb_pred_pob$compra)

ratio_arb_pob<-gasto_arb_pob/compras_arb_pob

mean(train_pob_vf$price)

###---Bagging Chapinero---####
bag_pred_chap<-as.data.frame(cbind(train_chap_vf$property_id, train_chap_vf$price ,predict_bag_chap))
colnames(bag_pred_chap)<-c("property_id","price","predict_lm")
bag_pred_chap$price<-as.numeric(bag_pred_chap$price)
bag_pred_chap$predict_lm<-as.numeric(bag_pred_chap$predict_lm)
bag_pred_chap$dif<-(bag_pred_chap$price - bag_pred_chap$predict_lm)

bag_pred_chap$mayor<-ifelse(bag_pred_chap$predict_lm>=bag_pred_chap$price,1,0)
bag_pred_chap$menor<-ifelse(bag_pred_chap$predict_lm<bag_pred_chap$price & bag_pred_chap$dif<=40000000
                            ,1,0)
bag_pred_chap$compra<-ifelse(bag_pred_chap$mayor==1|bag_pred_chap$menor==1,1,0)

bag_pred_chap <- bag_pred_chap %>%  filter(compra == 1) 
gasto_bag_chap<-sum(bag_pred_chap$predict_lm)
compras_bag_chap<-sum(bag_pred_chap$compra)

ratio_bag_chap<-gasto_bag_chap/compras_bag_chap

mean(train_chap_vf$price)

###---Bagging Poblado---####
bag_pred_pob<-as.data.frame(cbind(train_pob_vf$property_id, train_pob_vf$price ,predict_bag_pob))
colnames(bag_pred_pob)<-c("property_id","price","predict_lm")
bag_pred_pob$price<-as.numeric(bag_pred_pob$price)
bag_pred_pob$predict_lm<-as.numeric(bag_pred_pob$predict_lm)
bag_pred_pob$dif<-(bag_pred_pob$price - bag_pred_pob$predict_lm)

bag_pred_pob$mayor<-ifelse(bag_pred_pob$predict_lm>=bag_pred_pob$price,1,0)
bag_pred_pob$menor<-ifelse(bag_pred_pob$predict_lm<bag_pred_pob$price & bag_pred_pob$dif<=40000000
                           ,1,0)
bag_pred_pob$compra<-ifelse(bag_pred_pob$mayor==1|bag_pred_pob$menor==1,1,0)

bag_pred_pob<- bag_pred_pob %>%  filter(compra == 1) 
gasto_bag_pob<-sum(bag_pred_pob$predict_lm)
compras_bag_pob<-sum(bag_pred_pob$compra)

ratio_bag_pob<-gasto_bag_pob/compras_bag_pob

mean(train_pob_vf$price)

###---XGBOOST Chapinero---####
xgb_pred_chap<-as.data.frame(cbind(train_chap_vf$property_id, train_chap_vf$price ,pred_xgb_chap))
colnames(xgb_pred_chap)<-c("property_id","price","predict_lm")
xgb_pred_chap$price<-as.numeric(xgb_pred_chap$price)
xgb_pred_chap$predict_lm<-as.numeric(xgb_pred_chap$predict_lm)
xgb_pred_chap$dif<-(xgb_pred_chap$price - xgb_pred_chap$predict_lm)

xgb_pred_chap$mayor<-ifelse(xgb_pred_chap$predict_lm>=xgb_pred_chap$price,1,0)
xgb_pred_chap$menor<-ifelse(xgb_pred_chap$predict_lm<xgb_pred_chap$price & xgb_pred_chap$dif<=40000000
                            ,1,0)
xgb_pred_chap$compra<-ifelse(xgb_pred_chap$mayor==1|xgb_pred_chap$menor==1,1,0)

xgb_pred_chap <- xgb_pred_chap %>%  filter(compra == 1) 
gasto_xgb_chap<-sum(xgb_pred_chap$predict_lm)
compras_xgb_chap<-sum(xgb_pred_chap$compra)

ratio_xgb_chap<-gasto_xgb_chap/compras_xgb_chap

mean(train_chap_vf$price)

###---XGBOOST Poblado---####
xgb_pred_pob<-as.data.frame(cbind(train_pob_vf$property_id, train_pob_vf$price ,pred_xgb_pob))
colnames(xgb_pred_pob)<-c("property_id","price","predict_lm")
xgb_pred_pob$price<-as.numeric(xgb_pred_pob$price)
xgb_pred_pob$predict_lm<-as.numeric(xgb_pred_pob$predict_lm)
xgb_pred_pob$dif<-(xgb_pred_pob$price - xgb_pred_pob$predict_lm)

xgb_pred_pob$mayor<-ifelse(xgb_pred_pob$predict_lm>=xgb_pred_pob$price,1,0)
xgb_pred_pob$menor<-ifelse(xgb_pred_pob$predict_lm<xgb_pred_pob$price & xgb_pred_pob$dif<=40000000
                           ,1,0)
xgb_pred_pob$compra<-ifelse(xgb_pred_pob$mayor==1|xgb_pred_pob$menor==1,1,0)

xgb_pred_pob <- xgb_pred_pob %>%  filter(compra == 1) 
gasto_xgb_pob<-sum(xgb_pred_pob$predict_lm)
compras_xgb_pob<-sum(xgb_pred_pob$compra)

ratio_xgb_pob<-gasto_xgb_pob/compras_xgb_pob

mean(train_pob_vf$price)

#### -------- Tabla de resultados -------####
#Chapinero
results_chap=results_chap %>% mutate(Gasto =  c(gasto_ols_chap,gasto_ridge_chap,gasto_lasso_chap,
                                                gasto_arb_chap,gasto_bag_chap, gasto_xgb_chap))

results_chap=results_chap %>% mutate(Compras =  c(compras_ols_chap,compras_ridge_chap,compras_lasso_chap,
                                                  compras_arb_chap,compras_bag_chap, compras_xgb_chap))

results_chap=results_chap %>% mutate(Ratio =  c(ratio_ols_chap,ratio_ridge_chap,ratio_lasso_chap,
                                                ratio_arb_chap,ratio_bag_chap, ratio_xgb_chap))

results_chap$Gasto<- (results_chap$Gasto)/1000000
results_chap$Ratio<- (results_chap$Ratio)/1000000

results_chap


library(xtable)
xtable(results_chap, caption = "Resultados Modelos Chapinero", digits = 1)

#Poblado
results_pob=results_pob %>% mutate(Gasto =  c(gasto_ols_pob,gasto_ridge_pob,gasto_lasso_pob,
                                                gasto_arb_pob,gasto_bag_pob, gasto_xgb_pob))

results_pob=results_pob %>% mutate(Compras =  c(compras_ols_pob,compras_ridge_pob,compras_lasso_pob,
                                                  compras_arb_pob,compras_bag_pob, compras_xgb_pob))

results_pob=results_pob %>% mutate(Ratio =  c(ratio_ols_pob,ratio_ridge_pob,ratio_lasso_pob,
                                                ratio_arb_pob,ratio_bag_pob, ratio_xgb_pob))

results_pob$Gasto<- (results_pob$Gasto)/1000000
results_pob$Ratio<- (results_pob$Ratio)/1000000

results_pob

library(xtable)
xtable(results_pob, digits = 1)

library(xtable)
xtable(results_pob, caption = "Resultados Modelos El Poblado", digits = 1)

##############################################

#Guardar ambiente
#save.image(file = "C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet3_Ramos_Uribe_Urquijo/scripts/workspace")


#Cargar ambiente 

#load("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet3_Ramos_Uribe_Urquijo/scripts/workspace")

############# PREDICCION FINAL PARA CADA UNO ####################
#Chapinero
glimpse(test_chap_vf)


final_chap_test <- test_chap_vf %>% 
  select(property_id,price,new_piso_vf,
         new_estrato_vf,new_cuartos_vf,
         surface_total2,dist_bar,
         dist_parque,dist_banco,
         dist_estacionbus,dist_police,
         new_banos_vf,apto)


glimpse(final_chap_test)

#Poblado
glimpse(test_pob_vf)


final_pob_test <- test_pob_vf %>% 
  select(property_id,price,new_piso_vf,
         new_estrato_vf,new_cuartos_vf,
         surface_total2,dist_bar,
         dist_parque,dist_banco,
         dist_estacionbus,dist_police,
         new_banos_vf,apto)


glimpse(final_pob_test)


#Chapinero

#---Predicciones
final_chap_test$pred_ols_chap_final<-(predict(modelo_lm_chap, newdata = final_chap_test))^2


#Poblado

#---Predicciones
final_pob_test$pred_xgb_pob_final<-predict(xgboost_pob, final_pob_test)

##########Archivo de predicciones

setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet3_Ramos_Uribe_Urquijo/document")

#Chapinero
id_test_property_chap<-final_chap_test$property_id
price_pred_chap<-final_chap_test$pred_ols_chap_final
predicciones_chapinero <- data.frame(id_test_property_chap, price_pred_chap)

write.csv(predicciones_chapinero, file = "predictions_chapinero_ramos_uribe_urquijo.csv")

#Poblado
id_test_property_pob<-final_pob_test$property_id
price_pred_pob<-final_pob_test$pred_xgb_pob_final
predicciones_poblado<-data.frame(id_test_property_pob, price_pred_pob)

write.csv(predicciones_poblado, file = "predictions_poblado_ramos_uribe_urquijo.csv")








