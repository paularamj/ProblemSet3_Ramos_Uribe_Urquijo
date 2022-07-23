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
       leaflet, #visualizci贸n
       tmaptools, #geocode
       osmdata # Get OSM data
)

##############################Cargar los datos#################################
#setwd("C:/Users/pau_9/Documents/GitHub/ProblemSet3_Ramos_Uribe_Urquijo")
#setwd("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 3/ProblemSet3_Ramos_Uribe_Urquijo")
setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet3_Ramos_Uribe_Urquijo")

train<-readRDS("dataPS3/train.Rds")
test<-readRDS("dataPS3/test.Rds")

##########Explorci贸n de los datos########
skim(train)
##Variables con mayor porcentaje de missing values (surface_covered, surface_total)
table(is.na(train$surface_covered))
table(is.na(train$surface_total))
##Datos de 谩rea en formato texto en la descripci贸n

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

##Caja de coordenada que contiene el poligono de Chapinero - Bogot谩
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)
train_chapinero <- st_crop(db, chapinero)

##Caja de coordenada que contiene el pol铆gono de Poblado - Medell铆n
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
housing_chapinero <- st_join(x=train_chapinero , y=Bogota_mzn) #Validaci贸n se mantienen las 15165 obs
housing_poblado <- st_join(x=train_poblado , y=Medellin_mzn) #Validaci贸n se mantienen las 1677 obs

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
min_dist_p <- apply(dist_bar_pob , 1 , min) #distancia m铆nima a cada bar
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


#Imputar Variables rescatadas de la descripci髇 a surface_total

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

#================== Crear variable Piso con la informaci髇 extraida de la descripcion ====================#

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

## ======Con criterio experto, revisamos los datos que tengan sentido respecto al area total ===== ##

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

#Eliminamos los outliers, quitando los pisos mayores a 13

housing_poblado<- housing_poblado %>% 
  filter(new_piso_vf <=26)

ggplot(housing_poblado, aes(x=new_piso_vf)) +
  geom_boxplot(fill= "darkblue", alpha=0.4)

summary(housing_poblado$new_piso_vf)

#================== Crear variable Estrato con la informaci髇 extraida de la descripcion ====================#

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


##=== Cargar info de CENSO data para imputar estrato ===##

## load data
mnz_censo = import("http://eduard-martinez.github.io/data/fill-gis-vars/mnz_censo.rds")

## about data
browseURL("https://eduard-martinez.github.io/teaching/meca-4107/7-censo.txt")

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
H_NRO_CUARTOS = "N鷐ero de cuartos en total"
HA_TOT_PER = "Total personas en el hogar"
V_TOT_HOG = "Total de hogares en la vivienda"
VA1_ESTRATO = "Estrato de la vivienda (seg鷑 servicio de energ韆)"
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


## Join
colnames(df_bog)
colnames(housing_chapinero)

housing_chapinero = left_join(housing_chapinero,df_bog,by=c("COD_DANE_ANM"))
colnames(house_censo)




## Calcular Buffers para estrato 

house_buf = st_buffer(housing_chapinero,dist=1000)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% addCircles(data=housing_chapinero)

house_buf = st_join(house_buf,house[,"surface_total"])

st_geometry(house_buf) = NULL

house_buf_mean = house_buf %>% group_by(property_id) %>% summarise(surface_new_3=mean(surface_total.y,na.rm=T))

house_mnz = left_join(house_mnz,house_buf_mean,"property_id")


















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


#Imputar Medianas Manzanas del estrato

# Chapinero


housing_chapinero = housing_chapinero %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_estrato_vf = ifelse(is.na(new_estrato),
                              yes = estrato_mediana,
                              no = new_estrato))



# Poblado


housing_poblado = housing_poblado %>% group_by(MANZ_CCNCT) %>% 
  mutate(new_estrato_vf = ifelse(is.na(new_estrato),
                                 yes = estrato_mediana,
                                 no = new_estrato))


#-----Volvemos a validar NAs

#Chapinero

table(is.na(housing_chapinero$new_estrato_vf))

#Poblado

table(is.na(housing_poblado$new_estrato_vf))

sum(is.na(housing_chapinero$MANZ_CCNCT))

#Eliminamos NAs que no pudimos capturar

#Chapinero

housing_chapinero <- housing_chapinero[!is.na(housing_chapinero$new_piso_vf),]

#Poblado

housing_poblado <- housing_poblado[!is.na(housing_poblado$new_piso_vf),]











#####Modelos######
#Predicci贸n del precio


##Arboles de decisi贸n
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


