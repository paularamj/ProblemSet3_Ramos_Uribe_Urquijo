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
train<-readRDS("C:/Users/pau_9/Documents/GitHub/ProblemSet3_Ramos_Uribe_Urquijo/dataPS3/train.Rds")
#train<-readRDS("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 3/ProblemSet3_Ramos_Uribe_Urquijo/dataPS3/train.Rds")
test<-readRDS("C:/Users/pau_9/Documents/GitHub/ProblemSet3_Ramos_Uribe_Urquijo/dataPS3/test.Rds")
#test<-readRDS("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 3/ProblemSet3_Ramos_Uribe_Urquijo/dataPS3/test.Rds")

##########Data########
db<- st_as_sf(x=train,coords=c("lon","lat"),crs=4326) ##Lectura de datos espaciales
leaflet() %>% addTiles() %>% addCircles(data=db)
class(db)

##Chapinero
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)
train_chapinero <- st_crop(db, chapinero)

##Poblado
poblado <- getbb(place_name = "Comuna 14 - El Poblado", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") 
leaflet() %>% addTiles() %>% addPolygons(data=poblado)
train_poblado <- st_crop (db, poblado)

available_features() #Escoger features 

######Variables de OSM######

###Variable: Bares

bar_chap <- opq(bbox = st_bbox(chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bar")
class(bar_chap)
osm_bar_chap<- bar_chap %>% osmdata_sf()
##points
bares_chapinero <- osm_bar$osm_points %>% select(osm_id,amenity) 
class(bares_chapinero)

bar_pob <- opq(bbox = st_bbox(poblado)) %>%
  add_osm_feature(key = "amenity", value = "bar")
class(bar_pob)
osm_bar_pob<- bar_pob %>% osmdata_sf()
##points
bares_poblado <- osm_bar_pob$osm_points %>% select(osm_id,amenity) 
class(bares_poblado)

###Variable: Estaciones de bus
est_bus_chap <- opq(bbox = st_bbox(chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bus_station")
class(est_bus_chap)
#lista de los objetos
osm_eb_chap<- est_bus_chap%>% osmdata_sf()
#points
estaciones_chapinero <- osm_eb_chap$osm_points %>% select(osm_id,amenity) 

est_bus_pob <- opq(bbox = st_bbox(poblado)) %>%
  add_osm_feature(key = "amenity", value = "bus_station")
class(est_bus_pob)
osm_eb_pob<- est_bus_pob%>% osmdata_sf()
#points
estaciones_poblado <- osm_eb_pob$osm_points %>% select(osm_id,amenity) 

###Variable: Parques

parques_chap <- opq(bbox = st_bbox(chapinero)) %>%
  add_osm_feature(key = "leisure", value = "park")
osm_park_cha<- parques_chap %>% osmdata_sf()
#poligonos
parques_chapinero<- osm_park_cha$osm_polygons %>% select(osm_id,leisure) 

parques_pob <- opq(bbox = st_bbox(poblado)) %>%
  add_osm_feature(key = "leisure", value = "park")
osm_park_pob<- parques_pob %>% osmdata_sf()
#poligonos
parques_pob <- osm_park_pob$osm_polygons %>% select(osm_id,leisure) 

###Variable: Bancos
bancos = opq(bbox = st_bbox(Pol_db)) %>%
  add_osm_feature(key = "amenity", value = "bank")

osm_bks<- bancos %>% osmdata_sf()
banks <- osm_bks$osm_points %>% select(osm_id,amenity) 

##Inspección gráfica
leaflet() %>% addTiles() %>% 
  addCircleMarkers(data=db , col="red", weight=2)%>% #datos
  addPolygons(data=parks , col="green") %>%  #parques
  addCircles(data=bares , col="blue") %>%  #bares
  addCircles(data=banks , col="black" , weight=2)%>% # bancos
  addCircles(data=bus_station , col="yellow")  #estaciones de bus

##Variables de description


##Agregar a la base de datos 

###Estadisticas descriptivas y mapas


#####Modelos######



