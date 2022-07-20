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
Pol_db  <- st_bbox(db)
class(Pol_db)


##Chapinero
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)
train_chapinero <- st_crop (train, chapinero)

##Poblado
poblado <- getbb(place_name = "Comuna 14 - El Poblado", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") 
leaflet() %>% addTiles() %>% addPolygons(data=poblado)
train_poblado <- st_crop (train , poblado)

available_features() #Escoger features 
##Variables de OSM

###bares
bar <- opq(bbox = st_bbox(Pol_db)) %>%
  add_osm_feature(key = "amenity", value = "bar")
class(bar)

osm_bar<- bar %>% osmdata_sf()
bares <- osm_bar$osm_points %>% select(osm_id,amenity) 
class(bares)

###estaciones de bus
estacion_bus <- opq(bbox = st_bbox(Pol_db)) %>%
  add_osm_feature(key = "amenity", value = "bus_station")
class(estacion_bus)

osm_bs<- estacion_bus %>% osmdata_sf()
bus_station <- osm_bs$osm_points %>% select(osm_id,amenity) 

###parques
parques <- opq(bbox = st_bbox(Pol_db)) %>%
  add_osm_feature(key = "leisure", value = "park")

osm_ps<- parques %>% osmdata_sf()
parks <- osm_ps$osm_polygons %>% select(osm_id,leisure) 

###bancos
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



