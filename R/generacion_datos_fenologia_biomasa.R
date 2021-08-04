library(tidyverse)
library(dplyr)
library(lubridate)
library(raster)
library(sf)
library(stars)
library(dplyr)
library(SPEI)
library(sirad)
library(naniar)

in.biomasa<-"data/biomasa"
in.vector<-"data/shp"
in.raster2<-"E:/ALBERTO/HEMERA/PROYECTO/sen2agri"
out.csv<-"output/csv"

#Leer datos ----------------------------------------------------------------------
#raster fenologia
list.files(in.raster2, full.names = T, pattern = "l3b_ndvi_05") %>%
  list.files(., full.names = T, pattern = "metric_estimation.tif$") %>%
  read_stars()->pheno2

list.files(in.raster2, full.names = T, pattern = "l3b_ndvi_05") %>%
  list.files(., full.names = T, pattern = "all_ndvis.tif$") %>%
  read_stars()->ndvi2


#raster lai
list.files(in.raster2, full.names = T, pattern = "l3b_lai_05") %>%
  list.files(., full.names = T, pattern = "LAI_img.tif$") %>%
  read_stars()->lai
names(lai)<-substr(names(lai), start = 12, stop=19)


#vectores
list.files(in.vector, full.names = T, pattern="coberturas2.shp$") %>%
  read_sf()->coberturas

list.files(in.vector, full.names = T, pattern="pts_muestreo_maiz.shp$") %>%
  read_sf() %>%
  st_transform(., crs=st_crs(pheno2))%>%
  mutate(muestra=c(6,7,8,9,10)) %>% 
  dplyr::select("muestra")->pts_maiz

list.files(in.vector, full.names = T, pattern="pts_muestreo_trigo.shp$") %>%
  read_sf() %>%
  st_transform(., crs=st_crs(pheno2)) %>%
  mutate(muestra=c(1,2,3,4,5)) %>%
  dplyr::select("muestra")->pts_trigo

puntos<-rbind(pts_trigo,pts_maiz)


#biomasa
dir(in.biomasa, full.names = T, pattern = "trigo") %>%
  read.csv(., sep=";") %>% as_tibble() %>%
  mutate(cultivo="trigo")->datos.trigo

dir(in.biomasa, full.names = T,  pattern = "maiz") %>%
  read.csv(., sep=";") %>% as_tibble() %>%
  mutate(cultivo="maiz") %>%
  mutate(muestra=muestra+5)->datos.maiz # se cambio la numeracion de las muestras



#Biomasa    ---------------------------------------------------------------------------------
names(datos.trigo)<-names(datos.maiz)
rbind(datos.trigo,datos.maiz) %>%
  mutate(fecha=dmy(fecha))->datos.muestreo



#Fenologia   --------------------------------------------------------------------------------
#fechas ndvi
list.files(in.raster2, full.names = T, pattern = "l3b_ndvi_05") %>%
  list.files(., full.names = T, pattern = "all_dates") %>%
  read.csv(header=FALSE)->fechas

fechas.n18<-fechas$V1 %>% ymd()

#extrarer fenologia
st_extract(pheno2,puntos) %>%
  as_tibble() %>% 
  spread(band,metric_estimation.tif )%>%
  mutate(punto=1:10) %>% # son 5 bandas
  mutate(cultivo=rep(c("trigo", "maiz"), c(5,5)), variable="feno") %>%
  gather(band, valor, -punto, -cultivo,-geometry, -variable) %>%
  mutate(metric_fecha=as.Date(valor-1, origin="2020-01-01"))->tabla.f

#extrarer ndvi
ndvi2 %>%
  st_extract(.,puntos) %>%
  as_tibble() %>% 
  spread(band,all_ndvis.tif)->ndvi3

names(ndvi3)<-c("geometry", as.character(fechas.n18))
ndvi3[c(-11,-15)]->ndvi4 #porque hay dos fechas repetidas

names(ndvi4)
ndvi4 %>%
  mutate(punto=1:10) %>% 
  mutate(cultivo=rep(c("trigo", "maiz"), c(5,5)), variable="ndvi") %>%
  gather(fecha, valor, -punto, -cultivo, -geometry, -variable) %>%
  mutate(fecha=ymd(fecha)) %>%
  filter(valor>=0) %>%
  group_by(cultivo, punto) %>%
  mutate(cndvi=cumsum(valor))->tabla.n

#LAI   --------------------------------------------------------------------------------
#extraer lai
st_extract(lai,puntos) %>%
  as_tibble() %>%
  mutate(punto=1:10, cultivo=rep(c("trigo", "maiz"), c(5,5))) %>%
  gather(fecha, valor, -geometry, -punto, -cultivo) %>%
  mutate(variable="lai", fecha=ymd(fecha)) %>%
  filter( valor>=0) %>%
  group_by(cultivo, punto) %>%
  mutate(clai=cumsum(valor))->tabla.lai


#Exportar datos   ----------------------------------------------------------------------
setwd(out.csv)
write_delim(datos.muestreo, "datos_muestreo.csv", delim = ";")
write_delim(tabla.f, "datos_feno.csv", delim = ";" )
write_delim(tabla.n, "datos_ndvi.csv", delim = ";")
write_delim(tabla.lai, "datos_lai.csv", delim = ";")

