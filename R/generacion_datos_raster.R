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
out.tif<-"output/tif"

#Leer datos ----------------------------------------------------------------------
#raster fenologia
list.files(in.raster2, full.names = T, pattern = "l3b_ndvi_05") %>%
  list.files(., full.names = T, pattern = "all_ndvis.tif$") %>%
  stack()->ndvi
  read_stars()->ndvi2

#fechas ndvi
list.files(in.raster2, full.names = T, pattern = "l3b_ndvi_05") %>%
  list.files(., full.names = T, pattern = "all_dates") %>%
  read.csv(header=FALSE)->fechas
fechas.n18<-fechas$V1 %>% ymd()
ndvi2 %>%   st_set_dimensions(3, values = fechas.n18)->ndvi2 
ndvi2 [[1]][ndvi2 [[1]] < 0]=NA



#NDVI   --------------------------------------------------------------------------------
#extrarer ndvi

coberturas %>%
  filter(LC=="cultivo", id=="10") %>%
  st_geometry() ->trigo.sf

coberturas %>%
  filter(LC=="cultivo", id=="6") %>%
  st_geometry()->maiz.sf

st_crop(ndvi2,maiz.sf) %>%
  st_as_stars()->ndvi.maiz
st_crop(ndvi2,trigo.sf) %>%
  st_as_stars()->ndvi.trigo

ndvi.maiz[[1]][ndvi.maiz[[1]] < 0]=NA
ndvi.trigo[[1]][ndvi.trigo[[1]] < 0]=NA

color2<-colorRampPalette(c("#C2543C", "#D97529","#EDA813", "#F7D707", "#C6F700","#35E300", "#0EC441", "#1E9E84", 
                           "#166D8A", "#0C2F7A"))
plot(ndvi.maiz,col = color2(10), breaks=seq(0,1, by=0.1), box_col = grey(1))
plot(ndvi.trigo,col = color2(10), breaks=seq(0,1, by=0.1), box_col = grey(1))




#exportar
setwd(out.tif)
write_stars(ndvi.maiz, "ndvi_maiz.tif")
write_stars(ndvi.trigo, "ndvi_trigo.tif")


# library(ggplot2)
# library(viridis)
# library(ggthemes)
# 
ggplot() +
  geom_stars(data = ndvi.trigo[1], aes(x, y, fill=all_ndvis.tif)) +
  facet_wrap("band") +
  scale_fill_gradientn(colours = color2(10))+
  coord_equal() +
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))


# 
# 
# 
# 
ggplot() + geom_stars(data = ndvi.trigo) +
  coord_equal() +
  facet_wrap(~band) +
  theme_void() +
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))
