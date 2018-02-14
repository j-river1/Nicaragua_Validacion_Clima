
rm(list=ls())

library(raster)
library(rgdal)
library(here)
library(tidyr)

lotes_inarosa  <- readOGR(here::here("DATOS","Diseño muestreo","Dmues_5HA.shp"))

suelo_datos <- read.csv(here::here('DATOS','suelo.csv'))

lotes_inarosa_df <- as.data.frame(lotes_inarosa)

names(lotes_inarosa_df)[3:4] <- c('logitude','latitude')

lotes <- readOGR(here::here('DATOS','Lotes','Lotes.shp'))

datasetsoilMerge <- data.frame(
           Puntos =lotes_inarosa_df$Puntos, 
           Lote = extract(lotes,lotes_inarosa_df[,3:4])$LOTE
           )

datasetsoilMerge$Puntos <- as.numeric(as.character(datasetsoilMerge$Puntos ))

datasetsoilMerge <- merge(datasetsoilMerge,suelo_datos,by.x='Puntos',
                          by.y = 'id',all.x=T,all.y=F)


head(datasetsoilMerge)

soil_consolidado <- ddply(datasetsoilMerge,~Lote,summarise,pH = mean(pH),CE_est_20 = mean(CE_est_20),
      arena_porcj=mean(arena_porcj),arcilla_porcj = mean(arcilla_porcj),limo_porcj = mean(limo_porcj))

summary(soil_consolidado)


soil_consolidado <- soil_consolidado[complete.cases(soil_consolidado),]

write.csv(soil_consolidado,here::here('DATOS','soil_consolidado.csv'),row.names = F)

