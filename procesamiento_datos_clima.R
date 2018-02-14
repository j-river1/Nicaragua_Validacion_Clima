
rm(list=ls())

library(here)
library(ggplot2)
library(scales)
library(dplyr)
library(plyr)
library(tidyr)

fechas_ciclos <- read.csv(here::here("DATOS","fecha_siembra_cosecha.csv"))

fechas_ciclos$Fecha_siembra_ini <- as.Date(fechas_ciclos$Fecha_siembra_ini,
                                           "%m/%d/%Y")

fechas_ciclos$Fecha_siembra_fin <- as.Date(fechas_ciclos$Fecha_siembra_fin,
                                           "%m/%d/%Y")

fechas_ciclos$Fecha_cosecha_ini <- as.Date(fechas_ciclos$Fecha_cosecha_ini,
                                           "%m/%d/%Y")

fechas_ciclos$Fecha_cosecha_fin <- as.Date(fechas_ciclos$Fecha_cosecha_fin,
                                           "%m/%d/%Y")



fechas_ciclos$Fecha_siembra_ini;fechas_ciclos$Fecha_cosecha_fin



rects <- data.frame(ciclo = fechas_ciclos$CICLO,xstart = fechas_ciclos$Fecha_siembra_ini, 
                    xend = fechas_ciclos$Fecha_cosecha_fin  )

rects$ciclo <- factor(rects$ciclo,levels=c("VER. 13","INV. 13","VER. 14","INV. 14",
                                           "VER. 15", "INV. 15", "VER. 16","INV. 16",     "VER. 17"))


g <- ggplot()+geom_segment(data=rects,aes(x = xstart, y = ciclo, xend = xend, yend = ciclo))+
    geom_point(data=rects,aes(x = xstart, y = ciclo))+
    geom_point(data=rects,aes(x = xend, y = ciclo))+
    scale_x_date(date_breaks = "6 month",labels = date_format("%m-%Y"),
                 limits = as.Date(c('2012-09-01','2017-06-01')))+xlab('Fecha')+
    ylab('Periodo')
    

ggsave(here::here("RESULTADOS/",'ciclo_siembra_epoca.png'),g ,width = 6,height = 4 )


climaAwhere <- read.csv(here::here('DATOS','GDA_AWHERE_Weather.csv'))

climaAwhere$date <- as.Date(climaAwhere$date,"%Y-%m-%d")

climaAwhere <- climaAwhere[c('date','temp_max','temp_min','precipitation',
                             'humid_max','humid_min','solar' )]

names(climaAwhere)[2:7] <- c('TX','TM','P','RHX','RHM','SR')

dataSetWeather <- gather(climaAwhere,variable,value,-date)

dataSetWeather$variable <- factor(dataSetWeather$variable,levels = c('TX','TM','P','RHX','RHM','SR'))

ghrap_weather <- ggplot(dataSetWeather,aes(x= date,y = value))+geom_point()+geom_smooth()+
    facet_grid(variable~.,scales = 'free_y')+theme_bw()


ggsave(here::here("RESULTADOS","weather_info.png"),ghrap_weather,
       width = 3.2,height = 6)

