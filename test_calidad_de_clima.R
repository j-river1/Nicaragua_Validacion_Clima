
# 
# Hugo Andres Dorado

library(here)
library(tidyverse)
library(gdata)


rm(list=ls())

# Lectura de datos

files_aWhere <- lapply(list.files(here::here('TEST_CLIMA','aWhereStations'),
                                  full.names = T),read.xls)

nam_files_aWhere <- list.files(here::here('TEST_CLIMA','aWhereStations'))

station_id <- substring(nam_files_aWhere,1,6)

datos_aWhere <- lapply(seq(length(files_aWhere)),function(x){
    data.frame(ID_STAT = station_id[x], files_aWhere[[x]],SOURCE = "aWhere")
})

names(datos_aWhere) <- station_id

nooa <- read.table(here::here('TEST_CLIMA','CDO1840167555083.txt'),sep=',',header=T)

head(nooa)

nooa <- nooa[c("STN...","YEARMODA","MAX","MIN","PRCP")]

names(nooa) <- c("ID_STATION","DATE","TMAX","TMIN","RAIN")

nooa$DATE <- as.Date(paste(substring(nooa$DATE,1,4),substring(nooa$DATE,5,6),substring(nooa$DATE,7,8),sep="-"))

nooa$TMAX <- gsub(" ","", nooa$TMAX)

nooa$TMAX <- gsub("*","", nooa$TMAX)

nooa$TMAX <-  as.character(nooa$TMAX)

nooa$TMAX <- as.numeric(gsub("[*]","", nooa$TMAX))

nooa$TMIN <- gsub(" ","", nooa$TMIN)

nooa$TMIN <-  as.character(nooa$TMIN)

nooa$TMIN <- as.numeric(gsub("[*]","", nooa$TMIN))

nooa$RAIN <- gsub(" ","", nooa$RAIN)

nooa$RAIN <- gsub("A|I|H|G","", nooa$RAIN)

nooa$RAIN <-  as.character(nooa$RAIN)

nooa$RAIN <- as.numeric(nooa$RAIN)

nooa$TMAX[nooa$TMAX == 9999.9] <- NA

nooa$TMIN[nooa$TMIN == 9999.9] <- NA

nooa$RAIN[nooa$RAIN == 99.99]  <- NA

nooa$RAIN <- nooa$RAIN*25.4

#----------------------------------------------------------------------------
# (Fahrenheit - 32) / 1.8

nooa$TMAX  <-  (nooa$TMAX - 32) / 1.8

nooa$TMIN  <-  (nooa$TMIN - 32) / 1.8

nooa$SOURCE <- "NOAA"

datos_awhere_consol <- do.call(rbind,datos_aWhere)

datos_awhere_consol <-  datos_awhere_consol[c("ID_STAT","date","temp_max", "temp_min" ,"precipitation","SOURCE")]

names(datos_awhere_consol)[1:5] <- c("ID_STATION",'DATE',"TMAX","TMIN","RAIN")

names(datos_awhere_consol)

datos_awhere_consol$DATE <- as.Date(as.character(datos_awhere_consol$DATE))


datos_climaticos_consol <- rbind(nooa,datos_awhere_consol)

variables_process <- gather(datos_climaticos_consol,variable,valor,-ID_STATION,-DATE,-SOURCE)

head(datos_climaticos_consol)

variables_process$SOURCE_ID_STATION  <- paste(variables_process$variable , datos_climaticos_consol$ID_STATION,sep= '-')

g <- ggplot(variables_process,aes(x= DATE, y =valor ))+
    geom_point(aes(colour=SOURCE,fill=SOURCE),alpha = 0.3)+
    facet_grid(variable ~ ID_STATION,scales = 'free')+#geom_smooth(aes(colour=SOURCE))+
    xlim(c(as.Date("2014-01-01"),as.Date("2017-12-31"))) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

g1 <- ggplot(variables_process,aes(x= DATE, y =valor ))+
   # geom_point(aes(colour=SOURCE,fill=SOURCE),alpha = 0.3)+
    facet_grid(variable ~ ID_STATION,scales = 'free')+geom_smooth(aes(colour=SOURCE))+
    xlim(c(as.Date("2014-01-01"),as.Date("2017-12-31"))) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(here::here('RESULTADOS','quiality_awhere_nooa_GSOD.png'),g,width =10 ,height =5 )
ggsave(here::here('RESULTADOS','quiality_awhere_nooa_2.png'),g1,width =10 ,height =5 )

# Validacion

names(nooa) <- paste(names(nooa),"NOAA" , sep='_')

nooa$stat_date <- paste(nooa$ID_STATION_NOAA,nooa$DATE_NOAA,sep='_')

datos_awhere_consol$stat_date <- paste(datos_awhere_consol$ID_STATION,datos_awhere_consol$DATE,sep="_")

names(datos_awhere_consol) <- paste(names(datos_awhere_consol),"aWhere", sep = "_")

consoloInfo <- merge(datos_awhere_consol,nooa,by.x = 'stat_date_aWhere',
      by.y ='stat_date' ,all.x =T,all.y = F,sort = F )


head(consoloInfo)

