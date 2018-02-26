
# 
# Hugo Andres Dorado

library(here)
library(tidyverse)
library(gdata)
library(zoo)
library(RcppRoll)
library(dplyr)


rm(list=ls())

# Lectura de datos

files_aWhere <- lapply(list.files(here::here('TEST_CLIMA','aWhereStations'),full.names = T),read.xls)

#save(files_aWhere, "Excel.RData")
load("Excel.RData")
nam_files_aWhere <- list.files(here::here('TEST_CLIMA','aWhereStations'))
station_id <- substring(nam_files_aWhere,1,6)
datos_aWhere <- lapply(seq(length(files_aWhere)),function(x)
                    {
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

nooa_aux <- nooa

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
    xlim(c(as.Date("2015-01-01"),as.Date("2015-12-31"))) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(here::here('RESULTADOS','CCAFS', 'quiality_awhere_CCAFS.png'),g,width =10 ,height =5 )
ggsave(here::here('RESULTADOS','CCAFS', 'quiality_awhere_CCAFS_2.png'),g1,width =10 ,height =5 )

# Validacion

names(nooa) <- paste(names(nooa),"NOAA" , sep='_')

nooa$stat_date <- paste(nooa$ID_STATION_NOAA,nooa$DATE_NOAA,sep='_')

datos_awhere_consol$stat_date <- paste(datos_awhere_consol$ID_STATION,datos_awhere_consol$DATE,sep="_")

names(datos_awhere_consol) <- paste(names(datos_awhere_consol),"aWhere", sep = "_")

consoloInfo <- merge(datos_awhere_consol,nooa,by.x = 'stat_date_aWhere',
      by.y ='stat_date' ,all.x =T,all.y = F,sort = F )


head(consoloInfo)


#Step by 10 days



#Arguments. data = number of data
divide_ten_days <- function(data)
{

    #Split table by station   
    
    small_table <- split(data, data$ID_STATION)
    
    #counts by 10 
    count_ten <- lapply(small_table, function(x)
                        {
        
                            #Starting
                            start <- as.Date(x$DATE[10])
        
                            #Rain sum
                            x$RAIN[is.na(x$RAIN)] <- 0 
                            
                            #Delete na
                            x <- na.omit(x)
                            
                            x$RAIN_TEN <- roll_sum(x$RAIN, n=10, fill = NA)
                            x$TMAX_TEN <- rollmean(x$TMAX, k=10, fill = NA)
                            x$TMIN_TEN <- rollmean(x$TMIN, k=10, fill = NA)
                            
                            lengthTabl <- as.numeric(length(x$TMAX))-6
                            #variables data frame
                            data_rain <- x$RAIN_TEN[seq(5, lengthTabl , by= 10)]
                            data_tmax <- x$TMAX_TEN[seq(5, lengthTabl , by=10)]
                            data_tmin <- x$TMIN_TEN[seq(5, lengthTabl , by=10)]
                            lengthdate <- length(data_rain)
                            days <- seq(start,by= 10 ,length.out = lengthdate)
                            
                            #data frame
                            data_ten <- data.frame(DATE = days,  ID_STATION= rep(unique(x$ID_STATION), lengthdate), 
                                                   RAIN = data_rain, TMAX = data_tmax, TMIN= data_tmin, 
                                                   SOURCE = rep(unique(x$SOURCE), lengthdate))
                            return (data_ten )
                 
                        })
    

    
    return (count_ten )
    
}


#Join list
#Ten days
#choose rank of date

#Join two columns
nooa$MATCH <- paste(nooa$ID_STATION, "_", nooa$DATE)
datos_awhere_consol$MATCH <- paste(datos_awhere_consol$ID_STATION, "_", datos_awhere_consol$DATE)

#Delete colunm match 
nooa_aux <-subset(nooa, nooa$MATCH %in% datos_awhere_consol$MATCH)
nooa <- subset( nooa, select = -MATCH )
datos_awhere_consol <- subset( datos_awhere_consol, select = -MATCH )



nooa_ten <- do.call(rbind, divide_ten_days(nooa_aux))
datos_awhere_consol_ten <- do.call(rbind, divide_ten_days(datos_awhere_consol))
datos_climaticos_consol_ten <- rbind(nooa_ten,datos_awhere_consol_ten)
variables_process_ten <- gather(datos_climaticos_consol_ten,variable,valor,-ID_STATION,-DATE,-SOURCE)



#join
variables_process_ten$SOURCE_ID_STATION  <- paste(variables_process_ten$variable , datos_climaticos_consol_ten$ID_STATION,sep= '-')

#
g_ten <- ggplot(variables_process_ten,aes(x= DATE, y =valor ))+
    geom_point(aes(colour=SOURCE,fill=SOURCE),alpha = 0.3)+
    facet_grid(variable ~ ID_STATION,scales = 'free')+
    xlim(c(as.Date("2014-01-01"),as.Date("2017-12-31"))) +
    theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

g1_ten <- ggplot(variables_process,aes(x= DATE, y =valor ))+
    # geom_point(aes(colour=SOURCE,fill=SOURCE),alpha = 0.3)+
    facet_grid(variable ~ ID_STATION,scales = 'free')+geom_smooth(aes(colour=SOURCE))+
    xlim(c(as.Date("2015-01-01"),as.Date("2015-12-31"))) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave(here::here('RESULTADOS','GSOD', '10_days', 'quality_awhere_CCAFS.png'),g_ten,width =10 ,height =5 )
ggsave(here::here('RESULTADOS','GSOD', '10_days', 'quality_awhere_CCAFS_2.png'),g1_ten,width =10 ,height =5 )



# Validacion of ten 

names(nooa_ten) <- paste(names(nooa_ten),"NOAA" , sep='_')
nooa_ten$stat_date <- paste(nooa_ten$ID_STATION_NOAA,nooa_ten$DATE_NOAA,sep='_')
datos_awhere_consol_ten$stat_date <- paste(datos_awhere_consol_ten$ID_STATION,datos_awhere_consol_ten$DATE,sep="_")
names(datos_awhere_consol_ten) <- paste(names(datos_awhere_consol_ten),"aWhere", sep = "_")
consoloInfo <- merge(datos_awhere_consol_ten,nooa_ten,by.x = 'stat_date_aWhere',by.y ='stat_date' ,all.x =T,all.y = F,sort = F )
consoloInfo$ID_STATION_aWhere <- as.character(consoloInfo$ID_STATION_aWhere)







tmax <- consoloInfo[,c("ID_STATION_aWhere","TMAX_aWhere","TMAX_NOAA")]

tmax <- tmax[complete.cases(tmax),]

tmax$var <- "tmax"

names(tmax)[2:3] <- c('aWhere','NOOA')

tmin <- consoloInfo[,c("ID_STATION_aWhere","TMIN_aWhere","TMIN_NOAA")]

tmin <- tmin[complete.cases(tmin),]

tmin$var <- 'tmin'

names(tmin)[2:3] <- c('aWhere','NOOA')


rain <- consoloInfo[,c("ID_STATION_aWhere","RAIN_aWhere","RAIN_NOAA")]

rain <- rain[complete.cases(rain),]

rain$var <- 'rain'

names(rain)[2:3] <- c('aWhere','NOOA')

data_to_test <- rbind(tmax,tmin,rain)

data_to_test$ID_STATION_aWhere_var <- paste(data_to_test$ID_STATION_aWhere,data_to_test$var,sep='_')

split_ci <- split(data_to_test,data_to_test$ID_STATION_aWhere_var)

rsquareMSE <- do.call(rbind,
                      lapply(split_ci,function(x){
                          data.frame( rsquare = round((cor(x[,2],x[,3]))^2 *100,2), 
                                      MSE = (sqrt(sum( (x[,2]-x[,3])^2 )/nrow(x))) ,aWhere = min(x$aWhere) + (max(x$aWhere)-min(x$aWhere))/4  ,
                                      NOOA =  max(x$NOOA) - (max(x$NOOA)-min(x$NOOA))/6)}))

rsquareMSE$ID_STATION_aWhere_var <- row.names(rsquareMSE)


rsquareMSE$PERFORMANCE <- paste("(R2 = ",rsquareMSE$rsquare, ' %, ',"MSE=",round(rsquareMSE$MSE,2), ')',sep="")

gEnd <- ggplot(data_to_test,aes(x=aWhere,y=NOOA))+geom_point(colour='red')+theme_bw()+
    facet_wrap(~ ID_STATION_aWhere_var ,scales='free',nrow = 7,ncol=3)+
    geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)+geom_text(data=rsquareMSE,aes(label=PERFORMANCE),size=2.5)+ylab("GSOD")


ggsave(here::here('RESULTADOS', 'GSOD', '10_days', 'Validacion.png'),gEnd,height = 8,width =10 )


