#Script works for reading CCAFS

#Read the files 

#Libraries
rm(list=ls())
library(here)
library(plyr)
library(gdata)
library(tidyr)
library(dplyr)
library(ggplot2)

#Read files and put format 

files <- list.files(here::here('Miramontes'),full.names = T)
files_FincaMiram <- lapply(list.files(here::here('Miramontes'),full.names = T), function (x)
                          {
                                file <- read.xls(x)
                                colnames (file) <- c("DATE", "RAIN", "TMAX", "TMIN", "RH", "SR")
                                file$DATE <- as.Date(as.character(file$DATE), format= "%Y-%m-%d")
                                file$SOURCE <- "FincaMiramontes"
                                file$ID_STATION <- "FincaMiramontes"
                                return(file)
                          })

#All data without HR
datos_fincamira_consol <- do.call(rbind,files_FincaMiram)
datos_fincamira_consol <-  datos_fincamira_consol [,c(-5,-6)]

# Lectura de datos of Awhere.
# Awhere


files_aWhere <- lapply(list.files(here::here('Datos_Awhere_Miramo'),full.names = T),read.xls)
nam_files_aWhere <- list.files(here::here('Datos_Awhere_Miramo'))
station_id  <- sapply(strsplit(nam_files_aWhere , "_"), "[", 1)


datos_aWhere <- lapply(seq(length(files_aWhere)),function(x)
{
    data.frame(ID_STAT = station_id[x], files_aWhere[[x]],SOURCE = "aWhere")
})

names(datos_aWhere) <- station_id
datos_awhere_consol <- do.call(rbind,datos_aWhere)
datos_awhere_consol <-  datos_awhere_consol[c("ID_STAT","date","temp_max", "temp_min" ,"precipitation","SOURCE")]
names(datos_awhere_consol)[1:5] <- c("ID_STATION",'DATE',"TMAX","TMIN","RAIN")
names(datos_awhere_consol)
datos_awhere_consol$DATE <- as.Date(as.character(datos_awhere_consol$DATE))




#Join Awhere with Miramontes

#Data Consolidados
datos_climaticos_consol <- rbind(datos_awhere_consol,datos_fincamira_consol)

#graphs 

variables_process <- gather(datos_climaticos_consol,variable,valor,-ID_STATION,-DATE,-SOURCE)
variables_process$SOURCE_ID_STATION  <- paste(variables_process$variable , datos_climaticos_consol$ID_STATION,sep= '-')

variables_process <- subset(variables_process, DATE >= "2013-01-01" &  DATE <="2015-12-31")



g <- ggplot(variables_process,aes(x= DATE, y =valor ))+
    geom_point(aes(colour=SOURCE,fill=SOURCE),alpha = 0.3)+
    facet_grid(variable ~ ID_STATION,scales = 'free')+#geom_smooth(aes(colour=SOURCE))+
    xlim(c(as.Date("2013-01-01"),as.Date("2015-12-31"))) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

g1 <- ggplot(variables_process,aes(x= DATE, y =valor ))+
    # geom_point(aes(colour=SOURCE,fill=SOURCE),alpha = 0.3)+
    facet_grid(variable ~ ID_STATION,scales = 'free')+geom_smooth(aes(colour=SOURCE))+
    xlim(c(as.Date("2013-01-01"),as.Date("2015-12-31"))) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(here::here('RESULTADOS','Finca_Miramontes', 'quality_awhere_Miramontes.png'),g,width =10 ,height =5 )
ggsave(here::here('RESULTADOS','Finca_Miramontes','quality_awhere_Miramontes_2.png'),g1,width =10 ,height =5 )



#Validación

names(datos_fincamira_consol) <- paste(names(datos_fincamira_consol),"FincaMiramontes" , sep='_')
datos_fincamira_consol$stat_date <- paste(datos_fincamira_consol$ID_STATION_FincaMiramontes,datos_fincamira_consol$DATE_FincaMiramontes,sep='_')
datos_awhere_consol$stat_date <- paste(datos_awhere_consol$ID_STATION,datos_awhere_consol$DATE, sep="_")
names(datos_awhere_consol) <- paste(names(datos_awhere_consol),"aWhere", sep = "_")
consoloInfo <- merge(datos_awhere_consol,datos_fincamira_consol,by.x = 'stat_date_aWhere',by.y ='stat_date' ,all.x =T,all.y = F,sort = F )


head(consoloInfo)
consoloInfo$ID_STATION_aWhere <- as.character(consoloInfo$ID_STATION_aWhere)
tmax <- consoloInfo[,c("ID_STATION_aWhere","TMAX_aWhere","TMAX_FincaMiramontes")]
tmax <- tmax[complete.cases(tmax),]
tmax$var <- "tmax"
names(tmax)[2:3] <- c('aWhere','FincaMiramontes')
tmin <- consoloInfo[,c("ID_STATION_aWhere","TMIN_aWhere","TMIN_FincaMiramontes")]
tmin <- tmin[complete.cases(tmin),]
tmin$var <- 'tmin'
names(tmin)[2:3] <- c('aWhere','FincaMiramontes')


rain <- consoloInfo[,c("ID_STATION_aWhere","RAIN_aWhere","RAIN_FincaMiramontes")]

rain <- rain[complete.cases(rain),]

rain$var <- 'rain'

names(rain)[2:3] <- c('aWhere','FincaMiramontes')

data_to_test <- rbind(tmax,tmin,rain)

data_to_test$ID_STATION_aWhere_var <- paste(data_to_test$ID_STATION_aWhere,data_to_test$var,sep='_')

split_ci <- split(data_to_test,data_to_test$ID_STATION_aWhere_var)

rsquareMSE <- do.call(rbind, lapply(split_ci,function(x)
                     {
                      data.frame( rsquare = round((cor(x[,2],x[,3]))^2 *100,2), 
                                  MSE = (sqrt(sum( (x[,2]-x[,3])^2 )/nrow(x))) ,aWhere = min(x$aWhere) + (max(x$aWhere)-min(x$aWhere))/4  ,
                                  FincaMiramontes =  max(x$FincaMiramontes) - (max(x$FincaMiramontes)-min(x$FincaMiramontes))/6)}))

rsquareMSE$ID_STATION_aWhere_var <- row.names(rsquareMSE)


rsquareMSE$PERFORMANCE <- paste("(R2 = ",rsquareMSE$rsquare, ' %, ',"MSE=",round(rsquareMSE$MSE,2), ')',sep="")

gEnd <- ggplot(data_to_test,aes(x=aWhere,y=FincaMiramontes))+geom_point(colour='red')+theme_bw()+
    facet_wrap(~ ID_STATION_aWhere_var ,scales='free',nrow = 7,ncol=3)+
    geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)+geom_text(data=rsquareMSE,aes(label=PERFORMANCE),size=2.5)+ylab("FincaMiramontes")


ggsave(here::here('RESULTADOS', 'Finca_Miramontes', 'Validacion.png'),gEnd,height = 8,width =10 )
ggplot(consoloInfo,aes(x=TMAX_aWhere,y=))+geom_point()


#Model ARIMA









