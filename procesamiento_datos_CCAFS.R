#Juan Camilo Rivera

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

files <- list.files(here::here('CCAFS'),full.names = T)
files_CCAFS <- lapply(list.files("./CCAFS"), function (x)
                {
                    #Read file 
                    file <- read.table(paste0(getwd(),"/CCAFS/",x), header=T)
                    
                    #Hour format
                    file$Date <- as.Date(as.character(file$Date), format = "%Y%m%d")
                    
                    #Value format
                    file$Value <- as.numeric(file$Value)
                    
                    return(file)
    
})

#Name of stations
nam_files_CCAFS <- list.files(here::here('CCAFS'),full.names = F)
nam_CCAFS <- unique(sapply(strsplit(nam_files_CCAFS, "_"), "[", 1))

#Variable
variablename_CCAFS <- unique(sapply(strsplit(nam_files_CCAFS, "_"), "[", 6))

#Read the files
read_join <- lapply(nam_CCAFS, function (x){
    
    #Read files
    files <- list.files(here::here('CCAFS'),full.names = F, pattern = x)
    complete_names <- paste0(here::here('CCAFS'), "/",files)
   
    myfiles <- lapply(complete_names, function (x) {
    
                                                    readfile <-  read.table(x, header=T)
                                                    
                                                        
                                                    #Identify variable
                                                    splitpa <- basename (x)
                                                    variable <- sapply(strsplit(splitpa , "_"), "[", 6)
                                                    id_station  <- sapply(strsplit(splitpa , "_"), "[", 1)
                                            
                                                    
                                                    #Change labels columns
                                                    if(variable== "prec.txt")
                                                    {
                                                        colnames(readfile) <- c("DATE", "RAIN")
                                                    }
                                                    
                                                    if(variable== "rhum.txt")
                                                    {
                                                        colnames(readfile) <- c("DATE", "RH")
                                                    }
                                                    
                                                    if(variable== "tmax.txt")
                                                    {
                                                        colnames(readfile) <- c("DATE", "TMAX")
                                                    }
                                                    
                                                    if(variable== "tmin.txt")
                                                    {
                                                        colnames(readfile) <- c("DATE", "TMIN")
                                                    }
                                                    
                                                    
                                                    readfile$ID_STATION <- id_station 
                                                   
                                                    
                                                    return(readfile)
        
    })

    
    #Format of Date
    format_files <- lapply(myfiles, function(x){
                                                x$DATE <- as.Date(as.character(x$DATE), format ="%Y%m%d")
                                                x$SOURCE <- "CCAFS"
                                                return(x)
                                                })

    
    return(format_files)
})



#Join list 
list_data <- list()
for ( i in 1:length(read_join))
{
    
    list_data[[i]] <- Reduce(merge,read_join[[i]]) 
}


#All data without HR
datos_ccff_consol <- do.call(rbind,list_data)
datos_ccff_consol <-  datos_ccff_consol[,-5]




# Lectura de datos of Awhere.
# Awhere


files_aWhere <- lapply(list.files(here::here('Datos_Awhere'),full.names = T),read.xls)
nam_files_aWhere <- list.files(here::here('Datos_Awhere'))
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




#Join Awhere with CCAFS

#Data Consolidados
datos_climaticos_consol <- rbind(datos_awhere_consol,datos_ccff_consol)

#graphs 

variables_process <- gather(datos_climaticos_consol,variable,valor,-ID_STATION,-DATE,-SOURCE)
variables_process$SOURCE_ID_STATION  <- paste(variables_process$variable , datos_climaticos_consol$ID_STATION,sep= '-')

variables_process <- subset(variables_process, DATE >= "2015-03-22" &  DATE <="2016-01-10")



g <- ggplot(variables_process,aes(x= DATE, y =valor ))+
    geom_point(aes(colour=SOURCE,fill=SOURCE),alpha = 0.3)+
    facet_grid(variable ~ ID_STATION,scales = 'free')+#geom_smooth(aes(colour=SOURCE))+
    xlim(c(as.Date("2015-03-22"),as.Date("2016-01-10"))) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

g1 <- ggplot(variables_process,aes(x= DATE, y =valor ))+
    # geom_point(aes(colour=SOURCE,fill=SOURCE),alpha = 0.3)+
    facet_grid(variable ~ ID_STATION,scales = 'free')+geom_smooth(aes(colour=SOURCE))+
    xlim(c(as.Date("2015-03-22"),as.Date("2016-01-10"))) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(here::here('RESULTADOS','quality_awhere_nooa_CCFSA.png'),g,width =10 ,height =5 )
ggsave(here::here('RESULTADOS','quiality_awhere_nooa_CCFSA_2.png'),g1,width =10 ,height =5 )





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
datos_ccff_consol$MATCH <- paste(datos_ccff_consol$ID_STATION, "_", datos_ccff_consol$DATE)
datos_awhere_consol$MATCH <- paste(datos_awhere_consol$ID_STATION, "_", datos_awhere_consol$DATE)

#Delete colunm match 
datos_ccff_consol_aux <-subset(datos_ccff_consol, datos_ccff_consol$MATCH %in% datos_awhere_consol$MATCH)
datos_ccff_consol <- subset( datos_ccff_consol, select = -MATCH )
datos_awhere_consol <- subset( datos_awhere_consol, select = -MATCH )



datos_ccff_consol_ten <- do.call(rbind, divide_ten_days(datos_ccff_consol_aux))
datos_awhere_consol_ten <- do.call(rbind, divide_ten_days(datos_awhere_consol))
datos_climaticos_consol_ten <- rbind(datos_ccff_consol_ten,datos_awhere_consol_ten)
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

names(datos_ccff_consol_ten) <- paste(names(datos_ccff_consol_ten),"CCAFS" , sep='_')
datos_ccff_consol_ten$stat_date <- paste(datos_ccff_consol_ten$ID_STATION_CCAFS,datos_ccff_consol_ten$DATE_CCAFS,sep='_')
datos_awhere_consol_ten$stat_date <- paste(datos_awhere_consol_ten$ID_STATION,datos_awhere_consol_ten$DATE,sep="_")
names(datos_awhere_consol_ten) <- paste(names(datos_awhere_consol_ten),"aWhere", sep = "_")
consoloInfo <- merge(datos_awhere_consol_ten,datos_ccff_consol_ten,by.x = 'stat_date_aWhere',by.y ='stat_date' ,all.x =T,all.y = F,sort = F )
consoloInfo$ID_STATION_aWhere <- as.character(consoloInfo$ID_STATION_aWhere)







tmax <- consoloInfo[,c("ID_STATION_aWhere","TMAX_aWhere","TMAX_CCAFS")]

tmax <- tmax[complete.cases(tmax),]

tmax$var <- "tmax"

names(tmax)[2:3] <- c('aWhere','CCAFS')

tmin <- consoloInfo[,c("ID_STATION_aWhere","TMIN_aWhere","TMIN_CCAFS")]

tmin <- tmin[complete.cases(tmin),]

tmin$var <- 'tmin'

names(tmin)[2:3] <- c('aWhere','CCAFS')


rain <- consoloInfo[,c("ID_STATION_aWhere","RAIN_aWhere","RAIN_CCAFS")]

rain <- rain[complete.cases(rain),]

rain$var <- 'rain'

names(rain)[2:3] <- c('aWhere','CCAFS')

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


