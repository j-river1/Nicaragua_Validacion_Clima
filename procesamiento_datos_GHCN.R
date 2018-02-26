#Author: Juan Camilo Rivera

#Script works for reading files from GHCN and convert to format for using our code.

#Libraries
rm(list=ls())
library(here)
library(plyr)



#Start date
start_data <- 2013
end_data <- 2018




#**Read files** 
# the files with extention .dly. These files are in folder GHCN
files_GHCN <- lapply(list.files(here::here('GHCND'),full.names = T, pattern = ".dly"),
                     read.fwf, widths = c(11, 4, 2, 4, rep(c(5, 1, 1, 1),31)))

#Name of stations
nam_files_GHCN <- list.files(here::here('GHCND'),full.names = F, pattern = ".dly")
station_id <- substring(nam_files_GHCN,7,11)
datos_GHCN <- lapply(seq(length(files_GHCN)),
                     function(x){
                     data.frame(ID_STAT = station_id[x], files_GHCN[[x]],SOURCE = "GHCN")
                     })
names(datos_GHCN) <- station_id



#**Conditions star and end data, names columns **
#Choose files data with meet start and end data.
choose_files <- lapply(datos_GHCN, function(x)
{
    #Choosing the variables and labeling
    
    #days
    days <-  c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17", "18","19","20",
               "21", "22", "23", "24", "25", "26", "27", "28","29", "30", "31")
    
    #quality days
    quality_days <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8","Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15",
     "Q16", "Q17", "Q18", "Q19", "Q20","Q21", "Q22","Q23", "Q24","Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31") 
    
    
    
    #change labels columns fo days
    colnames(x)[seq(6, 129, by=4)] <- days
    colnames(x)[seq(8, 129, by=4)] <- quality_days
    colnames(x)[3] <- "year"
    colnames(x)[4] <- "month"
    colnames(x)[5] <- "variable"
    
    x <- x[c("year","month", "variable", days, quality_days )]

                            
    #rows meet with star and end date and quality day equals to blank PRCP TAVG TMAX TMIN
    files <- subset (x, year >= start_data & year <= end_data)
    files$variable <- as.character(files$variable)
    files$variable [files$variable=="PRCP"] <-"RAIN" 
    
    #Choose the rows with variables 
    index <- which(files$variable %in% c("RAIN", "TMAX", "TMIN"))
    files <- files[index, ]
    
    
    

    if(nrow(files)==0)
    {
        files <- NULL
    }
    
    return(files)

}
)                      
choose_files <- choose_files[!sapply(choose_files, is.null)] 
 
#According to https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt the variable QFLAQ1 (where 1 represents
#first day of month) is quality of data, if it is blank then data is ok!.

#Change the labels for days.


    
    
    
 