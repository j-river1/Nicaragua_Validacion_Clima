
# Analisis exploratorio de datos información INAROSA 2017

# Hugo Andres Dorado B
# 05-02-2018

rm(list=ls())

library(here)
library(ggplot2)
library(tidyr)


# Analisis exploratorio de manejo

dataSetInarosa <- read.csv(here('DATOS','datos_inarosa_rendimiento.csv'))

summary(dataSetInarosa)

table(dataSetInarosa$LOTE,dataSetInarosa$CICLO)

png(here('RESULTADOS','distribucion_rendimiento.png'),height = 350  ,width = 600 )

layout(rbind(1:2))
hist(dataSetInarosa$RENDIMIENTO,breaks = 30,las=1,main = "Histograma",
     xlab = 'Rendimiento (QQ/MZ)',col = 'aquamarine2')

boxplot(dataSetInarosa$RENDIMIENTO,breaks = 40,las=1,main = 'Boxplot',
        ylab='Rendimiento (QQ/MZ)',col = 'aquamarine2')

abline(h = 240,lty =2)

dev.off()

dataSetInarosa <- dataSetInarosa[dataSetInarosa$RENDIMIENTO < 250 & dataSetInarosa$RENDIMIENTO > 15 ,]

ciclo_siembra_boxplot <- ggplot(dataSetInarosa,aes(x= CICLO,y=RENDIMIENTO))+geom_boxplot()+theme_bw()+
    ylab('Rendimiento (QQ/MZ)')+xlab('Ciclo Siembra')

ggsave(here("RESULTADOS/","ciclo_siembra_boxplot.png"),ciclo_siembra_boxplot ,width =6,height = 4)

ciclo_siembra_dispersion <- ggplot(dataSetInarosa,aes(x= CICLO,y=RENDIMIENTO))+geom_point()+
    geom_smooth(method = 'loess')+ylab('Rendimiento (QQ/MZ)')+
    xlab('Ciclo de siembra')+theme_bw()
ggsave(here("RESULTADOS/","ciclo_siembra_dispersion.png"),ciclo_siembra_dispersion ,width =6,height = 4)

sort(table(dataSetInarosa$VARIEDAD.REFINADA))


dataSetInarosa_var_selc <- dataSetInarosa[dataSetInarosa$VARIEDAD.REFINADA %in% c("F-50", "ANAR-97" ,"PALOS-2" ),]

dataSetInarosa_var_selc <- droplevels(dataSetInarosa_var_selc)

summary(dataSetInarosa_var_selc)

variedad <- ggplot(dataSetInarosa_var_selc,aes(x=VARIEDAD.REFINADA,y=RENDIMIENTO))+geom_boxplot()+
    xlab('Variedad')+ylab('Rendimiento (QQ/MZ)')+theme_bw()
ggsave(here("RESULTADOS/","variedad.png"),variedad ,width = 5,height =4 )

ciclo_variedad <- ggplot(dataSetInarosa_var_selc,aes(x= CICLO,y=RENDIMIENTO))+
    geom_boxplot(aes(fill=VARIEDAD.REFINADA))+
    xlab('Ciclo de siembra')+ylab('Rendimiento (QQ/MZ)')+theme_bw() + 
    guides(fill=guide_legend(title='Variedad'))

ggsave(here("RESULTADOS/",'ciclo_variedad.png'),ciclo_variedad ,width = 6,height = 4 )

# Analisis exploratorio de suelo


# https://en.wikipedia.org/wiki/Conductivity_(electrolytic)

# https://www.sciencedirect.com/science/article/pii/S1674775513000711

dataSetInarosaSuelo <- read.csv(here('DATOS','suelo.csv'))

soild_dataset <-gather(dataSetInarosaSuelo[c("id","pH", "CE_est_20", "arena_porcj","arcilla_porcj",
                             "limo_porcj")],variable,value,-id)

variables_suelo_pre_lim <- ggplot( soild_dataset , aes(x =variable ,y = value))+geom_boxplot()+
    facet_wrap(~variable,scales = "free")
ggsave(here("RESULTADOS/", "variables_suelo_pre_lim.png"),variables_suelo_pre_lim ,width = 6, height = 5)

dataSetInarosaSuelo$CE_est_20 [dataSetInarosaSuelo$CE_est_20  < 30  | dataSetInarosaSuelo$CE_est_20 > 700 ] <- NA


dataSetInarosaSuelo$pH[dataSetInarosaSuelo$pH >20] <- c(5.23,5.08)


soild_dataset_2 <-gather(dataSetInarosaSuelo[c("id","pH", "CE_est_20", "arena_porcj","arcilla_porcj",
                                             "limo_porcj")],variable,value,-id)


variables_suelo_pos_lim <- ggplot( soild_dataset_2 , aes(x =variable ,y = value))+geom_boxplot()+
    facet_wrap(~variable,scales = "free")
ggsave(here("RESULTADOS/","variables_suelo_pos_lim.png"), variables_suelo_pos_lim,width = 6,height = 5)
