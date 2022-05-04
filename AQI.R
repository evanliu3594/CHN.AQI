library(tidyverse)
require(zoo)

DaliyMeanConc <- function(Conc) Conc %>% summarise(
  across(c(SO2,NO2,CO,PM2.5,PM10), mean),
  across(O3,~ max(zoo::rollmean(O3,k = 8,fill = 0,align = 'right')))
)

IAQI_hourly <- function(Pollu,Conc){
  
  if (!Pollu %in% c('SO2','NO2','CO','O3')) 
    stop(paste(Pollu, "不是1小时IAQI的计算项目",sep = ''))
  
  else{
    AQI_table <- list(
      AQI = c(0,  50, 100, 150,  200,  300,  400,  500, Inf),
      SO2 = c(0, 150, 500, 650,  800, Inf),
      NO2 = c(0, 100, 200, 700, 1200, 2340, 3090, 3840, Inf),
      CO =  c(0,   5,  10,  35,   60,   90,  120,  150, Inf),
      O3 =  c(0, 160, 200, 400,  800, 1000, 1200,  Inf)
    )
    
    i_min <- AQI_table[[Pollu]][which(AQI_table[[Pollu]] < Conc) %>% last]
    i_max <- AQI_table[[Pollu]][which(AQI_table[[Pollu]] >= Conc) %>% first]
    
    a_min <- AQI_table[["AQI"]][which(AQI_table[[Pollu]] < Conc) %>% last]
    a_max <- AQI_table[["AQI"]][which(AQI_table[[Pollu]] >= Conc) %>% first]
    
    if(i_max == Inf) return(Inf)
    else return(a_min + (a_max - a_min) * (Conc - i_min) / (i_max - i_min))
  }
}

AQI_Hourely <- function(SO2,NO2,CO,O3){
  max(
    IAQI_hourly("SO2",SO2),
    IAQI_hourly("NO2",NO2),
    IAQI_hourly("CO",CO),
    IAQI_hourly("O3",O3)
  )
}

IAQI_Daily <- function(Pollu,Conc) {
  
  if (!Pollu %in% c('SO2','NO2','CO','O3','PM10','PM2.5')) {
    stop(paste(Pollu, "不是1小时IAQI的计算项目",sep = ''))
  } else {
    AQI_table <- list(
      AQI =   c(0,  50, 100, 150, 200,  300,  400,  500, Inf),
      SO2 =   c(0,  50, 150, 475, 800, 1600, 2100, 2620, Inf),
      NO2 =   c(0,  40,  80, 180, 280,  565,  750,  940, Inf),
      CO =    c(0,   2,   4,  14,  24,   36,   48,   60, Inf),
      O3 =    c(0, 100, 160, 215, 265,  800,  Inf),
      PM10 =  c(0,  50, 150, 250, 350,  420,  500,  600, Inf),
      PM2.5 = c(0,  35,  75, 115, 150,  250,  350,  500, Inf)
    )
    
    i_min <- AQI_table[[Pollu]][which(AQI_table[[Pollu]] < Conc) %>% last]
    i_max <- AQI_table[[Pollu]][which(AQI_table[[Pollu]] >= Conc) %>% first]
    
    a_min <- AQI_table[["AQI"]][which(AQI_table[[Pollu]] < Conc) %>% last]
    a_max <- AQI_table[["AQI"]][which(AQI_table[[Pollu]] >= Conc) %>% first]
    
    if(i_max == Inf) return(Inf)
    else return(a_min + (a_max - a_min) * (Conc - i_min) / (i_max - i_min))
  }
}

AQI_Daily <- function(SO2,NO2,CO,PM10,PM2.5,O3){
  max(
    IAQI_Daily("SO2",SO2),
    IAQI_Daily("NO2",NO2),
    IAQI_Daily("CO",CO),
    IAQI_Daily("PM10",PM10),
    IAQI_Daily("PM2.5",PM2.5),
    IAQI_Daily("O3",O3)
  )
}
