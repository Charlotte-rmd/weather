# 0- Library ───────────────────────────────────────────────────────────────
library(microclimf)
library(terra)
library(dplyr)

setwd("~/Desktop/Internship_Oxford")


# 1- Import data ───────────────────────────────────────────────────────────────

dtm_10m <- rast('dtm_10m.tif')
veg_10m <- readRDS('Microclimf/veg_10m.rds')
soil_10m <- readRDS('Microclimf/soil_10m.rds')


#2- Loop model step by step ────────────────────────────────────────────────────

dir.create("Microclimate_10m", showWarnings = FALSE)

a <- 1000
b <- 500
count <- 1

while (a < 20634) {
  
  weather_data <- weather_data_hourly[a:(a + b), ]
  
  micropoint_10m <- microclimf::runpointmodel(
    weather = weather_data, reqhgt = 2, dtm = dtm_10m,
    vegp = veg_10m, soilc = soil_10m,
    runchecks = (count == 1),  # only check once
    zref = 2, windhgt = 10
  )
  
  microclimate_10m  <- modelin(micropoint_10m, veg_10m, soil_10m, dtm_10m)
  micro_moisture_10m <- microclimf::soilmdistribute(microclimate_10m)
  micro_radiation_10m <- twostream(micro_moisture_10m, reqhgt = 2)
  micro_windspeed_10m <- wind(micro_radiation_10m, reqhgt = 2)
  micro_groundtemp_10m <- soiltemp(micro_windspeed_10m, reqhgt = 2)
  micro_airtemp_10m <- aboveground(micro_groundtemp_10m, reqhgt = 2)
  
  str(micro_airtemp_10m)
  
  saveRDS(micro_airtemp_10m, file = paste0("Microclimate_10m/microclimate_10m_", count, ".rds"))
  
  print(paste0("One file more : ", count, "/20634"))
  
  count <- count + 1
  a <- a + b
}
