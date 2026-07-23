

library(terra)
library(dplyr)
library(stringr)
library(readr)
library(pbapply)
library(sf)

setwd("~/Desktop/Internship_Oxford/Comparaison")

temp_microclimate <- read.csv("temperature_microclimate_BNG.csv")

test <- rast("Comparaison/Thermolog_interpolated/temp_raster_2023-04-01_00-00-00.tif")
# crs(test) => British National Grid : EPSG : 27700


temp_thermologgers <- read.csv("Comparaison/temperature_thermologgers_BNG.csv")




# Import database ──────────────────────────────────────────────────────────────

temp_microclim <- rast("Temp_air_res=100m.tif")
temp_thermolog <- read.csv("thermolog_hourly_temp.csv")


plot(thermolog$temp ~ microclimate$temp, 
     xlab = 'Temperature at 2 m', ylab = 'Temperature at 10 m',
     main = 'Temperature comparison between predictions by era5 at 2 m \nand modelled at 10 m above ground',
     cex.main = 1, family = "serif",
     pch = 15, cex= 0.5, col = 'slategray')
abline(a = 0, b = 1, lwd = 1, col = 'darkred'



# Extract the data from the microclimate model ─────────────────────────────────

## Get coordinates of all centers
coordinates <- xyFromCell(temp_microclim, 1:ncell(temp_microclim))
dtm_100m <- readRDS('Microclimf/dtm_100m.rds')

ext(temp_microclim)  <- ext(dtm_100m)
crs(temp_microclim) <- crs(dtm_100m)



#Wide format has one row per cell, one column per time step
#long format has one row per cell per time step
library(tidyr)

temp_microclim_frame <- as.data.frame(temp_microclim, xy = TRUE)


micropoint_100m <- readRDS("micropoint_100m.rds")
micropoint_100m$tmeorig

tme <- as.character(micropoint_100m$tmeorig)
colnames(temp_microclim_frame)[3:26306] <- tme

temp_long <- temp_microclim_frame %>%
  pivot_longer(
    cols      = starts_with(c("2023", "2024", "2025")),
    names_to  = "datetime",
    values_to = "temp"
  )

write.csv(temp_long, "temperature_microclimate_BNG.csv")



# Extract the data from the thermologgers data ─────────────────────────────────



# List all tiffs in the folder
tiff_files <- list.files(pattern = "^temp_raster_.*\\.tif$",
  full.names = TRUE)

# Extract unique (x, y) coordinate pairs
unique_coords <- unique(temp_microclimate[, c("x", "y")])

# Convert to a SpatVector of points
pts <- vect(unique_coords, geom = c("x", "y"), crs = "EPSG:27700")


# Wrap extraction in a function
extract_tiff <- function(tiff_path) {
  
  r <- rast(tiff_path)
  pts_proj <- project(pts, crs(r))
  
  fname <- basename(tiff_path)
  date_part <- sub("temp_raster_(\\d{4}-\\d{2}-\\d{2})_(\\d{2}-\\d{2}-\\d{2})\\.tif", "\\1 \\2", fname)
  date_part <- gsub("(\\d{2})-(\\d{2})-(\\d{2})$", "\\1:\\2:\\3", date_part)
  datetime  <- as.POSIXct(date_part, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  extracted <- extract(r, pts_proj)
  
  data.frame(
    temperature = extracted[, 2],
    datetime    = datetime
  )
}


# Not recognized : file1 <- rast("temp_raster_2023-04-14_02-00-00.tif")
file2 <- rast("temp_raster_2023-10-09_09-00-00.tif")
# file3 <- rast("temp_raster_2023-08-18_04-00-00.tif")
file4 <- rast( "temp_raster_2023-08-14_04-00-00.tif")

# Run with progress bar
results_list <- pblapply(tiff_files, extract_tiff)

# Combine
final_dataset <- bind_rows(results_list)

sum(is.na(final_dataset$temperature)) #test if there are NAs

#Export hourly data
final_dataset_daily_means<-final_dataset %>% 
  mutate(day=substr(datetime, 1, 10))

##test number of observations per day
test1<-final_dataset_daily_means %>% 
  group_by(day) %>% 
  mutate(contar=length(temperature)) %>% 
  ungroup() #there are 3264 per day (24*136) as it should

test2<-final_dataset_daily_means %>% 
  group_by(TreeID) %>% 
  mutate(contar=length(temperature)) %>% 
  ungroup() #there are 19056 observations per tree, as it should

final_dataset_daily_means<-final_dataset_daily_means %>% 
  group_by(day, TreeID) %>% 
  summarise(mean_daily_temp=mean(temperature),
            mean_daily_sd=sd(temperature),
            mean_daily_se=mean_daily_sd/sqrt(length(temperature))) %>% 
  ungroup()

##Export
write.csv(final_dataset, "final_dataset_perhour_updated_Rogue2.csv")
write.csv(final_dataset_daily_means, "final_dataset_daily_means_updated_Rogue2.csv")
```








