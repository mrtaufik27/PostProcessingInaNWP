##############################################################################################
##                               WEB SCRAPPING OGIMET                                       ## 
##############################################################################################
# options("install.lock"=FALSE)
# per 3 hour
# required library
library(climate)
library(dplyr)

station_1 <- tryCatch({meteo_ogimet(date = c("2021-2-27", "2022-3-25"), 
                                    interval = "hourly",
                                    coords = FALSE, 
                                    station = 96933)}, error = function(e) e)

station_2 <- tryCatch({meteo_ogimet(date = c("2021-2-27", "2022-3-25"), 
                                    interval = "hourly",
                                    coords = FALSE, 
                                    station = 96937)}, error = function(e) e)

station_3 <- tryCatch({meteo_ogimet(date = c("2021-2-27", "2022-3-25"), 
                                    interval = "hourly",
                                    coords = FALSE, 
                                    station = 96935)}, error = function(e) e)

station_4 <- tryCatch({meteo_ogimet(date = c("2021-2-27", "2022-3-25"), 
                                    interval = "hourly",
                                    coords = FALSE, 
                                    station = 96973)}, error = function(e) e)

station_5 <- tryCatch({meteo_ogimet(date = c("2021-2-27", "2022-3-25"), 
                                    interval = "hourly",
                                    coords = FALSE, 
                                    station = 96987)}, error = function(e) e)

meteo_data <- rbind(station_1, station_2, station_3, station_4, station_5)

# getwd()
# setwd("G:/My Drive/05. BMKG/MOS")
# write.csv2(meteo_data, "D:/MOS/3hourly_meteo.csv")
