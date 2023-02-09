##############################################################################################
##                                 DATA PRE-PROCESSIONG                                         ## 
##############################################################################################

#####################################
## 1. MERGE NWP DATA (nwp_gabung.txt)
#####################################

{
  # rm(list = ls())
  library(dplyr)
  library(data.table)
  library(anytime)
  library(lubridate)
}


## UTC 00
nama.file.nwp.00 <- list.files(
  'G:/My Drive/01. BMKG/MOS/data/00', full.names = T, pattern = 'SM'
)

nama.file.nwp.12 <- list.files(
  'G:/My Drive/01. BMKG/MOS/data/12', full.names = T, pattern = 'SM'
)


isi.nwp.00 <- lapply(
  nama.file.nwp.00, function(input) {
    tabel <- fread(input)[, waktu := ymd_h(waktu)] %>% 
      setnames('waktu', 'Date')
    waktu1 <- tabel[1, Date]
    tabel[, hari := difftime(Date, waktu1, units = 'days') %>% as.integer]
    tabel[, UTC := 00]
  }
) %>% 
  rbindlist

isi.nwp.12 <- lapply(
  nama.file.nwp.12, function(input) {
    tabel <- fread(input)[, waktu := ymd_h(waktu)] %>% 
      setnames('waktu', 'Date')
    waktu1 <- tabel[1, Date]
    tabel[, hari := difftime(Date, waktu1, units = 'days') %>% as.integer]
    tabel[, UTC := 12]
  }
) %>% 
  rbindlist

isi.nwp <- rbind(isi.nwp.00, isi.nwp.12)

# isi.nwp %>% fwrite('D:/MOS/nwp_gabung.txt')

################################################
## 2. MERGE NWP & OGIMET (gabung_ogimet_NWP.txt)
################################################


nwp <- fread('G:/My Drive/01. BMKG/MOS/data/nwp_gabung.txt')
View(nwp)

obs <- fread('G:/My Drive/01. BMKG/MOS/data/observasi -ME48-2021-02-27-2022-03-31.csv')
View(obs)

colnames(obs) <- c('station_id', 'Date', 'pre-obs', 'T-obs', 'RH-obs')

set.lokasi <- function(input) switch (
  input,
  '96933' = 'SMP1',
  '96937' = 'SMP2',
  '96935' = 'SMJ',
  '96973' = 'SMK',
  '96987' = 'SMB'
)

obs$lokasi <- (sapply(obs$station_id %>% as.character, set.lokasi))
obs$Date <- as.POSIXct(obs$Date,format="%Y-%m-%d %H:%M:%S") 

nwp$lokasi <- (nwp$lokasi)
str(obs)
str(nwp)
obs <- na.omit(obs)

obs$Date <- as.character(obs$Date)
nwp$Date <- as.character(nwp$Date)

gabung <- merge.data.table(
  nwp, obs, by = c('Date', 'lokasi'), all.x = TRUE, all.y = FALSE,
  sort = FALSE
)

str(gabung)
View(df)
# fwrite(gabung, 'G:/My Drive/01. BMKG/MOS/data/gabung_obs_NWP.txt')

################################################
## 3. DATA MANAGEMENT
################################################

# diambil data terbaru, 
# exclude missng value

{
  # rm(list = ls())
  library(dplyr)
  library(data.table)
  library(anytime)
  library(lubridate)
}

data <- read.table("G:/My Drive/01. BMKG/MOS/data/gabung_obs_NWP.txt", sep = ',', header = T)

View(data)
colnames(data)
str(data)

colnames(data)
colnames(data)[35] <- "UTC"
data$UTC <- as.integer(substr(data$UTC, 4, 5))
sapply(data, class)
is.na(data[,3:5]) <- data[,3:5]=="Request"
colnames(data)
data[,3:34] <- lapply(data[,3:34], as.numeric)
# df1[,6:34,] <- df1[,6:34,] %>% mutate_if(is.character,as.numeric)
data$lokasi <- as.factor(data$lokasi)
loc <- unique(data$lokasi)
str(data)

data$Date <- as.POSIXct(data$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
difftime(max(data$Date, na.rm = T), min(data$Date, na.rm = T), units = "days")
length(unique(data$Date))

df <- data %>% 
  group_by(Date, lokasi) %>% 
  filter(hari==min(hari),
         UTC==max(UTC)) %>%
  arrange(lokasi, Date)



# precipitation
df1 <- df %>%
  select(1:35,37 ) %>% as.data.frame()

# handle precipitation
colnames(df1)
df1$pre.nwp <- (df1$con_prec.mm.+df1$mic_prec.mm.+df1$sha_prec.mm.)


df1$pre.obs <- ifelse(df1$pre.obs==8888, 0, 
                      ifelse(df1$pre.obs==9999, NA,
                             df1$pre.obs))
str(df1)

# pre define rainfall intensity category with hourly category times three times 0.75
summary(df1$pre.obs)
summary(df1$pre.nwp)

df1$pre.obs.group <- ifelse(df1$pre.obs < 0.3*0.75, "no rain",
                            ifelse(df1$pre.obs <= 15*0.75, "light rain",
                                   ifelse(df1$pre.obs <= 30*0.75, "moderate rain",
                                          ifelse(df1$pre.obs <= 60*0.75, "heavy rain",
                                                 ifelse(df1$pre.obs > 60*0.75, "violent rain",
                                                        NA))))) %>% as.factor()

df1$pre.nwp.group <- ifelse(df1$pre.nwp < 0.3*0.75, "no rain",
                            ifelse(df1$pre.nwp <= 15*0.75, "light rain",
                                   ifelse(df1$pre.nwp <= 30*0.75, "moderate rain",
                                          ifelse(df1$pre.nwp <= 60*0.75, "heavy rain",
                                                 ifelse(df1$pre.nwp > 60*0.75, "violent rain",
                                                        NA))))) %>% as.factor()


summary(df1$pre.obs.group)
summary(df1$pre.nwp.group)

colnames(df1)

df1.1 <- df1[,-c(3,4,5)]

colnames(df1.1)

  
# Temperature
df2 <- df %>%
  select(1:35,38 ) 

# Humidity
df3 <- df %>%
  select(1:35,39 ) 

# write.table(df1.1, 'G:/My Drive/01. BMKG/MOS/data/data_complete_prec.txt', sep=',')
# write.table(df2, 'G:/My Drive/01. BMKG/MOS/data/data_complete_temp.txt', sep=',')
# write.table(df3, 'G:/My Drive/01. BMKG/MOS/data/data_complete_rh.txt', sep=',')
