# compile assessment and prediction model
{
  # rm(list = ls())
  library(dplyr)
  library(data.table)
  library(anytime)
  library(lubridate)
}

list.assessment <- list.files(
  'G:/My Drive/01. BMKG/MOS/data/', full.names = T, pattern = 'assessment'
)

model.assessment <- lapply(
  list.assessment, function(input) {
    tabel <- read.csv(input)
  }
) %>% 
  rbindlist


#################################################
list.predict <- list.files(
  'G:/My Drive/01. BMKG/MOS/data/', full.names = T, pattern = 'prediction'
)

model.predict <- lapply(
  list.predict, function(input) {
    tabel <- read.csv(input)
  }
) %>% 
  rbindlist

View(model.predict)
str(model.predict)

model.predict$date <- ifelse(model.predict$parameter=="curah hujan", "2022-2-8 15:00:00", model.predict$date)
model.predict$date <- as.POSIXct(model.predict$date,format="%Y-%m-%d %H:%M:%S")
model.predict <- data.frame(model.predict)

model.predict$tgl <- as.Date(model.predict$date,format="%Y-%m-%d") 

# model.predict[is.na(model.predict$tgl),6] <- min(model.predict$tgl, na.rm=T)

agg <- model.predict %>%
  # select(date, location, parameter, model, tgl) %>%
  group_by(location, parameter, model, tgl) %>%
  mutate(min=min(prediction, na.rm=T),
         max=max(prediction, na.rm=T)) %>%
  arrange(location)

a <- agg %>%
  filter()
# max <- model.predict %>%
#   # select(date, location, parameter, model, tgl) %>%
#   group_by(location, parameter, model, tgl) %>%
#   mutate(max=max(prediction, na.rm=T)) %>%
#   arrange(location)
# 
# gabung <- merge.data.frame(
#   min, max, by = c('date', 'location', 'parameter', 'model', 'tgl', 'prediction'), all.x = TRUE, all.y = TRUE,
#   sort = FALSE
# )
# 
# model.predict.fix <- merge.data.frame(
#   gabung, min, by = c('date', 'location', 'parameter', 'model', 'tgl', 'prediction'), all.x = TRUE, all.y = FALSE,
#   sort = FALSE
# )

#####################################################
# write.csv(model.assessment, "G:/My Drive/01. BMKG/MOS/data/all model ass.csv", row.names=F)
write.csv(agg, "G:/My Drive/01. BMKG/MOS/data/all model pred.csv", row.names=F)


#################
## curah hujan confusion matrix
df <- read.csv("G:/My Drive/01. BMKG/MOS/data/curah hujan model prediction.csv")

df.obs <- df %>%
  filter(model=="baseline")

df.pred <- df %>%
  filter(model!="baseline")

joined <- inner_join(
  df.pred,
  df.obs,
  by = c("date", "location", "parameter"),
  suffix = c("_p", "_o")
) %>%
  mutate(truth = case_when(
    prediction_p == 2 & prediction_o == 2  ~ "TP",
    prediction_p == 1 & prediction_o == 1  ~ "TN",
    prediction_p == 2 & prediction_o == 1  ~ "FP",
    prediction_p == 1 & prediction_o == 2  ~ "FN",
  ))

table(joined$truth)

write.csv(joined, "G:/My Drive/01. BMKG/MOS/data/cm_curah hujan.csv", row.names=F)


##########################
############################## prediksi
# data nwp
{
  # rm(list = ls())
  library(dplyr)
  library(data.table)
  library(anytime)
  library(lubridate)
}

nama.file.nwp.12 <- list.files(
  'G:/My Drive/01. BMKG/MOS/data/data prediksi', full.names = T, pattern = 'S'
)

df <- lapply(
  nama.file.nwp.12, function(input) {
    tabel <- fread(input, header=T)[, waktu := ymd_h(waktu)] %>% 
      setnames('waktu', 'Date')
    waktu1 <- tabel[1, Date]
    tabel[, hari := difftime(Date, waktu1, units = 'days') %>% as.integer]
    tabel[, UTC := 12]
  }
) %>% 
  rbindlist

df$Date <- as.character(df$Date)
is.na(df[,3:5]) <- df[,3:5]=="Request"
df[,3:34] <- lapply(df[,3:34], as.numeric)
df$lokasi <- as.factor(df$lokasi)
df$Date <- as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")

loc <- levels(df$lokasi)
levels(df$lokasi)[1] <- "SMP1"
loc


str(df)

a <- read.table("G:/My Drive/01. BMKG/MOS/data/data_fix_rh_obs.txt", sep=',')

colnames(df) <- colnames(a)[1:35]
# precipitation, temperature, humidity
df1 <- na.omit(df)
summary(df1)

# handle precipitation
colnames(df1)
df1 <- df1 %>%
  mutate(pre.nwp = select(., 3:5) %>% rowSums(na.rm = TRUE))

summary(df1)

str(df1)

# pre define rainfall intensity category with hourly category times three times 0.75
summary(df1$pre.nwp)

# df1$pre.nwp.group <- ifelse(df1$pre.nwp < 0.3*0.75, "no rain",
#                             ifelse(df1$pre.nwp <= 15*0.75, "light rain",
#                                    ifelse(df1$pre.nwp <= 30*0.75, "moderate rain",
#                                           ifelse(df1$pre.nwp <= 60*0.75, "heavy rain",
#                                                  ifelse(df1$pre.nwp > 60*0.75, "violent rain",
#                                                         NA))))) %>% as.factor()


df1$pre.nwp.group <- ifelse(df1$pre.nwp < 0.3*0.75, "no rain", "rain") %>% as.factor()
table(df1$pre.nwp.group)
df1.1 <- df1[,-c(3,4,5)]
df1.1 <- subset(df1.1, select=-c(clmix.kg.kg., hari, UTC))



# load best fit model for humidity
# SMB Bagged Tree
hr_smb <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/H_bagged tree_SMB.rds")

df.smb <- na.omit(df) %>%
  filter(lokasi == "SMB") 

rownames(df.smb) <- df.smb$Date

df.smb <- subset(df.smb, select=-c(1,2,34,35,5,15))
hr_smb_pred <- predict(hr_smb, df.smb)

# SMJ conf Tree
hr_smj <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/H_conf tree_SMJ.rds")

df.smj <- na.omit(df) %>%
  filter(lokasi == "SMJ") 

rownames(df.smj) <- df.smj$Date

df.smj <- subset(df.smj, select=-c(1,2,34,35,5,15))
hr_smj_pred <- predict(hr_smj, df.smj)

# SMK decision tree
hr_smk <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/H_decision tree_SMK.rds")

df.smk <- na.omit(df) %>%
  filter(lokasi == "SMK") 

rownames(df.smk) <- df.smk$Date

df.smk <- subset(df.smk, select=-c(1,2,34,35,5,15))
hr_smk_pred <- predict(hr_smk, df.smk)

# SMP1 conf tree
hr_smp1 <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/H_conf tree_SMP1.rds")

df.smp1 <- na.omit(df) %>%
  filter(lokasi == "SMP1") 

rownames(df.smp1) <- df.smp1$Date

df.smp1 <- subset(df.smp1, select=-c(1,2,34,35,5,15))
hr_smp1_pred <- predict(hr_smp1, df.smp1)

# SMP2 bagged tree
hr_smp2 <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/H_bagged tree_SMP2.rds")

df.smp2 <- na.omit(df) %>%
  filter(lokasi == "SMP2") 

rownames(df.smp2) <- df.smp2$Date

df.smp2 <- subset(df.smp2, select=-c(1,2,34,35,5,15))
hr_smp2_pred <- predict(hr_smp2, df.smp2)


# load best fit model for temperature
# SMB conf Tree
t_smb <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/T_conf tree_SMB.rds")


t_smb_pred <- predict(t_smb, df.smb)

# SMJ conf Tree
t_smj <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/T_conf tree_SMJ.rds")
t_smj_pred <- predict(t_smj, df.smj)

# SMK baseline / conf tree
t_smk <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/T_conf tree_SMK.rds")
t_smk_pred <- predict(t_smk, df.smk)

# SMP1 conf tree
t_smp1 <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/T_conf tree_SMP1.rds")
t_smp1_pred <- predict(t_smp1, df.smp1)

# SMP2 decision tree
t_smp2 <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/T_decision tree_SMP2.rds")
t_smp2_pred <- predict(t_smp2, df.smp2)

# load best fit model for precipitation
# SMB baseline / linear
p_smb <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/prec_linear_SMB.rds")

df.p.smb <- na.omit(df1.1) %>%
  filter(lokasi == "SMB") 

rownames(df.p.smb) <- df.p.smb$Date

p_smb_pred <- predict(p_smb, df.p.smb)

# SMJ linear
p_smj <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/prec_linear_SMJ.rds")

df.p.smj <- na.omit(df1.1) %>%
  filter(lokasi == "SMJ") 

rownames(df.p.smj) <- df.p.smj$Date

p_smj_pred <- predict(p_smj, df.p.smj)

# SMK linear
p_smk <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/prec_linear_SMK.rds")

df.p.smk <- na.omit(df1.1) %>%
  filter(lokasi == "SMK") 

rownames(df.p.smk) <- df.p.smk$Date

p_smk_pred <- predict(p_smk, df.p.smk)

# SMP1 PLS
p_smp1 <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/prec_PLS_SMP1.rds")

df.p.smp1 <- na.omit(df1.1) %>%
  filter(lokasi == "SMP1") 

rownames(df.p.smp1) <- df.p.smp1$Date

p_smp1_pred <- predict(p_smp1, df.p.smp1)

# SMP2 elastic
p_smp2 <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/prec_elastic_SMP2.rds")

df.p.smp2 <- na.omit(df1.1) %>%
  filter(lokasi == "SMP2") 

rownames(df.p.smp2) <- df.p.smp2$Date

p_smp2_pred <- predict(p_smp2, df.p.smp2)

# load best fit model for curah hujan
# SMB bagged tree
rf_smb <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/curah hujan_bagged tree_SMB.rds")

df.p.smb <- na.omit(df1.1) %>%
  filter(lokasi == "SMB") 

rownames(df.p.smb) <- df.p.smb$Date
df.p.smb <- subset(df.p.smb, select=-c(Date, lokasi))

rf_smb_pred <- predict(rf_smb, df.p.smb)

# SMJ bagged tree
rf_smj <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/curah hujan_bagged tree_SMJ.rds")

df.p.smj <- na.omit(df1.1) %>%
  filter(lokasi == "SMJ") 

rownames(df.p.smj) <- df.p.smj$Date
df.p.smj <- subset(df.p.smj, select=-c(Date, lokasi))

rf_smj_pred <- predict(rf_smj, df.p.smj)

# SMK bagged tree
rf_smk <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/curah hujan_bagged tree_SMK.rds")

df.p.smk <- na.omit(df1.1) %>%
  filter(lokasi == "SMK") 

rownames(df.p.smk) <- df.p.smk$Date
df.p.smk <- subset(df.p.smk, select=-c(Date, lokasi))

rf_smk_pred <- predict(rf_smk, df.p.smk)

# SMP1 bagged tree
rf_smp1 <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/curah hujan_bagged tree_SMP1.rds")

df.p.smp1 <- na.omit(df1.1) %>%
  filter(lokasi == "SMP1") 

rownames(df.p.smp1) <- df.p.smp1$Date
df.p.smp1 <- subset(df.p.smp1, select=-c(Date, lokasi))

rf_smp1_pred <- predict(rf_smp1, df.p.smp1)

# SMP2 bagged tree
rf_smp2 <- readRDS("G:/My Drive/01. BMKG/MOS/data/model/curah hujan_bagged tree_SMP2.rds")

df.p.smp2 <- na.omit(df1.1) %>%
  filter(lokasi == "SMP2") 

rownames(df.p.smp2) <- df.p.smp2$Date
df.p.smp2 <- subset(df.p.smp2, select=-c(Date, lokasi))

rf_smp2_pred <- predict(rf_smp2, df.p.smp2)

## saving
ls()

Pattern1<-grep("pred",names(.GlobalEnv),value=TRUE)
# Pattern1_list<-do.call("rbind",mget(Pattern1))
# a <- bind_rows(Pattern1_list, .id = "column_label")
# 
# df <- data.frame(matrix(unlist(Pattern1_list), byrow=TRUE))
# 

pred.table <- list()
pred.table[[1]] <- cbind(unlist(hr_smb_pred), "SMB", 'humidity')
pred.table[[2]] <- cbind(unlist(hr_smj_pred), "SMJ", 'humidity')
pred.table[[3]] <- cbind(unlist(hr_smk_pred), "SMK", 'humidity')
pred.table[[4]] <- cbind(unlist(hr_smp1_pred), "SMP1", 'humidity')
pred.table[[5]] <- cbind(unlist(hr_smp2_pred),"SMP2", 'humidity')

pred.table[[6]] <- cbind(unlist(t_smb_pred), "SMB", 'temperature')
pred.table[[7]] <- cbind(unlist(t_smj_pred), "SMJ", 'temperature')
pred.table[[8]] <- cbind(unlist(t_smk_pred), "SMK", 'temperature')
pred.table[[9]] <- cbind(unlist(t_smp1_pred), "SMP1", 'temperature')
pred.table[[10]] <- cbind(unlist(t_smp2_pred),"SMP2", 'temperature')

pred.table[[11]] <- cbind(unlist(p_smb_pred), "SMB", 'precipitation')
pred.table[[12]] <- cbind(unlist(p_smj_pred), "SMJ", 'precipitation')
pred.table[[13]] <- cbind(unlist(p_smk_pred), "SMK", 'precipitation')
pred.table[[14]] <- cbind(unlist(p_smp1_pred), "SMP1", 'precipitation')
pred.table[[15]] <- cbind(unlist(p_smp2_pred),"SMP2", 'precipitation')

pred.table[[16]] <- cbind(unlist(rf_smb_pred), "SMB", 'rainfall')
pred.table[[17]] <- cbind(unlist(rf_smj_pred), "SMJ", 'rainfall')
pred.table[[18]] <- cbind(unlist(rf_smk_pred), "SMK", 'rainfall')
pred.table[[19]] <- cbind(unlist(rf_smp1_pred), "SMP1", 'rainfall')
pred.table[[20]] <- cbind(unlist(rf_smp2_pred),"SMP2", 'rainfall')

pred.tab <- data.frame(do.call(rbind, pred.table))
colnames(pred.tab) <- c('pred','lokasi', 'parameter')

date <- df.p.smp1$Date
pred.tab$date <- rep(date,times=20)

# write.csv(pred.tab, "G:/My Drive/01. BMKG/MOS/data/hasil prediksi.csv", row.names=F)

# data observasi

obs <- fread('G:/My Drive/01. BMKG/MOS/data/data obs -ME48-2022-10-10-2022-10-14.csv')
View(obs)

colnames(obs) <- c('station_id', 'Date', 'pre.obs', 'Temp', 'T max', 'T min', 'humidity')

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
obs$pre.obs <- ifelse(obs$pre.obs==8888, 0, 
                      ifelse(obs$pre.obs==9999, NA,
                             obs$pre.obs))
obs$lokasi <- as.factor(obs$lokasi)

str(obs)
obs <- as.data.frame(na.omit(obs[,-c(1,5,6)]))

str(obs)
obs.melt <- melt(obs, id=c('lokasi', 'Date'))
obs.melt$note <- "observasi"
colnames(obs.melt)
obs.melt <- obs.melt[,c(4,1,3,2,5)]
write.csv(obs.melt, "G:/My Drive/01. BMKG/MOS/data/obs untuk hasil prediksi.csv", row.names=F)

