##############################################################################################
##                                 EXPLORATORY DATA ANALYSIS                                         ## 
##############################################################################################

{
  library(ggplot2)
  library(data.table)
  library(tidyverse)
  library(naniar)
  
}

df1 <- fread("G:/My Drive/01. BMKG/MOS/data/gabung_obs_NWP.txt")
df2 <- read.table("G:/My Drive/01. BMKG/MOS/data/data_complete_prec.txt", sep=',', header=T)
# df2 <- read.table("G:/My Drive/01. BMKG/MOS/data/data_complete_temp.txt", sep=',', header=T)
# df3 <- read.table("G:/My Drive/01. BMKG/MOS/data/data_complete_rh.txt", sep=',', header=T)

str(df1)
df1$Date <- as.POSIXct(df1$Date, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
df1$lokasi <- as.factor(df1$lokasi)
df1$retrieve <- as.factor(df1$retrieve)
df1 <- df1 %>% mutate_if(is.character, as.numeric)

str(df2)

df2$Date <- as.POSIXct(df2$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
df2$lokasi <- as.factor(df2$lokasi)
df2$pre.nwp.group <- as.factor(df2$pre.nwp.group)
df2$pre.obs.group <- as.factor(df2$pre.obs.group)

levels(df2$pre.obs.group)
summary(df2$pre.obs.group)
df2$pre.obs.group <- factor(df2$pre.obs.group, 
                    levels=c("no rain", "light rain", "moderate rain", "heavy rain", "violent rain"))

levels(df2$pre.nwp.group)
summary(df2$pre.nwp.group)
df2$pre.nwp.group <- factor(df2$pre.nwp.group, 
                            levels=c("no rain", "light rain", "moderate rain", "heavy rain", "violent rain"))



######################
## MISSING VALUE of RAW DATA
######################

any_na(df1)
n_miss(df1)
prop_miss(df1)
df1 %>% is.na() %>% colSums()
miss_var_summary(df1)
miss_var_table(df1)
miss_case_summary(df1)
miss_case_table(df1)
gg_miss_var(df1)
gg_miss_upset(df1)
gg_miss_fct(df1, fct = lokasi)

summary(df1)

######################
## MISSING VALUE of MERGED DATA
######################

any_na(df2)
n_miss(df2)
prop_miss(df2)
df2 %>% is.na() %>% colSums()
miss_var_summary(df2)
miss_var_table(df2)
miss_case_summary(df2)
miss_case_table(df2)
gg_miss_var(df2)
gg_miss_upset(df2)
is.factor(df2$lokasi)
gg_miss_fct(df2, fct = lokasi)

summary(df2)

#########################
## HANDLING MISSING VALUE
#########################

df <- drop_na(df2)

any_na(df)
n_miss(df)
prop_miss(df)
df %>% is.na() %>% colSums()
miss_var_summary(df)
miss_var_table(df)
miss_case_summary(df)
miss_case_table(df)
gg_miss_var(df)
gg_miss_upset(df)
is.factor(df$lokasi)
gg_miss_fct(df, fct = lokasi)

summary(df)

######################
## OUTLIER
######################

str(df)
colnames(df)
summary(df)

df %>%  tidyr::gather("id", "value",c(3:14)) %>% 
  ggplot(., aes( y = value))+geom_boxplot()+facet_wrap(vars(id), nrow = 2, scales = "free_y" )

df %>%  tidyr::gather("id", "value",c(15:26)) %>% 
  ggplot(., aes( y = value))+geom_boxplot()+facet_wrap(vars(id), nrow = 2, scales = "free_y" )

df %>%  tidyr::gather("id", "value",c(27:30,33,34)) %>% 
  ggplot(., aes( y = value))+geom_boxplot()+facet_wrap(vars(id), nrow = 2, scales = "free_y" )

boxplot.stats(df$pre.obs)$out
out <- boxplot.stats(df$pre.obs)$out
out_ind <- which(df$pre.obs %in% c(out))
out_ind

df3 <- df[-out_ind,]


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# b <- remove_outliers(df2, na.rm = F)


ggplot(df, aes(y=pre.obs.group))+
  geom_bar(aes(fill=pre.obs.group), na.rm=TRUE)+
  # stat_smooth(colour="green")+
  facet_wrap(~lokasi)+
  labs(title = "rainfall categories plot",
       subtitle = "start date: 27 Feb 2021, end date: 25 March 2022",
       y = "", x = "")+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title=""))

df$YM <- format(as.Date(df$Date), "%Y-%m")

ggplot(df, aes(x= YM, fill=pre.obs.group))+
  geom_bar(position="dodge")+
  labs(title = "rainfall categories plot by month",
       subtitle = "start date: 27 Feb 2021, end date: 25 March 2022",
       y = "", x = "")+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title=""))+
  geom_text(stat='count', aes(YM, label=..count..), position = position_dodge(width = 1), vjust = -0.2)


library(ggcorrplot)
colnames(df)
model.matrix(~0+., data=df[,-c(2, 31,32,35:37)]) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

# write.table(df, 'G:/My Drive/01. BMKG/MOS/data/data_fix_rainfall.txt', sep=',')

