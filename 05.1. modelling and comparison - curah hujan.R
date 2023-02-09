# PCA
# https://www.datacamp.com/tutorial/pca-analysis-r
# https://towardsdatascience.com/learn-principle-component-analysis-in-r-ddba7c9b1064
# https://www.r-bloggers.com/2018/08/pca-revisited-using-principal-components-for-classification-of-faces/
# https://rpubs.com/tomytjandra/PCA-before-classification

# Caret
# https://towardsdatascience.com/how-to-build-a-complete-classification-model-in-r-and-caret-73c68c3392e1
# https://towardsdatascience.com/a-guide-to-using-caret-in-r-71dec0bda208
# https://rpubs.com/Isaac/caret_reg
# https://machinelearningmastery.com/compare-models-and-select-the-best-using-the-caret-r-package/
# 
# Imbalance data
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/# 
# https://dpmartin42.github.io/posts/r/imbalanced-classes-part-1
# https://towardsdatascience.com/multi-class-imbalance-154ef2b15816
# https://cran.r-project.org/web/packages/scutr/scutr.pdf
# https://cran.r-project.org/web/packages/scutr/readme/README.html

##############################################################################################
##                      DATA PREPARATION AND MODELLING                                      ## 
##############################################################################################

########################
## reference and library
########################


{
  library(parallel)
  library(caret)
  library(AppliedPredictiveModeling)
  library(data.table)
  library(Hmisc)
  library(elasticnet)
  library(lars)
  library(pls)
  library(kernlab)
  library(dplyr)
  library(MLmetrics)
  library(party)
  library(ROSE) # imbalance data
  library(scutr) # imbalanced data
  library(ROSE)
  library(smotefamily)
}

# ncore <- detectCores()-1

df <- read.table("G:/My Drive/01. BMKG/MOS/data/data_fix_rainfall.txt", sep=',')
ls()
str(df)

nrow(df)
str(df)
df$Date <- as.POSIXct(df$Date)
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                       as.factor)
colnames(df)

loc <- levels(df$lokasi)



# scutted <- SCUT(df1, "pre.obs.group")
# table(scutted$pre.obs.group)
# 
# ret <- SCUT(df1, "pre.obs.group", undersample = undersample_hclust,
#             usamp_opts = list(dist_calc="manhattan"))
# 
# table(ret$pre.obs.group)






# PCA
# select only for numerical feature

# colnames(df1.rose)
# summary(df1.rose)
# df.pca <- prcomp(df1.rose[,-30], center = TRUE, scale. = TRUE)
# df.pca
# 
# varexplained <- cumsum(df.pca$sdev^2) / sum(df.pca$sdev^2) * 100
# # min.90 <- 1:which(varexplained >= 90)[1]
# 
# plot(df.pca$sdev, type='l')
# plot(df.pca$sdev, type='o')
# plot(varexplained, type='o')
# abline(h=90)
# 
# data <- df.pca$x[, 1:9]
# eigen.pca <- df.pca$rotation
# eigen.pca[, -c(1:9)] <- 0
# 
# sf <- list(center = df.pca$center, scale = df.pca$scale)


# # bikin model
# # lm(y ~ data.latih)
# 
# data.baru <- df[1:10]
# 
# data.input <- scale(data.baru, center = sf$center, scale = sf$scale)
# data.input <- data.input %*% eigen.pca
# 
# data.input[]









eval.table <- list()
prediction.table <- list()
for (i in 1:length(loc)) {
    # imbalanced data
    table(df$pre.obs.group)
    
    df1 <- df %>%
      filter(lokasi == loc[i]) 
    
    rownames(df1) <- df1$Date
    
    table(df1$pre.obs.group)
    colnames(df1)
    df1 <- subset(df1, select=-c(Date, lokasi, pre.obs, hari, UTC, YM,pre.nwp.group, clmix.kg.kg. ))
    y <- which( colnames(df1)=="pre.obs.group" )
    df1$pre.obs.group <- ifelse(df1$pre.obs.group=="no rain","no rain", "rain")
    table(df1$pre.obs.group)
    # df1$pre.obs.group <- ifelse(df1$pre.obs.group==4, 0, 1)
    
    df1.rose <- ROSE(pre.obs.group ~ ., data = df1, seed = 1)$data
    table(df1.rose$pre.obs.group)

    df1.rose$pre.nwp.group <- ifelse(df1.rose$pre.nwp< 0.3*0.75, "no rain", "rain")
    table(df1.rose$pre.nwp.group)

    # df1.scutted <- SCUT(df1, "pre.obs.group")
    # table(df1.scutted$pre.obs.group)
    # df1.scutted$pre.nwp.group <- ifelse(df1.rose$pre.nwp< 0.3*0.75, "no rain", "rain") 
    # table(df1.scutted$pre.nwp.group)
    
    
    # cross validation
    controlObject <- trainControl(method = "repeatedcv"
                                  , repeats = 5
                                  , number = 10
                                  # , summaryFunction = multiClassSummary
                                  # , classProbs = TRUE
                                  )
    
    # creates model objects
    # Splitting dataset
    # df1 <- df1.scutted
    df1 <- df1.rose
    str(df1)
    df1[sapply(df1, is.character)] <- lapply(df1[sapply(df1, is.character)], 
                                           as.factor)
    # train_rows <- sample(1:(nrow(df1)*0.75))
    # 
    # trainingData = df1[train_rows, ]
    # testData = df1[-train_rows, ]
    # testData <- testData[1:25,]
    
    set.seed(3456)
    trainIndex <- createDataPartition(df1$pre.obs.group, p = .9,
                                      list = FALSE,
                                      times = 1)
    trainingData <- df1[ trainIndex,]
    testData <- df1[-trainIndex,]
  
    
    ######################################################## Baseline 
    set.seed(669); ptm <- proc.time()
    
    cm.baseline =   confusionMatrix(testData$pre.nwp.group,
                    reference = testData$pre.obs.group,
                    positive = "rain",
                    mode = "prec_recall")
    
    cm.baseline
    acc.baseline = cm.baseline$overall[1]
    acc.baseline
    precision.baseline <- cm.baseline$byClass[5]
    recall.baseline <- cm.baseline$byClass[6]
    f1.baseline <- cm.baseline$byClass[7]
    
    tm.baseline = proc.time() - ptm
    tm.baseline
    
    ######################################################## logistic regression 
    set.seed(669); ptm <- proc.time()
    glm <- train(pre.obs.group ~  . , data = trainingData, method = "glm", trControl = controlObject) 
    pred.glm = predict(glm, testData[,-y])
    
    cm.glm =   confusionMatrix(pred.glm,
                                    reference = testData$pre.obs.group,
                                    positive = "rain",
                                    mode = "prec_recall")
    
    cm.glm
    acc.glm = cm.glm$overall[1]
    precision.glm <- cm.glm$byClass[5]
    recall.glm <- cm.glm$byClass[6]
    f1.glm <- cm.glm$byClass[7]
    
    tm.glm <- proc.time() - ptm
    tm.glm
    
    ######################################################## random forest 
    # set.seed(669); ptm <- proc.time()
    # rf <- train(pre.obs.group ~  . , data = trainingData, method = "rf", trControl = controlObject) 
    # pred.rf = predict(rf, testData[,-y])
    # 
    # cm.rf =   confusionMatrix(pred.rf,
    #                                 reference = testData$pre.obs.group,
    #                                 positive = "rain",
    #                                 mode = "prec_recall")
    # 
    # cm.rf
    # acc.rf = cm.rf$overall[1]
    # precision.rf <- cm.rf$byClass[5]
    # recall.rf <- cm.rf$byClass[6]
    # f1.rf <- cm.rf$byClass[7]
    # tm.rf = proc.time() - ptm
    # tm.rf

    ######################################################## Bagged Tree
    set.seed(669); ptm <- proc.time()
    treebag <- train(pre.obs.group ~  . , data = trainingData, method = "treebag", trControl = controlObject) 
    pred.treebag = predict(treebag, testData[,-y])
    
    cm.treebag =   confusionMatrix(pred.treebag,
                              reference = testData$pre.obs.group,
                              positive = "rain",
                              mode = "prec_recall")
    
    cm.treebag
    acc.treebag = cm.treebag$overall[1]
    precision.treebag <- cm.treebag$byClass[5]
    recall.treebag <- cm.treebag$byClass[6]
    f1.treebag <- cm.treebag$byClass[7]
    tm.treebag = proc.time() - ptm
    tm.treebag
    
    ####################################################### Cond Inf Tree
    set.seed(669); ptm <- proc.time()
    ctree <- train(pre.obs.group ~  . , data = trainingData, method = "ctree", tuneLength = 30, trControl = controlObject) 
    pred.ctree = predict(ctree, testData[,-y])
    
    cm.ctree =   confusionMatrix(pred.ctree,
                                   reference = testData$pre.obs.group,
                                   positive = "rain",
                                   mode = "prec_recall")
    
    cm.ctree
    acc.ctree = cm.ctree$overall[1]
    precision.ctree <- cm.ctree$byClass[5]
    recall.ctree <- cm.ctree$byClass[6]
    f1.ctree <- cm.ctree$byClass[7]
    tm.ctree = proc.time() - ptm
    tm.ctree
    
    ######################################################## CART
    set.seed(669); ptm <- proc.time()
    rpart <- train(pre.obs.group ~  . , data = trainingData, method = "rpart", tuneLength = 30, trControl = controlObject) 
    pred.rpart = predict(rpart, testData[,-y])
    
    cm.rpart =   confusionMatrix(pred.rpart,
                                 reference = testData$pre.obs.group,
                                 positive = "rain",
                                 mode = "prec_recall")
    
    cm.rpart
    acc.rpart = cm.rpart$overall[1]
    precision.rpart <- cm.rpart$byClass[5]
    recall.rpart <- cm.rpart$byClass[6]
    f1.rpart <- cm.rpart$byClass[7]
    tm.rpart = proc.time() - ptm
    tm.rpart
    
    ##########################
    ## measuring performance
    ##########################
    
    acc <- c(acc.baseline
             , acc.glm
             # , acc.rf
             , acc.treebag
             , acc.ctree
             , acc.rpart)


    precision <- c(precision.baseline
             , precision.glm
             # , precision.rf
             , precision.treebag
             , precision.ctree
             , precision.rpart)
    
    recall <- c(recall.baseline
             , recall.glm
             # , recall.rf
             , recall.treebag
             , recall.ctree
             , recall.rpart)
    
    f1 <- c(f1.baseline
             , f1.glm
             # , f1.rf
             , f1.treebag
             , f1.ctree
             , f1.rpart)
    
    tm <- c(tm.baseline
             , tm.glm
             # , tm.rf
             , tm.treebag
             , tm.ctree
             , tm.rpart)
    model <- c('baseline', 'logistics', 'bagged tree', 'conf tree', 'decision tree')
    
    eval <- data.frame(loc[i], acc, precision, recall, f1, tm, model)
    eval.table[[i]] <- eval

    prediction <- cbind( rownames(testData), testData[,y]
                         , pred.glm
                         # , pred.rf
                         , pred.treebag
                         , pred.ctree
                         , pred.rpart
                         , loc[i]
                         , 'curah hujan')
    prediction.table[[i]] <- prediction
    
    # save model
    list.model <- list(glm
                       ,treebag
                       ,ctree
                       ,rpart)
    model <- c('logistics', 'bagged tree', 'conf tree', 'decision tree')
    for (j in 1:length(list.model)) {
      saveRDS(list.model[j],  
              paste("G:/My Drive/01. BMKG/MOS/data/model/curah hujan_",model[j],"_",loc[i],".rds",sep=""))
    }
    # my_model <- readRDS("model.rds")
    
    }


pred.table <- data.frame(do.call(rbind, prediction.table))
colnames(pred.table) <- c('date','baseline', 'logistics', 'bagged tree', 'conf tree', 'decision tree', 'location', 'parameter')
pred <- melt(pred.table, id=c('date','location', 'parameter'))
colnames(pred) <- c('date','location', 'parameter', 'model', 'prediction')

model_evaluation <- do.call(rbind, eval.table)
eval <- melt(model_evaluation, id=c('loc.i.', 'model'))
eval[,5] <- 'curah hujan'
colnames(eval) <- c('Location', 'Model', 'Assessment', 'Value', 'Parameter')

best_fit_model <- eval %>%
  select(Location, Model, Assessment, Value, Parameter) %>%
  group_by(Location, Assessment, Parameter) %>%
  filter(case_when(Assessment=="tm" ~ Value==min(Value), #When
                   T ~ Value==max(Value)) #Otherwise
  ) %>%
  arrange(Location)


write.csv(eval, "G:/My Drive/01. BMKG/MOS/data/curah hujan model assessment.csv", row.names=F)
write.csv(pred, "G:/My Drive/01. BMKG/MOS/data/curah hujan model prediction.csv", row.names=F)
write.csv(best_fit_model, "G:/My Drive/01. BMKG/MOS/data/curah hujan best fit model.csv", row.names=F)

