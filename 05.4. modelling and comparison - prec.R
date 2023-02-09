##############################################################################################
##                      DATA PREPARATION AND MODELLING                                         ## 
##############################################################################################

########################
## reference and library
########################

# https://rpubs.com/Isaac/caret_reg
# https://machinelearningmastery.com/compare-models-and-select-the-best-using-the-caret-r-package/

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
}

# ncore <- detectCores()-1

# df <- fread("G:/My Drive/01. BMKG/MOS/data/data_fix.txt")
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

eval.table <- list()
prediction.table <- list()
for (i in 1:length(loc)) {
  
    df1 <- df %>%
      filter(lokasi == loc[i]) 
    rownames(df1) <- df1$Date

    df1 <- df1 %>%
      select_if( is.numeric)
    
    
    colnames(df1)
    df1 <- subset(df1, select=-c(clmix.kg.kg., hari, UTC))
    y <- which( colnames(df1)=="pre.obs" )
    describe(df1)
    summary(df1)
    
    # CORRELATION
    # scatter plot
    featurePlot(x = df1[, !names(df1) %in% c("pre.obs")], y = df1[, names(df1) %in% c("pre.obs")],
                between = list(x = 1, y = 1),
                type = c("g", "smooth")) 
    
    
    
    # pca
    # df.pca <- prcomp(df1[,-28], center = TRUE, scale. = TRUE)
    # df.pca
    # 
    # varexplained <- cumsum(df.pca$sdev^2) / sum(df.pca$sdev^2) * 100
    # min.90 <- 1:which(varexplained >= 90)[1]
    # y <- max(min.90)+1
    # 
    # plot(df.pca$sdev, type='l')
    # plot(df.pca$sdev, type='o')
    # plot(varexplained, type='o')
    # 
    # df2 <- as.data.frame(df.pca$x[, min.90])
    # eigen.pca <- df.pca$rotation
    # eigen.pca[, -c(min.90)] <- 0
    # 
    # sf <- list(center = df.pca$center, scale = df.pca$scale)
    # 
    # df2$pre.obs <- df1[,28]
    # colnames(df2)
    # df1<- df2
  
    # cross validation
    controlObject <- trainControl(method = "repeatedcv", repeats = 5, number = 10)
    
    # creates model objects
    # Splitting dataset
    train_rows <- sample(1:(nrow(df1)*0.9))
    
    trainingData = df1[train_rows, ]
    testData = df1[-train_rows, ]
    testData <- testData[1:25,]
    
    ######################################################## Baseline 
    # set.seed(669); ptm <- proc.time()
    # 
    # Rsquare.baseline = R2_Score( testData[,28],testData[,29])
    # Rsquare.baseline
    # 
    # rmse.baseline <- RMSE(testData[,29], testData[,28])
    # rmse.baseline
    # mse.baseline <- MSE(testData[,29], testData[,28])
    # mse.baseline
    # tm.baseline = proc.time() - ptm
    # tm.baseline
    
    
    
    ######################################################## linear regression 
    set.seed(669); ptm <- proc.time()
    linearReg <- train(pre.obs ~  . , data = trainingData, method = "lm", trControl = controlObject) 
    pred.lm = predict(linearReg, testData[,-y])
    
    Rsquare.lm = R2_Score(pred.lm, testData[,y])
    Rsquare.lm
    rmse.lm <- RMSE(pred.lm, testData[,y])
    rmse.lm
    mse.lm <- MSE(pred.lm , testData[,y])
    mse.lm
    tm.lm <- proc.time() - ptm
    tm.lm
    
    ######################################################## random forest 
    # set.seed(669); ptm <- proc.time()
    # rfModel <- train(pre.obs ~  . , data = trainingData, method = "rf", trControl = controlObject) 
    # pred.rf = predict(rfModel, testData[,-29])
    # 
    # Rsquare.rf = R2_Score(pred.rf, testData[,29])
    # RMSE(pred.rf, testData[,29])
    # MSE(pred.rf , testData[,29])
    # tm = proc.time() - ptm
    # tm
    
    ######################################################## Elastic Net
    set.seed(669); ptm <- proc.time()
    enetGrid <- expand.grid(.lambda = c(0, .001, .01, .1), .fraction = seq(0.05, 1, length = 20))
    enetModel <- train(pre.obs ~ . , data = trainingData , method = "enet", preProc = c("center", "scale"), tuneGrid = enetGrid, trControl = controlObject)
    pred.enet = predict(enetModel, testData[,-y])
    
    Rsquare.enet = R2_Score(pred.enet, testData[,y])
    Rsquare.enet
    rmse.enet <- RMSE(pred.enet, testData[,y])
    rmse.enet
    mse.enet <- MSE(pred.enet, testData[,y])
    mse.enet
    tm.enet = proc.time() - ptm
    tm.enet
    
    ######################################################## Partial Least Squares
    set.seed(669); ptm <- proc.time()
    plsModel <- train(pre.obs ~ . , data = trainingData , method = "pls", preProc = c("center", "scale"), tuneLength = 15, trControl = controlObject)
    pred.pls = predict(plsModel, testData[,-y])
    
    Rsquare.pls = R2_Score(pred.pls, testData[,y])
    Rsquare.pls
    rmse.pls <- RMSE(pred.pls, testData[,y])
    rmse.pls
    mse.pls <- MSE(pred.pls, testData[,y])
    mse.pls
    tm.pls = proc.time() - ptm
    tm.pls
    
    ######################################################## Support Vector Machines 
    # set.seed(669); ptm <- proc.time()
    # svmRModel <- train(pre.obs ~ . , data = trainingData, method = "svmRadial",
    #                    tuneLength = 15, preProc = c("center", "scale"),  trControl = controlObject)
    # 
    # pred.svm = predict(svmRModel, testData[,-y])
    # 
    # Rsquare.svm = R2_Score(pred.svm, testData[,y])
    # Rsquare.svm
    # RMSE(pred.svm, testData[,y])
    # MSE(pred.svm, testData[,y])
    # tm = proc.time() - ptm
    # tm
    
    ######################################################## Bagged Tree
    set.seed(669); ptm <- proc.time()
    treebagModel <- train(pre.obs ~ . , data = trainingData, method = "treebag", trControl = controlObject)
    
    pred.tree = predict(treebagModel, testData[,-y])
    
    Rsquare.tree = R2_Score(pred.tree, testData[,y])
    Rsquare.tree
    rmse.tree <- RMSE(pred.tree, testData[,y])
    rmse.tree
    mse.tree <- MSE(pred.tree, testData[,y])
    tm.tree = proc.time() - ptm
    tm.tree
    
    ####################################################### Cond Inf Tree
    set.seed(669); ptm <- proc.time()
    ctreeModel <- train(pre.obs ~ . , data = trainingData , method = "ctree", tuneLength = 10, trControl = controlObject)
    
    pred.ctree = predict(ctreeModel, testData[,-y])
    Rsquare.ctree = R2_Score(pred.ctree, testData[,y])
    Rsquare.ctree
    rmse.ctree <- RMSE(pred.ctree, testData[,y])
    rmse.ctree
    mse.ctree <- MSE(pred.ctree, testData[,y])
    mse.ctree
    tm.ctree = proc.time() - ptm
    tm.ctree
    
    ######################################################## CART
    set.seed(669); ptm <- proc.time()
    rpartModel <- train(pre.obs ~ . , data = trainingData , method = "rpart", tuneLength = 30, trControl = controlObject)
    pred.rpart = predict(rpartModel, testData[,-y])
    
    Rsquare.rpart = R2_Score(pred.rpart, testData[,y])
    Rsquare.rpart
    rmse.rpart <- RMSE(pred.rpart, testData[,y])
    rmse.rpart
    mse.rpart <- MSE(pred.rpart, testData[,y])
    mse.rpart
    tm.rpart = proc.time() - ptm
    tm.rpart
    
    ##########################
    ## measuring performance
    ##########################
    
    Rsquare <- c(Rsquare.baseline, Rsquare.lm, Rsquare.enet, Rsquare.pls, Rsquare.tree, Rsquare.ctree, Rsquare.rpart)
    rmse <- c(rmse.baseline, rmse.lm, rmse.enet, rmse.pls, rmse.tree, rmse.ctree, rmse.rpart)
    mse <- c(mse.baseline, mse.lm, mse.enet, mse.pls, mse.tree, mse.ctree, mse.rpart)
    tm <- c(tm.baseline[[2]], tm.lm[[2]], tm.pls[[2]], tm.enet[[2]], tm.tree[[2]], tm.ctree[[2]], tm.rpart[[2]])
    model <- c('baseline', 'linear', 'elastic', 'PLS', 'bagged tree', 'conf tree', 'decision tree')
    
    eval <- data.frame(loc[i], Rsquare, rmse, mse, tm, model)
    eval.table[[i]] <- eval

    prediction <- cbind(rownames(testData), testData[,y], pred.lm, pred.enet,  pred.pls, pred.tree, pred.ctree, pred.rpart, loc[i], 'prec obs')
    prediction.table[[i]] <- prediction
    
    # save model
    list.model <- list(linearReg
                       , enetModel
                       , plsModel
                       ,treebagModel
                       ,ctreeModel
                       ,rpartModel)
    model <- c('linear', 'elastic', 'PLS', 'bagged tree', 'conf tree', 'decision tree')
    for (j in 1:length(list.model)) {
      saveRDS(list.model[j],  
              paste("G:/My Drive/01. BMKG/MOS/data/model/prec_",model[j],"_",loc[i],".rds",sep=""))
    }
    # my_model <- readRDS("model.rds")
    }

pred.table <- data.frame(do.call(rbind, prediction.table))
colnames(pred.table) <- c('date','baseline', 'linear', 'elastic', 'PLS', 'bagged tree', 'conf tree', 'decision tree', 'location', 'parameter')
pred <- melt(pred.table, id=c('date','location', 'parameter'))
colnames(pred) <- c('date','location', 'parameter', 'model', 'prediction')


model_evaluation <- do.call(rbind, eval.table)
eval <- melt(model_evaluation, id=c('loc.i.', 'model'))
eval[,5] <- 'prec obs'
colnames(eval) <- c('Location', 'Model', 'Assessment', 'Value', 'Parameter')

best_fit_model <- eval %>% 
  select(Location, Model, Assessment, Value, Parameter) %>% 
  group_by(Location, Assessment, Parameter) %>% 
  filter(case_when(Assessment=="Rsquare" ~ Value==max(Value), #When 
                   T ~ Value==min(Value)) #Otherwise
  ) %>%
  arrange(Location)

write.csv(eval, "G:/My Drive/01. BMKG/MOS/data/prec obs model assessment.csv", row.names=F)
write.csv(pred, "G:/My Drive/01. BMKG/MOS/data/prec obs model prediction.csv", row.names=F)
write.csv(best_fit_model, "G:/My Drive/01. BMKG/MOS/data/precipitation best fit model.csv", row.names=F)

# allResamples <- resamples(list("Linear Reg" = linearReg, 
#                                # "SVM" = svmRModel , 
#                                # "PLS" = plsModel ,  
#                                "Elastic Net" = enetModel ,
#                                "Bagged Tree" = treebagModel , 
#                                "Cond Inf Tree" = ctreeModel ,  
#                                "CART" = rpartModel 
# ))
# parallelplot(allResamples)
# 
# #####################
# #####################
# 
# 
# parallelplot(allResamples , metric = "Rsquared")
# 
# perf.grid[order(perf.grid$RMSE.test, decreasing=F),]
# 
# predicted.SVM.trans = predict(svmRModel.trans , solTestXtrans) 
# residualValues.SVM <- solTestY - predicted.SVM.trans
# summary(residualValues.SVM)
# 
# sd(residualValues.SVM)
# 
# # Observed values versus predicted values
# axisRange <- extendrange(c(solTestY, predicted.SVM.trans))
# plot(solTestY, predicted.SVM.trans, ylim = axisRange, xlim = axisRange)
# abline(0, 1, col = "darkgrey", lty = 2)
# 
# # Predicted values versus residuals
# plot(predicted.SVM.trans, residualValues.SVM, ylab = "residual")
# abline(h = 0, col = "darkgrey", lty = 2)
# 
# predicted.lin_reg.trans = predict(linearReg.trans, solTestXtrans) 
# residualValues.reg_lin <- solTestY - predicted.lin_reg.trans
# summary(residualValues.reg_lin)
# 
# sd(residualValues.reg_lin)
# 
# # Observed values versus predicted values
# axisRange <- extendrange(c(solTestY, predicted.lin_reg.trans))
# plot(solTestY, predicted.lin_reg.trans, ylim = axisRange, xlim = axisRange)
# abline(0, 1, col = "darkgrey", lty = 2)
# 
# # Predicted values versus residuals
# plot(predicted.lin_reg.trans, residualValues.reg_lin, ylab = "residual")
# abline(h = 0, col = "darkgrey", lty = 2)
# 
# ##############
# ## Accuracy vs scalability
# ##############
# 
# perf.grid[order(perf.grid$time, decreasing=F),]
# 
# 
# 
# 
# 
# 
