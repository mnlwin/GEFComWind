# 
# #>>>>>>>>>>>>>> run different iterations starting from this point <<<<<<<<<<
# 
# #initialize variables
# RMSE_kfolds = vector('list',5) # RMSE for 10 farms for each fold
# predictions.cv = vector('list',10)  # list of predictions for each fold
# errors.cv = vector('list',10)  #list of differences between prediction and target
# presub_errors = vector('list',10) #  ####this is the n x 2 vector of just errors and associated local wind speed
# 
# 
# ptm = proc.time()
# 
# #  generate 5 subsets of the training data - first define training row subsets
# 
# train_rows = vector('list',5)
# for (k in 0:4){
#   train_rows[[k+1]] = (1+k*(nrow(trainX)/5)):((k+1)*(nrow(trainX)/5))
# }
# 
# 
# for (k in 5){             
#   
#   data.train = data.frame(trainX[-train_rows[[k]], ],check.names = T)  #assign training data to new variable
#   data.cv =  data.frame(trainX[train_rows[[k]], ],check.names=T)   #assign test data to new variable
#   
#   
#   print(paste('Number of trees =', nTreesGBM))
#   
#   for (i in 1:10){
#     print(paste('Training fold #', k, 'for wind farm #', i, sep = ' ' ))
#     
#     target.train = data.frame(prevTrain.raw[[i]]$TARGETVAR[-train_rows[[k]]])  #target variables needs to be chosen for each farm in loop
#     names(target.train)[1] = 'TARGETVAR'
#     target.cv =data.frame(prevTrain.raw[[i]]$TARGETVAR[train_rows[[k]]])
#     names(target.cv)[1] = 'TARGETVAR'
#     
#     data.train.local = trainX.local[[i]][-train_rows[[k]],]
#     
#     
#     #     rfcv(data.train, target.train, cv.fold=2, scale="log", step=0.5,
#     #          mtry=function(p) p/3, recursive=FALSE)
#     
#     #MAIN RF CALL   
#     #myfit.rf = randomForest(data.train, target.train$TARGETVAR, ntree=nTreesRF)
#     
#     
#     ##########   use this RF call if tuning is desired  #################
#     # 
#     #     myfit.rf = tuneRF(data.train, target.train,ntreeTry=150, 
#     #                       stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=T) 
#     #     
#     ########################################################################### 
#     
#     
#     #     ##################### try parallel random forest here             ###################
#     #     
#     #     ptm = proc.time()
#     
#     #     registerDoMC(4)
#     #     myfit.rf = foreach(ntree=rep(40, 4), .combine=combine, .packages='randomForest') %dopar% {
#     #       randomForest(data.train, target.train$TARGETVAR, ntree=ntree)
#     #     }
#     
#     #     print(proc.time()-ptm)
#     #     
#     #     ###########################################################################
#     
#     # 
#     #     # fit GBM Model
#     #     myfit.rf <-
#     #       gbm(target.train$TARGETVAR~.,         # formula
#     #           data=data.train,                   # dataset
#     #           var.monotone=NULL,  # -1: monotone decrease,
#     #                                         # +1: monotone increase,
#     #                                         #  0: no monotone restrictions
#     #           distribution="gaussian",      # see the help for other choices
#     #           n.trees=nTreesGBM,                 # number of trees
#     #           shrinkage=0.1,               # shrinkage or learning rate,
#     #                                         # 0.001 to 0.1 usually work
#     #           interaction.depth=3,          # 1: additive model, 2: two-way interactions, etc.
#     #           bag.fraction = 0.5,           # subsampling fraction, 0.5 is probably best
#     #           train.fraction = 0.5,         # fraction of data for training,
#     #                                         # first train.fraction*N used for training
#     #           n.minobsinnode = 10,          # minimum total weight needed in each node
#     #           cv.folds = 3,                 # do 3-fold cross-validation
#     #           keep.data=TRUE,               # keep a copy of the dataset with the object
#     #           verbose=FALSE,                # don't print out progress
#     #           n.cores=2)                    # use only a single core (detecting #cores is
#     #     # error-prone, so avoided here)
#     #     
#     
#     
#     predictions.rf = predict(myfit.rf, data.cv)
#     predictions.cv[[i]] = rbind(predictions.cv[[i]],(cbind(predictions.rf))) #pred in each fold get appended by rows
#     
#     test_errors =  predictions.rf - target.cv
#     errors.cv[[i]] = rbind(errors.cv[[i]],(cbind(test_errors)))  #errors in each fold get appended by rows
#     
#     Amp100 = data.train.local$Amp100
#     presub_errors[[i]] = rbind(presub_errors[[i]],cbind(Amp100,test_errors))
#     
#     RMSE.rf=sqrt(mean((predictions.rf-target.cv)^2))
#     RMSE_kfolds[[k]] = rbind(RMSE_kfolds[[k]],(cbind(RMSE.rf)))
#     
#     print(paste('Cross Validation fold #', k, 'finished for wind farm #', i, sep = ' ' ))
#     print(paste('RMSE for CV fold #', k, '=', as.table(RMSE_kfolds[[k]]), sep = ' ' ))
#   }
# }
# print((proc.time()-ptm)/60)
# 
# 
# #### Calculate average of RMSE across k-fold cross validation results
# RMSE_avg5folds = c()
# for(i in 1:10) {# 10 RMSE values, one for each wind farm 
#   RMSE_avg5folds = rbind(RMSE_avg5folds,mean(c(RMSE_kfolds[[1]][i],RMSE_kfolds[[2]][i],RMSE_kfolds[[3]][i],
#                                                RMSE_kfolds[[4]][i],RMSE_kfolds[[5]][i])))
# }
# View(RMSE_avg5folds)
# 
# 
# #### Calculate 1% quantiles for error distributions in each farm
# #### Plot histogram of errors and empirical quantiles for each wind farm
# for (i in 1:10){
#   par(mfrow=c(1,2))
#   a = quantile(errors.cv[[i]][,1], probs = seq(0, 1, 0.01), na.rm = F,names = F, type = 7)
#   b =errors.cv[[i]][,1]
#   
#   
#   hist(b,breaks = 500,xlab=paste("Predicted values - observed values"), ylab="Frequency",
#        main =paste('Histogram of errors at Farm',as.character(i), sep=" "))
#   plot(a,xlab=paste("1% quantiles"), ylab="Power Output",
#        main =paste('1% quantiles for Farm',as.character(i), sep=" "))
# }
# par(mfrow=c())
# 
# 
# 
