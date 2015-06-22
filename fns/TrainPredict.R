TrainPredict <- function (trainX, trainY, testX, testY,
                         farms = 1:10, 
                         nTreesRF = 150, 
                         nTreesGBM = 1000, 
                         model = 'ensemble', 
                         tuneRF = 'n', 
                         tuneGBM = 'cv',
                         distribution = 'gaussian',
                         train.frac.gbm = 1) {
  
  ptm = proc.time()
  writeLines(paste('nTreesRF = ', nTreesRF,'\n', 'nTreesGBM = ', nTreesGBM, '\n', sep = ''))
  
  #  Main Loop for Train and Predict   ----------------------------------------
  fits.rf = vector('list', 10)
  fits.gbm = vector('list', 10)
  predictions.test = vector('list', 10)
  best.iter = c()
  
  for (i in farms){
    writeLines(paste('Training wind farm #', i, sep = '' ))
    target.train = trainY[[i]]$TARGETVAR
    dataX = trainX[[i]]
    dataX.test = testX[[i]]
    
    if (model == 'ensemble'){
      #  if ENSEMBLE  -----------------------------------------------------------
      myfit.rf = RunRF(dataX, target.train, nTreesRF, tuneRF)      
      myfit.gbm = RunGBM(dataX, target.train, nTreesGBM, train.frac.gbm, tuneGBM)
      best.iter[i]=myfit.gbm$best.iter
      myfit.gbm = myfit.gbm$myfit.gbm
      
      fits.rf[[i]]=myfit.rf
      fits.gbm[[i]]=myfit.gbm
      
      predictions.gbm = predict(myfit.gbm, dataX.test, best.iter[i])
      predictions.rf = predict(myfit.rf, dataX.test)
      predictions.rf = (predictions.rf+predictions.gbm)/2
      predictions.test[[i]]$predictions = predictions.rf
      
      writeLines(paste('Finished training wind farm #', i, '!\n', sep = '' ))
      
    } else if (model == 'rf'){
      # if RANDOM FOREST  -----------------------------------------------------
      myfit.rf = RunRF(dataX, target.train, nTreesRF, tuneRF)
      
      fits.rf[[i]]=myfit.rf
      
      predictions.rf = predict(myfit.rf, dataX.test)
      predictions.test[[i]]$predictions = predictions.rf
      
      writeLines(paste('Finished training wind farm #', i, '!\n', sep = '' )) 
      
    } else if (model == 'gbm'){
      # if GBM  ---------------------------------------------------------------
      myfit.gbm = RunGBM(dataX, target.train, nTreesGBM, train.frac.gbm, distribution, tuneGBM)
      best.iter[i]=myfit.gbm$best.iter
      myfit.gbm = myfit.gbm$myfit.gbm
      
      fits.gbm[[i]]=myfit.gbm
      
      predictions.gbm = predict(myfit.gbm, dataX.test, best.iter[i])
      predictions.test[[i]]$predictions = predictions.gbm
      
      writeLines(paste('Finished training wind farm #', i, '!\n', sep = '' ))
      
    } else if (model == 'svm') {
      #run svm only
    } else {
      writeLines("Select 'model' as either: 'ensemble', 'rf', 'gbm', or 'svm' ...")
      return()
    }
  }
  
  
  #  calculate RMSE  ----------------------------------------------------------
  RMSE.test = c()
  for (i in farms){  
    RMSE.test[i] = sqrt(mean((predictions.test[[i]]$predictions-testY[[i]]$TARGETVAR)^2,na.rm =T))
  }
  print(data.frame(RMSE.test))
  
  
  #  plot predictions vs. Amp100  ---------------------------------------------
#   for (i in 1:10){
#     xtst = dataX.test[[i]]$Amp100
#     ytst = testY[[i]]$TARGETVAR
#     ypred = predictions.test[[i]]$predictions
#     plot(xtst, ytst, main = paste('Predictions for farm', i), ylab = 'Predicted Power',
#          xlab = 'Amp100', col = 'blue', xlim = c(0,20), ylim = c(0,1.05))
#     points(xtst,ypred, col = 'red', cex = .3, pch = 19)
#     legend('topleft', c('target','predicted'), pch=c(1,19),  col=c("blue","red"), cex = .7)
#   }
  
  
  #  Write completion confirmation and return output  -------------------------
  writeLines('\nModel training complete!')
  cat(paste('Time elapsed = ', round((proc.time()-ptm)[3]/60, digits = 2), 'minutes'))
   
  output = list('predictions.test' = predictions.test, 'fits.rf' = fits.rf, 
                 'fits.gbm' = fits.gbm, 'RMSE.test' = RMSE.test)
  return(output)
}

