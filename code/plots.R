


#  Plot all in nFarms: Predictions vs. Target  ---------------------------------
for (i in nFarms){
  plot(testX[[i]]$Amp100,testY[[i]]$TARGETVAR, col ='blue')
  points(testX[[i]]$Amp100,predictions.test[[i]]$predictions, col ='red')
  
  plot(testY[[i]]$TARGETVAR, col ='blue')
  lines(testY[[i]]$TARGETVAR, col ='blue')
  points(predictions.test[[i]]$predictions, col ='red')
  lines(predictions.test[[i]]$predictions, col ='red')
}

# Plot predicted quantiles -----------------------------------------------------
for (k in 1:50) {
  plot(all_quant_preds[k,])
}
