


#  Plot all in nFarms: Predictions vs. Target  ---------------------------------
range = 1:720
for (i in nFarms){
  plot(testX[[i]]$Amp100,testY[[i]]$TARGETVAR, col ='blue')
  points(testX[[i]]$Amp100,predictions.test[[i]]$predictions, col ='red')

  plot(testY[[i]]$TARGETVAR[range], col ='steelblue', lwd = '2')
  lines(testY[[i]]$TARGETVAR[range], col ='steelblue')
  points(predictions.test[[i]]$predictions, col ='red')
  lines(predictions.test[[i]]$predictions[range], col ='tomato', lwd = '2')
#   points(quantPred10, col = 'green')
  # lines(quantPred10[range], col = 'springgreen', lwd = '3')
}

range = 201:400
for (i in nFarms){
  plot(testX[[i]]$Amp100,testY[[i]]$TARGETVAR, col ='blue')
  points(testX[[i]]$Amp100,predictions.test[[i]]$predictions, col ='red')
  
  plot(testY[[i]]$TARGETVAR[range], col ='steelblue', lwd = '2')
  lines(testY[[i]]$TARGETVAR[range], col ='steelblue')
  points(predictions.test[[i]]$predictions[range], col ='black')
  lines(predictions.test[[i]]$predictions[range], col ='black', lwd = '2')
  lines(all_quant_preds[range,90], col ='tomato', lwd = '3')
  lines(lossVector[range,90], col = 'red', lwd = '1')
  
  #   points(quantPred10, col = 'green')
  lines(all_quant_preds[range,10], col = 'springgreen', lwd = '3')
  lines(lossVector[range,10], col = 'purple', lwd = '1')
}



# Plot predicted quantiles -----------------------------------------------------
for (k in 1:50) {
  plot(all_quant_preds[k,])
}
