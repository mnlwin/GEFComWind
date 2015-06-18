# The actual work

#  Train Models and Predict on Test  ------------------------------------------
model.train = TrainPredict(trainX, trainY, testX, testY,
                           farms = 10,                           
                           nTreesRF = 150, 
                           nTreesGBM = 3000, 
                           model = 'rf', 
                           tuneRF = 'n', 
                           tuneGBM = 'test',
                           train.frac.gbm = .8)
predictions.test = model.train$predictions.test
RMSE.test = model.train$RMSE.test
rm(model.train)

#  Choose Quantile Function Here  ---------------------------------------------

#########     guassian error dist here:
#gauss_err_dist = rnorm(100000, mean = 0, sd = .6)
# gauss_quant = quantile(gauss_err_dist, probs = seq(0, 1, 0.01), na.rm = F, names = F, type = 7)
# gauss_quant = gauss_quant[c(-1,-101)]
# pred_gauss_quant = vector('list',10)

#####     laplace error dist here:
#gauss_err_dist = rdoublex(80000, mu = 0, lambda  = .13)
gauss_err_dist = rlogis(80000,location = -1, scale =.09)
hist(gauss_err_dist, breaks = 1000)
gauss_quant = quantile(gauss_err_dist, probs = seq(0, 1, 0.01), na.rm = F, names = F, type = 7)
gauss_quant = gauss_quant[c(-1,-101)]
pred_gauss_quant = vector('list',10)

for (i in 1:10){
  for (j in 1:length(predictions.test[[i]]$predictions)){
    temp = gauss_quant + predictions.test[[i]]$predictions[j]
    pred_gauss_quant[[i]] = rbind(pred_gauss_quant[[i]],t(temp))
  }
}

#  Set lower and upper limits to 0 and 1  -------------------------------------
for (i in 1:10){  #for each wind farm
  for (j in 1:length(pred_gauss_quant[[i]][,1]) ) {  #for each row of the solutions
    
    for (k in 1:99){ #check each cell, quantile value.  if neg, set = 0, if <1, set =1
      
      if (pred_gauss_quant[[i]][j,k] < 0){
        pred_gauss_quant[[i]][j,k] = 0
      }
      else if (pred_gauss_quant[[i]][j,k] > 1){
        pred_gauss_quant[[i]][j,k] = 1
      }
    }
  }
}

#  Combine quantile predictions in one file  ----------------------------------
all_quant_preds=c()

for (i in 1:10){
  all_quant_preds = rbind(cbind(all_quant_preds),cbind(pred_gauss_quant[[i]]))
}
#write.csv(all_quant_preds,file=paste("quantile_est.combined_forKWM.csv",sep=""), row.names=FALSE, col.names=FALSE)

#  Calculate Pinball Loss  ----------------------------------------------------

# y = target variable 
# q = my quantile value, ie, pred_gauss_quant[[1]][i,j]
# a = target quantile number
# L = loss

loss_total = c()

for (i in 1:10){                                #for each wind farm
  loss_farm = 0
  L=0
  for (j in 1:length(pred_gauss_quant[[i]][,1])){      #for each row of predictions
    for (a in 1:99){                            #for each quantile
      
      q = pred_gauss_quant[[i]][j,a]                 #target quantile value is equal to quantile prediction value
      y = testY[[i]]$TARGETVAR[j]            #observed value = y
      
      if  (y<q){                                  #if observed is less than predicted, 
        L = (1-a/100)*(q-y)                     #calculate loss for cell
        loss_farm = loss_farm + L             #add this cell's loss to total loss
      }
      
      else if (y>=q){                             #if observed is greater/equal to predicted
        L = (a/100)*(y-q)                       #calculate loss for cell
        loss_farm = loss_farm + L             #add to total loss
      }
    }
  }
  
  print(paste('avg. pinball loss of farm', i, '=', loss_farm/(99*length(pred_gauss_quant[[i]][,1]))))
  loss_total[i] = loss_farm
}

print(paste('expexcted pinball loss score =', as.character(sum(loss_total)/(10*length(pred_gauss_quant[[i]][,1])*99))), sep = '')

View(loss_total/(99*length(pred_gauss_quant[[i]][,1])))

