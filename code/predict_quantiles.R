# Project: WindUpdated
# Author: Min Lwin
# Maintainer: Who to complain to <m.lwin@utexas.edu>

#  Choose Quantile Function Here  ---------------------------------------------

#########     guassian error dist here:
#gauss_err_dist = rnorm(100000, mean = 0, sd = .6)
# gauss_quant = quantile(gauss_err_dist, probs = seq(0, 1, 0.01), na.rm = F, names = F, type = 7)
# gauss_quant = gauss_quant[c(-1,-101)]
# pred_gauss_quant = vector('list',10)

#####     laplace error dist here:
#gauss_err_dist = rdoublex(80000, mu = 0, lambda  = .13)
gauss_err_dist = rlogis(80000,location = 0, scale =.09)
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
