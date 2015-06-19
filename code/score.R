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
      
      if (is.na(y)){y=0}
      
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

