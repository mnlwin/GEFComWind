cat("\014")
rm(list=ls())

#  Load Packages  -------------------------------------------------------------

require(gbm)
require(parcor)
require(rgl)
require(gtools)
require(e1071)
require(randomForest)
require(dlm)
require(foreach)
require(doMC)
require(smoothmest)
require(ggplot2)
require(DataCombine)

#  System Parameters  ---------------------------------------------------------

task = 7                    # Task Week
prevTrain.length = 10200   # number of rows in the training data for last week

#  Change the Directory and Source --------------------------------------------

if (.Platform$OS.type == 'windows'){
  prefix = "C:"
} else {
  prefix =  ""
}

working.dir = paste(prefix, "/Users/Min/Google Drive/Research/Projects/GEFCom2014/Wind/Week", task, "/Week", task, "_data", sep = '')
fn.dir = paste(prefix, '/Users/Min/Google Drive/Research/Projects/GEFCom2014/Wind/fns', sep = '')

setwd(fn.dir)
for (f in list.files(pattern = "*.R")) {
  source(f)
}

setwd(working.dir)
rm(prefix, working.dir, fn.dir, f)

#  Load Raw Data  ------------------------------------------------------------- 

rawData = LoadRaw(task)
  train.raw = rawData$train.raw
  test.raw = rawData$test.raw
  prevTrain.raw = rawData$prevTrain.raw
  prevScoring.raw = rawData$prevScoring.raw
rm(rawData)

#  Section 1 - Feature Engineering  -------------------------------------------

input.opt = 1 
input.data = FeatureEng2(prevTrain.raw, prevScoring.raw, input.opt)           #THIS IS THE STRUCTURE OF TRAINING DATA
  trainX = input.data$trainX             # data frame of all Training x's
  testX = input.data$testX                # data frame of all Test/Scoring x's
  trainX.local = input.data$TrainX.local       # local x's 
  testX.local = input.data$TestX.local
  trainY = input.data$TrainY
  testY = input.data$TestY
rm(input.data, input.opt)

#remove outliers
rows.outliers = OutlierRows(trainY, trainX.local)
for (i in 1:10){
trainX.local[[i]] = trainX.local[[i]][-rows.outliers[[i]],]
trainY[[i]]$TARGETVAR = trainY[[i]]$TARGETVAR[-rows.outliers[[i]]]
}

#  plot and view smoothed data
plot(prevScoring.raw[[1]]$U100)
lines(testX.local[[1]]$U100, col='red')

plot(prevTrain.raw[[1]]$U100)
lines(trainX.local[[1]]$U100, col='red')

#  distribution of predictor variables

PredictorDist(testX.local, trainX.local)

#  Section 1.2 - Detect Outliers  ---------------------------------------------
par(mfrow = c(1,2))
for (i in 1:10){
  x = trainX.local[[i]]$Amp100
  y = trainY[[i]]$TARGETVAR
  up.scl = c(1, 1, 1, 1, 1, 1, .5, .6, .9, 1)
  up.shft = c(4, 4, 4, 4, 4, 4, 5, 4.5, 4.5, 3)
  low.scl = c(.8, .8, .6, 1, 1, .8, .5, .5, .8, .7)
  low.shft = c(10, 10, 10, 10, 9, 9, 11, 11.5, 10.5, 9)
  upperlim = 1/(1+exp(up.scl[i]*(-x+up.shft[i]))) + .05
  lowerlim = 1/(1+exp(low.scl[i]*(-x+low.shft[i]))) -.2
    
  plot(x, y, main = paste('Outlier Zones for Farm', i), ylab = 'Measured Power',
       xlab = 'Amp100', xlim = c(0,20), ylim = c(0,1.05))  
  points(x, upperlim, col = 'red', pch = 19, cex = .1)
  points(x, lowerlim, col = 'red', pch =19, cex =.1)
  points(x[which(y>upperlim)],y[which(y>upperlim)], col = 'blue', pch=19)
  points(x[which(y<lowerlim)],y[which(y<lowerlim)], col = 'blue', pch=19)
}

#  Section 2 - Empirical Error Quantiles  -------------------------------------

#  Section 3 - Train Models and Generate Predictions  -------------------------
      
model.train = TrainPredict(trainX, trainY, testX, testY,
                           farms = 1:10,                           
                           nTreesRF = 150, 
                           nTreesGBM = 1000, 
                           model = 'ensemble', 
                           tuneRF = 'n', 
                           tuneGBM = 'test',
                           train.frac.gbm = .8)
predictions.test = model.train$predictions.test
RMSE.test = model.train$RMSE.test
rm(model.train)


######################################################
######################################################
######################################################
######################################################
####################################
######  CREATE QUANTILE PREDICTIONS ##############
# 
# Point estimates were calculated in the previous step.  In this step we will bin the predictions based on local Amp100.
# These bins correspond to the same bins as the emprical error quantile bins.  The error distribution around each point 
# estimate will be calculated from the quantile associated with the bin it is located in.
# 
###############################################################


#create ONE generic gaussian quantile - vary sd to see impact on score

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



#####
#now set lower and upper limits to quantiles as 0 and 1
#########






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


# For each set of quantile predictions, write to CSV file, separate for each farm
# 
# for (i in 1:10){
#   
#   write.csv(pred_gauss_quant[[i]],file=paste("quantile_est.Zone",i,".csv",sep=""))
#   
# }
#   


#quantile predictions in one file


all_quant_preds=c()

for (i in 1:10){
  all_quant_preds = rbind(cbind(all_quant_preds),cbind(pred_gauss_quant[[i]]))
}

#write.csv(all_quant_preds,file=paste("quantile_est.combined_forKWM.csv",sep=""), row.names=FALSE, col.names=FALSE)











###################################################################################
##############  check pinball loss on Eval data from last week  ######################
#######################################################################################



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
      y = prevScoring.raw[[i]]$TARGETVAR[j]            #observed value = y
    
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

# correlation plot of all features - sorted by hierarchical clustering
# corrplot::corrplot(cor(testX[,-1]), order = "original", tl.cex = .55, method = "color")
