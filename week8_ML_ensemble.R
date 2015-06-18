cat("\014")
rm(list=ls())

###########################  Version Notes ###############################
# 
# In this version, the code is adjusted so that errors are calculated across
# k-fold cross validation sets.  This should give a better estimate of the error
# variance, and hence wider and more realistic quantiles to add the point estimates to.
# 
# Another method to try is to just estimate the error variance as constant.  Right now 
# in the previous version, variance was considered to change as a function of Amp100.
# Although this appears more realistic, a simplified estimate of error may produce better
# results.
#
#
#
################################# Load Modules ############################ 
##### 
require(gbm)
require(bootstrap)
require(MASS)
require(ridge)
require(utils)
require(parcor)
require(gptk)
require(GPfit)
require(neuralnet)
require(rgl)
require(gtools)
require(e1071)
require(randomForest)
require(FNN)
require(dlm)
require(foreach)
require(doMC)
require(smoothmest)
require(plot3D)

################################# System Parameters ###############################
#####################
Task = 7                    # Task Week
PrvWkTrain.Length = 10200   # number of rows in the training data for last week
TrainRatio = 0.8            # ratio of training data used for CV
NTreesRF = 150
NTreesGBM = 1000                # Number of Trees in the GBM
QuantileMode = 1



#################################  Change the Directory ############################
#####

if (.Platform$OS.type=='windows'){
  prefix = "C:"
} else {
  prefix =  ""
}

workingdir = paste(prefix, "/Users/Min/Google Drive/Research/Projects/GEFCom2014/Wind/Week7/Week7_data", sep = '')
setwd(workingdir)

source(paste(prefix, '/Users/Min/Google Drive/Research/Projects/GEFCom2014/Wind/Week7/fns/Sys_fns.r', sep = ''))

##############################################################################################
##############################################################################################
#######################       Part 1 - Load Raw Data             #############################
##############################################################################################
##############################################################################################


# Load raw data in create Train/Test sets, do the same for subsets of previous data
# convert timestamps into factor variables for four data sets

ptm = proc.time()

# Read in data from working directory and store

SourceData=ReadData(Task)
Train.raw=SourceData$TrainingData
Test.raw=SourceData$TestData
rm(SourceData) 


#convert timestmap to factors for month and time
Train.raw=ConvertTime(Train.raw)  
Test.raw=ConvertTime.test(Test.raw)


# Create variables for previous week's data: training and scoring

PrevTrain.raw = vector('list', 10)
PrevScoring.raw = vector('list',10)

for (i in 1:10){
  PrevTrain.raw[[i]] = Train.raw[[i]][1:PrvWkTrain.Length,]
  PrevScoring.raw[[i]] = Train.raw[[i]][(PrvWkTrain.Length +1): TrainLength,]
}

print(round((proc.time()-ptm)/60))



##############################################################################################
##############################################################################################
#######################       Part 2 - Feature Engineering       #############################
##############################################################################################
##############################################################################################
#
#       Choose Input option and convert raw data into extracted features
#       Also, include local X variables in separate list of 10
#
#       Call Feature Engineering on PREVIOUS Week's RAW DATA

source(paste(prefix, '/Users/Min/Google Drive/Research/Projects/GEFCom2014/Wind/Week7/fns/Smooth_KM.r', sep = ''))
source(paste(prefix, '/Users/Min/Google Drive/Research/Projects/GEFCom2014/Wind/Week7/fns/FeatureEng.r', sep = ''))

InputOption = 12 
InputData=FeatureEng(PrevTrain.raw,PrevScoring.raw, InputOption)           #THIS IS THE STRUCTURE OF TRAINING DATA
PrevTrainX = InputData$TrainX[[1]]              # data frame of all Training x's
PrevScoringX = InputData$TestX[[1]]             # data frame of all Test/Scoring x's
PrevTrainX.local = InputData$TrainX.local       # local x's 
PrevScoringX.local = InputData$TestX.local
rm(InputData)

#########   Sanity Check #############

#plot and view smoothed data
plot(PrevScoring.raw[[1]]$U100)
lines(PrevScoringX$U100, col='red')

plot(PrevTrain.raw[[1]]$U100)
lines(PrevTrainX$U100, col='red')





####################################################################################
####################################################################################
############   Part 4 - TRAIN on Entire Prev. Week Training Data    ################
####################################################################################
####################################################################################
#
# In the previous section, we split the Training data into training and validation using 80% split and ran
# k-fold cross validation to choose model parameters and estimate average RMSE across k-folds. 
# 
# In this section, we will use the entire training data set to train our model with the parameters
# chosen above.  The TEST set will be the SCORING data.
# 
# 

## Input Option for training features chosen in RUN 1



#run code here
RMSE_alltrain= c()
fits.rf = vector('list',10)
fits.gbm = vector('list',10)
predict_train = vector('list',10)
best.iter = c()

  
### If InputOption > 6, TrainX is list of length 1, do not need to iterate in for loop
  ptm = proc.time()
  train_rows = 1:PrvWkTrain.Length

#   for global training use this input data:
  data.train = data.frame(PrevTrainX,check.names = T)
  data.test =  data.train
  

print(paste('Number of trees for RF =', NTreesRF, 'Num of trees for GBM =', NTreesGBM))

  for (i in 1:10){
    print(paste('Training wind farm #', i, sep = ' ' ))
    #   for local training use this input data:
#       data.train = data.frame(PrevTrainX.local[[i]],check.names = T)
#       data.test =  data.train
    
    target.train = PrevTrain.raw[[i]]$TARGETVAR
    target.test = target.train
    
    ##fit using four functions: glm, svm, gbm,rf

    #MAIN RF CALL   
   myfit.rf = randomForest(data.train, target.train, ntree=NTreesRF)
    print('Random Forest training complete')
    
    #####   Use this call if Tune RF desired  ############
#     bestmtry = tuneRF(data.train, target.train,ntreeTry=150, 
#                       stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE) 
#     a = which.min(bestmtry[,2])
#     
#     myfit.rf = randomForest(data.train, target.train, ntree=NTreesRF,mtry=bestmtry[a,1])
#     print('Random Forest training complete')
#     
    ##########   use this RF call if parallel is desired  #################
# 
#     registerDoMC(4)
#     myfit.rf = foreach(ntree=rep(40, 4), .combine=combine, .packages='randomForest') %dopar% {
#       randomForest(data.train, target.train, ntree=ntree)
#     }
# 

    ########################################################################### 

    
    myfit.gbm =
      gbm(target.train~.,         # formula
          data=data.train,                   # dataset
          var.monotone=NULL,  # -1: monotone decrease,
          # +1: monotone increase,
          #  0: no monotone restrictions
          distribution="gaussian",      # see the help for other choices
          n.trees=NTreesGBM,                 # number of trees
          shrinkage=0.01,               # shrinkage or learning rate,
          # 0.001 to 0.1 usually work
          interaction.depth=3,          # 1: additive model, 2: two-way interactions, etc.
          bag.fraction = 0.5,           # subsampling fraction, 0.5 is probably best
          train.fraction = .85,         # fraction of data for training,
          # first train.fraction*N used for training
          n.minobsinnode = 10,          # minimum total weight needed in each node
          cv.folds = 0,                 # do 5-fold cross-validation
          keep.data=TRUE,               # keep a copy of the dataset with the object
          verbose=FALSE,                # don't print out progress
          n.cores=2)                    # use only a single core (detecting #cores is
    # error-prone, so avoided here)
    print('GBM training complete')

    best.iter[i] = gbm.perf(myfit.gbm,method="test",plot.it=T)
    
    fits.rf[[i]]=myfit.rf
    fits.gbm[[i]]=myfit.gbm

    predictions.gbm = predict(myfit.gbm, data.test, best.iter[i])
    predictions.rf = predict(myfit.rf, data.test)
    predictions.rf = (predictions.rf+predictions.gbm)/2
    predict_train[[i]]=predictions.rf
    
    
    RMSE.rf=sqrt(mean((predictions.rf-target.test)^2))
    
    RMSE_alltrain = rbind(RMSE_alltrain,(cbind(RMSE.rf)))
    print(paste('Training RMSE for farm', i, '=', RMSE.rf, sep =' '))
    print(paste('Finished Training wind farm #', i, sep = ' ' ))
  }
print('MODEL TRAINING COMPLETE - FITS FOR EACH FARM STORED IN LIST:  fits[[i]]')
print(proc.time()-ptm)
print(RMSE_alltrain)

#calculate error vectors
# errors_train = vector('list',10)
# 
# for (i in 1:10){
#   target.test =Train.raw[[i]]$TARGETVAR[train_rows]
#   errors_train[[i]] = predict_train[[1]]- target.test
#   
# }
# 
# plot(data.train$Amp100,errors_train[[1]])

########################################################
#
#
#   MODEL TRAINING COMPLETE - FITS FOR EACH FARM STORED IN LIST:  fits[[i]]
#
#
##########################################


source(paste(prefix, '/Users/Min/Google Drive/Research/Projects/GEFCom2014/Wind/Week7/fns/Smooth_KM.r', sep = ''))


############ NExt Create predictions on Eval Data -  Will use emprical error quantiles and add predicted point estimates
          
######################################################
######################################################
######################################################
######################################################
############                              ############
############  Predict on SCORING Data     ############
############                              ############
######################################################
#
# In the previous step, emprical error quantiles were calculated for 6 bins of wind speed, in each wind farm.  
# The objective is to use these quantiles to represent the expected error distribution when we predict the point estimates.
# We will estimate this distribution by first calculating point estimates, then depending on which bin the estimated point
# estimate alls into, we will add the point estimate value to each percentile value in the associated error distribution.
# 
#### Call Feature Engineering - choose data structure for EVALUATION DATA same as on training data

data.scoring =  data.frame(PrevScoringX,check.names=T)

#predictions in 10 different files
predict.scoring = vector('list',10)
presub_preds = vector('list',10)
RMSE.scoring = data.frame()
errors_pred = vector('list',10)

for (i in 1:10){
  target.scoring = PrevScoring.raw[[i]]$TARGETVAR
  LocalX.scoring = PrevScoringX.local[[i]]
  
  predictions.gbm = predict(fits.gbm[[i]], data.scoring, best.iter[i])
  predictions.rf = predict(fits.rf[[i]], data.scoring)
  predictions.scoring = (predictions.rf+predictions.gbm)/2
  predictions.scoring = Smooth_KM(data.frame(predictions.scoring)) #to smooth or not to smooth?
  
  predict.scoring[[i]] = predictions.scoring
  #write.table(predict.scoring[[i]],file=paste("PointEstimates.Zone",i,".csv",sep=""), row.names=FALSE, col.names=FALSE)

  Amp100 = LocalX.scoring$Amp100
  presub_preds[[i]] = rbind(presub_preds[[i]],cbind(Amp100,predictions.scoring)) 
  ####this is the n x 2 vector of just errors and associated local wind speed
  #list of 10, 1 for each windfarm
  
  errors_pred[[i]] = predictions.scoring - target.scoring
  RMSE_eval=sqrt(mean((predictions.scoring-target.scoring)^2))
  RMSE.scoring = rbind(RMSE.scoring,(cbind(RMSE_eval)))
}

RMSE.scoring

# #predictions in one file
# predictall=c()
# 
# for (i in 1:10){
#   predictall = rbind(cbind(predictall),cbind(predict(fits[[i]], data.scoring)))
# }

#write.table(predictall,file=paste("pt_est_combined_submit3.csv",sep=""), row.names=FALSE, col.names=FALSE)

#### Visualize prediction errors  ########

par(mfrow=c(1,2))
for (i in 1:10){
  plot(PrevScoringX.local[[i]]$Amp100, errors_pred[[i]],ylim=c(-.7,.7), xlim = c(0,20), 
       main = paste('farm',i))
#   plot(PrevScoringX.local[[i]]$logistic, errors_pred[[i]],ylim=c(-.7,.7), xlim = c(0,1.1), 
#        main = paste('farm',i))
   hist(errors_pred[[i]],breaks=20,ylim=c(0,300), xlim = c(-.7,.7))
}




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





#########################
#Now, from above we have binned the predictions. For each point estimate, we will add the quantile values
#in the assocaited bin. This will generate the error quantiles around the point estimates 

#create ONE generic gaussian quantile - vary sd to see impact on score

#########     guassian error dist here:
#gauss_err_dist = rnorm(100000, mean = 0, sd = .15)
# gauss_quant = quantile(gauss_err_dist, probs = seq(0, 1, 0.01), na.rm = F, names = F, type = 7)
# gauss_quant = gauss_quant[c(-1,-101)]
# pred_gauss_quant = vector('list',10)


#####     laplace error dist here:
#gauss_err_dist = rdoublex(80000, mu = 0, lambda  = .1)
gauss_err_dist = rlogis(80000,location = 0, scale =.09)
#gauss_err_dist = rcauchy(80000,location = 0, scale =.08)
hist(gauss_err_dist, breaks = 1000)
gauss_quant = quantile(gauss_err_dist, probs = seq(0, 1, 0.01), na.rm = F, names = F, type = 7)
gauss_quant = gauss_quant[c(-1,-101)]
pred_gauss_quant = vector('list',10)

for (i in 1:10){
  for (j in 1:length(presub_preds[[i]][,2])){
   temp = gauss_quant + presub_preds[[i]][j,2]
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
loss_matrix = matrix(,nrow = 10*length(pred_gauss_quant[[i]][,1]),ncol = 99 )
for (i in 1:10){                                #for each wind farm
  loss_farm = 0
  L=0
  for (j in 1:length(pred_gauss_quant[[i]][,1])){      #for each row of predictions
    for (a in 1:99){                            #for each quantile
    
      q = pred_gauss_quant[[i]][j,a]                 #target quantile value is equal to quantile prediction value
      y = PrevScoring.raw[[i]]$TARGETVAR[j]            #observed value = y
    
      if  (y<q){                                  #if observed is less than predicted, 
        L = (1-a/100)*(q-y)                     #calculate loss for cell
        loss_farm = loss_farm + L             #add this cell's loss to total loss
        loss_matrix[j+(i-1)*744,a]=L
      }
       
      else if (y>=q){                             #if observed is greater/equal to predicted
        L = (a/100)*(y-q)                       #calculate loss for cell
        loss_farm = loss_farm + L             #add to total loss
        loss_matrix[j+(i-1)*744,a]=L
      }
    }
  }
  
  print(paste('avg. pinball loss of farm', i, '=', loss_farm/(99*length(pred_gauss_quant[[i]][,1]))))
  loss_total[i] = loss_farm
}

print(paste('expexcted pinball loss score =', as.character(sum(loss_total)/(10*length(pred_gauss_quant[[i]][,1])*99))), sep = '')

View(loss_total/(99*length(pred_gauss_quant[[i]][,1])))

# correlation plot of all features - sorted by hierarchical clustering
# newframe=data.frame(PrevScoringX[,-1],PrevScoring.raw[[10]]$TARGETVAR)
# names(newframe)[65]='TARGET'
# corrplot::corrplot(cor(newframe), order = "hclust", tl.cex = .55, method = "color")
# 
# 
# corrplot::corrplot(cor(PrevScoringX[,-1]), order = "original", tl.cex = .55, method = "color")

# 
# ###### Visualize errors ######
# # 
# 
# plot3d(x = seq(0, 1, length.out = nrow(loss_matrix[1:744,])),
#          y = seq(0, 1, length.out = ncol(loss_matrix[1:744,])), loss_matrix[1:744,], col=rainbow(4))
# 
# contour2D(loss_matrix)
# b = c()
# for (i in 1:99){
# b[i] = sum(loss_matrix[(1:744),i])
# 
# }
# plot(loss_matrix[2388,])
# hist(loss_matrix,breaks=100)
# plot(loss_matrix[,50])
# 
