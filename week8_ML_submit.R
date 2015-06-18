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
require(scatterplot3d)
require(rgl)
require(gtools)
require(e1071)
require(randomForest)
require(FNN)
require(snow)
require(dlm)
require(foreach)
require(doMC)

################################# System Parameters ###############################
#####################
Task = 7                    # Task Week
PrvWkTrain.Length = 10200   # number of rows in the training data for last week
TrainRatio = 0.8            # ratio of training data used for CV
NTreesRF = 150
NTrees = 500                # Number of Trees in the GBM
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
InputData=FeatureEng(Train.raw,Test.raw, InputOption)           #THIS IS THE STRUCTURE OF TRAINING DATA
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




##############################################################################################
##############################################################################################
##################         Part 3 - Empirical Error Quantiles         ########################
##############################################################################################
##############################################################################################
#
# In this section we can experiment with choices of input option, and parameters
# of the model.  At the end of the section, the code will calculate RMSE across k-fold
# cross-validation.  Errors between predictions and test set are also calculated
# here.  Quantile estimates of the errors can then be determined.
# 
#    This section will work on Previous Week's Training/Scoring data
#

#     CHECK EXPECTED RMSE USING K-FOLD CROSS VALIDATION   - NEXT STEP WILL BE TO TRAIN/PREDICT CHECK LOSS FN


#>>>>>>>>>>>>>> run different iterations starting from this point <<<<<<<<<<

#initialize variables
RMSE_kfolds = vector('list',5) # RMSE for 10 farms for each fold
predictions.cv = vector('list',10)  # list of predictions for each fold
errors.cv = vector('list',10)  #list of differences between prediction and target
presub_errors = vector('list',10) #  ####this is the n x 2 vector of just errors and associated local wind speed


ptm = proc.time()

#  generate 5 subsets of the training data - first define training row subsets

train_rows = vector('list',5)
for (k in 0:4){
  train_rows[[k+1]] = (1+k*(nrow(PrevTrainX)/5)):((k+1)*(nrow(PrevTrainX)/5))
}


for (k in 5){             
  
  data.train = data.frame(PrevTrainX[-train_rows[[k]], ],check.names = T)  #assign training data to new variable
  data.cv =  data.frame(PrevTrainX[train_rows[[k]], ],check.names=T)   #assign test data to new variable
  
  
  print(paste('Number of trees =', NTreesRF))
  
  for (i in 1:10){
    print(paste('Training fold #', k, 'for wind farm #', i, sep = ' ' ))
        
    target.train = data.frame(PrevTrain.raw[[i]]$TARGETVAR[-train_rows[[k]]])  #target variables needs to be chosen for each farm in loop
    names(target.train)[1] = 'TARGETVAR'
    target.cv =data.frame(PrevTrain.raw[[i]]$TARGETVAR[train_rows[[k]]])
    names(target.cv)[1] = 'TARGETVAR'
    
    data.train.local = PrevTrainX.local[[i]][-train_rows[[k]],]
        

#     rfcv(data.train, target.train, cv.fold=2, scale="log", step=0.5,
#          mtry=function(p) p/3, recursive=FALSE)
    #MAIN RF CALL   
   myfit.rf = randomForest(data.train, target.train$TARGETVAR, ntree=NTreesRF)
    
    
      ##########   use this RF call if tuning is desired  #################
# 
#     myfit.rf = tuneRF(data.train, target.train,ntreeTry=150, 
#                       stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=T) 
#     
      ########################################################################### 
    

#     ##################### try parallel random forest here             ###################
#     
#     ptm = proc.time()
#     registerDoMC(4)
#     myfit.rf = foreach(ntree=rep(150, 2), .combine=combine, .packages='randomForest') %dopar% {
#       randomForest(data.train, target.train$TARGETVAR, ntree=ntree)
#     }
#   
#     print(proc.time()-ptm)
#     
#     ###########################################################################


# 
#     # fit GBM Model
#     myfit.rf <-
#       gbm(target.train~.,         # formula
#           data=data.train,                   # dataset
#           var.monotone=NULL,  # -1: monotone decrease,
#                                         # +1: monotone increase,
#                                         #  0: no monotone restrictions
#           distribution="gaussian",      # see the help for other choices
#           n.trees=1000,                 # number of trees
#           shrinkage=0.05,               # shrinkage or learning rate,
#                                         # 0.001 to 0.1 usually work
#           interaction.depth=3,          # 1: additive model, 2: two-way interactions, etc.
#           bag.fraction = 0.5,           # subsampling fraction, 0.5 is probably best
#           train.fraction = 0.5,         # fraction of data for training,
#                                         # first train.fraction*N used for training
#           n.minobsinnode = 10,          # minimum total weight needed in each node
#           cv.folds = 3,                 # do 3-fold cross-validation
#           keep.data=TRUE,               # keep a copy of the dataset with the object
#           verbose=FALSE,                # don't print out progress
#           n.cores=1)                    # use only a single core (detecting #cores is
#     # error-prone, so avoided here)
#     
    
    
    predictions.rf = predict(myfit.rf, data.cv)
    predictions.cv[[i]] = rbind(predictions.cv[[i]],(cbind(predictions.rf))) #pred in each fold get appended by rows

    test_errors =  predictions.rf - target.cv
    errors.cv[[i]] = rbind(errors.cv[[i]],(cbind(test_errors)))  #errors in each fold get appended by rows

    Amp100 = data.train.local$Amp100
    presub_errors[[i]] = rbind(presub_errors[[i]],cbind(Amp100,test_errors))
    
    RMSE.rf=sqrt(mean((predictions.rf-target.cv)^2))
    RMSE_kfolds[[k]] = rbind(RMSE_kfolds[[k]],(cbind(RMSE.rf)))

    print(paste('Cross Validation fold #', k, 'finished for wind farm #', i, sep = ' ' ))
    print(paste('RMSE for CV fold #', k, '=', as.table(RMSE_kfolds[[k]]), sep = ' ' ))
  }
}
print((proc.time()-ptm)/60)


#### Calculate average of RMSE across k-fold cross validation results
RMSE_avg5folds = c()
  for(i in 1:10) {# 10 RMSE values, one for each wind farm 
    RMSE_avg5folds = rbind(RMSE_avg5folds,mean(c(RMSE_kfolds[[1]][i],RMSE_kfolds[[2]][i],RMSE_kfolds[[3]][i],
                                             RMSE_kfolds[[4]][i],RMSE_kfolds[[5]][i])))
  }
View(RMSE_avg5folds)



#### Calculate 1% quantiles for error distributions in each farm
#### Plot histogram of errors and empirical quantiles for each wind farm
for (i in 1:10){
  par(mfrow=c(1,2))
  a = quantile(presub_errors[[i]][,2], probs = seq(0, 1, 0.01), na.rm = F,names = F, type = 7)
  b =presub_errors[[i]][,2]
  
  
  hist(b,breaks = 500,xlab=paste("Predicted values - observed values"), ylab="Frequency",
       main =paste('Histogram of errors at Farm',as.character(i), sep=" "))
  plot(a,xlab=paste("1% quantiles"), ylab="Power Output",
       main =paste('1% quantiles for Farm',as.character(i), sep=" "))
}
par(mfrow=c())





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
fits = vector('list',10)
predict_train = vector('list',10)

  
### If InputOption > 6, TrainX is list of length 1, do not need to iterate in for loop
  ptm = proc.time()
  train_rows = 1:PrvWkTrain.Length
  
  data.train = data.frame(PrevTrainX,check.names = T)
  data.test =  data.train
  

print(paste('Number of trees =', NTreesRF))

  for (i in 1:10){
    print(paste('Training wind farm #', i, sep = ' ' ))
    
    target.train = Train.raw[[i]]$TARGETVAR
    target.test = target.train
    
    ##fit using four functions: glm, svm, gbm,rf

    
    
    ##fit using four functions: glm, svm, gbm,rf
    bestmtry = tuneRF(data.train, target.train,ntreeTry=300, 
                      stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE) 
    a = which.min(bestmtry[,2])
    
    myfit.rf = randomForest(data.train, target.train, ntree=NTreesRF,mtry=bestmtry[a,1])
    
    
    ##########   use this RF call if tuning is desired  #################
#     
#         myfit.rf = tuneRF(data.train, target.train,ntreeTry=150, 
#                           stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=T) 
#         
#     ########################################################################### 
    
    
    fits[[i]]=myfit.rf
    
    predictions.rf = predict(myfit.rf, data.test)
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





#######################################################
########          generate quantiles          #########
########################################################
# There are two possibilites for generating quantiles for each wind farm:
#   
#   (1)  Assume constant variance in the error distribution.
#        In this case, just need to fit the quantile function for the empirical errors 
#        measured during cross validation over k-folds.
# 
#   (2)  Consider that error variance is NOT CONSTANT.  Intially, we will assume
#        that error variance is a function of Amp100 at the local wind farm.  Plotting emprical
#        error on the test set indicates this is reasonable.
# 
#        In this scenario, we need to calculate different quantile distributions depending on Amp100.
#        The procedure is as follows:
# 
##############################################################################################
# First, we need to determine the windspeeds at which we want to use different quantile distributions
# In order to determine this we look at the Amp100 data at each wind farm in the entire traing set.
# We calculate the 20% quantiles of Amp100 at each farm so that approximately 1/5 of the data points lie in
# each bin.

windspeed_cutoffs = vector('list',10)

for (i in 1:10){
  
  data.train = data.frame(PrevTrainX.local[[i]],check.names = T)  
  
  cutoffs = quantile(data.train$Amp100, probs = seq(0, 1, 0.20), na.rm = F,
                             names = F, type = 7)
  windspeed_cutoffs[[i]] = rbind(0,data.frame(cutoffs))  # we add 
  rm(cutoffs)
}

windspeed_cutoffs[[3]][7,] = 20 

print('Calculation of cutoffs for wind speed bins complete')



####### Subset (bin) the error data based on the above windspeed cutoffs 
####### Create 6 subests based for each of 10 windfarms
binned_errors = vector('list',10)  #list of subsets for each windfarm, each windfarm has a list of 6 bins

for (j in 1:10){
  subset = vector('list',6)   #create temporary vector to store 6 bins for errors at each windfarm
  
  for (i in 2:7){
      subset[[i-1]] = subset(presub_errors[[j]],(presub_errors[[j]][,1]>windspeed_cutoffs[[j]][i-1,] & 
                                                        presub_errors[[j]][,1]<=windspeed_cutoffs[[j]][i,]))
    }
  binned_errors[[j]] = subset   
  rm(subset)
}
#select each bin of errors by binned_errors[[j]][[k]] - each bin contains both error and associated Amp100

######
#  Sanity Check - sum of length of each bin. should = 40800 in each bin
############

 for (j in 1:10){
   sum=0
      for(k in 1:6){
      len = length(binned_errors[[j]][[k]][,1])
       sum = sum + len
     
       }
  print(sum)
 }
########################################





#####
# The k-fold CV errors were binned above according to wind speed
# Now, calculate quantiles based on the error distribution in each bin (6 total) in each windfarm.  So 6*10 = 60 quantiles needed.
#these are the emprical error quantiles, binned by Amp100


emp_err_quantiles = vector('list',10)  #list of empirical error quantiles for each bin at each windfarm

for (j in 1:10){
  quant_calc = vector('list',6)   #create temporary vector to store quantile calcs for 6 bins
  
  for (i in 1:6){
    
    quant_calc[[i]] = quantile(-binned_errors[[j]][[i]][,2], probs = seq(0, 1, 0.01), na.rm = F, names = F, type = 7)
    quant_calc[[i]] = quant_calc[[i]][c(-1,-101)]
    
  }
  
  emp_err_quantiles[[j]] = quant_calc   
  rm(quant_calc)
}

# note farm 9, bin 1 is empty.  in other farms, 
#the first bin has very few points, so replace bin 1 with bin 2
for (i in 1:10){
  emp_err_quantiles[[i]][[1]] = emp_err_quantiles[[i]][[2]] 
}
#select each bin of errors by binned_errors[[j]][[k]] - each bin contains both error and associated Amp100


#########
# Sanity Check - plot all quantiles for each wind farm
#######

for (j in 1:10){
  par(mfrow=c(2,3))
  for (i in 1:6){ #start at 2 since some farms don't have any points in bin 1
    
    plot(emp_err_quantiles[[j]][[i]])
  }
}
print('looks reasonable!')



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


for (i in 1:10){
  target.socring = PrevScoring.raw[[i]]$TARGETVAR
  LocalX.scoring = PrevScoringX.local[[i]]
  
  predictions.scoring = predict(fits[[i]], data.scoring)
  predict.scoring[[i]] = predictions.scoring
  #write.table(predict.scoring[[i]],file=paste("PointEstimates.Zone",i,".csv",sep=""), row.names=FALSE, col.names=FALSE)

  Amp100 = LocalX.scoring$Amp100
  presub_preds[[i]] = rbind(presub_preds[[i]],cbind(Amp100,predictions.scoring)) 
  ####this is the n x 2 vector of just errors and associated local wind speed
  #list of 10, 1 for each windfarm
  
  
  RMSE_eval=sqrt(mean((predictions.scoring-target.socring)^2))
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


####### Subset (bin) the predictions based on the  windspeed cutoffs 
####### Create 6 subests  for each of 10 windfarms
binned_preds = vector('list',10)  #list of subsets for each windfarm, each windfarm has a list of 6 bins

for (j in 1:10){
  subset = vector('list',6)   #create temporary vector to store 6 bins for errors at each windfarm
  
  for (i in 2:7){
    subset[[i-1]] = subset(presub_preds[[j]],(presub_preds[[j]][,1]>windspeed_cutoffs[[j]][i-1,] & 
                                                 presub_preds[[j]][,1]<=windspeed_cutoffs[[j]][i,]))
  }
  binned_preds[[j]] = subset   
  rm(subset)
}
#select each bin of errors by binned_preds[[j]][[k]] - each bin contains both prediction and associated Amp100

######
#  Sanity Check - sum of length of each bin. should = 744 in each bin
############

for (j in 1:10){
  sum=0
  for(k in 1:6){
    len = length(binned_preds[[j]][[k]][,1])
    sum = sum + len
    
  }
  print(sum)
}
########################################





#########################
#Now, from above we have binned the predictions. For each point estimate, we will add the quantile values
#in the assocaited bin. This will generate the error quantiles around the point estimates 

#these are the emprical error quantiles, 6 bins based on Amp100, for each of 10 windfarms

scale.factor = 1.08
predicted_quant = vector('list',10) # one for each farm

for (i in 1:10){  # for all wind farms
  
  quant_diff = vector('list',6)   #create temporary vector to store 6 bins of quantile predictions
  
  for (j in 1:6){ # for each bin in each farm
    
    if (length(binned_preds[[i]][[j]][,1])>0){  #check if bin has any points in it
      
      
      for (k in 1:length(binned_preds[[i]][[j]][,1])){
        
        temp = binned_preds[[i]][[j]][k,2] + emp_err_quantiles[[i]][[j]]*scale.factor
        quant_diff[[j]] = rbind(quant_diff[[j]],t(temp))
      }
      
      row.names(quant_diff[[j]]) = row.names(binned_preds[[i]][[j]])
      
    }
  
  }
  predicted_quant[[i]] = quant_diff
  rm(quant_diff)
  
}


######
#  Sanity Check 1 - sum of length of each bin. should = 744 in each bin
############

for (i in 1:10){
  sum=0
  for(j in 1:6){
    len = length(predicted_quant[[i]][[j]][,1])
    sum = sum + len
    
  }
  print(sum)
}


#########
# Sanity Check 2 - plot all quantiles for each wind farm
#######

for (i in 1:10){
  par(mfrow=c(2,3))
  for (j in 2:6){ 
    
    plot(predicted_quant[[i]][[j]][5,])  #only plot the first quantile prediction, should have same shape as quantile
  }
}
print('looks reasonable!')







################
# COMBINE PREDICTION QUANTILES AND ORDER BASED ON ROWNAME FOR EACH WIND FARM

#First - combine predictions quantiles across 6 quantile bins

#predicted_quant_comb = vector('list',10)
solutions = vector('list', 10)

for (i in 1:10){
  predicted_quant_comb = data.frame()
  for(j in 1:6){
    predicted_quant_comb =rbind(predicted_quant_comb,data.frame(predicted_quant[[i]][[j]]))
    solutions[[i]] = cbind(as.numeric(row.names(predicted_quant_comb)), predicted_quant_comb)
  }
  solutions[[i]]=solutions[[i]][order(solutions[[i]][,1]),]
  rm(predicted_quant_comb)
}




######
#  Sanity Check 1 - sum of length of each farm should = 744 in each bin
############

for (i in 1:10){
  sum=0
  
    len = length(solutions[[i]][,1])
    sum = sum + len
    

  print(sum)
}

#####
#now set lower and upper limits to quantiles as 0 and 1
#########






for (i in 1:10){  #for each wind farm
  for (j in 1:length(solutions[[i]][,1]) ) {  #for each row of the solutions
    
    for (k in 2:100){ #check each cell, quantile value.  if neg, set = 0, if <1, set =1
    
      if (solutions[[i]][j,k] < 0){
        solutions[[i]][j,k] = 0
      }
      else if (solutions[[i]][j,k] > 1){
        solutions[[i]][j,k] = 1
      }
    }
  }
}


# For each set of quantile predictions, write to CSV file, separate for each farm
# 
# for (i in 1:10){
#   
#   write.csv(solutions[[i]],file=paste("quantile_est.Zone",i,".csv",sep=""))
#   
# }
#   
#quantile predictions in one file
all_quant_preds=c()

for (i in 1:10){
  all_quant_preds = rbind(cbind(all_quant_preds),cbind(solutions[[i]]))
}

write.csv(all_quant_preds,file=paste("quantile_est.wk7_submit.csv",sep=""), row.names=FALSE, col.names=FALSE)




