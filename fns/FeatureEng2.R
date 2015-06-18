FeatureEng2<-function(train.raw, test.raw, input.opt, detUnique = 'yes'){
  
  train.len = nrow(train.raw[[1]])
  test.len = nrow(test.raw[[1]])
  
#  Create Vectors trainY and testY for Target Variables  --------------------
  trainY = vector('list', 10)
  testY = vector('list', 10)
  
  for (i in 1:10){
    
    trainY[[i]]$TARGETVAR = train.raw[[i]]$TARGETVAR
    
    if (length(test.raw[[i]]$TARGETVAR) > 0){
      
      testY[[i]]$TARGETVAR = test.raw[[i]]$TARGETVAR
      
    } else {
      writeLines("Target variables for this week's data not avaialable")
    } 
  }
  
#  Code for Various Input Options  ------------------------------------------
  if (input.opt==1){     # local u and v components of wind (4 variables)
    
    trainX = vector('list', 10)
    testX = vector('list', 10)
    all.features = vector('list', 10)
    amp100 = vector('list', 10)
    
    for (i in 1:10){
      
      trainX[[i]] = train.raw[[i]][,c('time','month')]    #initialize with time/month columns
      trainX[[i]] = rbind(trainX[[i]], test.raw[[i]][, c('time','month')])  #smooth train and test together
      
      dataA = train.raw[[i]]
      dataB = test.raw[[i]]
      
      data.smooth = rbind(dataA[,c("U10","V10","U100","V100")], dataB[,c("U10","V10","U100","V100")] )
      data.smooth = data.frame(SmoothKM(data.smooth, 1))
      names(data.smooth) = c("U10","V10","U100","V100")
      
      ### Training data ###       
      data.smooth$Amp10 = as.numeric(sqrt((data.smooth[,1])^2+(data.smooth[,2])^2))
      data.smooth$Amp100 = as.numeric(sqrt((data.smooth[,3])^2+(data.smooth[,4])^2))
      #data.smooth$Ampratio = data.smooth$Amp100/data.smooth$Amp10
      data.smooth$logistic = 1/(1+exp(-(data.smooth$Amp100 - 7)))
      #data$logistic = 1/(1+exp(-(data$Amp100 - 8)/2))  #adjusted logistic
      data.smooth = slide(data.smooth, Var = "Amp100", NewVar = 'lag1Amp100', slideBy = -1)
      data.smooth = slide(data.smooth, Var = "Amp100", NewVar = 'lag2Amp100',slideBy = -2)

      all.features[[i]] = data.smooth
      amp100[[i]] = data.smooth$Amp100[1:train.len]
    } 
    
    for (i in 1:10){
      for(j in 1:10){      
      trainX[[i]] = cbind(trainX[[i]], all.features[[j]])
      }
    }
    
    writeLines(paste('Input Option', input.opt, 'chosen: variables consist of smoothed variables', 
                     'plus month and two engineered variables (65 variables)'))
       
  }         

#  inialize variables for unique column detection  ----------------------------
  TempX = vector('list',1)
  for (i in 1:10){
    TempX[[i]] = trainX[[i]][,c(-1,-2)]
  
  
  ## Detect Unique Columns and remove TIME
  TempX[[i]]=round(TempX[[i]],digits=7)
  TempX[[i]]=unique(t(TempX[[i]]))  # detect unique
# TempX[[1]]=t(TempX[[1]])  #dont detect unique
  TempX[[i]]=data.frame(t(TempX[[i]]))     # data Frame
  trainX[[i]]= cbind(trainX[[i]]$month,TempX[[i]])
  names(trainX[[i]])[1] = 'month'
  #split trainX and testX
  testX[[i]] = trainX[[i]][(train.len+1) : (train.len + test.len),]
  trainX[[i]] = trainX[[i]][1:train.len,]
  }

      
  writeLines('Feature variables stored in trainX and testX')
  Output=list("trainX" = trainX, "testX" = testX, 
              'trainY' = trainY, 'testY'= testY, 'amp100' = amp100)
  return(Output)
  
  
  }
