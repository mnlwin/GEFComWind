#### Function: FeatureEng - Generate the Input Data Structure
FeatureEng<-function(train.raw, test.raw, InputOption){
  
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
  if (InputOption==1){     # local u and v components of wind (4 variables)
    
    trainX=vector('list',10)  
    
    for (i in 1:10){
      trainX[[i]] = train.raw[[i]][,c('U10','V10','U100','V100')]          
    }                
  }         
  
  else if (InputOption==2){     #local Amplitude and angles of wind speed (4 variables) 
    
    trainX=vector('list',10)  
    
    for (i in 1:10){
      ZonalTotalData=train.raw[[i]]
      
      Amp10=sqrt(ZonalTotalData$U10^2+ZonalTotalData$V10^2)
      Ang10=Cart2Polar(ZonalTotalData$U10,ZonalTotalData$V10)
      
      Amp100=sqrt(ZonalTotalData$U100^2+ZonalTotalData$V100^2)
      Ang100=Cart2Polar(ZonalTotalData$U100,ZonalTotalData$V100)
      
      trainX[[i]]=as.data.frame(cbind(Amp10, Ang10, Amp100, Ang100))
      
    }
  }          
  
  
  else if (InputOption==3){   #local U and V + amp angles (8 variables)
    
    trainX=vector('list',10)  
    
    for (i in 1:10){
      
      trainX[[i]] = train.raw[[i]][,c('U10','V10','U100','V100')]  
      
      ZonalTotalData=train.raw[[i]]
      
      Amp10=sqrt(ZonalTotalData$U10^2+ZonalTotalData$V10^2)
      Ang10=Cart2Polar(ZonalTotalData$U10,ZonalTotalData$V10)
      
      Amp100=sqrt(ZonalTotalData$U100^2+ZonalTotalData$V100^2)
      Ang100=Cart2Polar(ZonalTotalData$U100,ZonalTotalData$V100)
      
      trainX[[i]]=as.data.frame(cbind(trainX[[i]],Amp10, Ang10, Amp100, Ang100))
      
    }   
  }                 
  
  
  else if (InputOption==4){     #Option 1 + time and month (6 variables)
    
    trainX=vector('list',10)  
    
    for (i in 1:10){
      trainX[[i]] = train.raw[[i]][,c('time','month','U10','V10','U100','V100')]          
    }                  
  }
  
  
  else if (InputOption==5){     #Option 2 + time and month (6 variables)
    
    trainX=vector('list',10)  
    
    for (i in 1:10){
      ZonalTotalData=train.raw[[i]]
      
      Amp10=sqrt(ZonalTotalData$U10^2+ZonalTotalData$V10^2)
      Ang10=Cart2Polar(ZonalTotalData$U10,ZonalTotalData$V10)
      
      Amp100=sqrt(ZonalTotalData$U100^2+ZonalTotalData$V100^2)
      Ang100=Cart2Polar(ZonalTotalData$U100,ZonalTotalData$V100)
      
      trainX[[i]]=as.data.frame(cbind(train.raw[[i]][,c('time','month')],Amp10, Ang10, Amp100, Ang100))
      
    }             
  }
  
  
  else if (InputOption==6){  #Option 3 + time and month (10 variables)
    
    trainX=vector('list',10)  
    
    for (i in 1:10){
      
      trainX[[i]] = train.raw[[i]][,c('time','month','U10','V10','U100','V100')]  
      
      ZonalTotalData=train.raw[[i]]
      
      Amp10=sqrt(ZonalTotalData$U10^2+ZonalTotalData$V10^2)
      Ang10=Cart2Polar(ZonalTotalData$U10,ZonalTotalData$V10)
      
      Amp100=sqrt(ZonalTotalData$U100^2+ZonalTotalData$V100^2)
      Ang100=Cart2Polar(ZonalTotalData$U100,ZonalTotalData$V100)
      
      trainX[[i]]=as.data.frame(cbind(trainX[[i]],Amp10, Ang10, Amp100, Ang100))
      
    }
    writeLines('Input Option 6 chosen: x variables consist of local variables
          plus time and month (10 variables)')
  }
  
  
  else if (InputOption==7){ #Main dataset - combines time/month with 80 featurs
    
    trainX=vector('list',1)
    trainX[[1]] = train.raw[[1]][,c('time','month')]
    
    for (i in 1:10){
      
      ZonalTotalData=train.raw[[i]]
      
      Amp10=sqrt(ZonalTotalData$U10^2+ZonalTotalData$V10^2)
      Ang10=Cart2Polar(ZonalTotalData$U10,ZonalTotalData$V10)
      
      Amp100=sqrt(ZonalTotalData$U100^2+ZonalTotalData$V100^2)
      Ang100=Cart2Polar(ZonalTotalData$U100,ZonalTotalData$V100)
      
      trainX[[1]]=as.data.frame(cbind(trainX[[1]],train.raw[[i]][,c('U10','V10','U100','V100')],Amp10, Ang10, Amp100, Ang100))
    } 
    
    writeLines('Input Option 7 chosen: x variables consist of global wind variables 
          plus time and month (82 variables)')
    
  }
  
  
  
  #create classes for curtailment
  else if (InputOption==8){ 
    for (i in 1:TrainLength){
      
      if (dataset1[i,1] == 0 & dataset1[i,3] > 3.6 & dataset1[i,5] > 4.5 ){
        dataset1[i,7]= "curtail"
      }
      else {dataset1[i,7]= "normal"}
    }
    
    dataset1[,7] = as.factor(dataset1[,7])
    colnames(dataset1) <- c("target", "time", "amp10", "ang10", "amp100", "ang100", "curtailment")
    
    
    #view curtailment data for wind farm 1
    attach(dataset1)
    plot(amp10, target, col = c('red', 'black')[curtailment])
    detach(dataset1)
    
    
    ### filter curtailment data
    
    nocurt = subset(dataset1, curtailment =='normal')
    curt = subset(dataset1, curtailment =='curtail')
  }
  
  else if (InputOption==9){ #remove wind direction angle data - it does not seem to be relevant
    
    trainX=vector('list',1)
    trainX[[1]] = train.raw[[1]][,c('time','month')]
    
    for (i in 1:10){
      
      ZonalTotalData=train.raw[[i]]
      
      Amp10=sqrt(ZonalTotalData$U10^2+ZonalTotalData$V10^2)      
      Amp100=sqrt(ZonalTotalData$U100^2+ZonalTotalData$V100^2)
      
      trainX[[1]]=as.data.frame(cbind(trainX[[1]],train.raw[[i]][,c('U10','V10','U100','V100')],Amp10, Amp100))
    } 
    writeLines('Input Option 9 chosen: x variables consist of global variables with Ang10 and Ang 100 removed, plus time and month (49 variables)')
  }
  
  
  else if (InputOption==10){ #smooth input data for input option 7
    
    trainX=vector('list',1)
    trainX[[1]] = train.raw[[1]][,c('time','month')]
    
    for (i in 1:10){
      
      DataA=train.raw[[i]]
      
      Data = DataA[,c("U10","V10","U100","V100")]
      Data_Smooth = SmoothKM(Data)
      
      ### Training Data ### 
      Data$U10 = as.numeric(Data_Smooth[,1])
      Data$V10 = as.numeric(Data_Smooth[,2])
      
      Data$U100 = as.numeric(Data_Smooth[,3])
      Data$V100 = as.numeric(Data_Smooth[,4])
      
      Data$Amp10 = as.numeric(sqrt((Data_Smooth[,1])^2+(Data_Smooth[,2])^2))
      Data$Amp100 = as.numeric(sqrt((Data_Smooth[,3])^2+(Data_Smooth[,4])^2))
      
      
      trainX[[1]]=as.data.frame(cbind(trainX[[1]],Data))
    } 
    writeLines('Input Option 10 chosen: variables consist of smoothed global variables, plus time and month (49 variables)')
  }
  
  
  else if (InputOption==11){ #smooth input data and extra features
    
    trainX=vector('list',1)
    trainX[[1]] = train.raw[[1]][,c('time','month')]
    
    for (i in 1:10){
      
      DataA=train.raw[[i]]
      
      Data = DataA[,c("U10","V10","U100","V100")]
      Data_Smooth = SmoothKM(Data)
      
      ### Training Data ### 
      Data$U10 = as.numeric(Data_Smooth[,1])
      Data$V10 = as.numeric(Data_Smooth[,2])
      
      Data$U100 = as.numeric(Data_Smooth[,3])
      Data$V100 = as.numeric(Data_Smooth[,4])
      
      Data$Amp10 = as.numeric(sqrt((Data_Smooth[,1])^2+(Data_Smooth[,2])^2))
      Data$Amp100 = as.numeric(sqrt((Data_Smooth[,3])^2+(Data_Smooth[,4])^2))
      Data$Ampratio = Data$Amp100/Data$Amp10
      # Data$Ampdiff = Data$Amp100 - Data$Amp10
      Data$logisitic = 1/(1+exp(-Data$Amp100))
      
      
      trainX[[1]]=as.data.frame(cbind(trainX[[1]],Data))
    } 
    writeLines('Input Option 11 chosen: variables consist of smoothed global variables, plus month and two engineered variables (65 variables)')
  }
  
  else if (InputOption==12){ #smooth input data and extra features
    
    trainX=vector('list',1)
    trainX[[1]] = train.raw[[1]][,c('time','month')]                    #initialize with time/month columns
    trainX[[1]] = rbind(trainX[[1]],test.raw[[1]][,c('time','month')])  #smooth train and test together
    
    trainX.local = vector('list', 1)
    testX.local = vector('list', 1)
    
    for (i in 1:10){
      
      DataA=train.raw[[i]]
      DataB = test.raw[[i]]
      
      Data = rbind(DataA[,c("U10","V10","U100","V100")],DataB[,c("U10","V10","U100","V100")] )
      Data_Smooth = SmoothKM(Data)
      
      ### Training Data ### 
      Data$U10 = as.numeric(Data_Smooth[,1])
      Data$V10 = as.numeric(Data_Smooth[,2])
      
      Data$U100 = as.numeric(Data_Smooth[,3])
      Data$V100 = as.numeric(Data_Smooth[,4])
      
      Data$Amp10 = as.numeric(sqrt((Data_Smooth[,1])^2+(Data_Smooth[,2])^2))
      Data$Amp100 = as.numeric(sqrt((Data_Smooth[,3])^2+(Data_Smooth[,4])^2))
      Data$Ampratio = Data$Amp100/Data$Amp10
      Data$logistic = 1/(1+exp(-(Data$Amp100 - 7)))
      #Data$logistic = 1/(1+exp(-(Data$Amp100 - 8)/2))  #adjusted logistic
      
      
      trainX[[1]]=data.frame(cbind(trainX[[1]],Data))
      
    
      trainX.local[[i]] = data.frame(cbind(trainX[[1]][,2],Data))
      trainX.local[[i]] = trainX.local[[i]][1:nrow(train.raw[[1]]),]
      names(trainX.local[[i]])[1] = 'month'
      
      
      testX.local[[i]] = data.frame(cbind(trainX[[1]][,2],Data))
      testX.local[[i]] = testX.local[[i]][(nrow(train.raw[[1]])+1):(nrow(train.raw[[1]])+nrow(test.raw[[1]])),]
      names(testX.local[[i]])[1] = 'month'
      
    } 
    

    
    writeLines('Input Option 12 chosen: variables consist of smoothed variables, plus month and two engineered variables (65 variables)')
  }
  
  else if (InputOption == 13){ #Option 12 w/ rejected outliers
    
    trainX = vector('list',10)
    trainX.local = vector('list', 10)
    testX.local = vector('list', 10)
    
    for (i in 1:10){
      
      trainX[[i]] = train.raw[[i]][,c('time','month')]                    #initialize with time/month columns
      trainX[[i]] = rbind(trainX[[i]],test.raw[[i]][,c('time','month')])  #smooth train and test together
      
      DataA = train.raw[[i]]
      DataB = test.raw[[i]]
      
      Data = rbind(DataA[,c("U10","V10","U100","V100")],DataB[,c("U10","V10","U100","V100")] )
      Data_Smooth = Data
      
      ### Training Data ### 
      Data$U10 = as.numeric(Data_Smooth[,1])
      Data$V10 = as.numeric(Data_Smooth[,2])
      
      Data$U100 = as.numeric(Data_Smooth[,3])
      Data$V100 = as.numeric(Data_Smooth[,4])
      
      Data$Amp10 = as.numeric(sqrt((Data_Smooth[,1])^2+(Data_Smooth[,2])^2))
      Data$Amp100 = as.numeric(sqrt((Data_Smooth[,3])^2+(Data_Smooth[,4])^2))
      Data$Ampratio = Data$Amp100/Data$Amp10
      Data$logistic = 1/(1+exp(-(Data$Amp100 - 7)))
      #Data$logistic = 1/(1+exp(-(Data$Amp100 - 8)/2))  #adjusted logistic
      
      
      trainX[[i]]=data.frame(cbind(trainX[[i]],Data))
      
      
      trainX.local[[i]] = data.frame(cbind(trainX[[i]][,2],Data))
      trainX.local[[i]] = trainX.local[[i]][1:nrow(train.raw[[1]]),]
      names(trainX.local[[i]])[1] = 'month'
      
      
      testX.local[[i]] = data.frame(cbind(trainX[[i]][,2],Data))
      testX.local[[i]] = testX.local[[i]][(nrow(train.raw[[1]])+1):(nrow(train.raw[[1]])+nrow(test.raw[[1]])),]
      names(testX.local[[i]])[1] = 'month'
      
     
    } 
    
    writeLines('Input Option 13 chosen: variables consist of smoothed variables, plus month and two engineered variables (65 variables)')
    
  }
  
  
  
  ###############################################################
  #inialize variables for unique column detection
  TempX = vector('list',1)
  for (i in 1){
    TempX[[i]] = trainX[[i]][,c(-1,-2)]
  }
  
  ## Detect Unique Columns and remove TIME
  TempX[[1]]=round(TempX[[1]],digits=7)
  TempX[[1]]=unique(t(TempX[[1]]))  # detect unique
# TempX[[1]]=t(TempX[[1]])  #dont detect unique
  TempX[[1]]=data.frame(t(TempX[[1]]))     # Data Frame
  trainX[[1]]= cbind(trainX[[1]]$month,TempX[[1]])
  names(trainX[[1]])[1] = 'month'
  
  # split trainX rows into training and test
  
  testX=vector('list',1)
  testX[[1]] = trainX[[1]][(nrow(train.raw[[1]])+1):(nrow(train.raw[[1]])+nrow(test.raw[[1]])),]
  trainX[[1]] = trainX[[1]][1:nrow(train.raw[[1]]),]
  

  ## NOTE TIME is removed 
  
  
  writeLines('Feature variables stored in trainX')
  Output=list("trainX" = trainX, "testX" = testX, 'trainX.local' = trainX.local, 'testX.local' = testX.local,
              'trainY' = trainY, 'testY'= testY)
  return(Output)
  
  
  }
