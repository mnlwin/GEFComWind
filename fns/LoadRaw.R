LoadRaw <- function (Task, trainingWeek) {  
  
  ptm = proc.time()
  
  currwd = getwd()
  
  #  If submitting this week's predictions, pull data from task week, 
  #  Else temporarily change directories to prev week and pull training data length\
  
  if (task == trainingWeek){
    
    setwd(paste(currwd,"/data/",Task,sep=""))
    prevTrainLength = 1
    
  } else {
    setwd(paste(currwd,"/data/",trainingWeek,sep=""))
    
    SourceData = ReadData(trainingWeek)
    prevTrainLength = SourceData$trainLength
    rm(SourceData) 
  
    setwd(paste(currwd,"/data/",Task,sep=""))
  }

  
  # Read in data from working directory and store
  
  SourceData = ReadData(Task)
  train.raw = SourceData$TrainingData
  test.raw = SourceData$TestData
  rm(SourceData) 
  
  
  #convert timestmap to factors for month and time
  writeLines('Converting time stamps into factor variables...\n')
  train.raw = ConvertTime(train.raw)  
  test.raw = ConvertTime(test.raw)
  
  # Create variables for previous week's data: training and scoring
  
  prevTrain.raw = vector('list', 10)
  prevScoring.raw = vector('list',10)
  
  for (i in 1:10){
    prevTrain.raw[[i]] = train.raw[[i]][1:prevTrainLength,]
    prevScoring.raw[[i]] = train.raw[[i]][(prevTrainLength +1): trainLength,]
  }
  
  rawData = list('train.raw' = train.raw, 'test.raw' = test.raw, 
                 'prevTrain.raw' = prevTrain.raw, 'prevScoring.raw' = prevScoring.raw)
  #writeLines('Raw data loaded as "train.raw", "test.raw", "prevTrain.raw", and "prevScoring.raw"')
  
  setwd(currwd)
  return(rawData)
  
  
}

