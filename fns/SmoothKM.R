SmoothKM <- function(Data, setdv = 1){
  
  Data = as.ts(Data)
  Data_Smooth = NULL
  for (i in 1:ncol(Data))
    {
      Wind = Data[,i]
      n <- length(Wind)

      mod1 <- dlmModPoly(order = 1, dV = setdv, dW = 1)
      WindFilt1 <- dlmFilter(Wind, mod1)

      WindSmooth <- dlmSmooth(WindFilt1)

    
      hwid <- qnorm(0.025, lower = FALSE) *  sqrt(unlist(dlmSvd2var(WindSmooth$U.S,WindSmooth$D.S)))
      smooth <- cbind(WindSmooth$s, as.vector(WindSmooth$s) + hwid %o% c(-1, 1))
      
      
      Data_Smooth=cbind(Data_Smooth,dropFirst(smooth[,1]))
  }
  
  writeLines(paste('smoothing constant, dv = ', setdv))
  return(Data_Smooth)
}