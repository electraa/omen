SeasonalTest <- function(input,ppy){
  
 # ppy<-frequency(input)
 # cat(ppy)
#  cat("------")
  tcrit<-c(1.28,1.645,1.96,2.33,2.58)
  
  if ((length(input)<3*ppy)|(ppy==1)){
    test_seasonal = FALSE
  }else{
    test_seasonal <-c()
    for (i in 1:5){
      xacf = acf(input, plot = FALSE)$acf[-1, 1, 1]
      clim = tcrit[i]/sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
      test_seasonal <- c(test_seasonal, ( abs(xacf[ppy]) > clim[ppy] ) )
    }
    
  }
  cat(test_seasonal)
  if (all(is.na(test_seasonal))==TRUE){ 
    test_seasonal <- "Non seasonal"
  }else if (length(test_seasonal)==1){ 
    test_seasonal <- "Non seasonal"
  }else{
    if (test_seasonal[5]==T){
      test_seasonal <- "Seasonal (confidence 99%)"
    }else if (test_seasonal[4]==T){
      test_seasonal <- "Seasonal (confidence 98%)"
    }else if (test_seasonal[3]==T){
      test_seasonal <- "Seasonal (confidence 95%)"
    }else if (test_seasonal[2]==T){
      test_seasonal <- "Seasonal (confidence 90%)"
    }else if (test_seasonal[1]==T){
      test_seasonal <- "Seasonal (confidence 80%)"
    }else{
      test_seasonal <- "Non seasonal"
    }
  }
  #If the function returs something greater than "Seasonal (confidence 90%)", then make autobox 1 (do decomposition)
  return(test_seasonal)
}
