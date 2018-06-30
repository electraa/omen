#finds the number of periods per year by testing seasonality for all possible periods
#Last Updated: 30-06-2018

findppy <- function(InSample){
  InSample=as.numeric(InSample)
  tcrit = 1.645
  ppylist<-c(1,2,3,4,6,7,12)
  tlist<-c()
  for (ppyID in 1:7){
    tlist<-c(tlist,SeasonalityTestppy(InSample, ppylist[ppyID]))
  }
  results<-data.frame(ppylist,tlist)
  results<-results[results$tlist==TRUE,]
  
  if (nrow(results)==0){
    ppy <- 1
  }else{
    ppy <- max(results$ppylist)
  }
  
  return(ppy)
}

SeasonalityTestppy <- function(input, ppy){
  tcrit = 1.645
  test_seasonal<-NA
  if ((length(input)<3*ppy)|ppy==1){
    test_seasonal = FALSE
  }else{
    xacf = acf(input, plot = FALSE)$acf[-1, 1, 1]
    clim = tcrit/sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] )
    
    if (is.na(test_seasonal)==TRUE){ test_seasonal = FALSE }
  }
  
  return(test_seasonal)
}