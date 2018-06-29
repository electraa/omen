#   Last Update: 21/04/2015 17:00
#   Preparation for forecast
#   Requires InSample, Method (Naive,Optimal,ETS,MAPA,Theta)
#   Output$Method -- Returns the method used for forecasting
#   Output$DeseasonalizedInsample -- Returns the deseasonalized timeseries
#   Output$NoOutliers -- Returns the timeseries without outliers
#   Output$NoOutliersSea -- Returns the deseasonalized timeseries without outliers
#   Output$InSample -- Returns the original timeseries
#   Output$NewInSample -- Returns the deseasonalized timeseries without outliers
#   Output$SIDec -- Insample decomposition
#   Output$Indexes -- Returns the Seasonal Indexes
PreForecast<-function(InSample, Outliers, Method, Horizon, ppy,type1, type2, type3) {
  NoOutliers<-InSample
  NoOutliersSea<-InSample
  seasonal<-InSample
  NewInSample<-InSample
  SIdeco<-matrix(1,ncol=1,nrow=length(InSample))
  SeasonalityIndexes<-matrix(1,ncol=1,nrow=Horizon)  
  if (ppy=='Unknown')
    ppy='NULL'
  else 
    ppy=as.numeric(ppy)
  #Check Input per method
  if ((Method=="ETS")||(Method=="MAPA")){
    TestSeasonal<-SeasonalityTest(InSample=InSample, ppy=ppy,ForecastHorizon=Horizon)
    NewInSample<-TestSeasonal$DeseasonalizedTS
    seasonal<-TestSeasonal$DeseasonalizedTS
    SeasonalityIndexes<-TestSeasonal$SeasonalIndexesB
    SIdeco<-TestSeasonal$SIinsample


    
    #Remove Outliers
    if (Outliers==TRUE) {
      RemoveOutlier<-RemoveOutliers(insample=InSample,observations=length(InSample),type1, type2, type3)
      InSample<-RemoveOutlier[[1]]
      NoOutliers<-RemoveOutlier[[1]]
    }
  }
  else if ((Method=="Theta")||(Method=="Naive")){


    TestSeasonal<-SeasonalityTest(InSample=InSample, ppy=ppy,ForecastHorizon=Horizon)
    NewInSample<-TestSeasonal$DeseasonalizedTS
    seasonal<-TestSeasonal$DeseasonalizedTS
    SeasonalityIndexes<-TestSeasonal$SeasonalIndexesB
    SIdeco<-TestSeasonal$SIinsample

    #Remove Outliers
    if (Outliers==TRUE) {
      RemoveOutlier<-RemoveOutliers(insample=NewInSample,observations=length(NewInSample))
      NewInSample<-RemoveOutlier$Outinsample
      NoOutliers<-RemoveOutlier$Outinsample
    }
  }
  else {
    #Run Seasonality Test
    TestSeasonal<-SeasonalityTest(InSample=InSample, ppy=ppy,ForecastHorizon=Horizon)
    NewInSample<-TestSeasonal$DeseasonalizedTS
    seasonal<-TestSeasonal$DeseasonalizedTS
    SeasonalityIndexes<-TestSeasonal$SeasonalIndexesB
    SIdeco<-TestSeasonal$SIinsample
    
    #Remove Outliers
    if (Outliers==TRUE) {
      RemoveOutlierSea<-RemoveOutliers(insample=NewInSample,observations=length(NewInSample))
      NewInSample<-RemoveOutlierSea$Outinsample
      NoOutliersSea<-RemoveOutlierSea$Outinsample
      RemoveOutlier<-RemoveOutliers(insample=InSample,observations=length(InSample))
      InSample<-RemoveOutlier$Outinsample
      NoOutliers<-RemoveOutlier$Outinsample
    }
    }
  Output=list("Method"=Method,"DeseasonalizedInsample"=seasonal,"NoOutliers"=NoOutliers, "NoOutliersSea"=NoOutliersSea,"InSample"=InSample ,"NewInSample"=NewInSample, "Indexes"=SeasonalityIndexes, "SIDec"=SIdeco)
  return(Output)
  }