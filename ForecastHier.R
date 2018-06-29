#   Last Update: 29/06/2018 16:50
#   ForecastTS - Produce forecast model and forecasts for a Hierarchical timeseries
#   Requires InSample, Periods per Year, ForecastHorizon, Method (Naive,Optimal,ETS,MAPA,Theta)
#   Output$Method -- Returns the method used for forecasting
#   Output$ForecastModel -- Returns the forecast model
#   Output$ForecastModel -- Returns the produced forecasts

ForecastHier<-function(InSample,ppy,ForecastHorizon,Method) {
  #Run Seasonality Test
  TestSeasonal<-SeasonalityTest(InSample=InSample, ppy=ppy,ForecastHorizon=ForecastHorizon)
  NewInSample<-TestSeasonal$DeseasonalizedTS
  seasonal<-TestSeasonal$DeseasonalizedTS
  SeasonalityIndexes<-TestSeasonal$SeasonalIndexesB
  SIdec<-TestSeasonal$SIinsample
  #Only Theta Forecasts are added
      Method='Theta'
      ForecastModel <- Theta$Model*SIdec
      Forecasts <- Theta$Forecasts*SeasonalityIndexes

  Output=list("Method"=Method,"ForecastModel"=ForecastModel,"Forecasts"=Forecasts)
  return(Output)
}
