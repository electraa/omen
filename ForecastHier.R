#   Last Update: 15/04/2015 17:50
#   ForecastTS - Produce forecast model and forecasts for a timeseries
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
  # #Selected Method Optimal
  # if (Method=='Optimal'){
  #   #Calculate values for all methods
  #   MAPA <- mapa(InSample, ppy, fh = ForecastHorizon, ifh = 1, minimumAL = 1, maximumAL = 12,comb = "mean", paral = 0, display = 0, outplot = 0, hybrid = FALSE,model = "ZZZ", conf.lvl = NULL)
  #   ETS <- forecast(ets(InSample, model="ZZZ") ,h=ForecastHorizon)
  #   Theta <- ThetaClassic(NewInSample,ForecastHorizon)
  #   Naive <- forecast(naive(NewInSample ,h=ForecastHorizon))
  #   Naive$fitted[1] <-Naive$fitted[2]
  # #  ARIMA <- forecast(auto.arima(InSample, stationary=FALSE, seasonal=TRUE,ic="bic", stepwise=TRUE,  test="kpss", allowdrift=TRUE)
  # #                    ,h=ForecastHorizon)
  #   #Calculate errors for all methods
  #   criteriaMAPA=MAPA$MSE
  #   criteriaTheta=ErrorsCalculation(Theta$Model*SIdec,InSample)$MeanErrors["MSE"]
  #   criteriaETS=ErrorsCalculation(ETS$fitted,InSample)$MeanErrors["MSE"]
  #   criteriaNaive=ErrorsCalculation(Naive$fitted*SIdec,InSample)$MeanErrors["MSE"]
  # #  criteriaARIMA=ErrorsCalculation(ARIMA$fitted,InSample)$MeanErrors["MSE"]
  #   criteria=c(criteriaNaive,criteriaMAPA,criteriaTheta,criteriaETS)
  #   #Selection of the best method
  #   minimum=which.min(criteria)
  #   if (minimum==1){
  #     Method='Naive'
  #     ForecastModel <- Naive$fitted*SIdec
  #     Forecasts <- Naive$mean*SeasonalityIndexes
  #   }
  #   else if (minimum==2){
  #     Method='MAPA'
  #     ForecastModel <- MAPA$infor
  #     Forecasts <- MAPA$outfor
  #   }  
  #   else if (minimum==3){
      Method='Theta'
      ForecastModel <- Theta$Model*SIdec
      Forecasts <- Theta$Forecasts*SeasonalityIndexes
    # }
    # else if (minimum==4){
    #   Method='ETS'
    #   ForecastModel <- ETS$fitted
    #   Forecasts <- ETS$mean
    # }
    # else if (minimum==5){
    #   Method='ARIMA'
    #   ForecastModel <- ARIMA$fitted      
    #   Forecasts <- ARIMA$mean
    # }
 # }
  # #Selected Method Naive
  # else if (Method=='Naive'){   
  #   NaiveForecasts <- forecast(naive(NewInSample ,h=ForecastHorizon))
  #   NaiveForecasts$fitted[1] <-NaiveForecasts$fitted[2]
  #   ForecastModel <- NaiveForecasts$fitted*SIdec
  #   Forecasts <- NaiveForecasts$mean*SeasonalityIndexes
  # }
  # #Selected Method MAPA
  # else if (Method=='MAPA'){
  #   MAPAForecasts <- mapa(InSample, ppy, fh = ForecastHorizon, ifh = 1, minimumAL = 1, maximumAL = 12,comb = "mean", paral = 0, display = 0, outplot = 0, hybrid = FALSE,model = "ZZZ", conf.lvl = NULL)
  #   ForecastModel <- MAPAForecasts$infor
  #   Forecasts <- MAPAForecasts$outfor
  # }
  # #Selected Method ETS
  # else if (Method=='ETS'){
  #   EtsForecasts <- forecast(ets(InSample, model="ZZZ") ,h=ForecastHorizon)
  #   ForecastModel <- EtsForecasts$fitted
  #   Forecasts <- EtsForecasts$mean
  # }
  # #Selected Method ETS
  # else if (Method=='ARIMA'){
  #   ARIMAForecasts <- forecast(auto.arima(InSample, stationary=FALSE, seasonal=TRUE,ic="bic", stepwise=TRUE,  test="kpss", allowdrift=TRUE)
  #                              ,h=ForecastHorizon)
  #   ForecastModel <- ARIMAForecasts$fitted
  #   Forecasts <- ARIMAForecasts$mean
  # }
  # #Selected Method Theta
  # else{
  #   ThetaForecasts <- ThetaClassic(NewInSample,ForecastHorizon)
  #   ForecastModel <- ThetaForecasts$Model*SIdec
  #   Forecasts <- ThetaForecasts$Forecasts*SeasonalityIndexes
  # }

  Output=list("Method"=Method,"ForecastModel"=ForecastModel,"Forecasts"=Forecasts)
  return(Output)
}
