#   Last Update: 15/04/2015 17:50
#   ForecastTS - Produce forecast model and forecasts for a timeseries
#   Requires InSample, Periods per Year, ForecastHorizon, Method (Naive,Optimal,ETS,MAPA,Theta)
#   Output$Method -- Returns the method used for forecasting
#   Output$ForecastModel -- Returns the forecast model
#   Output$ForecastModel -- Returns the produced forecasts

Forecast<-function(InSample, NewInSample, SeasonalityIndexes, SIdec ,ppy,ForecastHorizon,Method) {

  #Selected Method Optimal
  if (Method=='Optimal'){
   
    #Calculate values for all methods
    #must find number 30%
    Number<-length(InSample)*5%/%100
    if (Number%%12<=6){
      Number<-Number%/%12
    } else {
      Number<-Number%/%12+1
    }
    criteriatheta<-0
    criteriaMAPA<-0
    criteriaETS<-0
    criteriaARIMA<-0
    criteriaHolt<-0
    criteriaSES<-0
    criteriaDamped<-0
    criteriaNaive<-0
    for (i in 1:Number){
     criteriatheta <- criteriatheta + ErrorsCalculation(ThetaClassic(head(NewInSample,length(NewInSample)-i*12),12)$Forecasts*tail(SeasonalityIndexes,12), tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
     ETS <- forecast(ets(head(InSample,length(InSample)-i*12), model="ZZZ") ,h=12)
     criteriaETS <- criteriaETS + ErrorsCalculation( ETS$mean, tail(head(NewInSample,length(NewInSample)-(i-1)*12),12))$MeanErrors["MSE"]
     criteriaMAPA<- criteriaMAPA + ErrorsCalculation( mapa(head(InSample,length(InSample)-i*12), ppy, fh = 12, ifh = 1, minimumAL = 1, maximumAL = 12,comb = "mean", paral = 0, display = 0, outplot = 0, hybrid = FALSE,model = "ZZZ", conf.lvl = NULL)$outfor, tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
     Naive <- forecast(naive(head(NewInSample,length(NewInSample)-i*12) ,h=12))
     Naive$fitted[1] <-Naive$fitted[2]
     criteriaNaive<-criteriaNaive + ErrorsCalculation( Naive$mean*tail(SeasonalityIndexes,12), tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
     ARIMA <- forecast(auto.arima(head(InSample,length(InSample)-i*12), stationary=FALSE, seasonal=TRUE,ic="bic", stepwise=TRUE,  test="kpss", allowdrift=TRUE),h=12)
     criteriaARIMA<-criteriaARIMA + ErrorsCalculation( ARIMA$mean, tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
     SES<-forecast(ses(head(InSample,length(InSample)-i*12),h=12))
     criteriaSES<-criteriaSES+ErrorsCalculation( SES$mean, tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
     Holt<-forecast(holt(head(InSample,length(InSample)-i*12),h=12))
     criteriaHolt<-criteriaSES+ErrorsCalculation( Holt$mean, tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
     Damped<-forecast(holt(head(InSample,length(InSample)-i*12),damped=TRUE, h=12))
     criteriaDamped<-criteriaDamped+ErrorsCalculation( Damped$mean, tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
     
    }
     
    
    
    criteria=c(criteriaNaive,criteriaMAPA,criteriatheta,criteriaETS,criteriaARIMA,criteriaSES,criteriaDamped,criteriaNaive)
    #Selection of the best method
    criteria <- criteria[!is.na(criteria)]
    minimum<-which.min(criteria)
    MAPA <- mapa(InSample, ppy, fh = ForecastHorizon, ifh = 1, minimumAL = 1, maximumAL = 12,comb = "mean", paral = 0, display = 0, outplot = 0, hybrid = FALSE,model = "ZZZ", conf.lvl = NULL)
    ETS <- forecast(ets(InSample, model="ZZZ") ,h=ForecastHorizon)
    Theta <- ThetaClassic(NewInSample,ForecastHorizon)
    Naive <- forecast(naive(NewInSample ,h=ForecastHorizon))
    Naive$fitted[1] <-Naive$fitted[2]
    ARIMA <- forecast(auto.arima(InSample, stationary=FALSE, seasonal=TRUE,ic="bic", stepwise=TRUE,  test="kpss", allowdrift=TRUE)
                      ,h=ForecastHorizon)
    SES<- forecast(ses(InSample, h = ForecastHorizon))
    Holt<- forecast(holt(InSample, damped=FALSE, h = ForecastHorizon))
    Damped<- forecast(holt(InSample, damped=TRUE, h = ForecastHorizon))
    
    if (minimum==1){
      Method='Naive'
      ForecastModel <- Naive$fitted*SIdec
      Forecasts <- Naive$mean*SeasonalityIndexes
    }
    else if (minimum==2){
      Method='MAPA'
      ForecastModel <- MAPA$infor
      Forecasts <- MAPA$outfor
    }  
    else if (minimum==3){
      Method='Theta'
      ForecastModel <- Theta$Model*SIdec
      Forecasts <- Theta$Forecasts*SeasonalityIndexes
    }
    else if (minimum==4){
      Method='ETS'
      ForecastModel <- ETS$fitted
      Forecasts <- ETS$mean
    }
    else if (minimum==5){
      Method='ARIMA'
      ForecastModel <- ARIMA$fitted      
      Forecasts <- ARIMA$mean
    }
    else if (minimum==6){
      Method='SES'
      ForecastModel <- SES$model      
      Forecasts <- SES$mean
    }
    else if (minimum==7){
      Method='Damped'
      ForecastModel <- Damped$model      
      Forecasts <- Damped$mean
    }
    else if (minimum==8){
      Method='Holt'
      ForecastModel <- Holt$model      
      Forecasts <- Holt$mean
    }
  }
  #Selected Method Naive
  else if (Method=='Naive'){   
    NaiveForecasts <- forecast(naive(NewInSample ,h=ForecastHorizon))
    NaiveForecasts$fitted[1] <-NaiveForecasts$fitted[2]
    ForecastModel <- NaiveForecasts$fitted*SIdec
    Forecasts <- NaiveForecasts$mean*SeasonalityIndexes
  }
  #Selected Method MAPA
  else if (Method=='MAPA'){
    MAPAForecasts <- mapa(InSample, ppy, fh = ForecastHorizon, ifh = 1, minimumAL = 1, maximumAL = 12,comb = "mean", paral = 0, display = 0, outplot = 0, hybrid = FALSE,model = "ZZZ", conf.lvl = NULL)

    ForecastModel <- MAPAForecasts$infor
    Forecasts <-MAPAForecasts$outfor
  }
  #Selected Method ETS
  else if (Method=='ETS'){
    EtsForecasts <- forecast(ets(InSample, model="ZZZ") ,h=ForecastHorizon)
    ForecastModel <- EtsForecasts$fitted
    Forecasts <- EtsForecasts$mean
  }
  #Selected Method ETS
  else if (Method=='ARIMA'){
    ARIMAForecasts <- forecast(auto.arima(InSample, stationary=FALSE, seasonal=TRUE,ic="bic", stepwise=TRUE,  test="kpss", allowdrift=TRUE)
                               ,h=ForecastHorizon)
    ForecastModel <- ARIMAForecasts$fitted
    Forecasts <- ARIMAForecasts$mean
  }
  else if (Method=='SES'){
    SESForecasts<- ses(NewInSample,h = ForecastHorizon)
   
    ForecastModel <- SESForecasts$fitted*SIdec
    Forecasts <- SESForecasts$mean*SeasonalityIndexes
  }
  else if (Method=='Holt'){
    HoltForecasts<- holt(NewInSample, damped=FALSE, h = ForecastHorizon)
    ForecastModel <- HoltForecasts$fitted*SIdec
    Forecasts <- HoltForecasts$mean*SeasonalityIndexes
  }
  else if (Method=='Damped'){
    DampedForecasts<- holt(NewInSample, damped=TRUE, h = ForecastHorizon)
    ForecastModel <- DampedForecasts$fitted*SIdec
    Forecasts <- DampedForecasts$mean*SeasonalityIndexes
  }
  #Selected Method Theta
  else{
    ThetaForecasts <- ThetaClassic(NewInSample,ForecastHorizon)
    ForecastModel <- ThetaForecasts$Model*SIdec
    Forecasts <- ThetaForecasts$Forecasts*SeasonalityIndexes
  }

  Output=list("Method"=Method,"ForecastModel"=ForecastModel,"Forecasts"=Forecasts)
  return(Output)
}
