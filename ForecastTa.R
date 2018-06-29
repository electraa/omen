
ForecastTa<-function(data, ForecastHorizon) {
  taggsea<-c(1,2,3,4,6,12)
  Output=list(list(),list(),list(),list(),list(),list())
  for (k in 1:6){
   Horizon<-round(ForecastHorizon/taggsea[k])
   currdata<-data[[k]]
   currdata$InSample<-ts(currdata$InSample,frequency=taggsea[7-k])
   currdata$NewInSample<-ts(currdata$NewInSample,frequency=taggsea[7-k])
   
  if (currdata$Method=='Optimal'){
    
    #Calculate values for all methods
    #must find number 30%
    Number<-length(currdata$InSample)*30%/%100
    if (Number%%12<=6){
      Number<-Number%/%12
    } else {
      Number<-Number%/%12+1
    }
    criteriatheta<-0
    criteriaETS<-0
    criteriaARIMA<-0
    criteriaHolt<-0
    criteriaSES<-0
    criteriaDamped<-0
    criteriaNaive<-0
    for (i in 1:Number){
      criteriatheta <- criteriatheta + ErrorsCalculation(ThetaClassic(head(currdata$NewInSample,length(currdata$NewInSample)-i*12),12)$Forecasts*tail(currdata$Indexes,12), tail(head(currdata$InSample,length(currdata$InSample)-(i-1)*12),12))$MeanErrors["MSE"]
      ETS <- forecast(ets(head(currdata$InSample,length(currdata$InSample)-i*12), model="ZZZ") ,h=12)
      criteriaETS <- criteriaETS + ErrorsCalculation( ETS$mean, tail(head(currdata$NewInSample,length(currdata$NewInSample)-(i-1)*12),12))$MeanErrors["MSE"]
      Naive <- forecast(naive(head(currdata$NewInSample,length(currdata$NewInSample)-i*12) ,h=12))
      Naive$fitted[1] <-Naive$fitted[2]
      criteriaNaive<-criteriaNaive + ErrorsCalculation( Naive$mean*tail(currdata$Indexes,12), tail(head(currdata$InSample,length(currdata$InSample)-(i-1)*12),12))$MeanErrors["MSE"]
      ARIMA <- forecast(auto.arima(head(currdata$InSample,length(currdata$InSample)-i*12), stationary=FALSE, seasonal=TRUE,ic="bic", stepwise=TRUE,  test="kpss", allowdrift=TRUE),h=12)
      criteriaARIMA<-criteriaARIMA + ErrorsCalculation( ARIMA$mean, tail(head(currdata$InSample,length(currdata$InSample)-(i-1)*12),12))$MeanErrors["MSE"]
      SES<-forecast(ses(head(currdata$InSample,length(currdata$InSample)-i*12),h=12))
      criteriaSES<-criteriaSES+ErrorsCalculation( SES$mean, tail(head(currdata$InSample,length(currdata$InSample)-(i-1)*12),12))$MeanErrors["MSE"]
      Holt<-forecast(holt(head(currdata$InSample,length(currdata$InSample)-i*12),h=12))
      criteriaHolt<-criteriaSES+ErrorsCalculation( Holt$mean, tail(head(currdata$InSample,length(currdata$InSample)-(i-1)*12),12))$MeanErrors["MSE"]
      Damped<-forecast(holt(head(currdata$InSample,length(currdata$InSample)-i*12),damped=TRUE, h=12))
      criteriaDamped<-criteriaDamped+ErrorsCalculation( Damped$mean, tail(head(currdata$InSample,length(currdata$InSample)-(i-1)*12),12))$MeanErrors["MSE"]
    }
    
    
    
    criteria=c(criteriaNaive,criteriatheta,criteriaETS,criteriaARIMA,criteriaSES,criteriaDamped,criteriaNaive)
    #Selection of the best method
    criteria <- criteria[!is.na(criteria)]
    minimum<-which.min(criteria)
    ETS <- forecast(ets(currdata$InSample, model="ZZZ") ,h=Horizon)
    Theta <- ThetaClassic(currdata$NewInSample,Horizon)
    Naive <- forecast(naive(currdata$NewInSample ,h=Horizon))
    Naive$fitted[1] <-Naive$fitted[2]
    ARIMA <- forecast(auto.arima(currdata$InSample, stationary=FALSE, seasonal=TRUE,ic="bic", stepwise=TRUE,  test="kpss", allowdrift=TRUE)
                      ,h=Horizon)
    SES<- forecast(ses(currdata$InSample, h = Horizon))
    Holt<- forecast(holt(currdata$InSample, damped=FALSE, h = Horizon))
    Damped<- forecast(holt(currdata$InSample, damped=TRUE, h = Horizon))
    
    if (minimum==1){
      currdata$Method='Naive'
      Naive <- forecast(naive(currdata$NewInSample ,h=Horizon))
      Naive$fitted[1] <-Naive$fitted[2]
      ForecastModel <- Naive$fitted*currdata$SIdec
      Forecasts <- Naive$mean*currdata$Indexes

    }
    else if (minimum==2){
      currdata$Method='Theta'
      ForecastModel <- Theta$Model*currdata$SIdec
      Forecasts <- Theta$Forecasts*currdata$Indexes
    }
    else if (minimum==3){
      currdata$Method='ETS'
      ForecastModel <- ETS$fitted
      Forecasts <- ETS$mean
    }
    else if (minimum==4){
      currdata$Method='ARIMA'
      ForecastModel <- ARIMA$fitted      
      Forecasts <- ARIMA$mean
    }
    else if (minimum==5){
      currdata$Method='SES'
      ForecastModel <- SES$model      
      Forecasts <- SES$mean
    }
    else if (minimum==6){
      currdata$Method='Damped'
      ForecastModel <- Damped$model      
      Forecasts <- Damped$mean
    }
    else if (minimum==7){
      currdata$Method='Holt'
      ForecastModel <- Holt$model      
      Forecasts <- Holt$mean
    }
  }
  #Selected Method Naive
  else if (currdata$Method=='Naive'){   
    NaiveForecasts <- forecast(naive(currdata$NewInSample ,h=Horizon))
    NaiveForecasts$fitted[1] <-NaiveForecasts$fitted[2]
    ForecastModel <- NaiveForecasts$fitted*currdata$SIDec
    Forecasts <- NaiveForecasts$mean*head(currdata$Indexes,Horizon)

  }
  #Selected currdata$Method ETS
  else if (currdata$Method=='ETS'){
    EtsForecasts <- forecast(ets(currdata$InSample, model="ZZZ") ,h=Horizon)
    ForecastModel <- EtsForecasts$fitted
    Forecasts <- EtsForecasts$mean
  }
  #Selected currdata$Method ETS
  else if (currdata$Method=='ARIMA'){
    ARIMAForecasts <- forecast(auto.arima(currdata$InSample, stationary=FALSE, seasonal=TRUE,ic="bic", stepwise=TRUE,  test="kpss", allowdrift=TRUE)
                               ,h=Horizon)
    ForecastModel <- ARIMAForecasts$fitted
    Forecasts <- ARIMAForecasts$mean
  }
  else if (currdata$Method=='SES'){
    SESForecasts<- forecast(ses(currdata$InSample,h = Horizon,level=c(80,95), fan=FALSE, 
                                initial=c("optimal","simple")), h = Horizon)
    
    ForecastModel <- SESForecasts$fitted
    Forecasts <- SESForecasts$mean
  }
  else if (currdata$Method=='Holt'){
    HoltForecasts<- forecast(holt(currdata$InSample, damped=FALSE), h = Horizon)
    ForecastModel <- HoltForecasts$fitted
    Forecasts <- HoltForecasts$mean
  }
  else if (currdata$Method=='Damped'){
    DampedForecasts<- forecast(holt(currdata$InSample, damped=TRUE), h = Horizon)
    ForecastModel <- DampedForecasts$fitted
    Forecasts <- DampedForecasts$mean
  }
  #Selected currdata$Method Theta
  else{
    ThetaForecasts <- ThetaClassic(currdata$NewInSample,Horizon)
    ForecastModel <- head(ThetaForecasts$Model,length(currdata$SIDec))*currdata$SIDec
    Forecasts <- ThetaForecasts$Forecasts*head(currdata$Indexes,Horizon)

  }
  Output[[k]]=list("Method"=currdata$Method,"ForecastModel"=ForecastModel,"Forecasts"=Forecasts)
  }
#  Output=list("Method"=Method,"ForecastModel"=ForecastModel,"Forecasts"=Forecasts)
  return(Output)
}
