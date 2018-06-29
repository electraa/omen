#   Last Update: 15/04/2015 17:50
#   ForecastTS - Produce forecast model and forecasts for a timeseries
#   Requires InSample, Periods per Year, ForecastHorizon, Method (Naive,Optimal,ETS,MAPA,Theta)
#   Output$Method -- Returns the method used for forecasting
#   Output$ForecastModel -- Returns the forecast model
#   Output$ForecastModel -- Returns the produced forecasts

ForecastInter<-function(InSample ,ForecastHorizon,Method) {
  
  #Selected Method Optimal
  if (Method=='Optimal'){
    
    #Calculate values for all methods
    #must find number 30%
    Number<-length(InSample)*30%/%100
    if (Number%%12<=6){
      Number<-Number%/%12
    } else {
      Number<-Number%/%12+1
    }
    criteriaMAPA<-0
    criteriaSBA<-0
    criteriaCroston<-0
    criteriaTSB<-0
  
    for (i in 1:Number){
      criteriaMAPA <- criteriaMAPA + ErrorsCalculation(imapa(head(InSample,length(InSample)-i*12),12)$frc.out, tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
      SBA <- crost(head(InSample,length(InSample)-i*12), type="sba",h=12)
      criteriaSBA <- criteriaSBA + ErrorsCalculation( SBA$frc.out, tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
      criteriaTSB<- criteriaTSB + ErrorsCalculation(tsb(head(InSample,length(InSample)-i*12), h = 12)$frc.out, tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
      criteriaCroston<-criteriaCroston + ErrorsCalculation(crost(head(InSample,length(InSample)-i*12), type="croston",h=12)$frc.out, tail(head(InSample,length(InSample)-(i-1)*12),12))$MeanErrors["MSE"]
      
      
    }
    
    
    
    criteria=c(    criteriaMAPA,
                   criteriaSBA,
                   criteriaCroston,
                   criteriaTSB)
    #Selection of the best method
    criteria <- criteria[!is.na(criteria)]
    minimum<-which.min(criteria)
    MAPA <- imapa(InSample, h = ForecastHorizon)
    Croston <- crost(InSample, type="croston" ,h=ForecastHorizon)
    SBA <- crost(InSample, type="sba" ,h=ForecastHorizon)
    TSB <- tsb(InSample ,h=ForecastHorizon)
   
    if (minimum==1){
      Method='iMAPA'
      ForecastModel <- MAPA$frc.in
      Forecasts <- MAPA$frc.out
    }
    else if (minimum==2){
      Method='SBA'
      ForecastModel <- SBA$frc.in
      Forecasts <- SBA$frc.out
    }  
    else if (minimum==3){
      Method='Croston'
      ForecastModel <- Croston$frc.in
      Forecasts <- Croston$frc.out
    }
    else if (minimum==4){
      Method='TSB'
      ForecastModel <- TSB$frc.in
      Forecasts <- TSB$frc.out
    }
   
  }
  #Selected Method Naive
  else if (Method=='iMAPA'){   
    MAPA <- imapa(InSample, h = ForecastHorizon)
   
    ForecastModel <- MAPA$frc.in
    Forecasts <- MAPA$frc.out
  }
  #Selected Method MAPA
  else if (Method=='Croston'){
    Croston <- crost(InSample, type="croston" ,h=ForecastHorizon)
    ForecastModel <- Croston$frc.in
    Forecasts <- Croston$frc.out
  }
  #Selected Method ETS
  else if (Method=='SBA'){
    SBA <- crost(InSample, type="sba" ,h=ForecastHorizon)
    ForecastModel <- SBA$frc.in
    Forecasts <- SBA$frc.out
  }
  #Selected Method ETS
  else if (Method=='TSB'){
    TSB <- tsb(InSample ,h=ForecastHorizon)    
    ForecastModel <- TSB$frc.in
    Forecasts <- TSB$frc.out
  }
 
  
  Output=list("Method"=Method,"ForecastModel"=ForecastModel,"Forecasts"=Forecasts)
  return(Output)
}
