ExpandTa<-function(datafor){
  taggsea<-c(1,2,3,4,6,12)
  forecastsout<-list(list(),list(),list(),list(),list(),list())
  forecastsout[[1]]<-datafor[[1]]
  
  for (i in 2:6){
    
    currdata<-datafor[[2]]
    ForecastsFull <- data.frame(x=numeric((length(datafor[[i]]$Forecasts)*(taggsea[i]))))
    forecastsrow<-0
    for (tarow in 1:length(datafor[[i]]$Forecasts)){
      for (j in 1:taggsea[i]){
        forecastsrow<-forecastsrow+1
        ForecastsFull$x[forecastsrow]<-currdata$Forecasts[tarow]
        
      }
    }
    ForecastModelFull <- data.frame(x=numeric(length(datafor[[i]]$ForecastModel)*(taggsea[i])))
    ForecastModelrow<-0
    for (tarow in 1:length(datafor[[i]]$ForecastModel)){
      
      for (j in 1:taggsea[i]){
        ForecastModelrow<-ForecastModelrow+1
        
        ForecastModelFull$x[ForecastModelrow]<-currdata$ForecastModel[tarow]
      }
    }
    
    forecastsout[[i]]<-list("Method"=currdata$Method,"ForecastModel"=ForecastModelFull$x,"Forecasts"=ForecastsFull$x)
  }
  forecastmodelavg=forecastsout[[1]]$ForecastModel
  forecastsavg=forecastsout[[1]]$Forecasts
  for(i in 2:6){
    forecastmodelavg<- forecastmodelavg+ head(forecastsout[[i]]$ForecastModel,length(forecastsout[[1]]$ForecastModel))
    forecastsavg<- forecastsavg+ head(forecastsout[[i]]$Forecasts, length(forecastsout[[1]]$Forecasts))
  }
  forecastmodelavg=forecastmodelavg/6
  forecastsavg=forecastsavg/6
  outputex<-list("Method"=forecastsout[[1]]$Method,"ForecastModel"=forecastmodelavg,"Forecasts"=forecastsavg)
  return(outputex)
}