#   Last Update: 22/03/2015 08:40
#   Seasonality Test, Check if a timeseries is seasonal or not
#   Requires Insample, Periods per Year, tcrit (if isn't provided = 1.645)
#   Output[[1]] -- Flag indicates if timeseries is seasonal (0,1)
#   Output[[2]] -- Deseasonalized Timeseries
#   Output[[3]] -- Seasonal Indexes

SeasonalityTest <- function(InSample, ppy, tcrit, ForecastHorizon){
  if ((ppy=='NULL')||(ppy=='Unknown'))
    ppy = findppy(InSample)
  ppy<-as.numeric(ppy)
  observations=length(InSample)
  SeasonalIndexes<-matrix(ncol=1,nrow=ppy) #seasonality indexes
  SIinsample<-matrix(ncol=1,nrow=length(InSample))
  SIinsample[1:length(InSample)]=1
  sumacf = 0
  IsSeasonal = 0
  #Check for tcrit
  if (missing(tcrit))
    tcrit = 1.645
  #Check the number of observations
  if (ppy==1){
    IsSeasonal = 0
    DesTS = InSample
    SeasonalIndexes[]=1
  }
  else {
  if (observations <= ppy){
    IsSeasonal = 0
    DesTS = InSample
    SeasonalIndexes[]=1
  }
  else{  
    acfc = acf(InSample, lag.max = ppy ,type = c("correlation"),plot = FALSE, demean = TRUE)
    for (r in 2:(ppy-1)){
      sumacf = sumacf + (acfc$acf[r+1])^2
    }
    limits = tcrit*((1 + 2*(acfc$acf[2]+sumacf))/observations)^0.5
    if (abs(acfc$acf[ppy+1])> limits){
      IsSeasonal = 1
    }
    
    if (IsSeasonal == 1){
      DecompositionSI = DecomposeC(InSample, ppy)
      DesTS = InSample/(DecompositionSI$Seasonality)
      SeasonalIndexes[] = DecompositionSI$Seasonality[1:ppy]
      SIinsample[1:length(InSample)]=DecompositionSI$Seasonality
    }
    else{
      DesTS = InSample
      SeasonalIndexes[]=1
      SIinsample[1:length(InSample)]=1
    }
  }
  
  }
  
 ## newindexes= SIinsample[(length(InSample)-ppy+1):length(InSample)]
  newindexes=SeasonalIndexes
  df1<-data.frame(x=1:((ForecastHorizon%/%ppy)*ppy),y=newindexes)  ###
  SIC<-df1
  if (ForecastHorizon%%ppy!=0){
    df2<-data.frame(x=((ForecastHorizon%/%ppy)*ppy+1):((ForecastHorizon%/%ppy)*ppy+(ForecastHorizon%%ppy)),y=head(newindexes, (ForecastHorizon%%ppy))) ###
    SIC <- as.matrix(rbind(df1, df2))
  }
  
  ind <- matrix(ncol=1,nrow=ForecastHorizon)
  for (i in 1:ForecastHorizon){
    ind[i]<-SIC[i,2]
  }  
  Output= list("IsSeasonal"=IsSeasonal,"DeseasonalizedTS"= DesTS,"SeasonalIndexes"= SeasonalIndexes,"SIinsample"= SIinsample, "SeasonalIndexesB"=ind)
  return(Output)
}