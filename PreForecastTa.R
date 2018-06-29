PreForecastTa<-  function(InSample, Outliers, Method, Horizon, ppy,type1, type2, type3,smoothdatain,smmethod,imain) {
  taggdata<-taggregation(InSample,ppy)
  Output=list(list(),list(),list(),list(),list(),list())
  ################################
  source("SeasonalityTest.R")

  
  
  ######TS for forecasting#####
  data<-InSample
  annual_indexes<-seq(1,length(InSample),12)
  data1 <- ts(taggdata$d1[annual_indexes],frequency=1)
  semiannual_indexes<-seq(1,length(InSample),6)
  data2 <-  ts(taggdata$d2[semiannual_indexes],frequency=1)
  triannual_indexes<-seq(1,length(InSample),4)
  data3 <-  ts(taggdata$d3[triannual_indexes],frequency=1)
  tetraannual_indexes<-seq(1,length(InSample),3)
  data4 <-  ts(taggdata$d4[tetraannual_indexes],frequency=1)
  eksannual_indexes<-seq(1,length(InSample),2)
  data6 <-  ts(taggdata$d6[eksannual_indexes],frequency=1)
  fulldata<-list(data,data6,data4,data3,data2,data1)

  taggsea<-c(1,2,3,4,6,12)
  for (j in 1:6){
    # Method="SES"
    NoOutliers<-fulldata[[j]]
    NoOutliersSea<-fulldata[[j]]
    seasonal<-fulldata[[j]]
    NewInSample<-fulldata[[j]]
    SIdeco<-matrix(1,ncol=1,nrow=length(fulldata[[j]]))
    SeasonalityIndexes<-matrix(1,ncol=1,nrow=round(Horizon/taggsea[j]))  
    #Check Input per method
    if ((Method=="ARIMA")||(Method=="SES")||(Method=="Holt")||(Method=="Damped")||(Method=="iMAPA")||(Method=="Croston")||(Method=="SBA")||(Method=="TSB")){
      TestSeasonal<-SeasonalityTest(InSample=fulldata[[j]], ppy=taggsea[7-j],ForecastHorizon=round(Horizon/taggsea[j]))
      NewInSample<-TestSeasonal$DeseasonalizedTS
      seasonal<-TestSeasonal$DeseasonalizedTS
      SeasonalityIndexes<-TestSeasonal$SeasonalIndexesB
      SIdeco<-TestSeasonal$SIinsample

      #Remove Outliers
      if (Outliers==TRUE) {
        RemoveOutlier<-RemoveOutliers(insample=ts(fulldata[[j]],taggsea[7-j]),observations=length(fulldata[[j]]),type1, type2, type3)
        InSample<-RemoveOutlier[[1]]
        NoOutliers<-RemoveOutlier[[1]]
      }
    }
    else if ((Method=="ETS")||(Method=="MAPA")||(Method=="Theta")||(Method=="Naive")){
      
      TestSeasonal<-SeasonalityTest(InSample=fulldata[[j]], ppy=taggsea[7-j],ForecastHorizon=round(Horizon/taggsea[j]))
      NewInSample<-TestSeasonal$DeseasonalizedTS
      seasonal<-TestSeasonal$DeseasonalizedTS
      SeasonalityIndexes<-TestSeasonal$SeasonalIndexesB
      SIdeco<-TestSeasonal$SIinsample
      
      #Remove Outliers
      if (Outliers==TRUE) {
        RemoveOutlier<-RemoveOutliers(insample=NewInSample,observations=length(NewInSample), type1, type2, type3)
        NewInSample<-RemoveOutlier[[1]]
        NoOutliers<-RemoveOutlier[[1]]
      }
    }
    else {
      #Run Seasonality Test
      TestSeasonal<-SeasonalityTest(InSample=fulldata[[j]], ppy=taggsea[7-j],ForecastHorizon=round(Horizon/taggsea[j]))
      NewInSample<-TestSeasonal$DeseasonalizedTS
      seasonal<-TestSeasonal$DeseasonalizedTS
      SeasonalityIndexes<-TestSeasonal$SeasonalIndexesB
      SIdeco<-TestSeasonal$SIinsample
      
      #Remove Outliers
      if (Outliers==TRUE) {
        RemoveOutlierSea<-RemoveOutliers(insample=NewInSample,observations=length(NewInSample),type1,type2,type3)
        NewInSample<-RemoveOutlierSea[[1]]
        NoOutliersSea<-RemoveOutlierSea[[1]]
        RemoveOutlier<-RemoveOutliers(InSample=InSample,observations=length(InSample))
        InSample<-RemoveOutlier[[1]]
        NoOutliers<-RemoveOutlier[[1]]
      }
    }
    
    Output[[j]]=list("Method"=Method,"DeseasonalizedInsample"=seasonal,"NoOutliers"=NoOutliers, "NoOutliersSea"=NoOutliersSea,"InSample"=fulldata[[j]] ,"NewInSample"=NewInSample, "Indexes"=SeasonalityIndexes, "SIDec"=SIdeco)
  }
  if (smoothdatain==T){
    
    Output[[1]]$InSample=smoothwmethod(Output[[1]]$InSample,smmethod,imain)
    Output[[1]]$NewInSample=smoothwmethod(ts(Output[[1]]$NewInSample ,12),smmethod,imain)
    #InSample#
    tdata<-ts(Output[[1]]$InSample,12)
    taggdata<-taggregation(tdata,ppy)
    
    #------Making New Smooth Based Temporal Aggregation while keeping Seasonal Indexes Based on original data------#
    
    annual_indexes<-seq(1,length(InSample),12)
    Output[[6]]$InSample <-ts(taggdata$d1[annual_indexes],frequency=1)
    semiannual_indexes<-seq(1,length(InSample),6)
    Output[[5]]$InSample<-ts(taggdata$d2[semiannual_indexes],frequency=1)
    triannual_indexes<-seq(1,length(InSample),4)
    Output[[4]]$InSample<-ts(taggdata$d3[triannual_indexes],frequency=1)
    tetraannual_indexes<-seq(1,length(InSample),3)
    Output[[3]]$InSample<-ts(taggdata$d4[tetraannual_indexes],frequency=1)
    eksannual_indexes<-seq(1,length(InSample),2)
    Output[[2]]$InSample <-ts(taggdata$d6[eksannual_indexes],frequency=1)
    
    

    #NEWInSample
    tdata1<-ts(Output[[1]]$NewInSample,12)
    taggdata1<-taggregation(tdata1,ppy)
    #------Making New Smooth Based Temporal Aggregation while keeping Seasonal Indexes Based on original data------#
    annual_indexes<-seq(1,length(Output[[1]]$NewInSample),12)
    Output[[6]]$NewInSample <- ts(taggdata1$d1[annual_indexes],frequency=1)
    semiannual_indexes<-seq(1,length(Output[[1]]$NewInSample),6)
    Output[[5]]$NewInSample <- ts(taggdata1$d2[semiannual_indexes],frequency=1)
    triannual_indexes<-seq(1,length(Output[[1]]$NewInSample),4)
    Output[[4]]$NewInSample <- ts(taggdata1$d3[triannual_indexes],frequency=1)
    tetraannual_indexes<-seq(1,length(Output[[1]]$NewInSample),3)
    Output[[3]]$NewInSample <- ts(taggdata1$d4[tetraannual_indexes],frequency=1)
    eksannual_indexes<-seq(1,length(Output[[1]]$NewInSample),2)
    Output[[2]]$NewInSample <- ts(taggdata1$d6[eksannual_indexes],frequency=1)
    
  }
  return(Output)
}
