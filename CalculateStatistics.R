CalStats<-function(insample){
  
  tl <- function(x, ...){
    fit <- supsmu(1:length(x), x)
    out <- ts(cbind(trend=fit$y, remainder=x-fit$y))
    tsp(out) <- tsp(as.ts(x))
    return(structure(list(time.series=out),class="stl"))
  }
  
  #Fill NAs
  insample <- na.approx(insample)
  #Deal with negative values
  MinValue<-min(insample)
  if (MinValue<=0){
    insample<-insample+abs(MinValue)+1
  }
  
  Frequency<-frequency(insample)
  
  lambda<-BoxCox.lambda(insample, method=c("loglik"), lower=-1, upper=1) 
  insample2<-BoxCox(insample,lambda)
  
  if (Frequency>1){
    decresults<-stl(insample2,s.window = "periodic")
    Seasonality <- decresults$time.series[,1]
    Randomness <- decresults$time.series[,3]
    Trend <- decresults$time.series[,2]
  }else{
    decresults<-tl(insample2)
    Seasonality <- decresults$time.series[,1]-decresults$time.series[,1]
    Randomness <- decresults$time.series[,2]
    Trend <- decresults$time.series[,1]
  }
  
  IndSlevel<-max((1-(var(Randomness)/var(insample2-Trend))),0)
  IndTlevel<-max((1-(var(Randomness)/var(insample2-Seasonality))),0)
  IndRlevel<-spectral_entropy(insample)[1]
  
  return(c(Randomness=IndRlevel,Trend=IndTlevel,Seasonality=IndSlevel))
  
}
