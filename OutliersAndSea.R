#The ta/tb values displayed are the maximum available - By default we use ta=tb=0
#Calculates outliers by checking three possible options:
# (a) By fitting a forecasting model & observing unususal values
# (b) By examining the distribution of the forecasting errors
# (c) By identifying extreme values
SEA1<-function(input,ta=10,tb=25){
  
  tl <- function(x, ...){
    fit <- supsmu(1:length(x), x)
    out <- ts(cbind(trend=fit$y, remainder=x-fit$y))
    tsp(out) <- tsp(as.ts(x))
    return(structure(list(time.series=out),class="stl"))
  }
  
  if ((frequency(input)>1)&(length(input)>3*frequency(input))){
    Decf <- stats::decompose(input,type="multiplicative")
    SIndexes<-as.numeric(Decf$seasonal)
    Y <- as.numeric(input)
    D <- as.numeric(input/Decf$seasonal)
    TC <- as.numeric(Decf$trend)
    Fm <- as.numeric(forecast(ets(input, model = "ZZZ"),h=1)$fitted)
    
    notnas <- ts(na.omit(TC),frequency = 1)
    forward <- as.numeric(holt(as.numeric(notnas),h=(length(input)-length(notnas))/2)$mean)
    backward <- as.numeric(holt(as.numeric(rev(notnas)),h=(length(input)-length(notnas))/2)$mean)
    TC <- c(backward,notnas,forward)
  }else{
    Y = Y <-as.numeric(input)
    SIndexes<-rep(1,length(input))
    Fm <- as.numeric(forecast(ets(input, model = "ZZN"),h=1)$fitted)
    TC <- tl(input) ; TC <- as.numeric(TC[[1]][,1])
  }
  
  seatable <- data.frame(Y,D,TC,Fm)
  #First method - By fitting a forecasting model & observing unususal values
  seatable$R1 <- D/TC
  seatable$R2 <- D/Fm
  seatable$SEA <- F ; seatable$Adj <- Y
  for (k in 1:nrow(seatable)){
    if ( ((seatable$R1[k]>=(1.1-ta/100))|(seatable$R1[k]<=(0.9+ta/100)))&
         ((seatable$R2[k]>=(1.25-tb/100))|(seatable$R2[k]<=(0.75+ta/100))) ){
      seatable$SEA[k] <- T
      seatable$Adj[k] <- NA
    }
  }
  
  #Fill Nas
  seatable<-seatable[,c("Y","SEA","Adj")]
  seatable$row<-c(1:nrow(seatable))
  if (is.na(seatable$Adj[1])==T){ 
    temp<-na.omit(seatable)
    coefs<-coef(lm(seatable$Y~seatable$row))
    seatable$Adj[1]<-coefs[1]+coefs[2]
  }
  if (is.na(seatable$Adj[nrow(seatable)])==T){ 
    temp<-na.omit(seatable)
    coefs<-coef(lm(seatable$Y~seatable$row))
    seatable$Adj[nrow(seatable)]<-coefs[1]+coefs[2]*nrow(seatable)
  }
  seatable$Adj2<-na.approx(seatable$Adj)*SIndexes
  for (i in 1:nrow(seatable)){
    if (is.na(seatable$Adj[i])){ seatable$Adj[i]<-seatable$Adj2[i] }
  }
  seatable$Adj2<-NULL
  
  seatable<-seatable[,c("Adj","Y","SEA")]
  return(seatable)
}
 
SEA2<-function(input,ta=3){
  
  tl <- function(x, ...){
    fit <- supsmu(1:length(x), x)
    out <- ts(cbind(trend=fit$y, remainder=x-fit$y))
    tsp(out) <- tsp(as.ts(x))
    return(structure(list(time.series=out),class="stl"))
  }
  
  if ((frequency(input)>1)&(length(input)>3*frequency(input))){
    Decf <- stats::decompose(input,type="multiplicative")
    SIndexes<-as.numeric(Decf$seasonal)
    Y <- as.numeric(input)
    D <- as.numeric(input/Decf$seasonal)
    TC <- as.numeric(Decf$trend)
    Fm <- as.numeric(forecast(ets(input, model = "ZZZ"),h=1)$fitted)
    
    notnas <- ts(na.omit(TC),frequency = 1)
    forward <- as.numeric(holt(as.numeric(notnas),h=(length(input)-length(notnas))/2)$mean)
    backward <- as.numeric(holt(as.numeric(rev(notnas)),h=(length(input)-length(notnas))/2)$mean)
    TC <- c(backward,notnas,forward)
  }else{
    Y = Y <-as.numeric(input)
    SIndexes<-rep(1,length(input))
    Fm <- as.numeric(forecast(ets(input, model = "ZZN"),h=1)$fitted)
    TC <- tl(input) ; TC <- as.numeric(TC[[1]][,1])
  }
  
  seatable <- data.frame(Y,D,TC,Fm)
  #Second method - By examining the distribution of the forecasting errors
  Dbar <- mean(D,na.rm = T)
  Fbar <- mean(Fm,na.rm = T)
  stdf <- (mean((Fm-Fbar)^2))^0.5
  seatable$SEA <- F ; seatable$Adj <- Y
  for (k in 1:nrow(seatable)){
    if ( (D[k]>=(Dbar+(3-ta)*stdf))|(D[k]<=(Dbar-(3-ta)*stdf)) ){
      seatable$SEA[k] <- T
      seatable$Adj[k] <- NA
    }
  }
  
  #Fill Nas
  seatable<-seatable[,c("Y","SEA","Adj")]
  seatable$row<-c(1:nrow(seatable))
  if (is.na(seatable$Adj[1])==T){ 
    temp<-na.omit(seatable)
    coefs<-coef(lm(seatable$Y~seatable$row))
    seatable$Adj[1]<-coefs[1]+coefs[2]
  }
  if (is.na(seatable$Adj[nrow(seatable)])==T){ 
    temp<-na.omit(seatable)
    coefs<-coef(lm(seatable$Y~seatable$row))
    seatable$Adj[nrow(seatable)]<-coefs[1]+coefs[2]*nrow(seatable)
  }
  seatable$Adj2<-na.approx(seatable$Adj)*SIndexes
  for (i in 1:nrow(seatable)){
    if (is.na(seatable$Adj[i])){ seatable$Adj[i]<-seatable$Adj2[i] }
  }
  seatable$Adj2<-NULL
  seatable<-seatable[,c("Adj","Y","SEA")]
  return(seatable)
}

SEA3<-function(input,ta=5){
   
   tl <- function(x, ...){
     fit <- supsmu(1:length(x), x)
     out <- ts(cbind(trend=fit$y, remainder=x-fit$y))
     tsp(out) <- tsp(as.ts(x))
     return(structure(list(time.series=out),class="stl"))
   }
   
   if ((frequency(input)>1)&(length(input)>3*frequency(input))){
     Decf <- stats::decompose(input,type="multiplicative")
     SIndexes<-as.numeric(Decf$seasonal)
     Y <- as.numeric(input)
     D <- as.numeric(input/Decf$seasonal)
     TC <- as.numeric(Decf$trend)
     Fm <- as.numeric(forecast(ets(input, model = "ZZZ"),h=1)$fitted)
     
     notnas <- ts(na.omit(TC),frequency = 1)
     forward <- as.numeric(holt(as.numeric(notnas),h=(length(input)-length(notnas))/2)$mean)
     backward <- as.numeric(holt(as.numeric(rev(notnas)),h=(length(input)-length(notnas))/2)$mean)
     TC <- c(backward,notnas,forward)
   }else{
     Y = Y <-as.numeric(input)
     SIndexes<-rep(1,length(input))
     Fm <- as.numeric(forecast(ets(input, model = "ZZN"),h=1)$fitted)
     TC <- tl(input) ; TC <- as.numeric(TC[[1]][,1])
   }
   
   seatable <- data.frame(Y,D,TC,Fm)
   
   #Third method - By identifying extreme values
   forward <- as.numeric(holt(ts(D,frequency = 1),h=3)$mean)
   backward <- as.numeric(holt(rev(ts(D,frequency = 1)),h=3)$mean)
   D7 <- c(backward,D,forward) ; D5 <- c(backward[1:2],D,forward[1:2])
   
   Ratio<-as.numeric(na.omit(ma(D7,7)))/as.numeric(na.omit(ma(D5,5)))
   seatable$SEA <- F ; seatable$Adj <- input
   for (k in 1:nrow(seatable)){
     if ( (Ratio[k]>=(1.05-ta/100))|(Ratio[k]<=(0.95+ta/100)) ){
       seatable$SEA[k] <- T
       seatable$Adj[k] <- NA
     }
   }
   
   #Fill Nas
   seatable<-seatable[,c("Y","SEA","Adj")]
   seatable$row<-c(1:nrow(seatable))
   if (is.na(seatable$Adj[1])==T){ 
     temp<-na.omit(seatable)
     coefs<-coef(lm(seatable$Y~seatable$row))
     seatable$Adj[1]<-coefs[1]+coefs[2]
   }
   if (is.na(seatable$Adj[nrow(seatable)])==T){ 
     temp<-na.omit(seatable)
     coefs<-coef(lm(seatable$Y~seatable$row))
     seatable$Adj[nrow(seatable)]<-coefs[1]+coefs[2]*nrow(seatable)
   }
   seatable$Adj2<-na.approx(seatable$Adj)*SIndexes
   for (i in 1:nrow(seatable)){
     if (is.na(seatable$Adj[i])){ seatable$Adj[i]<-seatable$Adj2[i] }
   }
   seatable$Adj2<-NULL
   
   seatable<-seatable[,c("Adj","Y","SEA")]
   return(seatable)
 }
 
