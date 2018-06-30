#Removes Outliers according to the Chen and Liu approach (R package tsoutliers)
RemoveOutliers <- function(insample, observations, type1, type2, type3){
  typesin=NULL

  if (missing(type1)&&missing(type2)&&missing(type3)){
    typesin=c("AO")
  }else {
    if (type1==TRUE){
      typesin=c("AO")
      if (type2==T) typesin=cbind(typesin,"LS")
      if (type3==T) typesin=cbind(typesin,"TC")
    }else{
      if ((type2==T)&&(type3==T)) typesin=c("LS","TC")
      else if (type3==T) typesin=c("TC")
      else if (type2==T) typesin=c("LS")
      else typesin=NULL
    }
  }
Effects=NULL
if (!is.null(typesin)){
##################  Remove Outliers  ###############################################
if (observations<=50){
  cval=3
}else if (observations>=450){
  cval=4
}else{
  cval=3 + 0.0025 * (observations - 50)
}
outliers<-tso(insample, cval = cval, delta = 0.7, n.start = 50,
              types = typesin, maxit = 1, maxit.iloop = 4, 
              cval.reduce = 0.14286, remove.method = "en-masse", tsmethod = "stsm") 
if (length(outliers$effects)==0){
  Effects[1:observations]=0
}else{
  Effects=outliers$effects
}

for (i in 1:observations){
  if (is.na(Effects[i])==TRUE){
    Effects[i]=0
  }
}

outinsample=insample-Effects
outlierstype<-outliers[[1]]$type
Type=as.data.frame(merge(outliers[[1]]$type,outliers[[1]]$ind, by="row.names",all=TRUE))
TempChanges=insample
if ('TC' %in% outliers[[1]]$type){
df<- outliers[[1]][outliers[[1]]$type=="TC" ,] 
  for (i in 1:length(df[,1])){
    TempChanges[df[i,c("ind")]]<-as.numeric(insample[df[i,c("ind")]])-as.numeric(df[i,c('coefhat')])
  }}
AddOutliers<-insample
if ('AO' %in% outliers[[1]]$type){
  
df<- outliers[[1]][outliers[[1]]$type=="AO" ,] 
  for (i in 1:length(df)){
    AddOutliers[df[i,c("ind")]]<-insample[df[i,c("ind")]]-df[i,c('coefhat')]
  }}
LevShifts=insample
if ('LS' %in% outliers[[1]]$type){
LevShifts<-outinsample-AddOutliers-TempChanges+insample +insample}
colnames(Type)<-c("rn","type","value")


Output=list("Outinsample"=outinsample, "LS"=Effects, "Type"=Type, "LShifts"=LevShifts, "AOutliers"=AddOutliers, "TChanges"=TempChanges )
}else{
  Output=list("Outinsample"=insample, "LS"=Effects, "Type"=typesin, "LShifts"=insample, "AOutliers"=insample, "TChanges"=insample )
  
}
  return(Output)
}