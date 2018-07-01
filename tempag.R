divisors <- function(x){
  #  Vector of numberes to test against
  y <- seq_len(x-1)
  #  Modulo division. If remainder is 0 that number is a divisor of x so return it
  y[ x%%y == 0 ]
}

taggregation = function(input,ppy){
  output<-matrix(0,ncol=(length(divisors(ppy))+1),nrow=length(input))
  output<-as.data.frame(output)
  colnames(output)<-c(append("data",paste0("d",divisors(ppy))))
  output$data<-input
  if("d2" %in% colnames(output))
  {
    output$d2<-output$data
    for (i in seq(from=2, to=length(input), by=2)){
      output[i,c('d2')]<-(output[i,c('d2')]+output[i-1,c('d2')])/2
      output[i-1,c('d2')]<- output[i,c('d2')]
    }
  }
  
  if("d3" %in% colnames(output))
  {
    output$d3<-output$data
    for (i in seq(from=3, to=length(input), by=3)){
      output[i,c('d3')]<-(output[i,c('d3')]+output[i-1,c('d3')]+output[i-2,c('d3')])/3
      output[i-1,c('d3')]<- output[i,c('d3')]
      output[i-2,c('d3')]<- output[i,c('d3')]
    }
  }
  if("d4" %in% colnames(output))
  {
    output$d4<-output$data
    for (i in seq(from=4, to=length(input), by=4)){
      output[i,c('d4')]<-(output[i,c('d4')]+output[i-1,c('d4')]+output[i-2,c('d4')]+output[i-3,c('d4')])/4
      output[i-1,c('d4')]<- output[i,c('d4')]
      output[i-2,c('d4')]<- output[i,c('d4')]
      output[i-3,c('d4')]<- output[i,c('d4')]
    }
  }
  if("d6" %in% colnames(output))
  {
    output$d6<-output$data
    for (i in seq(from=6, to=length(input), by=6)){
      output[i,c('d6')]<-(output[i,c('d6')]+output[i-1,c('d6')]+output[i-2,c('d6')]+output[i-3,c('d6')]+output[i-4,c('d6')]+output[i-5,c('d6')])/6
      output[i-1,c('d6')]<- output[i,c('d6')]
      output[i-2,c('d6')]<- output[i,c('d6')]
      output[i-3,c('d6')]<- output[i,c('d6')]
      output[i-4,c('d6')]<- output[i,c('d6')]
      output[i-5,c('d6')]<- output[i,c('d6')]
    }
  }
  if("d1" %in% colnames(output))
  {
    output$d1<-output$data
    
    sums <- c(rep_len(NA, ppy - 1), rowSums(embed(output$data, ppy)))
    for (i in seq(from=ppy, to=length(input), by=ppy)){
      
      for (j in 0:(ppy-1))
      output[i-j,c('d1')]<-sums[i]/ppy
    
    }
  }

  #Fix Column Names
  if("data" %in% colnames(output)) setnames(output, old = c('data'), new = c("Original Timeseries"))
  if("d1" %in% colnames(output)) setnames(output, old = c('d1'), new = c("Annual Model"))
  if("d2" %in% colnames(output)) setnames(output, old = c('d2'), new = c("2-Month Model"))
  if("d3" %in% colnames(output)) setnames(output, old = c('d3'), new = c("Trimester Model"))
  if("d4" %in% colnames(output)) setnames(output, old = c('d4'), new = c("Quarterly Model"))
  if("d6" %in% colnames(output)) setnames(output, old = c('d6'), new = c("Half-Year Model"))
  if("d12" %in% colnames(output)) setnames(output, old = c('d12'), new = c("Monthly Model"))
  #Round Decimal
  output<-round(output,2)
  return(output)
}