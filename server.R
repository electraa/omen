#########Libraries##########
options(shiny.maxRequestSize=30*1024^10) 
memory.limit(size=2322332243200)
library(shiny)
library(D3TableFilter)
library(parallel)
library(smooth)
library(tis)
library(DBI)
library(RColorBrewer)
library(data.table)
library(tseries)
library(lattice)
library(mice)
library(shinydashboard)
library(rCharts)
library(forecast)
library(reshape2)
library(MAPA)
library(fracdiff)
library(zoo)
library(tsoutliers)
library(RMySQL)
library(rmarkdown)
library(tsintermittent)
library(stats)
library(tufte)
library(RJSONIO)
library(Rcpp)
library(Amelia)
library(Mcomp)
library(ForeCA)
library(dropR)
library(Rlab)
library(highcharter)
library(igraph)
library(dplyr)
library(shinyjs)
library(networkD3)
require(data.tree)
library(stringr)
library(DT)
library(pander)

####External Functions####
source("PreForecast.R")
source("OutliersAndSea.R")
source("SeasonalTest.R")
source("CalculateStatistics.R")
source("Forecast.R")
source("ForecastInter.R")
source("ForecastHier.R")
source("PreForecastSm.R")
source("ForecastSm.R")
source("PreForecastTa.R")
source("ForecastTa.R")
source("ExpandTa.R")
source("Smooth.R")
source("DecomposeC.R")
source("Outliers.R")
source("findppy.R")
source("SeasonalityTest.R")
source("ThetaClassic.R")
source("ErrorsCalculations.R")
source("smoothwmethod.R")
source("tempag.R")



`%then%` <- shiny:::`%OR%`

##########Shiny server.R##########

shinyServer(function(input, output, session) { 
  source("dbinteraction.R", local =T)
  source("Login.R", local =T)
  observeEvent(input$savefile1, {
    dbsaveinput()
  })
  
  session$onSessionEnded(function() {		
    dbDisconnectAll()		
  })
  
  observeEvent(input$helpme, {
    showModal(modalDialog(
      title = "Help",
      infotext(),
      easyClose = TRUE
    ))
  })
  observeEvent(input$aboutme, {
    showModal(modalDialog(
      title = "About Us",
      usertext(),
      easyClose = TRUE
    ))
  })
  
  dbsaveinput<- reactive({
    
    conn = dbConnect(MySQL(), user='root', password='fsu123fsu', dbname='omen', host="147.102.23.20")
    inputdata<-x()[,c("Date","QTY")]
    
    colnames(inputdata)<-c("datetime","value")
    timepoint<-1:nrow(inputdata)
    
    tsname<-paste0(USER$Username,'_',input$savefiletitle)
    
    inputdata<-(as.data.frame(cbind(inputdata,timepoint,tsname)))
    
    
    dbWriteTable(conn,'timeseriesdata',inputdata,overwrite=F, append=T,row.names = F,nrows=nrow(inputdata))
    dbDisconnectAll()
    
  })
  
  output$welcometext<-renderText({
    if (USER$Logged == TRUE)
      msg<-paste0("Welcome back, ", USER$Username, "!")
    else 
      msg<-paste0("Welcome, Guest!")
    msg
    
  })
  output$loggeduser<-renderText({
    USER$Logged
  })
  
  filetitle<-eventReactive(input$savefile1, {
    input$savefiletitle
  })
  
  output$savetitlemsg <- renderText({
    if(is.null(filetitle())) {
      text <- 'Please enter a title'
    } else if(filetitle() %in% savedtsbyusr()) {
      text <- 'Title already exists.'
    } else {
      text <- 'Saved!'
    }
    text
  })
  
  output$timeseriesoptions<-renderUI({
    if ((USER$Logged=="TRUE")&&(input$loadsaved != "None")&&(!((USER$Username=='Guest')||(USER$Username=='_Guest_'))))
      return(
        box( status="primary",width=12,title = "Save options",
             
             strong("Time series name"),
             br(),
             textInput("timeseriesnameval",label="",value=input$loadsaved),
             checkboxInput("deleteselectedts", label='Delete selected time series'),
             
             textOutput("tsoptionsmessage"),
             actionButton("savetimeseriesoptions", "Save changes")
             
        )
      )
  })
  htxt <- reactiveValues(txt="")
  updatetsname <- reactiveValues(name="")
  observeEvent(input$savetimeseriesoptions, {
    updatetsname$name=" "
  })
  observeEvent(input$savetimeseriesoptions, {
    #Check If you have to delete the ts from the database!
    if (input$deleteselectedts == T) {
      #Delete TS
      if((USER$Username=='Guest')||(USER$Username=='_Guest_')) {
        htxt$txt<-"You cannot delete this User's Timeseries!"
      } else{
      conn = dbConnect(MySQL(), user='root', password='fsu123fsu', dbname='omen', host="147.102.23.20")
      tsnamefield<-paste0(USER$Username,"_",input$loadsaved)
      rs = dbSendQuery(conn, paste0("delete from omen.timeseriesdata where tsname='",tsnamefield,"'"))
      htxt$txt<-"Successfully deleted selected time series!"
      
      }
    }
    else {
      #Rename TS
      conn = dbConnect(MySQL(), user='root', password='fsu123fsu', dbname='omen', host="147.102.23.20")
      #check if name exists
      
      tsnamefield<-paste0(USER$Username,"_",input$loadsaved)
      updatedtsnamefield<-paste0(USER$Username,"_",input$timeseriesnameval)
      if (tsnamefield!=updatedtsnamefield){
        rs = dbSendQuery(conn, paste0("select * from omen.timeseriesdata where tsname='",updatedtsnamefield,"'"))
        selected = fetch(rs, n=-1)
        if (nrow(selected)==0) {
          htxt$txt<-"Successfully renamed selected time series!"
          updatetsname$name<-" "
          rs = dbSendQuery(conn, paste0("update omen.timeseriesdata set tsname='",updatedtsnamefield,"' where tsname='",tsnamefield,"'"))
        }else{
          htxt$txt<-"Time series name already exists!"
        }
      }else{
        htxt$txt<-"No changes detected!"
      }
    }
  }) 
  
  output$tsoptionsmessage<-renderText({
    htxt$txt
  })
  
  output$userinfo<-renderUI({
    if (!is.na(USER$Username)){
      box( status="primary",width=12,
           
           strong("Username"),
           p(USER$Username), br(),
           strong("Email"),
           p(USER$Email)
           
           ,br(),br(),
           strong("Load saved time series"),
           radioButtons("loadsaved", label = (" "), savedtsbyusr())
           
      )
    }
  })
  savedtsbyusr<-reactive({
    result=c("None"="None")
    if (updatetsname$name==" ")
      updatetsname$name=""
    rs = dbSendQuery(conn, paste0("select tsname from omen.timeseriesdata ",updatetsname$name))
    df = fetch(rs, n=-1)
    df=df[!duplicated(df),]
    
    templisted<-as.data.frame(unlist(strsplit(as.character(df),"_")))
    listed<-as.matrix(templisted)
    output$testtest<-renderText({
      listed[2,1]
    })
    for (i in 1:nrow(templisted)){
      if (listed[i,1]==USER$Username){
        result=rbind(result,c("Timeseries"=listed[i+1,1]))
      }
    }
    
    result
  })
  observeEvent(input$Login, {
    updateinputpanel()
  })
  updateinputpanel<-reactive({
    
  })
  observeEvent(input$ppyin, {
    updateppy()
  })
  updateppy<-reactive({
    updateSelectInput(session, "decfr", selected = input$ppyin)
  })
  observeEvent(input$decfr, {
    decfr()
  })
  decfr<-reactive({
    updateSelectInput(session, "ppyin", selected = input$ppyin2)
  })
  
  
  observeEvent(input$considerasintermittent, {
    intermittent()
  })
  observeEvent(input$useinter, {
    forecastQTYb()
  })
  intermittent<-reactive({
    if (input$considerasintermittent==T){
      intermittent<-T
    }else{
      intermittent<-F
    }
    intermittent
  })
  
  infotext<-reactive({
    if (input$activetab=="welcome"){
      text<-HTML('<h4> Input</h4><p><img src="fileformat.png" alt="csvfileformat" align="left" style="width:50px;height:50px;">
                   &nbsp; This tab enables the user
                   &nbsp; to Login and choose a  
                   &nbsp; timeseries file from his account
                   &nbsp; or even delete a timeseries file he has previously uploaded.')
    }
    else if (input$activetab=="home"){
      text<-HTML('<h4> Input</h4><p><img src="fileformat.png" alt="csvfileformat" align="left" style="width:50px;height:50px;">
                 &nbsp; This tab enables the user
                 &nbsp; to import a time series to  
                 &nbsp; OMEN in the form of a .csv 
                 &nbsp; file. The first column of the 
                 file must contain the timestamp of the 
                 observations, while the second the data.
                 The comma separators must be selected 
                 after uploading the file in order to help
                 OMEN read it.
                 ')
    }
    else if(input$activetab=="stats"){
      text<-HTML('<h4> Statistics</h4><p><img src="area-chart.png" alt="statistics" align="left" style="width:50px;height:50px;">
                   &nbsp; In this tab a detailed view 
                   &nbsp; at the basic statistic analysis 
                   &nbsp; of the imported time series 
                   &nbsp; (mean, median, standard 
                   deviation etc.) is provided. The boxplot 
                   and the density estimates diagram
                   can be used to visualize the distribution
                   of the data, also quantified through metrics 
                   refering to the skewness, the kurtosis and the 
                   quantiles of the data. ACF and PACF
                   plots can be exploited to describe the way the 
                   observations are related per lagging
                   period. This could be useful for
                   identifying seasonal patterns, trend and randomness,
                   also quantified with relative metrics. The user can finally
                   check the data for normality and stationarity, as well as for
                   long-term dependencies (Hurst).
                     ')
    }
    else if (input$activetab=="tso"){
      text<-HTML('<h4> Outliers</h4><p><img src="linechart2.png" alt="statistics" align="left" style="width:50px;height:50px;">
                   &nbsp; Abnormalities in time series, 
                   &nbsp; such as extreme values, can    
                   &nbsp; have a significant penalty to 
                   &nbsp; forecasting performance. In this 
                   tab the detection approach of Chen and 
                   Liu is applied in order to detect outliers
                   and normalize the time series. Such    
                   outliers may be Additive Outliers ("AO"),
                   Level Shifts ("LS") or Temporary Changes 
                   ("TC"). The user selections will determine
                   which of the abovementioned types of   
                   outliers will be considered and removed
                    (if found).                           
                   If the time series is considered     
                   seasonal, the deseasonalized one will
                   be normalized instead.               
               <ul>
               <li>Chen,  C.,  Liu,  L.-M.,  1993.  
                   Joint  estimation  of  model  parameters
                   and  outlier  effects  in  time  series.
                   Journal of the American Statistical 
                   Association 88 (421), 284-297.
               </li>
               <li>Lopez de Lacalle, J., 2015. 
                   Detection of Outliers in Time Series,
                   Version:  0.6, <a href="http://jalobe.com">http://jalobe.com</a>
               </li>
               </ul>')}
    else if (input$activetab=="decompose"){
      text<-HTML('<h4> Decompose</h4><p><img src="area-chart.png" alt="statistics" align="left" style="width:50px;height:50px;">
                   &nbsp; In this tab OMEN decomposes 
                   &nbsp; the imported time series in  
                   &nbsp; order to provide a clearer 
                   &nbsp; picture of the time series  
                   components. These are the Trend, 
                   the Seasonality and the Randomness
                    (Noise) of the original data. To 
                   do so, the classical multiplicative 
                   decomposition by moving averages is 
                   applied. Additionally, in order to help
                   the user decide whether the time series
                   should be deseasonalized, seasonal ratios
                   are presented per frequency period. 
                   Overlapping seasonal ratios indicate 
                    significant seasonal patterns, and 
                    vice versa. 
               <ul>
               <li> Kendall, M., Stuart, A., 1983. 
                    The advanced theory of statistics 3, 410-414.
               </li>
               </ul>')
    }
    else if (input$activetab=="tempag"){
      text<-HTML('<h4> Temporal Aggregation</h4><p><img src="calendar.png" alt="statistics" align="left" style="width:50px;height:50px;">
                 &nbsp; Temporal aggregation is a 
                 &nbsp; method for transforming  
                 &nbsp; the original data to 
                 &nbsp; alternative time frequencies. 
                 As an example, imagine creating
                 quarterly or yearly time series by
                 aggregating a monthly one. This
                 emphasizes different time series 
                 characteristics per aggregation level,
                 such as seasonality, level and trend,
                 leading to significant improvements
                 in forecasting accuracy even when
                 simple forecasting models are used to
                 produce the individual forecast. In
                 lower aggregation levels, high 
                 frequency components like seasonality
                 are dominant, while as the aggregation
                 level increases low frequency 
                 components, such as level and trend, 
                  are becoming more clear. 
                  
                  
                 In this tab temporal aggregation is
                 performed to all possible frequencies 
                 and the individual time series are  
                 presented to highlight the hidden 
                  information mentioned above.')
    }
    else if (input$activetab=="smooth"){
      text<- HTML('<h4> Smoothing</h4><p><img src="linechart2.png" alt="statistics" align="left" style="width:50px;height:50px;">
                   &nbsp; This tab offers a list of  
                   &nbsp; methods which can be used 
                   &nbsp; to smooth the time series.
                   &nbsp; The smoothing methods will 
                   eliminate the noise and help the
                   models specify more accurately 
                   the level and the trend of the 
                   time series, leading to potentially
                   better forecasts.
                   
                   The first one computes a simple  
                   moving average of a specified order,
                   while the second one uses a non-linear
                   smoothing mechanism based on the theta 
                   transformation.
                   
                   If the time series is considered 
                   seasonal or/and abnormal, the 
                   deseasonalized or/and the normalized
                   one will be smoothed instead.
                   <ul>
                   
                   <li> Hyndman R. J., Athanasopoulos G.,
                   Bergmeir C., Cinelli  C., 
                   Khan  Y., Mayer Z., Razbash S.,
                   Schmidt D., Shaub D., Tang Y.,
                   Wang E., Zhou Z.,  2015.  
                   Forecasting  Functions  for  Time 
                   Series  and  Linear Models 
                   Description  Methods  and  tools 
                   for displaying  and  analysing
                   univariate  time  series  
                   forecastsincluding  exponential
                   smoothing  via  state  space  
                   models  and  automatic  ARIMA  
                   modelling,  Version:6.2,
                   <a href="http://github.com/robjhyndman/forecast">Forecast  Package</a>.
                   </li>
                   
                   <li> Assimakopoulos V., (1995) 
                   “A Successive Filtering
                   Technique for Identifying 
                   Long- term Trends”,
                   Journal of Forecasting, John Wiley,
                   Vol.14, pp.35-43. 
                   </li>
                   </ul>')
    }
    else if (input$activetab=="data"){
      text<-HTML('<h4>Forecasting</h4><p><img src="area-chart.png" alt="statistics" align="left" style="width:50px;height:50px;">

                 
                 
                 &nbsp; In this tab the user decides 
                 &nbsp; which forecasting methods will 
                 &nbsp; be used to predict your time 
                 &nbsp; series. Depending on your  
                 selections, this can be the original
                 time series or the one calculated 
                 after the deseasonalization, 
                 normalization and smoothing processes
                 have taken place. The choices are 
                 given below:
                 <ol>
                 
                 <li> Naïve: The simplest way to                                       
                 predict. The forecasts are 
                 equal to the last known
                 observation. It is usually used
                 as a benchmark or for predicting
                 extremely noisy time series. </li>
                 
                 <li> Exponential Smoothing (ETS): 
                 The best-fitting model from the
                 exponential smoothing family of
                 methods is selected based on the
                 minimization of an information 
                 criteria. In case deseasonalization  
                 has not been applied, seasonal 
                 models are considered.
                 
                 </li>
                 
                 <li> Simple Exponential Smoothing   
                 (SES): Exponential smoothing
                 used for stable time series.
                 </li>	  
                 <li> Holt: Exponential smoothing 
                 used for trended time series.</li>
                 <li> Damped: Exponential smoothing
                 used for damped trended time
                 series.</li>
                 <li> Theta: The theta decomposition  
                 method, well-known for winning
                 the M3-competition. 
                 </li>
                 <li> Autoregressive Integrated    
                 Moving Average models (ARIMA):
                 The best-fitting ARIMA model 
                 is selected based on the
                 minimization of an information
                 criteria. In case 
                 deseasonalization has not been
                 applied, seasonal models are
                 considered
                 </li>    
                 
                 
                 <li> Multiple Aggregation 
                 Prediction Algorithm (MAPA):
                 Another way to apply temporal
                 aggregation. The main 
                 difference between MAPA and 
                 the typical approach of temporal
                 aggregation is that instead of
                 combining the forecasts from the
                 individual time levels, forecasts
                 are decomposed to their time 
                 series components and the 
                 combination of these generates
                 the final forecast. In case of
                 deseasonalization, seasonal 
                 models are not considered.
                 </li>
                 
                 <li> Auto-forecast: All of the above
                 mentioned methods are applied on
                 the time series. Their performance
                 is evaluated and the best one
                 is chosen to predict.</li>
                 
                 </li>
                 </ol>
                 
                   ')
    }
    else if (input$activetab=="errors"){
      text<-HTML('<h4> Errors</h4><p><img src="area-chart.png" alt="statistics" align="left" style="width:50px;height:50px;">
                 &nbsp; In this tab the forecasting  
                 &nbsp; performance is assessed using 
                 &nbsp; multiple error metrics. The 
                 &nbsp; in-sample and out-sample  
                 evaluation refer to the errors
                 calculated using the training and
                 the surplus data (if any), 
                 respectively, as defined at the
                 import tab. Each error metric
                 provides different information
                 as follows:
                 <ol>
                 
                 <li> Mean Error (ME): Evaluates  
                 the model in terms of bias. 
                 Positive values indicate  
                 pessimistic forecasts, while
                 negative optimistic forecasts.
                 Its value is scale dependent. 
                 </li>    
                 
                 <li>Mean Absolute Error (MAE):
                 Evaluates the model in terms 
                 of accuracy. The lowest its  
                 value, the better the accuracy
                 of the model. Its value is 
                 scale dependent. </li>
                 
                 <li>Mean Squared Error (MSE):
                 Evaluates the model in terms
                 of accuracy. In contrast with
                 MAE, which averages the 
                 individual point errors, MSE
                 emphasizes large point errors.
                 In this respect, MSE can be 
                 used to assess the robustness 
                 of the model. Its value is  
                 scale dependent.  
                 </li>
                 
                 
                 <li>Root Mean Squared Error(RMSE):
                 It has the same use with MSE. 
                 However, its value is  
                 comparable with that of  
                 the data. 
                 </li>
                 
                 <li>Mean Percentage Error(MPE):  
                 Evaluates the model in terms 
                 of bias. Positive values indicate
                 pessimistic forecasts, while 
                 negative optimistic forecasts.
                 Its value does not depend on the
                 scale of the data and can 
                 therefore be used to assess the
                 model across multiple time series.
                 </li>
                 
                 <li>Mean Absolute Percentage Error 
                 (MAPE): Evaluates the model in
                 terms of accuracy. The lowest its
                 value, the better the accuracy 
                 of the model. Its value does not
                 depend on the scale of the data 
                 and can therefore be used to
                 assess the model across multiple
                 time series.
                 </li>    
                 
                 <li>Symmetric Mean Absolute Error
                 (sMAPE): Can be used instead of
                 MAPE for evaluating the accuracy 
                 of the models. The advantage of 
                 doing so is that sMAPE is 
                 calculated even when zero values
                 are present. Its value is 
                 limited from 0 to 200.  
                 </li>
                 
                 <li>Mean Absolute Scaled Error
                 (MAsE): This error metric 
                 compares the accuracy of the 
                 selected method with that of
                 Naïve. In this regard, MAsE is 
                 the ratio of the forecast error
                 deriving from the method whose
                 performance is to be measured 
                 divided by the error of a 
                 benchmark method. This way, the 
                 error measures the performance of 
                 the method comparing it directly  
                 to that of a simpler benchmark. 
                 </li>
                 
                 </ol>
             ')
    }
    text
  })
  usertext<-reactive({
    
    HTML('<div align="center"> <img src="vassimc.jpg" alt="vassim" align="left" style="width:55px;height:55px;">
                                    <a href="http://fsu.gr/en/professor/vassilis-assimakopoulos"><h4> Vassilis Assimakopoulos</h4></a>
           <br><br><br>
          <img src="axilleas_raptis.png" alt="arapt" align="left" style="width:50px;height:50px;">
                                    <a href="http://fsu.gr/en/postgraduate-students/evangelos-spiliotis"><h4> Achilleas  Raptis</h4></a>
          <br><br><br>
          <img src="vspil.jpg" alt="vspil" align="left" style="width:55px;height:55px;">
                                    <a href="http://fsu.gr/en/postgraduate-students/evangelos-spiliotis"><h4> Evangelos Spiliotis</h4></a> </p>
          <br><br><br>
          <img src="elec.gif" alt="eskep" align="left" style="width:55px;height:55px;">
                                    <a href="http://fsu.gr/en/undergratuate-students/electra-skepetari"><h4> Electra Skepetari</h4></a> </p>
          </div>')
    
    
  })
  
  inputcsv<-reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    inputcsv<-read.csv(inFile$datapath, header = input$header, sep=input$sep)
    inputcsv
    
  })
  
  ##########Date Input Box Fix#####
  observeEvent(x(), {
    
    data1<-x()
    colnames(data1)<-c("Date","QTY")
    data1$Date2<-gsub("-", "", data1$Date)
    data1$Date2<-gsub("\\.", "", data1$Date2)
    data1$Date2<-gsub("/", "", data1$Date2)
    if (length(data1[3, c("Date2")]>8)) {
      for (i in 1:length(data1$Date2)){
        
        data1[i, c("Date4")]<-substr(as.character(data1[i, c("Date2")]),1,6)
        
      }
    }
    
    updateDateRangeInput(session, "dates",
                         label = "Date range",
                         start = paste0(substr(as.character(data1[1, c("Date2")]),1,4),"-",substr(as.character(data1[1, c("Date2")]),5,6),"-01"),
                         end = paste0(substr(as.character(data1[length(data1$Date2), c("Date2")]),1,4),"-",substr(as.character(data1[length(data1$Date2), c("Date2")]),5,6),"-01"),
                         min = paste0(substr(as.character(data1[1, c("Date2")]),1,4),"-",substr(as.character(data1[1, c("Date2")]),5,6),"-01"),
                         max = paste0(substr(as.character(data1[length(data1$Date2), c("Date2")]),1,4),"-",substr(as.character(data1[length(data1$Date2), c("Date2")]),5,6),"-01")
                         
    )
  })
  
  
  
  ##########Data Time Filtering##########
  
  dateinput<-reactive({
    dateinput<-paste(as.character(input$dates), collapse = "U")
    date<- as.array(unlist(strsplit(dateinput, "[U]")))
    date
  })
  
  fileppy<-reactive({ ###find input ts sampling frequency based on difference in date column
    
    inputcsv<-x()
    date1<-as.character(inputcsv[3,c("Date")])
    date2<-as.character(inputcsv[4,c("Date")])
    month1<-as.numeric(substr(date1,6,7))
    year1<-as.numeric(substr(date1,1,4))
    month2<-as.numeric(substr(date2,6,7))
    year2<-as.numeric(substr(date2,1,4))
    if (month2-month1<0){
      dift<-12-month1+month2+(year2-year1-1)*12
    }
    else{
      dift<-(year2-year1)*12+month2-month1
    }
    
    
    if (dift==1) 
      fileppy=12
    else if (dift==3)
      fileppy=4
    else if (dift==4)
      fileppy=3
    else if (dift==6)
      fileppy=2
    else if (dift==2)
      fileppy=6
    else
      fileppy=1
  })
  x<-reactive({ 
    
    validate(
      need(((input$file1 != '')||(input$loadsaved != "None")) , 'Please choose a file to import') 
    )
    
    #Date Filtering
    if (((USER$Logged=="TRUE")&&(input$loadsaved == "None"))&&(!is.na(input$file1))){
      validate(
        need((ncol(inputcsv())==2) , "Your .csv file does not have enough columns.") 
      )
      
      FirstYear<-as.numeric(substr(as.character(dateinput()[1]),1,4))
      FirstMonth<-as.numeric(substr(as.character(dateinput()[1]),6,7))
      TSStartPeriod <-100*FirstYear+FirstMonth #Get first forecast
      data1<-inputcsv()
      colnames(data1)<-c("Date","QTY")
      data1$Date2<-gsub("-", "", data1$Date)
      data1$Date2<-gsub("\\.", "", data1$Date2)
      data1$Date2<-gsub("/", "", data1$Date2)
      if (length(data1[3, c("Date2")]>8)) {
        for (i in 1:length(data1$Date2)){
          
          data1[i, c("Date4")]<-substr(as.character(data1[i, c("Date2")]),1,6)
          
        }
      }
      data1
      data1$Date<-as.numeric(data1$Date4)
      
      data1 <- data1[data1$Date>=TSStartPeriod ,]  
      selected<-data1
      selected[is.na(selected)] <- 0
      selected[,c("QTY")]<-as.numeric(as.character(selected[,c("QTY")]))
      
      selected$MONTH<-selected$Date
      selected[,c("Date","QTY")]
    }else {
      tsnamefield<-paste0(USER$Username,"_",input$loadsaved)
      #tsnamefield<-paste0("electra","_","n1940")
      rs = dbSendQuery(conn, paste0("select * from omen.timeseriesdata where tsname='",tsnamefield,"'"))
      selected = fetch(rs, n=-1)
      colnames(selected)<-c("Date","QTY","timepoint","tsname","id")
      selected[,c("QTY")]<-as.numeric(as.character(selected[,c("QTY")]))
      selected<-selected[,c("Date","QTY")]
      # output$testtableee<-renderTable({
      #   selected
      # })
      selected
    }
    
  })
  xsel<-reactive({
    x()#[,c("Date","QTY")]
  })
  #####editable
  rv <- reactiveValues(cachedTbl = NULL, new = 0)
  
  output$tbl2 <- renderD3tf({
    original<-x()[,c("QTY")]
    edited<-upx()[,c("QTY")]
    jdata<-as.data.frame(cbind(original,edited))
    colnames(jdata)<-c("Original Data","Edited Data")
    #jdata$rn<-c(1:nrow(jdata))
    
    
    #temp<-jdata[1,] ; temp<-rbind(temp,jdata)
    enableEdit(session, "tbl2", c(2))
    d3tf(jdata, 
         showRowNames = TRUE,
         colNames = colnames(jdata),
         edit = "col_2");
  })
  
  
  observe({
    if(is.null(input$tbl2_edit)) return(NULL);
    edit <- input$tbl2_edit;
    isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      
      if (is.null(xupdatev$ts))
        xupdatev$ts <<- x()[,c("QTY")]
      
      to_edit = xupdatev$ts
      
      to_edit[row] = val
      
      xupdatev$ts <<- to_edit
      
      confirmEdit(session, tbl = "tbl2", row = row, col = col, id = id, value = val);
      
    })
  })
  xupdatev<-reactiveValues(ts=NULL)
  observeEvent(xupdatev$ts,{xupdate()})
  observeEvent(x(),{xupdatev$ts<<-NULL})
  xupdate<-reactive({
    res<-x()
    if (!is.null(xupdatev$ts))
      res$QTY<-xupdatev$ts
    res
    
  })
  
  upx<-reactive({
    
    if (length(xupdate())==0){
      unlisted<-x()
    }else{
      unlisted<-xupdate()
      unlisted[,2]<-as.numeric(as.character(unlisted[,2]))
      colnames(unlisted)=colnames(x())
    }
    unlisted
    
  })
  eddata<-reactive({
    original<-x()[,c("QTY")]
    edited<-upx()[,c(2)]
    jdata<-as.data.frame(cbind(original,edited))
    colnames(jdata)<-c("OriginalData","EditedData")
    jdata$rn<-c(1:nrow(jdata))
    jdata<-as.matrix(jdata)
  })
  output$inputplot<-renderChart2({
    jdata<-eddata()
    cat(typeof(as.numeric(jdata[,2])))
    h21 <- rCharts::Highcharts$new()
    h21$series(name="Imported data",data = as.numeric(jdata[,1]), type = "line")
    h21$series(name="Edited data",data=as.numeric(jdata[,2]), type="line")
    h21$title(text = " ")
    h21$exporting(enabled = F)
    h21$xAxis(categories = jdata[,3])
    
    return(h21)
  })
  
  observeEvent(input$tbl, {
    x()
  })
  ##########JudgeTab############
  
  output$sliders <- renderUI({
    if (is.null(input$horizonin) || is.na(input$horizonin)) {
      n <- 1
    } else {
      n <-input$horizonin
    }
    
    lapply(1:n, function(i) {
      
      numericInput(
        inputId = paste0("num", (i-1))
        , label = paste0("Forecast", i)
        , value = round(judgedata()$JudgementalForecasts[i],2)
        , step = 0.1
        , width=300
      )
    })
  })
  observeEvent(input$resetjudge, {
    resetjudge()
  })
  resetjudge<-reactive({
    for (i in 1:input$horizonin){
      
      updateSliderInput(session, paste0("num", (i-1)), value = NA)
      
    }
  })
  judgedata<-reactive({
    datatable<-as.data.frame(tail(na.omit(FinalTable()$fqvalueb),as.numeric(input$horizonin)))
    datatable<-cbind(datatable,datatable)
    colnames(datatable)=c("OriginalForecasts","JudgementalForecasts")
    inputs<-data.frame(lapply(1:input$horizonin, function(i) {
      input[[paste0("num", i-1)]]
    }))
    inputs<-t(inputs)
    
    for (i in 1:input$horizonin){
      if (is.na(inputs[i])){
        datatable$JudgementalForecasts[i]<-datatable$OriginalForecasts[i]
      }else{
        datatable$JudgementalForecasts[i]<-inputs[i]
        
      }
      
    }
    as.data.frame(datatable)
  })
  output$judgetable<-renderTable({
    as.data.frame(judgedata())
  })
  output$judgeplot<-renderChart2({
    
    jdata<-as.data.frame(judgedata())
    jdata$rn<-1:nrow(jdata)
    
    h19 = rCharts::Highcharts$new()
    h19$series(name="Original",data = jdata$OriginalForecasts, type = "line")
    h19$series(name="Judgemental",data=jdata$JudgementalForecasts, draggableY=T,type="line")
    h19$title(text = "")
    h19$exporting(enabled = F)
    
    return(h19)
  })
  finalforecasts<-reactive({
    if (input$usejudgemental==T){
      finalforecasts<-judgedata()$JudgementalForecasts
    }else{
      finalforecasts<-judgedata()$JudgementalForecasts
    }
  })
  
  ########Missing Data Tab##########
  IDIVal<-reactive({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- as.data.frame(upx()$QTY[upx()$Date<=FirstForecast])
    category<-idclass(insample,type=c("PKa","SBC","KHa","KH","PK"),a.in=NULL,
                      outplot=c("summary","detail","none"),plot.focus=NULL)
    round(as.numeric(category$p),2)
  })
  output$IDI <- renderValueBox({
    valueBox(
      IDIVal(), "IDI", width=12, icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$missingvaluesbox<-renderUI({
      return(box(title="Missing data page",width=12,status="primary",solidHeader=F, collapsible=TRUE,radioButtons("missingdatafilling","Choose a method to fill missing data",c("Use last non-zero value"="option1","Use linear regression"="option2", "Multivariate imputations (trend & period)"="option3", "Bootstrap EM Algorithm"="option4")),hidden(checkboxInput("considermissing","Consider missing data adjustments", value=TRUE))
      ))  
  })
  output$missingvaluestablebox<-renderUI({
    if (IDIVal()>=1) 
      return(box(title="Original & filled data",width=12,status="primary",solidHeader=F, collapsible=TRUE,
                 div(style = ' height: 1000px;overflow-x: scroll;overflow-y:scroll',tableOutput("datafills"))))
  })
  datafilling<-reactive({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    df1<- as.data.frame(upx()$QTY[upx()$Date<=FirstForecast])
    df <- as.data.frame(upx()$QTY[upx()$Date<=FirstForecast])
    
    if (input$missingdatafilling=="option1"){
      df<-na.locf(with(df, { is.na(df) <- df == 0; df }))
      rdiff<-nrow(df1)-nrow(df)+1
      df$rn<-rdiff:nrow(df1)
      df1$rn<-1:nrow(df1)
      df<-merge(df1,df, by="rn",all=F)
      colnames(df)<-c("row", "Original Timeseries", "Adjusted Timeseries")
    }else if (input$missingdatafilling=="option2"){
      df$Original<-df1[,1]
      df$row<-c(1:nrow(df))
      if (is.na(df[1,1])==T){ 
        temp<-na.omit(df)
        coefs<-coef(lm(temp$Original~temp$row))
        df[1,1]<-coefs[1]+coefs[2]
      }
      df[,1]<-na.approx(df[,1])
      colnames(df)<-c("Adjusted Timeseries", "Original Timeseries", "row")
    }else if (input$missingdatafilling=="option3"){
      completed<-df ; colnames(completed)<-"x"
      completed$seq<-c(1:nrow(df))
      completed$fqc<-head(rep(1:frequency(df[,1]),nrow(df)),nrow(df))
      completed<-mice(completed, m = 15, maxit = 5, diagnostics = TRUE, printFlag = FALSE, seed = 1)
      completed<-complete(completed)$x
      df<-as.data.frame(completed)
      df$Original<-df1[,1]
      df$row<-c(1:nrow(df))
      colnames(df)<-c("Adjusted Timeseries", "Original Timeseries", "row")
    }else{
      
      df[df == 0] <- NA
      countna=sum(is.na(df))
      dt<-df
      #create datatable t-s / timestamp
      df$rn<-1:nrow(df)
      #We create 6 bootstraps
      if (countna>0){
        out<-amelia(df,m=6,p2s=0,polytime=2,ts="rn")
        #outf would be our new t-s
        outf<-(out$imputations$imp1[,1]+out$imputations$imp2[,1]+out$imputations$imp3[,1]+
                 out$imputations$imp4[,1]+out$imputations$imp5[,1]+out$imputations$imp6[,1])/6
        df=cbind(df1,outf)
      }else{
        df=cbind(df1,df1)
      }
      colnames(df)<-c("Original Timeseries", "Adjusted Timeseries")
    }
    df[,c("Original Timeseries", "Adjusted Timeseries")]
  })
  output$datafills<-renderTable({
    as.data.frame(datafilling())
  })
  output$missingvaluesplot<-renderUI({
    if (IDIVal()>=1) 
      return(box(title="Original & filled time series",width=12,status="primary",solidHeader=T, collapsible=TRUE,showOutput("missvalplot", "highcharts"),
                 HTML('<style>.rChart {width: 100%; height: 600px}</style>')))
  })
  output$missvalplot<-renderChart2({
    df<-datafilling()
    df$Observations<-1:nrow(df)
    dfmelt<-melt(df,id="Observations")
    h23 = hPlot( x='Observations', y = "value", group = "variable", data = dfmelt, type = "line")
    h23$chart(zoomType = "xy")
    h23$colors('rgba(63, 127, 191, 1)', 'rgba(63, 63, 191, 0.5)','rgba(36, 141, 183, 0.84)', 'rgba(52, 166, 136,1)', 'rgba(87, 100, 254, 0.6)' , 'rgba(25,25,112, 0.5)')
    return (h23)
  })
  ########Stats Tab##########
  output$Minx <- renderValueBox({
    
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    minx<-round(min(insample,na.rm = T),2)
    
    valueBox(
      minx, "Min", icon = icon("bar-chart"),
      color = "blue"
    )
  })
  output$MeanValue <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    
    valueBox(
      round(mean(insample, na.rm=T),2), "Mean", icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$Maxx <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    maxx<-round(max(insample,na.rm = T),2)
    
    valueBox(
      maxx, "Max", icon = icon("bar-chart"),
      color = "blue"
    )
  })
  output$Quantile25 <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    q25<-round(boxplot(insample)$stats[2],2)
    
    valueBox(
      q25, "1st quantile", icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$MedianValue <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    
    valueBox(
      round(median(insample, na.rm=T),2), "Median", icon = icon("bar-chart"),
      color = "blue"
    )
  })
  output$Quantile75 <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    q75<-round(boxplot(insample,plot=F)$stats[4],2)
    
    valueBox(
      q75, "3rd quantile", icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$SdValue <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    
    valueBox(
      round(sd(insample, na.rm=T),2), "Standard deviation", icon = icon("bar-chart"),
      color = "blue"
    )
  })
  output$Skewness <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    s <- sd(insample,na.rm=TRUE)
    xbar <- mean(insample,na.rm=TRUE)
    sk <- round(abs(mean((insample-xbar)^3,na.rm=TRUE)/s^3),2)
    
    valueBox(
      sk, "Skewness", icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$Kurtosis <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    s <- sd(insample,na.rm=TRUE)
    xbar <- mean(insample,na.rm=TRUE)
    k <- round(mean((insample-xbar)^4,na.rm=TRUE)/s^4,2)
    
    valueBox(
      k, "Kurtosis", icon = icon("bar-chart"),
      color = "blue"
    )
  })
  output$CVValue <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    
    valueBox(
      paste0(round(((sd(insample, na.rm=T)/mean(insample, na.rm=T))*100),2),"%"), "Coefficient of variation", icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$RValue <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- upx()$QTY[upx()$Date<=FirstForecast]
    
    no_nas<-na.omit(cbind(c(1:length(insample)),insample))
    valueBox(
      round(cor(no_nas[,1],no_nas[,2]),2), "Linear correlation coefficient", icon = icon("bar-chart"),
      color = "blue"
    )
  })
  output$Obs <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    
    valueBox(
      length(insample), "Observations", icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$Hurst <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    H <- round(fracdiff(na.contiguous(insample),0,0)[["d"]] + 0.5,2)
    valueBox(
      H, "Hurst", icon = icon("bar-chart"),
      color = "blue"
    )
  })
  output$Nonlinearity <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    NL<-round(terasvirta.test(na.contiguous(insample))[["statistic"]],2)
    valueBox(
      NL, "Non-linearity", icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$Stationarity <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    Stationarity<-Box.test(insample,lag=min(20,length(insample)-1),type="Ljung-Box")$p.value
    #Box.test:Large p-values are indicative of stationarity, and small p-values suggest non-stationarity 
    #adf.test(insample, alternative = "stationary")$p.value
    #The null-hypothesis for an ADF test is that the data are non-stationary
    #Large p-values are indicative of non-stationarity, and small p-values suggest stationarity 
    if (Stationarity>0.05){
      Stationarity<-"Yes"
    }else{
      Stationarity<-"No"
    }
    
    valueBox(
      Stationarity, "Stationarity", icon = icon("bar-chart"),
      color = "blue"
    )
  })
  output$Normality <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    #deal with NAs
    df=df1<-as.data.frame(insample)
    df$Original<-df1[,1]
    df$row<-c(1:nrow(df))
    if (is.na(df[1,1])==T){ 
      temp<-na.omit(df)
      coefs<-coef(lm(temp$Original~temp$row))
      df[1,1]<-coefs[1]+coefs[2]
    }
    insample<-na.approx(df[,1])
    normality<-shapiro.test(head(insample,500))$p.value
    if (normality<0.05){
      normality<-"Yes"
    }else{
      normality<-"No"
    }
    
    valueBox(
      normality, "Normality", icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$RandomnessI <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    Randomness<-round(CalStats(insample)[1]*100,2)
    valueBox(
      Randomness, "Randomness intensity (%)", icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$TrendI <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    Trend<-round(CalStats(insample)[2]*100,2)
    valueBox(
      Trend, "Trend intensity (%)", icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  output$SeasonalI <- renderValueBox({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    Seasonal<-round(CalStats(insample)[3]*100,2)
    valueBox(
      Seasonal, "Seasonal intensity (%)", icon = icon("bar-chart"),
      color = "blue"
    )
  })
  
  output$acfdata<-renderTable({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    data<-as.data.frame(acf(insample,na.action=na.pass, lag.max = max(fileppy(),12) )$acf)
    colnames(data)<-"Auto Correlation Function"
    data$Lag<-as.character(round(as.numeric(row.names(data))-1,0))
    data<-data[-1,]
    data<-data[,c("Lag","Auto Correlation Function")]
    data$pacf<-pacf(insample,na.action=na.pass)$acf[1:nrow(data)]
    colnames(data)[3]<-"Partial Auto Correlation Function"
    data
  }, 
  include.rownames=FALSE)
  output$acfplot<-renderChart2({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    data<-as.data.frame(acf(insample,na.action=na.pass, lag.max = max(fileppy(),12))$acf)
    colnames(data)<-"Auto Correlation Function"
    data$Lag<-as.numeric(row.names(data))-1
    data<-data[-c(1), ]
    h7 = hPlot(y="Auto Correlation Function",x="Lag",data = data, type = "column")
    
    return (h7)
  })
  output$pacfplot<-renderChart2({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    data<-as.data.frame(pacf(insample,na.action=na.pass, lag.max = max(fileppy(),12))$acf)
    colnames(data)<-"Partial Auto Correlation Function"
    data$Lag<-as.numeric(row.names(data))
    h8 = hPlot(y="Partial Auto Correlation Function",x="Lag",data = data, type = "column")
    
    return (h8)
  })
  boxplotdata<-reactive({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    datab<-data.frame(r = integer(length(upx()$QTY[upx()$Date<=FirstForecast])),
                      val = numeric(length(upx()$QTY[upx()$Date<=FirstForecast])))
    datab$val<-upx()$QTY[upx()$Date<=FirstForecast]
    datab$r<-1
    datab
  })
  output$boxplot<-renderChart2({
    
    datab<-boxplotdata()
    bwstats = setNames(as.data.frame(boxplot(val ~ r, data = datab, plot = F)$stats), 
                       nm = NULL)
    h9 = Highcharts$new()
    h9$set(series = list(list(name = "Data", data = bwstats)))
    h9$xAxis(categories = levels(datab$val), title = list(text = "Data"))
    h9$yAxis(title = list(text = "Value"))
    h9$chart(type = "boxplot")
    return(h9)
    output$statsprogress<-renderText({100})
    
  })
  output$densityplot<-renderChart2({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())
    dnst<-density(insample, na.rm = T)
    x<-dnst$x ; y<-dnst$y/sum(dnst$y)
    nframe<-data.frame(x,y)
    colnames(nframe)<-c("Value","Probability")
    h10<-hPlot(y="Probability",x="Value",data = nframe, type = "line")
    return(h10)
  })
  ######End Of Stats Tab#########
  
  ######Forecasts######
  
  forecastQTY<- reactive({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    ######frequency#####
    if (input$ppyin=='Unknown') {
      tsfrequency<-fileppy()
    }
    else {
      tsfrequency<-as.numeric(as.character(input$ppyin))
    }
    
    if (input$considermissing==T)
      insample <- ts(datafilling()[,c("Adjusted Timeseries")], frequency=fileppy())
    else
      insample<-ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())

    ##Intermittent demand or Continuous Demand?##
    if (intermittent()==F){
      if (input$tain==T){
        datamul<-PreForecastTa(InSample=insample, Outliers=input$removeoutliersdec, Method=input$methodin, Horizon=input$horizonin, ppy=input$ppyin,type1=input$ao, type2=input$ls, type3=input$tc, smoothdatain=input$considersmoothing,smmethod=input$smoothmethodin,imain=input$imain ) 
        data<-datamul[[1]]
        forecastqty1<-ForecastTa(datamul,input$horizonin)   
        output$testtable<-renderText({datamul[[2]]$NewInSample})
        forecastqty<-ExpandTa(forecastqty1)
      }else if (input$considersmoothing==T){
        #get forecast arguments
        data<-PreForecastsm(InSample=insample, Outliers=input$removeoutliersdec, Method=input$methodin, Horizon=input$horizonin, ppy=input$ppyin,type1=input$ao, type2=input$ls, type3=input$tc) 
        #smoothing
        data$InSample=smoothwmethod(data$InSample,input$smoothmethodin,input$imain)
        data$NewInSample=smoothwmethod(data$NewInSample,input$smoothmethodin,input$imain)
        #forecast
        forecastqty<-Forecastsm(InSample=data$InSample, NewInSample=data$NewInSample, SeasonalityIndexes=data$Indexes, SIdec=data$SIDec ,ppy=fileppy(),ForecastHorizon=input$horizonin,Method=input$methodin)
      }else{
        #get forecast arguments
        data<-PreForecast(InSample=insample, Outliers=F, Method=input$methodin, Horizon=input$horizonin, ppy=input$ppyin,type1=input$ao, type2=input$ls, type3=input$tc) 
        #forecast 
        forecastqty<-Forecast(InSample=data$InSample, NewInSample=data$NewInSample, SeasonalityIndexes=data$Indexes, SIdec=data$SIDec ,ppy=fileppy(),ForecastHorizon=input$horizonin,Method=input$methodin)
      }
      if ((input$removeoutliersdec==TRUE)&&(input$methodin=='Optimal')&&((forecastqty[[1]]=='Naive')||(forecastqty[[1]]=='Theta'))) data$NoOutliers<-data$NoOutliersSea
      
      
      quantity_forecast_final=c(forecastqty[[2]],forecastqty[[3]])
      return(list("forecast"=quantity_forecast_final, "method"=forecastqty[[1]], "NoOutliers"=data$NoOutliers, "Deseasonalized"=data$DeseasonalizedInsample))
    }
    else if ((intermittent()==T)||(input$useinter==T)){
      insample <- as.data.frame(upx()$QTY[upx()$Date<=FirstForecast])
      #get forecast arguments
      data<-PreForecast(InSample=insample, Outliers=F, Method=input$methodin2, Horizon=input$horizonin2, ppy=1,type1=input$ao, type2=input$ls, type3=input$tc) 
      #forecast 
      forecastqty<-ForecastInter(InSample=data$InSample,ForecastHorizon=input$horizonin2,Method=input$methodin2)
      if ((input$removeoutliersdec2==TRUE)&&(input$methodin2=='Optimal')&&((forecastqty[[1]]=='Naive')||(forecastqty[[1]]=='Theta'))) data$NoOutliers<-data$NoOutliersSea
      
      
      quantity_forecast_final=c(forecastqty[[2]],forecastqty[[3]])
      return(list("forecast"=round(quantity_forecast_final,2), "method"=forecastqty[[1]], "NoOutliers"=data$NoOutliers, "Deseasonalized"=data$DeseasonalizedInsample))
    }
    
  })
  
  forecastQTYb <- reactive({
    quantity_forecast_final<-forecastQTY()$forecast
    return(quantity_forecast_final)
  })
  forecastQTYc<-reactive({
    OutliersSelection()[[1]]
  })
  forecastQTYd<-reactive({
    insampledes<-forecastQTY()$Deseasonalized
    insampledes
  })
  forecastQTYe<-reactive({
    
    ######frequency######
    FirstForecast <- paste(dateinput()[2]) #Get first forecast
    if (input$ppyin=='Unknown') {
      tsfrequency<-findppy(upx()$QTY)
    }
    else {
      tsfrequency<-as.numeric(as.character(input$ppyin))
    }
    data<-data.frame(y=1:input$horizonin)
    insample <- ts(upx()$QTY,frequency=fileppy()) #Get user selected data
    data$forecasts<-tail(forecastQTYb(),as.numeric(input$horizonin))
    data$AA <- as.numeric(rownames(data))
    errorqty1a<-ErrorsCalculation(tail(FinalTable()$fqvalue,input$horizonin), tail(FinalTable()$qvalue,input$horizonin))
    errorqty<-t(errorqty1a[[2]])
    
    plotin<-input$plotin
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstYearData<-as.numeric(substr(as.character(dateinput()[1]),1,4))
    FirstMonthData<-as.numeric(substr(as.character(dateinput()[1]),6,7))
    yd<-12*(FirstYear-FirstYearData-1)+12-FirstMonthData+FirstMonth    
    MSE<-ErrorsCalculation(head(FinalTable()$fqvalue, yd), head(FinalTable()$qvalue, yd))$MeanErrors["MSE"]
    observations<-as.numeric(length(insample))
    data$CL1<-data$forecasts+as.numeric(as.character(input$clin))*sqrt(data$AA)*sqrt(MSE)
    data$CL2<-data$forecasts-as.numeric(as.character(input$clin))*sqrt(data$AA)*sqrt(MSE)
    
    as.data.frame(data)
  })
  
  ###End Of Forecasts###
  
  FinalTable <-reactive({
    
    TSSY<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    TSSM<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstYear<-as.numeric(substr(as.character(dateinput()[1]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[1]),6,7))
    
    
    ####fixing ending date###
    
    if (input$horizonin%%12+TSSM>12){
      TSEY<-TSSY+input$horizonin%/%12+1
      TSEM<-input$horizonin%%12+TSSM-12
    }
    else
    {
      TSEY<-TSSY+input$horizonin%/%12
      TSEM<-input$horizonin%%12+TSSM
    }
    ##########
    
    originaldataqtyuncut<-ts(upx()$QTY,frequency=fileppy(),start = c(FirstYear, FirstMonth))
    originaldataqtyuncut.m<- data.frame(yr=round(time(originaldataqtyuncut), digits = 2),qvalueuncut=as.matrix(originaldataqtyuncut)) 
    originaldataqty <- ts(upx()$QTY,frequency=fileppy(),start = c(FirstYear, FirstMonth), end = c(TSSY,TSSM)) #Get user selected data
    originaldataqty.m<- data.frame(yr=round(time(originaldataqty), digits = 2),qvalue=as.matrix(originaldataqty)) 
     if (!((intermittent()==T)||(input$useinter==T))){
    deseasonalizedqty<-ts(forecastQTYd(),frequency=fileppy(),start = c(as.numeric(FirstYear), as.numeric(FirstMonth)),end = c(TSSY,TSSM))
    deseasonalizedqty.m<- data.frame(yr=round(time(deseasonalizedqty), digits = 2),qvaluec=as.matrix(deseasonalizedqty))
}
    forecastqty<-ts(forecastQTYb(),frequency=fileppy(),start = c(as.numeric(FirstYear), as.numeric(FirstMonth)))
    forecastqty.m<- data.frame(yr=round(time(forecastqty), digits = 2),fqvalue=as.matrix(forecastqty))
    
    qtydata<-merge(originaldataqty.m,forecastqty.m, by = "yr", all=TRUE)
    
    #forecastqty<- ts(tail(forecastQTYb(),input$horizonin),frequency=fileppy(), start = c(as.numeric(FirstYear), as.numeric(FirstMonth)), end=c(TSEY,TSEM))
    
    qtydata<-merge(qtydata,originaldataqtyuncut.m,by = "yr", all=TRUE)
    #qtydata<-merge(qtydata,fforecasts.m,by = "yr", all=TRUE)
    
    
    if (input$removeoutliersdec==TRUE) {  
      outliersdataqty<-ts(forecastQTYc(),frequency=fileppy(),start = c(as.numeric(FirstYear), as.numeric(FirstMonth)))
      outliersdataqty.m<- data.frame(yr=round(time(outliersdataqty), digits = 2),qvalueb=as.matrix(outliersdataqty))
      qtydata<-merge(qtydata,outliersdataqty.m,by = "yr", all=TRUE)
    }
    if (!((intermittent()==T)||(input$useinter==T)))
    qtydata<-merge(qtydata,deseasonalizedqty.m,by = "yr", all=TRUE)
    
    #dokimi
    forecastqtyb<- window(forecastqty, start = c(as.numeric(FirstYear), as.numeric(FirstMonth)), frequency=fileppy())
    #
    #sti thesi tou katw
    #forecastqtyb<- window(forecastqty, start =c(TSSY,TSSM), end = c(TSEY,TSEM), frequency=fileppy())
    fqvalueb.m<- data.frame(yr=round(time(forecastqtyb), digits = 2),fqvalueb=as.matrix(forecastqtyb))
    qtydata<-merge(qtydata,fqvalueb.m,by = "yr", all=TRUE)
    
    
    inputcsv<-upx()
    
    date1<-as.character(inputcsv[3,c("Date")])
    date2<-as.character(inputcsv[4,c("Date")])
    month1<-as.numeric(substr(date1,6,7))
    year1<-as.numeric(substr(date1,1,4))
    month2<-as.numeric(substr(date2,6,7))
    year2<-as.numeric(substr(date2,1,4))
    if (month2-month1<0){
      dift<-12-month1+month2+(year2-year1-1)*12
    }
    else{
      dift<-(year2-year1)*12+month2-month1
    }
    months<-" months"
    seqby<-paste0(abs(dift),months)
    #########
    
    
    finaldata<-qtydata
    Date <- as.character(format(seq(as.Date(paste0(as.character(FirstYear),"-",as.character(FirstMonth),"-01")), by=seqby, length.out=nrow(finaldata)), format="%m-%Y"))
    DatePlot <- as.numeric(as.POSIXct(seq(as.Date(paste0(as.character(FirstYear),"-",as.character(FirstMonth),"-01")), by=seqby, length.out=nrow(finaldata))))*1000.01 
    finaldata<-cbind(qtydata,Date,DatePlot)
    finaldata
  })
  output$OutputTestForecast<-renderPrint({
    
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    ######frequency#####
      tsfrequency<-fileppy()
    
    tsfrequency

    if (input$considermissing==T)
      insample <- ts(datafilling()[,c("Adjusted Timeseries")], frequency=fileppy())
    else
      insample<-ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=fileppy())

insample
  })
  OutputTable<-reactive({
    datatable<-as.data.frame(FinalTable())
    colnames(datatable)[colnames(datatable)=="Date"] <- "Timestamp"
    display<-datatable[,c( "Timestamp")]
    
    colnames(datatable)[colnames(datatable)=="qvalueuncut"] <- "Value"
    colnames(datatable)[colnames(datatable)=="fqvalueb"] <- "Value Forecast"
    display<-cbind(display,datatable[,c( "Value","Value Forecast")])
    
    display <- cbind(AA = rownames(display), display)
    colnames(display)[colnames(display)=="AA"] <- "ID"
    colnames(display)[colnames(display)=="display"] <- "Timestamp"
    display
  })  
  output$table <- renderTable({
    data<-OutputTable()
    rownames(data) <- NULL
    data},include.rownames=FALSE)
  output$table2 <- renderTable({
    data<-OutputTable()
    rownames(data) <- NULL
    data},include.rownames=FALSE)
  
  #######Outliers Tab#######
  
  OutliersSelection<- reactive({
    
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    if ((input$considermissing==T)&&(length(input$considermissing)>0))
      insample <- ts(datafilling()[,c("Adjusted Timeseries")], frequency=fileppy())
    else
      insample<-ts(upx()$QTY[upx()$Date<=FirstForecast],start=c(FirstYear,FirstMonth), frequency=fileppy())
    
    
    if (input$outliersmethodin=="tsoutliers"){
      RemoveOutlier<-RemoveOutliers(insample,observations=length(insample),type1=input$ao, type2=input$ls, type3=input$tc)
      NoOutliers<-RemoveOutlier#$Outinsample
    }else if (input$outliersmethodin=="sea1") {
      NoOutliers<-SEA1(insample,ta=input$sea1a,tb=input$sea1b)
    }else if (input$outliersmethodin=="sea2") {
      NoOutliers<-SEA2(insample,ta=input$sea2a)
    }else{
      NoOutliers<-SEA3(insample,ta=input$sea3a)
    }
    NoOutliers
  })
  OutliersTable1<-reactive({
    
    data2<-as.data.frame(as.numeric(OutliersSelection()[[1]])) 
    colnames(data2)<-"outliers"
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    if ((input$considermissing==T)&&(length(input$considermissing)>0))
      insample <- datafilling()[,c("Adjusted Timeseries")]
    else
      insample<-ts(upx()$QTY[upx()$Date<=FirstForecast],start=c(FirstYear,FirstMonth), frequency=fileppy())
    
    data2$originaldata<-insample
    data2$index<-1:nrow(data2)
    datatable<-as.data.frame(data2)
    colnames(datatable)[colnames(datatable)=="outliers"] <- "Adjusted Timeseries"
    colnames(datatable)[colnames(datatable)=="originaldata"] <- "Original Timeseries"
    datatable<-datatable[,c("Original Timeseries","Adjusted Timeseries")]
    datatable
    
  })
  output$OutliersTable<-renderTable({
    OutliersTable1()
  })
  output$OutliersPlot<- renderChart2({
    data2<-as.data.frame(as.numeric(OutliersSelection()[[1]])) 
    colnames(data2)<-"outliers"
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    cat(data2$outliers)
    cat(length(data2$outliers))
    #insample <- head(upx()$QTY, length(OutliersSelection()[[1]]))
    if ((input$considermissing==T)&&(length(input$considermissing)>0))
      insample <- datafilling()[,c("Adjusted Timeseries")]
    else
      insample<-ts(upx()$QTY[upx()$Date<=FirstForecast],start=c(FirstYear,FirstMonth), frequency=fileppy())
    t=T
    if (input$outliersmethodin=="tsoutliers"){
      if (length(OutliersSelection()[[4]])<length(insample))
      data2$outliers<-insample
      t=F
    }
    cat(t)
    cat(insample)
    data2$originaldata<-insample
    colnames(data2)[which(names(data2) == "outliers")] <- "Adjusted Timeseries"
    colnames(data2)[which(names(data2) == "originaldata")] <- "Original Timeseries"
    
    if ((input$outliersmethodin=="tsoutliers")&&(t)){
      data2$TC<-OutliersSelection()[[6]]
      data2$AO<-OutliersSelection()[[5]]
      data2$LS<-OutliersSelection()[[4]]
      typeframe<-OutliersSelection()[[3]]
      typeframe$rn<-1:nrow(typeframe)
      otype<-dcast(typeframe, rn ~ type, value.var="value")
      colnames(data2)[which(names(data2) == "Adjusted Timeseries")] <- "E. Adjusted Timeseries"
      colnames(data2)[which(names(data2) == "Original Timeseries")] <- "A. Original Timeseries"
      colnames(data2)[which(names(data2) == "LS")] <- "B. Level Shifts"
      colnames(data2)[which(names(data2) == "TC")] <- "C. Temporary Changes"
      colnames(data2)[which(names(data2) == "AO")] <- "D. Additive Outliers"
    }else{
      colnames(data2)[which(names(data2) == "Adjusted Timeseries")] <- "B. Adjusted Timeseries"
      colnames(data2)[which(names(data2) == "Original Timeseries")] <- "A. Original Timeseries"
    }
    data2$index<-1:nrow(data2)
    
    
    datamelt<-melt(data2,id="index")
    h11 = hPlot(x = "index", y = "value",group = "variable", data = datamelt, type = "line")
    # h11$chart(height=500, width=700)
    
    h11$params$width <- 800
    # h11$title(text = "Outliers")
    h11$exporting(enabled = F)
    h11$chart(zoomType = "xy")
    h11$yAxis(title="")
    h11$colors( 'rgba(48, 43, 43, 1)', 'rgba(21,111,147,0.7)' ,'rgba(21,145,147,0.8)', 'rgba(12,74,115,0.8)','rgba(12,115,91,0.8)')
    
    return(h11)
    
  })
  
  ######End Of Outliers######
  
  #############Smoothing Tab######
  
  smb<-reactive({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    if ((input$considermissing==T)&&(length(input$considermissing)>0))
      otimeseries <- ts(datafilling()[,c("Adjusted Timeseries")], frequency=fileppy())
    else
      otimeseries<-ts(upx()$QTY[upx()$Date<=FirstForecast],start=c(FirstYear,FirstMonth), frequency=fileppy())
    
    
    if (input$removeoutliersdec == T){
      insample<-ts(OutliersTable1()[,c("Adjusted Timeseries")])
    }else{
      if ((input$considermissing==T)&&(length(input$considermissing)>0))
        insample <- ts(datafilling()[,c("Adjusted Timeseries")], frequency=fileppy())
      else
        insample<-ts(upx()$QTY[upx()$Date<=FirstForecast],start=c(FirstYear,FirstMonth), frequency=fileppy())
      
     # insample <- ts(upx()$QTY[upx()$Date<=FirstForecast])
    }
    if (input$considerdecomposition== T){
      TestSeasonal<-SeasonalityTest(InSample=insample, ppy=input$decfr,ForecastHorizon=input$horizonin)
      insample<-TestSeasonal$DeseasonalizedTS
    }
    TestSeasonal<-SeasonalityTest(InSample=insample, ppy=fileppy() ,ForecastHorizon=input$horizonin)
    NewInSample<-TestSeasonal$DeseasonalizedTS
    sm<-smoothwmethod(NewInSample,input$smoothmethodin,input$imain)
    result<-as.data.frame(cbind(round(ts(sm,1),2),ts(otimeseries,1),ts(OutliersTable1()[,c("Adjusted Timeseries")],1),round(ts(TestSeasonal$DeseasonalizedTS,1),2)))
    data<-result
    colnames(data)<-c("Smooth Timeseries","Original Timeseries","Timeseries Without Outliers", "Deseasonalized Timeseries")
    if ((input$removeoutliersdec == T)&&(input$considerdecomposition== T)){
      keeps<-data[,c("Original Timeseries","Timeseries Without Outliers", "Deseasonalized Timeseries","Smooth Timeseries")]
    }
    else if (input$removeoutliersdec == T){
      keeps<-data[,c("Original Timeseries","Timeseries Without Outliers","Smooth Timeseries")]
      
    }
    else if (input$considerdecomposition== T){
      keeps<-data[,c("Original Timeseries", "Deseasonalized Timeseries","Smooth Timeseries")]
      
    }else{
      keeps<-data[,c("Original Timeseries","Smooth Timeseries")]
      
    }
    
    #data[,keeps]
    # data
    keeps
  })
  output$smtab2<-renderTable({
    data<-round(smb(),2)
    sho<-data[,c("Original Timeseries","Smooth Timeseries")]
    colnames(sho)<-c("Original data", "Smoothened data")
    sho
  })
  smbplot<-reactive({
    data<-smb()
    if ((input$removeoutliersdec == T)&&(input$considerdecomposition== T)){
      colnames(data)<-c("A. Original Timeseries","B. Timeseries Without Outliers", "C. Deseasonalized Timeseries","D. Smooth Timeseries")    }
    else if (input$removeoutliersdec == T){
      colnames(data)<-c("A. Original Timeseries","B. Timeseries Without Outliers", "C. Smooth Timeseries") 
      
    }
    else if (input$considerdecomposition== T){
      colnames(data)<-c("A. Original Timeseries", "B. Deseasonalized Timeseries","C. Smooth Timeseries") 
      
    }else{
      colnames(data)<-c("A. Original Timeseries","B. Smooth Timeseries")
      
    }
    data
  })
  output$plotsm<-renderChart2({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    insample <- upx()$QTY[upx()$Date<=FirstForecast]
    
    dataaa<-as.data.frame(smbplot())
    #  dataaa$original<-insample
    dataaa$Observations<-1:nrow(dataaa)#as.numeric(row.names(dataaa))
    #  colnames(dataaa)<-c("Smooth","Original","Row Name")
    simsubmelt<- melt(dataaa,id="Observations")
    #     output$test<-renderTable({
    #       dataaa<-dataaa[,c( "Original","Smooth")]
    #       dataaa
    #     })
    h13 = hPlot( x='Observations', y = "value", group = "variable", data = simsubmelt, type = "line")
    h13$colors( 'rgba(48, 43, 43, 1)', 'rgba(21,111,147,0.7)' ,'rgba(21,145,147,0.8)', 'rgba(12,74,115,0.8)')
    return(h13)
  })
  
  ######End of Smoothing Tab######
  
  ##########Decomposition Tab##########
  
  output$decompositiontext<-renderText({
    
    if (input$decfr=="Unknown"){
      if (input$removeoutliersdec==F){
        indata<-upx()
        indata[is.na(indata)] <- 0 
        FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
        FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
        FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
        InSample <- ts(indata$QTY[indata$Date<=FirstForecast])
        InSample[is.na(InSample)] <- 0
      }else{
        InSample<-ts(OutliersTable1()[,c("Adjusted Timeseries")])
      }
      
      ppy=input$decfr
      if (ppy=='Unknown')
        ppy = findppy(InSample)
      ppy<-as.numeric(ppy)
      tout<-paste0("Unknown decomposition frequency is selected. The frequency used is the one that fits the model the best. (Automatically selected frequency = ",ppy," )")
    }else{
      tout=""
    }
    tout
  })
  decompositionprob<-reactive({
    if (input$removeoutliersdec==F){
      indata<-upx()
      indata[is.na(indata)] <- 0 
      FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
      FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
      FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
      InSample <- ts(indata$QTY[indata$Date<=FirstForecast])
      InSample[is.na(InSample)] <- 0
    }else{
      InSample<-ts(OutliersTable1()[,c("Adjusted Timeseries")])
    }
    
    ppy=input$decfr
    if (ppy=='Unknown')
      ppy = findppy(InSample)
    ppy<-as.numeric(ppy)
    
    input<-ts(as.numeric(InSample),frequency = ppy)
    
    result<-SeasonalTest(input,ppy)
    # 
    # sumacf=0
    # acfc = acf(InSample, lag.max = ppy ,type = c("correlation"),plot = FALSE, demean = TRUE)
    # for (r in 2:(ppy-1)){
    #   sumacf = sumacf + (acfc$acf[r+1])^2
    # }
    # limits = 1.645*((1 + 2*(acfc$acf[2]+sumacf))/length(InSample))^0.5
    # decompositionprob=pt(abs(acfc$acf[ppy+1])/(((1 + 2*(acfc$acf[2]+sumacf))/length(InSample))^0.5), df=Inf)
    # decompositionprob
  })
  observeEvent(input$activetab=="decompose",{
   shinyjs::hide("decfr")
  })
  output$considerdec<-renderUI({
    if (decompositionprob()=="Non seasonal")
      def1 = F
    else
      def1 = T
    checkboxInput("considerdecomposition", label="Consider seasonal adjustments",value = def1)
  })
  output$DecProb<-renderValueBox({
    infoBox(
      "Seasonal Test Results", decompositionprob(),  icon = icon("bar-chart"),
      color = "light-blue"
    )
  })
  Decomposition<- reactive({
    validate(
      need( (!is.na(datafilling()))&&(length(datafilling())>0), "Please select a data set")
    )
    if ((input$removeoutliersdec==F)&&(input$considermissing==F)){
      FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
      FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
      FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
      indata<-upx()
      #indata[is.na(indata)] <- 0 
      insample <- ts(indata$QTY[indata$Date<=FirstForecast])
    }else{
      if (input$removeoutliersdec)
       insample<-ts(OutliersTable1()[,c("Adjusted Timeseries")])
      else
        insample<-ts(ts(datafilling()[,c("Adjusted Timeseries")]))
      
      }
    
    output$testtab<-renderText({as.numeric(insample)})
    ppy<-input$decfr
    if ((ppy=='NULL')||(ppy=='Unknown'))
      ppy = findppy(insample)
    ppy<-as.numeric(ppy)
    
    dec<-DecomposeC(insample,as.numeric(ppy))
    dec
    
    
    
  })
  output$testdec<-renderText({FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
  FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
  FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
  indata<-upx()
  indata[is.na(indata)] <- 0 
  insample <- ts(indata$QTY[indata$Date<=FirstForecast])
  length(insample)})
  output$decRandomness<-renderChart2({
    data<-Decomposition()
    data$Randomness<-as.numeric(as.character(data$Randomness))
    data$Observations<-as.numeric(row.names(data))
    h1 = hPlot( x='Observations', y='Randomness', data = data, type = "line")
    
    return (h1)
  })
  output$decSeasonality<-renderChart2({
    data<-Decomposition()
    data$Seasonality<-as.numeric(as.character(data$Seasonality))
    data$Observations<-as.numeric(row.names(data))
    h2 = hPlot( x='Observations', y='Seasonality', data = data, type = "line")
    
    return (h2)
  })
  output$decCyrcle<-renderChart2({
    data<-Decomposition()
    data$Cyrcle<-as.numeric(as.character(data$Cyrcle))
    data$Observations<-as.numeric(row.names(data))
    h3 = hPlot( x='Observations', y='Cyrcle', data = data, type = "line")
    
    return (h3)
  })
  output$decTrend<-renderChart2({
    indata<-upx()
    indata[is.na(indata)] <- 0
    data<-Decomposition()
    data$Observations<-as.numeric(row.names(data))
    h4 = hPlot( x='Observations', y='Trend', data = data, type = "line")
    h4$yAxis(min= round(min(indata$QTY)-max(indata$QTY)*10/100, 0), max=round(max(indata$QTY)+max(indata$QTY)*10/100, 0))
    #h4$chart(height = "100%")
    #h4$chart(forceY = c(round(min(indata)-max(indata)*10/100, 0), round(max(indata)+max(indata)*10/100, 0)))
    
    return (h4)
  })
  output$decLE<-renderChart2({
    data<-Decomposition()$LE
    if (input$removeoutliersdec==F){
      indata<-upx()
      indata[is.na(indata)] <- 0 
      FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
      FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
      FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
      InSample <- ts(indata$QTY[indata$Date<=FirstForecast])
      InSample[is.na(InSample)] <- 0
    }else{
      InSample<-ts(OutliersTable1()[,c("Adjusted Timeseries")])
    }
    
    ppy=input$decfr
    if (ppy=='Unknown')
      ppy = findppy(InSample)
    ppy<-as.numeric(ppy)
    max <- ppy
    
    x <- seq_along(data)
    dataLE <- split(data, ceiling(x/max))
    dataLE <- as.data.frame(dataLE)
    dataLE$Observations<-as.numeric(row.names(dataLE))
    simsubmelt = melt(dataLE,id="Observations")
    h6 = hPlot( x='Observations', y = "value", group = "variable", data = simsubmelt, type = "line")
    
    
    return (h6)
  })
  
  output$decPlot<-renderChart2({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    indata<-upx()
    indata[is.na(indata)] <- 0 
    #insample <- ts(indata$QTY[indata$Date<FirstForecast])
    if ((input$considermissing==T)&&(length(input$considermissing)>0))
      insample <- ts(datafilling()[,c("Adjusted Timeseries")], frequency=fileppy())
    else
      insample<-ts(upx()$QTY[upx()$Date<=FirstForecast],start=c(FirstYear,FirstMonth), frequency=fileppy())
    
    if (input$removeoutliersdec==F){
      
      outliers <-insample
    }else{
      
      outliers<-ts(OutliersTable1()[,c("Adjusted Timeseries")])}
    
    
    data<-as.data.frame(Decomposition()[,c("Data","Deseasonalized")])
    #   data<-data[,c("Deseasonalized")]
    data$Original<-insample
    colnames(data)[which(names(data) == "Original")] <- "A. Original Timeseries"
    colnames(data)[which(names(data) == "Deseasonalized")] <- "C. Adjusted Timeseries"
    colnames(data)[which(names(data) == "Data")] <- "B. Timeseries Without Outliers"
    data$Observations<-as.numeric(row.names(data))
    datamelt<-melt(data, id="Observations")
    h22 = hPlot( x='Observations', y='value',group="variable", data = datamelt , type = "line")
    h22$colors( 'rgba(48, 43, 43, 1)', 'rgba(21,111,147,0.7)' ,'rgba(21,145,147,0.8)', 'rgba(12,74,115,0.8)','rgba(12,115,91,0.8)')
    h22$yAxis(min= round(min(indata$QTY)-max(indata$QTY)*10/100, 0), max=round(max(indata$QTY)+max(indata$QTY)*10/100, 0))
    
    
    return (h22)
  })
  output$decompRes<-renderTable({
    data<-as.data.frame(Decomposition()[,c("Data","Deseasonalized")])
    colnames(data)<-c("Original Data","Adjusted Data")
    data
  })
  
  #########End of Decomposition########
  # output$testtableee<-renderTable({
  #   as.data.frame(FinalTable())
  # })
  output$plot <- renderChart2({
  	  if (!((intermittent()==T)||(input$useinter==T))){
    datatable<-FinalTable()
    colnames(datatable)[colnames(datatable)=="Date"] <- "TimeStamp"
    colnames(datatable)[colnames(datatable)=="qvalue"] <- "Value"
    colnames(datatable)[colnames(datatable)=="qvaluec"] <- "Value Deseasonalized"
    colnames(datatable)[colnames(datatable)=="fqvalueb"] <- "Value Forecast"
    simsub=datatable
    
    
    
    # #Add Confidence Levels
    if (input$clin!="none"){
      TSSY<-as.numeric(substr(as.character(dateinput()[2]),1,4))
      TSSM<- as.numeric(substr(as.character(dateinput()[2]),6,7))
      qtycl1a<-ts(forecastQTYe()$CL1,frequency=fileppy(),start = c(as.numeric(TSSY),as.numeric(TSSM)))
      qtycl1.m<- data.frame(yr=round(time(qtycl1a), digits = 2),qtycl1=as.matrix(qtycl1a))
      datatable<-merge(datatable,qtycl1.m,by = "yr", all=TRUE)
      qtycl2a<-ts(forecastQTYe()$CL2,frequency=fileppy(),start = c(as.numeric(TSSY),as.numeric(TSSM)))
      qtycl2.m<- data.frame(yr=round(time(qtycl2a), digits = 2),qtycl2=as.matrix(qtycl2a))
      datatable<-merge(datatable,qtycl2.m,by = "yr", all=TRUE)
      colnames(datatable)[colnames(datatable)=="qtycl1"] <- "Confidence Level 1"
      colnames(datatable)[colnames(datatable)=="qtycl2"] <- "Confidence Level 2"}
    simsub=datatable
    
    
    simsub$ID<-1:nrow(simsub)
    preparation<-smbplot()
    if (input$considersmoothing==F) #if smoothing is FALSE ignore last column
      preparation<-preparation[,1:(ncol(preparation)-1)]
    preparation$ID<-1:nrow(preparation)
    simsub<- merge(simsub,preparation, by="ID", all=TRUE)
    if (input$considersmoothing==T){
      level<-chartr("012345678", "ABCDEFGHI", ncol(smbplot()))
      colnames(simsub)[colnames(simsub)=="Value Forecast"] <- paste0(level, ". Value Forecast")
      if (input$clin!="none"){
        level1<-chartr("012345678", "ABCDEFGHI", ncol(smbplot())+1)
        colnames(simsub)[colnames(simsub)=="Confidence Level 1"] <- paste0(level1, ". Confidence Level 1")
        
        level2<-chartr("012345678", "ABCDEFGHI", ncol(smbplot())+2)
        colnames(simsub)[colnames(simsub)=="Confidence Level 2"] <- paste0(level2, ". Confidence Level 2")
        
        simsub<-simsub[,c(c("DatePlot",paste0(level, ". Value Forecast"),paste0(level1, ". Confidence Level 1"),paste0(level2, ". Confidence Level 2")),colnames(smbplot()))]
        
      }else {
        simsub<-simsub[,c(c("DatePlot",paste0(level, ". Value Forecast")),colnames(smbplot()))]
      }
      
    }
    else {
      level<-chartr("012345678", "ABCDEFGHI", ncol(smbplot())-1)
      
      colnames(simsub)[colnames(simsub)=="Value Forecast"] <- paste0(level, ". Value Forecast")
      
      if (input$clin!="none"){
        level1<-chartr("012345678", "ABCDEFGHI", ncol(smbplot()))
        colnames(simsub)[colnames(simsub)=="Confidence Level 1"] <- paste0(level1, ". Confidence Level 1")
        
        level2<-chartr("012345678", "ABCDEFGHI", ncol(smbplot())+1)
        colnames(simsub)[colnames(simsub)=="Confidence Level 2"] <- paste0(level2, ". Confidence Level 2")
        
        simsub<-simsub[,c(c("DatePlot",paste0(level, ". Value Forecast"),paste0(level1, ". Confidence Level 1"),paste0(level2, ". Confidence Level 2")),colnames(smbplot()[1:(ncol(smbplot())-1)]))]
        
      }else {
        simsub<-simsub[,c(c("DatePlot",paste0(level, ". Value Forecast")),colnames(smbplot()[1:(ncol(smbplot())-1)]))]
      }
      
      
    }
    
    
    simsubmelt = melt(simsub,id="DatePlot")
    
    color<-c('rgba(43, 42, 42, 1.0)')
    if (input$removeoutliersdec==T)
      color<-c(color,c('rgba(87, 100, 254, 0.6)'))
    if (input$considerdecomposition==T)
      color<-c(color,c('rgba(90, 174, 174, 0.5)'))
    if (input$considersmoothing==T)
      color<-c(color,c('rgba(25,25,112, 0.5)'))
    color<-c(color,c('rgba(13, 104, 195, 0.86)'))
    color<-c(color,c('rgba(166, 9, 44, 0.6)'))
    color<-c(color,c('rgba(166, 9, 44, 0.6)'))
    
    
    
    h5 = hPlot(x = "DatePlot", y = "value", group = "variable", data = simsubmelt, type = "line")
    h5$colors(color)
    h5$title(text = forecastQTY()$method)
    h5$exporting(enabled = F)
    h5$chart(zoomType = "xy")
    h5$yAxis(title="")
    h5$xAxis(type = 'datetime', format = "%Y")  
    h5$tooltip(formatter = "#!  function(key, x, y){ 
                var monthNames = [ 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December' ];
                return 'Date: '+monthNames[((new Date(this.point.x)).getMonth())] + ' ' +(new Date(this.point.x)).getFullYear()  + '<br>'  +'Value: ' + this.point.y 
  } !#")
    }
    return(h5)
  })  
  
  ########INTERMITTENT DEMAND FORECASTS############
  
  output$interplot <- renderChart2({
  	datatable<-OutputTable()[,c("Value","Value Forecast")]
    datatable$DatePlot=FinalTable()[,c("DatePlot")]

    simsub=datatable
    simsubmelt = melt(simsub,id="DatePlot")
    
    color<-c('rgba(43, 42, 42, 1.0)')
    color<-c(color,c('rgba(13, 104, 195, 0.86)'))
    color<-c(color,c('rgba(166, 9, 44, 0.6)'))
    color<-c(color,c('rgba(166, 9, 44, 0.6)'))
    
    
    
    h20 = hPlot(x = "DatePlot", y = "value", group = "variable", data = simsubmelt, type = "line")
    h20$colors(color)
        h20$title(text = "Intermittent")#forecastQTY()$method)
    h20$exporting(enabled = F)
    h20$chart(zoomType = "xy")
    h20$yAxis(title="")
    h20$xAxis(type = 'datetime', format = "%Y")  
    h20$tooltip(formatter = "#!  function(key, x, y){ 
               var monthNames = [ 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December' ];
               return 'Date: '+monthNames[((new Date(this.point.x)).getMonth())] + ' ' +(new Date(this.point.x)).getFullYear()  + '<br>'  +'Value: ' + this.point.y 
  } !#")
    return(h20)
    
  })  
  #################################################
  
  output$CLTable<-renderTable({
    if (input$clin!="none"){
      data<-data.frame(ID=1:input$horizonin)
      display<-data
      title1<-c(" ")
      title2<-c(" ")
      data$QTYforecasts<-as.numeric(as.character(forecastQTYe()$forecasts))
      data$QTYCL1<-forecastQTYe()$CL1
      data$QTYCL2<-as.numeric(as.character(forecastQTYe()$CL2))
      colnames(data)[colnames(data)=="QTYCL1"] <- "Value:Upper Confidence Level"
      colnames(data)[colnames(data)=="QTYCL2"] <- "Value:Lower Confidence Level"
      colnames(data)[colnames(data)=="QTYforecasts"] <- "Value Forecast"
      display<-cbind(display,data[,c( "Value Forecast","Value:Upper Confidence Level","Value:Lower Confidence Level")])
      title1<-c(title1, "", "waterdemand", "")
      title2<-c(title2, "Forecast", "Upper Confidence Level", "Lower Confidence Level")
      display
    }
  },include.rownames=FALSE)
  
  ###########Error Tab##########
  
  output$errortable<- renderTable({
    errors()
  },include.rownames=FALSE)
  errors <-reactive({
    
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=1) 
    error1<-ErrorsCalculation(head(FinalTable()$fqvalue, length(insample)), head(FinalTable()$qvalue, length(insample)))
    errorqty<-t(error1[[2]])
    errorqty[1]<-as.character(round(as.numeric(errorqty[1]) , digits = 2))
    errorqty[2]<-paste(round(as.numeric(errorqty[2]) , digits = 2)," %", sep="")
    errorqty[3]<-paste(round(as.numeric(errorqty[3]) , digits = 2)," %", sep="") 
    errorqty[4]<-as.character(round(as.numeric(errorqty[4]) , digits = 2))
    errorqty[5]<-paste(round(as.numeric(errorqty[5]) , digits = 2)," %", sep="")
    errorqty
  })
  output$errorscatter<-renderChart2({
    
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=1) 
    scdata<-data.frame(cbind(head(FinalTable()$fqvalue, length(insample)),head(FinalTable()$qvalue, length(insample))))
    
    colnames(scdata)<-c("Forecast Model","Insample")
    scdata$insample2<-scdata$Insample  
    scdatamelt<-melt(scdata,id="Insample")
    h14 = hPlot(x = "Insample", y = "value",group="variable", data = scdatamelt, type = "scatter")
    h14$title(text = "Insample-Forecast Model")
    h14$exporting(enabled = F)
    h14$chart(zoomType = "xy")
    return(h14)
  })
  output$mescatter<-renderChart2({
    
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=1) 
    error1<-ErrorsCalculation(head(FinalTable()$fqvalue, length(insample)), head(FinalTable()$qvalue, length(insample)))
    errorqty<-t(error1[[1]])
    colnames(errorqty)<-c("e","pe","ape","se","sape")
    scdata<-data.frame(rn=1:length(errorqty[,c("e")]))
    scdata$error<-errorqty[,c("e")]
    
    h16 = hPlot(x = "rn", y = "error", data = scdata, type = "column")
    h16$title(text = "Insample-Forecast Model: Errors")
    h16$exporting(enabled = F)
    h16$chart(zoomType = "xy")
    return(h16)
  })
  output$forecasterrortable<- renderTable({
    forecasterrors()
  },include.rownames=FALSE)
  forecasterrors <-reactive({
    
    
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=1) 
    allsample<-ts(upx()$QTY,frequency=1)
    lengthdfa<-length(allsample)-length(insample)
    if (lengthdfa>0){
    if (length(insample)<length(upx()$QTY)) {
      dfa<-tail(upx()$QTY,as.numeric(lengthdfa))
    }else{
      dfa<-NULL
    }
    dfb<-finalforecasts()
    if (length(na.omit(dfa))>=length(na.omit(dfb))) {
      yd<-as.numeric(input$horizonin)
      
    } else{
      yd<-length(dfa)
      
    }
    df1<-head(dfa,yd)
    df2<-head(dfb,yd)
    
    
    
    errorqty1a<-ErrorsCalculation(df1, df2)
    
    errorqty<-t(errorqty1a[[2]])
    errorqty[1]<-as.character(round(as.numeric(errorqty[1]) , digits = 2))
    errorqty[2]<-paste(round(as.numeric(errorqty[2]) , digits = 2)," %", sep="")
    errorqty[3]<-paste(round(as.numeric(errorqty[3]) , digits = 2)," %", sep="") 
    errorqty[4]<-as.character(round(as.numeric(errorqty[4]) , digits = 2))
    errorqty[5]<-paste(round(as.numeric(errorqty[5]) , digits = 2)," %", sep="")
    errorqty  
    }else{
      errorqty <- data.frame(ME=character(),
                             MPE=character(),
                             MAPE=character(),
                             MSE=character(),
                             sMAPE=character(),
                              stringsAsFactors=FALSE)
      errorqty<-rbind(c("ME","MPE","MAPE","MSE","sMAPE"),c("Na","Na","Na","Na","Na"))
      
      
    }
    errorqty

  })
  output$forecasterrorscatter<-renderChart2({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=1) 
    allsample<-ts(upx()$QTY,frequency=1)
    lengthdfa<-length(allsample)-length(insample)
    if (length(insample)<length(upx()$QTY)) {
      dfa<-tail(upx()$QTY,as.numeric(lengthdfa))
    }else{
      dfa<-NULL
    }
    dfb<-finalforecasts()#tail(na.omit(FinalTable()[,c("fqvalue")]),input$horizonin)
    if (length(na.omit(dfa))>=length(na.omit(dfb))) {
      yd<-as.numeric(input$horizonin)
      
    } else{
      yd<-length(dfa)
      
    }
    df1<-as.data.frame(head(dfa,yd))
    df2<-as.data.frame(head(dfb,yd))
    
    df1$rn<-1:yd
    df2$rn<-1:yd
    
    scdata<-merge(df1,df2,by="rn",all.y=TRUE)
    # scdata<-na.omit(scdata)
    colnames(scdata)<-c("rn","Out Of Sample","Forecasts")
    scdata<-scdata[,c("Out Of Sample","Forecasts")]
    scdata$OutOfSample<-scdata[,c("Out Of Sample")]
    scdatamelt<-melt(scdata,id="OutOfSample")  
    h15 = hPlot(x = "OutOfSample", y = "value",group="variable", data = scdatamelt, type = "scatter")
    h15$title(text = "Insample-Forecast Model")
    h15$exporting(enabled = F)
    h15$chart(zoomType = "xy")
    return(h15)
  })
  output$forecastmescatter<-renderChart2({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    
    insample <- ts(upx()$QTY[upx()$Date<=FirstForecast],frequency=1) 
    allsample<-ts(upx()$QTY,frequency=1)
    lengthdfa<-length(allsample)-length(insample)
    if (length(insample)<length(upx()$QTY)) {
      dfa<-tail(upx()$QTY,as.numeric(lengthdfa))
    }else{
      dfa<-NULL
    }
    dfb<-finalforecasts()#tail(na.omit(FinalTable()[,c("fqvalue")]),input$horizonin)
    if (length(na.omit(dfa))>=length(na.omit(dfb))) {
      yd<-as.numeric(input$horizonin)
      
    } else{
      yd<-length(dfa)
      
    }
    df1<-head(dfa,yd)
    df2<-head(dfb,yd)
    
    errorqty1a<-ErrorsCalculation(df1, df2)
    
    errorqty<-t(errorqty1a[[1]])
    colnames(errorqty)<-c("e","pe","ape","se","sape")
    scdata<-data.frame(rn=1:length(errorqty[,c("e")]))
    scdata$error<-errorqty[,c("e")]
    scdata$zero<-0
    scdatamelt<-melt(scdata,id="rn")
    h17 = hPlot(x = "rn", y = "error", data = scdata, type = "column")
    
    h17$title(text = "Out Of Sample-Forecasts: Errors")
    h17$exporting(enabled = F)
    h17$chart(zoomType = "xy")
    return(h17)
  })
  
  #########End of Error########
  ###########Hierarchical Clustering###########
  uploaddataset<-reactive({
    inFile <- input$datasetcsv
    if (is.null(inFile))
      return(NULL)
    inputcsv<-read.csv(inFile$datapath, header = T, sep=";")
    inputcsv
    
  })
  dsvariables<-reactive({
    validate(
      need(length(uploaddataset())!=0," Please Upload a Dataset.")
    )
    colnames(uploaddataset()[,2:ncol(uploaddataset())])
  })
  
  output$hiertsin<-renderUI({
    selectizeInput(inputId="hiertschoices", label="Select time series:", choices=dsvariables(), multiple = TRUE,options = list(maxItems = input$hiervarin))
  })
  
  starthierarchy<-reactive({
    validate(
      need(length(input$hiertschoices)==input$hiervarin," Please Complete the Hierarchy Variables"))
    ts<-c(input$hiertschoices)
    pt<-c(rep("Null", length(input$hiertschoices)))
    m<-rbind(ts,pt)
    rownames(m)<-c("Timeseries:","Parent:")
    m
    
  })
  
  
  
  observeEvent(input$hiertable, {
    starthierarchy()
  })
  
  observeEvent(input$hierlevels, {
    session$sendCustomMessage(type = "resetValue", message = "hiertable")
  })  
  observeEvent(input$hiervarin, {
    session$sendCustomMessage(type = "resetValue", message = "hiertable")
  })  
  observeEvent(input$hiertschoices, {
    session$sendCustomMessage(type = "resetValue", message = "hiertable")
  })
  
  
  buildedges1<-reactive({
    validate(
      need(length(input$hiertschoices)==input$hiervarin," Please Complete the Hierarchy Variables"))
    relations<-1:length(input$hiertschoices)
    
    for (i in 1:(length(input$hiertschoices))){
      relations[i]<-input[[paste0("par",i-1)]]
    }
    
    relations<-cbind(relations,input$hiertschoices)
    
    colnames(relations)<-c("relations","children")
    fulldf<-relations
    
  })
  buildedges<-reactive({
    fulldf<-as.data.frame(buildedges1(),stringsAsFactors=FALSE)
    total<- data.frame(relations=character(),
                       children=character())
    for (i in 1:(length(input$hiertschoices))){
      #Curr holds the selectized options and their (unique) vectors. 
      #Every time we enter a child we have to link it to its parent.
      curr<-cbind(relations=as.character(rep(fulldf$relations[i],length(unique(uploaddataset()[,c(input$hiertschoices[i])])))),
                  children=as.character(paste0(fulldf$relations[i],"_",unique(uploaddataset()[,c(input$hiertschoices[i])]))))
      innertotal<-data.frame(relations=character(),
                             children=character())
      if (i>1){
        for (j in 1:(length(unique(uploaddataset()[,c(input$hiertschoices[i-1])]))))
        {
          
          innercurr<-cbind(relations=as.character(rep(paste0("",unique(uploaddataset()[,c(input$hiertschoices[i-1])])[j]),length(unique(uploaddataset()[,c(input$hiertschoices[i])])))),
                           children=paste0(as.character(rep(unique(uploaddataset()[,c(input$hiertschoices[i-1])])[j],length(unique(uploaddataset()[,c(input$hiertschoices[i])])))),"_",as.character(unique(uploaddataset()[,c(input$hiertschoices[i])]))))
          innertotal<-rbind(innertotal,innercurr)
        }
        total<-rbind(total,innertotal)
      }else{
        total<-rbind(total,curr)
        
      }
    }
    total<-as.data.frame(total,stringsAsFactors = F)
    colnames(total)<-c("relations","children")
    ship<-str_split_fixed(total$children, "_", 2)
    
    colnames(ship)<-c("par","cld")
    ship<-as.data.frame(ship,stringsAsFactors = F)
    for (i in 1:nrow(total)){
      curr<-total$relations[i]
      for (j in 1:i){
        if (as.character(ship[j,2])==as.character(curr)){
          total$relations<-gsub(curr,paste0(ship[j,1],"_",ship[j,2]),total$relations)
          break
        }
      }
      
      
    }
    total
    
  })
  actualsplitdataset<-reactive({
    validate(
      need(length(input$mydata)!=0," Please Click a Node.")
    )
    #Getting Group Info
    dato<-as.data.frame(buildedges())
    be<-as.data.frame(dato)
    rel<-transform(as.data.frame(be),id=as.numeric(factor(relations)))
    colnames(rel)<-c("Parent","name","group")
    as.data.frame(rel)
    
    #Getting Clicked Edge
    clicked<-as.data.frame(str_split_fixed(input$mydata,"_",2))
    colnames(clicked)<-c("name","group")
    clickedgroup<-clicked$group
    cgroup<-clicked$group
    clickedname<-clicked$name
    
    #Track Tree Route
    selections<- data.frame(value=character(),  group=character())
    ship<-str_split_fixed(rel$name, "_", 2)
    colnames(ship)<-c("par","cld")
    ship<-as.data.frame(ship,stringsAsFactors = F)
    notfound=T
    i=1
    #track tree route
    parents<-data.frame(value=character())
    while (notfound){
      curr<-rel$group[i]
      if (as.character(curr)==as.character(cgroup)) {
        a<-as.data.frame(str_split_fixed(rel$name[i] ,"_",2))
        val=a[,2]
        gr=curr
        selections<-rbind(c(val,gr),selections)
        par=rel$Parent[i]
        parents<-rbind(parents,par)
        cgroup<-head(subset(rel, name==par , select=group),1)
        i=0
        if (par=="None")
          notfound=F
      }
      i=i+1
    }
    
    clckd<-str_split_fixed(input$mydata,"_",2)[[1]][1]
    parents<-as.data.frame(parents)
    
    xy.df<-cbind(head(input$hiertschoices,length(t(c(str_split(as.character(parents[[1]])[1],"_")[[1]],as.character(clickedname))))-1),
                 tail(c(str_split(as.character(parents[[1]])[1],"_")[[1]],as.character(clickedname)),length(t(c(str_split(as.character(parents[[1]])[1],"_")[[1]],as.character(clickedname))))-1))
    
    df<-as.data.frame(uploaddataset())
    #find clicked columns in dataset
    vals<-c()
    for (i in 1:ncol(df)){
      if (colnames(df)[i]%in%xy.df[,1]){
        vals<-c(vals,i)
      }
    }
    vals<-c(1,vals)
    
    #aggregate results & select result
    df<-as.data.table(uploaddataset())
    dfs<-as.data.frame(df)[,c(vals)]
    
    colnames(df)[ncol(df)]<-"temp"
    df <- df[, temp:=as.numeric(as.character(temp))]
    df<-df[, sum(temp), by=dfs]
    
    for (i in 1:nrow(xy.df))
      df<-df[which(subset(df,select=c(xy.df[i,1]))==xy.df[i,2]),]
    df<-as.data.frame(df)
  })
  
  TopDownP<-reactive({
    validate(
      need(length(input$mydata)!=0," Please Click a Node.")
    )
    #Getting Group Info
    dato<-as.data.frame(buildedges())
    be<-as.data.frame(dato)
    rel<-transform(as.data.frame(be),id=as.numeric(factor(relations)))
    colnames(rel)<-c("Parent","name","group")
    as.data.frame(rel)
    
    #Getting Clicked Edge
    clicked<-as.data.frame(str_split_fixed(input$mydata,"_",2))
    colnames(clicked)<-c("name","group")
    clickedgroup<-clicked$group
    cgroup<-clicked$group
    clickedname<-clicked$name
    
    
    #Track Tree Route
    selections<- data.frame(value=character(),  group=character())
    ship<-str_split_fixed(rel$name, "_", 2)
    colnames(ship)<-c("par","cld")
    ship<-as.data.frame(ship,stringsAsFactors = F)
    notfound=T
    i=1
    #track tree route
    parents<-data.frame(value=character())
    while (notfound){
      curr<-rel$group[i]
      if (as.character(curr)==as.character(cgroup)) {
        a<-as.data.frame(str_split_fixed(rel$name[i] ,"_",2))
        val=a[,2]
        gr=curr
        selections<-rbind(c(val,gr),selections)
        par=rel$Parent[i]
        parents<-rbind(parents,par)
        cgroup<-head(subset(rel, name==par , select=group),1)
        i=0
        if (par=="None")
          notfound=F
      }
      i=i+1
    }
    
    #clckd<-str_split_fixed(input$mydata,"_",2)[[1]][1]
    parents<-as.data.frame(parents)
    
    # xy.df<-cbind(head(input$hiertschoices,length(t(c(str_split(as.character(parents[[1]])[1],"_")[[1]],as.character(clickedname))))-1),
    #              tail(c(str_split(as.character(parents[[1]])[1],"_")[[1]],as.character(clickedname)),length(t(c(str_split(as.character(parents[[1]])[1],"_")[[1]],as.character(clickedname))))-1))
    # 
    xy.df<-cbind(head(input$hiertschoices,length(t(c(str_split(as.character(parents[[1]])[1],"_")[[1]],as.character(clickedname))))-1),
                 tail(c(str_split(as.character(parents[[1]])[1],"_")[[1]],as.character(clickedname)),length(t(c(str_split(as.character(parents[[1]])[1],"_")[[1]],as.character(clickedname))))-1))
    
    df<-as.data.frame(uploaddataset())
    #find clicked columns in dataset
    vals<-c()
    for (i in 1:ncol(df)){
      if (colnames(df)[i]%in%xy.df[,1]){
        vals<-c(vals,i)
      }
    }
    vals<-c(1,vals)
    
    #aggregate results & select result
    df<-as.data.table(uploaddataset())
    parentdf<-as.data.table(uploaddataset())
    dfs<-as.data.frame(df)[,c(vals)]
    
    colnames(df)[ncol(df)]<-"temp"
    colnames(parentdf)[ncol(parentdf)]<-"temp"
    colnames(parentdf)[1]<-"the_date"
    df <- df[, temp:=as.numeric(as.character(temp))]
    parentdf<- parentdf[, temp:=as.numeric(as.character(temp))]
    df<-df[, sum(temp), by=dfs]
    parentdf<-parentdf[,sum(temp),by=the_date]
    
    uniquevalues<-as.character(unlist(unique(subset(df,select=c(xy.df[1,1])))))
    for (i in 2:nrow(xy.df)){
      uniquevalues<-expand.grid(uniquevalues,as.character(unlist(unique(subset(df,select=c(xy.df[i,1]))))))
    }
    
    uniquevalues
    colnames(uniquevalues)=input$hiertschoices
    output$testedges<-renderPrint({
      uniquevalues
    })
    uv<-data.frame(t(sapply(uniquevalues,c)))
    #here is the parent timeseries
    df<-as.data.table(uploaddataset())
    colnames(df)[ncol(df)]<-"temp"
    df <- df[, temp:=as.numeric(as.character(temp))]
    df<-df[, sum(temp), by=df[,c(1)]]
    parent_timeseries<-as.data.frame(df)
    colnames(parent_timeseries)=c("the_date","p")
    #parent_timeseries columns: the_date, V1
    down<-parent_timeseries
    vals<-c()
    df<-as.data.table(uploaddataset())
    for (i in 1:ncol(df)){
      if (colnames(df)[i]%in%input$hiertschoices){
        vals<-c(vals,i)
      }
    }
    vals<-c(1,vals)
    for (i in 1:nrow(uniquevalues))
    {  
      df<-as.data.table(uploaddataset())
      colnames(df)[ncol(df)]<-"temp"
      df <- df[, temp:=as.numeric(as.character(temp))]
      dfs<-as.data.frame(df)[,c(vals)]
      df<-df[, sum(temp), by=dfs]
      for (j in 1:length(input$hiertschoices))
        df<-df[which(subset(df,select=c(as.character(input$hiertschoices[j])))==as.character(uniquevalues[i,j])),]
      df<-as.data.frame(df)
      df<-df[,c(1,ncol(df))]
      tsstring<-""
      for (k in 1:ncol(uniquevalues))
        tsstring<-paste0(tsstring,as.character(uniquevalues[i,k]),"-")
      output$testedges2<-renderPrint({tsstring})
      colnames(df)<-c("the_date",paste0("",tsstring))
      down<-merge(down,df,by="the_date",all=T)
    }
    
    down
    
    down
  })
  parameters<-reactive({
    down<-TopDownP()
    down$day = strftime(down$the_date,'%u')
    down
    down[is.na(down)] <- 0
    down[,2:(ncol(down))] <- lapply(down[,2:(ncol(down))], function(x) as.numeric(as.character(x)))
    
    df<-down
    dt<- aggregate(df[,-1],df["day"],sum)
    dt
    for (i in 3:ncol(dt)){
      dt[,i]<-dt[,i]/dt[,2]
    }
    dt
  })
  tdforecasts<-reactive({
    df<-as.data.table(uploaddataset())
    colnames(df)[ncol(df)]<-"temp"
    df <- df[, temp:=as.numeric(as.character(temp))]
    df<-df[, sum(temp), by=df[,c(1)]]
    parent_timeseries<-as.data.frame(df)
    colnames(parent_timeseries)=c("the_date","parentts")
    parent_timeseries[is.na(parent_timeseries)] <- 0
    fc<-parent_timeseries
    #Step 1: Find IDI
    insample <- parent_timeseries$parentts
    category<-idclass(insample,type=c("PKa","SBC","KHa","KH","PK"),a.in=NULL,
                      outplot=c("summary","detail","none"),plot.focus=NULL)
    idival<-round(as.numeric(category$p),2)
    if (idival>1.2) {
      #treat as intermittent
      res<-ForecastInter(insample,input$horizonhier,"Optimal")
      fc<-res[[3]]
    }else{
      #treat as continuous
      data<-PreForecast(insample, F, 'Optimal', input$horizonhier, 6,F, F, F)
      res<-Forecast(InSample=data$InSample, NewInSample=data$NewInSample, SeasonalityIndexes=data$Indexes, SIdec=data$SIDec ,ppy=7,ForecastHorizon= input$horizonhier,Method='Theta')
      fc<-as.data.frame(res[[3]])
      
    }
    fulldf<-TopDownP()
    lastdt<-tail(fulldf$the_date,1)
    lastdt<-as.numeric(as.character(strftime(lastdt,'%u')))
    colnames(fc)<-"forecasts"
    fc$day<-1:nrow(fc)
    fc$day<-fc$day+lastdt#lastdt:(nrow(fc)+lastdt)
    fc$day<-fc$day%%7+1
    tsname<-colnames(fulldf)[7]
    for (i in 3:(ncol(fulldf)-1)){
      tsname<-colnames(fulldf)[i]
      temp<-1:nrow(fc)
      for (j in 1:nrow(fc)){
        myparam<-parameters()[ which(parameters()[,c("day")]==fc$day[j]),c(tsname)]
        temp[j]<-as.numeric(as.character(fc$forecasts[j]))*as.numeric(as.character(myparam))
      }
      fc[,c(tsname)]<-temp
    }
    colnames(fc)[1]<-"Sum of Forecasts"
    fc$day<-NULL
    fc
  })
  buforecasts<-reactive({
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Creating bottom time series", value = 0)
    progress$inc(0, detail = paste( "Splitting Sata...")) 
    fulldf<-TopDownP()
    insample <- fulldf[,3]
    insample[is.na(insample)]<-0
    tsname<-colnames(fulldf)[3]
    progress$inc(0, detail = paste( "Data Ready! Proceeding to Forecasts")) 
    fullfc<- data.frame(matrix(vector(), input$horizonhier, ncol(fulldf)-3))
    colnames(fullfc)<-colnames(fulldf)[3:(ncol(fulldf)-1)]
    for (t in 3:(ncol(fulldf)-1)){
      #Step 1: Find IDI
      insample <- fulldf[,t]
      insample[is.na(insample)]<-0
      tsname<-colnames(fulldf)[t]
      #2: Check if empty
      if (mean(insample)==0) {
        fc<-as.data.frame(rep(0,input$horizonhier))
      }else{
        #3: Intermittent?
        category<-idclass(insample,type=c("PKa","SBC","KHa","KH","PK"),a.in=NULL,
                          outplot=c("summary","detail","none"),plot.focus=NULL)
        idival<-round(as.numeric(category$p),2)
        if (idival>1.2) {
          #4a: treat as intermittent
          res<-ForecastInter(insample,input$horizonhier,"Optimal")
          fc<-as.data.frame(res[[3]])
          
          
        }else{
          #4b: treat as continuous
          res<-ForecastHier(InSample=insample,ppy=7,ForecastHorizon= input$horizonhier,Method='Optimal')
          fc<-as.data.frame(res[[3]])
          
        }
      }
      
      fullfc[,c(tsname)]<-fc
      progress$inc(1/(ncol(fulldf)-3), detail = paste( t ," / ", (ncol(fulldf)-1) ,"complete !")) 
    }
    fullfc
    # 
    bu<-colSums(t(fullfc))
    t(bu)
    
    
  })
  # output$testedges<-renderPrint({
  #   TopDownP()
  # })
  
  output$uishowdetailedhier<-renderTable({
    #printing the clicked timeseries
    mydata<-as.data.frame(actualsplitdataset())
    mydata
  })
  
  #Make Network Graph
  output$myNetwork <- renderForceNetwork({
    dato<-as.data.frame(buildedges())
    
    be<-as.data.frame(dato)
    
    rel<-transform(as.data.frame(be),id=as.numeric(factor(relations)))
    colnames(rel)<-c("Parent","name","group")
    
    be1<-as.character(be$relations)
    be2<-as.character(be$children)
    for (i in 1:nrow(be)){
      be1<-replace(be1,be1==be[i,c('children')],i+1)
      be2<-replace(be2,be2==be[i,c('children')],i+1)
    }
    be1<-replace(be1,be1=="None",1)
    be<-as.data.frame(cbind(be1,be2))
    
    g<-graph.empty(n=length(unique(dato$children))+1,directed=F)%>%
      add.edges(as.integer(as.vector(t(be))))
    
    g3<-igraph_to_networkD3(g,group=c(0,rel$group))
    g3$nodes$name<-c("None",as.character(str_split_fixed(rel$name, "_", 2)[,2]))
    g3$nodes$group<-c(0,rel$group)
    
    #Shiny.onInputChange("mydata",d.name+"_"+d.group);
    MyClickScript<-'Shiny.onInputChange("mydata",d.name+"_"+d.group);'
    forceNetwork(Links=g3$links,Nodes=g3$nodes,clickAction=MyClickScript,
                 Source='source',Target='target',NodeID='name',
                 Group='group',zoom=TRUE,
                 opacity=1)
    
    
  })
  
  output$hierresults<-renderPrint({
    input$mydata
  })
  output$hierforecastresults<-renderTable({
    if (input$choosebutd==1)
      as.data.frame(buforecasts())
    else
      as.data.frame(tdforecasts())
  })
  
  ####Select Hierarchy Choices####
  output$selectparent <- renderUI({
    if (is.null(input$hiertschoices) ) {
      n <- 1
    } else {
      n <-length(input$hiertschoices)
    }
    
    lapply(1:n, function(i) {
      
      selectInput(
        inputId = paste0("par", (i-1))
        , label = paste0("Parent of ", input$hiertschoices[i])
        , choices =  c("None",input$hiertschoices[c(-i)])
        , selected =  c("None",input$hiertschoices[c(-i)])[i]
      )
    })
  })
  
  #######END HIERARCHICAL###########
  ###########Temporal Aggregation Tab##########
  
  taggdata<-reactive({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    if (input$considermissing==T)
      insample <- datafilling()[,c("Adjusted Timeseries")]
    else
      insample<-upx()$QTY[upx()$Date<=FirstForecast]
    ppy<-fileppy()
    tagd<-taggregation(insample,ppy)
    tagd<-as.data.frame(tagd)
    tagd
    
  })
  numtomonth<-reactive({
    data<-c("January", "February","March","April","May","June","July","August","September","October","November","December")
    data
  })
  numtotrimester<-reactive({
    data<-c("1st Trimester", "2nd Trimester","3rd Trimester","4th Trimester")
    data
  })
  numtoquarter<-reactive({
    data<-c("1st Quarter", "2nd Quarter","3rd Quarter")
    data
  })
  output$consultagg<-renderUI({
    selectInput("consultvar","Choose which aggregation level to inspect", choices=colnames(taggdata()),selected = "Annual Model")
  })
  
  inspectresult<-reactive({
    FirstYear<-as.numeric(substr(as.character(dateinput()[2]),1,4))
    FirstMonth<-as.numeric(substr(as.character(dateinput()[2]),6,7))
    FirstForecast <-100*FirstYear+FirstMonth #Get first forecast
    if (input$considermissing==T)
      insample <- datafilling()[,c("Adjusted Timeseries")]
    else
      insample<-upx()$QTY[upx()$Date<=FirstForecast]
    
    if (input$ppyin == 'Unknown') {
      tsfrequency<-findppy(insample)
    }
    else {
      tsfrequency<-as.numeric(as.character(input$ppyin))
    }
    
    if (input$consultvar == "Annual Model"){
      model<-taggdata()[,c(1)]
      sn<-12/tsfrequency
      if (sn==1) sn=12
      else if (sn==12) sn=1
      GrowthRate=(1/sn*sum(model[((nrow(model)-sn+1):(nrow(model)))])-1/(nrow(model)-sn)*sum(model[1:(nrow(model)-sn)]))/(1/(nrow(model)-sn)*sum(model[1:(nrow(model)-sn)]))

      res=paste0("The growth rate of the time-series is ",GrowthRate,".")
    }else if(input$consultvar == "Original Timeseries"){
      if (input$ppyin == 'Unknown') {
        tsfrequency<-findppy(insample)
      }
      else {
        tsfrequency<-as.numeric(as.character(input$ppyin))
      }
      s<-SeasonalTest(taggdata()[,c(1)],tsfrequency)
      if (s!="Non Seasonal") {
        datadates<-x()[,c(1)]
        datadates<-datadates[1:tsfrequency]
        dec<-DecomposeC(taggdata()[,c(1)],tsfrequency)
        si<-dec$Seasonality[1:tsfrequency]
        
        maxmonth<-as.numeric(substr(as.character(datadates[which.max(si)]),5,7))
        minmonth<-as.numeric(substr(as.character(datadates[which.min(si)]),5,7))
        cat(minmonth)
        if (tsfrequency==12)
          res<-paste0("The best performing month is ",numtomonth()[maxmonth], " while the worst performing month is ",numtomonth()[minmonth])
      }
      
    }else if(input$consultvar == "Quarterly Model"){
      
      
      s<-SeasonalTest(taggdata()[,c("Quarterly Model")],3)
      if (s!="Non Seasonal") {
        datadates<-x()[,c(1)]
        datadates<-datadates[1:tsfrequency]
        dec<-DecomposeC(taggdata()[,c("Quarterly Model")],3)
        si<-dec$Seasonality[1:3]
        
        maxquarter<-as.numeric(as.character(which.max(si)))
        minquarter<-as.numeric(as.character(which.min(si)))

        
          res<-paste0("The best performing quarter is the ",numtoquarter()[maxquarter], " while the worst performing quarter is the ",numtoquarter()[minquarter])
      }
      
    }else if(input$consultvar == "Trimester Model"){
      
     
      s<-SeasonalTest(taggdata()[,c("Trimester Model")],4)
      if (s!="Non Seasonal") {
        datadates<-x()[,c(1)]
        datadates<-datadates[1:tsfrequency]
        dec<-DecomposeC(taggdata()[,c("Trimester Model")],4)
        si<-dec$Seasonality[1:4]
        
        maxtrimester<-as.numeric(as.character(which.max(si)))
        mintrimester<-as.numeric(as.character(which.min(si)))
        
        
        res<-paste0("The best performing trimester is the ",numtotrimester()[maxtrimester], " while the worst performing trimester is the ",numtotrimester()[mintrimester])
      }
      
    }else if(input$consultvar == "2-Month Model"){
      
      
      s<-SeasonalTest(taggdata()[,c("2-Month Model")],6)
      if (s!="Non Seasonal") {
        datadates<-x()[,c(1)]
        datadates<-datadates[1:tsfrequency]
        dec<-DecomposeC(taggdata()[,c("2-Month Model")],6)
        si<-dec$Seasonality[1:6]
        
        maxmonth1<-as.numeric(substr(as.character(datadates[which.max(si*2)-1]),5,7))
        minmonth1<-as.numeric(substr(as.character(datadates[which.min(si*2)-1]),5,7))
        maxmonth2<-as.numeric(substr(as.character(datadates[which.max(si*2)]),5,7))
        minmonth2<-as.numeric(substr(as.character(datadates[which.min(si*2)]),5,7))
        
        max2<-paste0(numtomonth()[maxmonth1],"-",numtomonth()[maxmonth2]," period")
        min2<-paste0(numtomonth()[minmonth1],"-",numtomonth()[minmonth2]," period")
        
        res<-paste0("The best performing 2-Month period is the ",max2, " while the worst performing 2-Month period is the ",min2)
      }
      
    }else{
      ##Half-Year Model##
      
      s<-SeasonalTest(taggdata()[,c("Half-Year Model")],2)
      if (s!="Non Seasonal") {
        datadates<-x()[,c(1)]
        datadates<-datadates[1:tsfrequency]
        dec<-DecomposeC(taggdata()[,c("Half-Year Model")],2)
        si<-dec$Seasonality[1:2]
        
        maxmonth1<-as.numeric(substr(as.character(datadates[which.max(si)*6-5]),5,7))
        minmonth1<-as.numeric(substr(as.character(datadates[which.min(si)*6-5]),5,7))
        maxmonth2<-as.numeric(substr(as.character(datadates[which.max(si)*6-4]),5,7))
        minmonth2<-as.numeric(substr(as.character(datadates[which.min(si)*6-4]),5,7))
        maxmonth3<-as.numeric(substr(as.character(datadates[which.max(si)*6-3]),5,7))
        minmonth3<-as.numeric(substr(as.character(datadates[which.min(si)*6-3]),5,7))
        maxmonth4<-as.numeric(substr(as.character(datadates[which.max(si)*6-2]),5,7))
        minmonth4<-as.numeric(substr(as.character(datadates[which.min(si)*6-2]),5,7))
        maxmonth5<-as.numeric(substr(as.character(datadates[which.max(si)*6-1]),5,7))
        minmonth5<-as.numeric(substr(as.character(datadates[which.min(si)*6-1]),5,7))
        maxmonth6<-as.numeric(substr(as.character(datadates[which.max(si)*6]),5,7))
        minmonth6<-as.numeric(substr(as.character(datadates[which.min(si)*6]),5,7))
        

        
        mx<-paste0(numtomonth()[maxmonth1],"-",numtomonth()[maxmonth2],"-",numtomonth()[maxmonth3],"-",numtomonth()[maxmonth4],"-",numtomonth()[maxmonth5],"-",numtomonth()[maxmonth6]," period")
        mn<-paste0(numtomonth()[minmonth1],"-",numtomonth()[minmonth2],"-",numtomonth()[minmonth3],"-",numtomonth()[minmonth4],"-",numtomonth()[minmonth5],"-",numtomonth()[minmonth6]," period")
        cat(mx)
        cat(mn)
        res<-paste0("The best performing 6-Month period is the ",mx, " while the worst performing 6-Month period is the ",mn)
      }
    }
    res
  })
 
   output$taggtest<-renderText({
    inspectresult()
   })
  output$taggtable<-renderTable({
    DF<-as.data.frame(taggdata())
    DF
  })

  output$taggplot<-renderChart2({
    data<-as.data.frame(taggdata())
    data$Observations<-as.numeric(row.names(data))
    simsubmelt<-melt(data, id="Observations", factorsAsStrings=F)
    simsubmelt[,1] = as.vector(simsubmelt[,1] )
    simsubmelt[,2] = as.vector(simsubmelt[,2] )
    simsubmelt[,3] = as.vector(simsubmelt[,3] )
    h18 = hPlot( x="Observations", y="value",group="variable", data = simsubmelt, type = "line")
    h18$colors('rgba(63, 127, 191, 1)', 'rgba(63, 63, 191, 0.5)','rgba(36, 141, 183, 0.84)', 'rgba(52, 166, 136,1)', 'rgba(87, 100, 254, 0.6)' , 'rgba(25,25,112, 0.5)')
    return (h18)
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(OutputTable(), file,fileEncoding ='UTF-8',row.names=FALSE)
    }
  )
  output$downloadOutData <- downloadHandler(
    filename = function() {
      paste('outlierdata-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(OutliersTable1(), file,fileEncoding ='UTF-8',row.names=FALSE)
    }
  )
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('Report ', Sys.time() , ' ', Sys.Date(), sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('Report.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'Report.Rmd')
      
      out <- rmarkdown::render('Report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
})