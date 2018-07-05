library(rCharts)
library(shinydashboard)
library(D3TableFilter)
library(shinyjs)
library(data.tree)
library(networkD3)
library(DT)

shinyUI(
  dashboardPage( 
    dashboardHeader(

      title= "Omen",
      tags$li(class = "dropdown", actionLink("aboutme", icon("users")), style = "padding-top: 1px; padding-bottom: 1px; color: #fff;"),
      tags$li(class = "dropdown", actionLink("helpme", icon("question-circle")), style = "padding-top: 1px; padding-bottom: 1px; color: #fff;")
      
    ),
    dashboardSidebar( useShinyjs(), 
      sidebarMenu(id ="activetab",

                  menuItem("Home", tabName = "welcome", icon = icon("home")),
                  menuItem("Import time series", tabName = "home", icon = icon("gears")),
                  menuItem("Statistics", tabName = "stats", icon = icon("signal")),
                  menuItem("Missing data", tabName = "misd", icon = icon("pencil-square-o")),
                  menuItem("Handling outliers", tabName = "tso", icon = icon("area-chart")),
                  menuItem("Decomposition", tabName = "decompose", icon = icon("sitemap")),
                  menuItem("Smoothing", tabName = "smooth", icon = icon("level-down")),
                  menuItem("Multiple temporal aggregation", tabName = "tempag", icon = icon("cubes")),
                  menuItem("Forecasting", tabName = "data", icon = icon("line-chart")),
                  menuItem("Judgemental adjustments", tabName = "judge", icon = icon("arrows-v",lib = "font-awesome")),
                  menuItem("Error metrics & assessment", tabName = "errors", icon = icon("thumb-tack")),
                  hidden(menuItem("Hierarchical clustering", tabName = "hierarchy", icon = icon("toggle-down"))),
                  menuItem("Report", tabName = "export", icon = icon("download"))
                  
      )
    ),
    dashboardBody(  
      
      includeCSS("custom.css"),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        tabItem(tabName="welcome",
                column(width=7,
                       fluidRow(
                         h3("Login to your account, create a new one or, if you prefer, directly upload a file in the next page"),
                         fluidRow(
                           box(title="Home page",status="primary",width=12,
                               radioButtons("loginchoice", "Choose a sign option", c("Continue without an account"="GUEST", "Sign in"="IN", "Sign up"="UP"))),
                           conditionalPanel(
                             condition = "input.loginchoice == 'IN'",
                             box(  title="Login",status="primary",width=12,
                                   uiOutput('uiLogin'))),
                           conditionalPanel(
                             condition = "input.loginchoice == 'UP'",
                             box( status="primary",width=12,
                                  uiOutput('uiNewAccount')))
                         ))
                       
                ),
                column(width=5,
                       fluidRow(
                         uiOutput("userinfo"),uiOutput("timeseriesoptions")
                       ))
        ),
        tabItem(tabName="home",
                column(width=7,
                       fluidRow(
                         
                         box( title="Import time series page",status="primary",width=12,
                              fileInput('file1', 'Please choose a file to import',
                                        accept = c(
                                          'text/csv',
                                          'text/comma-separated-values',
                                          'text/tab-separated-values',
                                          'text/plain',
                                          '.csv',
                                          '.tsv'
                                        )
                              ),
                              checkboxInput('header', 'Header', TRUE),
                              radioButtons('sep', 'Separator',
                                           c(Comma=',',
                                             Semicolon=';',
                                             Tab='\t'),
                                           ','),
                              selectInput("ppyin", label="Data frequency", choices =c("Unknown"="Unknown", "1"=1, "2"=2, "3"=3, "4"=4, "12"=12)),
                              dateRangeInput("dates", label = "Date Selection", start  = "1983-01-02", end    = "1992-01-02",startview = "decade")
                              
                         )),
                       fluidRow(
                         
                         
                         box( title="View imported & edited time series",status="primary",width=12,
                              
                              showOutput("inputplot", "highcharts"),
                              HTML('<style>.rChart {width: 100%; height: 600px}</style>')))),
                fluidRow(column(width=3,
                                
                                fluidRow(
                                  conditionalPanel("input.loadsaved != 'None' || input.file1 !=NULL",
                                                   box( title="Save imported time series to my account",status="primary",width=12,
                                                        textInput("savefiletitle", "Time series name", ""),
                                                        actionButton("savefile1", "Save"),
                                                        textOutput("savetitlemsg")
                                                   )),
                                  box( title="View & edit imported data",status="primary",width=12,
                                       d3tfOutput('tbl2', height = "auto"),
                                       tableOutput("testeditd3"))
                                ),fluidRow())
                ))
        , tabItem(tabName = "data",
                column(width=8,
                       fluidRow(
                       	box(title="Intermittent Demand Index",width=12,status = "primary",
                             valueBoxOutput("IDI"),checkboxInput("considerasintermittent","Use intermittent demand forecast methods", value=FALSE)),
                         conditionalPanel(condition="input.considerasintermittent==false",
                                          box(title="Forecasting page",width=12, collapsible=T,  status="primary",
                                              
                                              
                                              selectInput("methodin", label="Choose a method", choices =c("Optimal", "Naive","MAPA", "ETS", "Theta", "ARIMA", "SES", "Holt", "Damped"), selected="Theta")
                                              ,
                                              conditionalPanel(
                                                condition = "input.methodin != 'MAPA'",
                                                checkboxInput("tain", label="Temporal Aggregation",value=F))
                                              ,
                                              selectInput("clin", label="Confidence level", choices =c("None"="none","99%"="2.58", "98%"="2.33", "95%"="1.96", "90%"="1.645", "80%"="1.28"),multiple=FALSE),
                                              sliderInput("horizonin", "Horizon",min = 1, max = 36, value = 3),
                                              selectInput("ppyin2", label="Frequency:", choices =c("Unknown"="Unknown", "1"=1, "2"=2, "3"=3, "4"=4, "12"=12))
                                              
                                          )),
                         conditionalPanel(condition="input.considerasintermittent==true", 
                                          box(title="Forecasting page",width=12, collapsible=T,  status="primary",
                                              selectInput("methodin2", label="Choose a method", choices =c("iMAPA", "SBA","TSB", "Croston"), selected="Croston"),
                                              sliderInput("horizonin2", "Horizon:",min = 1, max = 36, value = 3),
                                              checkboxInput("useinter", label="Use Intermittent Forecasts", value = FALSE)
                                              
                                          ))
                         
                       ),  
                       fluidRow(
                         conditionalPanel(condition="input.considerasintermittent==false",
                                          box(title="Historical data & forecasts",width=12, collapsible=T, status="primary", solidHeader = TRUE,
                                              showOutput("plot", "highcharts"), HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                                          )
                         ),
                         conditionalPanel(condition="input.considerasintermittent==true",
                                          box(title="Historical data & forecasts",width=12, collapsible=T, status="primary", solidHeader = TRUE,
                                              showOutput("interplot", "highcharts"), HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                                          )
                         )
                       )
                ),column(width=4,
                       fluidRow(
                         conditionalPanel(condition="input.considerasintermittent==false",
                                          box(title="Historical data & forecasts", width=12, collapsible=T,status="primary", solidHeader = F,div(style = 'overflow-x: scroll', tableOutput('table')), tableOutput('testtable'),div(style = 'height: 1200px;overflow-x: scroll;overflow-y:scroll', tableOutput("CLTable")),downloadLink('downloadData', 'Download')))
                       ),
                       conditionalPanel(condition="input.considerasintermittent==true",
                                        fluidRow(box(title="Data", width=12, collapsible=T,status="primary", solidHeader = TRUE,div(style = ' height: 1200px;overflow-x: scroll;overflow-y:scroll', tableOutput('table2'))))
                       )
                ),verbatimTextOutput('OutputTestForecast')
                ,fluidRow()
                
        ),
        tabItem(tabName = "decompose",
                column(width=8,
                fluidRow(
                  box(title="Decomposition page",width=12, status="primary",collapsible=T,
                      shinyjs::hidden(div(
                      selectInput("decfr", label="Data frequency", choices =c("Unknown"="Unknown", "1"=1, "2"=2, "3"=3, "4"=4, "12"=12)))),
                      valueBoxOutput("DecProb"),
                      textOutput("decompositiontext"),
                      uiOutput("considerdec")
                  )
                ),
                fluidRow(
                  box(title="Seasonally adjusted time series", status = "primary", solidHeader = TRUE,width=12, collapsible=T,  
                      showOutput("decPlot", "highcharts"),
                      HTML('<style>.rChart {width: 100%; height: 600px}</style>'))),
                fluidRow(
                  
                  box(title="Randomness", status = "primary", solidHeader = TRUE,width=6, collapsible=T,
                      showOutput("decRandomness", "highcharts"),
                      HTML('<style>.rChart {width: 100%; height: 600px}</style>')),
                  box(title="Cycle", status = "primary", solidHeader = TRUE,width=6, collapsible=T,
                      showOutput("decCyrcle", "highcharts"),
                      HTML('<style>.rChart {width: 100%; height: 600px}</style>'))
                ),
                fluidRow(
                  box(title="Trend", status = "primary", solidHeader = TRUE,width=6, collapsible=T, height = '50%', 
                      showOutput("decTrend", "highcharts"),
                      HTML('<style>.rChart {width: 100%; height: 600px}</style>')),
                  box(title="Seasonality", status = "primary", solidHeader = TRUE,width=6, collapsible=T,  height = '50%',
                      showOutput("decSeasonality", "highcharts"),
                      HTML('<style>.rChart {width: 100%; height: 600px}</style>'))
                ),
                fluidRow(
                  box(title="Seasonplot (Seasonal indexes)", status = "primary", solidHeader = TRUE,width=12, collapsible=T,
                      showOutput("decLE", "highcharts"),
                      HTML('<style>.rChart {width: 100%; height: 600px}</style>'), textOutput("testdec")   ))
                
                ),
                  column(width=4,
                         fluidRow(
                         box(title="Original & deseasonalized data", status = "primary", solidHeader = F,width=12, collapsible=T,  
                             tableOutput("decompRes"),
                             HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                         ))
                ),
                fluidRow()
                
        ),
        tabItem(tabName = "stats",
                box( title="Statistics page",status="primary",width=12),
                
                fluidRow(
                  valueBoxOutput("Minx"),
                  valueBoxOutput("MeanValue"),
                  valueBoxOutput("Maxx"))
                
                ,
                fluidRow(
                  valueBoxOutput("Quantile25"),
                  valueBoxOutput("MedianValue"),
                  valueBoxOutput("Quantile75"))
                ,
                fluidRow(
                  valueBoxOutput("SdValue"),
                  valueBoxOutput("Skewness"),
                  valueBoxOutput("Kurtosis"))
                ,
                fluidRow(
                  valueBoxOutput("CVValue"),
                  valueBoxOutput("RValue"),
                  valueBoxOutput("Obs"))
                ,
                fluidRow(
                  valueBoxOutput("Hurst"),
                  valueBoxOutput("Normality"),
                  valueBoxOutput("Stationarity"))
                ,
                fluidRow(
                  valueBoxOutput("TrendI"),
                  valueBoxOutput("SeasonalI"),
                  valueBoxOutput("RandomnessI"))
                ,
                fluidRow(
                  column(width=12,
                         box(title="ACF & PACF", status = "primary", solidHeader = TRUE, collapsible=T, width = 4,
                             div(style = 'overflow-x: scroll', tableOutput("acfdata"))),
                         box(title="ACF diagram", status = "primary", solidHeader = TRUE, collapsible=T, width = 4,
                             showOutput("acfplot", "highcharts"),
                             HTML('<style>.rChart {width: 100%; height: 600px}</style>')),
                         box(title="PACF diagram", status = "primary", solidHeader = TRUE, collapsible=T, width = 4,
                             showOutput("pacfplot", "highcharts"),
                             HTML('<style>.rChart {width: 100%; height: 600px}</style>'))
                  )),fluidRow(
                  column(width=12,
                         
                         box(title="Boxplot", status = "primary", solidHeader = TRUE, collapsible=T, width = 6,
                             showOutput("boxplot", "highcharts"),
                             HTML('<style>.rChart {width: 100%; height: 600px}</style>')),
                         box( title="Kernel density estimates",status="primary", solidHeader = TRUE, collapsible=T,  width = 6,
                              showOutput("densityplot", "highcharts"),
                              HTML('<style>.rChart {width: 100%; height: 600px}</style>'))
                         
                  )
                )
        ),
        tabItem(tabName = "tso",
                
                column(width=8,
                       fluidRow(
                         box(title="Handling outliers page",width=12,status="primary",solidHeader=F, collapsible=TRUE,
                             
                             
                             selectInput("outliersmethodin", label="Choose a method to handle your outliers", choices =c("By fitting a forecasting model & observing unususal values"="sea1",
                                                                                                                         "By examining the distribution of the forecasting errors"="sea2",
                                                                                                                         "By identifying extreme values"="sea3",
                                                                                                                         "By the Chen & Liu procedure"="tsoutliers")),
                             conditionalPanel(
                               condition = "input.outliersmethodin == 'tsoutliers'",
                               checkboxInput('ao', 'Additive Outliers (AO)', F),
                               checkboxInput('ls', 'Level Shifts (LS)', F),
                               checkboxInput('tc', 'Temporary Changes (TC)', F),br()),
                                conditionalPanel(
                                  condition = "input.outliersmethodin == 'sea1'",
                                  sliderInput("sea1a","Ta Value:",min=0,max=10,value=0),
                                  sliderInput("sea1b","Tb Value:",min=0,max=25,value=0)
                                ),
                               
                              conditionalPanel(
                                 condition = "input.outliersmethodin == 'sea2'",
                                 sliderInput("sea2a","Ta Value:",min=0,max=3,value=0)
                               ),

                             conditionalPanel(
                               condition = "input.outliersmethodin == 'sea3'",
                               sliderInput("sea3a","Ta Value:",min=0,max=5,value=0)
                             ),

                             checkboxInput('removeoutliersdec', "Consider Outlier Adjustments", FALSE)
                             
                         )
                       ),
                       fluidRow(box(title="Original & normalised time series",width=12,status="primary",solidHeader=TRUE, collapsible=TRUE,
                                    showOutput("OutliersPlot", "highcharts"),
                                    HTML('<style>.rChart {width: 100%; height: 600px}</style>')))
                ),
                
                column(width=4,
                       box(title="Original & normalised data",width=12,status="primary",solidHeader=F, collapsible=TRUE,
                           
                           div(style = ' height: 800px;overflow-x: scroll;overflow-y:scroll', tableOutput("OutliersTable")), downloadLink('downloadOutData', 'Download')
                       )),
                fluidRow()),
        tabItem(tabName = "misd",
                
                column(width=8,
                       fluidRow(uiOutput("missingvaluesbox")),
                       fluidRow( uiOutput("missingvaluesplot"))
                       
                ),
                
                column(width=4,
                       fluidRow(uiOutput("missingvaluestablebox"))
                ),
                
                fluidRow()
                
        ),
        tabItem(tabName="errors",
                box(title="Error metrics & assessment page",width=12, collapsible=T,  status="primary"),
                fluidRow(
                  box(title="Insample-Forecast Model Errors",width=3,status="primary",solidHeader=TRUE, collapsible=TRUE,
                      div(style = 'overflow-x: scroll', tableOutput("errortable"))
                      
                  ),
                  box(title="Out Of Sample-Forecasts Errors",width=3,status="primary",solidHeader=TRUE, collapsible=TRUE,
                      div(style = 'overflow-x: scroll', tableOutput("forecasterrortable"))
                      
                  )
                ),
                fluidRow(
                  box(title="Insample-Forecast Model Scatter Plot",status="primary",solidHeader=TRUE, collapsible=TRUE,
                      showOutput("errorscatter", "highcharts"),
                      HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                  ),
                  box(title="Out Of Sample-Forecast Scatter Plot",status="primary",solidHeader=TRUE, collapsible=TRUE,
                      showOutput("forecasterrorscatter", "highcharts"),
                      HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                  )
                ),
                fluidRow(
                  box(title="Error Plot",status="primary",solidHeader=TRUE, collapsible=TRUE,
                      showOutput("mescatter", "highcharts"),
                      HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                  ),
                  box(title="Forecast Error Plot",status="primary",solidHeader=TRUE, collapsible=TRUE,
                      showOutput("forecastmescatter", "highcharts"),
                      HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                  )
                )
        ),
        tabItem(tabName = "smooth",
                fluidRow(
                  column(width=8,
                         box(title="Smoothing page",status="primary",width=12,solidHeader=F, collapsible=TRUE,
                             selectInput("smoothmethodin", label="Choose a method to smoothen your data", choices =c("Non Linear Smooth"="nonlinearsmooth", "Simple Moving Average"="KMO")),
                             conditionalPanel(
                               condition = "input.smoothmethodin == 'KMO'",
                               sliderInput("imain", "Order of moving average smoother",min = 3, max = 12, value = 4)),
                             checkboxInput("considersmoothing", label="Consider Smoothing Adjustments",value = F)
                             
                         ),
                         box(title="Original & smoothened time series",width=12,status="primary",solidHeader=TRUE, collapsible=TRUE,
                             
                             showOutput("plotsm", "highcharts"),
                             HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                         )
                         
                  ),
                  column(width=4,
                         
                         fluidRow(box(title="Original & smoothened data",width=12,status="primary",solidHeader=FALSE, collapsible=TRUE,
                                      div(style = ' height: 800px;overflow-x: scroll;overflow-y:scroll', tableOutput("smtab2"))
                         ))
                  )
                ),
                fluidRow()),
        
        
        hidden(tabItem(tabName = "hierarchy",
                fluidRow(
                  column(width=8,
                         box(title="Hierarchical clustering page", status="primary", width=12, solidHeader=F, collapsible=TRUE, fileInput("datasetcsv","Please upload a dataset")),
                         box(title="Hierarchical parameters",status="primary", width=12, solidHeader=F, collapsible=TRUE,
                             sliderInput("hiervarin", "Number of nodes", min = 2, max = 12, value = 2),
                             uiOutput('hiertsin'),
                             tags$script("
                      Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                        Shiny.onInputChange(variableName, null);
                      });
                    "),
                             radioButtons("choosebutd", label = "Choose an aggregation method",
                                          choices = list("Bottom Up" = 1, "Top Down" = 2), 
                                          selected = 2),
                             sliderInput("horizonhier", "Horizon",min = 1, max = 36, value = 6)
                             ,
                             box(title="Plot",width=12,status="primary",solidHeader=TRUE, collapsible=TRUE,
                                 
                                 forceNetworkOutput("myNetwork"),
                                 HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                             )),
                         box(title="Results",width=12,status="primary",solidHeader=FALSE, collapsible=TRUE,
                             
                             div(style = ' height: 800px;overflow-x: scroll;overflow-y:scroll',tableOutput('hierforecastresults'))
                         ))
                  
                  ,
                  column(width=4,
                         
                         fluidRow(
                           box(title="Table",width=12,status="primary",solidHeader=FALSE, collapsible=TRUE,
                               uiOutput("selectparent"),
                               
                               tableOutput('uishowdetailedhier')
                           )
                         )
                  )),
                fluidRow())),
        
        
        tabItem(tabName = "tempag",
                box(title="Multiple temporal aggegation page",width=12, collapsible=T,  status="primary", uiOutput("consultagg"),textOutput("taggtest"),textOutput("tempagtest")),
                fluidRow(
                  column(width=8,
                         box(title="Time series across different temporal aggregation levels",width=12,status="primary",solidHeader=TRUE, collapsible=TRUE,
                             showOutput("taggplot", "highcharts"),
                             HTML('<style>.rChart {width: 100%; height: 600px}</style>'))),
                  column(width=4,
                         
                         box(title="Original & temporally aggregated data",width=12,status="primary",solidHeader=F, collapsible=TRUE,
                             div(style = ' height: 600px;overflow-x: scroll;overflow-y:scroll', tableOutput('taggtable'))))
                )
        ),
        tabItem(tabName="judge",
                box(title="Judgmental adjustments page",width=12, collapsible=T,  status="primary"),
                (column(width=8,
                        box( title="Original forecasts & adjustments",width=12,status="primary",solidHeader = TRUE,checkboxInput('usejudgemental', 'Use judgement', TRUE),  showOutput("judgeplot", "highcharts"), tags$head(tags$script(src="draggable-points.js")),tags$head(tags$script(src="export-csv.js"))
                             ,
                             HTML('<style>.rChart {width: 100%; height: 600px}</style>'),
                             div(style = 'overflow-x: scroll', tableOutput("judgetable")),tableOutput("testjtable"))
                )),
                fluidRow(
                  column(width=4,
                         box( title="Judgemental adjustments",width=12,status="primary", solidHeader = F, uiOutput("sliders"))
                  )
                )
        ),
        tabItem(tabName="export",
                fluidRow(column(width=6,
                                box( title="Report page",width=12,status="primary", solidHeader = F, 
                                     textInput("reportitle", "Report title", "Timeseries Forecasting"),
                                     radioButtons('format', 'Document format', c('Word'), inline = TRUE),
                                     downloadButton('downloadReport'))
                ),
                (column(width=6,
                        hidden(box( title="File Preview",width=12,status="primary",solidHeader = TRUE
                        ))
                )))
        )
        
      )
    )
  )
)
