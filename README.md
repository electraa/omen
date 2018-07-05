# OMEN: Promoting forecasting support systems
```
∗Corresponding author
Email address: electra@fsu.gr(Ilektra Skepetari)
```

```
Ilektra Skepetari∗, Evangelos Spiliotis^1 , Achilleas Raptis^1 , Vassilios Assimakopoulos
Forecasting and Strategy Unit, School of Electrical and Computer Engineering, 
National Technical University of Athens, 
9 Iroon Polytechniou Str, 15773 Zografou Athens, Greece
```
### Abstract

Nowadays, forecasting is of utmost importance when it comes to planning, which is the backbone of every successful company. In order to help companies in their forecasting and decision-making procedures, many Forecasting Support Systems have been developed through the years. While these systems can be proven quite helpful in general, it seems that they tend to be deficient both in technological and methodological aspect. In the present paper we indicate that the main vulnerabilities of typical ’off-the-shelf’ solutions are their limited customizability, non web-based architecture and poor user interface, although
outdated methods and lack of the judgmental component are also issues of significant importance. We also make some suggestions regarding the way a system can be involved and set open-source solutions as a key parameter for dealing with all the problems mentioned above. In this respect we present OMEN, a fully customizable web-based forecasting tool which uses exclusively open-source solutions and a modern interface to support companies and practitioners. The feautures of OMEN are used to demonstrate the advantages of its principles and structure and all its modules, including data mining, data pre-processing and
forecasting, are exposed to promote changes in modern Forecasting Support Systems. 
```
Keywords: Forecasting support systems, Shiny, R, web-based FSS, OMEN
```

### Introduction

Forecasting plays a pivotal role in the operations of modern management. It is an important and necessary aid to planning and planning is the backbone of effective operations. In this respect, numerous Forecasting Support Systems (FSSs) have been developed through the years to assist companies apply forecasting procedures and support managerial decisions. While the majority of the typical off-the-self solutions are quite powerful and generic, they tend to be deficient in three key aspects: Inadequate web-based architecture, lack of customizability, and poor user interfaces. As far to our knowledge, most FSSs are currently windows based applications, some of which are even locally installed. A way forward is the replacement of these outdated applications, with web-based applications. This replacement will offer a number of advantages, such as 24/7 accessibility and availability, cross-platform and device compatibility, and no
need for installing additional software or manual updating. Customizability is also a feature promoted through web-based solutions, an issue of significant importance given that each company has its special forecasting needs. FSSs should be able to be adjusted according to the requirements, the products, the systems and the nature of the company considering different architectures, interfaces, forecasting models etc. This is much easier to be done
through web-based applications given their flexibility and the implementation of any change on the server-side.
Moreover, the existing FSSs do not provide the user with state-of-the-art methods including data collection, pre-processing, forecast modeling, model selection and performance evaluation as better approaches become available. This is mainly due to the difficulty of the developers to implement or update methods of high complexity to their systems which, even when done, is a time intensive process. As a result the forecasting abilities of the FSSs always
lack comparing with the academic standards, a disappointing fact given that research is done for improving and supporting processes in real life applications. To accomplish customizability and reduce the time needed for developing simultaneously, the use of open-source solutions is suggested. For instance, the R [37] statistical software is publicly available and has numerous advantages. The enthusiastic and exponentially growing user base offers for free not only ready-to-use functions and state-of-the-art methods but also invaluable advice. Currently, there are approximately 180 time series related packages, including among others
forecasting, univariate and multivariate modelling, decomposition, and dynamic regression models. The functions are well-documented and open-source, which certainly increases the acceptability of the users. Python is also a great solution since its usage has grown over time and includes functions for almost any statistical operation, especially after the introduction of pandas [32] for operating on structured data. Furthermore, modern organizations and utilities have to produce in daily basis large numbers of forecasts. This requires the identification and parameterization of appropriate models for each series, a process which must be performed automatically to reduce the associated workload. On the other hand, manual or semi-automatic processes may be required when an expert is using the system for producing forecasts on a detailed level. In both cases, and given that the user often has negligible forecasting background, a user friendly
interface is required in order to fully exploit the capabilities of the system. Simplicity and documentation are key elements here and should be achieved through a comprehensible and modern solution.
In this paper we try to demonstrate how to deal with the above mentioned issues by presenting OMEN^1 , a fully customizable web-based FSS using exclusively open-source solutions and a modern interface. The tool is built on Shiny [26] under R and is able to capture data from multiple data sources, apply data cleansing and pre-processing and produce forecasts using state-of-the-art methods and models. Forecasting can be performed automatically or
manually according to the needs of the user, providing relative error metrics and statistics. The rest of the paper is organized as follows: Section 2 describes the structure of the tool, including the architecture of the system and its implementation through Shiny, R. In section  the many features and functionalities of OMEN are outlined, while a demonstration of the system and a discussion is performed in section 4. Finally, concluding remarks and future perspectives are given in section 5.

### System architecture

In this section, a detailed description of the structure and the architecture of the system is presented. OMEN consists of two separate entities, the database server and the application web server. The first one is a data warehouse where all the data of OMEN users are stored while the other is used for the smooth and uninterrupted execution of the R code.

#### Database Server (Data Base)

Omen users are able to feed data to the system using two different procedures. The first and the simplest one is by using the integrated import functionality and upload well-structured files directly to the system. The alternative one is by using OMEN Database Server. The strong point and significant advantage of Database Server is that allows users not only to import bigger files but also to save their data for future use or reference. Omen Database Server, which is deployed based on MySQL database, contains both raw user data, the transformed data and the final results, all structured in time series format.

#### Application Web Server (R & R libraries)

The whole OMEN tool is based on R [37], a language that during the last decade has become one of the most popular and important tools for computational statistics and data science. A fundamental advantage of R is that it is supported by a growing community of more than 2 million users and thousands of developers worldwide which has developed and distributes hundreds of open source packages. In order to build an integrated solution, except the built-in functions of Rs base package, the following sophisticated packages have been used:

- Shiny: Shiny [26] is an open source R package which provides a robust web framework  for building web applications using R. Using this package we were able to slightly  transform our R code and create a powerful web application which is accessible through  any modern web browser, independently of the device used.
- shinydashboard: shinydashboard [6] is an ad hoc package provides numerous design  features on top of ’Shiny’, making it easy to create attractive dashboards.
- RMySQL: As mentioned above, all the useful data for OMEN tool are stored, after their transformation and cleansing, in a MySQL database. RMySQL package [17] is  a database adapter that enables the connection between R and the MySQL database.
    Through this package and with the aid of appropriate SQL queries, OMEN communicates directly with the database and retrieves the required data based on users choices.


- tsoutliers: One of the transformations that OMEN provides is the automatic detection of timeseries outliers [11]. Due to the manipulation of such data abnormalities, like  innovative outliers, additive outliers, level shifts, etc. the incorporation of tsoutliers package was of the utmost importance.
- forecast: Forecast package [19] is an integrated open source package developed by Rob Hyndman which provides algorithms and tools for time series analysis and forecast.  Despite the copious features of this package, for the development of our tool 2 basic functions have been used: ets() and accuracy(). The first one provides automated forecasting using exponential smoothing models and the second one the accurate calculation of forecast errors.
- MAPA: MAPA package [25] by Nikolaos Kourentzes and Fotios Petropoulos is designed for using the Multiple Aggregation Prediction Algorithm (MAPA) for timeseries forecasting.
- tsintermittent: Another package developed by Kourentzes and Petropoulos [28] for analyzing and forecasting intermittent demand time series and slow moving items is used for the manipulation of intermittent demand data and has been integrated to OMEN mechanisms.
- rCharts: rCharts [42] is a R package, developed and maintained by Vaidyanathan to create and publish interactive visualizations through R. Although rCharts supports multiple javascript charting libraries, such as MorrisJS, NVD3 and xCharts, many others including the HighCharts library have been used for the development of this  application due to its interactive features.
- RMarkdown: RMarkdown [1] is another RStudios package that enables easy creation  of dynamic documents, presentations, and reports through R. By using RMarkdown package OMEN allows users to produce reports in multiple formats such as .doc, .pdf and .html files.

Our code is configured to work with shiny Web Server Open Source Edition using a CentOS 6.4 x86 server accessible via the Internet. The strong point of shiny Web Server (Open Source Edition) is that multiple shiny applications sessions can be handled simultaneously, hence two or more users are able to use Omen simultaneously without installing any application on their system and therefore without facing any compatibility issue.
Finally, it is mentioned that the graphic user interface is based on HTML and CSS and it has been responsive designed to fits the screen of any device. An abstract approach of OMEN tool is presented in Figure 1.


![alt text](https://preview.ibb.co/b5sGwd/architecture.png)
```
Figure 1. Architecture of OMEN
```
### The modules of the system

Within this section the modules of OMEN are presented, indicating the main features
of the system and its capabilities. The modules aim into analyzing and effectively pre-
processing the data in order adequate decisions to be made and the time series to be prop-
erly prepared for forecasting. OMEN supports a variety of transformations, adjustments,
decomposition and forecasting methods applicable both to continuous and intermittent de-
mand time series, while judgmental adjustments and a detailed assessment frameworks is
provided for better evaluating and supporting the final forecasts of the users.
In order to simplify the forecasting process and enable forecasting for practitioners with
limited statistical knowledge, the modules of OMEN (menu items) are sequentially linked
demonstrating which parameters should be defined till forecasting is completed. The mod-
ules are accessed and handled through a user-friendly and dynamic interface developed under
Shiny, while all the processes are based on R scripts developed by the authors and available
R packages published by leading academics and researches. This boosts the customizability
of the system, bringing easily state of the art methods close to forecasters via a web-based
and interactive solution. An overview of the modules of OMEN is presented inFigure 2,
while a summary of their aim and content is given as follows:

Import Time Series:This menu item enables the user to import a time series to OMEN
in order to begin the forecasting procedure. The time series can be imported either through
a csv file, or through the database of the system. In the first case the uploaded file must
include two columns: one for the timestamp of the observations and one for the data. The
user must also specify whether the file includes titles, as well as the character used to separate
the fields(Comma, Semicolon or Tab). In the second case the time series which have been
previously uploaded and saved by the user are displayed and one can be directly selected to
retrieve the data needed.


The frequency of the selected time series can be defined by the user or set automatically
by the system based on the autocorrelation of the data. In the latter case, a seasonality test
based on the autocorrelation coefficients is performed for each possible frequency and their
significance is then evaluated suggesting the one with the highest value. However, in order
a time series to be considered as seasonal a 90% confidence must be reported. The sample
of data which will be used during the forecasting procedure(in-sample)can be selected
chronically through a calendar. This will also indicate the forecast origin. In case more data
are available after the selected timestamp(out-sample), OMEN will use them to assess more
effectively the forecasting performance.

Statistics: At this point a high level statistical analysis is performed to the imported
time series in order the user to inspect the primary characteristics of the data. Apart
from the basic statistics (mean, median, variance, linear correlation coefficient etc.), the
autocorrelation and the partial autocorrelation factors are visualized in order to indicate
possible seasonal patterns and trend. A boxplot can also be used to detect outliers or
high variations and suggest relative pre-processing such as transformations and handling
of extreme values. Finally, the intermittent demand interval (interval of zero values) is
calculated to inform whether continuous or intermittent demand methods should be used
for forecasting.

Figure 2. The simplified interface of OMEN organized in practical and comprehensive menu


```
items. The selected menu item displays the forecasting module for continuous time series.
```
Transformations:In case of high variations, transformations should be used to rescale the
historical data, simplify their patterns and make them more consistent across the whole data
set [36]. This can enhance the whole forecasting performance since usually simpler patterns
lead to more accurate forecasts [5]. OMEN offers a variety of power transformations which
can be applied manually by adjusting the lambda parameter of the Box-Cox transformation.
Well-known transformations such as square roots, cube roots and logarithms are directly
provided though drop-down menus for ease of use. To further assist the user, Guerrero’s
[16] method can be applied to automatically select the lambda parameter, by minimizing
the coefficient of variation for subseries of the original one.

Handling Outliers:Abnormalities in time series can spoil the patterns of the time series
and introduce a significant penalty to the forecasting performance. This is mainly caused
due to a carry-over effect of the outlier on the forecast and a bias in the estimates of the
model parameters [30]. When referring to extreme values, transformations usually manage
to mitigate this effect, however rescaled data can still include outliers, especially when
referring to level shifts or the selected lambda parameter is close to one and insignificant
differences are reported between transformed and original data. In this tab the detection
approach of Chen and Liu [7] is applied through thetsoutliers R package [11] in order
to detect outliers and normalize the time series. Such outliers may be Additive Outliers
(”AO”), Level Shifts (”LS”) or Temporary Changes (”TC”). The selections of the user will
determine which of the abovementioned types of outliers will be considered and removed if
found.

Decompose: OMEN can be used to decompose the adjusted time series and provide a
clear picture of its components which, as defined through the classical decomposition [31],
are distinguished into the seasonal, trend, cyclical and error (randomness) component [9].
This feature can also be exploited to boost the forecasting performance since, in cases of
significant seasonality and adoption of non-seasonal forecasting models, deseasonalization
and seasonalization of the time series before and after forecasting can become quite beneficial.
In this respect, the classical multiplicative decomposition by moving averages is applied [23]
and seasonal indexes are calculated for the selected frequency. In order to assist the user
decide whether the time series should be deseasonalized, the system visualizes the seasonal
ratios per frequency period forming a seasonal-plot. Overlapping seasonal ratios indicate
significant seasonal patterns, and vice versa. The probability of a seasonal pattern is also
reported.


Figure 3: Left: Detection of additive outliers and level shifts in a monthly time series. Right:
Smoothed time series after applying a moving average of 7.

Smoothing:Following the decomposition of the time series, this menu item offers the user a
list of methods which can be applied to smooth the adjusted time series. Through smoothing,
the component of noise will be shrinked or completely eliminated, emphasizing that way
the useful characteristics of the time series (level and trend) and leading to potentially
better forecasts. The first of the provided methods computes a simple moving average
of a specified order (centered for even orders) [20], while the second one is smoothing
using the NadarayaWatson kernel regression estimate [33, 43]. The user can adapt the
level of smoothing by adjusting the order of the moving average and the scale of the kernels
bandwidth, respectively. In both cases, forecasting and backcasting is used to fill the missing
values introduced at the front and the end of the smoothed time series using the exponential
smoothing state space model (ETS) [22]. The third option of the user is a non-linear
smoothing mechanism based on the theta transformation for identifying long term trends
introduced by Assimakopoulos [3] according to which strong local variances are detected
and gradually limited based on the values of the neighboring observations.

Temporal aggregation:Temporal aggregation is a method for transforming the original
data to alternative time frequencies. In this respect, given a monthly time series, a semi-
annual, a quarterly and a yearly time series can be created by aggregating accordingly the
original observations on the axis of time. This emphasizes different time series characteristics
per aggregation level [2], such as seasonality, level and trend, leading to significant improve-
ments in forecasting accuracy even when simple forecasting models are used to produce the
individual forecast. In lower aggregation levels, high frequency components like seasonality
are dominant, while as the aggregation level increases low frequency components, such as
level and trend, are becoming more clear [29]. A variety of different approaches for imple-
menting temporal aggregation can be found in the literature according to the sample used to
produce the aggregated time series (overlapping or non overlapping) and the method chosen


for combining the individual forecasts (mean, median, adjusted weights etc.). Within OMEN
a simple approach for implementing temporal aggregation has been developed: Based on
the selected (or detected) frequency the time series is aggregated to all possible higher levels
and forecasts are produced using the exponential smoothing family of models [14]. The
individual forecasts are then combined using equal weights. The aggregated time series are
visualized in a common diagram to highlight the effect described above, as shown inFigure
4.

Figure 4: Left: Implementation of temporal aggregation through OMEN. A monthly time
series is aggregated to all possible time levels to emphasize different characteristics per ag-
gregation level. Right: Judgmental adjustments of original forecasts using the JavaScript
based drag option.

Forecasting (Continuous Time Series):This tab is the heart of OMEN since, following
the analysis and the pre-process of the original time series, the user can now decide which
forecasting methods will be used to predict. Depending on the selections made throughout
the forecasting process, the time series can be either the original, or an adjusted one calcu-
lated after the normalization, deseasonalization and smoothing processes have taken place.
Seasonal models are considered only in case deseasonalization has not been applied. The
toolbox includes both traditional and state of the art methods as follows:

1. Naive: The simplest way to predict. The forecasts are equal to the last known obser-
    vation.
2. Exponential Smoothing: The best-fitting model from the exponential smoothing family
    of methods [14] is selected based on the minimization of the Akaike Information
    Criteria (AIC) [38]. The ETS [22] function of the forecast package is used for
    automating the process. Popular models of the family, such as Simple Exponential
    Smoothing(SES), Holt and Damped [15], are directly available.
3. Theta: The Theta decomposition method, well-known for outperforming its competi-
    tors in the M3-competition [4].


4. Autoregressive Integrated Moving Average models (ARIMA): The best-fitting ARIMA
    model is selected based on the minimization of the AIC. The auto.arima [18] function
    of theforecastpackage is used for automating the process.
5. Multiple Aggregation Prediction Algorithm (MAPA): Another way to apply temporal
    aggregation. The main difference between MAPA [26] and the typical approach pre-
    sented in the’Temporal aggregation’tab is that instead of combining the forecasts from
    the individual time levels, forecasts are decomposed to their time series components
    and the combination of these generates the final forecast. The robustness and the
    performance of MAPA has been reported across various studies and data sets [29, 27].
6. Optimal: This selection can be used as an’Auto-forecast’option in cases when fore-
    casts must be calculated quickly or the user is a practitioner with little knowledge
    on forecasting. The abovementioned methods are applied on the time series. Us-
    ing the rolling origin evaluation procedure [40] with the validation window matching
    the forecasting horizon, the performance of each method is evaluated based on the
    minimization of the squared errors and the ’best fitted’ model is selected. Given the
    customizability of OMEN and its compatibility with state of the art research, different
    ’Auto-forecast’ mechanisms can become available, namely the multi-step ahead rolling
    origins, as proposed by Fildes and Petropoulos [12].

Forecasting (Intermittent Demand Time Series): Intermittent demand time series,
which dominate service and inventories in many industries, involve infrequently requested
items resulting in sporadic demand. Such time series cannot be effectively handled by the
methods included in the previous list for continuous data. As literature indicates, Croston
[8] is the standard method to forecast intermittent demand. However, SBA [39] can be
used to approximately limit its positive bias by the proposal of estimator and TSB [41] to
produce unbiased forecasts for all points in time by updating the demand probability instead
of the demand interval. iMAPA [35] is also an alternative for introducing the advantages
of temporal aggregation and MAPA to intermittent demand. OMEN includes all of the
abovementioned methods and applyes them through thetsintermittent [28] package while
state of the art approaches [24] are used for optimizing their parameters and enhancing
their performance.

Judgmental adjustments: Forecast adjustment commonly occurs when organizational
forecasters adjust a statistical forecast of demand to take into account factors which are
excluded from the statistical calculation [10]. However adjustments can also become useful
in cases of limited historical data where statistical forecasting may be proven quite ineffec-
tive or when forecasting is performed by a group of experts. OMEN offers the ability of
adjusting the statistically produced predictions either manually by typing the desired value,
or graphically by dragging the point forecasts of a JavaScript based diagram, as presented
in Figure 4.

Error Metrics and Assessment:This menu item provides the user with a completed re-
port regarding the performance of the total forecasting procedure by assessing its forecasting


accuracy and bias through multiple error metrics. An in-sample and out-of-sample evalua-
tion can both become available based on the sample used to train the models when the time
series was imported. Namely, Mean Error (ME) and Mean Percentage Error (MPE) can
be used to evaluates the model in terms of bias, while Mean Absolute Error (MAE), Mean
Squared Error (MSE), Mean Absolute Percentage Error (MAPE) and symmetric Mean Ab-
solute Error (sMAPE) in terms of accuracy. Mean Absolute Scaled Error (MAsE) is also
calculated, given the recommendations reported by Hyndman and Koehler [21] and more
recently confirmed by Franses [13]. Scatter plots and relative diagrams can additionally be
used to visualize potential bias, large point errors and patterns among the errors.

4. Discussion

Forecasting Support Systems refer to a set of processes with an objective of accurately
forecasting variables of interest within a company. Such systems may not be limited to a set
of statistical methods, but also provide support for interactions between statistical outputs
and management judgment, as well as procedures for storing, retrieving and presenting
information in an effective way. To do so, an accurate and rapid system is required, providing
detailed forecasts in a user-friendly, efficient and intuitive environment.
In that perspective, OMEN, an integrated analysis and forecasting tool was introduced.
In the present section we attempt to explain the limitations of typical off-the-shelf solutions
and justify how the adoption of some of OMEN’s feautures could promote modern FSSs.The
innovation of OMEN lies in two main aspects: Technological and Methodological. The
first axis refers to the exploitation of modern solutions, such as web-based technology, to
expand the flexibility, the customizability and the effectiveness of the tool. The second axis
includes technical infrastructures for implementing and promoting state-of-the-art methods
and models.

4.1. Technological Aspect

4.1.1. Web-Based Solution

Typical FSSs are developed as windows-based applications and installed locally on the
system of the users. In this regard, data are stored on each system separately and each user
forecasts individually specific time series. The individual forecasts are then gathered and
sent to the manager in charge. In more advanced solutions the raw data and the forecasts
are stored on a server. However the forecasting process continues to be performed locally
increasing that way the complexity of the solution and its limitations in terms of availability,
accessibility, compatibility and use of resources.
Moving on to a web-based application like OMEN, offers the solution to all the problems
mentioned above. First of all, web-based applications provide multiple users with remote
and 24-7 unlimited access from any device of their choice. This means that the users can
work simultaneously, anywhere and anytime using the device of their choice. The solution
is always available and operational even if the user is out of office. Moreover, since all the
processes are performed on the server, the requirements for using the application become
negligible transforming even a simple smart phone into an integrated FSS.


Additionally, OMEN connects directly to the companys servers to simplify data handling.
Any information needed is already stored in the system’s database and any authorized user
can use them for analysis and forecasting without importing any additional files. No in-
stallations or updates are required and the data of interest are always up-to-date for all
the users. Any system update is performed by the administrator of OMEN directly and
is automatically delivered to all users. Central administration also improves the customiz-
ability and the interoperability of the system, a crucial issue given that each company has
unique forecasting needs. For example, an FSS should allow for a customizable presentation
tier allowing different companies or even different managerial levels or work-groups within a
company to maximize their efficiency by using it while, depending on the product and the
nature of the company, different forecasting models should be considered per case.

4.1.2. Customizability
As each company has its very individual forecasting needs, customizability becomes a
significant issue. For instance, forecasting for retailers demand essentially differs to forecast-
ing energy consumption both in terms of data mining and handling and forecasting methods
and models. Undoubtedly there can be not one FSS to fit them all but it should be easy to
customize according to the examined application. At present, most of off-the-self solutions
are very generic trying to serve multiple types of companies. This leads to the development
of extremely complex systems which on the one hand make their use very complicated for a
typical user, and on the other hand do not ensure than all the needs and the requirements
of the company using it are delivered.
To overcome these problems OMEN is developed using exclusively open source solutions
and more specifically R. To date, more than 2000 packages extending the R language in ev-
ery domain are available, with more added every day. The community of R is continuously
increasing and evolving offering not only ready-to-use functions but also free documentation
and help for the provided solutions. This helps the system to be easily adjusted based on the
requirements of the company since the ready-to-use functions can be exploited to decrease
the time of developing, while little coding is required for combining the individual services.
Most important, the administrator is not restricted to using pre-defined and contributed
by other users set of R routines but can extend with his own functions. Connections with
other useful applications such as MySQL database, Apache web-server and the Google Maps
API are also possible expanding the customizability and the opportunities of the system.
Moreover, as mentions in the previous sub-section, since OMEN is web-based, the cod-
ing is performed only by the administrator of the system and any change is immediately
implemented on the application and becomes available to all users.

4.1.3. User Interface
The interface is one of the most crucial components of a FSS, connecting the user with
the engine of the system. In some cases, however, commercial FSSs focus on the forecasting
engine, to the detriment of an efficient and user-friendly interface. This phenomenon is
connected with the fact that developers try to build a generic FSS able to serve different users
of diverse needs. In such cases, the interface of the FSS ends up with many needless options,


models, tools and features, transforming it into a complicated, confusing, and ineffective
solution. Developers should keep in mind that, like most of the ERPs, their FSS is most
likely to be used by practitioners who are inexperienced in the field of forecasting and that
simplicity is the key towards their convenience.
In this respect, we tried to keep the interface of OMEN as simple as possible, offering
only capabilities which are useful and instantly implementable by the final user. Since
the purpose of the FSS is to provide non-experts with forecasts of specific variables for
a selection of consumption intervals and areas, the interface of OMEN provides only the
necessary toolbox to allow the user to customize the forecasting process, while conveniently
setting all decision-related options.
More specifically, OMEN is built under Shiny, an open-source package that provides a
robust web framework for constructing web-based applications using R. Shiny enables the
application to be organized in menu items supporting different data handling and forecasting
processes. Given its structure, OMEN is accessible remotely by any authorized user and
indicates the steps that the user should follow to produce accurate forecasts in a clear way.
To further assist the user, a help item is available in every menu item demonstrating its
role within the forecasting process and providing useful information for its abilities. Among
others, also provides illustrative visualizations generated using the rCharts library, which
renders them interactive. The user can zoom, remove the available lines and display the
values or timestamps. Simple active buttons, such as radio buttons, check boxes, drop down
menus, calendars and sliders further assist the interaction between the system and the user.

4.2. Methodological Aspect

4.2.1. State-of-the-art methods and frameworks

It is true that to increase forecasting performance, a company must implement the state-
of-the-art methods and models for data collection, cleansing, forecasting and validation.
However, given the complexity of such algorithms, it becomes clear that developers of FSSs
have trouble in including them is their systems. Therefore, they either decide to exclude
them from their solutions, or they spend many man-months in coding and developing. Open-
source solutions can help mitigate this phenomenon.
Leading academics and researches from around the world use R and other open-source
language to develop the latest methods in statistics, machine learning, and predictive mod-
eling. There are expansive, cutting-edge edge extensions to R in finance, economics, and
dozens of other fields included in more that 2000 packages. As presented in section 3, OMEN
exploits many time series related packages offering both classic and state-of-the-art methods
and tools for displaying and analysing effectively time series forecasts.
Characteristic examples consist the cutting-edge functions for automatic selection algo-
rithms for exponential smoothing [22] and ARIMA [19] models, as well as the Theta method
[4]. The last one is quite blatant firstly due to the low complexity of the model, and sec-
ondly given that the Theta method outperformed all the competitor methods and FSSs in
the M3-Competition. These models, as well as the rest of the R functions, can be used right
after the release from their developers and implemented directly in OMEN providing the
users the most the forecasting society can offer.


4.2.2. Intermittent demand forecasting

Intermittent demand patterns occur quite often in a companys practice; for example in
spare parts. Given the zero values across the time series, these data need special treat-
ment when forecasted in order an adequate performance to be achieved and relative models
should be applied. However, most forecasting software packages provide little to no sup-
port in handling intermittent demand data and in some cases do not integrate any methods
specifically designed for forecasting such data. Only inventory software include techniques
for intermittent demand, as well as classification strategies suggested in the literature.
OMEN is built to satisfy the needs of inventory and retail companies and, as the need
of forecasting intermittent demand data is common in practice, it includes many functions
of the tsintermittent package of R [28] to do so. The package, as mentioned in section
3, is appropriate for analysing and forecasting intermittent demand or slow moving items
time series and includes both the state-of-the-art forecasting models and methods for their
optimal parameterization.

4.2.3. Temporal aggregation

An additional technique that most FFSs seem to ignore, is the one of temporal aggrega-
tion, which also seems to produce accurate results as forecasting the data not only at one
but on multiple aggregation levels has been proven to increase forecasting accuracy for both
fast- and slow-moving time series [29]. As far to our knowledge, SAS recently became the
first of the off-the-shelf FSSs to include a form of temporal aggregation among its forecasting
models.
In this respect, OMEN has one tab dedicated to the temporal aggregation technique so
that the user can see how the original data transform across different frequencies and capture
different series components per level. An appropriate model from the systems toolbox can be
selected to produce forecasts for each level and then the individual forecasts are combined to
boost forecasting performance. Finally, as mentioned earlier, multiple temporal aggregation
is available both for continuous and intermittent demand time series mainly for cases that
model selection becomes an issue and reliable automated forecasts are needed across a large
number of time series.

4.2.4. Judgment
Judgmental forecasting is vital in modern organizations, especially when insufficient
amount of data are available for producing reliable statistical forecasts, noisy series are pro-
vided or valuable external information must be taken into consideration. Typical examples
include forecasting sales while promoting products or a forthcoming special event must be
taken into account. In cases like this an expert or a group of decision makers are responsible
for forecasting or adjusting statistical forecasts. Thus, FSSs should facilitate judgmental
interventions.
Despite the numerous studies that support judgmental forecasting, current forecasting
systems are only partly successful in adding the judgment component in their features.
At present OMEN supports only unstructured judgmental adjustments for improving the
forecasts produced by the provided forecasting models.


5. Conclusions and future extensions

Forecasting support systems are used in every day life to assist forecasting procedures and
support important decisions of companies. Despite their improvement through the years,
vulnerabilities are still observed in three key aspects: non web-based solutions, lack of
customizability and complex user interfaces. In the present paper we present OMEN, a fully
customizable web-based FSS with a modern and efficient interface which uses exclusively
open-source solutions and supports state-of-the-art methods and models. Through OMEN
we demonstrate the limitations of typical off-the-shelf solutions and explain how some of its
feautures could be used to improve modern FSSs.
As presented, typical off-the-shelf solutions are windows-based applications which limit
the availability, the accessibility and the compatibility of the system, while increasing their
complexity and the use of resources. Moreover, given their architecture and structure,
FSSs are difficult to be customized leading to inflexible and generic software which are not
targeted to the special needs of each company and can therefore be proven quite ineffective.
This also leads to a confusing interface with needless options, models and tools that make
the use of the tool difficult even for experienced users. Problems are also detected in the
methodological aspect, including outdated methods and lack of the judgment component
across the forecasting procedure.
By exploiting exclusively free open-source solutions we indicate how a powerful FSS like
OMEN can be developed offering a lot of advantages both in technological and methodologi-
cal dimension. Using Shiny, R, OMEN consist an adaptable and easy to customize web-based
tool enabling 24/7 access of multiple users simultaneously by the device of their choice. A
simplified interface organized in comprehensive and well documented menu items facilitate
its use for practitioners, while state-of-the-art algorithms, such as temporal aggregation,
intermittent demand and continuous time series forecasting models, become easily available
using the cutting-edge functions provided by academics and researches to the community of
R.
Motivated by the suggestions of Petropoulos [34], our future work is focused on further
improving OMEN and bringing modern FSSs ways forward. This includes exploitation of
cloud-based solutions to reduce execution times on intensive workload and provide cost-
efficient solutions for companies with no ICT infrastructures. Cross-sectional aggregation
methods should also be considered for effectively dealing with hierarchical time series, while
the judgmental component should be expanded, providing not only judgmental adjustments
but also judgmental forecasting methods, such as structured analogies and Delphi, as well
as model selection techniques.

References

```
[1] Allaire, J., Cheng, J., Xie, Y., McPherson, J., Chang, W., Allen, J., Wickham, H., Atkins, A., Hyn-
dman, R., RStudio, jQuery Foundation, jQuery contributors, Otto, M., Thornton, J., Bootstrap con-
tributors, Twitter, Farkas, A., Jehl, S., Sagalaev, I., MacFarlane, J., Google, Raggett, D., W3C, 2016.
rmarkdown: Dynamic documents for r. R package, version 0.9.2.
[2] Andrawis, R. R., Atiya, A. F., El-Shishiny, H., 2011. Combination of long term and short term forecasts,
with application to tourism demand forecasting. International Journal of Forecasting 27 (3), 870 – 886.
```

[3] Assimakopoulos, V., 1995. A successive filtering technique for identifying long- term trends. Journal of
Forecasting, John Wiley 14 (1), 35 – 43.
[4] Assimakopoulos, V., Nikolopoulos, K., 2000. The theta model: a decomposition approach to forecasting.
International Journal of Forecasting 16 (4), 521 – 530.
[5] Beaumont, A. N., 2014. Data transforms with exponential smoothing methods of forecasting. Interna-
tional Journal of Forecasting 30 (4), 918 – 927.
[6] Chang, W., RStudio, Almasaeed Studio, Adobe Systems Incorporated, 2015. shinydashboard: Create
dashboards with shiny. R package, version 0.5.1.
[7] Chen, C., Liu, L.-M., 1993. Joint estimation of model parameters and outlier effects in time series.
Journal of the American Statistical Association 88 (421), 284 – 297.
[8] Croston, J., 1972. Forecasting and stock control for intermittent demand. Operational Research Quar-
terly 23 (1), 289 – 303.
[9] Davey, A., Flores, B., 1993. Identification of seasonality in time series: A note. Mathematical and
Computer Modelling 18 (6), 73 – 81.
[10] Davydenko, A., Fildes, R., 2013. Measuring forecasting accuracy: The case of judgmental adjustments
to sku-level demand forecasts. International Journal of Forecasting 29 (3), 510 – 522.
[11] de Lacalle, J. L., 2015. Detection of outliers in time series. Version: 0.6, [http://jalobe.com.](http://jalobe.com.)
[12] Fildes, R., Petropoulos, F., 2015. Simple versus complex selection rules for forecasting many time series.
Journal of Business Research 68 (8), 1692 – 1701.
[13] Franses, P. H., 2016. A note on the mean absolute scaled error. International Journal of Forecasting
32 (1), 20 – 22.
[14] Gardner, E. S., 1985. Exponential smoothing: the state of the art. Journal of Forecasting 4 (1), 1 – 28.
[15] Gardner, E. S., 2006. Exponential smoothing: The state of the artpart II. International Journal of
Forecasting 22 (4), 637 – 666.
[16] Guerrero, V. M., 1993. Time-series analysis supported by power transformations. Journal of Forecasting
12 (1), 37 – 48.
[17] Hyndman, R., 2015. forecast: Forecasting functions for time series and linear models. R package version
5.8, [http://github.com/robjhyndman/forecast.](http://github.com/robjhyndman/forecast.)
[18] Hyndman, R., Khandakar, Y., 2008. Automatic time series forecasting: the forecast package for r.
Journal of Statistical Software 26 (3), 1 22.
[19] Hyndman, R., Khandakar, Y., 2008. Automatic time series forecasting: the forecast package for R.
Journal of Statistical Software 26 (3), 1 – 22.
[20] Hyndman, R. J., Athanasopoulos, G., Bergmeir, C., Cinelli, C., Khan, Y., Mayer, Z., Razbash, S.,
Schmidt, D., Shaub, D., Tang, Y., Wang, E., Zhou, Z., 2015. forecast: Forecasting functions for time
series and linear models. Version 6.2.
[21] Hyndman, R. J., Koehler, A. B., 2006. Another look at measures of forecast accuracy. International
Journal of Forecasting 22 (4), 679 – 688.
[22] Hyndman, R. J., Koehler, A. B., Snyder, R. D., Grose, S., 2002. A state space framework for automatic
forecasting using exponential smoothing methods. International Journal of Forecasting 18 (3), 439 –
454.
[23] Kendall, M., Stuart, A., 1983. The advanced theory of statistics 3, 410–414.
[24] Kourentzes, N., 2014. On intermittent demand model optimisation and selection. International Journal
of Production Economics 156, 180 – 190.
[25] Kourentzes, N., Petropoulos, F., 2014. MAPA: Multiple Aggregation Prediction Algorith. R package
version 1.9. [http://CRAN.R-project.org/package=MAPA.](http://CRAN.R-project.org/package=MAPA.)
[26] Kourentzes, N., Petropoulos, F., 2014. Multiple Aggregation Prediction Algorithm, Version: 1.9,
[http://kourentzes.com/forecasting/2014/04/19/multiple-aggregation-prediction-algorithm-mapa/.](http://kourentzes.com/forecasting/2014/04/19/multiple-aggregation-prediction-algorithm-mapa/.)
[27] Kourentzes, N., Petropoulos, F., 2015. Forecasting with multivariate temporal aggregation: The case
of promotional modelling. International Journal of Production Economics, –.
[28] Kourentzes, N., Petropoulos, F., 2015. Intermittent time series forecasting. r package version 1.8.
[29] Kourentzes, N., Petropoulos, F., Trapero, J. R., 2014. Improving forecasting by estimating time series


structural components across multiple frequencies. International Journal of Forecasting 30 (2), 291 –
302.
[30] Ledolter, J., 1989. The effect of additive outliers on the forecasts from arima models. International
Journal of Forecasting 5 (2), 231 – 240.
[31] Makridakis, S., Wheelwright, S. C., McGee, V. E., 1983. Forecasting: Methods and applications (Second
Edition). New York: Wiley.
[32] McKinney, W., 2010. Data structures for statistical computing in python. Proceedings of the 9th Python
in Science Conference, 51 – 56.
[33] Nadaraya, E. A., 1964. On estimating regression. Theory of Probability and its Applications 9 (1), 141
2.
[34] Petropoulos, F., 2015. Forecasting support systems: Ways forward. Foresight: The International Jour-
nal of Applied Forecasting, issue 39, 5 – 11.
[35] Petropoulos, F., Kourentzes, N., Jun 2015. Forecast combinations for intermittent demand. J Oper Res
Soc 66 (6), 914–924.
[36] Proietti, T., Ltkepohl, H., 2013. Does the boxcox transformation help in forecasting macroeconomic
time series? International Journal of Forecasting 29 (1), 88 – 99.
[37] R Development Core Team, 2008. R: A language and environment for statistical computing. R Foun-
dation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, [http://www.R-project.org.](http://www.R-project.org.)
[38] Sakamoto, Y., I. M., G., K., 1986. Akaike information criterion statistics. D. Reidel Publishing Com-
pany.
[39] Syntetos, A. A., Boylan, J. E., 2005. The accuracy of intermittent demand estimates. International
Journal of Forecasting 21 (2), 303 – 314.
[40] Tashman, L. J., 2000. Out-of-sample tests of forecasting accuracy: an analysis and review. International
Journal of Forecasting 16 (4), 437 – 450.
[41] Teunter, R. H., Syntetos, A. A., Babai, M. Z., 2011. Intermittent demand: Linking forecasting to
inventory obsolescence. European Journal of Operational Research 214 (3), 606 – 615.
[42] Vaidyanathan, R., 2013. rCharts: Interactive Charts using Javascript Visualization Librarie. R package
version 0.4.5.
[43] Watson, G. S., 1964. Smooth regression analysis. Sankhy: The Indian Journal of Statistics, Series A
26 (4), 359 372.


