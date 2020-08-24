#Data about Motor
str(motororg)#Data Frame type with 48 observation and 1 Variable
'data.frame':	48 obs. of  1 variable:
 $ complaints: int  27 34 31 24 18 19 17 12 26 14 `
#Create a new Date Column from 2015-01-01
motorad_Beschwerde<-motororg%>%
mutate(Date= seq.Date(from=as.Date("2015-01-01"), by="months",length.out = 48))
#Transform the Data Frame to tsibble
motorad_Beschwerde<-as_tsibble(motorad_Beschwerde)%>%
update_tsibble(index = Date, regular = TRUE)
#Creat a Time serie from motorag data
ts.motor<-ts(motororg,start = c(2015,01,01),freq=12)
#plot the Time Serie, we can see the Stationarity 
plot(ts.motor,col="blue")
#The Time Serie is Stationar, we have no signifikant Correlation
acf(ts.motor)
#Decompose Function with Trend and Saesonal
decom.mot<-decompose(ts.motor)
plot(decompose(ts.motor,type="multiplicative"))
#we have a 6 month seasonality 
plot(decom.mot$figure,type="b",las=2,ylab="Seasonality index")
#Control the type of data motorad_Beschwerde
str(motorad_Beschwerde)
#The Time Serie Visualisation
motorad_Beschwerde%>%
autoplot(colour="blue")+
xlab("Zeit")+
ylab("Beschwerde")+
ggtitle("Kundenbeschwerde")
#STL is an acronym for “Seasonal and Trend decomposition using Loess”, while Loess is a method for estimating nonlinear relationships.
#Make a STL objekt
motorad_Beschwerde %>% stlf() %>% autoplot()
#
motorad_Beschwerde%>%
gg_season(complaints)

#Analysis the distrubition of the data 
ggpairs(data=motorad_Beschwerde)
#Remove NA Value 
motorad_Beschwerde%>%
filter(!is.na(complaints))
#
motorad_Beschwerde %>% ACF(complaints) %>% autoplot()
motorad_Beschwerde %>% PACF(complaints) %>% autoplot()
model_motorbeschwerde<-auto.arima(ts.motor, d=1,  stationary = TRUE,
seasonal=TRUE,stepwise=FALSE, trace=TRUE,  nmodels = 300 ,approximation=TRUE,  test = c("kpss", "adf", "pp")
)
Ljung-Box test

data:  Residuals from ARIMA(2,0,0)(1,0,0)[12] with non-zero mean
Q* = 12.322, df = 6, p-value = 0.05515

Model df: 4.   Total lags used: 10

checkresiduals(model_motorbeschwerde)
summary(model_motorbeschwerde)
Series: ts.motor 
ARIMA(2,0,0)(1,0,0)[12] with non-zero mean 

Coefficients:
         ar1      ar2    sar1     mean
      0.2748  -0.1297  0.3449  19.4637
s.e.  0.1420   0.1472  0.1589   1.4868

sigma^2 estimated as 45.78:  log likelihood=-158.6
AIC=327.2   AICc=328.63   BIC=336.55

Training set error measures:
                     ME     RMSE      MAE       MPE     MAPE      MASE
Training set -0.2620073 6.477811 5.230958 -20.26634 38.56988 0.7327412
                    ACF1
Training set 0.002709276
model_motorbeschwerde%>%forecast(h=12)%>%
  autoplot()
  

  






