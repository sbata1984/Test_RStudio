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
motorad_Beschwerde %>% stl(s.window='periodic') %>% autoplot()
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
motorad_Beschwerde
  

  






