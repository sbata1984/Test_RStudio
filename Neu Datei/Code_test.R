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
#Control the type of data motorad_Beschwerde
str(motorad_Beschwerde)
#The Time Serie Visualisation
motorad_Beschwerde%>%
  autoplot(colour="blue")+
 xlab("Zeit")+
  ylab("Beschwerde")+
  ggtitle("Kundenbeschwerde")
#Analysis the distrubition of the data 
ggpairs(data=motorad_Beschwerde)
motorad_Beschwerde %>% ACF(complaints) %>% autoplot()

motorad_Beschwerde%>%
  mutate(Month=yearmonth(Date))
  group_by(Month)%>%
  forecast(h = 6) %>%
  hilo()




