 
y=na.omit(Kaffe_m)%>%   #remove NA
mutate(Month = seq.Date(as.Date("2016-10-01"),by="months",length.out = 45))

y=y%>%
filter(Bestellung > 1)
nrow(y)#43 row
as.numeric(y$Bestellung)#change Bestellung to nummeric
str(y)
y$`Letzte Zeichen`<-NULL
c<-as.data.frame(y)
y
kl
kl = c %>%
mutate(Month= yearmonth(Month))

#########TSLM model macht keine Sinn aber ab 2018 sieht viel besser#########
es gibt eine Verzerung vor 2018

kl<-as_tsibble(y)
kl<-kl%>%
  mutate(jahr=year(Month))
kl$jahr<-  as.factor(kl$jahr)
#Die Daten mit einem Linearen Model
Model2<-augment(lm_kl)%>%
  ggplot(aes(x=Month))+
  geom_line(aes(y=Bestellung,colour="Nachfrage_TS"))+
  geom_line(aes(y=.fitted ,colour="Angepasster Trend"))+
    xlab("Zeit")+ylab("Nachfrage")+
    ggtitle("")+
  guides(Daten=guide_level(title = ""))
Model2
#############Boxplot
bl<-kl%>%
  filter(year(Month)>2017)
bl
#############################

# 0. Define custom color palette and prepare the data
my3cols <- c("#E7B800", "#2E9FDF", "#FC4E07","blue","green")
# 1. Create a box plot (bp)
p <- ggplot(kl, aes(x = jahr, y = Bestellung))
bxp <- p + geom_boxplot(aes(color = jahr)) +
  scale_color_manual(values = my3cols)
bxp

dp <- p + geom_dotplot(aes(color = jahr, fill = jahr),
                       binaxis='y', stackdir='center') +
  scale_color_manual(values = my3cols) + 
  scale_fill_manual(values = my3cols)
figure1 <- ggarrange(bxp, dp ,bxr, dr,
                    labels = c("Boxplot_Mit Outlier", "Streuungsmaße_Mit Outlier","Boxplot_Ohne Outlier", "Streuungsmaße_Ohne Outlier"),
                    ncol = 2, nrow = 2)

figure1
###################################
cl<-kl%>%
filter(Month> c("2017-04-01"))
cl$Letzte.Zeichen<-NULL
ts.cl<-ts(cl$Bestellung,start=c(2017,05,01),freq=12)
ts.kl<-ts(kl$Bestellung,,start=c(2016,10,01),freq=12)
layout(1:2)
acf(ts.cl,main="TS_Ohne Outlier")
acf(ts.kl,main="TS_Mit Outlier")
#Box test für n=43 Werte 
l=2*sqrt(length(cl))
Box.test(ts.cl,l)
	Box-Pierce test
ggpairs(cl)
data:  ts.cl
X-squared = 0.80128, df = 3.4641, p-value = 0.8991
###############
pacf(ts.cl)
r <- ggplot(cl, aes(x = Month, y = Bestellung))
bxr <- r + geom_boxplot(aes(color = jahr)) +
  scale_color_manual(values = my3cols)
dr <- r + geom_dotplot(aes(color = jahr, fill = jahr),
                       binaxis='y', stackdir='center') +
  scale_color_manual(values = my3cols) + 
  scale_fill_manual(values = my3cols)
dr
figure3<-ggarrange(bxr, dr, 
                    labels = c("A", "B"),
                    ncol = 2, nrow = 2)

figure3
x
###############################
kl %>% 
    ggpairs(.,
    legend = 1,
    columns = c(2,4), 
    mapping = ggplot2::aes(colour=jahr), 
    lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1))) +
    theme(legend.position = "bottom")  
###### Remove die Outlier from DF

ts.kl<-ts(kl$Bestellung, start=c(2016,10),freq=12)
kl.outliers <- tso(ts.kl,types = c("AO","LS","TC"))
plot(kl.outliers)

####
figure <- ggarrange(Model1, Model2,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure
######
kl %>%
  mutate(diff_close = difference(Bestellung)) %>%#prüft man die stationarität ob es signifikant
  features(diff_close, ljung_box, lag = 10)

install.packages(urca)
kl %>% #hier bestimmen wir welche Werte soll aus die Signifikant Test bestimmt
  features(Bestellung, unitroot_kpss)
 kpss_stat kpss_pvalue    #der Wert soll kleiner als 1 bei kpss_stat und das stimmt auch so, wir brauchen keine Deffirenzierung
  kpss_stat kpss_pvalue
      <dbl>       <dbl>
1    0.0823         0.1
kl %>%
  mutate(diff_close = difference(Bestellung)) %>%
  features(diff_close, unitroot_kpss)#jetzt hat noch winzige Wert als vorher 0.083
 kpss_stat kpss_pvalue
      <dbl>       <dbl>
1    0.0823         0.1
kl %>%
  features(Bestellung, unitroot_ndiffs)

autoplot(kl,colour="blue")+xlab("Zeit")+ylab("Nachfragemenge")+ggtitle("Zeitreihe Kaffemachinen")+ theme(plot.title = element_text(hjust = 0.5))
#####Model 1 Linear ohne Rend und seasonalität
bl<-kl%>%
  filter(year(Month)>2017)
model_TSLM<-bl%>%
  model(tslm=TSLM(Bestellung~Month ))
#####Model1
Model1<-augment(model_TSLM) %>%
  ggplot(aes(x = Month)) +
    geom_line(aes(y = .fitted, colour = "Angepasster Trend")) +
  geom_line(aes(y = Bestellung, colour = "Nachfrage")) +

  labs(x = "Zeit", y = "Nachfrage",
       title = "")

model_lm_tr_sea<-bl%>%
  model(tslm=TSLM(Bestellung~ trend()+season()))
report(model_lm_tr_sea)

augment(model_lm_tr_sea) %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Bestellung, colour = "Bestellung")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(x = "Year", y = "Megalitres",
       title = "Quarterly Beer Production")
#ich jkann resid sehen, bzw. das Unterschied zwischen forecast und fitted modell
augment( model_lm_tr_sea)%>%
  autoplot(.resid)
#die Resid hat kein mean 0 
augment( model_lm_tr_sea)%>%
ggplot(aes(x = .resid)) +
geom_histogram(bins = 10) +
ggtitle("Histogram of residuals")

    model_lm_tr_sea %>% gg_tsresiduals()
    
    # keine Bedeutung für Kaffe_nachfragehat
    augment(model_lm_tr_sea) %>%
  ggplot(aes(x = Bestellung, y = .fitted,
             colour = factor(quarter(Month)))) +
    geom_point() +
    ylab("Fitted") + xlab("Actual values") +
    ggtitle("Quarterly beer production") +
    scale_colour_brewer(palette="Dark2", name="Quarter") +
    geom_abline(intercept=0, slope=1)
    ggpa
    ################################# Fourier nicht optimal##########
    fourier_kl <- kl %>%
  model(TSLM(Bestellung ~ trend() + fourier(K=2)))
report(fourier_kl)
 fourier_kl%>%
   gg_tsresiduals()
   augment(fourier_kl)%>%
  ggplot(aes(x=Month))+
  geom_line(aes(y=Bestellung,colour="Bestellung"))+
  geom_line(aes(y=.fitted ,colour="fitted"))+
      geom_line(aes(y=.resid ,colour="resid"))+
    xlab("Time")+ylab("Bestellung")+
    ggtitle("TS-Model und Angepasste Model")
  guides(colour=guide_level(title = "Level")) 
    
    
    
################
augment(model_TSLM)%>%
  ggplot(aes(x=Month))+
  geom_line(aes(y=Bestellung,colour="Bestellung"))+
  geom_line(aes(y=.fitted ,colour="fitted"))+
    xlab("Time")+ylab("Bestellung")+
    ggtitle("TS-Model und Angepasste Model")
  guides(colour=guide_level(title = "Level"))
  
  


  
  augment(model_TSLM)%>%
    ggplot(aes(x= Bestellung , y= .fitted))+
    geom_point()+
    ylab("Fitted Bestellung")+
    xlab("Akuelle bestellung")+
    ggtitle("Aktuelle TS und Fitted Werte")+
      geom_abline(intercept=0, slope=1)
  
  model_TSLM%>% gg_tsresiduals()
  
    
  augment(model_TSLM)%>%
    ggplot(aes(x= .fitted , y= .resid))+
    geom_point()+
    ylab("resid Bestellung")+
    xlab("fitted bestellung")+
    ggtitle("fitted und Resideul")
  
  
####################################################  
kl<-as_tsibble(kl)%>%
update_tsibble(index = Month, regular = TRUE)

dcmp<- kl %>%
  model(STL(Bestellung))
components(dcmp)
kl%>%
  autoplot(Bestellung, color="blue")+
  autolayer(components(dcmp),trend ,color="red")+
    xlab("Bestellung")+
  ylab("Zeit")+
  ggtitle("Kaffe-Maschine")
____Naiv Forecaste_________________________

#Benchmark methods______________________________________
kl #Dadurch bekommen die Werte
%>%
  model(NAIVE(Bestellung)) %>%
  forecast(h = 6) %>%
  hilo()


kl %>%
  model(NAIVE(Bestellung)) %>%
  forecast(h = 6,bootstrap=TRUE) %>%
autoplot(kl)
#Prediction intervals from bootstrapped residuals
Fit <- kl%>%
  model(NAIVE(Bestellung))
sim <- Fit %>% generate(h = 6, times = 3, bootstrap = TRUE)
sim
kl%>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Bestellung)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
  ggtitle("Kaffemaschine Forecast") +
  guides(col = FALSE)
############hat selbe function like decompose

components(dcmp) %>%
  autoplot() + xlab("Year")
 dcmp %>% 
gg_subseries(season_year)

##################Angepasste trend###############
kl%>%
  autoplot(Bestellung, color="blue")+
  autolayer(components(dcmp),season_adjust ,color="red")+
    xlab("Bestellung")+
  ylab("Zeit")+
  ggtitle("Kaffe-Maschine")

##################################################################
kl%>%#Die jahre werden explizit aussortiert und dargestellt
   gg_season(Bestellung,labels = "left")
################################# 
 kl %>%
  model(STL(Bestellung ~ trend(window=10) + season(window='periodic'),#smaller window nuber allow for more rapid changes
    robust = TRUE)) %>%
  components() %>%
  autoplot()
 ##########################################
kl%>%
  features(Bestellung,quantile,pro=seq(0,1,by=0.25))
 kl%>%
  features(Bestellung, feat_stl)
_____________________Forecaste with decomposition__________________
 dcmp <- kl %>%
  model(x11 = feasts:::X11(Bestellung, type =  "multiplicative")) %>%
  components()
 
 dcmp
dcmp <- kl%>%
  model(STL(Bestellung ~ trend(window = 7), robust=TRUE)) %>%
  components() %>%
  select(-.model)
dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) + ylab("New Bestellung index") +
  ggtitle("Naive forecasts of seasonally adjusted Kaffe_Maschine data")
fit_dcmp <- kl %>%
  model(stlf = decomposition_model(
             STL(Bestellung ~ trend(window = 7), robust = TRUE),
             NAIVE(season_adjust)))
fit_dcmp%>% gg_tsresiduals()#ich muss die Outlier entfernen
  ___________________________________________________________________________________________________
kl<-  kl%>%
   update_tsibble(index = Month,regular = TRUE)
 str(kl)
View(train_Kaffe)
 
 View(kl)
 # Set training data from Nov 2016 to Nov 2019
train_Kaffe <- kl %>% filter_index("2016 Nov" ~ "2019 Nov")
# Fit the models
kaffe_fit <- train_Kaffe %>%
  model(
    Mean = MEAN(Bestellung),
    `Naïve` = NAIVE(Bestellung),
    `Seasonal naïve` = SNAIVE(Bestellung)
  )
# Generate forecasts for 14 quarters
kaffe_fc <- kaffe_fit %>% forecast(h=12)
# Plot forecasts against actual values
kaffe_fc %>%
  autoplot(train_Kaffe, level = NULL) 
    autolayer(filter_index(kl, "2019 Dez" ~ .), color = "black") +
    ggtitle("Forecasts for Kaffe_Maschine Sales") +
    xlab("Year") + ylab("Menge") +
    guides(colour=guide_legend(title="Forecast"))
 
#hier trend und Naive forecast
 fit_dcmp <- kl %>%
  model(stlf = decomposition_model(
             STL(Bestellung~ trend(window = 7), robust = TRUE),
             NAIVE(season_adjust)))
fit_dcmp %>%
  forecast() %>%
  autoplot(us_retail_employment)
 
 Forcast with Mean and NAive, SNAIVE und Random Walk
kaff_train<-kl%>%
    slice(1:34)#like subset function
#Bild a Model
kaffe_fit <- kaff_train %>%
  model(
    Mean = MEAN(Bestellung),
    `Naïve` = NAIVE(Bestellung),
    `Seasonal naïve` = SNAIVE(Bestellung),
    Drift = RW(Bestellung ~ drift()))
#Forcast
kaffe_fc <- kaffe_fit %>%
  forecast(h = 10)
#Autoplot
kaffe_fc %>%
  autoplot(kl, level = NULL) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))
#Genauigkeit prüfen
accuracy(kaffe_fc, kl)

  .model         .type    ME  RMSE   MAE   MPE  MAPE  MASE    ACF1
  <chr>          <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
1 Drift          Test  -8.59 10.1   8.59 -58.5  58.5 0.509 -0.0347
2 Mean           Test  -5.56  7.07  5.56 -39.5  39.5 0.329 -0.342 
3 Naïve          Test  -5.56  7.07  5.56 -39.5  39.5 0.329 -0.342 
4 Seasonal naïve Test   3.22  8.89  7.89  11.4  47.3 0.468 -0.102 


 _______________________________________________________________________________



kl
 kl%>%
 autoplot(Bestellung,colour="blue")+xlab("Zeit")+ylab("Nachfrage")
 kl%>%
 ggplot(aes(x=Month))+
  geom_line(aes(y=Bestellung))
 kl
 fit_kl_arima<-kl%>%
   model(ARIMA(Bestellung~PDQ(0,0,0),  stepwise = FALSE, approximation = FALSE))
#jetzt rechnet das model besser ARIMA Werte wegen Stepwise and approximation
 report(fit_kl_arima)
 Series: Bestellung 
Model: ARIMA(0,0,1) w/ mean #Model Empfehlung

Coefficients:
         ma1  constant
      0.6746   22.2539
s.e.  0.1017    3.5336

sigma^2 estimated as 204.5:  log likelihood=-174.69
AIC=355.38   AICc=355.99   BIC=360.66



 fit_kl_arima %>% forecast(h=6) %>% autoplot(slice(kl, (n()-43):n()))
 kl %>% ACF(Bestellung) %>% autoplot()
 str(kl)
 kl$Letzte.Zeichen<-NULL
 view(kl)
 fi_arima<-kl%>%
   model(ARIMA(Bestellung~pdq(0,0,1)))
 fi_arima %>% forecast(h=6) %>% autoplot(slice(kl, (n()-43):n())) 
 report(fi_arima)
 #############################
 #kl Verteilung
 

kl %>% 
  add_predictions(lm_kl) %>%
  ggplot(aes(Month, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")





 #Die Varianz sinkt mit der Zeit

  
ggplot(yl, aes(x=year,y= Bestellung)) + 
  geom_hex(bins = 50)
outlayer<-boxplot(kl$Bestellung)$out
outlayer
cl<-kl$Bestellung - outlayer
lowerbestellung<-

outlier.chicken <- tsoutliers::tso(ts.kl,types = c("AO","LS","TC"),maxit.iloop=10)
outlier.chicken
plot(outlier.chicken)

View(cl)
#############################################

dcmp <- cl%>%
  model(stl =STL(Bestellung))
components(dcmp)

components(Bestellung)%>% 
  autoplot()+ xlab("Year")
