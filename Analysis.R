#read data files:

c_tax_bands <- read.csv('data - council tax bands 2018.csv')
dwellings_type <- read.csv('data - dwellings type 2017.csv')
local_authority <- read.csv('data - local authority and urbanrural.csv')
median_prop_value <- read.csv('data - Median property prices DZ 2014 to 2018.csv') # - response variable.
no_of_rooms <- read.csv('data - number or rooms 2017.csv')
information <- read.csv('data - simd2016.csv')
library(sf)

##########
#EDA
##########

#shape files visualization
shape <- st_read("SG_DataZone_Bdry_2011.shp")
shape2 <- st_transform(x=shape, crs='+proj=longlat +datum=WGS84 +no_defs')

#for simplicity, we will replace the naas with mean of that column
information$Income_rate[is.na(information$Income_rate)]<-mean(information$Income_rate,na.rm=TRUE)
information$Employment_rate[is.na(information$Employment_rate)]<-mean(information$Employment_rate,na.rm=TRUE)
information$CIF[is.na(information$CIF)]<-mean(information$CIF,na.rm=TRUE)
information$DEPRESS[is.na(information$DEPRESS)]<-mean(information$DEPRESS,na.rm=TRUE)
information$LBWT[is.na(information$LBWT)]<-mean(information$LBWT,na.rm=TRUE)
information$Attendance[is.na(information$Attendance)]<-mean(information$Attendance,na.rm=TRUE)
information$Attainment[is.na(information$Attainment)]<-mean(information$Attainment,na.rm=TRUE)
information$NEET[is.na(information$NEET)]<-mean(information$NEET,na.rm=TRUE)
information$HESA[is.na(information$HESA)]<-mean(information$HESA,na.rm=TRUE)
information$crime_rate[is.na(information$crime_rate)]<-mean(information$crime_rate,na.rm=TRUE)

median_prop_value_new <- median_prop_value[,3:7]
median_prop_value_new$Medianprice2014[is.na(median_prop_value_new$Medianprice2014)] <- rowMeans(median_prop_value_new, na.rm = TRUE)[is.na(median_prop_value_new$Medianprice2014)]  # Replace by row means
median_prop_value_new$Medianprice2015[is.na(median_prop_value_new$Medianprice2015)] <- rowMeans(median_prop_value_new, na.rm = TRUE)[is.na(median_prop_value_new$Medianprice2015)]  # Replace by row means
median_prop_value_new$Medianprice2016[is.na(median_prop_value_new$Medianprice2016)] <- rowMeans(median_prop_value_new, na.rm = TRUE)[is.na(median_prop_value_new$Medianprice2016)]  # Replace by row means
median_prop_value_new$Medianprice2017[is.na(median_prop_value_new$Medianprice2017)] <- rowMeans(median_prop_value_new, na.rm = TRUE)[is.na(median_prop_value_new$Medianprice2017)]  # Replace by row means
median_prop_value_new$Medianprice2018[is.na(median_prop_value_new$Medianprice2018)] <- rowMeans(median_prop_value_new, na.rm = TRUE)[is.na(median_prop_value_new$Medianprice2018)]  # Replace by row means
median_prop_value_new$name<- median_prop_value$name
median_prop_value_new$DZ<- median_prop_value$DZ

#reorganise the different data sets to one set
info<-merge(median_prop_value_new, information, by = "DZ")[-c(1:7)]

median_prop_value_new.drop<- median_prop_value_new[-c(773, 799 ,813, 2477, 5162, 5165, 5166, 5167, 5728, 5738, 5739), ]
info.drop<- info[-c(773, 799 ,813, 2477, 5162, 5165, 5166, 5167, 5728, 5738, 5739), ]

info_with_lable<-merge(median_prop_value_new, information, by = "DZ")[-c(2:7)]
info_with_lable.drop<- info_with_lable[-c(773, 799 ,813, 2477, 5162, 5165, 5166, 5167, 5728, 5738, 5739), ]

covariates<- merge(info_with_lable.drop, local_authority, by = "DZ")
covariates<- merge(covariates, c_tax_bands, by = "DZ")
covariates<- merge(covariates, no_of_rooms, by = "DZ")
covariates<- merge(covariates, dwellings_type, by = "DZ")
#data frame of covariates
covariates<-covariates[-c(36:43)]
covariates_with_house_price<- merge(covariates, median_prop_value_new.drop, by='DZ')


#polygon and shape files
sf_use_s2(FALSE)
library(spdep)
sp.dat <- merge(shape2, covariates_with_house_price, all.x=FALSE, by.x="DataZone", by.y="DZ")
W.nb <- poly2nb(sp.dat, row.names = shape2$Name)
W <- nb2mat(W.nb, style = "B", zero.policy = TRUE)
W.list <- nb2listw(W.nb, style = "B")


#add centriods
coords.sp <- st_centroid(shape2)$geometry
coords <- st_coordinates(coords.sp)
shape2$easting <- coords[ ,1] / 1000
shape2$northing <- coords[ ,2] / 1000
shape2$easting.scaled <- scale(shape2$easting, center=TRUE, scale=TRUE)
shape2$northing.scaled <- scale(shape2$northing, center=TRUE, scale=TRUE)

#long data
library(tidyr)
covariates_with_house_price<- covariates_with_house_price[-c(50)]

covariates_with_house_price <- cbind(newColName = rownames(covariates_with_house_price), covariates_with_house_price)
rownames(covariates_with_house_price) <- 1:nrow(covariates_with_house_price)
#remove 2018 and convert to long format
data_long<- covariates_with_house_price[-c(61)]
data_long$Medianprice2017<- NA
data_long<- data_long %>% pivot_longer(cols=c('Medianprice2014', 'Medianprice2015',
                                              'Medianprice2016','Medianprice2017'),
                                       names_to='Year',
                                       values_to='Price')
data_long$Year<-replace(data_long$Year,data_long$Year=="Medianprice2014","2014")
data_long$Year<-replace(data_long$Year,data_long$Year=="Medianprice2015","2015")
data_long$Year<-replace(data_long$Year,data_long$Year=="Medianprice2016","2016")
data_long$Year<-replace(data_long$Year,data_long$Year=="Medianprice2017","2017")

coords_shape<- shape2[c('DataZone','easting.scaled','northing.scaled')]
coords_shape<- st_drop_geometry(coords_shape) 

data_long_coords<- merge(data_long, coords_shape, by.x='DZ', by.y='DataZone')
covariates_with_house_price<- merge(covariates_with_house_price, coords_shape, by.x='DZ', by.y='DataZone')


#####
#correlations plot
#####
library(corrplot)
library(dplyr)
corrplot(cor(corr_covar), type = 'upper', tl.cex=0.5, tl.offset = 0.2)

corr_covar<- select(covariates_with_house_price, 'population','All','crime_rate','nocentralheat_rate','overcrowded_rate','population','Working_age_population_revised',
                    'Noquals','Attainment','Attendance','Bands_AC',
                    'Bands_DE','Bands_FH','Employment_rate','Income_rate','drive_GP','urbanrural8_code',
                    'drive_petrol','drive_PO','PT_GP',
                    'PT_Post','PT_retail','drive_primary','drive_retail','drive_secondary',
                    'DRUG','EMERG','ALCOHOL','CIF','HESA','DEPRESS','LBWT','NEET',
                    'SMR',
                    'Medianprice2014','Medianprice2015','Medianprice2016','Medianprice2017',
                    'Medianprice2018')
corr_covar %>% relocate(any_of(c('population','All','crime_rate','nocentralheat_rate','overcrowded_rate','population','Working_age_population_revised',
                                 'Noquals','Attainment','Attendance','Bands_AC',
                                 'Bands_DE','Bands_FH','Employment_rate','Income_rate','drive_GP','urbanrural8_code',
                                 'drive_petrol','drive_PO','PT_GP',
                                 'PT_Post','PT_retail','drive_primary','drive_retail','drive_secondary',
                                 'DRUG','EMERG','ALCOHOL','CIF','HESA','DEPRESS','LBWT','NEET',
                                 'SMR',
                                 'Medianprice2014','Medianprice2015','Medianprice2016','Medianprice2017',
                                 'Medianprice2018')))
library(ggcorrplot)
ggcorrplot(cor(information[,2:29]))


#####
#Visualization
#####
sp.dat.glasgow<- subset(sp.dat, LA_name=='Glasgow City')
library(leaflet)
colours <- colorNumeric(palette = "YlOrRd", domain = sp.dat.glasgow$Medianprice2014, reverse=FALSE)
leaflet(data=sp.dat.glasgow) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(sp.dat.glasgow$Medianprice2014),
              color="grey", weight=1,
              fillOpacity = 0.9) %>%
  addLegend(pal = colours, values = sp.dat.glasgow$Medianprice2014,
            opacity = 1, title="Price(£)",
            labFormat = labelFormat(prefix = "£")) %>%
  addScaleBar(position="bottomleft")

######
#Confirm spatial correlation exists
######
montacarla<- moran.mc(x = sp.dat$Medianprice2018, listw = W.list, nsim = 10000)
montacarla
#small p value so spatial autocorrelation is present

##############
#Linear model
#############
data_long_coords$Year<- as.integer(data_long_coords$Year)

start.time <- Sys.time()

model.0<- lm(log(Price)~ population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
                   Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
                   drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
                   DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
                   nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
                   PT_Post+PT_retail+SMR+Working_age_population_revised+Year+
                   easting.scaled+northing.scaled, data=data_long_coords) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

prediction <- predict(model.0, newdata = data_long_coords)

fittedvalues.0<- ((prediction))[seq(0, length(model.0$fitted.values), 4)]
fittedvalues.0<-exp(fittedvalues.0)

Y<-covariates_with_house_price[c(60)]
colnames(Y)<-NULL
Y<- c(Y)
Y<- unlist(Y)
RMSE.0.1<- caret::RMSE(fittedvalues.0, Y)
RMSE.0.1/(max(Y)-min(Y))
RMSE.0.1


##############
#MCMC linear
##############

predictions<- rep(NA, 6773)
predictions<- as.vector(predictions)

y<-covariates_with_house_price[c(57, 58, 59, 60)]
y$Medianprice2017 <- NA
colnames(y)<-NULL
y<- c(y)
y<- unlist(y)

x<- covariates_with_house_price[c(3:30,35,37:39,51)]
x<- do.call(rbind, replicate(4, x, simplify=FALSE))

list2env(data.frame(x), envir = .GlobalEnv)

library(CARBayesST)
start.time<- Sys.time()
model1 <- ST.CARlinear(formula = log(y) ~ population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
                        Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
                        drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
                        DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
                        nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
                        PT_Post+PT_retail+SMR+Working_age_population_revised, family = "gaussian",
                      W = W, burnin = 20000, n.sample = 120000, thin = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#diagnostics
model1$modelfit
print(model1)
library(coda)
plot(model1$summary.results[,'Geweke.diag']) 
fitted(model)
coef(model)
par(mar=c(1,1,1,1))
plot(model1$samples$beta)
model1$accept

#RMSE
Y<-covariates_with_house_price[c(60)]
colnames(Y)<-NULL
Y<- c(Y)
Y<- unlist(Y)

RMSE1.1<- caret::RMSE(exp(model1$fitted.values[20320:27092]), Y)
RMSE1.1/(max(Y)-min(Y))

library(caret)
RMSE1.2<-caret::RMSE(exp(colMeans(model1$samples$Y)), Y)
length(colMeans(model1$samples$Y))

#save results
results<- cbind(covariates_with_house_price, MCMCPred=exp(colMeans(model1$samples$Y)))
write.csv(results, file='results.csv')

###
#MCMC anova
###

start.time<- Sys.time()
model2 <- ST.CARanova(formula = log(y) ~ population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
                         Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
                         drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
                         DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
                         nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
                         PT_Post+PT_retail+SMR+Working_age_population_revised, family = "gaussian",
                       W = W, burnin = 20000, n.sample = 120000, thin = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#diagnostics
print(model2$modelfit)
library(coda)
plot(model2$summary.results[,'Geweke.diag']) 
fitted(model2)
coef(model)
par(mar=c(1,1,1,1))
plot(model2$samples$beta)
model2$accept

#RMSE
Y<-covariates_with_house_price$Medianprice2017
colnames(Y)<-NULL
Y<- c(Y)
Y<- unlist(Y)

RMSE1.1<- caret::RMSE(exp(model2$fitted.values[20320:27092]), Y)
RMSE1.1/(max(Y)-min(Y))

##############
#INLA
##############
library(INLA)

#idd = independent random effects

data_long$newColName1<- data_long$newColName
data_long$newColName<- as.numeric(data_long$newColName)
data_long$newColName1<- as.numeric(data_long$newColName1)

formula3 <- log(Price) ~ 1 + f(newColName, model="bym",graph=W) +
  f(newColName1,Year,model="iid") + Year +population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
  Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
  drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
  DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
  nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
  PT_Post+PT_retail+SMR+Working_age_population_revised

model3 <- inla(formula3, family = "Gaussian", data=data_long,
               control.predictor=list(compute=TRUE),
               control.compute=list(dic=TRUE,cpo=TRUE, config=TRUE))


summary(model3)
model3$dic$dic
plot(model3)
model3$summary.fixed
model3$summary.random
model3$summary.fitted.values

samples.parameters3 <- inla.posterior.sample(n=5000, result=model3, add.names=TRUE, use.improved.mean = TRUE, skew.corr=TRUE)
samples.test <- array(NA, c(5000, 6773))
for(s in 1:5000)
{
  sigma.current <- sqrt(1 / samples.parameters3[[s]]$hyperpar[1])
  fitted.current <- samples.parameters3[[s]]$latent[seq(0, 27092, 4)]
  samples.test[s, ] <- rnorm(n=6773, mean=fitted.current, sd=rep(sigma.current , 6773))
}


#### Generate the predictions
results.test <- data.frame(prediction=rep(NA, 6773), LCI=rep(NA, 6773), UCI=rep(NA, 6773))
results.test$prediction <- apply(samples.test, 2, mean)
results.test[ ,2:3] <- t(apply(samples.test, 2, quantile, c(0.025, 0.975)))


#### Backtransform them to the exponentiated scale 

sigma2.mean <- 1 / model3$summary.hyperpar[1,1]
results.test$prediction <- exp(results.test$prediction + (sigma2.mean / 2))
results.test$LCI <- exp(results.test$LCI)
results.test$UCI <- exp(results.test$UCI)   


model3$summary.random$newColName1$mean

#fitted values standardized
fitted.values.3<-model3$summary.fitted.values$mean
library(dplyr)
fittedvalues.3<- fitted.values.3[seq(0, length(fitted.values.3), 4)]
fittedvalues.3<-exp(fittedvalues.3)



RMSE3.1<- caret::RMSE((fittedvalues.3), Y)
RMSE3.1/(max(Y)-min(Y))
RMSE3<- caret::RMSE(results.test$prediction, Y)
RMSE3/(max(Y)-min(Y))

hist(results.test$prediction, main= 'Predicted data')
hist(covariates_with_house_price$Medianprice2017, main= 'Actual data')

head(results.test$prediction,20)


#model 5 - type 2 INLA
data_long$Year1<- data_long$Year

formula4 <- log(Price) ~ 1 + f(newColName,model="bym",graph=W) +
  f(Year,model="rw1") + f(Year1,model="iid")+population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
  Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
  drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
  DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
  nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
  PT_Post+PT_retail+SMR+Working_age_population_revised

model4 <- inla(formula4, family = "Gaussian", data=data_long,
               control.predictor=list(compute=TRUE),
               control.compute=list(dic=TRUE,cpo=TRUE, config=TRUE))

samples.parameters4 <- inla.posterior.sample(n=5000, result=model4, add.names=TRUE, use.improved.mean = TRUE, skew.corr=TRUE)
samples.test4 <- array(NA, c(5000, 6773))
for(s in 1:5000)
{
  sigma.current4 <- sqrt(1 / samples.parameters4[[s]]$hyperpar[1])
  fitted.current4 <- samples.parameters4[[s]]$latent[seq(0, 27092, 4)]
  samples.test4[s, ] <- rnorm(n=6773, mean=fitted.current4, sd=rep(sigma.current4 , 6773))
}


#### Generate the predictions
results.test4 <- data.frame(prediction=rep(NA, 6773), LCI=rep(NA, 6773), UCI=rep(NA, 6773))
results.test4$prediction <- apply(samples.test4, 2, mean)
results.test4[ ,2:3] <- t(apply(samples.test4, 2, quantile, c(0.025, 0.975)))


#### Backtransform them to the exponentiated scale 

sigma2.mean4 <- 1 / model4$summary.hyperpar[1,1]
results.test4$prediction <- exp(results.test4$prediction + sigma2.mean4 / 2)
results.test4$LCI <- exp(results.test4$LCI)
results.test4$UCI <- exp(results.test4$UCI)   

RMSE4.1<- caret::RMSE(results.test4$prediction, Y)
RMSE4.1/(max(Y)-min(Y))

summary(model4)
model4$dic$dic
model4$summary.fixed
model4$summary.random


#fitted values standardized
fitted.values.4<-model4$summary.fitted.values$mean
library(dplyr)
fittedvalues.4<- fitted.values.4[seq(0, length(fitted.values.4), 4)]
fittedvalues.4<-exp(fittedvalues.4)

RMSE4<- caret::RMSE(fittedvalues.4, Y)
RMSE4/(max(Y)-min(Y))

#MODEL 6 - type 3 INLA
data_long$ID.area.year<- seq(1,length(data_long$newColName))


formula5 <- log(Price) ~ 1 + f(newColName,model="bym",graph=W) +
  f(Year,model="rw1") + f(Year1,model="iid")+
  f(ID.area.year,model="iid")+population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
  Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
  drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
  DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
  nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
  PT_Post+PT_retail+SMR+Working_age_population_revised

model5 <- inla(formula5, family = "Gaussian", data=data_long,      
               control.compute=list(dic=TRUE,cpo=TRUE, config=TRUE)) 

   
summary(model5)
model5$dic$dic
model5$summary.fixed
model5$summary.random
model5$summary.fitted.values
#fitted values standardized
fitted.values.5<-model5$summary.fitted.values$mean
library(dplyr)
fittedvalues.5<- fitted.values.5[seq(0, length(fitted.values.5), 4)]
fittedvalues.5<-exp(fittedvalues.5)

RMSE5<- caret::RMSE(fittedvalues.5, Y)
RMSE5/(max(Y)-min(Y))

samples.parameters5 <- inla.posterior.sample(n=5000, result=model5, add.names=TRUE, use.improved.mean = TRUE, skew.corr=TRUE)
samples.test5 <- array(NA, c(5000, 6773))
for(s in 1:5000)
{
  sigma.current5 <- sqrt(1 / samples.parameters5[[s]]$hyperpar[1])
  fitted.current5 <- samples.parameters5[[s]]$latent[seq(0, 27092, 4)]
  samples.test5[s, ] <- rnorm(n=6773, mean=fitted.current5, sd=rep(sigma.current5 , 6773))
}


#### Generate the predictions
results.test5 <- data.frame(prediction=rep(NA, 6773), LCI=rep(NA, 6773), UCI=rep(NA, 6773))
results.test5$prediction <- apply(samples.test5, 2, mean)
results.test5[ ,2:3] <- t(apply(samples.test5, 2, quantile, c(0.025, 0.975)))


#### Backtransform them to the exponentiated scale 

sigma2.mean5 <- 1 / model5$summary.hyperpar[1,1]
results.test5$prediction <- exp(results.test5$prediction + sigma2.mean5 / 2)
results.test5$LCI <- exp(results.test5$LCI)
results.test5$UCI <- exp(results.test5$UCI)   

RMSE5.1<- caret::RMSE(results.test5$prediction, Y)
RMSE5.1/(max(Y)-min(Y))

#save results
results<- cbind(results, INLAPred=results.test5$prediction)
write.csv(results, file='results.csv')

#plot the map of predicted results next to actual.

library(leaflet)
colours <- colorNumeric(palette = "PuOr", domain = sp.dat$Medianprice2017, reverse=TRUE)
leaflet(data=sp.dat) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(sp.dat$Medianprice2017),
              color="grey", weight=1,
              fillOpacity = 0.6) %>%
  addLegend(pal = colours, values = sp.dat$Medianprice2017,
            opacity = 1, title="Medianprice2017") %>%
  addScaleBar(position="bottomleft")


colours <- colorNumeric(palette = "PuOr", domain = fittedvalues.10, reverse=TRUE)
leaflet(data=sp.dat) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(fittedvalues.10),
              color="grey", weight=1,
              fillOpacity = 0.6) %>%
  addLegend(pal = colours, values = fittedvalues.10,
            opacity = 1, title="Predicted median price 2017") %>%
  addScaleBar(position="bottomleft")

library(RColorBrewer)
library(ggplot2)


plot1<- ggplot() + geom_sf(data = sp.dat, aes(fill = Medianprice2017)) +
  xlab("Longitude (degrees)") +
  ylab("Latitude (degrees)") +
  labs(title = "Median Price 2017", fill = "Medianprice2017") +
  theme(title = element_text(size=14)) +
  scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd"))

plot2<- ggplot() + geom_sf(data = sp.dat, aes(fill = fittedvalues.10)) +
  xlab("Longitude (degrees)") +
  ylab("Latitude (degrees)") +
  labs(title = "Predicted price 2017", fill = "Predicted price 2017") +
  theme(title = element_text(size=14)) +
  scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd"))

#inla variable selection

library(INLA); library(ggplot2); library(ggregplot)
library(dplyr)

HostModelSel <- INLAModelSel(log(y), population,ALCOHOL,All,Attainment,Attendance,Bands_AC,
                               Bands_DE,Bands_FH,CIF,crime_rate,DEPRESS,drive_GP,urbanrural8_code,
                               drive_petrol,drive_PO,drive_primary,drive_retail,drive_secondary,
                               DRUG,EMERG,Employment_rate,HESA,Income_rate,LBWT,NEET,
                               nocentralheat_rate,Noquals,overcrowded_rate,population,PT_GP,
                               PT_Post,PT_retail,SMR,Working_age_population_revised,"ID", "iid", "gaussian", data_long,ScaleVariables = F)



##############
#Random Forest
##############

data_predict<- data_long_coords[data_long_coords$Year == '2017',]
data_long_coords<- na.omit(data_long_coords)
library(ranger)
start.time <- Sys.time()
model.6<- ranger(log(Price)~ population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
                   Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
                   drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
                   DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
                   nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
                   PT_Post+PT_retail+SMR+Working_age_population_revised+Year+
                   easting.scaled+northing.scaled, verbos=TRUE, data=data_long_coords
                 , min.node.size=3, sample.fraction=0.5, replace=FALSE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

pred<- predict(model.6, data_predict)
model.6.predictions<- pred$predictions
model.6.predictions<- exp(model.6.predictions)


Y<-covariates_with_house_price[c(60)]
colnames(Y)<-NULL
Y<- c(Y)
Y<- unlist(Y)
RMSE.6.1<- caret::RMSE(model.6.predictions, Y)
RMSE.6.1/(max(Y)-min(Y))
RMSE.6.1

library(spm)

#grid search
# create hyperparameter grid
n_features <- length(setdiff(names(data_long_coords), "Price"))

hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)

# execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(log(Price)~ ALCOHOL+All+Attainment+Attendance+Bands_AC+
                  Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+
                  drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
                  DRUG+EMERG+Employment_rate.x+HESA+Income_rate.x+LBWT+NEET+
                  nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
                  PT_Post+PT_retail+SMR+Terraced+Working_age_population_revised+Year+
                  easting.scaled+northing.scaled, verbos=TRUE, data=data_long_coords,
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
  )
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

# assess top 10 models
hyper_grid %>%
  arrange(rmse) %>%
#  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)



##############
#grf: Geographically Weighted Random Forest Model
##############

library(SpatialML)
data_long_coords$Price<- log(data_long_coords$Price)

#find best bandwidth
model7.1<-grf.bw(log(Price)~ population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
                   Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
                   drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
                   DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
                   nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
                   PT_Post+PT_retail+SMR+Working_age_population_revised+Year, 
       data_long_coords, kernel="adaptive", coords=coords_grf,
       bw.min = NULL,bw.max = NULL, step = 1, trees=1000, mtry=NULL, importance="impurity",
       nthreads = 1, forests = FALSE, geo.weighted = TRUE)
#too much data

coords_grf<- cbind(data_long_coords$easting.scaled, data_long_coords$northing.scaled)

start.time <- Sys.time()
model.7<- grf(Price~ population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
                Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
                drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
                DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
                nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
                PT_Post+PT_retail+SMR+Working_age_population_revised+Year,
     dframe=data_long_coords, kernel="adaptive",  coords=coords_grf, ntree=500, 
     importance="impurity",  forests = TRUE,bw=500,
     geo.weighted = TRUE, print.results=TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


pred<- predict(model.7, data_predict)
model.7.predictions<- pred$predictions
model.7.predictions<- exp(model.7.predictions)
model.7$call

Y<-covariates_with_house_price[c(59)]
colnames(Y)<-NULL
Y<- c(Y)
Y<- unlist(Y)
RMSE.7.1<- caret::RMSE(model.7.predictions, Y)
RMSE.7.1/(max(Y)-min(Y))
RMSE.7.1

##############
#CAR RF 
##############
library(dplyr)
X.training<- data_long_coords%>%dplyr::select(c(newColName, ALCOHOL,All,Attainment,Attendance,Bands_AC,
                                         Bands_DE,Bands_FH,CIF,crime_rate,DEPRESS,drive_GP,urbanrural8_code,
                                         drive_petrol,drive_PO,drive_primary,drive_retail,drive_secondary,
                                         DRUG,EMERG,Employment_rate,HESA,Income_rate,LBWT,NEET,
                                         nocentralheat_rate,Noquals,overcrowded_rate,population,PT_GP,
                                         PT_Post,PT_retail,SMR,Working_age_population_revised,Year))

X.test<- data_predict%>%dplyr::select(c(newColName,ALCOHOL,All,Attainment,Attendance,Bands_AC,
                                 Bands_DE,Bands_FH,CIF,crime_rate,DEPRESS,drive_GP,urbanrural8_code,
                                 drive_petrol,drive_PO,drive_primary,drive_retail,drive_secondary,
                                 DRUG,EMERG,Employment_rate,HESA,Income_rate,LBWT,NEET,
                                 nocentralheat_rate,Noquals,overcrowded_rate,population,PT_GP,
                                 PT_Post,PT_retail,SMR,Working_age_population_revised,Year))
Y.training<-log(data_long_coords[c(59)])

coords.training<- data_long_coords[c(60,61)]
coords.test<- data_predict[c(60,61)]

W.list <- mat2listw(W, style="B")
graph <- nb2INLA("spatialgraph.adj", W.list$neighbours)
library(INLA)
graph <- inla.read.graph(filename = "spatialgraph.adj")
library(caret)
library(ranger)

attr(X.training, "na.action") <- NULL
attr(Y.training, "na.action") <- NULL
library(vctrs)

start.time<- Sys.time()
Model8<- CARForest(X.training, X.test, Y.training, coords.training, coords.test, n_tr=500, m_try=30, min_node=3,  logscale=TRUE, Graph=graph, R=5, inla_samples=5000, n=3)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

CARForest <- function(X.training, X.test, Y.training, coords.training, coords.test, n_tr, m_try, min_node, D, R, logscale, inla_samples, Graph, n)
{
  ####################
  #### Specify lengths
  ####################
  #### Lengths
  K.training <- nrow(X.training)
  K.test <- nrow(X.test)
  K.total <- K.training + K.test
  
  
  
  ##############################
  #### Initialise key quantities
  ##############################
  #### Initialise phi
  phi.training <- rep(0, K.training)
  phi.test <- rep(0, K.test)
  
  
  #### Set up the results list
  results <- as.list(rep(NA, R))
  names(results) <- 1:R

  
  ##################################################
  #### Fit the iterative algorithm with R iterations
  ##################################################
  for(r in 1:R)
  {
    #### Compute the adjusted data 
    Z <- Y.training - phi.training
    dat.training <- data.frame(Z=Z, X.training)
    colnames(dat.training)[1]<- 'Z'
    dat.test <- data.frame(Z=rep(0, K.test), X.test)
    
    
    #### Fit the RF model and make prediction
    mod.rf <- ranger(formula = Z~., data = dat.training, importance= 'impurity', num.trees=n_tr, mtry=m_try, min.node.size=min_node, oob.error=TRUE)
    mhat.training.oob <- mod.rf$predictions
    mhat.test <- predict(mod.rf, data=dat.test)$predictions
    
    
    #### Fit the CAR model
    dat.all <- rbind(dat.training, dat.test)
    dat.all$offset <- c(mhat.training.oob, mhat.test)
    dat.all$response <- unlist(c(Y.training, rep(NA, K.test)))
    dat.all$newColName<- as.numeric(dat.all$newColName)
    formula.car <-  response ~ offset(offset) +  f(newColName, model='besagproper2', graph = W, constr=TRUE,
                                                   hyper=list(lambda=list(prior="gaussian", param=c(0, 0.01)), 
                                                              prec=list(prior = "loggamma", param = c(1, 0.01))))
    mod.car <- inla(formula=formula.car, data=dat.all, family="gaussian",
                    control.compute=list(dic=TRUE, mlik=TRUE, waic=TRUE, config = TRUE),
                    control.fixed=list(mean=0, mean.intercept=0, prec=0.00001, prec.intercept=0.00001),
                    control.family=list(hyper=list(prec=list(prior="loggamma", param=c(1, 0.01)))),
                    control.predictor=list(compute=TRUE, link=1), scale=1, control.inla=list(strategy="auto"))   
    
    
    #### Update the estimate of phi
    phi.training <- mod.car$summary.random$newColName$mean
    phi.training <- vec_rep_each(phi.training, n)
    phi.test <- mod.car$summary.random$newColName$mean
    
    #### Predict from the CAR model
    samples.parameters <- inla.posterior.sample(n=inla_samples, result=mod.car, add.names=TRUE, use.improved.mean = TRUE, skew.corr=TRUE)
    samples.test <- array(NA, c(inla_samples, K.test))
    for(s in 1:inla_samples)
    {
      sigma.current <- sqrt(1 / samples.parameters[[s]]$hyperpar[1])
      fitted.current <- samples.parameters[[s]]$latent[(K.training+1):K.total]
      samples.test[s, ] <- rnorm(n=K.test, mean=fitted.current, sd=rep(sigma.current , K.test))
    }
    
    
    #### Generate the predictions
    results.test <- data.frame(prediction=rep(NA, K.test), LCI=rep(NA, K.test), UCI=rep(NA, K.test))
    results.test$prediction <- apply(samples.test, 2, mean)
    results.test[ ,2:3] <- t(apply(samples.test, 2, quantile, c(0.025, 0.975)))
    
    
    #### Backtransform them to the exponentiated scale if required
    if(logscale)
    {
      sigma2.mean <- 1 / mod.car$summary.hyperpar[1,1]
      results.test$prediction <- exp(results.test$prediction + sigma2.mean/2)
      results.test$LCI <- exp(results.test$LCI)
      results.test$UCI <- exp(results.test$UCI)   
    }else
    {}
    
    
    #### Save the results
    results[[r]] <- results.test
  }
  
  
  #### Return the results
  return(results)
}



model.8.predictions<- results$prediction

Y<-covariates_with_house_price[c(59)]
colnames(Y)<-NULL
Y<- c(Y)
Y<- unlist(Y)
RMSE.8.1<- caret::RMSE(Model8[["10"]][["prediction"]], Y)
RMSE.8.1/(max(Y)-min(Y))
RMSE.8.1




##############
#CAR RF 2 - spatial temopral CAR 
##############

train.year<- data_long_coords$Year
test.year<- data_predict$Year
train.year<- as.numeric(train.year)
test.year<- as.numeric(test.year)
library(dplyr)
X.training<- data_long_coords%>%dplyr::select(c(newColName, ALCOHOL,All,Attainment,Attendance,Bands_AC,
                                         Bands_DE,Bands_FH,CIF,crime_rate,DEPRESS,drive_GP,urbanrural8_code,
                                         drive_petrol,drive_PO,drive_primary,drive_retail,drive_secondary,
                                         DRUG,EMERG,Employment_rate,HESA,Income_rate,LBWT,NEET,
                                         nocentralheat_rate,Noquals,overcrowded_rate,population,PT_GP,
                                         PT_Post,PT_retail,SMR,Working_age_population_revised))

X.test<- data_predict%>%dplyr::select(c(newColName,ALCOHOL,All,Attainment,Attendance,Bands_AC,
                                 Bands_DE,Bands_FH,CIF,crime_rate,DEPRESS,drive_GP,urbanrural8_code,
                                 drive_petrol,drive_PO,drive_primary,drive_retail,drive_secondary,
                                 DRUG,EMERG,Employment_rate,HESA,Income_rate,LBWT,NEET,
                                 nocentralheat_rate,Noquals,overcrowded_rate,population,PT_GP,
                                 PT_Post,PT_retail,SMR,Working_age_population_revised))
Y.training<-log(data_long_coords[c(59)])

coords.training<- data_long_coords[c(63,64)]
coords.test<- data_predict[c(63,64)]

attr(X.training, "na.action") <- NULL
attr(Y.training, "na.action") <- NULL
attr(train.year, "na.action") <- NULL
attr(test.year, "na.action") <- NULL



start.time <- Sys.time()
Model9<- CARForest.2(X.training, X.test, Y.training, coords.training, coords.test, n_tr=1000, m_try=30, min_node=3,  logscale=TRUE, Graph=graph, R=5, inla_samples=5000, n=3,train.year=train.year, test.year=test.year)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
CARForest.2 <- function(X.training, X.test, Y.training, coords.training, coords.test, n_tr, m_try, min_node, D, R, logscale, inla_samples, Graph, n, train.year, test.year)
{
  ####################
  #### Specify lengths
  ####################
  #### Lengths
  K.training <- nrow(X.training)
  K.test <- nrow(X.test)
  K.total <- K.training + K.test
  
  
  
  ##############################
  #### Initialise key quantities
  ##############################
  #### Initialise phi
  phi.training <- rep(0, K.training)
  phi.test <- rep(0, K.test)
  
  
  #### Set up the results list
  results <- as.list(rep(NA, R))
  names(results) <- 1:R
  
  
  
  ##################################################
  #### Fit the iterative algorithm with R iterations
  ##################################################
  for(r in 1:R)
  {
    #### Compute the adjusted data 
    Z <- Y.training - phi.training
    dat.training <- data.frame(Z=Z, X.training)
    colnames(dat.training)[1]<- 'Z'
    dat.training<- dplyr::select(dat.training, -one_of(c( 'Year1', 'ID.area.year','Year')))
    dat.test <- data.frame(Z=rep(0, K.test), X.test)
    dat.test<- dplyr::select(dat.test, -one_of(c( 'Year1', 'ID.area.year','Year')))
    
    
    #### Fit the RF model and make prediction
    mod.rf <- ranger(formula = Z~., data = dat.training, importance= 'impurity', num.trees=n_tr, mtry=m_try, min.node.size=min_node, oob.error=TRUE)
    mhat.training.oob <- mod.rf$predictions
    mhat.test <- predict(mod.rf, data=dat.test)$predictions
    
    
    #### Fit the CAR model
    dat.all <- rbind(dat.training, dat.test)
    dat.all$offset <- c(mhat.training.oob, mhat.test)
    dat.all$response <- unlist(c(Y.training, rep(NA, K.test)))
    dat.all$newColName<- as.numeric(dat.all$newColName)
    dat.all$ID.area.year<- seq(1,length(dat.all$newColName))
    dat.all$Year<- (c(unlist(train.year), unlist(test.year)))
    dat.all$Year1<- dat.all$Year
    
    formula.car <-  response ~ offset(offset) + 1+ f(newColName, model='besagproper2', graph = W)+
      f(Year,model="rw1") + f(Year1,model="iid")+
      f(ID.area.year,model="iid")
    
    mod.car <- inla(formula=formula.car, data=dat.all, family="gaussian" ,control.predictor=list(compute=TRUE),
                    control.compute=list(dic=TRUE,cpo=TRUE, config=TRUE))   
    

    #### Update the estimate of phi
    phi.training <- mod.car$summary.random$ID.area.year$mean[1:K.training]
    #phi.training <- vec_rep_each(phi.training, n)
    phi.test <- mod.car$summary.random$ID.area.year$mean[K.training+1:K.total]
    
    #### Predict from the CAR model
    samples.parameters <- inla.posterior.sample(n=5000, result=mod.car, add.names=TRUE, use.improved.mean = TRUE, skew.corr=TRUE)
    samples.test <- array(NA, c(5000, K.test))
    for(s in 1:5000)
    {
      sigma.current <- sqrt(1 / samples.parameters[[s]]$hyperpar[1])
      fitted.current <- samples.parameters[[s]]$latent[20320:27092]
      samples.test[s, ] <- rnorm(n=K.test, mean=fitted.current, sd=rep(sigma.current , K.test))
    }
    
    
    #### Generate the predictions
    results.test <- data.frame(prediction=rep(NA, K.test), LCI=rep(NA, K.test), UCI=rep(NA, K.test))
    results.test$prediction <- apply(samples.test, 2, mean)
    results.test[ ,2:3] <- t(apply(samples.test, 2, quantile, c(0.025, 0.975)))
    
    
    #### Backtransform them to the exponentiated scale if required
    if(logscale)
    {
      sigma2.mean <- 1 / mod.car$summary.hyperpar[1,1]
      results.test$prediction <- exp(results.test$prediction + sigma2.mean / 2)
      results.test$LCI <- exp(results.test$LCI)
      results.test$UCI <- exp(results.test$UCI)   
    }else
    {}
    
    
    #### Save the results
    results[[r]] <- results.test
  }
  
  
  #### Return the results
  return(results)
}

model.9.predictions<- results$prediction

Y<-covariates_with_house_price[c(60)]
colnames(Y)<-NULL
Y<- c(Y)
Y<- unlist(Y)
RMSE.9.1<- caret::RMSE(Model9[["8"]][['prediction']], Y)
RMSE.9.1/(max(Y)-min(Y))
RMSE.9.1

#store restults
extimates_with_covar<- covariates_with_house_price
extimates_with_covar$estimates_2017<- Model9[["6"]][['prediction']]

results<- cbind(results, CARRFPred=Model9[["6"]][['prediction']])
write.csv(results, file='results.csv')

#############################
#CAR FR MCMC - too computationally expensive to work with
#############################

train.year<- data_long_coords$Year
test.year<- data_predict$Year
train.year$Year<- as.numeric(train.year$Year)
test.year$Year<- as.numeric(test.year$Year)
library(dplyr)
X.training<- data_long_coords%>%select(c(newColName, ALCOHOL,All,Attainment,Attendance,Bands_AC,
                                         Bands_DE,Bands_FH,CIF,crime_rate,DEPRESS,drive_GP,urbanrural8_code,
                                         drive_petrol,drive_PO,drive_primary,drive_retail,drive_secondary,
                                         DRUG,EMERG,Employment_rate,HESA,Income_rate,LBWT,NEET,
                                         nocentralheat_rate,Noquals,overcrowded_rate,population,PT_GP,
                                         PT_Post,PT_retail,SMR,Working_age_population_revised,Year))

X.test<- data_predict%>%select(c(newColName,ALCOHOL,All,Attainment,Attendance,Bands_AC,
                                 Bands_DE,Bands_FH,CIF,crime_rate,DEPRESS,drive_GP,urbanrural8_code,
                                 drive_petrol,drive_PO,drive_primary,drive_retail,drive_secondary,
                                 DRUG,EMERG,Employment_rate,HESA,Income_rate,LBWT,NEET,
                                 nocentralheat_rate,Noquals,overcrowded_rate,population,PT_GP,
                                 PT_Post,PT_retail,SMR,Working_age_population_revised,Year))
Y.training<-log(data_long_coords$Price)

coords.training<- data_long_coords[c(60,61)]
coords.test<- data_predict[c(60,61)]

attr(X.training, "na.action") <- NULL
attr(Y.training, "na.action") <- NULL
attr(train.year, "na.action") <- NULL
attr(test.year, "na.action") <- NULL

x<- covariates_with_house_price[c(3:30,35,37:39,51)]
x<- do.call(rbind, replicate(4, x, simplify=FALSE))
list2env(data.frame(x), envir = .GlobalEnv)
library(CARBayesST)
library(dplyr)


start.time <- Sys.time()
Model9<- CARForest.2(X.training, X.test, Y.training, coords.training, coords.test, n_tr=1000, m_try=30, min_node=5,  logscale=TRUE, Graph=graph, R=10, inla_samples=10000, n=3,train.year=train.year, test.year=test.year)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
CARForest.2 <- function(X.training, X.test, Y.training, coords.training, coords.test, n_tr, m_try, min_node, D, R, logscale, inla_samples, Graph, n, train.year, test.year)
{
  ####################
  #### Specify lengths
  ####################
  #### Lengths
  K.training <- nrow(X.training)
  K.test <- nrow(X.test)
  K.total <- K.training + K.test
  
  
  
  ##############################
  #### Initialise key quantities
  ##############################
  #### Initialise phi
  phi.training <- rep(0, K.training)
  phi.test <- rep(0, K.test)
  
  
  #### Set up the results list
  results <- as.list(rep(NA, R))
  names(results) <- 1:R
  
  
  
  ##################################################
  #### Fit the iterative algorithm with R iterations
  ##################################################
  for(r in 1:R)
  {
    #### Compute the adjusted data 
    Z <- Y.training - phi.training
    dat.training <- data.frame(Z=Z, X.training)
    colnames(dat.training)[1]<- 'Z'
    dat.training<- select(dat.training, -one_of(c('Year', 'Year1', 'ID.area.year')))
    dat.test <- data.frame(Z=rep(0, K.test), X.test)
    dat.test<- select(dat.test, -one_of(c('Year', 'Year1', 'ID.area.year')))

    
    #### Fit the RF model and make prediction
    mod.rf <- ranger(formula = Z~., data = dat.training, importance= 'impurity', num.trees=n_tr, mtry=m_try, min.node.size=min_node, oob.error=TRUE)
    mhat.training.oob <- mod.rf$predictions
    mhat.test <- predict(mod.rf, data=dat.test)$predictions
    
    
    #### Fit the CAR model
    
    dat.all <- rbind(dat.training, dat.test)
    dat.all$offset <- c(mhat.training.oob, mhat.test)
    dat.all$response <- unlist(c(Y.training, rep(NA, K.test)))

    model1 <- ST.CARlinear(formula = log(y) ~ offset(dat.all$offset)+population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
                             Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
                             drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
                             DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
                             nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
                             PT_Post+PT_retail+SMR+Working_age_population_revised, family = "gaussian",
                           W = W, burnin = 20000, n.sample = 120000, thin = 10)
    
   
    #### Update the estimate of phi
    phi.training <- model1$residuals$pearson[1:K.training]
    phi.test <- model1$residuals$pearson[K.training+1:K.total]
    
    #### Predict from the CAR model
    samples.parameters <- inla.posterior.sample(n=5000, result=mod.car, add.names=TRUE, use.improved.mean = TRUE, skew.corr=TRUE)
    samples.test <- array(NA, c(5000, K.test))
    for(s in 1:5000)
    {
      sigma.current <- sqrt(1 / samples.parameters[[s]]$hyperpar[1])
      fitted.current <- samples.parameters[[s]]$latent[20320:27092]
      samples.test[s, ] <- rnorm(n=K.test, mean=fitted.current, sd=rep(sigma.current , K.test))
    }
    
    
    #### Generate the predictions
    results.test <- data.frame(prediction=rep(NA, K.test), LCI=rep(NA, K.test), UCI=rep(NA, K.test))
    results.test$prediction <- apply(samples.test, 2, mean)
    results.test[ ,2:3] <- t(apply(samples.test, 2, quantile, c(0.025, 0.975)))
    
    
    #### Backtransform them to the exponentiated scale if required
    if(logscale)
    {
      sigma2.mean <- 1 / mod.car$summary.hyperpar[1,1]
      results.test$prediction <- exp(results.test$prediction + sigma2.mean / 2)
      results.test$LCI <- exp(results.test$LCI)
      results.test$UCI <- exp(results.test$UCI)   
    }else
    {}
    
    
    #### Save the results
    results[[r]] <- exp(model1$fitted.values[20320:27092])
  }
  
  
  #### Return the results
  return(results)
}



###############
#Analysis of results
###############
results <- read.csv('results.csv')
sp.dat.glasgow<- subset(results, LA_name=='Glasgow City')
sp.dat.glasgow <- merge(shape2, sp.dat.glasgow, all.x=FALSE, by.x="DataZone", by.y="DZ")

ggplot() + geom_sf(data = sp.dat.glasgow, aes(fill = Medianprice2017)) +
  xlab("Longitude (degrees)") +
  ylab("Latitude (degrees)") +
  labs(title = "Actual Median Price 2017", fill = "Medianprice2017") +
  theme(title = element_text(size=14)) +
  scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd"),
                       name="Price (£)")

library(leaflet)
colours <- colorNumeric(palette = "YlOrRd", domain = sp.dat.glasgow$CARRFPred, reverse=FALSE)
leaflet(data=sp.dat.glasgow) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(sp.dat.glasgow$CARRFPred),
              color="grey", weight=1,
              fillOpacity = 0.9) %>%
  addLegend(pal = colours, values = sp.dat.glasgow$CARRFPred,
            opacity = 1, title="Price(£)",
            labFormat = labelFormat(prefix = "£")) %>%
  addScaleBar(position="bottomleft")

RMSE<- caret::RMSE(sp.dat.glasgow$MCMCPred, sp.dat.glasgow$Medianprice2017)
RMSE/(max(sp.dat.glasgow$Medianprice2017)-min(sp.dat.glasgow$Medianprice2017))

results$difference <-results$MCMCPred-results$Medianprice2017
sum(results$difference)
results$differenceRF <-results$CARRFPred-results$Medianprice2017

################
#MCMC on 2018
################
data_long_pred<- covariates_with_house_price
data_long_pred$Medianprice2018<- NA
data_long_pred<- data_long_pred %>% pivot_longer(cols=c('Medianprice2014', 'Medianprice2015',
                                              'Medianprice2016','Medianprice2017','Medianprice2018'),
                                       names_to='Year',
                                       values_to='Price')
data_long_pred$Year<-replace(data_long_pred$Year,data_long_pred$Year=="Medianprice2014","2014")
data_long_pred$Year<-replace(data_long_pred$Year,data_long_pred$Year=="Medianprice2015","2015")
data_long_pred$Year<-replace(data_long_pred$Year,data_long_pred$Year=="Medianprice2016","2016")
data_long_pred$Year<-replace(data_long_pred$Year,data_long_pred$Year=="Medianprice2017","2017")
data_long_pred$Year<-replace(data_long_pred$Year,data_long_pred$Year=="Medianprice2018","2018")

predictions<- rep(NA, 6773)
predictions<- as.vector(predictions)

y<-covariates_with_house_price[c(57, 58, 59, 60, 61)]
y$Medianprice2018 <- NA
colnames(y)<-NULL
y<- c(y)
y<- unlist(y)

x<- covariates_with_house_price[c(3:30,35,37:39,51)]
x<- do.call(rbind, replicate(5, x, simplify=FALSE))

list2env(data.frame(x), envir = .GlobalEnv)

library(CARBayesST)
start.time<- Sys.time()
model10 <- ST.CARlinear(formula = log(y) ~ population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
                         Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
                         drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
                         DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
                         nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
                         PT_Post+PT_retail+SMR+Working_age_population_revised, family = "gaussian",
                       W = W, burnin = 20000, n.sample = 120000, thin = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

Y<-covariates_with_house_price[c(61)]
colnames(Y)<-NULL
Y<- c(Y)
Y<- unlist(Y)

RMSE10.1<- caret::RMSE(exp(model10$fitted.values[27093:33865]), Y)
RMSE10.1/(max(Y)-min(Y))
model10$modelfit

results<- cbind(results, MCMC2018Pred=exp(model10$fitted.values[27093:33865]))
write.csv(results, file='results.csv')

sp.dat.glasgow<- subset(results, LA_name=='Glasgow City')
sp.dat.glasgow <- merge(shape2, sp.dat.glasgow, all.x=FALSE, by.x="DataZone", by.y="DZ")


library(leaflet)
colours <- colorNumeric(palette = "YlOrRd", domain = sp.dat.glasgow$Medianprice2018, reverse=FALSE)
leaflet(data=sp.dat.glasgow) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(sp.dat.glasgow$Medianprice2018),
              color="grey", weight=1,
              fillOpacity = 0.9) %>%
  addLegend(pal = colours, values = sp.dat.glasgow$Medianprice2018,
            opacity = 1, title="Price(£)",
            labFormat = labelFormat(prefix = "£")) %>%
  addScaleBar(position="bottomleft")


#compare the results
results$diff<- results$MCMC2018Pred-results$Medianprice2018
sum(results$diff)

#subset the locations where the difference is less thant +-£1000
sub <- results%>%filter(diff < -70000 | diff >70000)

sub.shape <- merge(shape2, sub, all.x=FALSE, by.x="DataZone", by.y="DZ")

library(leaflet)
colours <- colorNumeric(palette = "YlOrRd", domain = sub.shape$Medianprice2018, reverse=FALSE)
leaflet(data=sub.shape) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(sub.shape$Medianprice2018),
              color="grey", weight=1,
              fillOpacity = 0.9) %>%
  addLegend(pal = colours, values = sub.shape$Medianprice2018,
            opacity = 1, title="Price(£)",
            labFormat = labelFormat(prefix = "£")) %>%
  addScaleBar(position="bottomleft")

ggplot() + geom_sf(data = sub.shape, aes(fill = MCMC2018Pred)) +
  xlab("Longitude (degrees)") +
  ylab("Latitude (degrees)") +
  labs(title = "Median Price 2018", fill = "MCMC2018Pred") +
  theme(title = element_text(size=14)) +
  scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd"),
                       name="Price (£)")




################
#MCMC on the future - 2019
################
data_long_19<- covariates_with_house_price
data_long_19$Medianprice2019<- NA
data_long_19<- data_long_19 %>% pivot_longer(cols=c('Medianprice2014', 'Medianprice2015',
                                                        'Medianprice2016','Medianprice2017','Medianprice2018','Medianprice2019'),
                                                 names_to='Year',
                                                 values_to='Price')
data_long_19$Year<-replace(data_long_19$Year,data_long_19$Year=="Medianprice2014","2014")
data_long_19$Year<-replace(data_long_19$Year,data_long_19$Year=="Medianprice2015","2015")
data_long_19$Year<-replace(data_long_19$Year,data_long_19$Year=="Medianprice2016","2016")
data_long_19$Year<-replace(data_long_19$Year,data_long_19$Year=="Medianprice2017","2017")
data_long_19$Year<-replace(data_long_19$Year,data_long_19$Year=="Medianprice2018","2018")
data_long_19$Year<-replace(data_long_19$Year,data_long_19$Year=="Medianprice2019","2019")


predictions<- rep(NA, 6773)
predictions<- as.vector(predictions)

y<-covariates_with_house_price[c(57, 58, 59, 60, 61)]
y$Medianprice2019 <- NA
colnames(y)<-NULL
y<- c(y)
y<- unlist(y)

x<- covariates_with_house_price[c(3:30,35,37:39,51)]
x<- do.call(rbind, replicate(6, x, simplify=FALSE))

list2env(data.frame(x), envir = .GlobalEnv)

library(CARBayesST)
start.time<- Sys.time()
model10 <- ST.CARlinear(formula = log(y) ~ population+ALCOHOL+All+Attainment+Attendance+Bands_AC+
                          Bands_DE+Bands_FH+CIF+crime_rate+DEPRESS+drive_GP+urbanrural8_code+
                          drive_petrol+drive_PO+drive_primary+drive_retail+drive_secondary+
                          DRUG+EMERG+Employment_rate+HESA+Income_rate+LBWT+NEET+
                          nocentralheat_rate+Noquals+overcrowded_rate+population+PT_GP+
                          PT_Post+PT_retail+SMR+Working_age_population_revised, family = "gaussian",
                        W = W, burnin = 20000, n.sample = 120000, thin = 10)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


model10$modelfit

results<- cbind(results, MCMC2019Pred=exp(model10$fitted.values[33866:40638]))
write.csv(results, file='results.csv')

sp.dat.glasgow<- subset(results, LA_name=='Glasgow City')
sp.dat.glasgow <- merge(shape2, sp.dat.glasgow, all.x=FALSE, by.x="DataZone", by.y="DZ")


library(leaflet)
colours <- colorNumeric(palette = "YlOrRd", domain = sp.dat.glasgow$MCMC2019Pred, reverse=FALSE)
leaflet(data=sp.dat.glasgow) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(sp.dat.glasgow$MCMC2019Pred),
              color="grey", weight=1,
              fillOpacity = 0.9) %>%
  addLegend(pal = colours, values = sp.dat.glasgow$MCMC2019Pred,
            opacity = 1, title="Price(£)",
            labFormat = labelFormat(prefix = "£")) %>%
  addScaleBar(position="bottomleft")






