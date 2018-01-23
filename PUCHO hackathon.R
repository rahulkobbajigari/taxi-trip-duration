#clearing the r environment


rm(list = ls(all=TRUE))
#loading required libraries
#library(geosphere)
library(timeDate)
#library(splusTimeDate)
library(dummies)
library(chron)
library(ggmap)
library(factoextra)
library(dplyr)
library(tibble)
library(lubridate)
library('ggplot2') 
library('scales')
library('grid') 
library('RColorBrewer')
library('corrplot') 
library('alluvial')
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats')

#reading the NYC taxi trip duration in r

train<-read.csv("train.csv")
test<-read.csv("test.csv")

#basic data understanding
### Observe the data
# * Find how many unique products
# * What are all the columns given in the data

summary(train)

#checking if consists of missing values in data

sum(is.na(train))

#if missing values we can omit the data
train1<-na.omit(train)
#str(train1)

#checking for outliers in the data
boxplot(train1$trip_duration,horizontal = T)




####DATA PREPROCESSING


###TRIP_DURATION 

#trip duration consists of min=1sec and maximum=3526282(980 hours).no one can travel 980 hours 
   #with taxi in a city the bill can be more and no one can travel in 1 sec so it consists of outliers in the target variable
# we can also assume that some people booked the taxi, after arriving the taxi there they cancelled the taxi trip from this we can get trip duration of 1 sec to 10 sec
#we can also remove that rows 
#values are in seconds 
#we can  converte in to minutes to compete easily

quantile(train1$trip_duration,c(0.99,0.999,0.99999,0.999999))
quantile(train1$trip_duration,c(0.001))
#it consists of  outliers with 0.999999 so i removed 
f<-function(x)
{
  q<-quantile(x,c(0.05,0.99999))
  x[x>q[2]]<- NA
  x
}
f(train1$trip_duration)->train1$trip_duration

train1<-na.omit(train1)
# time duration consists of in seconds so i tried to see in minutes to compute easily

boxplot(train1$trip_duration/60,horizontal = T)

dim(train1)




##LONGITUDE AND LATITUDE


#The coordinates of the  NYC city is
#city_long_border = (-74.03, -73.75)
#city_lat_border = (40.63, 40.85)
#the taxi cannot be drived out of the city so some coordinates are falling out- 
#-of these boarders so i  removed those coordinates fall out of these boarders



train1<- train1[train1$pickup_longitude >= -74.03  & train1$pickup_longitude <= -73.75,]

train1<- train1[train1$pickup_latitude >= 40.63 &  train1$pickup_latitude <= 40.85,]

train1 <- train1[train1$dropoff_longitude >= -74.03&train1$dropoff_longitude <= -73.75,]

train1<- train1[train1$dropoff_latitude >= 40.63&train1$dropoff_latitude <= 40.85,]


###visualization for longitude and latitude
#There is another insight here which is rather intuitive: trips to or from any of the airports (most prominently JFK) are unlikely to be very short. Thus, the a close distance of either pickup or dropoff to the airport could be a valuable predictor for longer trip_duration.
###after removing the outliers the pickupand dropoff are in same region

library(geosphere)
ny_map <- as.tibble(map_data("state", region = "new york:manhattan"))

set.seed(2017)
day_trips <- train1 %>%
  sample_n(200)

tpick <- day_trips %>%
  select(lon = pickup_longitude, lat = pickup_latitude)
tdrop <- day_trips %>%
  select(lon = dropoff_longitude, lat = dropoff_latitude)

p1 <- ggplot() +
  geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
  geom_point(data=tpick,aes(x=lon,y=lat),size=1,color='red',alpha=1)




p2 <-ggplot()+
  geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
  geom_point(data=tdrop,aes(x=lon,y=lat),size=1,color='blue',alpha=1)


#train1$vendor_id<-as.factor(train1$vendor_id)



#The trip volume per hour of the day depends somewhat on the month and strongly on the day of the week:




#conversions

train1 <- train1 %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime,tz="America/New_York"),
         vendor_id = factor(vendor_id))


test <- test %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime,tz="America/New_York"),
         vendor_id = factor(vendor_id))




##The trip volume per hour of the day depends somewhat on the month and strongly on the day of the week:

#Hide


p1 <- train1 %>%
  mutate(hpick = hour(pickup_datetime),
         Month = factor(month(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, Month) %>%
  count() %>%
  ggplot(aes(hpick, n, color = Month)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")

p2 <- train1 %>%
  mutate(hpick = hour(pickup_datetime),
         wday = factor(wday(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, wday) %>%
  count() %>%
  ggplot(aes(hpick, n, color = wday)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")

layout <- matrix(c(1,2),2,1,byrow=FALSE)
multiplot(p1, p2, layout=layout)






#while converting in to date format on missing value is occured so i removed that missing value from the data

sum(is.na(train1))
train1<-na.omit(train1)


#concatinating the dataset for feature engineering
#although it will be biased while doing any pre processing 
train1$trip_duration->trip_duration
trip_duration<-data.frame(trip_duration)
train1$trip_duration<-NULL
train1$dropoff_datetime->dropoff_datetime
train1$dropoff_datetime<-NULL
train2<-rbind(train1,test)








#from summary of distance min=0 and max=33 km
#By using latitude and longitude we can calculate distance in kilometers
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}
earth.dist(train2$pickup_longitude,train2$pickup_latitude,train2$dropoff_longitude,train2$dropoff_latitude)->distance
distance<-as.data.frame(distance)
summary(distance)




#train1$dropoff_datetime<-NULL
#converting the pickup_datetime in to date format 
date<-as.Date(train2$pickup_datetime)




#checking the maximum and minimum date to form in sequence
max(train2$pickup_datetime)
min(train2$pickup_datetime)


#holiday(year = getRmetricsOptions("2016"), Holiday = "USFederal")
#holidays(2016, type = "USFederal", move = FALSE)
#the date is formed in sequence to get the holidays list by using date.sequence
# A timeDate Sequence
date.sequence <- timeSequence(ymd_hms("2015-12-31 13:30:22 EST"), ymd_hms("2016-06-30 23:59:39 EDT"));  
date.sequence;

# checking the holidays in certain period of time
years.included <- unique( as.integer( format( x=date.sequence, format="%Y" ) ) );
holidays <- data.frame(holidayNYSE(years.included)) 
train2$holidays<-ifelse(train2$pickup_datetime%in%holidays$NewYork.x..i..,1,0)

# excluding the holidays all days are converted in to business days 
business.days <- date.sequence[isBizday(date.sequence, holidays)]; 
business.days
business.days<-data.frame(business.days)

# from datetime variable time is converted in to minutes to know at which minute taxi pickup is happened 
minutes<-as.POSIXlt(train2$pickup_datetime,'%Y-%m-%d %H:%M:%S',tz="America/New_York")
minutesofday<-minutes$hour*60+minutes$min
kmeans_minutes<-kmeans(minutesofday,24)

#by doing cluster peak rides will fall in one cluster
train2$kmeans_minutes<-kmeans_minutes$cluster


#fviz_cluster(kmeans_minutes,data = kmeans_minutes)

#clusplot(train1, kmeans_minutes$cluster, color=TRUE, shade=TRUE, 
 #        labels=2, lines=0)




pickup_weekdays1<-weekdays(train2$pickup_datetime)



pickup_weekdays<-data.frame(as.character(pickup_weekdays1))
str(pickup_weekdays)

#pickup_weekdays<-as.factor(pickup_weekdays)

pickup_weekdays<-ifelse(pickup_weekdays=="Saturday"|pickup_weekdays=="Sunday","weekends","weekdays")
pickup_weekdays<-(dummy(pickup_weekdays))
train2<-cbind(train2,pickup_weekdays)
names(train2)[13]<-paste("pickup_at_weekends")
names(train2)[12]<-paste("pickup_at_weekdays")
#pickup_at_holidays$pickup_at_holidays<-as.factor(pickup_at_holidays$pickup_at_holidays)




#no_of_rides<-as.data.frame(train1$pickup_datetime%>%group_by(kmeans_minutes)%>%dplyr::summarise(no_of_rides=sum(train1$pickup_datetime)))
#table(train1$pickup_datetime)





unique(train2$passenger_count)

#No_of_passengers<-{
#                   train2$passenger_count[train2$passenger_count==1]<-1
 #                  train2$passenger_count[train2$passenger_count>=2&train2$passenger_count<=4]<-2
  #                 train2$passenger_count[train2$passenger_count>=5]<-3
   #                train2$passenger_count
    #               }
#No_of_passengers<-as.factor(No_of_passengers)

#No_of_passengers<-(dummy(No_of_passengers))


#train2<-cbind(train2,No_of_passengers)
#names(train2)[14]<-paste("one_passengers")
#names(train2)[15]<-paste("few_passengers")
#names(train2)[16]<-paste("more_passengers")



#textAddress_pickup <- mapply(FUN = function(pickup_longitude,pickup_latitude) revgeocode(c(pickup_longitude,pickup_latitude)), train1$pickup_longitude,train1$pickup_latitude)
#textAddress_dropoff<-mapply(FUN = function(dropoff_longitude,dropoff_latitude) revgeocode(c(dropoff_longitude,dropoff_latitude)), train1$dropoff_longitude,train1$dropoff_latitude)



pickup_date<-data.frame(date(train2$pickup_datetime))
names(pickup_date)[1]<-paste("pickup_date")
pickup_hour<-data.frame(hour(train2$pickup_datetime))
names(pickup_hour)[1]<-paste("pickup_hour")
pickup_month<-data.frame(month(train2$pickup_datetime))
names(pickup_month)[1]<-paste("pickup_month")
pickup_week<-data.frame(week(train2$pickup_datetime))
names(pickup_week)[1]<-paste("pickup_week")


month_sin<-sin(pickup_month*2*pi/12)
names(month_sin)[1]<-paste("month_sin")
month_cos<-cos(pickup_month*2*pi/12)
names(month_cos)[1]<-paste("month_cos")
hour_sin<-sin(pickup_hour*2*pi/24)
names(hour_sin)[1]<-paste("hour_sin")
hour_cos<-cos(pickup_hour*2*pi/24)
names(hour_cos)[1]<-paste("hour_cos")
week_sin<-sin(pickup_week*2*pi/7)
names(week_sin)[1]<-paste("week_sin")
week_cos<-cos(pickup_week*2*pi/7)
names(week_cos)[1]<-paste("week_cos")






train3<-cbind(train2,month_sin,month_cos,hour_cos,hour_sin,week_sin,week_cos
        ,distance)


train3$holidays<-as.integer(train3$holidays)
str(train3)
train3$pickup_datetime<-NULL
#train3$passenger_count<-NULL

vendorid<-dummy(train2$vendor_id)
train3<-cbind(train3,vendorid)
store_and_fwd_flag<-dummy(train3$store_and_fwd_flag)
train3<-cbind(train3,store_and_fwd_flag)
train3$vendor_id<-NULL
train3$store_and_fwd_flag<-NULL

#train2$pickup_at_holidays<-as.factor(train2$pickup_at_holidays)
#splitting the train and test
train4<-train3[1:1439168,]
test4<-train3[1439169:2064302,]


#adding target variable in to train
train4<-cbind(train4,trip_duration)

#removing unecessory columns
train4$id<-NULL
#train4$pickup_datetime<-NULL
train4$holidays<-NULL



#visualization for vendor id




#vendor_id is in integer for so it should convert in to factor
#trip_duration is not normally distributed so log transformation is used on trip_duration
#trip_durations of about a minute might still be somehow possible, assuming that someone got into a taxi but then changed their mind before the taxi could move. Whether that should count as a "trip" is a different question.
#But trip durations of about 15 minutes (900 s) without any distance covered seem hardly possible. Unless they involve traffic jams
#It is also noteworthy that most trips in the less-than-a-minute-group were from vendor 1, whereas the 10-minute-group predominantly consists of vendor 2 taxis.



train1 %>%
  ggplot(aes(log(trip_duration+1), fill = vendor_id)) +
  geom_histogram(bins = 30)





train1 %>%
  ggplot(aes(passenger_count, fill = store_and_fwd_flag)) +
  geom_histogram()



pickup_weekdays2<-weekdays(train1$pickup_datetime)
train4$week<-pickup_weekdays2

#The day of the week can have a significant impact on the predictions because we expect the weekdays to be more congested than the weekends especially during the day time.  we can clearly see that the average pickup is higher on the weekdays than the weekends

train4 %>%
  ggplot(aes(x = week)) +
  geom_bar()

#	a positive correlation between trip distance and duration. An interesting finding is that the variance of duration increases, as the trip distance increases

train4 %>%
  ggplot(aes(x = distance,y=log(trip_duration+1))) +
  geom_point()





#a positive correlation
#between trip distance and duration. An interesting finding is
#that the variance of duration increases, as the trip distance
#increases

train4 %>%
  ggplot(aes(x = distance,y=trip_duration)) +
  geom_point()


#splitting the train in to traind and validation

library(caret)
row<-createDataPartition(train4$trip_duration,times = 1,p=0.7,list=F)

trip_train<-train4[row,]
trip_test<-train4[-row,]

str(train4)
str(test4)
test4$id->testid
testid<-data.frame(testid)
test4$id<-NULL
test4$pickup_datetime<-NULL
test4$holidays<-NULL


#randomforest
control <- trainControl(method="repeatedcv", number=3,repeats = 5, search="grid")
grid <- expand.grid(mtry=9)
control
class(control)
class(grid)
View(grid)
class(train2)


#devtools::install_github('topepo/caret/pkg/caret')



library(randomForest)
Rf<-randomForest(trip_duration~.,data = train4,ntree=5)



#xgboost
library(caret)

foldsCV <- createFolds(train4$trip_duration, k=5, list=TRUE, returnTrain=FALSE)

param <- list(booster = "gbtree"
              , objective = "reg:linear"
              ,nthread = 4
              , subsample = 0.7
              , max_depth = 5
              , colsample_bytree = 0.7
              , eta = 0.02
              , eval_metric = 'rmse'
              , base_score = 0.012 #average
              , min_child_weight = 50)


# Perform xgboost cross-validation
# Won't fit under kernel limit. Uncomment to run locally.
library(xgboost)

ind_Attr = setdiff(names(train4),"trip_duration")

dtrain = xgb.DMatrix(data = as.matrix(train4[,ind_Attr]),label = train4$trip_duration)

xgb_cv <- xgb.cv(data=dtrain,
                 params=param,
                 nrounds=320,
                 prediction=TRUE,
                 maximize=FALSE,
                 folds=foldsCV,
                 early_stopping_rounds = 30,
                 print_every_n = 5)
print(xgb_cv)
prediction=TRUE
# Check best results and get best nrounds
best_model  <-    xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)]

nrounds <- xgb_cv$best_iteration


xgb <- xgb.train(params = param
                 , data = dtrain
                 # , watchlist = list(train = dtrain)
                 , nrounds =320
                 , verbose = 2
                 , print_every_n = 5
                 #, feval = amm_mae
)
  importance <- xgb.importance(feature_names = ind_Attr, model = xgb)
print(importance)
xgb.plot.importance(importance_matrix = importance)


prediction_test_xgboost<-predict(xgb,data.matrix(test4))



predictions= data.frame("id"= testid, "trip_duration"= prediction_test_xgboost)
sum(is.na(predictions))
names(predictions)[1]<-paste("id")
write.csv(predictions, "predictions.csv", row.names = F)






# Seperate the dependent and independent variables
x_data = train4[,-23]
y_data = train4[,23]



# Load 'h2o' library. 
library(h2o)
localh2o = h2o.init() # Initialize h2o. This will kickstart a jvm machine in the background.


#str(train)
#Converting R object to an H2O Object
train4.h = as.h2o(x = train4)
test4.h = as.h2o(test4)

y = 'trip_duration'
x = setdiff(names(train4.h), y)


nfolds <- 2



# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train4.h,
                          nfolds = nfolds,
               
                          ntrees = 5,mtries = 9,
                          seed = 1)




 
 model_ai<-h2o.deeplearning(
   model_id="dl_model_tuned",
   activation = 'Tanh',
   training_frame=train4.h,
   x=x, 
   y=y, 
   overwrite_with_best_model=F,    
   hidden=c(128,128),          
   epochs=10,                      
   score_validation_samples=10000, 
   score_duty_cycle=0.025,         
   adaptive_rate=F,                
   rate=0.01, 
   rate_annealing=2e-6,            
   momentum_start=0.2,             
   momentum_stable=0.4, 
   momentum_ramp=1e7, 
   l1=1e-5,
   l2=1e-5,
   max_w2=10                      
 ) 
 
pred_train = h2o.predict(model_ai, test4.h) # Predict on h2o object
pred_train_class = as.data.frame(pred_train)


predictions= data.frame("id"= testid, "trip_duration"= pred_train_class)

sum(is.na(predictions))
names(predictions)[2]<-paste("trip_duration")
names(predictions)[1]<-paste("id")
write.csv(predictions, "predictions1.csv", row.names = F)
h2o.shutdown()
y





