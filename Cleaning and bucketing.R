library(RCurl)
library(jsonlite)
#Change the  file directory to where you want to save before proceeding
dataset <- getURL("https://s3.us-east-1.amazonaws.com/blackboard.learn.xythos.prod/5956621d575cd/8614406?response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27fall2019-survey-M02%25281%2529.json&response-content-type=application%2Fjson&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20191205T072108Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAIL7WQYDOOHAZJGWQ%2F20191205%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=2ba1b41df91c5756110d1e9dfc7835f2f885f77af8ffbaf410a92259ebe9099d")
df <- jsonlite::fromJSON(dataset)
#run from here on consequent iterations
write.csv(df,"C:\\Users\\Krishna\\Desktop\\Data Science Lab\\data_1.csv") #change directory
df<-read.csv("C:\\Users\\Krishna\\Desktop\\Data Science Lab\\data_1.csv") #change directory
df$Eating.and.Drinking.at.Airport<-as.numeric(df$Eating.and.Drinking.at.Airport)
df$Day.of.Month<-as.numeric(df$Day.of.Month)
df$Partner.Code<-as.factor(df$Partner.Code)
df$Partner.Name<-as.factor(df$Partner.Name)
df$Airline.Status<-as.factor(df$Airline.Status)
df$Type.of.Travel<-as.factor(df$Type.of.Travel)
df$Gender<-as.factor(df$Gender)
df$Flight.cancelled<-as.factor(df$Flight.cancelled)
df$Price.Sensitivity<-as.factor(df$Price.Sensitivity)
df$Class<-as.factor(df$Class)
df$Likelihood.to.recommend[(df$Likelihood.to.recommend<7)] <- 0 # 'Detractor'
df$Likelihood.to.recommend[(df$Likelihood.to.recommend==7)] <- 0 #'Passive'
df$Likelihood.to.recommend[(df$Likelihood.to.recommend==8)] <- 0# 'Passive'
df$Likelihood.to.recommend[(df$Likelihood.to.recommend==9)] <- 1# 'Promoter'
df$Likelihood.to.recommend[(df$Likelihood.to.recommend==10)] <-1# 'Promoter'
df$Likelihood.to.recommend<-as.factor(df$Likelihood.to.recommend)

#Bucketing the Numerical Variables based on the qauntiles . Doing this will improve the performance of the model. We need lesser number of levels for all variables so that they dont confuse the model and to avoid high variance.
# NAs have been treated after bucketing. All Nas will be changed to the Mode(since all of them are categorical variables and have only 4-6 levels each)
quantile(df$Age,c(0,0.1,0.2,0.25,0.30,0.40,0.50,0.55,0.60,0.70,0.75,0.80,0.850,0.90,1))
df$Age<-  ifelse(df$Age<=24,'<24',
                              ifelse(df$Age<=35,'25-35',
                                     ifelse(df$Age<=46,'36-46',
                                            ifelse(df$Age<=58,'47-58',
                                                   ifelse(df$Age<=71,'57-71','71+')))))
table(df$Age)

quantile(df$Flights.Per.Year,c(0,0.1,0.2,0.25,0.30,0.40,0.50,0.55,0.60,0.70,0.75,0.80,0.850,0.90,1))
df$Flights.Per.Year<-  ifelse(df$Flights.Per.Year<= 6 ,'<6',
                 ifelse(df$Flights.Per.Year<=12,'6-12',
                        ifelse(df$Flights.Per.Year<=25,'13-25',
                               ifelse(df$Flights.Per.Year<=38  ,'26-38','38+'))))
table(df$Flights.Per.Year)

quantile(df$Loyalty,c(0,0.1,0.2,0.25,0.30,0.40,0.50,0.55,0.60,0.70,0.75,0.80,0.850,0.90,1))
df$Loyalty<-  ifelse(df$Loyalty<= -0.75 ,'<-0.75',
                              ifelse(df$Loyalty<=-0.45,'-0.7501 to -0.45',
                                     ifelse(df$Loyalty<=0.0588,'-0.451 to 0.0588',
                                            ifelse(df$Loyalty<=0.3767  ,'0.059 to 0.3768','0.3768+'))))
table(df$Loyalty)

quantile(df$Loyalty,c(0,0.1,0.2,0.25,0.30,0.40,0.50,0.55,0.60,0.70,0.75,0.80,0.850,0.90,1))
df$Loyalty<-  ifelse(df$Loyalty<= -0.75 ,'<-0.75',
                     ifelse(df$Loyalty<=-0.45,'-0.7501 to -0.45',
                            ifelse(df$Loyalty<=0.0588,'-0.451 to 0.0588',
                                   ifelse(df$Loyalty<=0.3767  ,'0.059 to 0.3768','0.3768+'))))
table(df$Loyalty)
quantile(df$Total.Freq.Flyer.Accts,c(0,0.1,0.2,0.25,0.30,0.40,0.50,0.55,0.60,0.70,0.75,0.80,0.850,0.90,1))
df$Total.Freq.Flyer.Accts<-  ifelse(df$Total.Freq.Flyer.Accts==0 ,'0',
                     ifelse(df$Total.Freq.Flyer.Accts==1,'1','1+'))

table(df$Total.Freq.Flyer.Accts)

quantile(df$Departure.Delay.in.Minutes,c(0,0.1,0.2,0.25,0.30,0.40,0.50,0.55,0.60,0.70,0.75,0.80,0.850,0.90,1),na.rm=TRUE)
df$Departure.Delay.in.Minutes<-  ifelse(df$Departure.Delay.in.Minutes==0 ,'0',
                                    ifelse(df$Departure.Delay.in.Minutes<=60,'0-60 Mins','60+Minutes'))

table(df$Departure.Delay.in.Minutes)
df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes)==TRUE)]<-'0-60 Mins'

quantile(df$Arrival.Delay.in.Minutes,c(0,0.1,0.2,0.25,0.30,0.40,0.50,0.55,0.60,0.70,0.75,0.80,0.850,0.90,1),na.rm=TRUE)
df$Arrival.Delay.in.Minutes<-  ifelse(df$Arrival.Delay.in.Minutes==0 ,'0',
                                        ifelse(df$Arrival.Delay.in.Minutes<=60,'0-60 Mins','60+Minutes'))

table(df$Arrival.Delay.in.Minutes)
df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes)==TRUE)]<-'0-60 Mins'

quantile(df$Flight.time.in.minutes,c(0,0.1,0.2,0.25,0.30,0.40,0.50,0.55,0.60,0.70,0.75,0.80,0.850,0.90,1),na.rm=TRUE)
df$Flight.time.in.minutes<-  ifelse(df$Flight.time.in.minutes<=60 ,'LESS THAN 60 MINS',
                                      ifelse(df$Flight.time.in.minutes<=180,'60 MINS TO 180 MINS','180+ Minutes'))

table(df$Flight.time.in.minutes)

quantile(df$Flight.Distance,c(0,0.1,0.2,0.25,0.30,0.40,0.50,0.55,0.60,0.70,0.75,0.80,0.850,0.90,1),na.rm=TRUE)
df$Flight.Distance<-  ifelse(df$Flight.Distance<=400  ,'LESS THAN 400 MILES',
                                    ifelse(df$Flight.Distance<=1000.0,'400 MILES TO 1000.0 MILES','1000.0+ MILES'))

table(df$Flight.Distance)
df$Flight.Distance[which(is.na(df$Flight.Distance)==TRUE)]<-'400 MILES TO 1000.0 MILES'

# Choosing the reuired variables(16) for our model . All variables need to be Strings and must have only 6 levels at max.
df_new<-df[,c("Partner.Name","Age","Gender","Airline.Status","Price.Sensitivity",
             "Loyalty","Type.of.Travel","Total.Freq.Flyer.Accts","Class","Flights.Per.Year","Departure.Delay.in.Minutes","Arrival.Delay.in.Minutes"
             ,"Flight.time.in.minutes","Flight.Distance","Flight.cancelled","Likelihood.to.recommend")]

df_new$Departure.Delay.in.Minutes[which(df_new$Flight.cancelled=='Yes')] <- '0' # Setting the flight delays to 0 if the flight has been cancelled
df_new$Arrival.Delay.in.Minutes[which(df_new$Flight.cancelled=='Yes')] <- '0'
 
#df_new$Likelihood.to.recommend[which(is.na(df_new$Likelihood.to.recommend))] = 0
write.csv(df_new,"C:\\Users\\Krishna\\Desktop\\Data Science Lab\\df_new_1.csv") # Our final data set for our Logistic regression model
