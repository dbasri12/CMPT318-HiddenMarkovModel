#Q1
# set work directory
setwd("~/desktop/SFU/CMPT\ 318/Project")

# include libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(depmixS4)
library(ggbiplot)

#import data
rawData = read.table(file = "TermProjectData.txt", header = TRUE, sep = ",")

#prepare data for analysis
datetime = as.POSIXlt(paste(rawData$Date, rawData$Time), format = "%d/%m/%Y %H:%M:%S")
rawData$Date_time = c(datetime) #adding Date_time column
rawData$Day_of_week = c(weekdays(rawData$Date_time)) #adding Day_of_week column (for selecting weekdays)
rawData$Date_only = as.POSIXlt(rawData$Date, format = "%d/%m/%Y") #adding Date_only column (for selecting a specific day)
rawData$Time_plot = as.POSIXct(rawData$Time, format = "%H:%M:%S")
weekdays_data<-subset(rawData, Day_of_week=="Monday"|Day_of_week=="Tuesday"|Day_of_week=="Wednesday"|Day_of_week=="Thursday"|Day_of_week=="Friday")
weekdays_data<- separate(weekdays_data,Time, sep = ":", into = c("Hours", "Minutes", "Seconds")) 
weekdays_data<-mutate_at(weekdays_data,c("Hours", "Minutes", "Seconds"), as.numeric)
weekdays_data<-separate(weekdays_data,Date,sep="/",into=c("Date","Month","Year"))
weekdays_data<-mutate_at(weekdays_data,c("Date","Month","Year"),as.numeric)
weekdays_data<-subset(weekdays_data,Hours >=5 & Hours <9)
ntime_train=c(240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,240,240,240,240,240,240,240,240,
              240,240,240,238,240,240,240,240,240,240,240,
              240,240,240,240,240)
ntime_test=c(240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240,240,240,240,
             240,240,240,240,240,240,240)
train_data<-subset(weekdays_data,Year<2009)
train_data<-na.exclude(train_data)
train_data<-train_data[,sapply(train_data,is.numeric)]
train_data<-train_data[,sapply(train_data,is.numeric)]
train_data[is.na(train_data)]<-0
test_data<-subset(weekdays_data,Year>=2009)
test_data[is.na(test_data)]<-0
set.seed(1)

#Preper data for PCA
PCADT = read.table(file = "TermProjectData.txt", header = TRUE, sep = ",")
PCADT = subset(PCADT, select=-Date)
PCADT = subset(PCADT, select=-Time)
PCADT[is.na(PCADT)]=0
pca = prcomp(PCADT)
pca
load = abs(pca$rotation[,1])
load
rank = sort(load, decreasing=TRUE)
pcaVariation = pca$sdev^2
pcaPercentage = round(pcaVariation/sum(pcaVariation)*100, 1)
pcaPercentage

barplot(pcaPercentage, xlab="Principal Component", ylab="Percentage")


#UNIVARIATE DATA

train_hmm5_u<-fit(depmix(response=Voltage~1,
                         data=train_data,nstates=5,
                         ntimes=ntime_train))
train_hmm8_u<-fit(depmix(response=Voltage~1,
                         data=train_data,nstates=8,
                         ntimes=ntime_train))
train_hmm11_u<-fit(depmix(response=Voltage~1,
                          data=train_data,nstates=11,
                          ntimes=ntime_train))
train_hmm14_u<-fit(depmix(response=Voltage~1,
                          data=train_data,nstates=14,
                          ntimes=ntime_train))
train_hmm13_u<-fit(depmix(response=Voltage~1,
                          data=train_data,nstates=13,
                          ntimes=ntime_train))
test_hmm_u<-depmix(response=Voltage~1,data=test_data,nstates=11,ntimes=ntime_test)
test_norm_u<-setpars(test_hmm_u,getpars(train_hmm11))
f_test_norm_u<-forwardbackward(test_norm)


#MULTIVARIATE DATA 
train_hmm5<-fit(depmix(list(Voltage~1,Sub_metering_1~1),
                       data=train_data,nstates=5,family=list(gaussian(),poisson()),
                       ntimes=ntime_train))

print(train_hmm5)

train_hmm8<-fit(depmix(list(Voltage~1,Sub_metering_1~1),
                       data=train_data,nstates=8,family=list(gaussian(),poisson()),
                       ntimes=ntime_train))

print(train_hmm8)

train_hmm11<-fit(depmix(list(Voltage~1,Sub_metering_1~1),
                    data=train_data,nstates=11,family=list(gaussian(),poisson()),
                    ntimes=ntime_train))

print(train_hmm11)

train_hmm13<-fit(depmix(list(Voltage~1,Sub_metering_1~1),
                        data=train_data,nstates=13,family=list(gaussian(),poisson()),
                        ntimes=ntime_train))

print(train_hmm13)
train_hmm14<-depmix(list(Voltage~1,Sub_metering_1~1),
                      data=train_data,nstates=14,family=list(gaussian(),poisson()),
                      ntimes=ntime_train)

test_hmm<-depmix(list(Voltage~1,Sub_metering_1~1),
                 data=test_data,nstates=13,family=list(gaussian(),poisson()),
                 ntimes=ntime_test)

test_norm<-setpars(test_hmm,getpars(train_hmm13))
f_test_norm<-forwardbackward(test_norm)

plot(1:5,c(BIC(train_hmm5),BIC(train_hmm8),BIC(train_hmm11_2),BIC(train_hmm13),BIC(train_hmm14)),ty='b',xlab="Number of States", ylab="BIC" )
par(new=TRUE)
plot(1:5,c(logLik(train_hmm5),logLik(train_hmm8),logLik(train_hmm11_2),logLik(train_hmm13),logLik(train_hmm14)),axes=FALSE,xaxt="n",yaxt="n",ylab="",col="blue",xlab="",ty='b')
axis(side=4)
plot(1:5,c(BIC(train_hmm5_u),BIC(train_hmm8_u),BIC(train_hmm11_u),BIC(train_hmm13_u),BIC(train_hmm15_u)),ty='b',xlab="Number of States", ylab="BIC" )
par(new=TRUE)
plot(1:5,c(logLik(train_hmm5_u),logLik(train_hmm8_u),logLik(train_hmm11_u),logLik(train_hmm13_u),logLik(train_hmm15_u)),axes=FALSE,xaxt="n",yaxt="n",ylab="",col="blue",xlab="",ty='b')
axis(side=4)



#Data from Data1
anomalies1 = read.table(file = "Data1(WithAnomalies).txt", header = TRUE, sep = ",")
datetime = as.POSIXlt(paste(anomalies1$Date, anomalies1$Time), format = "%d/%m/%Y %H:%M:%S")
anomalies1$Date_time = c(datetime) #adding Date_time column
anomalies1$Day_of_week = c(weekdays(anomalies1$Date_time)) #adding Day_of_week column (for selecting weekdays)
anomalies1$Date_only = as.POSIXlt(anomalies1$Date, format = "%d/%m/%Y") #adding Date_only column (for selecting a specific day)

#Data from Data2
anomalies2 = read.table(file = "Data2(WithAnomalies).txt", header = TRUE, sep = ",")
datetime = as.POSIXlt(paste(anomalies2$Date, anomalies2$Time), format = "%d/%m/%Y %H:%M:%S")
anomalies2$Date_time = c(datetime) #adding Date_time column
anomalies2$Day_of_week = c(weekdays(anomalies2$Date_time)) #adding Day_of_week column (for selecting weekdays)
anomalies2$Date_only = as.POSIXlt(anomalies2$Date, format = "%d/%m/%Y") #adding Date_only column (for selecting a specific day)

#Data from Data3
anomalies3 = read.table(file = "Data3(WithAnomalies).txt", header = TRUE, sep = ",")
datetime = as.POSIXlt(paste(anomalies3$Date, anomalies3$Time), format = "%d/%m/%Y %H:%M:%S")
anomalies3$Date_time = c(datetime) #adding Date_time column
anomalies3$Day_of_week = c(weekdays(anomalies3$Date_time)) #adding Day_of_week column (for selecting weekdays)
anomalies3$Date_only = as.POSIXlt(anomalies3$Date, format = "%d/%m/%Y") #adding Date_only column (for selecting a specific day)

#Choose variables
anomalies1 = anomalies1[,c(1,2,5,7,8,11)]
anomalies2 = anomalies2[,c(1,2,5,7,8,11)]
anomalies3 = anomalies3[,c(1,2,5,7,8,11)]

#Subset data
anomalies1 = subset(anomalies1, Time >= "05:00:00" & Time <= "09:00:00")
anomalies2 = subset(anomalies2, Time >= "05:00:00" & Time <= "09:00:00")
anomalies3 = subset(anomalies3, Time >= "05:00:00" & Time <= "09:00:00")

#Set seed
set.seed(1)

#Log-likelihood 13 states--------------------------

#log-likelihood for Data1 (13 states)
anomalies1_set <- depmix(list(Voltage~1,Sub_metering_1~1),data=anomalies1,nstates=13,family=list(gaussian(),poisson()))

anomalies1_fit <- fit(anomalies1_set)

print(anomalies1_fit)

summary(anomalies1_fit)

#log-likelihood for Data2 (13 states)
anomalies2_set <- depmix(list(Voltage~1,Sub_metering_1~1),data=anomalies2,nstates=13,family=list(gaussian(),poisson()))

anomalies2_fit <- fit(anomalies2_set)

print(anomalies2_fit)

summary(anomalies2_fit)

#log-likelihood for Data3 (13 states)
anomalies3_set <- depmix(list(Voltage~1,Sub_metering_1~1),data=anomalies3,nstates=13,family=list(gaussian(),poisson()))

anomalies3_fit <- fit(anomalies3_set)

print(anomalies3_fit)

summary(anomalies3_fit)



#Plot data
ggplot(anomalies1, aes(x=Time, y=Voltage))+
  ggtitle("Anomalies1 Set: Voltage")+
  geom_point()+
  geom_smooth(method=lm)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(anomalies1, aes(x=Time, y=Sub_metering_1))+
  ggtitle("Anomalies1 Set: Sub_metering_1")+
  geom_point()+
  geom_smooth(method=lm)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(anomalies1, aes(x=Time, y=Sub_metering_2))+
  ggtitle("Anomalies1 Set: Sub_metering_2")+
  geom_point()+
  geom_smooth(method=lm)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(anomalies2, aes(x=Time, y=Voltage))+
  ggtitle("Anomalies2 Set: Voltage")+
  geom_point()+
  geom_smooth(method=lm)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(anomalies2, aes(x=Time, y=Sub_metering_1))+
  ggtitle("Anomalies2 Set: Sub_metering_1")+
  geom_point()+
  geom_smooth(method=lm)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(anomalies2, aes(x=Time, y=Sub_metering_2))+
  ggtitle("Anomalies2 Set: Sub_metering_2")+
  geom_point()+
  geom_smooth(method=lm)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(anomalies3, aes(x=Time, y=Voltage))+
  ggtitle("Anomalies3 Set: Voltage")+
  geom_point()+
  geom_smooth(method=lm)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(anomalies3, aes(x=Time, y=Sub_metering_1))+
  ggtitle("Anomalies3 Set: Sub_metering_1")+
  geom_point()+
  geom_smooth(method=lm)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(anomalies3, aes(x=Time, y=Sub_metering_2))+
  ggtitle("Anomalies3 Set: Sub_metering_2")+
  geom_point()+
  geom_smooth(method=lm)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
