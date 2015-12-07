# Download the data files and put them in the same directory
# if your are using Rstudio, you can use the Session button from the main menu
# to set the working directory 
# Make sure you have the following libraries installed for the analysis:
#  lubridate
#  ggplot2
#  grid
#  gridExtra
#  scales
#  Hmisc
#  corrplot
#  randomForest
#  rattle
#  caret
#  e1071
#  RGTk2 (for rattle), it is recommended you install his package before rattle
#                      Refer to the link below for installation
#                      https://gist.github.com/sebkopf/9405675
#  rattle   refer to the link below for rattle installation in case you 
#           run into problems during installation
#           http://rattle.togaware.com/rattle-install-mswindows.html
#  If during installation of the packages the R session ask you to install dependencies
#  please do so.

# Since the training of the models is computationally intensive, it is suggested to save
# all the R objects created in the seession by running the command:
# save.image("yourfilenamechoice.Rdata")
# Be ware that you save the objects after loading the Rdata, else you can loose 
# Previously saved data. 

#setwd("//umons.ac.be/users/531323/Desktop/Occupancy/data_paper/")
setwd("E:/Dropbox/Occupancy/data_paper")

# Run the save and load commands according to your training progress. 
# Pay attention to not
# overwrite the file with empty or less data
save.image(file="occupancy_last.RData")

#load("occupancy_last.RData")


# Loading data
datatraining <- read.table("datatraining.txt",header=TRUE,sep=",")
datatesting <- read.table("datatest.txt",header=TRUE,sep=",")
datatesting2 <- read.table("datatest2.txt",header=TRUE,sep=",")

#Reviewing the data classes
str(datatraining)
str(datatesting)
str(datatesting2)

datatraining$Occupancy <- as.factor(datatraining$Occupancy)
datatesting$Occupancy  <- as.factor(datatesting$Occupancy)
datatesting2$Occupancy  <- as.factor(datatesting2$Occupancy)

# Formating the date class for all the files
datatraining$date <- as.POSIXct(datatraining$date,tz="UTC") 
datatesting$date <- as.POSIXct(datatesting$date,tz="UTC") 
datatesting2$date <- as.POSIXct(datatesting2$date,tz="UTC") 

library(lubridate)
# helper functions for treating date/time stamp
second_day <- function(x) {
        # x is an object in posixct format
        s <- hour(x)*3600+minute(x)*60+second(x)
        
}

weekend_weekday <- function(x) {
        val <- weekdays(x)
        if (val == "Saturday" | val == "Sunday") {
                val2 = "Weekend"
        }
        else {
                val2= "Weekday"
        }
        return(val2)
}

# Used for plotting
Relevel_weekend <- function(x) {
        
        if (x == "Weekend") {
                val2 = 0
        }
        else {
                val2= 1
        }
        return(val2)
}



# Used to add the seconds and weekend weekday  columns
datatraining_b <- datatraining
datatraining_b$NSM <- second_day(datatraining_b$date)
# Use to add the weekend/weekday label
datatraining_b$WeekStatus <-unlist(lapply(datatraining$date,weekend_weekday))
summary(datatraining_b)
str(datatraining_b)
datatraining_b$WeekStatus <-as.factor(datatraining_b$WeekStatus)
str(datatraining_b)
# for datatesting
datatesting_b <- datatesting
datatesting_b$NSM <- second_day(datatesting_b$date)
# to add the weekend/weekday label
datatesting_b$WeekStatus <-unlist(lapply(datatesting_b$date,weekend_weekday))
summary(datatesting_b)
str(datatesting_b)
datatesting_b$WeekStatus <-as.factor(datatesting_b$WeekStatus)
# for datatesting 2
datatesting2_b <- datatesting2
datatesting2_b$NSM <- second_day(datatesting2_b$date)
# to add the weekend/weekday label
datatesting2_b$WeekStatus <-unlist(lapply(datatesting2_b$date,weekend_weekday))
summary(datatesting2_b)
str(datatesting2_b)
datatesting2_b$WeekStatus <-as.factor(datatesting2_b$WeekStatus)
str(datatesting2_b)





# Turning the Occupancy variable into a factor
datatraining$Occupancy <- as.factor(datatraining$Occupancy)
datatesting$Occupancy  <- as.factor(datatesting$Occupancy)
datatesting2$Occupancy  <- as.factor(datatesting2$Occupancy)

# Reviewing the data types again
str(datatraining)
str(datatraining_b)
str(datatesting)
str(datatesting2)

# Examining the classes distribution for skewness
prop.table(table(datatraining$Occupancy))
# 0              1
# 0.7876704      0.21232
prop.table(table(datatesting$Occupancy))
# 0              1
#0.635272 0.364728 
prop.table(table(datatesting2$Occupancy))
# 0         1 
# 0.7898893 0.2101107 


# Checking that there are not NAs in the data sets... neccesary for model training.
summary(datatraining)
summary(datatesting)
summary(datatesting2)

# Exploratory Figure
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
pushViewport(viewport(layout = grid.layout(6, 1)))

myplot1 <- ggplot(datatesting,aes(date))+geom_line(color="Red",aes(y=Temperature))+ylab("Temperature")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))        

myplot2 <- ggplot(datatesting,aes(date))+geom_line(color="Blue",aes(y=Humidity))+ylab("Humidity")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))        

myplot3 <- ggplot(datatesting,aes(date))+geom_line(color="deepskyblue1",aes(y=HumidityRatio))+ylab("HumidityRatio")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))        

myplot4 <- ggplot(datatesting,aes(date))+geom_line(color="Green",aes(y=CO2))+ylab("CO2 (ppm)")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))        

myplot5 <- ggplot(datatesting,aes(date))+geom_line(color="gold4",aes(y=Light))+ylab("Light (Lux)")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))        

myplot6 <- ggplot(datatesting,aes(date))+geom_line(color="Black",aes(y=as.numeric(Occupancy)))+ylab("Occupancy")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))  

#ggplot(dataaggregated3bclenaed,aes(date))+geom_line(color="Orange",aes(y=HumidityRatio))+ylab("HumidityRatio (kg/kg)")+xlab("Time")+
#       scale_x_datetime(breaks=date_breaks("30 min"),labels=date_format("%H:%M"),
#                        limits=as.POSIXct(c("2015-02-10 11:50","2015-02-10 18:50"),tz="GMT"))+theme(axis.text.x=element_text(angle=90,hjust=1))
#



print(myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(myplot2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(myplot3, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(myplot4, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(myplot5, vp = viewport(layout.pos.row = 5, layout.pos.col = 1))
print(myplot6, vp = viewport(layout.pos.row = 6, layout.pos.col = 1))

# These commands are used to ensure that the time axis for the 
# plots are all aligned 

myplot1 <- ggplot_gtable(ggplot_build(myplot1))
myplot2 <- ggplot_gtable(ggplot_build(myplot2))
myplot3 <- ggplot_gtable(ggplot_build(myplot3))
myplot4 <- ggplot_gtable(ggplot_build(myplot4))
myplot5 <- ggplot_gtable(ggplot_build(myplot5))
myplot6 <- ggplot_gtable(ggplot_build(myplot6))


maxWidth = unit.pmax(myplot1$widths[2:3],myplot2$widths[2:3],myplot3$widths[2:3],
                     myplot4$widths[2:3],myplot5$widths[2:3],myplot6$widths[2:3])
myplot1$widths[2:3] <- maxWidth
myplot2$widths[2:3] <- maxWidth
myplot3$widths[2:3] <- maxWidth
myplot4$widths[2:3] <- maxWidth
myplot5$widths[2:3] <- maxWidth
myplot6$widths[2:3] <- maxWidth

grid.arrange(myplot1, myplot2,
             myplot3, myplot4,
             myplot5, myplot6,ncol=1)
# Run the commented line if you want to safe the plot in a png file
# png(file="occupancy2.png",width=1250,height=1200,res=75)
grid.arrange(myplot1, myplot2,
             myplot3, myplot4,
             myplot5, myplot6,ncol=1)



# Pairs plot

                         
dataset = data.frame(occupancystatus = datatraining[,"Occupancy"],
                                              lda=predict(ModelLDA_ALL,datatraining),
                                              temperature=datatraining[,"Temperature"],
                                              Light=datatraining[,"Light"])
                         
 plda <- ggplot(dataset) + geom_point(aes(temperature,Light, colour = occupancystatus, shape = occupancystatus), size = 2.5) 
                         
                         
plot(plda)

                         
cols2 <- character(nrow(datatraining))
cols2[] <- "black"
cols2[datatraining$Occupancy %in% c("0")] <- "green"
cols2[datatraining$Occupancy %in% c("1")] <- "blue"
                         
pairs(datatraining[2:6], col=cols2, cex=1.1, cex.labels=1.5)

# time in seconds and weekend_weekday
cols2 <- character(nrow(datatraining_b))
cols2[] <- "black"
cols2[datatraining_b$Occupancy %in% c("0")] <- "green"
cols2[datatraining_b$Occupancy %in% c("1")] <- "blue"

#pairs(datatraining_b[c(2,3,4,5,6,8,9)], col=cols2, cex=1.1, cex.labels=1.5)

pairs(datatraining_b[c(2,3,4,5,6,8)], col=cols2, cex=1.1, cex.labels=1.5)

#releveling the weekend_weekday for visualization purposes


datatraining_b_2 <-datatraining_b

datatraining_b_2$WeekStatus <- unlist(lapply(datatraining_b_2$WeekStatus,Relevel_weekend))



pairs(datatraining_b_2[c(2,3,4,5,6,8,9)], col=cols2, cex=1.1, cex.labels=1.5)

cor(datatraining_b[c(2,3,4,5,6,8,9)])

# Obtaining correlation between variables

#cor(datatraining[2:6])
cor(datatraining_b_2[c(2,3,4,5,6,8,9)])


library(Hmisc)
correlation_result<-rcorr(as.matrix(datatraining[2:6]))
correlation_result
correlation_result$P


rcorr(as.matrix(datatraining_b_2[c(2,3,4,5,6,8,9)]),type = "pearson")
rcorr(as.matrix(datatraining_b_2[c(2,3,4,5,6,8,9)]),type = "spearman")



correlation_result_b<-rcorr(as.matrix(datatraining_b_2[c(2,3,4,5,6,8,9)]))
correlation_result_b
correlation_result_b$P

# Plotting the correlation plot

library(corrplot)
corrplot(correlation_result_b$r,type="upper", order="hclust", tl.col="black", tl.srt=45)


# Loading caret, randomForest, svm libraries

library(randomForest)
library(rattle)
library(caret)
library(e1071)
#
# When training the models, also execute the set.seed command to ensure 
# the tranined model is reproducible
# The models with ***_b include training data with NS and WS
# 

set.seed(1234)
ModelRF_ALL_b <- train(Occupancy~.-date,method="rf",data=datatraining_b)
ModelRF_ALL_b
# Accuracy 0.993
plot(ModelRF_ALL_b)
ModelRF_ALL_b$finalModel
varImp(ModelRF_ALL_b)
plot(varImp(ModelRF_ALL_b,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b,datatraining_b))
#100.0

sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b,datatesting_b))/dim(datatesting_b)[1]*100
#  95.53% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b,datatesting_b))
# 0.9553 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b,datatesting2_b))
# 0.9806 Acuracy




# Creating a plot of the random forest error
dev.off()
plot(1:500,ModelRF_ALL_b$finalModel$err.rate[,1], 
     ylim=range(c(ModelRF_ALL_b$finalModel$err.rate[,1], ModelRF_ALL_b$finalModel$err.rate[,2],
                  ModelRF_ALL_b$finalModel$err.rate[,3])),type='l',col='blue',axes=TRUE,xlab="Number of Trees",ylab="Error")
par(new=TRUE)
plot(1:500,ModelRF_ALL_b$finalModel$err.rate[,2],
     ylim=range(c(ModelRF_ALL_b$finalModel$err.rate[,1], ModelRF_ALL_b$finalModel$err.rate[,2],
                  ModelRF_ALL_b$finalModel$err.rate[,3])),type='l',col="red",axes=TRUE,xlab="Number of Trees",ylab="Error")
par(new=TRUE)
plot(1:500,ModelRF_ALL_b$finalModel$err.rate[,3],ylim=range(c(ModelRF_ALL_b$finalModel$err.rate[,1],
                                                              ModelRF_ALL_b$finalModel$err.rate[,2],
                                                              ModelRF_ALL_b$finalModel$err.rate[,3])),type='l',xlab="Number of Trees",ylab="Error")

legend("topright",legend= c("OOB","Not Occupied (0)","Occupied (1)"), bty ="n", lwd=1,
       col=c("blue","red","black"))
##






# T, H, Co2, W, NS, WS (no light)

set.seed(1234)
ModelRF_ALL_b_no_light <- train(Occupancy~.-date-Light,method="rf",data=datatraining_b)
ModelRF_ALL_b_no_light
# Accuracy 0.9926
plot(ModelRF_ALL_b_no_light)
ModelRF_ALL_b_no_light$finalModel
varImp(ModelRF_ALL_b_no_light)
plot(varImp(ModelRF_ALL_b_no_light,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_no_light,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_no_light,datatesting_b))/dim(datatesting_b)[1]*100
#  94.63% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_no_light,datatesting_b))
# 94.63 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_no_light,datatesting2_b))
# 64.86 Acuracy



# T, H, Light, W, NS, WS (no CO2)

set.seed(1234)
ModelRF_ALL_b_no_CO2 <- train(Occupancy~.-date-CO2,method="rf",data=datatraining_b)
ModelRF_ALL_b_no_CO2
# Accuracy 0.9936
plot(ModelRF_ALL_b_no_CO2)
ModelRF_ALL_b_no_CO2$finalModel
varImp(ModelRF_ALL_b_no_CO2)
plot(varImp(ModelRF_ALL_b_no_CO2,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_no_CO2,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_no_CO2,datatesting_b))/dim(datatesting_b)[1]*100
#  95.64% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_no_CO2,datatesting_b))
# 95.65 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_no_CO2,datatesting2_b))
# 97.21 Acuracy





# T, H, Light, W, NS, WS (no CO2, no Light)

set.seed(1234)
ModelRF_ALL_b_no_CO2noLight <- train(Occupancy~.-date-CO2-Light,method="rf",data=datatraining_b)
ModelRF_ALL_b_no_CO2noLight
# Accuracy 0.9924
plot(ModelRF_ALL_b_no_CO2noLight)
ModelRF_ALL_b_no_CO2noLight$finalModel
varImp(ModelRF_ALL_b_no_CO2noLight)
plot(varImp(ModelRF_ALL_b_no_CO2noLight,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_no_CO2noLight,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_no_CO2noLight,datatesting_b))/dim(datatesting_b)[1]*100
#  94.86% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_no_CO2noLight,datatesting_b))
# 94.86 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_no_CO2noLight,datatesting2_b))
# 91.66 Acuracy






# T, Light,  NS, WS 

set.seed(1234)
ModelRF_ALL_b_T_Light <- train(Occupancy~.-date-CO2-Humidity-HumidityRatio,method="rf",data=datatraining_b)
ModelRF_ALL_b_T_Light
# Accuracy 0.99397
plot(ModelRF_ALL_b_T_Light)
ModelRF_ALL_b_T_Light$finalModel
varImp(ModelRF_ALL_b_T_Light)
plot(varImp(ModelRF_ALL_b_T_Light,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_T_Light,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_T_Light,datatesting_b))/dim(datatesting_b)[1]*100
#  95.497% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_T_Light,datatesting_b))
# 95.5 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_T_Light,datatesting2_b))
# 97.28 Acuracy






# Humidity, Light,  NS, WS 

set.seed(1234)
ModelRF_ALL_b_Humidity_Light <- train(Occupancy~.-date-CO2-Temperature-HumidityRatio,method="rf",data=datatraining_b)
ModelRF_ALL_b_Humidity_Light
# Accuracy 0.9939
plot(ModelRF_ALL_b_Humidity_Light)
ModelRF_ALL_b_Humidity_Light$finalModel
varImp(ModelRF_ALL_b_Humidity_Light)
plot(varImp(ModelRF_ALL_b_Humidity_Light ,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_Humidity_Light,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_Humidity_Light,datatesting_b))/dim(datatesting_b)[1]*100
#  96.92% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_Humidity_Light,datatesting_b))
# 96.92 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_Humidity_Light,datatesting2_b))
# 98.31 Acuracy





# CO2, Light,  NS, WS 

set.seed(1234)
ModelRF_ALL_b_CO2_Light <- train(Occupancy~.-date-Humidity-Temperature-HumidityRatio,method="rf",data=datatraining_b)
ModelRF_ALL_b_CO2_Light
# Accuracy 0.9941
plot(ModelRF_ALL_b_CO2_Light)
ModelRF_ALL_b_CO2_Light$finalModel
varImp(ModelRF_ALL_b_CO2_Light)
plot(varImp(ModelRF_ALL_b_CO2_Light,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_CO2_Light,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_CO2_Light,datatesting_b))/dim(datatesting_b)[1]*100
#  96.06% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_CO2_Light,datatesting_b))
# 96.06 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_CO2_Light,datatesting2_b))
# 97.42 Acuracy





# CO2, Temp,  NS, WS 

set.seed(1234)
ModelRF_ALL_b_CO2_Temp <- train(Occupancy~.-date-Humidity-Light-HumidityRatio,method="rf",data=datatraining_b)
ModelRF_ALL_b_CO2_Temp
# Accuracy 0.9925
plot(ModelRF_ALL_b_CO2_Temp)
ModelRF_ALL_b_CO2_Temp$finalModel
varImp(ModelRF_ALL_b_CO2_Temp)
plot(varImp(ModelRF_ALL_b_CO2_Temp,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_CO2_Temp,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_CO2_Temp,datatesting_b))/dim(datatesting_b)[1]*100
#  95.64% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_CO2_Temp,datatesting_b))
# 95.65 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_CO2_Temp,datatesting2_b))
# 57.85 Acuracy



# Light,W, NS, WS 

set.seed(1234)
ModelRF_ALL_b_Light_W <- train(Occupancy~.-date-Humidity-Temperature-CO2,method="rf",data=datatraining_b)
ModelRF_ALL_b_Light_W
# Accuracy 0.9936
plot(ModelRF_ALL_b_Light_W)
ModelRF_ALL_b_Light_W$finalModel
varImp(ModelRF_ALL_b_Light_W)
plot(varImp(ModelRF_ALL_b_Light_W,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_Light_W,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_Light_W,datatesting_b))/dim(datatesting_b)[1]*100
#  96.47% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_Light_W,datatesting_b))
# 96.47 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_Light_W,datatesting2_b))
# 98.58 Acuracy




# Temperature,Humidity, NS, WS 

set.seed(1234)
ModelRF_ALL_b_Temperature_Humidity <- train(Occupancy~.-date-Light-HumidityRatio-CO2,method="rf",data=datatraining_b)
ModelRF_ALL_b_Temperature_Humidity
# Accuracy 0.9930
plot(ModelRF_ALL_b_Temperature_Humidity)
ModelRF_ALL_b_Temperature_Humidity$finalModel
varImp(ModelRF_ALL_b_Temperature_Humidity)
plot(varImp(ModelRF_ALL_b_Temperature_Humidity,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_Temperature_Humidity,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_Temperature_Humidity,datatesting_b))/dim(datatesting_b)[1]*100
#  96.67% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_Temperature_Humidity,datatesting_b))
# 94.67 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_Temperature_Humidity,datatesting2_b))
# 91.91 Acuracy








# Co2, NS, WS 

set.seed(1234)
ModelRF_ALL_b_CO2 <- train(Occupancy~.-date-Light-HumidityRatio-Temperature-Humidity,method="rf",data=datatraining_b)
ModelRF_ALL_b_CO2
# Accuracy 0.9923
plot(ModelRF_ALL_b_CO2)
ModelRF_ALL_b_CO2$finalModel
varImp(ModelRF_ALL_b_CO2)
plot(varImp(ModelRF_ALL_b_CO2,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_CO2,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_CO2,datatesting_b))/dim(datatesting_b)[1]*100
#  96.36% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_CO2,datatesting_b))
# 96.36 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_CO2,datatesting2_b))
# 65.2 Acuracy








# Temperature,NS, WS 

set.seed(1234)
ModelRF_ALL_b_Temperature <- train(Occupancy~.-date-Light-Humidity-HumidityRatio-CO2,method="rf",data=datatraining_b)
ModelRF_ALL_b_Temperature
# Accuracy 0.99
plot(ModelRF_ALL_b_Temperature)
ModelRF_ALL_b_Temperature$finalModel
varImp(ModelRF_ALL_b_Temperature)
plot(varImp(ModelRF_ALL_b_Temperature,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_Temperature,datatraining_b))
#99.88
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_Temperature,datatesting_b))/dim(datatesting_b)[1]*100
#  95.08% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_Temperature,datatesting_b))
# 95.08 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_Temperature,datatesting2_b))
# 92.52 Acuracy





# Light,NS, WS 

set.seed(1234)
ModelRF_ALL_b_Light <- train(Occupancy~.-date-Temperature-Humidity-HumidityRatio-CO2,method="rf",data=datatraining_b)
ModelRF_ALL_b_Light
# Accuracy 0.991
plot(ModelRF_ALL_b_Light)
ModelRF_ALL_b_Light$finalModel
varImp(ModelRF_ALL_b_Light)
plot(varImp(ModelRF_ALL_b_Light,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_Light,datatraining_b))
#99.88
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_Light,datatesting_b))/dim(datatesting_b)[1]*100
#  96.99% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_Light,datatesting_b))
# 97.00 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_Light,datatesting2_b))
# 97.81 Acuracy















# First obtaining the random forest models
set.seed(1234)
ModelRF_ALL <- train(Occupancy~.-date,method="rf",data=datatraining)
ModelRF_ALL
# Accuracy 0.993
plot(ModelRF_ALL)
ModelRF_ALL$finalModel
varImp(ModelRF_ALL)
plot(varImp(ModelRF_ALL,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_ALL,datatraining))
#100.0

sum(datatesting$Occupancy==predict(ModelRF_ALL,datatesting))/dim(datatesting)[1]*100
#  95.05% acuracy 
set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_ALL,datatesting))
# 0.9505 Acuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_ALL,datatesting2))
# 0.9716 Acuracy


## without Light
set.seed(1234)
ModelRF_noLight <- train(Occupancy~.-date-Light,method="rf",data=datatraining)
ModelRF_noLight
# Accuracy 0.9864
plot(ModelRF_noLight)
ModelRF_noLight$finalModel
varImp(ModelRF_noLight)
plot(varImp(ModelRF_noLight))

set.seed(1234)
sum(datatesting$Occupancy==predict(ModelRF_noLight,datatesting))/dim(datatesting)[1]*100
#  69.3059% acuracy 

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_noLight,datatraining))
# 0.9998
set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_noLight,datatesting))
# 0.6863 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_noLight,datatesting2))
# 0.3268 Accuracy

## without CO2
set.seed(1234)
ModelRF_noCO2 <- train(Occupancy~.-date-CO2,method="rf",data=datatraining)
ModelRF_noCO2

# Accuracy 0.9925
plot(ModelRF_noCO2)
ModelRF_noCO2$finalModel
varImp(ModelRF_noCO2)
plot(varImp(ModelRF_noCO2))

set.seed(1234)
sum(datatesting$Occupancy==predict(ModelRF_noCO2,datatesting))/dim(datatesting)[1]*100
# 93.9962 % acuracy 
set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_noCO2,datatraining))
#99.95

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_noCO2,datatesting))
#  .9403 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_noCO2,datatesting2))
# .9673  Accuracy
 
## without Light and CO2
set.seed(1234)
ModelRF_noCO2noLight <- train(Occupancy~.-date-CO2-Light,method="rf",data=datatraining)
ModelRF_noCO2noLight

# Accuracy 0.9697
plot(ModelRF_noCO2noLight)
ModelRF_noCO2noLight$finalModel
varImp(ModelRF_noCO2noLight)
plot(varImp(ModelRF_noCO2noLight))

set.seed(1234)
sum(datatesting$Occupancy==predict(ModelRF_noCO2noLight,datatesting))/dim(datatesting)[1]*100
# 75.68 % acuracy 
set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_noCO2noLight,datatraining))
#99.36

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_noCO2noLight,datatesting))
#  0.7568 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_noCO2noLight,datatesting2))
# 0.6259  Accuracy

# 
## without Light and CO2 and HUR
set.seed(1234)
ModelRF_noCO2noLightnoHumidityRatio <- train(Occupancy~.-date-CO2-Light-HumidityRatio
                                             ,method="rf",data=datatraining)
ModelRF_noCO2noLightnoHumidityRatio
# 0.968753
ModelRF_noCO2noLightnoHumidityRatio$finalModel$xNames


plot()
ModelRF_noCO2noLightnoHumidityRatio$finalModel
varImp(ModelRF_noCO2noLightnoHumidityRatio)
plot(varImp(ModelRF_noCO2noLightnoHumidityRatio))

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_noCO2noLightnoHumidityRatio,datatraining))
# 99.36



set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_noCO2noLightnoHumidityRatio,datatesting))
#  0.7565 Accuracy


set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_noCO2noLightnoHumidityRatio,datatesting2))
# 0.673  Accuracy







## RF CO2 and Temperature
set.seed(1234)
ModelRF_CO2andTemp <- train(Occupancy~.-date-Humidity-Light-HumidityRatio
                                             ,method="rf",data=datatraining)
ModelRF_CO2andTemp
# 0.9633583
ModelRF_CO2andTemp$finalModel$xNames


plot()
ModelRF_CO2andTemp$finalModel
varImp(ModelRF_CO2andTemp)
plot(varImp(ModelRF_CO2andTemp))

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_CO2andTemp,datatraining))
# 99.88



set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_CO2andTemp,datatesting))
#  0.7531 Accuracy


set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_CO2andTemp,datatesting2))
# 0.3693  Accuracy








# Model Random forest (only Temperature)

set.seed(1234)
ModelRF_Temp <- train(Occupancy~.-date-CO2-Light-HumidityRatio-Humidity
                                             ,method="rf",data=datatraining)
ModelRF_Temp
# 0.86305
ModelRF_Temp$finalModel$xNames


plot()
ModelRF_Temp$finalModel


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_Temp,datatraining))
# 87.87


set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_Temp,datatesting))
#  0.7073 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_noCO2noLightnoHumidityRatio,datatesting2))
# 0.673  Accuracy






# Model Random forest (only LIGHT)

set.seed(1234)
ModelRF_Light<- train(Occupancy~.-date-CO2-Temperature-HumidityRatio-Humidity
                      ,method="rf",data=datatraining)
ModelRF_Light
# 0.9826
ModelRF_Light$finalModel$xNames


plot()
ModelRF_Temp$finalModel


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_Light,datatraining))
# 0.992


set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_Light,datatesting))
#  0.9568 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_Light,datatesting2))
# 0.9791  Accuracy






# Model Random forest (only CO2)

set.seed(1234)
ModelRF_CO2 <- train(Occupancy~.-date-Light-Temperature-HumidityRatio-Humidity
                      ,method="rf",data=datatraining)
ModelRF_CO2
# 0.9826
ModelRF_CO2$finalModel$xNames


plot()
ModelRF_Temp$finalModel


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_CO2,datatraining))
# 0.9753



set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_CO2,datatesting))
#  0.7876 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_CO2,datatesting2))
# 0.6421  Accuracy




















# Model Random forest (Temperature&Light)

set.seed(1234)
ModelRF_TempandLight <- train(Occupancy~.-date-CO2-HumidityRatio-Humidity
                      ,method="rf",data=datatraining)
ModelRF_TempandLight
# 0.98902
ModelRF_TempandLight$finalModel$xNames



ModelRF_TempandLight$finalModel


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_TempandLight,datatraining))
# 99.86

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_TempandLight,datatesting))
#  0.931 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_TempandLight,datatesting2))
# 0.9593  Accuracy

# Model Random forest (Humidity&Light)

set.seed(1234)
ModelRF_HumidityndLight <- train(Occupancy~.-date-CO2-HumidityRatio-Temperature
                              ,method="rf",data=datatraining)
ModelRF_HumidityndLight
# 0.98902
ModelRF_HumidityndLight$finalModel$xNames


plot()
ModelRF_TempandLight$finalModel


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_HumidityndLight,datatraining))
# 99.96

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_HumidityndLight,datatesting))
#  0.9235 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_HumidityndLight,datatesting2))
# 0.9436  Accuracy





# Model Random forest (Light&CO2)

set.seed(1234)
ModelRF_LightandCO2 <- train(Occupancy~.-date-HumidityRatio-Temperature-Humidity
                                 ,method="rf",data=datatraining)
ModelRF_LightandCO2
# 0.9867
ModelRF_LightandCO2$finalModel$xNames


plot()
ModelRF_LightandCO2$finalModel


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_LightandCO2,datatraining))
# 99.95

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_LightandCO2,datatesting))
#  0.9261 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_LightandCO2,datatesting2))
# 0.9741  Accuracy


# Model Random forest (Light&HumidityRatio)

set.seed(1234)
ModelRF_LightandHR <- train(Occupancy~.-date-Humidity-Temperature-CO2
                             ,method="rf",data=datatraining)
ModelRF_LightandHR 
# 0.9867
ModelRF_LightandHR$finalModel$xNames


plot()
ModelRF_LightandHR$finalModel


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelRF_LightandHR,datatraining))
# 99.98

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelRF_LightandHR,datatesting))
#  0.9576 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelRF_LightandHR,datatesting2))
# 0.9768  Accuracy







# Second obtaining the gbm models

gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3,4),
                        n.trees = (1:30)*20,
                        shrinkage = 0.1,
                        n.minobsinnode = 10)

# fitControl <- trainControl(## 10-fold CV
#         method = "repeatedcv",
#         number = 10,
#         ## repeated ten times
#         repeats = 10)


set.seed(1234)
Modelgbm_ALL_b <- train(Occupancy~.-date,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_ALL_b
# Accuracy 0.992
plot(Modelgbm_ALL_b)


Modelgbm_ALL_b$finalModel
varImp(Modelgbm_ALL_b)
plot(varImp(Modelgbm_ALL_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_ALL_b,datatesting_b))/dim(datatesting_b)[1]*100
#  95.75985 acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_ALL_b,datatraining_b))
# 100.00

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_ALL_b,datatesting_b))
# 0.9576 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_ALL_b,datatesting2_b))
# 0.961 Acuracy



#Testing expanded Grid


set.seed(1234)
Modelgbm_ALL_b_eg <- train(Occupancy~.-date,data=datatraining_b,method="gbm",
                        tuneGrid = gbmGrid)
Modelgbm_ALL_b_eg
# Accuracy 0.992
plot(Modelgbm_ALL_b_eg)
Modelgbm_ALL_b_eg$finalModel
varImp(Modelgbm_ALL_b_eg)
plot(varImp(Modelgbm_ALL_b_eg))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_ALL_b_eg,datatesting_b))/dim(datatesting_b)[1]*100
#  94.446% acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_ALL_b_eg,datatraining_b))
# 99.66

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_ALL_b_eg,datatesting_b))
# 0.9445 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_ALL_b_eg,datatesting2_b))
# 0.9559 Acuracy













## without Light, but with NS and WS

set.seed(1234)
Modelgbm_nolight_b <- train(Occupancy~.-date-Light,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_nolight_b
# Accuracy 0.9917
plot(Modelgbm_nolight_b)
Modelgbm_nolight_b$finalModel
varImp(Modelgbm_nolight_b)
plot(varImp(Modelgbm_nolight_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_nolight_b,datatesting_b))/dim(datatesting_b)[1]*100
#  91.85% acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_nolight_b,datatraining_b))
# 100

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_nolight_b,datatesting_b))
# 0.9122 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_nolight_b,datatesting2_b))
# 0.5114 Acuracy


## without CO2, but with NS and WS

set.seed(1234)
Modelgbm_noCO2_b <- train(Occupancy~.-date-CO2,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_noCO2_b
# Accuracy 0.9932
plot(Modelgbm_noCO2_b)
Modelgbm_noCO2_b$finalModel
varImp(Modelgbm_noCO2_b)
plot(varImp(Modelgbm_noCO2_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_noCO2_b,datatesting_b))/dim(datatesting_b)[1]*100
#  95.98% acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_noCO2_b,datatraining_b))
# 100.0
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_noCO2_b,datatesting_b))
# 0.9598 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_noCO2_b,datatesting2_b))
# 0.9751 Acuracy





## without CO2, LIGHT, but with NS and WS

set.seed(1234)
Modelgbm_noCO2noLIGHT_b <- train(Occupancy~.-date-CO2-Light,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_noCO2noLIGHT_b
# Accuracy 0.9925
plot(Modelgbm_noCO2noLIGHT_b)
Modelgbm_noCO2noLIGHT_b$finalModel
varImp(Modelgbm_noCO2noLIGHT_b)
plot(varImp(Modelgbm_noCO2noLIGHT_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_noCO2noLIGHT_b,datatesting_b))/dim(datatesting_b)[1]*100
#  95.49% acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_noCO2noLIGHT_b,datatraining_b))
# 99.98

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_noCO2noLIGHT_b,datatesting_b))
# 0.955 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_noCO2noLIGHT_b,datatesting2_b))
# 0.9249 Acuracy





##  Light, Temperature, NS, WS

set.seed(1234)
Modelgbm_T_and_LIGHT_b <- train(Occupancy~.-date-CO2-Humidity-HumidityRatio,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_T_and_LIGHT_b
# Accuracy 0.9931
plot(Modelgbm_T_and_LIGHT_b)
Modelgbm_T_and_LIGHT_b$finalModel
varImp(Modelgbm_T_and_LIGHT_b)
plot(varImp(Modelgbm_T_and_LIGHT_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_T_and_LIGHT_b,datatesting_b))/dim(datatesting_b)[1]*100
#  96.36% acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_T_and_LIGHT_b,datatraining_b))
# 99.88

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_T_and_LIGHT_b,datatesting_b))
# 0.9636 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_T_and_LIGHT_b,datatesting2_b))
# 0.9881 Acuracy





##  Light, Humidity, NS, WS

set.seed(1234)
Modelgbm_Humidity_and_LIGHT_b <- train(Occupancy~.-date-CO2-Temperature-HumidityRatio,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_Humidity_and_LIGHT_b
# Accuracy 0.9913
plot(Modelgbm_Humidity_and_LIGHT_b)
Modelgbm_Humidity_and_LIGHT_b$finalModel
varImp(Modelgbm_Humidity_and_LIGHT_b)
plot(varImp(Modelgbm_Humidity_and_LIGHT_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_Humidity_and_LIGHT_b,datatesting_b))/dim(datatesting_b)[1]*100
#  96.7223% acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_Humidity_and_LIGHT_b,datatraining_b))
# 99.99

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_Humidity_and_LIGHT_b,datatesting_b))
# 0.9659 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_Humidity_and_LIGHT_b,datatesting2_b))
# 0.9863 Acuracy




##  Light, CO2, NS, WS

set.seed(1234)
Modelgbm_CO2_and_LIGHT_b <- train(Occupancy~.-date-Humidity-Temperature-HumidityRatio,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_CO2_and_LIGHT_b
# Accuracy 0.9904
plot(Modelgbm_CO2_and_LIGHT_b)
Modelgbm_CO2_and_LIGHT_b$finalModel
varImp(Modelgbm_CO2_and_LIGHT_b)
plot(varImp(Modelgbm_Humidity_and_LIGHT_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_CO2_and_LIGHT_b,datatesting_b))/dim(datatesting_b)[1]*100
#  96.06% acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_CO2_and_LIGHT_b,datatraining_b))
# 100.00

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_CO2_and_LIGHT_b,datatesting_b))
# 0.9606 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_CO2_and_LIGHT_b,datatesting2_b))
# 0.9857 Acuracy





##  Tem, CO2, NS, WS

set.seed(1234)
Modelgbm_CO2_and_Tem_b <- train(Occupancy~.-date-Humidity-Light-HumidityRatio,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_CO2_and_Tem_b
# Accuracy 0.9902
plot(Modelgbm_CO2_and_Tem_b)
Modelgbm_CO2_and_Tem_b$finalModel
varImp(Modelgbm_CO2_and_Tem_b)
plot(varImp(Modelgbm_CO2_and_Tem_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_CO2_and_Tem_b,datatesting_b))/dim(datatesting_b)[1]*100
#  91.13% acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_CO2_and_Tem_b,datatraining_b))
# 99.98

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_CO2_and_Tem_b,datatesting_b))
# 0.9614 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_CO2_and_Tem_b,datatesting2_b))
# 0.7771 Acuracy




##  Light,W, NS, WS

set.seed(1234)
Modelgbm_Light_and_W <- train(Occupancy~.-date-Humidity-CO2-Temperature,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_Light_and_W
# Accuracy 0.9931
plot(Modelgbm_Light_and_W)
Modelgbm_Light_and_W$finalModel
varImp(Modelgbm_Light_and_W)
plot(varImp(Modelgbm_Light_and_W))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_Light_and_W,datatesting_b))/dim(datatesting_b)[1]*100
#  96.735 acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_Light_and_W,datatraining_b))
# 99.99

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_Light_and_W,datatesting_b))
# 0.9674 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_Light_and_W,datatesting2_b))
# 0.9903 Acuracy



##  Temperat,Humidity, NS,WS

set.seed(1234)
Modelgbm_Temp_and_Humidity <- train(Occupancy~.-date-HumidityRatio-CO2-Light,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_Temp_and_Humidity 
# Accuracy 0.9895
plot(Modelgbm_Temp_and_Humidity)
Modelgbm_Temp_and_Humidity$finalModel
varImp(Modelgbm_Temp_and_Humidity)
plot(varImp(Modelgbm_Temp_and_Humidity))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_Temp_and_Humidity,datatesting_b))/dim(datatesting_b)[1]*100
#  95.609 acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_Temp_and_Humidity,datatraining_b))
# 99.95

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_Temp_and_Humidity,datatesting_b))
# 0.9561 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_Temp_and_Humidity,datatesting2_b))
# 0.9285 Acuracy






##  Temperat,NS,WS

set.seed(1234)
Modelgbm_Temp_b <- train(Occupancy~.-date-HumidityRatio-Humidity-CO2-Light,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_Temp_b
# Accuracy 0.9890
plot(Modelgbm_Temp_b)
Modelgbm_Temp_b$finalModel
varImp(Modelgbm_Temp_b)
plot(varImp(Modelgbm_Temp_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_Temp_b,datatesting_b))/dim(datatesting_b)[1]*100
#  95.90 acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_Temp_b,datatraining_b))
# 99.75

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_Temp_b,datatesting_b))
# 0.9591 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_Temp_b,datatesting2_b))
# 0.9257 Acuracy



##  Light,NS,WS

set.seed(1234)
Modelgbm_Light_b <- train(Occupancy~.-date-HumidityRatio-Humidity-CO2-Temperature,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_Light_b
# Accuracy 0.99
plot(Modelgbm_Light_b)
Modelgbm_Light_b$finalModel
varImp(Modelgbm_Light_b)
plot(varImp(Modelgbm_Light_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_Light_b,datatesting_b))/dim(datatesting_b)[1]*100
#  95.947 acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_Light_b,datatraining_b))
# 99.74

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_Light_b,datatesting_b))
# 0.9595 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_Light_b,datatesting2_b))
# 0.9847 Acuracy




##  Co2, NS,WS

set.seed(1234)
Modelgbm_CO2_b <- train(Occupancy~.-date-Light-HumidityRatio-Humidity-HumidityRatio-Temperature,method="gbm",data=datatraining_b,tuneGrid = gbmGrid)
Modelgbm_CO2_b
# Accuracy 0.9913
plot(Modelgbm_CO2_b)
Modelgbm_CO2_b$finalModel
varImp(Modelgbm_CO2_b)
plot(varImp(Modelgbm_CO2_b))

set.seed(1234)
sum(datatesting_b$Occupancy==predict(Modelgbm_CO2_b,datatesting_b))/dim(datatesting_b)[1]*100
#  96.21 acuracy 
set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(Modelgbm_CO2_b,datatraining_b))
# 99.99

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(Modelgbm_CO2_b,datatesting_b))
# 0.9621 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(Modelgbm_CO2_b,datatesting2_b))
# 0.7451 Acuracy









set.seed(1234)
Modelgbm_ALL <- train(Occupancy~.-date,method="gbm",data=datatraining,tuneGrid = gbmGrid)
Modelgbm_ALL
# Accuracy 0.9929
plot(Modelgbm_ALL)
Modelgbm_ALL$finalModel
varImp(Modelgbm_ALL)
plot(varImp(Modelgbm_ALL))

set.seed(1234)
sum(datatesting$Occupancy==predict(Modelgbm_ALL,datatesting))/dim(datatesting)[1]*100
#  93.05% acuracy 
set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(Modelgbm_ALL,datatraining))
# 99.98

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(Modelgbm_ALL,datatesting))
# 0.9306 Acuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(Modelgbm_ALL,datatesting2))
# 0.9514 Acuracy


## without Light
set.seed(1234)
Modelgbm_noLight <- train(Occupancy~.-date-Light,method="gbm",data=datatraining,tuneGrid = gbmGrid)
Modelgbm_noLight
# Accuracy 0.9696
plot(Modelgbm_noLight)
Modelgbm_noLight$finalModel
varImp(Modelgbm_noLight)
plot(varImp(Modelgbm_noLight))

set.seed(1234)
sum(datatesting$Occupancy==predict(Modelgbm_noLight,datatesting))/dim(datatesting)[1]*100
#  69.53% acuracy 
set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(Modelgbm_noLight,datatraining))
# 99.56

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(Modelgbm_noLight,datatesting))
# 0.6953 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(Modelgbm_noLight,datatesting2))
# 0.3881 Accuracy

## without CO2
set.seed(1234)
Modelgbm_noCO2 <- train(Occupancy~.-date-CO2,method="gbm",data=datatraining,tuneGrid = gbmGrid)
Modelgbm_noCO2

# Accuracy 0.9921
plot(Modelgbm_noCO2)
Modelgbm_noCO2$finalModel
varImp(Modelgbm_noCO2)
plot(varImp(Modelgbm_noCO2))

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(Modelgbm_noCO2,datatraining))
# 99.96

set.seed(1234)
sum(datatesting$Occupancy==predict(Modelgbm_noCO2,datatesting))/dim(datatesting)[1]*100
# 92.42 % acuracy 

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(Modelgbm_noCO2,datatesting))
#  .92422 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(Modelgbm_noCO2,datatesting2))
# .9527  Accuracy

## without Light and CO2
set.seed(1234)
Modelgbm_noCO2noLight <- train(Occupancy~.-date-CO2-Light,method="gbm",data=datatraining,tuneGrid = gbmGrid)
Modelgbm_noCO2noLight

# Accuracy 0.9636
set.seed(1234)
plot(Modelgbm_noCO2noLight)
Modelgbm_noCO2noLight$finalModel

varImp(Modelgbm_noCO2noLight)
plot(varImp(Modelgbm_noCO2noLight))

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(Modelgbm_noCO2noLight,datatraining))
#97.85%

set.seed(1234)
sum(datatesting$Occupancy==predict(Modelgbm_noCO2noLight,datatesting))/dim(datatesting)[1]*100
# 76.09 % acuracy 

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(Modelgbm_noCO2noLight,datatesting))
#  0.761 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(Modelgbm_noCO2noLight,datatesting2))
# 0.6375  Accuracy


## GBM without Light and CO2 and HUR
set.seed(1234)
ModelGBM_noCO2noLightnoHumidityRatio <- train(Occupancy~.-date-CO2-Light-HumidityRatio
                                             ,method="gbm",data=datatraining,tuneGrid = gbmGrid)
ModelGBM_noCO2noLightnoHumidityRatio
# 0.9646
ModelGBM_noCO2noLightnoHumidityRatio$finalModel$xNames


plot(ModelGBM_noCO2noLightnoHumidityRatio)
ModelGBM_noCO2noLightnoHumidityRatio$finalModel
varImp(ModelGBM_noCO2noLightnoHumidityRatio)
plot(varImp(ModelGBM_noCO2noLightnoHumidityRatio))

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelGBM_noCO2noLightnoHumidityRatio,datatraining))
#97.85

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelGBM_noCO2noLightnoHumidityRatio,datatesting))
#  0.7749 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelGBM_noCO2noLightnoHumidityRatio,datatesting2))
# 0.6534  Accuracy





## GBM CO2 and T

set.seed(1234)
ModelGBM_CO2_Temperature <- train(Occupancy~.-date-Humidity-Light-HumidityRatio
                                              ,method="gbm",data=datatraining,tuneGrid = gbmGrid)
ModelGBM_CO2_Temperature 
# 0.9582
ModelGBM_CO2_Temperature$finalModel$xNames


plot()
ModelGBM_CO2_Temperature$finalModel
varImp(ModelGBM_CO2_Temperature)
plot(varImp(ModelGBM_CO2_Temperature))

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelGBM_CO2_Temperature,datatraining))
#97.76

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelGBM_CO2_Temperature,datatesting))
#  0.7298 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelGBM_CO2_Temperature,datatesting2))
# 0.4646  Accuracy





#GBM only temperature

set.seed(1234)
ModelGBM_TEMP <- train(Occupancy~.-date-CO2-Light-HumidityRatio-Humidity
                                              ,method="gbm",data=datatraining,tuneGrid = gbmGrid)
ModelGBM_TEMP
# 0.8659
ModelGBM_TEMP$finalModel$xNames

ModelGBM_TEMP$finalModel
plot(ModelGBM_TEMP)


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelGBM_TEMP,datatraining))
#87.44


set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelGBM_TEMP,datatesting))
#  0.6469 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelGBM_TEMP,datatesting2))
# 0.8563  Accuracy







#GBM only LIGHT

set.seed(1234)
ModelGBM_LIGHT <- train(Occupancy~.-date-CO2-Temperature-HumidityRatio-Humidity
                       ,method="gbm",data=datatraining,tuneGrid = gbmGrid)
ModelGBM_LIGHT
# 0.98712
plot(ModelGBM_LIGHT)
ModelGBM_LIGHT$finalModel$xNames

ModelGBM_LIGHT$finalModel



set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelGBM_LIGHT,datatraining))
#98.77


set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelGBM_LIGHT,datatesting))
#  0.9786 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelGBM_LIGHT,datatesting2))
# 0.9932  Accuracy





#GBM only CO2

set.seed(1234)
ModelGBM_CO2 <- train(Occupancy~.-date-Light-Temperature-HumidityRatio-Humidity
                        ,method="gbm",data=datatraining,tuneGrid = gbmGrid)
ModelGBM_CO2
# 0.9182
ModelGBM_CO2$finalModel$xNames

ModelGBM_CO2$finalModel



set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelGBM_CO2,datatraining))
#92.15


set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelGBM_CO2,datatesting))
#  0.8713 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelGBM_CO2,datatesting2))
# 0.7462  Accuracy


















#GBM Temperature & Light

set.seed(1234)
ModelGBM_TEMPandLIGHT <- train(Occupancy~.-date-CO2-HumidityRatio-Humidity
                       ,method="gbm",data=datatraining,tuneGrid = gbmGrid)
ModelGBM_TEMPandLIGHT


# 0.9898644
ModelGBM_TEMPandLIGHT$finalModel$xNames




set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelGBM_TEMPandLIGHT,datatraining))
#99.45


set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelGBM_TEMPandLIGHT,datatesting))
#  0.955 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelGBM_TEMPandLIGHT,datatesting2))
# 0.9833  Accuracy


#GBM Humidity & Light

set.seed(1234)
ModelGBM_HUMIandLIGHT <- train(Occupancy~.-date-CO2-HumidityRatio-Temperature
                               ,method="gbm",data=datatraining,tuneGrid = gbmGrid)
ModelGBM_HUMIandLIGHT
# 0.9900900

ModelGBM_HUMIandLIGHT$finalModel$xNames
plot(ModelGBM_HUMIandLIGHT)

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelGBM_HUMIandLIGHT,datatraining))
#99.75


set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelGBM_HUMIandLIGHT,datatesting))
#  0.9452 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelGBM_HUMIandLIGHT,datatesting2))
# 0.9871  Accuracy



#GBM Ligh&CO2

set.seed(1234)
ModelGBM_LIGHTandCO2 <- train(Occupancy~.-date-HumidityRatio-Temperature-Humidity
                               ,method="gbm",data=datatraining,tuneGrid = gbmGrid)
ModelGBM_LIGHTandCO2
# 0.9887853

ModelGBM_LIGHTandCO2$finalModel$xNames


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelGBM_LIGHTandCO2,datatraining))
#99.26

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelGBM_LIGHTandCO2,datatesting))
#  0.937 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelGBM_LIGHTandCO2,datatesting2))
# 0.9834  Accuracy



#GBM Ligh&Humidity Ratio

set.seed(1234)
ModelGBM_LIGHTandHumidityRatio <- train(Occupancy~.-date-Humidity-Temperature-CO2
                              ,method="gbm",data=datatraining,tuneGrid = gbmGrid)
ModelGBM_LIGHTandHumidityRatio 
#0.9883569
plot(ModelGBM_LIGHTandHumidityRatio)
ModelGBM_LIGHTandHumidityRatio$finalModel$xNames


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelGBM_LIGHTandHumidityRatio,datatraining))
#99.52

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelGBM_LIGHTandHumidityRatio,datatesting))
#  0.9610 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelGBM_LIGHTandHumidityRatio,datatesting2))
# 0.9898  Accuracy



## CART model... ALL variables (including Time and WS)
ModelCART_ALL_b <- train(Occupancy~.-date,method="rpart",data=datatraining_b)
ModelCART_ALL_b

# 0.9915
plot(ModelCART_ALL_b)
varImp(ModelCART_ALL_b)
library(rattle)
library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_ALL_b$finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_ALL_b,datatraining_b))
# 99.39

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_ALL_b,datatesting_b))
# 0.9452 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_ALL_b,datatesting2_b))
# 0.9652 


## CART model... (including Time and WS) - NO LIGHT
ModelCART_NO_LIGHT_b <- train(Occupancy~.-date-Light,method="rpart",data=datatraining_b)
ModelCART_NO_LIGHT_b 
# 0.9714
plot(ModelCART_NO_LIGHT_b)
varImp(ModelCART_NO_LIGHT_b)
library(rattle)
library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_NO_LIGHT_b,datatraining_b))
# 96.55

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_NO_LIGHT_b,datatesting_b))
# 0.9471 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_NO_LIGHT_b,datatesting2_b))
# 0.6976 




## CART model... (including Time and WS) - NO CO2
ModelCART_NO_CO2_b <- train(Occupancy~.-date-CO2,method="rpart",data=datatraining_b)
ModelCART_NO_CO2_b 
# 0.9911
plot(ModelCART_NO_CO2_b)
varImp(ModelCART_NO_CO2_b)
library(rattle)
library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_NO_CO2_b,datatraining_b))
# 99.28

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_NO_CO2_b,datatesting_b))
# 0.9625 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_NO_CO2_b,datatesting2_b))
# 0.99 


## CART model... (including Time and WS) - NO CO2 NO light
ModelCART_NO_CO2NOLIGHT_b <- train(Occupancy~.-date-CO2-Light,method="rpart",data=datatraining_b)
ModelCART_NO_CO2NOLIGHT_b 
# 0.9030
plot(ModelCART_NO_CO2NOLIGHT_b)
varImp(ModelCART_NO_CO2NOLIGHT_b)
#library(rattle)
#library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_NO_CO2NOLIGHT_b,datatraining_b))
# 88.97

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_NO_CO2NOLIGHT_b,datatesting_b))
# 0.9081 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_NO_CO2NOLIGHT_b,datatesting2_b))
# 0.8936 



## CART model... NS,WS, T, Light
ModelCART_T_Light_b <- train(Occupancy~.-date-CO2-Humidity-HumidityRatio,method="rpart",data=datatraining_b)
ModelCART_T_Light_b
# 0.9912
plot(ModelCART_T_Light_b)
varImp(ModelCART_T_Light_b)
#library(rattle)
#library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_T_Light_b,datatraining_b))
# 99.28

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_T_Light_b,datatesting_b))
# 0.9625 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_T_Light_b,datatesting2_b))
# 0.99 



## CART model... NS,WS, Humidity, Light
ModelCART_Humidity_Light_b <- train(Occupancy~.-date-CO2-Temperature-HumidityRatio,method="rpart",data=datatraining_b)
ModelCART_Humidity_Light_b
# 0.9899
plot(ModelCART_Humidity_Light_b)
varImp(ModelCART_Humidity_Light_b)
#library(rattle)
#library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_Humidity_Light_b,datatraining_b))
# 98.86

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_Humidity_Light_b,datatesting_b))
# 0.9786 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_Humidity_Light_b,datatesting2_b))
# 0.9931





## CART model... NS,WS, CO2, Light
ModelCART_CO2_Light_b <- train(Occupancy~.-date-Humidity-Temperature-HumidityRatio,method="rpart",data=datatraining_b)
ModelCART_CO2_Light_b
# 0.986
plot(ModelCART_CO2_Light_b)
varImp(ModelCART_CO2_Light_b)
#library(rattle)
#library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_CO2_Light_b,datatraining_b))
# 98.89

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_CO2_Light_b,datatesting_b))
# 0.9782 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_CO2_Light_b,datatesting2_b))
# 0.9931




## CART model... NS,WS, CO2, Temperat
ModelCART_CO2_Temp_b <- train(Occupancy~.-date-Humidity-Light-HumidityRatio,method="rpart",data=datatraining_b)
ModelCART_CO2_Temp_b
# 0.986
plot(ModelCART_CO2_Temp_b)
varImp(ModelCART_CO2_Temp_b)
#library(rattle)
#library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_CO2_Temp_b,datatraining_b))
# 96.55

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_CO2_Temp_b,datatesting_b))
# 0.9471 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_CO2_Temp_b,datatesting2_b))
# 0.6976



## CART model... NS,WS, Light,W
ModelCART_Light_W <- train(Occupancy~.-date-Humidity-CO2-Temperature,method="rpart",data=datatraining_b)
ModelCART_Light_W
# 0.9913
plot(ModelCART_Light_W)
varImp(ModelCART_Light_W)
#library(rattle)
#library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_Light_W,datatraining_b))
# 99.37

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_Light_W,datatesting_b))
# 0.9689 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_Light_W,datatesting2_b))
# 0.9889

## CART model... NS,WS, Temperat,Humidity
ModelCART_Tem_Humidityb <- train(Occupancy~.-date-HumidityRatio-CO2-Light,method="rpart",data=datatraining_b)
ModelCART_Tem_Humidityb
# 0.9030
plot(ModelCART_Tem_Humidityb)
varImp(ModelCART_Tem_Humidityb)
#library(rattle)
#library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_Tem_Humidityb,datatraining_b))
# 88.97

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_Tem_Humidityb,datatesting_b))
# 0.9081 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_Tem_Humidityb,datatesting2_b))
# 0.8936




## CART model... NS,WS, Temperat,
ModelCART_Tem_b <- train(Occupancy~.-date-HumidityRatio-Humidity-CO2-Light,method="rpart",data=datatraining_b)
ModelCART_Tem_b 
# 0.9030
plot(ModelCART_Tem_b)
varImp(ModelCART_Tem_b)
#library(rattle)
#library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_Tem_b,datatraining_b))
# 88.97

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_Tem_b,datatesting_b))
# 0.9081 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_Tem_b,datatesting2_b))
# 0.8936



## CART model... NS,WS, Light
ModelCART_Light_b <- train(Occupancy~.-date-HumidityRatio-Humidity-CO2-Temperature,method="rpart",data=datatraining_b)
ModelCART_Light_b
# 0.9879
plot(ModelCART_Light_b)
varImp(ModelCART_Light_b)
#library(rattle)
#library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_Light_b,datatraining_b))
# 98.93

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_Light_b,datatesting_b))
# 0.9722 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_Light_b,datatesting2_b))
# 0.9926


## CART model... NS,WS, CO2
ModelCART_CO2_b <- train(Occupancy~.-date-HumidityRatio-Humidity-Light-Temperature,method="rpart",data=datatraining_b)
ModelCART_CO2_b
# 0.9696
plot(ModelCART_CO2_b)
varImp(ModelCART_CO2_b)
#library(rattle)
#library(caret)
# Plotting nicely the CART model
#fancyRpartPlot(ModelCART_NO_LIGHT_b $finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_CO2_b,datatraining_b))
# 96.55

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_CO2_b,datatesting_b))
# 0.9471 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_CO2_b,datatesting2_b))
# 0.6976














## CART model... ALL variables (without NS WS)
ModelCART_ALL <- train(Occupancy~.-date,method="rpart",data=datatraining)
ModelCART_ALL
# 0.9915
plot(ModelCART_ALL)
varImp(ModelCART_ALL)
library(rattle)
library(caret)
# Plotting nicely the CART model
fancyRpartPlot(ModelCART_ALL$finalModel)

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_ALL,datatraining))
# 99.30

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_ALL,datatesting))
# 0.9557 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_ALL,datatesting2))
# 0.9647 

# Cart model with no light
ModelCART_noLight <- train(Occupancy~.-date-Light,method="rpart",data=datatraining)
ModelCART_noLight
# 0.9316 
varImp(ModelCART_noLight)
fancyRpartPlot(ModelCART_noLight$finalModel)
set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_noLight,datatraining))
# 0.9305 Accuracy
set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_noLight,datatesting))
# 0.8465 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_noLight,datatesting2))
# 0.7896  


# Cart model with no CO2
ModelCART_noCO2 <- train(Occupancy~.-date-CO2,method="rpart",data=datatraining)
ModelCART_noCO2
# 0.9912
varImp(ModelCART_noCO2)
fancyRpartPlot(ModelCART_noCO2$finalModel)

confusionMatrix(datatraining$Occupancy,predict(ModelCART_noCO2,datatraining))
# 0.9923 Accuracy


confusionMatrix(datatesting$Occupancy,predict(ModelCART_noCO2,datatesting))
# 0.937 Accuracy

confusionMatrix(datatesting2$Occupancy,predict(ModelCART_noCO2,datatesting2))
# 0.9629 Accuracy  

# Cart model with no CO2 no Light
ModelCART_noCO2noLight <- train(Occupancy~.-date-CO2-Light,method="rpart",data=datatraining)
ModelCART_noCO2noLight
# 0.89709 Accuracy
confusionMatrix(datatraining$Occupancy,predict(ModelCART_noCO2noLight,datatraining))
# 88.28

confusionMatrix(datatesting$Occupancy,predict(ModelCART_noCO2noLight,datatesting))
# 0.8402 Accuracy

confusionMatrix(datatesting2$Occupancy,predict(ModelCART_noCO2noLight,datatesting2))
# 0.863 Accuracy  
varImp(ModelCART_noCO2noLight)
fancyRpartPlot(ModelCART_noCO2noLight$finalModel)


ModelCART_noCO2noLight_b <- train(Occupancy~.-date-CO2-Light,method="rpart",data=datatraining_b)
ModelCART_noCO2noLight_b
# 0.9047223
varImp(ModelCART_noCO2noLight_b)

#fancyRpartPlot(ModelCART_noCO2noLight$finalModel)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_noCO2noLight_b,datatraining_b))
#0.8897
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_noCO2noLight_b,datatesting_b))
#0.9081
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_noCO2noLight_b,datatesting2_b))
# 0.8936


# Cart model with no CO2 no Light no HumidityRatio
set.seed(1234)
ModelCART_noCO2noLightnoHumidityRatio <- train(Occupancy~.-date-CO2-Light-HumidityRatio,
                                               method="rpart",data=datatraining)
ModelCART_noCO2noLightnoHumidityRatio 
# 0.8912 Accuracy
ModelCART_noCO2noLightnoHumidityRatio$finalModel

varImp(ModelCART_noCO2noLightnoHumidityRatio)
fancyRpartPlot(ModelCART_noCO2noLightnoHumidityRatio$finalModel)

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_noCO2noLightnoHumidityRatio
                                              ,datatraining))
# 88.24

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_noCO2noLightnoHumidityRatio
                                              ,datatesting))
# 0.8402 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_noCO2noLightnoHumidityRatio,
                                               datatesting2))
# 0.863 Accuracy  



# Cart model with CO2 and Temperatuer
set.seed(1234)
ModelCART_CO2_TEMP <- train(Occupancy~.-date-Humidity-Light-HumidityRatio,
                                               method="rpart",data=datatraining)
ModelCART_CO2_TEMP
# 0.9294 Accuracy

ModelCART_CO2_TEMP$finalModel

varImp(ModelCART_CO2_TEMP)
fancyRpartPlot(ModelCART_CO2_TEMP$finalModel)

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_CO2_TEMP
                                               ,datatraining))
# 93.05

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_CO2_TEMP
                                              ,datatesting))
# 0.8465 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_CO2_TEMP,
                                               datatesting2))
# 0.7896 Accuracy  






# Cart model with no CO2 no Light no HumidityRatio B DATA
set.seed(1234)
ModelCART_noCO2noLightnoHumidityRatio_b <- train(Occupancy~.-date-CO2-Light-HumidityRatio,
                                               method="rpart",data=datatraining_b)
ModelCART_noCO2noLightnoHumidityRatio_b 
# 0.903019 Accuracy
ModelCART_noCO2noLightnoHumidityRatio_b$finalModel

varImp(ModelCART_noCO2noLightnoHumidityRatio_b)
fancyRpartPlot(ModelCART_noCO2noLightnoHumidityRatio_b$finalModel)

set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_noCO2noLightnoHumidityRatio_b
                                               ,datatraining_b))
# 88.97

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_noCO2noLightnoHumidityRatio_b
                                              ,datatesting_b))
# 0.9081 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_noCO2noLightnoHumidityRatio_b,
                                               datatesting2_b))
# 0.8936 Accuracy  









# CART model Only temperature

set.seed(1234)
ModelCART_Temp <- train(Occupancy~.-date-CO2-Light-HumidityRatio-Humidity,
                                               method="rpart",data=datatraining)
ModelCART_Temp 
# 0.8588 Accuracy

ModelCART_Temp$finalModel
varImp(ModelCART_Temp)
fancyRpartPlot(ModelCART_Temp$finalModel)


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_Temp
                                               ,datatraining))
# 85.88

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_Temp
                                              ,datatesting))
# 0.6653 Accuracy

set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_Temp,
                                               datatesting2))
# 86.51




# CART model Only LIGHT

set.seed(1234)
ModelCART_LIGHT <- train(Occupancy~.-date-CO2-Temperature-HumidityRatio-Humidity,
                        method="rpart",data=datatraining)
ModelCART_LIGHT
# 0.985584 Accuracy

ModelCART_LIGHT$finalModel
#varImp(ModelCART_Temp)
fancyRpartPlot(ModelCART_LIGHT$finalModel)


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_LIGHT
                                               ,datatraining))
# 0.9878

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_LIGHT
                                              ,datatesting))
# 0.9786 Accuracy

set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_LIGHT,
                                               datatesting2))
# 0.9931










# CART model Only CO2

set.seed(1234)
ModelCART_CO2 <- train(Occupancy~.-date-Light-Temperature-HumidityRatio-Humidity,
                         method="rpart",data=datatraining)
ModelCART_CO2
# 0.91856 Accuracy

ModelCART_CO2$finalModel
#varImp(ModelCART_Temp)
fancyRpartPlot(ModelCART_CO2$finalModel)


set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_CO2
                                               ,datatraining))
# 0.9218

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_CO2
                                              ,datatesting))
# 0.8713 Accuracy

set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_CO2,
                                               datatesting2))
# 0.7462










# CART model Temperature and Time seconds and Weekend _status

set.seed(1234)
ModelCART_Temp_b <- train(Occupancy~.-date-CO2-Light-HumidityRatio-Humidity,
                          method="rpart",data=datatraining_b)

ModelCART_Temp_b
# 0.90301
varImp(ModelCART_Temp_b)
fancyRpartPlot(ModelCART_Temp_b$finalModel)
set.seed(1234)

confusionMatrix(datatraining_b$Occupancy,predict(ModelCART_Temp_b
                                               ,datatraining_b))
# 88.97

set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelCART_Temp_b
                                              ,datatesting_b))
# 0.9081 Accuracy

set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelCART_Temp_b,
                                               datatesting2_b))
# 89.36







# CART model Temperature and Light

set.seed(1234)
ModelCART_TempandLight <- train(Occupancy~.-date-CO2-HumidityRatio-Humidity,
                        method="rpart",data=datatraining)
ModelCART_TempandLight 
# 0.8912 Accuracy
ModelCART_TempandLight$finalModel$xNames

#varImp(ModelCART_Temp)
fancyRpartPlot(ModelCART_TempandLight$finalModel)

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_TempandLight
                                               ,datatraining))
# 99.08

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_TempandLight
                                              ,datatesting))
# 0.851 Accuracy

set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_TempandLight,
                                               datatesting2))
# 95.43





# CART model Humidity and Light

set.seed(1234)
ModelCART_HumidityLight <- train(Occupancy~.-date-CO2-HumidityRatio-Temperature,
                                method="rpart",data=datatraining)
ModelCART_HumidityLight
# 0.8912 Accuracy
ModelCART_HumidityLight$finalModel$xNames

#varImp(ModelCART_Temp)
fancyRpartPlot(ModelCART_HumidityLight$finalModel)

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_HumidityLight
                                               ,datatraining))
# 98.86

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_HumidityLight
                                              ,datatesting))
# 0.9786 Accuracy

set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_HumidityLight,
                                               datatesting2))
# 99.31



# CART model Light and CO2

set.seed(1234)
ModelCART_LightandCO2 <- train(Occupancy~.-date-HumidityRatio-Temperature-Humidity,
                                 method="rpart",data=datatraining)
ModelCART_LightandCO2
# 0.8912 Accuracy
ModelCART_LightandCO2$finalModel$xNames

#varImp(ModelCART_Temp)
fancyRpartPlot(ModelCART_LightandCO2$finalModel)

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_LightandCO2
                                               ,datatraining))
# 98.89

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_LightandCO2
                                              ,datatesting))
# 0.9782 Accuracy

set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_LightandCO2,
                                               datatesting2))
# 99.31




# CART model Light and Humidity Ratio

set.seed(1234)
ModelCART_LightandHumidityRatio <- train(Occupancy~.-date-Humidity-Temperature-CO2,
                               method="rpart",data=datatraining)
ModelCART_LightandHumidityRatio
# 0.8912 Accuracy
ModelCART_LightandHumidityRatio$finalModel$xNames

#varImp(ModelCART_Temp)
fancyRpartPlot(ModelCART_LightandHumidityRatio$finalModel)

set.seed(1234)
confusionMatrix(datatraining$Occupancy,predict(ModelCART_LightandHumidityRatio
                                               ,datatraining))
# 98.94

set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelCART_LightandHumidityRatio
                                              ,datatesting))
# 0.9666 Accuracy

set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelCART_LightandHumidityRatio,
                                               datatesting2))
# 99.31





## lda models 
ModelLDA_ALL_b <- train(Occupancy~.-date,method="lda",data=datatraining_b)
ModelLDA_ALL_b$finalModel$xNames
ModelLDA_ALL_b$finalModel 
ModelLDA_ALL_b
#0.9878 
confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_ALL_b,datatraining_b))
#0.9885


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_ALL_b,datatesting_b))
# 0.979 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_ALL_b,datatesting2_b))
# 0.9933 Accuracy  


### no light , with WS, NS
ModelLDA_noLIGHT_b <- train(Occupancy~.-date-Light,method="lda",data=datatraining_b)
ModelLDA_noLIGHT_b$finalModel$xNames
ModelLDA_noLIGHT_b

#0.9304
confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_noLIGHT_b,datatraining_b))
#0.9312


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_noLIGHT_b,datatesting_b))
# 0.8488 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_noLIGHT_b,datatesting2_b))
# 0.7232 Accuracy  

### no CO2 , with WS, NS
ModelLDA_noCO2_b <- train(Occupancy~.-date-CO2,method="lda",data=datatraining_b)
ModelLDA_noCO2_b$finalModel$xNames
ModelLDA_noCO2_b
# 0.99

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_noCO2_b,datatraining_b))
#0.9877


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_noCO2_b,datatesting_b))
# 0.979 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_noCO2_b,datatesting2_b))
# 0.9896 Accuracy  


### no CO2 no Light, with WS, NS
ModelLDA_noCO2NoLight_b <- train(Occupancy~.-date-CO2-Light,method="lda",data=datatraining_b)
ModelLDA_noCO2NoLight_b$finalModel$xNames
ModelLDA_noCO2NoLight_b
# 0.8571

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_noCO2NoLight_b,datatraining_b))
#0.8578


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_noCO2NoLight_b,datatesting_b))
# 0.8593 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_noCO2NoLight_b,datatesting2_b))
# 0.867 Accuracy  




### Light, T, with WS, NS
ModelLDA_T_Light_b <- train(Occupancy~.-date-CO2-Humidity-HumidityRatio,method="lda",data=datatraining_b)
ModelLDA_T_Light_b$finalModel$xNames
ModelLDA_T_Light_b
# 0.9871

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_T_Light_b,datatraining_b))
#0.9875


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_T_Light_b,datatesting_b))
# 0.9790 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_T_Light_b,datatesting2_b))
# 0.9931 Accuracy  


### Light, Humidity, with WS, NS
ModelLDA_Humidity_Light_b <- train(Occupancy~.-date-CO2-Temperature-HumidityRatio,method="lda",data=datatraining_b)
ModelLDA_Humidity_Light_b$finalModel$xNames
ModelLDA_Humidity_Light_b
# 0.9931

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_Humidity_Light_b,datatraining_b))
#0.9875


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_Humidity_Light_b,datatesting_b))
# 0.9786 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_Humidity_Light_b,datatesting2_b))
# 0.9786 Accuracy  


### Light, CO2, with WS, NS
ModelLDA_CO2_Light_b <- train(Occupancy~.-date-Humidity-Temperature-HumidityRatio,method="lda",data=datatraining_b)
ModelLDA_CO2_Light_b$finalModel$xNames
ModelLDA_CO2_Light_b 
# 0.9824

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_CO2_Light_b,datatraining_b))
#0.9833


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_CO2_Light_b,datatesting_b))
# 0.9786 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_CO2_Light_b,datatesting2_b))
# 0.98058 Accuracy  



### Temperature CO2, with WS, NS
ModelLDA_CO2_Tem_b <- train(Occupancy~.-date-Humidity-Light-HumidityRatio,method="lda",data=datatraining_b)
ModelLDA_CO2_Tem_b$finalModel$xNames
ModelLDA_CO2_Tem_b
# 0.9101

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_CO2_Tem_b,datatraining_b))
#0.9101


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_CO2_Tem_b,datatesting_b))
# 0.8837 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_CO2_Tem_b,datatesting2_b))
# 0.8042 Accuracy  




### Light, W, with WS, NS
ModelLDA_Light_W_b <- train(Occupancy~.-date-Humidity-Temperature-CO2,method="lda",data=datatraining_b)
ModelLDA_Light_W_b$finalModel$xNames
ModelLDA_Light_W_b
# 0.9774

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_Light_W_b,datatraining_b))
#0.9779


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_Light_W_b,datatesting_b))
# 0.9786 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_Light_W_b,datatesting2_b))
# 0.9755 Accuracy  




### Temp,Humidity, with WS, NS
ModelLDA_Temp_Humidity_b <- train(Occupancy~.-date-HumidityRatio-Light-CO2,method="lda",data=datatraining_b)
ModelLDA_Temp_Humidity_b$finalModel$xNames
ModelLDA_Temp_Humidity_b
# 0.853

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_Temp_Humidity_b,datatraining_b))
#0.8545


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_Temp_Humidity_b,datatesting_b))
# 0.8593 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_Temp_Humidity_b,datatesting2_b))
# 0.885 Accuracy  





### Temp, with WS, NS
ModelLDA_Temp_b <- train(Occupancy~.-date-Humidity-HumidityRatio-Light-CO2,method="lda",data=datatraining_b)
ModelLDA_Temp_b$finalModel$xNames
ModelLDA_Temp_b
# 0.85994

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_Temp_b,datatraining_b))
#0.8609


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_Temp_b,datatesting_b))
# 0.8623 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_Temp_b,datatesting2_b))
# 0.8718 Accuracy  




### Light with WS, NS
ModelLDA_Light_b <- train(Occupancy~.-date-Humidity-HumidityRatio-Temperature-CO2,method="lda",data=datatraining_b)
ModelLDA_Light_b$finalModel$xNames
ModelLDA_Light_b
# 0.9744

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_Light_b,datatraining_b))
#0.9753


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_Light_b,datatesting_b))
# 0.9786 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_Light_b,datatesting2_b))
# 0.9784 Accuracy  




### CO2 with WS, NS
ModelLDA_CO2_b <- train(Occupancy~.-date-Humidity-HumidityRatio-Temperature-Light,method="lda",data=datatraining_b)
ModelLDA_CO2_b$finalModel$xNames
ModelLDA_CO2_b
# 0.890695

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_CO2_b,datatraining_b))
#0.889


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_CO2_b,datatesting_b))
# 0.9786 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_CO2_b,datatesting2_b))
# 0.7836 Accuracy  















### without NS and WS
ModelLDA_ALL <- train(Occupancy~.-date,method="lda",data=datatraining)
ModelLDA_ALL$finalModel$xNames
ModelLDA_ALL 

#0.987988 
confusionMatrix(datatraining$Occupancy,predict(ModelLDA_ALL,datatraining))
#0.9878


set.seed(1234)
confusionMatrix(datatesting$Occupancy,predict(ModelLDA_ALL,datatesting))
# 0.979 Accuracy
set.seed(1234)
confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_ALL,datatesting2))
# 0.9876 Accuracy  

ModelLDA_noLight <- train(Occupancy~.-date-Light,method="lda",data=datatraining)
ModelLDA_noLight
#0.9191
ModelLDA_noLight$finalModel$xNames


ModelLDA_noLight$finalModel

confusionMatrix(datatraining$Occupancy,predict(ModelLDA_noLight,datatraining))
#91.91
confusionMatrix(datatesting$Occupancy,predict(ModelLDA_noLight,datatesting))
#0.8533
confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_noLight,datatesting2))
# 0.7377

ModelLDA_noCO2 <- train(Occupancy~.-date-CO2,method="lda",data=datatraining)
ModelLDA_noCO2
#0.9854
ModelLDA_noCO2$finalModel$xNames


confusionMatrix(datatraining$Occupancy,predict(ModelLDA_noCO2,datatraining))
#98.55

confusionMatrix(datatesting$Occupancy,predict(ModelLDA_noCO2,datatesting))
#0.979
confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_noCO2,datatesting2))
# 0.9824


ModelLDA_noCO2noLight <- train(Occupancy~.-date-CO2-Light,method="lda",data=datatraining)
ModelLDA_noCO2noLight 
# 85.502 %
ModelLDA_noCO2noLight$finalModel$xNames
varImp(ModelLDA_noCO2noLight)
#0.852433

confusionMatrix(datatraining$Occupancy,predict(ModelLDA_noCO2noLight,datatraining))
# 0.8546
confusionMatrix(datatesting$Occupancy,predict(ModelLDA_noCO2noLight,datatesting))
#0.8544
confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_noCO2noLight,datatesting2))
# 0.8536


ModelLDA_noCO2noLightnoHumidityRatio <- train(Occupancy~.-date-CO2-Light-HumidityRatio,method="lda",data=datatraining)
ModelLDA_noCO2noLightnoHumidityRatio$finalModel$xNames
ModelLDA_noCO2noLightnoHumidityRatio
# 0.83511 
confusionMatrix(datatraining$Occupancy,predict(ModelLDA_noCO2noLightnoHumidityRatio,datatraining))
# 0.8367
confusionMatrix(datatesting$Occupancy,predict(ModelLDA_noCO2noLightnoHumidityRatio,datatesting))
#0.8567
confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_noCO2noLightnoHumidityRatio,datatesting2))
# 0.847


ModelLDA_noCO2noLightnoHumidity <- train(Occupancy~.-date-CO2-Light-Humidity,method="lda",data=datatraining)
ModelLDA_noCO2noLightnoHumidity
ModelLDA_noCO2noLightnoHumidity$finalModel$xNames
# 0.837 
confusionMatrix(datatesting$Occupancy,predict(ModelLDA_noCO2noLightnoHumidity,datatesting))
#0.8552
confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_noCO2noLightnoHumidity,datatesting2))
# 0.8517


### LDA CO2 and Temperature

ModelLDA_CO2_TEMP <- train(Occupancy~.-date-HumidityRatio-Light-Humidity,method="lda",data=datatraining)
ModelLDA_CO2_TEMP
ModelLDA_CO2_TEMP$finalModel$xNames
# 0.9017

confusionMatrix(datatraining$Occupancy,predict(ModelLDA_CO2_TEMP,datatraining))
# 0.9026

confusionMatrix(datatesting$Occupancy,predict(ModelLDA_CO2_TEMP,datatesting))
#0.8762
confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_CO2_TEMP,datatesting2))
# 0.8040











# LDA temperature


ModelLDA_TEMP <- train(Occupancy~.-date-CO2-Light-Humidity-HumidityRatio
                       ,method="lda",data=datatraining)

ModelLDA_TEMP

confusionMatrix(datatraining$Occupancy,predict(ModelLDA_TEMP,datatraining))

# 0.83421
ModelLDA_TEMP$finalModel$xNames
confusionMatrix(datatesting$Occupancy,predict(ModelLDA_TEMP,datatesting))
#0.8533
confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_TEMP,datatesting2))
# 0.8364





#LDA only Light
# CART model Only LIGHT

ModelLDA_LIGHT <- train(Occupancy~.-date-CO2-Temperature-HumidityRatio-Humidity,
                         method="lda",data=datatraining)
ModelLDA_LIGHT
# 0.985584 Accuracy

ModelLDA_LIGHT$finalModel


confusionMatrix(datatraining$Occupancy,predict(ModelLDA_LIGHT
                                               ,datatraining))
# 0.9638

confusionMatrix(datatesting$Occupancy,predict(ModelLDA_LIGHT
                                              ,datatesting))
# 0.9786 Accuracy


confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_LIGHT,
                                               datatesting2))
# 0.977 Accuracy





#LDA only Co2


ModelLDA_CO2 <- train(Occupancy~.-date-Light-Temperature-HumidityRatio-Humidity,
                        method="lda",data=datatraining)
ModelLDA_CO2
# 0.8838 Accuracy

ModelLDA_CO2$finalModel


confusionMatrix(datatraining$Occupancy,predict(ModelLDA_CO2
                                               ,datatraining))
# 0.8838

confusionMatrix(datatesting$Occupancy,predict(ModelLDA_CO2
                                              ,datatesting))
# 0.8619 Accuracy


confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_CO2,
                                               datatesting2))
# 0.7993 Accuracy










# LDA  Temperature and light

ModelLDA_TempandLight <- train(Occupancy~.-date-Humidity-HumidityRatio
                               -CO2,method="lda",data=datatraining)

ModelLDA_TempandLight$finalModel$xNames

confusionMatrix(datatraining$Occupancy,predict(ModelLDA_TempandLight,datatraining))
#0.9656

ModelLDA_TEMP$finalModel$xNames
confusionMatrix(datatesting$Occupancy,predict(ModelLDA_TempandLight,datatesting))
#0.979

confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_TempandLight,datatesting2))
# 0.9862

# LDA  Humidity and light

ModelLDA_HumidityandLight <- train(Occupancy~.-date-HumidityRatio-Temperature
                               -CO2,method="lda",data=datatraining)

ModelLDA_HumidityandLight$finalModel$xNames


confusionMatrix(datatraining$Occupancy,predict(ModelLDA_HumidityandLight,datatraining))
#0.9678

confusionMatrix(datatesting$Occupancy,predict(ModelLDA_HumidityandLight,datatesting))
#0.9786

confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_HumidityandLight,datatesting2))
# 0.9779


# LDA  Light and Co2

ModelLDA_LightandCO2 <- train(Occupancy~.-date-HumidityRatio-Temperature-Humidity
                                   ,method="lda",data=datatraining)

ModelLDA_LightandCO2$finalModel$xNames


confusionMatrix(datatraining$Occupancy,predict(ModelLDA_LightandCO2,datatraining))
#0.9753

ModelLDA_TEMP$finalModel$xNames
confusionMatrix(datatesting$Occupancy,predict(ModelLDA_LightandCO2,datatesting))
#0.9786

confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_LightandCO2,datatesting2))
# 0.9786



# LDA  Light and HR

ModelLDA_LightandHR <- train(Occupancy~.-date-CO2-Temperature-Humidity
                              ,method="lda",data=datatraining)

ModelLDA_LightandHR$finalModel$xNames


confusionMatrix(datatraining$Occupancy,predict(ModelLDA_LightandHR,datatraining))
#0.9681

ModelLDA_TEMP$finalModel$xNames
confusionMatrix(datatesting$Occupancy,predict(ModelLDA_LightandHR,datatesting))
#0.9786

confusionMatrix(datatesting2$Occupancy,predict(ModelLDA_LightandHR,datatesting2))
# 0.9739

