#15'1103 piolet test on NN

#####
#http://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
#####

remove(list=ls())

#use neuro network/logistic regression/SVM
library("foreign")
setwd("D:/HU_Stat/Business Analytics Data Science/Task")
data1<-read.table("BADS_SWT_Trainingset.csv", sep = ',',header=T)

cate_name<-c(
  "ACTVSUBS"  ,  	
  "ADULTS"  ,		
  "AGE1"  ,		
  "AGE2"  ,		
  "AREA"  ,		
  "ASL_FLAG"  ,		
  "CAR_BUY"   ,		
  "CARTYPE"   ,		
  "CHILDREN"  ,			
  "CRCLSCOD"  ,		
  "CREDITCD"  ,		
  "CRTCOUNT"  ,		
  "CSA"    ,		
  "CUSTOMER_ID"   ,		
  "DIV_TYPE"  ,		
  "DUALBAND"  ,		
  "DWLLSIZE"  ,		
  "DWLLTYPE"  ,		
  "EDUC1"   ,		
  "ETHNIC"  ,		
  "FORGNTVL"  ,		
  "HND_PRICE"   ,		
  "HHSTATIN"  ,		
  "HND_WEBCAP"  ,		
  "INCOME	"  ,		
  "INFOBASE"  ,		
  "KID0_2"  ,		
  "KID3_5"  ,		
  "KID6_10"   ,		
  "KID11_15"  ,		
  "KID16_17"  ,		
  "LAST_SWAP"   ,		
  "LOR"   ,		
  "MAILFLAG"  ,		
  "MAILORDR"  ,		
  "MAILRESP"  ,		
  "MARITAL"   ,		
  "MODELS"  ,		
  "MTRCYCLE"  ,		
  "NEW_CELL"   ,		
  "NUMBCARS"  ,		
  "OCCU1"   ,		
  "OWNRENT"   ,		
  "PCOWNER"   ,		
  "PHONES"  ,		
  "PRE_HND_PRICE"   ,		
  "PRIZM_SOCIAL_ONE"  ,		
  "PROPTYPE"  ,		
  "REF_QTY"   ,		
  "REFURB_NEW"  ,		
  "RV"  ,		
  "SOLFLAG"   ,		
  "TOT_ACPT"  ,		
  "TOT_RET"   ,		
  "TRUCK"   ,		
  "UNIQSUBS"  ,		
  "WRKWOMAN"  
)
cate_name_l<-c(tolower(cate_name),"HHstatin","REF_QTY","Customer_ID")

var.cat<-names(data1)[
  !names(data1) %in% cate_name_l  ]
nume<-data1[names(data1) %in% var.cat]
cate<-data1[!names(data1) %in% var.cat]

cate<-cbind(data1$churn,cate)
#nume<-cbind(data1$churn,nume)
colnames(cate)[1]<-"churn"

#not working cate<-cbind(data1$churn,data1[,115:173]) #all the category
#not working nume<-cbind(data1$churn,data1[,1:114]) #all numeric


#####NN
#####

#check for missing
mis<-apply(nume,2,function(x) sum(is.na(x)));mis
##quiet many...
NA_name_nume<-names(mis[mis != 0]) #call out all the numerical variable with NA
#names(data[mis==0])

#  for(i in SWT[, c(1:94, 96:119)]){
#      SWT[is.na(SWT[,i])] <- mean(SWT[,i], na.rm = TRUE)
#  }


#replace missing with mean
#nume_mean<-apply(nume,2,mean,na.rm=T)
#nume_mean

 "rev_Mean"     "mou_Mean"     "totmrc_Mean"  "da_Mean"      "ovrmou_Mean"  "ovrrev_Mean" 
 "vceovr_Mean"  "datovr_Mean"  "roam_Mean"    "rev_Range"    "mou_Range"    "totmrc_Range"
 "da_Range"     "ovrmou_Range" "ovrrev_Range" "vceovr_Range" "datovr_Range" "roam_Range"  
 "change_mou"   "change_rev"   "crtcount"     "rmcalls"      "rmmou"        "rmrev"       
 "avg6mou"      "avg6qty"      "avg6rev"      "income"       "retdays"      "eqpdays"    

## imputing all na with mean ##
nume$ rev_Mean[is.na(nume$rev_Mean)] <- mean(nume$rev_Mean,na.rm=T)
nume$ mou_Mean[is.na(nume$	mou_Mean	)] <- mean(nume$	mou_Mean	,na.rm=T)
nume$	totmrc_Mean	[is.na(nume$	totmrc_Mean	)] <- mean(nume$	totmrc_Mean	,na.rm=T)
nume$	da_Mean	[is.na(nume$	da_Mean	)] <- mean(nume$	da_Mean	,na.rm=T)
nume$	ovrmou_Mean	[is.na(nume$	ovrmou_Mean	)] <- mean(nume$	ovrmou_Mean	,na.rm=T)
nume$	ovrrev_Mean	[is.na(nume$	ovrrev_Mean	)] <- mean(nume$	ovrrev_Mean	,na.rm=T)
nume$	vceovr_Mean	[is.na(nume$	vceovr_Mean	)] <- mean(nume$	vceovr_Mean	,na.rm=T)
nume$	datovr_Mean	[is.na(nume$	datovr_Mean	)] <- mean(nume$	datovr_Mean	,na.rm=T)
nume$	roam_Mean	[is.na(nume$	roam_Mean	)] <- mean(nume$	roam_Mean	,na.rm=T)
nume$	rev_Range	[is.na(nume$	rev_Range	)] <- mean(nume$	rev_Range	,na.rm=T)
nume$	mou_Range	[is.na(nume$	mou_Range	)] <- mean(nume$	mou_Range	,na.rm=T)
nume$	totmrc_Range	[is.na(nume$	totmrc_Range	)] <- mean(nume$	totmrc_Range	,na.rm=T)
nume$	da_Range	[is.na(nume$	da_Range	)] <- mean(nume$	da_Range	,na.rm=T)
nume$	ovrmou_Range	[is.na(nume$	ovrmou_Range	)] <- mean(nume$	ovrmou_Range	,na.rm=T)
nume$	ovrrev_Range	[is.na(nume$	ovrrev_Range	)] <- mean(nume$	ovrrev_Range	,na.rm=T)
nume$	vceovr_Range	[is.na(nume$	vceovr_Range	)] <- mean(nume$	vceovr_Range	,na.rm=T)
nume$	datovr_Range	[is.na(nume$	datovr_Range	)] <- mean(nume$	datovr_Range	,na.rm=T)
nume$	roam_Range	[is.na(nume$	roam_Range	)] <- mean(nume$	roam_Range	,na.rm=T)
nume$	change_mou	[is.na(nume$	change_mou	)] <- mean(nume$	change_mou	,na.rm=T)
nume$	change_rev	[is.na(nume$	change_rev	)] <- mean(nume$	change_rev	,na.rm=T)
#nume$	crtcount	[is.na(nume$	crtcount	)] <- mean(nume$	crtcount	,na.rm=T)
nume$	rmcalls	[is.na(nume$	rmcalls	)] <- mean(nume$	rmcalls	,na.rm=T)
nume$	rmmou	[is.na(nume$	rmmou	)] <- mean(nume$	rmmou	,na.rm=T)
nume$	rmrev	[is.na(nume$	rmrev	)] <- mean(nume$	rmrev	,na.rm=T)
nume$	avg6mou	[is.na(nume$	avg6mou	)] <- mean(nume$	avg6mou	,na.rm=T)
nume$	avg6qty	[is.na(nume$	avg6qty	)] <- mean(nume$	avg6qty	,na.rm=T)
nume$	avg6rev	[is.na(nume$	avg6rev	)] <- mean(nume$	avg6rev	,na.rm=T)
nume$	income	[is.na(nume$	income	)] <- mean(nume$	income	,na.rm=T)
nume$	retdays	[is.na(nume$	retdays	)] <- mean(nume$	retdays	,na.rm=T)
nume$	eqpdays	[is.na(nume$	eqpdays	)] <- mean(nume$	eqpdays	,na.rm=T)
##

data2<-nume

#remove the 4 singularity variable
#comp_dat_Mean drop_blk_Mean attempt_Mean complete_Mean 
#NA because singularity, just omit it

var.out<-names(data2)[!names(data) %in% c("comp_dat_Mean","drop_blk_Mean", "attempt_Mean", "complete_Mean" )]
data<-data2[names(data2) %in% var.out]
names(data)

#mark the missing attrib dont use, data1 no NA
#data<-data1[mis==0] #cant get...
#data<-na.omit(data1, by=col) #example delete don't work

#find sample and divide to train test set, fit a glm first to see MSE
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(churn~., data=train, family=binomial(link="logit")) #take so long...124-ID
summary(lm.fit)

pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$churn)^2)/nrow(test)

#normalize b4 NN
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, #center = mins, 
                              scale = maxs - mins))
for (i in 1:120){
is.numeric(data[,i])
}

train_ <- scaled[index,]
test_ <- scaled[-index,]

#find the appor node for NN, usual 2/3 input variable (80 now)
install.packages("neuralnet")
library("neuralnet")
n <- names(train_)
f <- as.formula(paste("churn ~", paste(n[!n %in% "churn"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden= 2 #c(3,2)
                  ,linear.output=T)

#can plot
plot(nn)

#prediction on churn
pr.nn <- compute(nn,test_[,2:13])

pr.nn_ <- pr.nn$net.result*(max(data$churn)-min(data$churn))+min(data$churn)
test.r <- (test_$churn)*(max(data$churn)-min(data$churn))+min(data$churn)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

#we then compare the two MSEs
print(paste(MSE.lm,MSE.nn))

#para plot for two
par(mfrow=c(1,2))

plot(test$churn,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$churn,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

#same plot for two
plot(test$churn,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$churn,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

#cross validate
library(boot)
set.seed(200)
lm.fit <- glm(medv~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]

set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

mean(cv.error)
cv.error
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)


#####logistic
#####

#####
#http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
#####

#checking missing
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))
sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")

#select out variable wanted
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))

#replace missing with mean
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

#start to make model, divide train or test set
train <- data[1:800,]
test <- data[801:889,]
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

#look table of deviance
anova(model, test="Chisq")

#r square
library(pscl)
pR2(model)

#model prediction
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

#plot ROC curve and AUC value
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc