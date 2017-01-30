install.packages("gmodels")

library(xlsx)
library(xlsxjars)
library(rJava)
library("nnet")
library("caret")
library("randomForest")
library("rpart")
library("car")
library(plyr)
library(gmodels)
library("e1071")
library("Amelia")
library("mice")
library("VIM")
library("devtools")
library("rgl")
library("homals")
library("ForImp")
library("x12")
library("MASS")
library("mi")
library("foreign")
library("pROC")
library("neuralnet")
library("foreign")


#setwd("~/Desktop/Data Science")#setwd("C:/Users/AGkelameri/Dropbox/BADS/Codes")
#SWT<-read.csv("/Users/marieschild/Desktop/Data\ Science/SWT/BADS\ -\ SWT\ -\ Trainingset\ -\ 2015-10-27/BADS\ -\ SWT\ -\ Trainingset\ -\ 2015-10-27.csv")
#Testset<-read.csv(" /Users/marieschild/Desktop/Data\ Science/SWT/BADS\ -\ SWT\ -\ Trainingset\ -\ 2015-10-27/BADS\ -\ SWT\ -\ Testset\ -\ 2015-10-27.csv")
#Testset <- read.csv("C:/Users/AGkelameri/Dropbox/BADS/Codes/BADS - SWT - Testset - 2015-10-27.csv")
#setwd("/Users/Caro/Documents/uni/Statistik/BADS/Special Working Task/BADS - SWT - Trainingset - 2015-10-27")
#SWT<-read.csv("BADS - SWT - Trainingset - 2015-10-27.csv")
#Testset<-read.csv("/Users/Caro/Documents/uni/Statistik/BADS/Special Working Task/BADS - SWT - Testset - 2015-10-27.csv")
setwd("D:/HU_Stat/Business Analytics Data Science/Task")
SWT<-read.table("BADS_SWT_Trainingset.csv", sep = ',',header=T)
testset<-read.csv("BADS_SWT_Testset.csv", header=T, sep=",")

#categorical
categorical = SWT[, c(95, 120:173)]
categorical[] <- lapply(categorical, as.factor)
#recode all spaces to NA
SWT[SWT == ""] <- NA
#variables=data.frame(variables) #just saving for SPSS

########################################################
#Variablesselektion: 

#Caro: 
data<-read.csv("/Users/marieschild/Desktop/Data\ Science/SWT/BADS\ -\ SWT\ -\ Trainingset\ -\ 2015-10-27/BADS\ -\ SWT\ -\ Trainingset\ -\ 2015-10-27.csv")
attributes(data[, 35:69])
summary(data[ , 35:69], na=T)
#--> There are no NA's

grep("churn", colnames(data))
#Which variable number has churn? --> 173--> create new subset with variables and churn
cdata<-subset(data[,c(35:69, 173)])
cdata[cdata == ""] <- NA
summary(cdata)
tab<-cor(cdata, method="pearson")
#write.xlsx(tab, "/Users/Caro/Documents/uni/Statistik/BADS/Special Working Task/correlations.xlsx") 
#find out correlations between independent variables; attempt_Mean has high correlations with 
#almost all the other Mean-Variables; delete all variables which are correlated with attempt_Mean>0.6


#put all the rest of variables and do stepwise selection, i.e. forward selection and backward
#elimination; use step-command in R: uses BIC to compare models
?step
fit<-step(glm(churn~attempt_Mean+ cc_mou_Mean+drop_dat_Range+drop_vce_Range+blck_dat_Range+
           blck_vce_Range+callfwdv_Mean+recv_sms_Range+recv_vce_Range+threeway_Mean+unan_dat_Range+unan_vce_Range+mou_Mean+totmrc_Mean+ovrrev_Mean+
             rev_Range+totmrc_Range+change_mou+unan_vce_Mean, data=SWT,family=binomial), k=log(50000))
summary(fit)
#only drop_vce_Range, blck_vce_Range, threeway_Mean, mou_Mean, totmrc_Mean, ovrrev_Mean, 
#rev_Range, totmrc_Range, change_mou are left

#check outliers
stem(cdata$attempt_Mean)
stem(cdata$cc_mou_Mean)
stem(cdata$drop_vce_Range)
stem(cdata$blck_vce_Range)
#cc_mou_Mean, crop_vce_Range and blck_vce_Range have few outliers
#outliers are taken care of below
##################################

#nvariables<-SWT[, c(1:34,173) ]
#attributes(nvariables)
#tab<-cor(nvariables, method="pearson", use="complete.obs")
#write.xlsx(tab, "/Users/marieschild/Desktop/Data Science/SWT/ncorrelations.xlsx")


################################################
#############################################
#Continue with smaller dataset (selected variables)

workingdata<-subset(SWT, select=c(ethnic, eqpdays, months, crclscod,
                                  asl_flag, avg3mou, avg3qty, area, dualband, hnd_price, phones,
                                  models, churn, drop_vce_Range, blck_vce_Range, threeway_Mean, 
                                  mou_Mean, totmrc_Mean, ovrrev_Mean, rev_Range, totmrc_Range, change_mou))

#setting outliers>200 to 200
workingdata$cc_mou_Mean <- ifelse(workingdata$cc_mou_Mean>200,200,workingdata$cc_mou_Mean)
workingdata$drop_vce_Range <- ifelse(workingdata$drop_vce_Range>160,160,workingdata$drop_vce_Range)
workingdata$blck_vce_Range <- ifelse(workingdata$blck_vce_Range>400,400,workingdata$blck_vce_Range)

stem(workingdata$cc_mou_Mean)
stem(workingdata$drop_vce_Range)
stem(workingdata$blck_vce_Range)


#binning: setting age of second household member to factor variable with 4 levels: 
#child (0-18), young (19-30), workingage (31-65), senior (65+)
summary(workingdata$age2)
workingdata$age2<-as.numeric(workingdata$age2)
workingdata$age2[workingdata$age2=="0"]<-NA
#too many missings--> delete variable age2

#setting credit class code to factor with 3 levels: goodcredit (A-D), OKcredit(E-O), badcredit (P-Z) 
workingdata$crclscod<-as.numeric(workingdata$crclscod)
br2<-c(0,17,36,100 )
workingdata$crclscod<-.bincode(workingdata$crclscod, breaks=br2, include.lowest=T)
workingdata$crclscod<-as.factor(workingdata$crclscod)
levels(workingdata$crclscod)=list(GoodCredit=1, OKCredit=2, BadCredit=3)

#!!delete Variable active suscribers due to too few observations in one class


#ching's category variable #process all categorical into grouping dummy
##hnd_price into 3 group
#low(<100), mid(100-200), high(200+)
workingdata$hnd_price<-as.numeric(workingdata$hnd_price)
br_hnd_price<-c(-5,100,200,1000 )
workingdata$hnd_price<-.bincode(workingdata$hnd_price, breaks=br_hnd_price, include.lowest=T)
workingdata$hnd_price<-as.factor(workingdata$hnd_price)
levels(workingdata$hnd_price)=list(low=1, mid=2, high=3)


##numbers of phones into 3 group
#one(1), two(2), more(3+)
workingdata$phones<-as.numeric(workingdata$phones)
br_phones<-c(0,1,2,100 )
workingdata$phones<-.bincode(workingdata$phones, breaks=br_phones, include.lowest=T)
workingdata$phones<-as.factor(workingdata$phones)
levels(workingdata$phones)=list(one=1, two=2, more=3)


##number of models into 3 group
#one(1), two(2), more(3+)
workingdata$models<-as.numeric(workingdata$models)
br_models<-c(0,1,2,100 )
workingdata$models<-.bincode(workingdata$models, breaks=br_models, include.lowest=T)
workingdata$models<-as.factor(workingdata$models)
levels(workingdata$models)=list(one=1, two=2, more=3)



#set unknown=NA in dualband and delete missings
workingdata$dualband[workingdata$dualband=="U"] = NA

#make asl_flag 0 = no, 1 = yes dummy
dummyasl = as.numeric(workingdata$asl_flag) - 1

#make aggregated area variable (East, South, North, West) 
workingdata$bigarea = recode(workingdata$area, "c('DC/MARYLAND/VIRGINIA AREA','NEW ENGLAND AREA','NEW YORK CITY AREA','OHIO AREA','PHILADELPHIA AREA')='EAST';c('ATLANTIC SOUTH AREA','CENTRAL/SOUTH TEXAS AREA','DALLAS AREA','HOUSTEN AREA','NORTH FLORIDA AREA','SOUTH FLORIDA AREA','TENNESSEE AREA')='SOUTH';c('CHICAGO AREA','GREAT LAKES AREA','MIDWEST AREA')='NORTH'; else='WEST'")

#too many NA's---> delete variable age1



#recoding ethnicity roll-up code to factor w/ 9 levels:
#Asian, Northern European, Southern European, Mixed, Hispanic, Jewish, Arabic, African American, Pacific Islander
summary(workingdata$ethnic)
#setting 'unknown', 'blank', 'C', 'X' to NA
workingdata$ethnic[workingdata$ethnic=="U"] = NA
workingdata$ethnic[workingdata$ethnic==""] = NA
workingdata$ethnic[workingdata$ethnic=="X"] = NA
workingdata$ethnic[workingdata$ethnic=="C"] = NA

#############"Northern European" has a count of 27103, at least 4x bigger than the next biggest ethnic group. problem?########
ethnic_new = recode(workingdata$ethnic, "c('B','O')='Asian'; c('N','F','G','S')='NorthernEuropean'; c('M')='Mixed';
                    c('D','I')='SouthernEuropean'; c('H')='Hispanic'; c('J')='Jewish'; c('P')='PacificIslander'; c('R')='Arabic'; 
                    c('Z')='AfricanAmerican'")
workingdata$ethnic <- ethnic_new
#View(workingdata$ethnic)

#deleting lor and marital status due to too many NA's


workingdata$churn<-factor(workingdata$churn)


############################################################################################################################

#Missings for numeric
nums <- sapply(workingdata, is.numeric)
  numeric=workingdata[ , nums]
  sum(is.na(numeric))
  mean.imp <- function(x) { missing <- is.na(x) 
                              n.missing <-sum(missing) 
                              x.obs <-x[!missing] 
                              imputed <- x 
                              imputed[missing] <- mean(x.obs) 
                              return (imputed) 
  } 
  impute_numeric<-mean.imp(numeric)


cat <- sapply(workingdata, is.numeric)
categorical=workingdata[ , !cat]

wdata<-data.frame(c(impute_numeric, categorical))
summary(wdata)
wdata<-na.omit(wdata)

#Variablenselektion for Numerics
#rf<-randomForest(churn~.,data=impute_numeric, importance=TRUE)
#imp<-importance(rf,type=1,scale=TRUE)

#randomForest doesn't work!!

#Run the whole procedure also for the testset to have basis for prediction! 


###############################################################
###############################
#MODELS

#Tree
tree1<-rpart(churn~., data=wdata, method="class")
summary(tree1)

###Fitting log-regression

reg<-step(glm(churn~. , data=wdata, family="binomial"), k=log(40000))
summary(reg)

###############################################################################
#############################################################################
#Predictions for logit and tree

#adjust Testset to coding of Trainingset
#which(colnames(workingdata)=="churn")
#churn <- names(workingdata)[names(workingdata)=="workingdata$churn"]
#churn <- seq(from=3, to=3, length.out=50000)

#Testset <- cbind(Testset, churn)

Predset<-subset(Testset, select=c(ethnic, eqpdays, months, crclscod,
                                  asl_flag, avg3mou, avg3qty, area, dualband, hnd_price, phones,
                                  models, drop_vce_Range, blck_vce_Range, threeway_Mean, 
                                  mou_Mean, totmrc_Mean, ovrrev_Mean, rev_Range, totmrc_Range, change_mou))


Predset$cc_mou_Mean <- ifelse(Predset$cc_mou_Mean>200,200,Predset$cc_mou_Mean)
Predset$drop_vce_Range <- ifelse(Predset$drop_vce_Range>160,160,Predset$drop_vce_Range)
Predset$blck_vce_Range <- ifelse(Predset$blck_vce_Range>400,400,Predset$blck_vce_Range)

Predset$crclscod<-as.numeric(Predset$crclscod)
br2<-c(0,17,36,100 )
Predset$crclscod<-.bincode(Predset$crclscod, breaks=br2, include.lowest=T)
Predset$crclscod<-as.factor(Predset$crclscod)
levels(Predset$crclscod)=list(GoodCredit=1, OKCredit=2, BadCredit=3)

Predset$hnd_price<-as.numeric(Predset$hnd_price)
br_hnd_price<-c(-5,100,200,1000 )
Predset$hnd_price<-.bincode(Predset$hnd_price, breaks=br_hnd_price, include.lowest=T)
Predset$hnd_price<-as.factor(Predset$hnd_price)
levels(Predset$hnd_price)=list(low=1, mid=2, high=3)

Predset$phones<-as.numeric(Predset$phones)
br_phones<-c(0,1,2,100 )
Predset$phones<-.bincode(Predset$phones, breaks=br_phones, include.lowest=T)
Predset$phones<-as.factor(Predset$phones)
levels(Predset$phones)=list(one=1, two=2, more=3)

Predset$models<-as.numeric(Predset$models)
br_models<-c(0,1,2,100 )
Predset$models<-.bincode(Predset$models, breaks=br_models, include.lowest=T)
Predset$models<-as.factor(Predset$models)
levels(Predset$models)=list(one=1, two=2, more=3)


Predset$dualband[Predset$dualband=="U"] = NA

dummyasl = as.numeric(Predset$asl_flag) - 1

Predset$bigarea = recode(Predset$area, "c('DC/MARYLAND/VIRGINIA AREA','NEW ENGLAND AREA','NEW YORK CITY AREA','OHIO AREA','PHILADELPHIA AREA')='EAST';c('ATLANTIC SOUTH AREA','CENTRAL/SOUTH TEXAS AREA','DALLAS AREA','HOUSTEN AREA','NORTH FLORIDA AREA','SOUTH FLORIDA AREA','TENNESSEE AREA')='SOUTH';c('CHICAGO AREA','GREAT LAKES AREA','MIDWEST AREA')='NORTH'; else='WEST'")


#setting 'unknown', 'blank', 'C', 'X' to NA
Predset$ethnic[Predset$ethnic=="U"] = NA
Predset$ethnic[Predset$ethnic==""] = NA
Predset$ethnic[Predset$ethnic=="X"] = NA
Predset$ethnic[Predset$ethnic=="C"] = NA

ethnic_new = recode(Predset$ethnic, "c('B','O')='Asian'; c('N','F','G','S')='NorthernEuropean'; c('M')='Mixed';
                    c('D','I')='SouthernEuropean'; c('H')='Hispanic'; c('J')='Jewish'; c('P')='PacificIslander'; c('R')='Arabic'; 
                    c('Z')='AfricanAmerican'")
Predset$ethnic <- ethnic_new


############################################################################################################################

#Missings for numeric
nums_t <- sapply(Predset, is.numeric)
numeric_t=Predset[ , nums_t]
sum(is.na(numeric_t))
mean.imp <- function(x) { missing <- is.na(x) 
                          n.missing <-sum(missing) 
                          x.obs <-x[!missing] 
                          imputed <- x 
                          imputed[missing] <- mean(x.obs) 
                          return (imputed) 
} 
impute_numeric_t<-mean.imp(numeric)


cat <- sapply(Predset, is.numeric)
categorical=Predset[ , !cat]

Predset<-data.frame(c(impute_numeric_t, categorical))
Predset<-na.omit(Predset)


#Brier Score
n <- nrow(wdata)
split <- ceiling(n*0.6) # split point to partition into training and test set
random.idx <- sample(n) # to be fair, the exercise did not ask for a random sample, but it is good practice to shuffle data prior to sampling
idx.train <- random.idx[1:split] # define index vector for training data
idx.test  <- random.idx[(split+1):n] # and also for test data
train <- wdata[idx.train, ] # training set
test <-  wdata[idx.test , ] # test set

lr <- glm(churn~.,data=train, family=binomial(link="logit"))
y.lr <- predict(lr, newdata=test, type="response")
dtree <- rpart(churn~., data=train, method="class")
y.dt <- predict(dtree, newdata=test, type="prob")[,2] # recall that predict with type=prob produces a matrix of probabilities per class
#summary(wdata$churn)
#y <- wdata$churn
#create zero vector y to store churn values
y <- seq(from = 0, to = 0, length.out = length(y))
#where churn vector has label "1", replace in y with value 1
y[wdata$churn=="1"] <- 1
#y <- y1
#y <- as.numeric(wdata$churn)
#frequency(y)

brier.lr <- sum((y[idx.test]-y.lr)^2) / length(idx.test) 
brier.dt <- sum((y[idx.test]-y.dt)^2) / length(idx.test) 
sprintf("Brier score of logistic regression and dtree on the test set is %.4f and %.4f respectively.",
        brier.lr, brier.dt)
if (brier.lr>brier.dt)  {
  sprintf("The decision tree predicts more accurately.")
} else {
  if (brier.lr<brier.dt) {sprintf("Logistic regression predicts more accurately.")}
  else {sprintf("Both classifiers achieve the same level of accuracy.")}    
}

#Confusion matrices for each classifier
cutoff<-0.5
y.lr.class <- factor(y.lr>cutoff, labels=c("churn", "notchurn")) 
CrossTable(wdata$churn[idx.test], y.lr.class)
#Repeat the same as above for the decision tree classifier
y.dt.class <- factor(y.dt>cutoff, labels=c("churn", "notchurn")) 
CrossTable(wdata$churn[idx.test],y.dt.class)
table(wdata$churn[idx.test], y.lr.class)
tab.lr <- table(wdata$churn[idx.test], y.lr.class)
error.lr <- 1 - sum(diag(tab.lr))/sum(tab.lr) 
tab.dt <- table(wdata$churn[idx.test], y.dt.class)
error.dt <- 1 - sum(diag(tab.dt))/sum(tab.dt) 
sprintf("Classification error of logistic regression and dtree on the test set is %.4f and %.4f respectively.",
        error.lr, error.dt)


#Predictions!!
predict(tree1, newdata=Predset, type="prob")
predict(reg, newdata=Predset, type="response")

#################################################################################################
#############  NN  ##############################################################################
#################################################################################################

##preprocess for NN, change all factor into dummy...
NN_wdata<-subset(wdata, select=c(churn, ethnic, eqpdays, months, crclscod,
                                  asl_flag, avg3mou, avg3qty, dualband, hnd_price, phones,
                                  models, drop_vce_Range, blck_vce_Range, threeway_Mean, bigarea, 
                                  mou_Mean, totmrc_Mean, ovrrev_Mean, rev_Range, totmrc_Range, change_mou))

###
dummy_phones<-class.ind(NN_wdata$phones);
colnames(dummy_phones)<-c("phones_one","phones_two","phones_more")
dummy_models<-class.ind(NN_wdata$models);
colnames(dummy_models)<-c("models_one","models_two","models_more")
dummy_ethnic<-class.ind(NN_wdata$ethnic)
dummy_crclscod<-class.ind(NN_wdata$crclscod)
dummy_asl_flag<-class.ind(NN_wdata$asl_flag);
colnames(dummy_asl_flag)<-c("asl_flag_N","asl_flag_Y")
#omit??? dummy_dualband<-class.ind(NN_wdata$dualband);
dummy_hnd_price<-class.ind(NN_wdata$hnd_price)
dummy_bigarea<-class.ind(NN_wdata$bigarea)
dummy<-as.data.frame(cbind(dummy_phones, dummy_models, dummy_ethnic, dummy_crclscod,
              dummy_asl_flag, #omit?dummy_dualband, 
              dummy_hnd_price, dummy_bigarea))

### collect all numeric with factor into dummy
NN_data<-cbind(subset(NN_wdata,select=c(churn,  eqpdays, months, avg3mou, avg3qty, 
                                 drop_vce_Range, blck_vce_Range, threeway_Mean, 
                                 mou_Mean, totmrc_Mean, ovrrev_Mean, rev_Range, 
                                 totmrc_Range, change_mou)),
               dummy
               )

#find sample and divide to train test set, fit a glm first to see MSE
index <- sample(1:nrow(NN_data),round(0.75*nrow(NN_data)))
train <- NN_data[index,]
test <- NN_data[-index,]

#base comparison on logit model 
lm.fit <- glm(churn~., data=train, family=binomial(link="logit")) #take so long...124-ID
summary(lm.fit)

pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$churn)^2)/nrow(test)

#normalize b4 NN
maxs <- apply(NN_data, 2, max) 
mins <- apply(NN_data, 2, min)

scaled <- as.data.frame(scale(NN_data, #center = mins, 
                              scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

#find the appor node for NN, usual 2/3 input variable (80 now)
#install.packages("neuralnet")
library("neuralnet")
n <- names(train_)
f <- as.formula(paste("churn ~", paste(n[!n %in% "churn"], collapse = " + ")))
nnl2 <- neuralnet(f,data=train_,hidden=2,stepmax=1e+07,linear.output=T)

#can plot
plot(nnl2)

#prediction on churn
test.r <- (test_$churn - min(test_$churn))/(max(test_$churn)-min(test_$churn))

#change nn
pr.nnl32 <- compute(nnl32,test_[,1:13])
pr.nnl32_ <- (pr.nnl32$net.result - min(pr.nnl32$net.result))/(max(pr.nnl32$net.result)-min(pr.nnl32$net.result))
MSE.nnl32 <- sum((test.r - pr.nnl32_)^2)/nrow(test_)


(cbind(test.r,pr.nnl2_))


###Fitting NN#####
install.packages("pROC");library("pROC")
levels(Testset$churn) <- list(no="0", yes="1")
ctrl <- trainControl(summaryFunction = twoClassSummary,classProbs = TRUE)
set.seed(476)
lnnet <- train(Testset,y = Testset$churn,
               method = "nnet", metric = "ROC", 
               trControl = ctrl, preProcess = "range", 
               tuneLength = 2, trace = FALSE,maxit = 100);lnnet
##Asessing prediction
confusionMatrix(data = NN_data, reference = pr.nnl2_)
#ROC-curve
Roc <- roc(response = lnnet$pred$obs, predictor = lnnet$pred$1,levels = rev(levels(lnnet$pred$obs)))
plot(Roc, legacy.axes = TRUE)
auc(Roc)



#we then compare the two MSEs
print(paste(MSE.lm,MSE.nn22,MSE.nn53))

#para plot for two
par(mfrow=c(1,2))

plot(test$churn,pr.nn22_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$churn,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

par(mfrow=c(1,1))

#same plot for two
plot(test$churn,pr.nn22_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$churn,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

#cross validate
library(boot)
set.seed(200)
lm.fit <- glm(churn~.,data=NN_data)
cv.glm(NN_data,lm.fit,K=10)$delta[1]

set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(NN_data),round(0.9*nrow(NN_data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result #*(max(NN_data$churn)-min(NN_data$churn))+min(NN_data$churn)
  
  test.cv.r <- (test.cv$churn) #*(max(NN_data$churn)-min(NN_data$churn))+min(NN_data$churn)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

mean(cv.error)
cv.error
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)




