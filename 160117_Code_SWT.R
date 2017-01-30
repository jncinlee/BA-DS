library("nnet")
library("caret")
library("randomForest")
library("rpart")
install.packages("xlsx")
library("xlsx")

setwd("/Users/Caro/Documents/uni/Statistik/BADS/Special Working Task/BADS - SWT - Trainingset - 2015-10-27")
SWT<-read.csv("BADS - SWT - Trainingset - 2015-10-27.csv")
Testset<-read.csv("/Users/Caro/Documents/uni/Statistik/BADS/Special Working Task/BADS - SWT - Testset - 2015-10-27.csv")

#categorical
categorical = SWT[, c(95, 120:173)]
categorical[] <- lapply(categorical, as.factor)
#recode all spaces to NA
SWT[SWT == ""] <- NA
#variables=data.frame(variables) #just saving for SPSS

########################################################
#Variablesselektion: 

#Caro: 
data<-read.csv("BADS - SWT - Trainingset - 2015-10-27.csv")
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
           blck_vce_Range+callfwdv_Mean+recv_sms_Range+recv_vce_Range+threeway_Mean+unan_dat_Range+unan_vce_Range,
        data=cdata,family=binomial), k=log(50000))
summary(fit)
#only attempt_Mean, cc_mou_Mean, drop_vce_Range and blck_vce_Range are left

#check outliers
stem(cdata$attempt_Mean)
stem(cdata$cc_mou_Mean)
stem(cdata$drop_vce_Range)
stem(cdata$blck_vce_Range)
#cc_mou_Mean, crop_vce_Range and blck_vce_Range have few outliers
#outliers are taken care of below
##################################

nvariables<-SWT[, c(1:34,173) ]
attributes(nvariables)
tab<-cor(nvariables, method="pearson", use="complete.obs")
#write.xlsx(tab, "/Users/marieschild/Desktop/Data Science/SWT/ncorrelations.xlsx")


################################################
#############################################
#Continue with smaller dataset (selected variables)

workingdata<-subset(SWT, select=c(age1, ethnic, eqpdays, lor, marital, age2, actvsubs, months, crclscod,
                                  asl_flag, avg3mou, avg3qty, csa, area, dualband, hnd_price, phones,
                                  models, hnd_webcap, churn, attempt_Mean, cc_mou_Mean, drop_vce_Range, blck_vce_Range))

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
workingdata$age2[is.na(workingdata$age2)] <- mean(workingdata$age2, na.rm=TRUE)
breaks<-c(0,18,30,65,100 )
workingdata$age2<-.bincode(workingdata$age2, breaks=breaks, include.lowest=T)
workingdata$age2<-as.factor(workingdata$age2)
levels(workingdata$age2)=list(child=1, young=2, workingage=3, senior=4)

#setting credit class code to factor with 3 levels: goodcredit (A-D), OKcredit(E-O), badcredit (P-Z) 
workingdata$crclscod<-as.numeric(workingdata$crclscod)
br2<-c(0,17,36,100 )
workingdata$crclscod<-.bincode(workingdata$crclscod, breaks=br2, include.lowest=T)
workingdata$crclscod<-as.factor(workingdata$crclscod)
levels(workingdata$crclscod)=list(GoodCredit=1, OKCredit=2, BadCredit=3)


#setting number of active subscribers to factor with 4 levels: 
#No(0), One(1), Max5(2-5), Morethan5(5+)
workingdata$actvsubs<-as.numeric(workingdata$actvsubs)
br3<-c(-5,0,1,5,100 )
workingdata$actvsubs<-.bincode(workingdata$actvsubs, breaks=br2, include.lowest=T)
workingdata$actvsubs<-as.factor(workingdata$actvsubs)
levels(workingdata$actvsubs)=list(No=1, One=2, Max5=3,Morethan5=4)


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


##hnd_webcap web capability into 3 group
#yes("WC" or "WCMB"), no("NA"), dunno("UNKW")
workingdata$hnd_webcap<-as.numeric(workingdata$hnd_webcap)
br_hnd_webcap<-c(0,1,2,5)
workingdata$hnd_webcap<-.bincode(workingdata$hnd_webcap, breaks=br_hnd_webcap, include.lowest=T)
workingdata$hnd_webcap<-as.factor(workingdata$hnd_webcap)
levels(workingdata$hnd_webcap)=list(no=1, dunno=2, yes=3)


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
predict(tree1, newdata=Testset, type="prob")



###Fitting log-regression
levels(workingdata$churn) <- list(no="0", yes="1")
ctrl <- trainControl(summaryFunction = twoClassSummary,classProbs = TRUE)
set.seed(476)
lr <- train(workingdata,y = workingdata$churn, method = "glm", metric = "ROC", trControl = ctrl)
lr


reg<-step(glm(churn~., data=wdata, family="binomial"), k=log(40000))
fit<-glm(churn~., data=workingdata, family="binomial")
summary(fit)



##Asessing prediction
confusionMatrix(data = lr$pred$pred, reference = lr$pred$obs)
#ROC-curve
Roc <- roc(response = lr$pred$obs, predictor = lr$pred$1,levels = rev(levels(lr$pred$obs)))
plot(Roc, legacy.axes = TRUE)
auc(Roc)

