  library("nnet")
  library("caret")
  library("randomForest")
  library("e1071")
  library("Amelia")
  library("mice")
  library("VIM")
  library("randomForest")
  library("devtools")
  library("rgl")
  library("homals")
  library("ForImp")
  library("x12")
  library("MASS")
  library("mi")
  library("rpart")
  library("foreign")
  library("xlsx")
  library("xlsxjars")
  library("rJava")
  library("pROC")
  library("neuralnet")
  library("foreign")
  
  #setwd("~/Desktop")
  #SWT = read.csv("/Users/marieschild/Desktop/Data\ Science/SWT/BADS\ -\ SWT\ -\ Trainingset\ -\
  #               2015-10-27/BADS\ -\ SWT\ -\ Trainingset\ -\ 2015-10-27.csv",header=TRUE, sep=",")
  #testset<-read.csv("/Users/marieschild/Desktop/Data Science/SWT/Testset.csv", header=T, sep=",")
  
  setwd("D:/HU_Stat/Business Analytics Data Science/Task")
  SWT<-read.table("BADS_SWT_Trainingset.csv", sep = ',',header=T)
  testset<-read.csv("BADS_SWT_Testset.csv", header=T, sep=",")
  
  testset$churn<-NA
  dset<-rbind(SWT, testset)
  #Test und Training zusammengefugt, Spaces durch NA ersetzt, kategorische Variables faktorisiert!
  which(colnames(testset)=="churn")
  which(colnames(SWT)=="months")
  #categorical!
  categorical = SWT[, c(95, 120:173)]
  categorical[] <- lapply(categorical, as.factor)
  #recode all spaces to NA!
  SWT[SWT == ""] <- NA
  #variables=data.frame(variables) #just saving for SPSS
  #Deletion of Variables (based on missings >40% and outcome correlation >0,03)
  workingdata<-subset(SWT, select=c(age1, 
                                     ethnic, eqpdays, 
                                    lor, 
                                     marital, 
                                    age2, 
                                     children, 
                                    actvsubs, months, 
                                     crclscod, 
                                     asl_flag, avg3mou, avg3qty, 
                                     csa, 
                                     area, 
                                     dualband, 
                                    hnd_price, 
                                    phones, 
                                    models, 
                                    hnd_webcap, totrev, adjrev,
                                    
                                    churn)) #Missings for numeric
  
  #process all categorical into grouping dummy
  ##hnd_price into 3 group
  #hnd_price_low<-(SWT$hnd_price < 100 );
  #hnd_price_mid<-(100 <= SWT$hnd_price & SWT$hnd_price < 200 );
  #hnd_price_hig<-(SWT$hnd_price >= 200 );
  workingdata$hnd_price<-as.numeric(workingdata$hnd_price)
  br_hnd_price<-c(-5,100,200,1000 )
  workingdata$hnd_price<-.bincode(workingdata$hnd_price, breaks=br_hnd_price, include.lowest=T)
  workingdata$hnd_price<-as.factor(workingdata$hnd_price)
  levels(workingdata$hnd_price)=list(low=1, mid=2, high=3)
  
  
  ##phones into 3 group
  #phones_1<-(SWT$phones == 1);
  #phones_2<-(SWT$phones == 2);
  #phones_3plus<-(SWT$phones >=3);
  workingdata$phones<-as.numeric(workingdata$phones)
  br_phones<-c(0,1,2,100 )
  workingdata$phones<-.bincode(workingdata$phones, breaks=br_phones, include.lowest=T)
  workingdata$phones<-as.factor(workingdata$phones)
  levels(workingdata$phones)=list(one=1, two=2, more=3)
  
  
  ##models into 3 group
  #models_1<-(SWT$models == 1);
  #models_2<-(SWT$models == 2);
  #models_3plus<-(SWT$models >= 3);
  workingdata$models<-as.numeric(workingdata$models)
  br_models<-c(0,1,2,100 )
  workingdata$models<-.bincode(workingdata$models, breaks=br_models, include.lowest=T)
  workingdata$models<-as.factor(workingdata$models)
  levels(workingdata$models)=list(one=1, two=2, more=3)
  
    
  ##hnd_webcap into 3 group
  #hnd_webcap_y<-(SWT$hnd_webcap == "WC" | SWT$hnd_webcap == "WCMB"); #with web capability
  #hnd_webcap_n<-(SWT$hnd_webcap == "NA"); #without
  #hnd_webcap_dk<-(SWT$hnd_webcap == "UNKW"); #don't know
  workingdata$hnd_webcap<-as.numeric(workingdata$hnd_webcap)
  br_hnd_webcap<-c(0,1,2,5)
  workingdata$hnd_webcap<-.bincode(workingdata$hnd_webcap, breaks=br_hnd_webcap, include.lowest=T)
  workingdata$hnd_webcap<-as.factor(workingdata$hnd_webcap)
  levels(workingdata$hnd_webcap)=list(no=1, dunno=2, yes=3)


  ##ethnic tex #也超多 怎麼分? 用有沒有churn?
  ethnic_ca<-(ethnic %in% c("F","G","H","I","J","N")) #caucasian
  ethnic_as<-(ethnic %in% c("B",)) #asian
  
  ##marital tex
  marital_u<-(SWT$marital == "U") #unknown
  marital_m<-(SWT$marital %in% c("M","A")) #married
  marital_s<-(SWT$marital %in% c("S","B")) #ledig
  
  ##children tex
  children_y<-(SWT$marital =="Y") #have children below 17
  children_n<-(SWT$marital =="N") #don't
  
  ##crclscod tex #relation to churn?
  crclscod_a<-(SWT$crclscod %in% c("A","A2","A3","AA")) #a best rating
  crclscod_b<-(SWT$crclscod %in% c("B","B2","BA"))
  crclscod_c<-(SWT$crclscod %in% c("C","C2","C5","CA","CC","CY"))
  crclscod_d<-(SWT$crclscod %in% c("D","D2","D4","D5","DA"))
  crclscod_bad<-(!SWT$crclscod %in% c("A","A2","A3","AA",
                                      "B","B2","BA",
                                      "C","C2","C5","CA","CC","CY",
                                      "D","D2","D4","D5","DA"
                                      )) #rate below e (include e, not in above)
  
  ##asl_flag tex #兩個
  asl_flag_n<-(SWT$asl_flag == "N") #don't have accout limit
  asl_flag_y<-(SWT$asl_flag == "Y") #have
  
  #csa tex #通勤區
  csa_w
  csa_e
  csa_m
  
  #area tex
  area_w<-(SWT$area %in% c())
  area_e<-(SWT$area %in% c())
  area_m<-(SWT$area %in% c())
  
  ##dualband tex
  dualband_y<-(SWT$dualband == "Y") #have dualband
  dualband_n<-(SWT$dualband == "N") #don't have
  dualband_t<-(SWT$dualband == "T") #tri-band
  dualband_u<-(SWT$dualband == "U") #dunno
  
summary(workingdata)
plot(table(workingdata$csa,workingdata$area))
  
  #findout how much missing by row, 890 example mis>3 mostly 6,7 in working data
  rowmis<-apply(workingdata,1,function(x) sum(is.na(x)));length(rowmis[rowmis>3])
  
  nums <- sapply(workingdata, is.numeric)
  numeric=workingdata[ , nums]
  sum(is.na(numeric))
  mean.imp <- function(x) { 
                              missing <- is.na(x) 
                              n.missing <-sum(missing) 
                              x.obs <-x[!missing] 
                              imputed <- x 
                              imputed[missing] <- mean(x.obs) 
                              return (imputed) 
  } 
  impute_numeric<-mean.imp(numeric) #numerical with imputed mean
  sum(is.na(impute_numeric))
  #Collinearity for numeric variables erased variables (comp_dat_Mean, drop_blk_Mean, attempt_Mean, complete_Mean 
  #findLinearCombos(impute_numeric)!
  #summary(impute_numeric[,c(32, 53:55)])! !
  #Variablenselektion for Numerics!
  
  
  #########################
  ###########NN############
  #########################
  
  
  #find sample and divide to train test set, fit a glm first to see MSE
  index <- sample(1:nrow(impute_numeric),round(0.75*nrow(impute_numeric)))
  train <- impute_numeric[index,]
  test <- impute_numeric[-index,]
  
  #base comparison on logit model 
  lm.fit <- glm(churn~., data=train, family=binomial(link="logit")) #take so long...124-ID
  summary(lm.fit)
  
  pr.lm <- predict(lm.fit,test)
  MSE.lm <- sum((pr.lm - test$churn)^2)/nrow(test)
  
  #normalize b4 NN
  maxs <- apply(impute_numeric, 2, max) 
  mins <- apply(impute_numeric, 2, min)
  
  scaled <- as.data.frame(scale(impute_numeric, #center = mins, 
                                scale = maxs - mins))
  
  train_ <- scaled[index,]
  test_ <- scaled[-index,]
  
  #find the appor node for NN, usual 2/3 input variable (80 now)
  #install.packages("neuralnet")
  library("neuralnet")
  n <- names(train_)
  f <- as.formula(paste("churn ~", paste(n[!n %in% "churn"], collapse = " + ")))
  nnl632 <- neuralnet(f,data=train_,hidden=c(6,3,2),stepmax=1e+07,linear.output=T)
  #nn2   20min #nn3 #nn4 #nn8
  #nnl2  20min #nnl3 24min #nnl4 1hr #nnl5 ? #nnl7 9h #nnl8 7h49m
  #nn22  23minquit(45mquit) #nn32 #nn82
  #nnl22 20min #nnl32 #nnl52 
  #nn53  6h44m
  #nnl53 5h?   #nnl63 8h30
  #nnl632 2343
  
  
  #can plot
  plot(nnl53)
  
  #prediction on churn
  #test.r <- (test_$churn)*(max(impute_numeric$churn)-min(impute_numeric$churn))+min(impute_numeric$churn)

  #change nn
  #pr.nnl7 <- compute(nnl7,test_[,1:13])
  #pr.nnl7_ <- pr.nnl7$net.result*(max(impute_numeric$churn)-min(impute_numeric$churn))+min(impute_numeric$churn)
  #MSE.nnl7 <- sum((test.r - pr.nnl7_)^2)/nrow(test_)
  
  #we then compare the two MSEs
  #print(paste(MSE.lm,MSE.nn22,MSE.nn53))
  
  
  ####prediction on churn
  test.r <- (test_$churn - min(test_$churn))/(max(test_$churn)-min(test_$churn))
  
  ####change nn
  n.pr.nnl53 <- compute(nnl53,test_[,1:13])
  n.pr.nnl53_ <- (n.pr.nnl53$net.result - min(pr.nnl53$net.result))/
                (max(pr.nnl53$net.result)-min(pr.nnl53$net.result))
  n.MSE.nnl53 <- sum((test.r - n.pr.nnl53_)^2)/nrow(test_)
  
  result<-cbind(c(MSE.lm, MSE.nnl2, MSE.nnl22, MSE.nnl3, MSE.nnl32, MSE.nnl4, 
          MSE.nnl5, MSE.nnl52, MSE.nnl53, MSE.nnl63, MSE.nnl7, MSE.nnl8),
        c('-', n.MSE.nnl2, n.MSE.nnl22, n.MSE.nnl3, n.MSE.nnl32, n.MSE.nnl4, 
          n.MSE.nnl5, n.MSE.nnl52, n.MSE.nnl53, n.MSE.nnl63, n.MSE.nnl7, n.MSE.nnl8)
        );as.matrix(result)
  
  print(result)
  
  
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
  lm.fit <- glm(churn~.,data=impute_numeric)
  cv.glm(impute_numeric,lm.fit,K=10)$delta[1]
  
  set.seed(450)
  cv.error <- NULL
  k <- 10
  
  library(plyr) 
  pbar <- create_progress_bar('text')
  pbar$init(k)
  
  for(i in 1:k){
    index <- sample(1:nrow(impute_numeric),round(0.9*nrow(impute_numeric)))
    train.cv <- scaled[index,]
    test.cv <- scaled[-index,]
    
    nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
    
    pr.nn <- compute(nn,test.cv[,1:13])
    pr.nn <- pr.nn$net.result*(max(impute_numeric$churn)-min(impute_numeric$churn))+min(impute_numeric$churn)
    
    test.cv.r <- (test.cv$churn)*(max(impute_numeric$churn)-min(impute_numeric$churn))+min(impute_numeric$churn)
    
    cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
    
    pbar$step()
  }
  
  mean(cv.error)
  cv.error
  boxplot(cv.error,xlab='MSE CV',col='cyan',
          border='blue',names='CV error (MSE)',
          main='CV error (MSE) for NN',horizontal=TRUE)
  
  
  
  