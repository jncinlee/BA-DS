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
  hnd_price_low<-(SWT$hnd_price < 100 );
  hnd_price_mid<-(100 <= SWT$hnd_price & SWT$hnd_price < 200 );
  hnd_price_hig<-(SWT$hnd_price >= 200 );
  
  for (i in 1:50000){
    if(SWT$hnd_price < 100 )){
      hnd_price_low[i]<-1
    } else if (100 =< SWT$hnd_price <200 )){
      hnd_price_mid[i]<-1
    } else {
      hnd_price_hig[i]<-1
    }
  }
  
  cbind(SWT$hnd_price,hnd_price_low,hnd_price_mid,hnd_price_hig)
  
  ##phones into 3 group
  phones_1<-rep(0,50000);
  phones_2<-rep(0,50000);
  phones_3plus<-rep(0,50000);
  
  if(phones == 1){
    phones_1<-1
  } else if (phones == 2){
    phones_2<-1
  } else {
    phones_3plus<-1
  } 
  
  ##models into 3 group
  models_1<-rep(0,50000);
  models_2<-rep(0,50000);
  models_3plus<-rep(0,50000);
  
  if(models == 1){
    models_1<-1
  } else if (models == 2){
    models_2<-1
  } else {
    models_3plus<-1
  }
  
  ##hnd_webcap into 3 group
  hnd_webcap_y<-rep(0,50000);
  hnd_webcap_n<-rep(0,50000);
  hnd_webcap_dk<-rep(0,50000);
  
  if(hnd_webcap == "WC" | hnd_webcap == "WCMB"){
    hnd_webcap_y<-1
  } else if (hnd_webcap == "NA"){
    hnd_webcap_n<-1
  } else {
    hnd_webcap_dk<-1
  }
  
  
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
  nn4 <- neuralnet(f,data=train_,hidden=4,stepmax=1e+07,linear.output=F)
  #nn2 20min #nn3 #nn4 #nn8
  #nn22 23minquit(45mquit) #nn32 0004 #nn82
  #nn53 6h44m
  
  
  #can plot
  plot(nn)
  
  #prediction on churn
  pr.nn22 <- compute(nn22,test_[,1:13])
  
  pr.nn22_ <- pr.nn22$net.result*(max(impute_numeric$churn)-min(impute_numeric$churn))+min(impute_numeric$churn)
  test.r <- (test_$churn)*(max(impute_numeric$churn)-min(impute_numeric$churn))+min(impute_numeric$churn)
  
  MSE.nn22 <- sum((test.r - pr.nn22_)^2)/nrow(test_)
  
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
  
  
  
  