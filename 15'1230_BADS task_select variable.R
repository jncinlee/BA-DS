#105-139 variable selection
install.packages("cramer")
library("cramer")
install.packages("lsr")
library("lsr")

remove(list=ls())

#use neuro network/logistic regression/SVM
library("foreign")
setwd("D:/HU_Stat/Business Analytics Data Science/Task")
data1<-read.table("BADS_SWT_Trainingset.csv", sep = ',',header=T)

#choose out the variable 
jim<-cbind(data1$churn,data1[,105:139])
names(jim)[1]<-"churn"
names(jim)
[1] "data1$churn"      "totcalls"         "totmou"           "totrev"           "adjrev"          
[6] "adjmou"           "adjqty"           "avgrev"           "avgmou"           "avgqty"          
[11] "avg3mou"          "avg3qty"          "avg3rev"          "avg6mou"          "avg6qty"         
[16] "avg6rev"          "REF_QTY"          "tot_ret"          "tot_acpt"         "prizm_social_one"
[21] "div_type"         "csa"              "area"             "dualband"         "refurb_new"      
[26] "hnd_price"        "pre_hnd_price"    "phones"           "last_swap"        "models"          
[31] "hnd_webcap"       "truck"            "mtrcycle"         "rv"               "occu1"           
[36] "ownrent"

#run category correlation CramerV > 0.03 which is below
"csa"
"area"
"dualband"
"hnd_price"
"phones"
"models"
"hnd_webcap"

cv<-c()
for (i in 2:36){
cv[i]<-cramersV(jim$churn,jim[,i])
}
cbind(names(jim),cv)

#numeric correlation > 0.03
"avg3mou"
"avg3qty"
"totrev"
"adjrev"

jim_nna<-na.omit(jim)
cv<-c()
for (i in 2:36){
  cv[i]<-cor(jim_nna$churn,jim_nna[,i], method="spearman")
}
cbind(names(jim_nna),cv)


cramersV(jim$churn,jim$totrev)

lg<-glm(data1$churn~totcalls+totmou+totrev+adjrev+adjmou+adjqty+avgrev+avgmou+avgqty+avg3mou+avg3qty+avg3rev+avg6mou+avg6qty+avg6rev
        ,data=data)
summary(lg)
pairs(cbind(data1$churn,data[,2:16]))
boxplot(data[,2:16]) #totcalls totmou adjmou adjqty many outlier

totrev adjrev avgrev avgmou avg3mou avg3rev avg6rev

#cramer V >0.03
#missing less than 40%

