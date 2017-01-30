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
"csa"       "0.144389647250736"
"area"	    "0.0520009421438961"
"dualband"	"0.0535026974579284"
"hnd_price"  "0.111259669965496"
"phones"  "0.0479461190867926"
"models"  "0.0505900425917023"
"hnd_webcap"	"0.0671996770075394"

cv<-c()
for (i in 2:36){
cv[i]<-cramersV(jim$churn,jim[,i])
}
cbind(names(jim),cv)


#numeric correlation > 0.03
"totrev"  "0.0359548011200176"
"adjrev"	"0.0369967514204883"
"avg3mou" "-0.055718068509677"
"avg3qty"	"-0.0452275743909335"

jim_nna<-na.omit(jim)
cv<-c()
for (i in 2:36){
  cv[i]<-cor(jim_nna$churn,jim_nna[,i], method="spearman")
}
cbind(names(jim_nna),cv)


#then check above 11 variable in the description table provided by Lessmann
#does it had >40% missing value?
#below 11 variable satisfied condition that cramer V, correlation >0.03
#and missing less than 40%
"csa" "area" "dualband" "hnd_price" "phones" "models" "hnd_webcap" "avg3mou" "avg3qty" "totrev"
"adjrev"


#since all were satisfied, then check the outlier for continuous variables by boxplot ane eyes
boxplot(data[,2:16]) 
#totcalls totmou adjmou adjqty have many outlier, good news is all 4 of them are not in above 
#selected variables
#so we could finalized the variable with 105-139 as below 11 variables
"csa" "area" "dualband" "hnd_price" "phones" "models" "hnd_webcap" "avg3mou" "avg3qty" "totrev"
"adjrev"


#ps. I extra done the simple logstic regression for continuous variable result showing that
'totrev adjrev avgrev avgmou avg3mou avg3rev avg6rev'
#are highly significant to churn variable, which I include 3 in above 11
#this is only reference for variable selection
lg<-glm(data1$churn~totcalls+totmou+totrev+adjrev+adjmou+adjqty+avgrev+avgmou+avgqty+avg3mou+avg3qty+avg3rev+avg6mou+avg6qty+avg6rev
        ,data=data)
summary(lg)



