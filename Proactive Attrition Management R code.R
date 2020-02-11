rm(list=ls())
library(readxl)
library(DataExplorer)
library(dplyr)
require(ggplot2)
library(purrr)
library(Hmisc)
library(MASS)

churn<-read_excel("Logistic Regression Case Study.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, skip = 0)
str(churn)
#Dropped Churndep,retcalls,retccpt
rem<-c("CHURNDEP","RETACCPT","RETCALL","RETCALLS")
churn<-churn[,-which(names(churn)%in% rem)]
#---------Converting categorical variables as factors and numerical as numericals-----------


fac<-c("CHILDREN","CREDITA","CREDITAA","CREDITB","CREDITC","CREDITDE","CREDITGY",
   "CREDITZ","PRIZMRUR","PRIZMUB","PRIZMTWN","MCYCLE",
   "CHURN","REFURB","WEBCAP","TRUCK","RV","OCCPROF","OCCCLER","OCCCRFT"
   ,"OCCSTUD","OCCHMKR","OCCRET","OCCSELF","OWNRENT","MARRYUN","MARRYYES","MARRYNO"
   ,"MAILORD","MAILRES","MAILFLAG","TRAVEL","PCOWN","CREDITCD"
    ,"NEWCELLY","NEWCELLN","REFER")
num<-c("REVENUE","MOU","RECCHRGE","DIRECTAS","OVERAGE","ROAM","CHANGEM","CHANGER","DROPVCE",
    "BLCKVCE","UNANSVCE","CUSTCARE","THREEWAY","MOUREC","OUTCALLS","INCALLS",
    "PEAKVCE","OPEAKVCE","DROPBLK","CALLWAIT","MONTHS","UNIQSUBS",
    "ACTVSUBS","PHONES","MODELS","EQPDAYS","AGE1","AGE2"
   ,"INCOME","CREDITAD","SETPRC")
names(churn[fac])
#Removing variables with 0 standard deviation
sd_0<-apply(churn[num],2,sd)
churn<-dplyr::select(churn,-CALLFWDV)
#Dividing data into Factor and numerical variables
cat_vars<-data.frame(apply(churn[,fac],2,as.factor))#converting factor variables to factor

num_vars<-churn[num]

#---------------Missing value treatment-------------------- 

#we won't get the right mean as missing values in income and setprc
#variables are represented as 0 changing them to NA.

incmiss<-which(churn$INCOME==0)
churn$INCOME[incmiss]<-NA 
sum(is.na(churn$INCOME))

#As missing values are missing at random we impute them by mean.

incmean<-mean(churn$INCOME,na.rm = T)
churn$INCOME[incmiss]<-incmean
#Dropping the variable which represented income missing because it's not needed further
churn<-churn[ ,-(which( names(churn) %in% c("INCMISS")))]
sum(is.na(churn$INCOME))

hpricemiss<-which(churn$SETPRC==0)
churn$SETPRC[hpricemiss]<-NA
sum(is.na(churn$SETPRC))
hpricemean<-mean(churn$SETPRC,na.rm=T)
churn$SETPRC[hpricemiss]<-hpricemean
churn<-churn[ ,-(which( names(churn) %in% c("SETPRCM")))]


#Missing value treatment for all the other variables


churn[,num] <- apply(data.frame(churn[,num]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
churn[,fac] <- apply(data.frame(churn[,fac]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})

#--------------Outlier Treatment--------------
var_Summ=function(x){
        if(class(x)=="numeric"){
                Var_Type=class(x)
                n<-length(x)
                nmiss<-sum(is.na(x))
                mean<-mean(x,na.rm=T)
                std<-sd(x,na.rm=T)
                var<-var(x,na.rm=T)
                min<-min(x,na.rm=T)
                p1<-quantile(x,0.01,na.rm=T)
                p5<-quantile(x,0.05,na.rm=T)
                p10<-quantile(x,0.1,na.rm=T)
                q1<-quantile(x,0.25,na.rm=T)
                q2<-quantile(x,0.5,na.rm=T)
                q3<-quantile(x,0.75,na.rm=T)
                p90<-quantile(x,0.9,na.rm=T)
                p95<-quantile(x,0.95,na.rm=T)
                p99<-quantile(x,0.99,na.rm=T)
                max<-max(x,na.rm=T)
                UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
                LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
                UC2=quantile(x,0.99,na.rm=T)
                LC2=quantile(x,0.01,na.rm=T)
                iqr=IQR(x,na.rm=T)
                UC3=q3+1.5*iqr
                LC3=q1-1.5*iqr
                ot1<-max>UC1 | min<LC1 
                ot2<-max>UC2 | min<LC2 
                ot3<-max>UC3 | min<LC3
                return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
        }
        else{
                Var_Type=class(x)
                n<-length(x)
                nmiss<-sum(is.na(x))
                fre<-table(x)
                prop<-prop.table(table(x))
                
                
                return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
        }
}
num_stats<-t(data.frame(apply(churn[num], 2, var_Summ)))
cat_stats<-data.frame(t(apply(churn[fac], 2, var_Summ)))

out <- function(x){
        quantiles <- quantile( x, c(.01, .99 ),na.rm=TRUE )
        x[ x < quantiles[1] ] <- quantiles[1]
        x[ x > quantiles[2] ] <- quantiles[2]
        x
}
#All columns have outlier data so we need to apply it on all
churn[,num]<-data.frame(apply(churn[,num],2,out))
#-----------------------
#Factor analysis
library(psych)
library(GPArotation)
str(churn)
sd_0<-apply(churn[num],2,sd)
churn<-dplyr::select(churn,-CALLFWDV)
corrm<- cor(churn[,num])
write.csv(corrm,"correlation matrix.csv")
scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=F) 
eigen(corrm)$values                                                    
eigen_values<- mutate(data.frame(eigen(corrm)$values)
                      ,cum_sum_eigen=cumsum(eigen.corrm..values)
                      , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                      , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values)) 
eigen_values<-write.csv(eigen_values,"eigen_values.csv")

FA<-fa(r=corrm, 9, rotate="varimax", fm="ml")
Fac_sort<-fa.sort(FA) 
Loadings<-data.frame(Fac_sort$loadings[1:ncol(churn[,num]),])
write.csv(Loadings,"loadings.csv")
#---------------------------------




                        #***Variable selection for categorical variables***#

fit.cat<- glm(CHURN~.,data = cat_vars,family = "binomial",na.action = na.omit)
anov.cat<- anova(fit.cat, test="Chisq")
anov.cat
# significant categoric variables obtained from chisquare analysis
imp_catvars <-cat_vars[,c("CHURN","CREDITA","CREDITAA","CREDITB","CREDITC","CREDITDE",
                         'PRIZMRUR','PRIZMUB',
                         "REFURB","WEBCAP",'OCCPROF','OCCRET',
                         'OWNRENT','MARRYUN',"MAILORD",'NEWCELLY')]


                      #***Variable selection for numerical variables***#
num_vars$CHURN<-churn$CHURN

a1<-aov(formula <- CHURN ~. , data = num_vars)
summary(a1)
selected_numvars<- subset(num_vars,select= c("REVENUE","MOU","RECCHRGE",'OVERAGE',
                                             'ROAM',"CHANGEM",'CHANGER','DROPVCE','CUSTCARE',
                                             'THREEWAY','PEAKVCE','OPEAKVCE',
                                             'MONTHS','UNIQSUBS','ACTVSUBS',
                                             'PHONES','MODELS','EQPDAYS','AGE1','CREDITAD','SETPRC'))
final_data<-cbind.data.frame(selected_numvars,imp_catvars)
final_data$CALIBRAT<-churn$CALIBRAT

#Dividing data into training,validation and testing using Calibrat variable
#Calibrat 1-train
#         0-Validation
churn_train<- final_data[final_data$CALIBRAT==1,]

churn_valid<-final_data[final_data$CALIBRAT==0,]



#Dropping calibrat which represented that whether the observation is for training or validation purpose

churn_train<-churn_train[,-38]
churn_valid<-churn_valid[,-38]

                #Logistic regression
fit<-glm(CHURN~.,family = "binomial",data=churn_train,control = list(maxit = 100))
#--------------Removing influential observations---------------
summary(fit)
hl<-hatvalues(fit)
churn_train<-churn_train[hl<=(3*50)/40000,]


#----------------doing stepwise logistic regression----------
fit2<-glm(CHURN~.,family = binomial,data=churn_train,control = list(maxit = 100))
step.model <- fit2 %>% stepAIC(trace = FALSE,direction = "both")
summary(step.model)
library(car)
# Variables reduced after step modelling
# PEAKVCE + OPEAKVCE + CALLWAIT + MODELS+ REFER+ CREDITA + 
# PRIZMRUR + OCCPROF + OCCCRFT + OCCRET + OCCSELF + MAILORD

fit3<-glm( CHURN ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
              CHANGER + DROPVCE + THREEWAY + PEAKVCE + MONTHS + UNIQSUBS + 
              ACTVSUBS + PHONES + EQPDAYS + AGE1 + CREDITAD + SETPRC + 
              CREDITAA + CREDITB + CREDITC + CREDITDE + PRIZMRUR + PRIZMUB + 
              REFURB + WEBCAP + MARRYUN + MAILORD + NEWCELLY,
            family = "binomial",data = churn_train)

summary(fit3)
vif(fit3)
#Revenue has a vif of 11 so removing it.
#Removing MARRYUN,PRIZMRUR 

fit4<-glm( CHURN ~MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
             CHANGER + DROPVCE + THREEWAY + PEAKVCE + MONTHS + UNIQSUBS + 
             ACTVSUBS + PHONES + EQPDAYS + AGE1 + CREDITAD + SETPRC + 
             CREDITAA + CREDITB + CREDITC + CREDITDE + PRIZMUB + 
             REFURB + WEBCAP + MAILORD + NEWCELLY,
           family = "binomial",data = churn_train)

summary(fit4)
vif(fit4)
#Removing Prizmub, creditAA,creditC as they are not important 
fit5<- glm(CHURN ~ MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + CHANGER + DROPVCE + 
             THREEWAY + PEAKVCE + MONTHS + UNIQSUBS + ACTVSUBS + 
             PHONES + EQPDAYS + AGE1 + CREDITAD  + SETPRC + CREDITB + 
             CREDITDE + REFURB + WEBCAP + MAILORD +
             NEWCELLY,family = "binomial",data = churn_train)
summary(fit5)

rocinfo<-roc(churn_train$CHURN,fit5$fitted.values,legacy.axis=T)


#----------------------

rocdf<-data.frame(tpp=rocinfo$sensitivities*100,fpp=(1-rocinfo$specificities)*100,thresholds=rocinfo$thresholds)
max<-filter(rocdf,tpp>=65 & fpp<=50)
#0.48 would be good threshold looking at the true positive and false positive rate
churn_train_new$predictchurn2<-predict(fit6,type="response")
churn_train_new$predictchurn2<-ifelse(churn_train_new$predictchurn2>0.48,1,0)
churn_train_new$predictchurn2<-as.factor(churn_train_new$predictchurn2)
confusionMatrix(churn_train_new$predictchurn2,churn_train_new$CHURN)#Accuracy=.5822
source("C:/Abhinav/Institute/R/Analytics/Regression/Concordance.R")
Concordance(fit5) #Concordance=0.616

#--------------
 # Now checking all the assumptions made for logistic regression
 #1) Multicollinearity is dealt with by Vif used earlier
 #2) High leverage points have been dealt above
 #3) There is a linear relationship between the logit of the outcome and each predictor variables
 #Checking third assumption
mydata <- churn_train %>%
select_if(is.numeric)
 predictors<-colnames(mydata)
probabilities<-predict(fit5,type="response")
 library(tidyr)
 mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
   gather(key = "predictors", value = "predictor.value", -logit)
 
   
   
  ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
   geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#If we leave out discrete numerical variables. 
#For most of the continuous variables there isn't a linear relationship between the logit of the outcome and each predictor variables
#So i believe another classification method should be used as this assumption is not
#totally fulfilled.
#----------------Decile Analysis------------------
train<- cbind(churn_train, Prob=predict(fit5, type="response")) 



##Creating Deciles for training data
decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))

train1$CHURN<-as.numeric(as.character(train1$CHURN))
decile_grp<-group_by(train1,decile)

decile_summ_train<-dplyr::summarize(decile_grp, total_cnt=n(), min_prob=min(Prob),max_prob=max(Prob), churn_cnt=sum(CHURN), 
                             non_default_cnt=total_cnt -churn_cnt )
decile_summ_train<-arrange(decile_summ_train, desc(decile))

write.csv(decile_summ_train,"fit_train_DA.csv",row.names = F)

#---------Test data set------------
test1<- cbind(churn_valid, Prob=predict(fit5,churn_valid, type="response")) 

test1$CHURN<-as.numeric(as.character(test1$CHURN))
##Creating Deciles
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
decile_test<-group_by(test1,decile)

decile_summ_test<-dplyr::summarize(decile_test, total_cnt=n(), min_prob=min(Prob),max_prob=max(Prob), churn_cnt=sum(CHURN), 
                                    non_default_cnt=total_cnt -churn_cnt )
decile_summ_test<-arrange(decile_summ_test, desc(decile))

write.csv(decile_summ_test,"fit_test_DA.csv",row.names = F)

#-------------Writing model coefficients to a file
summary(fit5)
write.csv(fit5$coefficients,"Final_model_coeff2.csv")

#-----------# getting the standardized beta coefficients
require(QuantPsyc)
stb= data.frame(lm.beta(fit5))

#-------------Performance measures----------
#-------CONFUSION MATRIX-------
churn_train$predictchurn<-predict(fit4,type="response")
churn_train$predictchurn<-ifelse(churn_train$predictchurn>0.5,1,0)
class(churn_train$predictchurn)
churn_train$predictchurn<-as.factor(churn_train$predictchurn)
churn_train$CHURN<-as.factor(churn_train$CHURN)
library(caret) 
library(e1071)
confusionMatrix(churn_train$predictchurn,churn_train$CHURN)
#---------CONCORDANCE----------
source("C:/Abhinav/Institute/R/Analytics/Regression/Concordance.R")
concord<-Concordance(fit5)
write.csv(concord,"concordance.csv",row.names = F)

#-------------ROC----------------
library(pROC)
par(pty="s")
roc(churn_train$CHURN,fit5$fitted.values,plot=T,legacy.axis=T,percent=T,xlab="false positive percentage",
    ylab="True Positive percentage",print.auc=T)
#------------LIFT CHART--------------

lift <- function(depvar, predcol, groups=10){
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Run Function
dt1 = lift(train1$CHURN, train1$Prob, groups = 10)

# lift chart for train
plot(dt1$bucket, dt1$Cumlift, type="l", ylab="Cumulative lift", 
     xlab="Bucket",main = "Lift chart for training set")

# lift chart for test
dt2= lift(test1$CHURN,test1$Prob,groups = 10)
plot(dt2$bucket, dt2$Cumlift, type="l", ylab="Cumulative lift", 
     xlab="Bucket",main = "lift chart for Validation set")

write.csv(dt1,"gain table_train2.csv")
write.csv(dt2,"gain table_test2.csv")