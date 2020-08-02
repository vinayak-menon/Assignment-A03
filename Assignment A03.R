rm(list=ls())
#setting working directory
setwd("C:/Users/Vinayak/Documents/GitHub/Assignment A03")
#loading required packages
library(ggplot2)
library(GGally)
library(dplyr)
library(readxl)
library(broom)
library(boot)
library(gridExtra)
#accessing CCPP data
ccpp <- read_xlsx("Folds5x2_pp.xlsx")
ccpp <- as.data.frame(ccpp)
attach(ccpp)


#####################
# MODEL EXPLORATION #
#####################

#summary of data
summary(ccpp)

#scatter plots
ATPE <- ggplot(ccpp,aes(AT,PE))+
  geom_point()+
  geom_smooth()

VPE<- ggplot(ccpp,aes(V,PE))+
  geom_point()+
  geom_smooth()

APPE<-ggplot(ccpp,aes(AP,PE))+
  geom_point()+
  geom_smooth()

RHPE<- ggplot(ccpp,aes(RH,PE))+
  geom_point()+
  geom_smooth()

grid.arrange(ATPE,VPE,APPE,RHPE, ncol = 2,nrow=2, widths = c(4, 6))

#correlogram plot
ggpairs(ccpp[,-5])

#plotting MSE values for various orders
cv_err <- matrix(0,nrow=10,ncol=4)

for (i in 1:10){
  glm_fit <- glm(PE~poly(AT,i), data = ccpp)
  cv_err[i,1] <- cv.glm(ccpp, glm_fit,K=10)$delta[1]
  
  glm_fit <- glm(PE~poly(V,i), data = ccpp)
  cv_err[i,2] <- cv.glm(ccpp, glm_fit,K=10)$delta[1]
  
  glm_fit <- glm(PE~poly(AP,i), data = ccpp)
  cv_err[i,3] <- cv.glm(ccpp, glm_fit,K=10)$delta[1]
  
  glm_fit <- glm(PE~poly(RH,i), data = ccpp)
  cv_err[i,4] <- cv.glm(ccpp, glm_fit,K=10)$delta[1]
}
cv_err <-as.data.frame(cv_err)
colnames(cv_err)<-c("AT","V","AP","RH")
cv_err$order<-1:10

#plotting MSE of each independent variable for orders upto 10
AT_cv<-ggplot(cv_err,aes(x=order,y=AT))+
  geom_path()+
  geom_point()+scale_x_discrete(name="Order",breaks=1:10)+scale_y_continuous(name = "MSE-AT")

V_cv<-ggplot(cv_err,aes(x=1:10,y=V))+
  geom_path()+
  geom_point()+scale_x_discrete(name="Order",breaks=1:10)+scale_y_continuous(name = "MSE-V")

AP_cv <-ggplot(cv_err,aes(x=1:10,y=AP))+
  geom_path()+
  geom_point()+scale_x_discrete(name="Order",breaks=1:10)+scale_y_continuous(name = "MSE-AP")

RH_cv<- ggplot(cv_err,aes(x=1:10,y=RH))+
  geom_path()+
  geom_point()+scale_x_discrete(name="Order",breaks=1:10)+scale_y_continuous(name = "MSE-RH")

grid.arrange(AT_cv,V_cv,AP_cv,RH_cv,ncol=2,nrow=2)

#normalizing data
for (i in 1:5) {
  ccpp[,i] <- (ccpp[,i]-mean(ccpp[,i]))/sd(ccpp[,i])
}
#summary of normalized data
summary(ccpp)

##################
# MODEL CREATION #
##################

#MODEL 1- STANDARD MULTIPLE LINEAR REGRESSION
m_1 <- lm(PE~.,data=ccpp)
summary(m_1)

#MODEL 2-HIGHER ORDER TERMS
m_2 <- lm(PE~.+I(AT^3)+I(AP^3),data=ccpp)
summary(m_2)

#MODEL 3- INTERACTION EFFECTS
m_3 <- lm(PE~.+(AT*AP)+(AT*V)+(AT*RH),data=ccpp)
summary(m_3)

#MODEL 4- INTERACTION EFFECTS +HIGHER ORDER TERMS
m_4 <- lm(PE~.+(AT*AP)+(AT*V)+(AT*RH)+I(AT^3)+I(AP^3),data=ccpp)
summary(m_1)

##################
# MODEL ANALYSIS #
##################

#Obtaining various measures of fit
fit<-as.data.frame(matrix(0,ncol = 5,nrow = 4))
rownames(fit)<-c("Model 1","Model 2","Model 3","Model 4")
colnames(fit)<-c("RSQ","RSE","AIC","BIC","MSE")

#R-squared
fit$RSQ[1] <-summary(m_1)$adj.r.squared
fit$RSQ[2] <-summary(m_2)$adj.r.squared
fit$RSQ[3] <-summary(m_3)$adj.r.squared
fit$RSQ[4] <-summary(m_4)$adj.r.squared

#RSE
fit$RSE[1] <-summary(m_1)$sigma
fit$RSE[2] <-summary(m_2)$sigma
fit$RSE[3] <-summary(m_3)$sigma
fit$RSE[4] <-summary(m_4)$sigma

#AIC
fit$AIC[1] <-AIC(m_1)
fit$AIC[2] <-AIC(m_2)
fit$AIC[3] <-AIC(m_3)
fit$AIC[4] <-AIC(m_4)

#BIC
fit$BIC[1] <-BIC(m_1)
fit$BIC[2] <-BIC(m_2)
fit$BIC[3] <-BIC(m_3)
fit$BIC[4] <-BIC(m_4)

#MSE through Cross Validation
fit$MSE[1]<-cv.glm(ccpp,glm(PE~.,data=ccpp),K=10)$delta[1]
fit$MSE[2]<-cv.glm(ccpp,glm(PE~.+I(AT^3)+I(AP^3),data=ccpp),K=10)$delta[1]
fit$MSE[3]<-cv.glm(ccpp,glm(PE~.+(AT*AP)+(AT*V)+(AT*RH),data=ccpp),K=10)$delta[1]
fit$MSE[4]<-cv.glm(ccpp,glm(PE~.+(AT*AP)+(AT*V)+(AT*RH)+I(AT^3)+I(AP^3),data=ccpp),K=10)$delta[1]

#printing measure of fit table
print(round(fit,3))

#QQ PLOT OF MODEL 4
par(mfrow=c(1,2))
plot(m_4,2,main ="Model 4")
