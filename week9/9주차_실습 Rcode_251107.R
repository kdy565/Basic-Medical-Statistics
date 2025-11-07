###############데이터 불러오기###############
#데이터 불러오기 - XLSX(1)
#install.packages("readxl")
library("readxl")
df_logit<-read_excel("logit_OR.xlsx")
#특정Sheet만 불러오기
#read_excel("logit_OR.xlsx", sheet="Sheet1")
#데이터 불러오기 - XLSX(2)
#install.packages("openxlsx")
#library("openxlsx")
#df_logit<-read.xlsx("logit_OR.xlsx")

install.packages("gmodels")
library("gmodels")

###############데이터 파악하기###############
#데이터(변수)속성 확인
str(df_logit)
str(df_logit$SMK) #SMK 변수의 유형 확인
class(df_logit$COPD) #COPD 변수의 유형 확인

#결측치 수 확인
sum(is.na(df_logit))
#범주형 변수로 변환하기
df_logit$SMK<-factor(df_logit$SMK)
df_logit$COPD<-factor(df_logit$COPD)
df_logit$Education<-factor(df_logit$Education) 
df_logit$Sex<-factor(df_logit$Sex)
df_logit$Drink<-factor(df_logit$Drink) 
df_logit$asthma<-factor(df_logit$asthma) 
df_logit$Obesity<-factor(df_logit$Obesity)
df_logit$Fat_meal<-factor(df_logit$Fat_meal)
df_logit$Physical_activity<-as.factor(df_logit$Physical_activity)
str(df_logit)

#as.factor()를 사용해도 결과는 동일함

###############로지스틱 회귀분석###############

#####1.이분형 변수
#glm()사용
LOR1 <- glm(COPD~SMK, data=df_logit, family = binomial) #로지스틱회귀분석 시행 후 LOR1 변수에 저장
summary(LOR1)
exp(coef(LOR1)) #OR
exp(confint.default(LOR1))#CI
LOR1_1<-glm(COPD==0~SMK, data=df_logit, family=binomial) #Event를 지정하기.
summary(LOR1_1)


#lrm()사용
install.packages("rms")
library("rms")

LOR2<-lrm(COPD~SMK, data=df_logit)
LOR2
exp(coef(LOR2)) #OR
exp(confint.default(LOR2))#CI
LOR2_1<-lrm(COPD==0~SMK, data=df_logit) #Event를 지정하기.
LOR2_1

#####2.범주형 변수 (3개 이상의 범주를 갖는 설명변수)
str(df_logit$Education)
LOR3<-glm(COPD~Education, data=df_logit, family = binomial)
summary(LOR3)
exp(coef(LOR3))
exp(confint.default(LOR3))

df_logit$Education<-factor(df_logit$Education, levels=c(2,1,3,4)) #기준 범주를 변경 기준 2
# df_logit$Education <- relevel(df_logit$Education, ref = "2") #동일한 방법
LOR3<-glm(COPD~Education, data=df_logit, family = binomial)
summary(LOR3)
exp(coef(LOR3))
exp(confint.default(LOR3))

df_logit$Education<-factor(df_logit$Education, levels=c(3,1,2,4)) #기준 범주를 변경 기준 3
# df_logit$Education <- relevel(df_logit$Education, ref = "3") #동일한 방법
LOR3<-glm(COPD~Education, data=df_logit, family = binomial)
summary(LOR3)
exp(coef(LOR3))
exp(confint.default(LOR3))

df_logit$Education<-factor(df_logit$Education, levels=c(4,1,2,3)) #기준 범주를 변경 기준 4
# df_logit$Education <- relevel(df_logit$Education, ref = "4") #동일한 방법
LOR3<-glm(COPD~Education, data=df_logit, family = binomial)
summary(LOR3)
exp(coef(LOR3))
exp(confint.default(LOR3))

#####3.교란변수를 포함
#1)교란요인-노출요인, 교란요인-결과요인과의 관련성 파악
#카이제곱검정(교란변수-노출변수)
chisq.test(df_logit$Sex, df_logit$SMK, correct=FALSE)
#카이제곱검정(교란변수-결과변수)
chisq.test(df_logit$Sex, df_logit$COPD, correct=FALSE)
#2)로지스틱회귀분석
LOR4<-glm(COPD~SMK+Sex, data=df_logit, family = binomial)
summary(LOR4)
exp(coef(LOR4))
exp(confint.default(LOR4))


#####4.다변수 로지스틱 회귀분석
#glm()사용 - 연구하고자 하는 설명 변수를 모두 포함하여 분석하기

#1)자체 full model
LOR5<-glm(COPD~SMK+age+Education+BMI+Drink+asthma+Sex,data=df_logit,family=binomial)
summary(LOR5)
exp(coef(LOR5))
exp(confint.default(LOR5))
# exp 하나의 표로 보여주기
OR_result <- cbind(
  OR = exp(coef(LOR5)),
  LCL = exp(confint.default(LOR5))[,1],
  UCL = exp(confint.default(LOR5))[,2]
)
OR_result

#2)통계적으로 유의하지 않은 변수 제외하고 모형 확인 (Education, Drink)
LOR6<-glm(COPD~SMK+age+BMI+asthma+Sex,data=df_logit,family=binomial)
summary(LOR6)
exp(coef(LOR6))
exp(confint.default(LOR6))
# exp 하나의 표로 보여주기
OR_result <- cbind(
  OR = exp(coef(LOR6)),
  LCL = exp(confint.default(LOR6))[,1],
  UCL = exp(confint.default(LOR6))[,2]
)
OR_result


