install.packages("readxl")
library("readxl")
df_logit<-read_excel("logit_OR.xlsx")

install.packages("gmodels")
library("gmodels")

df_logit$Obesity            <- factor(df_logit$Obesity)
df_logit$Physical_activity  <- factor(df_logit$Physical_activity)
df_logit$Fat_meal           <- factor(df_logit$Fat_meal)
df_logit$Drink              <- factor(df_logit$Drink)
df_logit$Sex                <- factor(df_logit$Sex) 
df_logit$Education          <- factor(df_logit$Education) 
df_logit$age <- as.numeric(df_logit$age)

str(df_logit[, c("Obesity","Physical_activity","Fat_meal","Drink","Sex","Education","age")])

LOR5<-glm(Obesity~Physical_activity+age+Fat_meal+Drink+Sex+Education,data=df_logit,family=binomial)
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

#2)통계적으로 유의하지 않은 변수 제외하고 모형 확인 (Education)
LOR6<-glm(Obesity~Physical_activity+age+Fat_meal+Drink+Sex,data=df_logit,family=binomial)
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

