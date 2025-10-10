install.packages("readr")
library("readr")
data1<-read_csv("sample_1500.csv")
View(data1) #V는 대문자

#1. 산점도 그리기 (scatter plot)
plot(data1$TOT_CHOLE,data1$BMI)


#정규성검증
# 1) Q-Q plot
qqnorm(data1$TOT_CHOLE)
qqline(data1$TOT_CHOLE)

qqnorm(data1$BMI)
qqline(data1$BMI)

# 2) histogram
hist(data1$TOT_CHOLE)
hist(data1$BMI)

# statistical test
#Shapiro-Wilk test 
# H0: 해당 변수는 정규분포를 따른다, H1: 해당 변수는 정규분포를 따르지 않는다 
shapiro.test(data1$TOT_CHOLE)
shapiro.test(data1$BMI)

#Lilliefors test (수정된 Kolmogorov-Smirnov test)
install.packages("nortest")
library("nortest")
lillie.test(data1$TOT_CHOLE)
lillie.test(data1$BMI)

#BMI는 정규분포, TOT_CHOLE는 약간 skew 되어있고 p값<0.05이지만, 연구에서 정규분포를 따른다고 봐도 무방하다...

#1-2 Pearson's correlation
cor.test(data1$TOT_CHOLE, data1$BMI, method="pearson")
#p-value : 0.0121
#r : -0.06488401

#1-3 단순회귀모형 (linear regression)
LR1<-lm(TOT_CHOLE~BMI, data=data1)
summary(LR1)

# Call:
#   lm(formula = TOT_CHOLE ~ BMI, data = data1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -106.488  -26.192   -1.884   22.266  205.197 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 224.1786     8.5221  26.306   <2e-16 ***
#   BMI          -0.8709     0.3466  -2.512   0.0121 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 38.12 on 1493 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.00421,	Adjusted R-squared:  0.003543 
# F-statistic: 6.312 on 1 and 1493 DF,  p-value: 0.0121












#2. 산점도 그리기 (scatter plot)
plot(data1$edu,data1$BMI)


#정규성검증
# 1) Q-Q plot
qqnorm(data1$edu)
qqline(data1$edu)

qqnorm(data1$BMI)
qqline(data1$BMI)

# 2) histogram
hist(data1$edu)
hist(data1$BMI)
# statistical test
#Shapiro-Wilk test 
# H0: 해당 변수는 정규분포를 따른다, H1: 해당 변수는 정규분포를 따르지 않는다 
shapiro.test(data1$edu)
shapiro.test(data1$BMI)

#Lilliefors test (수정된 Kolmogorov-Smirnov test)
install.packages("nortest")
library("nortest")
lillie.test(data1$edu)
lillie.test(data1$BMI)

#BMI는 정규분포, edu는 따르지 않음

#2-2 Spearman's correlation
cor.test(data1$edu, data1$BMI, method="spearman")
#p-value : 0.6479
#r : -0.0118158 