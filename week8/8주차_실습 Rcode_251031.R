##문제 1##

########[통계적 가설 설정]########
#귀무가설(H0):흡연여부와 만성폐쇄성폐질환 이환은 관련성이 없다(또는 서로 독립이다).
#대립가설(H1): 흡연여부와 만성폐쇄성폐질환 이환은 관련성이 있다(또는 서로 독립이 아니다).

########[변수 결측치 및 분포 확인]########
####(1)워킹 디렉토리 지정


####(2)데이터 불러오기
library(readr)
data1<-read_csv("COPD.csv")


####(3)변수 유형 확인
str(data1$SMK)
str(data1$COPD)

####(4)결측치 확인
table(is.na(data1$SMK)) #흡연여부 결측 수 확인(1)
table(is.na(data1$COPD)) #COPD 결측 수 확인(1)
sum(is.na(data1$SMK)) #흡연여부 결측 수 확인(2)
sum(is.na(data1$COPD)) #COPD 결측 수 확인(2)

####(5)결측치 제거
install.packages("dplyr")
library(dplyr)
data1_1 <- data1 %>% 
  filter(!is.na(SMK) & !is.na(COPD)) #SMK, COPD 결측치 제거

sum(is.na(data1_1$SMK)) #흡연여부 결측 수 재확인
sum(is.na(data1_1$COPD)) #COPD 결측 수 재확인

####(6) 교차표 작성하기 
install.packages("gmodels")
library("gmodels")
CrossTable(data1_1$SMK, data1_1$COPD, prop.chisq=FALSE)

#기대빈도 함께 확인하기 
library("gmodels")
CrossTable(data1_1$SMK, data1_1$COPD, prop.chisq=FALSE, expected=TRUE)

########[가설검정 수행]########

##카이제곱 검정 수행:chisq.test()
chisq.test(data1_1$SMK, data1_1$COPD, correct=FALSE)
chisq.test(data1_1$SMK, data1_1$COPD, correct=FALSE)$expected
#기대빈도<5인 셀이 있을 경우에는 경고가 자동으로 나옴


##카이제곱 검정 수행:CrossTable: 교차표와 카이제곱 검정 결과 한번에 확인하기
library("gmodels")
CrossTable(data1_1$SMK, data1_1$COPD, prop.chisq=FALSE, chisq=TRUE)


##문제 2##

########[통계적 가설 설정]########
#귀무가설(H0):흡연여부와 간암 이환은 관련성이 없다(또는 서로 독립이다).
#대립가설(H1): 흡연여부와 간암 이환은 관련성이 있다(또는 서로 독립이 아니다).

########[변수 결측치 및 분포 확인]########

####(2)데이터 불러오기(csv 파일 불러오기)
library("readr")
data2<-read_csv("sample_1500.csv")

####(3)변수 유형 확인
str(data2$smok)
str(data2$liver_cancer)

####(4)결측치 확인
sum(is.na(data2$smok))
sum(is.na(data2$liver_cancer))

####(5)결측치 제거 - 결측치 없음.

####(6)교차표 작성하기 
library("gmodels")
CrossTable(data2$smok, data2$liver_cancer, prop.chisq=FALSE)

########[가설검정 수행]########
##기대빈도 확인해보기 
chisq.test(data2$smok, data2$liver_cancer, correct=FALSE)$expected
#관찰빈도와 기대빈도 한꺼번에 확인
library("gmodels")
CrossTable(data2$smok, data2$liver_cancer, prop.chisq=FALSE, expected=TRUE)

#카이제곱검정 수행 시 경고메시지 확인해보기
chisq.test(data2$smok, data2$liver_cancer, correct=FALSE)

###############피셔 정확성 검정###############
####(1)fisher.test()
fisher.test(data2$smok, data2$liver_cancer)

####(2).CrossTable 사용하기: fisher=TRUE
library("gmodels")
CrossTable(data2$smok, data2$liver_cancer, prop.chisq=FALSE, fisher=TRUE,expected=TRUE)

