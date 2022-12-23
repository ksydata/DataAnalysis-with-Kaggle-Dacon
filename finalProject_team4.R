### 통계분석실습 통계학과 노호석 교수님 기말 프로젝트


## 은행에서 예금을 수신하기 위한 데이터 기반 상품홍보 전략수립 프로젝트 ####

  # I. 신원미상 고객 제거 : 결측치 줄이기 또는 결측값 대체
  # II. 채무불이행 (yes) 고객 예측 모델 구현 : 예측변수 결정, 혼동행렬(채점표)을 만들 검증용(validation) 데이터 셋을 훈련용(test) 데이터 셋과 분리할 비율 결정 (7:3, 8:2, 9:1)
  # III. 은행 수신상품 가입 여부 marketing(try or defer) 예측 모델 구현 : 분류의 문제
  # IV. 예측 모델을 근거로 은행의 수익성 평가

  # 거절할 것 같은 고객은 보류, 예금하기로 결정할 것 같은 고객에 대해서만 연락)하여 classification을 하는 learner

# 문제 목록
  # 1) 임계값(튜닝 파라미터) 최적화를 위한 반복문 작성
  # 2) 퍼포먼스 측정치의 차이를 반영하여 혼동행렬 커스텀
  # 3) 어떤 머신러닝 모델을 사용할지 비교하여 결정

'''
install.packages(dplyr)
install.packages(forcats)
install.packages(ggplot2)
install.packages(mice)
install.packages(VIM)
install.packages(DMwR)
# Error : to install.packages "Data Mining with R"
# remotes::install_github("cran/DMwR")

install.packages(rsample)
install.packages(caret)
install.packages(vip)
install.packages(car)
install.packages(caTools)
install.packages(rpart)
install.packages(rpart.plot)
install.packages(corrplot)
install.packages(RColorBrewer)
install.packages(rattle)

install.packages(rsample)
install.packages(ipred)
install.packages(randomForest)
install.packages(mlbench)
install.packages(dunn.test)
install.packages(forcats)
install.packages(HH)
install.packages(NbClust)
install.packages(factoextra)
'''

library(dplyr)
library(forcats)
library(ggplot2)
library(mice)
library(VIM)
library(DMwR)

library(rsample)
library(caret)
library(vip)
library(car)
library(caTools)
library(rpart)
library(rpart.plot)
library(corrplot)
library(RColorBrewer)
library(rattle)
library(rsample)
library(ipred)
library(randomForest)
library(mlbench)
library(dunn.test)
library(forcats)
library(HH)
library(NbClust)
library(factoextra)
library(MLmetrics)
library(bnlearn)
library(e1071)



getwd()
setwd("C:/PDSR/RforStatistics_FinalProject")
# 파일경로
load("data_train.Rda")

{
  Codebook = data.frame(
    features = colnames(data_train), 
    descripts = c(
      "나이", "type of job", "marital status", "교육수준", 
      
      "채무불이행 여부", "주택대출이 있는지 여부", "개인대출이 있는지 여부", 
      
      "telemarketing 수단", "마지막으로 진행한 contact했을 때의 월", "마지막으로 진행한 contact의 요일", "이번 홍보에서 해당 고객을 contact한 횟수", "지난 홍보와 관련하여 해당 고객을 contact하고 지나간 날수", "이번 홍보 이전에 해당 고객을 contact한 횟수", "이전 홍보로 해당 고객을 contact했을 때 마케팅 성공여부",
      
      "고용변동율", "소비자물가지수", "소비자신뢰지수", 
      "has the client subscribed a term deposit? (binary: 'yes','no')")
  )}



# 1. 데이터 전처리 ####

# 1.1 결측값 탐색 ####

# 신원 정보
str(data_train)
summary(data_train$age)
ggplot(data_train, aes(age)) + geom_histogram(fill = "lightgreen", col = "black")
boxplot(data_train$age)$stat


table(data_train$job)
ggplot(data_train, aes(x = job, fill = job)) + geom_bar() + scale_fill_brewer(palette = "Set3")
  # unknown (34)
table(data_train$marital)
ggplot(data_train, aes(x = marital, fill = marital)) + geom_bar() + scale_fill_brewer(palette = "Paired")
  # unknown (14)
table (data_train$education)
ggplot(data_train, aes(x = education, fill = education)) + geom_bar() + scale_fill_brewer(palette = "Spectral")
  # unknown (158)


# 신용 정보
table(data_train$default) 
  # 740명 결측 (unknown)
table(data_train$housing) 
  # 96명 결측 (unknown)
table(data_train$loan) 
  # 96명 결측 (unknown)

which(data_train$default == "unknown" 
      & data_train$housing == "unknown" 
      & data_train$loan == "unknown") 
  # 16명 결측 (3개 열(변수)에 공통으로 결측된 행 위치)

which(data_train$housing == "unknown" 
      & data_train$loan == "unknown") 
  # 96명 결측 (2개 열(변수)에 공통으로 결측된 행 위치)


# 은행 마케팅 정보
table(data_train$pdays) 
table(data_train$pdays == 0)
data_train[which(data_train$pdays == 0), ]
  # 3731명 결측 (999) / 2명은 0일 경과 했으며(8월의 수요일) 상품 가입한 고객
table(data_train$poutcome)
table(data_train$poutcome == 0)
table(data_train$previous)
  # 3348명 결측 (nonexistent)

length(which(data_train$pdays == 999
             & data_train$poutcome == "nonexistent"))
length(which(data_train$previous == 0
             & data_train$poutcome == "nonexistent"))
  # 3348명 (2개 열(변수)에 공통으로 결측된 행 위치)
  # 컨택한 적 없어, 마케팅 성패 없고, 마지막 컨택 후 경과일도 없는 고객



# 1.2. 결측값 처리 및 EDA ####

levels(data_train$job)[levels(data_train$job) == "unknown"] <- NA
levels(data_train$marital)[levels(data_train$marital) == "unknown"] <- NA
levels(data_train$education)[levels(data_train$education) == "unknown"] <- NA

levels(data_train$housing)[levels(data_train$housing) == "unknown"] <- NA
levels(data_train$loan)[levels(data_train$loan) == "unknown"] <- NA
table(is.na(data_train$housing))
table(is.na(data_train$loan))

levels(data_train$poutcome)[levels(data_train$poutcome) == "nonexistent"] <- 0
table(data_train$poutcome)
table(data_train$previous)
data_train$pdays <- ifelse(data_train$pdays == 999, NA, data_train$pdays)
table(is.na(data_train$pdays))


# 1.2.1. 여신 서비스 이용 NA ####

length(which(is.na(data_train$loan) & data_train$default == "unknown"))
  # 대출 여부 알 수 없는 96명 중 80명은 채무불이행 없으며, 나머지 16명은 알 수 없음

table(data_train$default)
  # unknown 740 "yes" or "no" 예측
  # 은행의 전통적 수익구조 : 예대마진은 최종 비즈니스 목표
  # 수신상품 (예금) : 현금이 없으면 향후 중간에 해지 : 가입을 할지 여부만큼 중요한 정보
  # 채무불이행 했던 고객, 신용대출과 주택담보대출을 동시에 받아 유동성이 낮은 고객은 제거 여지 有

table(data_train$housing == "yes" & data_train$loan == "yes")
# 344명은 주택담보대출과 신용대출을 동시에 받은 고객
table(data_train$housing == "yes" & data_train$loan == "yes" & data_train$default == "no")
  # 278명은 대출 이력 있으나 채무불이행 이력 없는 고객

# ** 278명 중 과거 컨택 안했으나 자발적으로 상품 가입한 고객 49명 탐색 **
data_yesdebt <- data_train[which(data_train$housing == "yes" & data_train$loan == "yes" & data_train$default == "no"), ]
data_yesdebt %>% filter(y == "yes") %>% group_by(pdays, previous, poutcome) %>% summarise(count = n())

data_yesdebt %>% filter(y == "yes" & poutcome == 0) %>% group_by(campaign) %>% summarise(count = n())
  # 해당 49명에 대한 이번 상품 마케팅 횟수
print(data_yesdebt %>% filter(y == "yes" & poutcome == 0) %>% group_by(campaign, marital, job) %>% summarise(count = n()), n = 29)
print(data_yesdebt %>% filter(y == "yes" & poutcome == 0) %>% group_by(marital, job) %>% summarise(count = n()), n = 29)
  # (married, admin) (married, blue-collar), (married, retired), (married, technician)
  # (single, admin)



# ** 채무불이행 여부 알 수 없는 고객 특성 탐색(대출 이력 있는 66명 대상) **
table(data_train$housing == "yes" & data_train$loan == "yes" & data_train$default == "unknown")
data_nulldebt <- data_train[which(is.na(data_train$housing) & is.na(data_train$loan)), ]

data_nulldebt %>% filter(y == "yes") %>% group_by(previous, poutcome) %>% summarise(count = n())
  # 상품 가입 고객 중 마케팅 안한 고객 비중 높음 (14/19)
data_nulldebt %>% filter(y == "yes") %>% group_by(marital) %>% summarise(count = n())
  # married 비중 높음
data_nulldebt %>% filter(y == "yes") %>% group_by(job) %>% summarise(count = n())
  # admin, blue-collar, technician

data_nulldebt %>% filter(y == "no") %>% group_by(marital) %>% summarise(count = n())
  # married, single 비중 높음
data_nulldebt %>% filter(y == "no") %>% group_by(job) %>% summarise(count = n())
  # blue-collar, admin, technician


# 1.2.2. 이전 은행수신상품(정기예금) 마케팅 NA #### 

length(which(data_train$previous == 0 & data_train$poutcome == 0))
  # 과거 컨택한 적 없어 마케팅 성패 없는 고객 3348명
length(which(is.na(data_train$pdays) & data_train$poutcome != 0 & data_train$previous != 0))
  # 과거 컨택하여 마케팅 성패는 있지만 마지막 컨택 후 경과일은 없는 고객 383명 탐색

data_pcontact <- data_train[which(is.na(data_train$pdays) & data_train$poutcome != 0 & data_train$previous != 0), ]
table(data_pcontact$poutcome == "failure" & data_pcontact$y == "no")
table(data_pcontact$poutcome == "failure" & data_pcontact$y == "yes")
  # 고객 383명 모두 과거 마케팅에 실패하였으나, 정기예금 가입한 고객은 113명
  # ** 113명은 컨택/마케팅 없이 자발적으로 정기예금에 가입한 고객(이하 '자기주도형') **

data_pcontact %>% filter(y == "yes") %>% group_by(default, housing) %>% summarise(nhousing = n())
data_pcontact %>% filter(y == "yes") %>% group_by(default, loan) %>% summarise(nloan = n())
  # ** 자기주도형, 연체 없는 고객 특성 : housing은 42:62, loan은 97:7 (no:yes) **


# 1.3. 데이터 시각화 및 결측값 처리(제거 또는 대체) ####

# 제거 가능한 신원미상 고객 탐색

mice_plot <- aggr(data_train, col = c("grey", "blue"), numbers = TRUE, sortVars = TRUE, labels = names(data_train), cex.axis = 0.7, gap = 3, ylab = c("결측치", "패턴"))

data_train[which(is.na(data_train$job) & is.na(data_train$marital)), ]
  # job & marital 둘다 결측 

table(is.na(data_train$job))
table(is.na(data_train$marital))
table(data_train$education)
data_train[which(is.na(data_train$job) & is.na(data_train$marital)), ]
  # job & education 둘다 결측
data_train[which(is.na(data_train$job) & is.na(data_train$marital)), ] %>% filter(y == "no")

data_train[which(is.na(data_train$marital) & is.na(data_train$education)), ]
  # marital & education 둘다 결측 
data_train[which(is.na(data_train$marital) & is.na(data_train$education)), ] %>% filter(y == "no")

data_train[which(is.na(data_train$job) & is.na(data_train$education)), ]
  # job & education 둘다 결측
data_train[which(is.na(data_train$job) & is.na(data_train$education)), ] %>% filter(y == "no")

# 제거 가능한 신원미상 고객 제거 -> 취소

'''
which(is.na(data_train$job) & is.na(data_train$education) & data_train$y == "no")
data_train <- data_train[-c(1511, 1656, 2043, 2717, 3003, 3013, 3051, 3863), ]
which(is.na(data_train$marital) & is.na(data_train$education) & data_train$y == "no")
data_train <- data_train[-2270, ]
which(is.na(data_train$job) & is.na(data_train$marital) & data_train$y == "no")
data_train <- data_train[-c(2831, 3599), ]
'''

# 1.4. 이상치 처리 ####

boxplot(data_train$emp.var.rate)$stat
  # 0
boxplot(data_train$cons.price.idx)$stat
  # 0
boxplot(data_train$cons.conf.idx)$stat
  # outlier
summary(data_train$cons.conf.idx)
which(data_train$cons.conf.idx > summary(data_train$cons.conf.idx)[5] + 1.5*IQR(data_train$cons.conf.idx)) 
  # 55개
  # 소비자 신뢰지수가 높을수록 은행 수신상품 가입할 여지가 있다고 판단





# 1.5. 상관분석 및 카이제곱 검정 ####

names(data_train)
  # numeric
cor_trn <- cor(data_train[, c(1, 11, 12, 13, 16, 17)], use = "pairwise.complete.obs")
cor_trn <- round(cor_trn, 2)
corrplot(cor_trn,
         method = "number",
         type = "lower",
         order = "hclust",
         # 유사한 상관계수끼리 군집화
         addCofe.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F
         # 대각 행렬 제외
)

  # factor : [, c(2,3,4,5,6,7,8,9,10,14,18)]
  # NA's : [, c(2,3,4,6,7)]
  # default를 예측하기 위한 두 범주형 변수의 카이제곱 검정

str(data_train)

# 1.5.1. job, marital, education, y ####
freq2.3 <- table(data_train[,2], data_train[,3])
proportions(freq2.3, margin = 1)
chisq.test(freq2.3)

freq2.4 <- table(data_train[,2], data_train[,4])
proportions(freq2.4, margin = 1)
chisq.test(freq2.4)

freq2.6 <- table(data_train[,2], data_train[,6])
proportions(freq2.6, margin = 1)
chisq.test(freq2.6)
  # p-value = 0.7267

freq2.7 <- table(data_train[,2], data_train[,7])
proportions(freq2.7, margin = 1)
chisq.test(freq2.7)
  # p-value = 0.1792

freq2.18 <- table(data_train[,2], data_train[,18])
proportions(freq2.18, margin = 1)
chisq.test(freq2.18)

freq3.18 <- table(data_train[,3], data_train[,18])
proportions(freq3.18, margin = 1)
chisq.test(freq3.18)

freq4.18 <- table(data_train[,4], data_train[,18])
proportions(freq4.18, margin = 1)
chisq.test(freq4.18)



# 1.5.2. housing :loan, contact / loan : housing ####
freq6.7 <- table(data_train[,6], data_train[,7])
proportions(freq6.7, margin = 1)
chisq.test(freq6.7)

freq6.14 <- table(data_train[,6], data_train[,14])
proportions(freq6.14, margin = 1)
chisq.test(freq6.14)
  # p-value = 0.4464

freq7.14 <- table(data_train[,7], data_train[,14])
proportions(freq7.14, margin = 1)
chisq.test(freq7.14)
  # p-value = 0.5626

freq6.8 <- table(data_train[,6], data_train[,8])
proportions(freq6.8, margin = 1)
chisq.test(freq6.8)

freq7.8 <- table(data_train[,7], data_train[,8])
proportions(freq7.8, margin = 1)
chisq.test(freq7.8)
  # p-value = 1

freq6.18 <- table(data_train[,6], data_train[,18])
proportions(freq6.18, margin = 1)
chisq.test(freq6.18)
  # p-value = 0.4361

freq7.18 <- table(data_train[,7], data_train[,18])
proportions(freq7.18, margin = 1)
chisq.test(freq7.18)
  # p-value = 0.5083



# 1.5.3. 결측치 대체 ####

job_mod <- mice(data_train[, c(2, 3, 4, 18)], method = "rf", printFlag = F)
job_mod <- complete(job_mod)
table(job_mod$job)
data_train$job <- job_mod$job
data_train$marital <- job_mod$marital
data_train$education <- job_mod$education
summary(data_train)

housing_mod <- mice(data_train[, c(6, 7, 8)], method = "rf", printFlag = F)
housing_mod <- complete(housing_mod)
table(housing_mod$housing)
table(housing_mod$loan)
data_train$housing <- housing_mod$housing
summary(data_train$housing)

loan_mod <- mice(data_train[, c(7, 6)], method = "rf", printFlag = F)
loan_mod <- complete(loan_mod)
table(loan_mod$loan)
data_train$loan <- loan_mod$loan
summary(data_train$loan)

mice_plot <- aggr(data_train, col = c("grey", "blue"), numbers = TRUE, sortVars = TRUE, labels = names(data_train), cex.axis = 0.7, gap = 3, ylab = c("결측치", "패턴"))



# 1.5.4. 범주형 파생변수 생성 후 가설검정 ####

data_train <- data_train %>% mutate(
  generation = case_when(age <= 19 ~ "teen", age <= 32 ~ "youth", age <= 38 ~ "midlife", age <= 47 ~ "senior", age <= 69 ~ "yold", age <= 92 ~ "old"))
data_train$generation <- factor(
  data_train$generation, 
  levels = c("teen", "youth", "midlife", "senior", "yold", "old"))
table(data_train$generation)
ggplot(data = data_train, aes(x = generation, fill = generation)) + geom_bar() + scale_fill_brewer(palette = "Pastel2")

# Trial and Error 1 (일원분산분석) ####

# 정규성 검토 (Ha 채택, 조건 불충족)
# 등분산성 검정 (H0 채택, 조건 충족)

hist((data_train$campaign)[levels(data_train$generation) == "teen"])
hist((data_train$campaign)[levels(data_train$generation) == "youth"])
hist((data_train$campaign)[levels(data_train$generation) == "midlife"])
hist((data_train$campaign)[levels(data_train$generation) == "senior"])
hist((data_train$campaign)[levels(data_train$generation) == "yold"])
hist((data_train$campaign)[levels(data_train$generation) == "old"])

shapiro.test((data_train$campaign)[levels(data_train$generation) == "youth"])

leveneTest(campaign ~ generation, data = data_train)
# 2P-value = 0.2547

# 등분산성 조건 충족 전제한 일원분산분석 
# (H0 채택, 연령대(gen) 집단 간 campaign 표본평균의 차이는 통계적으로 유의하지 않다는 결과)
anova <- aov(campaign ~ generation, data = data_train)
summary(anova)
# 2P-value = 0.211


# Trial and Error 2 (이원분산분석) ####

# 정규성 검토 (Ha 채택, 조건 불충족)
# 집단 간 등분산성 검정 (Ha 채택, 조건 불충족)

hist((data_train$previous)[levels(data_train$generation) == "teen"])
hist((data_train$previous)[levels(data_train$generation) == "youth"])
hist((data_train$previous)[levels(data_train$generation) == "midlife"])
hist((data_train$previous)[levels(data_train$generation) == "senior"])
hist((data_train$previous)[levels(data_train$generation) == "yold"])
hist((data_train$previous)[levels(data_train$generation) == "old"])

shapiro.test((data_train$previous)[levels(data_train$generation) == "youth"])

leveneTest(previous ~ generation, data = data_train)

# 이분산성 전제한 일원분산분석 (Welch's ANOVA)
# (Ha 채택, 연령대(gen) 집단 간 campaign 표본평균의 차이는 통계적으로 유의하다는 결과)
oneway.test(previous ~ generation, data = data_train, var.equal = FALSE)

# 사후분석 (Bonferroni Correction)
# old Mu = teen Mu > youth Mu = midlife Mu = senior Mu = yold Mu
# 초고령과 고등학생 세대의 과거 상품 마케팅 건수 모평균은 청년, 중년, 장년, 고령 세대의 모평균보다 큰 값을 가지는 결과

dunn.test(data_train$previous, data_train$generation, method = "bonferroni")

# IV1(gen)와 DV(previous)에 대하여 IV2(job / marital / education / housing / loan)이 상호작용효과가 있는지(Ha) 이원분산분석 수행
# (Ha 채택, marital / education / housing)

data_train %>% group_by(generation) %>% summarise(mean = mean(previous)) %>% arrange(mean)
data_train$generation <- factor(
  data_train$generation, 
  levels = c("senior", "youth", "midlife", "yold", "teen", "old"))
# 일원분산분석에서 표본평균 가장 작은 순서로 집단 먼저 출력되도록 함


# 직업 변수는 세대 집단별 과거 캠페인 횟수 모평균 차이를 강화하는 상호작용효과가 없다

tanova_j <- aov(previous ~ generation * job, data = data_train)
summary(tanova_j)
# print(model.tables(tanova_j, "means"), digits = 2)
interaction.plot(data_train$generation, data_train$job, data_train$previous, bty='l', main = "interaction effect plot between Gen and Job", col = c(1:11))
# interaction2wt(previous ~ generation * job, data = data_train)


# 가족유형 변수는 세대 집단별 과거 캠페인 횟수 모평균 차이를 강화하는 상호작용효과가 있다

tanova_m <- aov(previous ~ generation * marital, data = data_train)
summary(tanova_m)
interaction.plot(data_train$generation, data_train$marital, data_train$previous, bty='l', main = "interaction effect plot between Gen and Marital", col = c(1:4))


# 교육수준 변수는 세대 집단별 과거 캠페인 횟수 모평균 차이를 강화하는 상호작용효과가 있다

tanova_e <- aov(previous ~ generation * education, data = data_train)
summary(tanova_e)
interaction.plot(data_train$generation, data_train$education, data_train$previous, bty='l', main = "interaction effect plot between Gen and Edu", col = c(1:7))


# 주택담보대출 변수는 세대 집단별 과거 캠페인 횟수 모평균 차이를 강화하는 상호작용효과가 있다

tanova_h <- aov(previous ~ generation * housing, data = data_train)
summary(tanova_h)
interaction.plot(data_train$generation, data_train$housing, data_train$previous, bty='l', main = "interaction effect plot between Gen and housing", col = c(1:2))

# 신용대출 변수는 세대 집단별 과거 캠페인 횟수 모평균 차이를 강화하는 상호작용효과가 없다

tanova_l <- aov(previous ~ generation * loan, data = data_train)
summary(tanova_l)
interaction.plot(data_train$generation, data_train$loan, data_train$previous, bty='l', main = "interaction effect plot between Gen and loan", col = c(1:2))


# Trial and Error 3 (카이제곱 독립성 검정 chi-squared) ####

py <- table(data_train$generation, data_train$poutcome)
cy <- table(data_train$generation, data_train$y)
prop.table(py, margin = 1)
prop.table(cy, margin = 1)

chisq.test(py)
# 세대 변수는 과거 마케팅 성공 여부에 영향을 미친다
chisq.test(cy)
# 세대 변수는 현재 정기예금 가입 여부에 영향을 미친다. 

py_df <- data_train %>% group_by(generation, poutcome) %>% summarise(count = n())
ggplot(data = py_df, aes(x = generation, y = count, fill = generation)) + geom_col() + scale_fill_brewer(palette = "YlGnBu") + facet_wrap(~poutcome) + geom_text(aes(label = count), position = position_dodge(width = 1)) + ggtitle("과거 은행 상품마케팅 성패에 따른 세대별 고객 빈도")

cy_df <- data_train %>% group_by(generation, y) %>% summarise(count = n())
ggplot(data = cy_df, aes(x = generation, y = count, fill = generation)) + geom_col() + scale_fill_brewer(palette = "Reds") + facet_wrap(~y) + geom_text(aes(label = count), position = position_dodge(width = 1)) + ggtitle("현재 은행 정기예금 가입에 따른 세대별 고객 빈도")



# 2.1. test 데이터 전처리 ####

setwd("C:/PDSR/")
test <- read.csv("test.csv", header = TRUE, na = NA, sep = ",")
for (i in c(3:11, 15, 19, 20)) {
  train[, i] <- as.factor(train[, i])
}

test$job <- ifelse(test$job == "unknown", NA, test$job)
test$marital <- ifelse(test$marital == "unknown", NA, test$marital)
test$education<- ifelse(test$education == "unknown", NA, test$education)
test$housing <- ifelse(test$housing == "unknown", NA, test$housing)
test$loan <- ifelse(test$loan == "unknown", NA, test$loan)
test$poutcome <- ifelse(test$poutcome == "nonexistent", 0, test$poutcome)
test$pdays <- ifelse(test$pdays == 999, NA, test$pdays)


str(test)
for (i in c(3:11, 15, 19)) {
  test[, i] <- as.factor(test[, i])
}



# 2.2. 결측치 대체 ####

job_mod <- mice(test[, c(3, 4, 5)], method = "rf", printFlag = F)
job_mod <- complete(job_mod)
table(job_mod$job)
test$job <- job_mod$job
test$marital <- job_mod$marital
test$education <- job_mod$education
summary(test)

housing_mod <- mice(test[, c(7, 8, 9)], method = "rf", printFlag = F)
housing_mod <- complete(housing_mod)
table(housing_mod$housing)
table(housing_mod$loan)
test$housing <- housing_mod$housing
summary(test$housing)

loan_mod <- mice(test[, c(8, 7)], method = "rf", printFlag = F)
loan_mod <- complete(loan_mod)
table(loan_mod$loan)
test$loan <- loan_mod$loan
summary(test$loan)

mice_plot <- aggr(test, col = c("grey", "blue"), numbers = TRUE, sortVars = TRUE, labels = names(test), cex.axis = 0.7, gap = 3, ylab = c("결측치", "패턴"))

test <- test %>% mutate(generation = case_when(age <= 19 ~ "teen", age <= 32 ~ "youth", age <= 38 ~ "midlife", age <= 47 ~ "senior", age <= 69 ~ "yold", age <= 98 ~ "old"))
test$generation <- factor(test$generation, levels = c("teen", "youth", "midlife", "senior", "yold", "old"))

max(data_train$age)
max(test$age)

table(is.na(test$generation))
summary(test$generation)
test[is.na(test$generation), ]




# cleaning ####

rm(anova)
rm(cor_trn)
rm(cy)
rm(py)
rm(cy_df)
rm(py_df)
rm(data_nulldebt)
rm(data_pcontact)
rm(data_yesdebt)
rm(housing_mod)
rm(job_mod)
rm(loan_mod)
rm(mice_plot)
rm(tanova_e)
rm(tanova_h)
rm(tanova_j)
rm(tanova_l)
rm(tanova_m)

rm(freq2.18)
rm(freq2.3)
rm(freq2.4)
rm(freq2.6)
rm(freq2.7)
rm(freq3.18)
rm(freq4.18)
rm(freq6.14)
rm(freq6.18)
rm(freq7.8)
rm(freq7.14)
rm(freq7.18)
rm(freq6.7)
rm(freq6.8)

'''
columns <- data.frame(
  train = colnames(data_train),
  test = c(colnames(test), "x", "x")
)
'''

rm(columns)

write.csv(data_train, file = "train_SAP.csv")
write.csv(test, file = "test_SAP.csv")



# 2.3. 전처리 완료된 데이터 로드 ####

setwd("C:/PDSR/")
train <- read.csv("train_SAP.csv", header = TRUE, na = NA)
train$X <- NULL
train$y <- as.factor(train$y)

table(train$y)
train$y <- as.factor(train$y)
table(train$y)

# [Trial & Error 1] 
# train$y <- factor(train$y, levels = c("0", "1"))

# [Trial & Error 2]
# train[train$y == "no", ] <- "0"
# train[train$y == "yes", ] <- "1"

# [Trial & Error 3]
# train$y <- as.integer(train$y)
table(train$y)
train$y <- as.factor(train$y)


split <- createDataPartition(train$y, p = 0.8, list = FALSE)
training <- train[split, ]
validation <- train[-split, ]

test <- read.csv("test_SAP.csv", header = TRUE, na = NA)
test$X <- NULL



# 3.1. Machine Learning : logistic regression ####

set.seed(12345)
logit <- glm(y ~ ., data = training[, -12], family = "binomial")
summary(logit)
logit_pred <- predict(logit, newdata = validation[, -12], type = "response")
summary(logit_pred)
boxplot(logit_pred)$stat
# logit$fitted.values

str(training$y)
str(validation$y)
str(logit_pred)


# 3.2. [mis-classification error?] (TN + FN) / (TP + TN + FP + FN) ####
# 0*FP -3*FN -2*TN + (6-1)*TP



perform <- data.frame(threshold = seq(0.05, 0.95, 0.01), performance_measure = 0)
threshold <- seq(0.05, 0.95, 0.01)
# [class 분류를 위한 임계값 수열]
i = 0
# [threshold별 performance measure값의 행 번호 생성] 
validation[, 20:120] <- 0
# [빈 공간 벡터 생성]

for (c in threshold) {
  i <- i + 1
  validation[, i + 19] <- ifelse(logit_pred >= c, "yes", "no")
  
  # Error in `[<-.data.frame`(`*tmp*`, , i + 19, value = c(`1` = "yes", `8` = "yes", : new columns would leave holes after existing columns #
  
  validation[, i + 19] <- as.factor(validation[, i + 19])
  
  perform[i, 2] <- c(
    5*sum((validation[, i + 19] == "yes") & (validation$y == "yes")) 
    -2*sum((validation[, i + 19] == "yes") & (validation$y == "no")) 
    -3*sum((validation[, i + 19] == "no") & (validation$y == "yes"))
    +0*sum((validation[, i + 19] == "no" & validation$y == "no"))
  )
}

par(mfrow = c(2,2))

ggplot(data = perform, aes(x = threshold, y = performance_measure)) + geom_line(col = "blue") + ggtitle("performance measure of glm with threshold")
# [performance_measure가 최대인 threshold] logistic reg = 0.16, 292
# [f1 score가 최대인 threshold] 0.35

ggplot(data = perform2, aes(x = threshold, y = performance_measure)) + geom_line(col = "darkgreen") + ggtitle("accuracy of glm with threshold")
# [accuracy가 최대인 threshold] [0.38, 0.48] 664


perform2 <- data.frame(threshold = seq(0.05, 0.95, 0.01), performance_measure = 0)
threshold <- seq(0.05, 0.95, 0.01)
i = 0
validation[, 20:120] <- NULL
validation[, 20:120] <- 0

for (c in threshold) {
  i <- i + 1
  validation[, i + 19] <- ifelse(logit_pred >= c, "yes", "no")
  
  # Error in `[<-.data.frame`(`*tmp*`, , i + 19, value = c(`1` = "yes", `8` = "yes", : new columns would leave holes after existing columns #
  
  validation[, i + 19] <- as.factor(validation[, i + 19])
  
  perform2[i, 2] <- c(
    1*sum((validation[, i + 19] == "yes") & (validation$y == "yes")) 
    +0*sum((validation[, i + 19] == "yes") & (validation$y == "no")) 
    +0*sum((validation[, i + 19] == "no") & (validation$y == "yes"))
    +1*sum((validation[, i + 19] == "no" & validation$y == "no"))
  )
}



# 3.3. glm : submission.csv ####

logit_test <- glm(y ~ ., data = train[, -13], family = "binomial")
logit_yhat <- predict(logit, newdata = train, type = "response")
logit_yhat <- ifelse(logit_yhat >= 0.16, "yes", "no")
logit_yhat <- as.factor(logit_yhat)
test$yhat <- logit_yhat
table(test$yhat)

# submission <- test[, c("ID", "yhat")]
# write.csv(submission, "submission.csv")



# 4. Machine Learning : tree model ####

set.seed(12345)
tree <- rpart(y ~ ., data = training[, -12], method = "class", control = rpart.control(cp = 0))
# summary(tree)

par(mfrow = c(1,1))
rpart.plot(tree)
tree$cptable
# cp [0.00875] [0.00375]

tree <- rpart(y ~ ., data = training[, -12], method = "class", control = rpart.control(cp = 0.00875))
rpart.plot(tree)

tree <- rpart(y ~ ., data = training[, -12], method = "class", control = rpart.control(cp = 0.00375))
rpart.plot(tree)

tree_pred <- predict(tree, newdata = validation, method = "response")
tree_pred <- data.frame(tree_pred)

perform <- data.frame(threshold = seq(0.05, 0.95, 0.01), performance_measure = 0)
threshold <- seq(0.05, 0.95, 0.01)
i = 0
validation[, 20:120] <- 0

for (c in threshold) {
  i <- i + 1
  validation[, i + 19] <- ifelse(tree_pred$yes >= c, "yes", "no")
  validation[, i + 19] <- as.factor(validation[, i + 19])
  
  perform[i, 2] <- c(
    5*sum((validation[, i + 19] == "yes") & (validation$y == "yes")) 
    -2*sum((validation[, i + 19] == "yes") & (validation$y == "no")) 
    -3*sum((validation[, i + 19] == "no") & (validation$y == "yes"))
    +0*sum((validation[, i + 19] == "no" & validation$y == "no"))
  )
}

perform2 <- data.frame(threshold = seq(0.05, 0.95, 0.01), performance_measure = 0)
threshold <- seq(0.05, 0.95, 0.01)
i = 0
validation[, 20:120] <- NULL
validation[, 20:120] <- 0

for (c in threshold) {
  i <- i + 1
  validation[, i + 19] <- ifelse(tree_pred$yes >= c, "yes", "no")
  validation[, i + 19] <- as.factor(validation[, i + 19])
  
  perform2[i, 2] <- c(
    1*sum((validation[, i + 19] == "yes") & (validation$y == "yes")) 
    +0*sum((validation[, i + 19] == "yes") & (validation$y == "no")) 
    +0*sum((validation[, i + 19] == "no") & (validation$y == "yes"))
    +1*sum((validation[, i + 19] == "no" & validation$y == "no"))
  )
}

ggplot(data = perform, aes(x = threshold, y = performance_measure)) + geom_line(col = "blue") + ggtitle("performance measure of decision tree with threshold")
# [performance_measure가 최대인 threshold] decision tree 
# cp = 0.00875 [0.12, 0.18] : 228
# cp = 0.00375 [0.11, 0.14] : 126

ggplot(data = perform2, aes(x = threshold, y = performance_measure)) + geom_line(col = "orange") + ggtitle("accuracy of decision tree with threshold")
# [accuracy가 최대인 threshold] 
# cp = 0.00875 [0.24, 0.35] : 654
# cp = 0.00375 [0.22, 0.27] : 639



# 5.1. Machine Learning : bagging ####

set.seed(12345)
bagging <- bagging(y ~ ., data = training[, -12], method = "class", nbagg = 500, control = rpart.control(cp = 0.00875))

bagging_pred <- predict(bagging, newdata = validation, type = "prob")
bagging_pred <- data.frame(bagging_pred)

perform <- data.frame(threshold = seq(0.05, 0.95, 0.01), performance_measure = 0)
threshold <- seq(0.05, 0.95, 0.01)
i = 0
validation[, 20:120] <- 0

for (c in threshold) {
  i <- i + 1
  validation[, i + 19] <- ifelse(bagging_pred$yes >= c, "yes", "no")
  validation[, i + 19] <- as.factor(validation[, i + 19])
  
  perform[i, 2] <- c(
    5*sum((validation[, i + 19] == "yes") & (validation$y == "yes")) 
    -2*sum((validation[, i + 19] == "yes") & (validation$y == "no")) 
    -3*sum((validation[, i + 19] == "no") & (validation$y == "yes"))
    +0*sum((validation[, i + 19] == "no" & validation$y == "no"))
  )
}

perform2 <- data.frame(threshold = seq(0.05, 0.95, 0.01), performance_measure = 0)
threshold <- seq(0.05, 0.95, 0.01)
i = 0
validation[, 20:120] <- NULL
validation[, 20:120] <- 0

for (c in threshold) {
  i <- i + 1
  validation[, i + 19] <- ifelse(bagging_pred$yes >= c, "yes", "no")
  validation[, i + 19] <- as.factor(validation[, i + 19])
  
  perform2[i, 2] <- c(
    1*sum((validation[, i + 19] == "yes") & (validation$y == "yes")) 
    +0*sum((validation[, i + 19] == "yes") & (validation$y == "no")) 
    +0*sum((validation[, i + 19] == "no") & (validation$y == "yes"))
    +1*sum((validation[, i + 19] == "no" & validation$y == "no"))
  )
}


ggplot(data = perform, aes(x = threshold, y = performance_measure)) + geom_line(col = "darkblue") + ggtitle("performance measure of bagging with threshold")
# [performance_measure가 최대인 threshold] bagging (cp = 0.00875)
# nbagg = 500 0.17 : 250 
# nbagg = 100 0.17 : 206
# nbagg = 1000 

ggplot(data = perform2, aes(x = threshold, y = performance_measure)) + geom_line(col = "gold") + ggtitle("accuracy of bagging with threshold")
# [accuracy가 최대인 threshold] bagging (cp = 0.00875)
# nbagg = 500 [0.32, 0.33] : 659
# nbagg = 100 [0.94, 0.95] : 650



# 5.2. bagging : submission.csv ####

bagging <- bagging(y ~ ., data = train[, -12], method = "class", nbagg = 500, control = rpart.control(cp = 0.00875))
bg_yhat <- predict(bagging, newdata = test, type = "prob")
bg_yhat <- data.frame(bg_yhat)
bg_yhat$yes <- ifelse(bg_yhat$yes >= 0.25, "yes", "no")
bg_yhat$yes <- as.factor(bg_yhat$yes)
test$yhat <- bg_yhat$yes
table(test$yhat)
table(is.na(test$yhat))
submission_bg <- test[, c("ID", "yhat")]
# write.csv(submission_bg, "submission_bg.csv", row.names = FALSE)



# 6.1. Machine Learning : Random Forest ####

set.seed(12345)
randomforest <- randomForest(y ~ ., data = training[, -12], method = "class", ntree = 500, mtry = 4)
# [ntree] 부트스트랩 표본 수 100개로 설정
# [mtry] 가지치기에 사용될 변수의 수 4개로 설정
# 최적의 수 sqrt(18) = 4.242641

rf_pred <- predict(randomforest, newdata = validation[, -12], type = "prob")
rf_pred <- data.frame(rf_pred)

perform <- data.frame(threshold = seq(0.05, 0.95, 0.05), performance_measure = 0)
threshold <- seq(0.05, 0.95, 0.05)
i = 0
validation[, 20:200] <- 0

for (c in threshold) {
  i <- i + 1
  validation[, i + 19] <- ifelse(rf_pred$yes >= c, "yes", "no")
  validation[, i + 19] <- as.factor(validation[, i + 19])
  
  perform[i, 2] <- c(
    5*sum((validation[, i + 19] == "yes") & (validation$y == "yes")) 
    -2*sum((validation[, i + 19] == "yes") & (validation$y == "no")) 
    -3*sum((validation[, i + 19] == "no") & (validation$y == "yes"))
    +0*sum((validation[, i + 19] == "no" & validation$y == "no"))
  )
}


perform2 <- data.frame(threshold = seq(0.05, 0.95, 0.05), performance_measure = 0)
threshold <- seq(0.05, 0.95, 0.05)
i = 0
validation[, 20:200] <- NULL
validation[, 20:200] <- 0

for (c in threshold) {
  i <- i + 1
  validation[, i + 19] <- ifelse(rf_pred$yes >= c, "yes", "no")
  validation[, i + 19] <- as.factor(validation[, i + 19])
  
  perform2[i, 2] <- c(
    1*sum((validation[, i + 19] == "yes") & (validation$y == "yes")) 
    +0*sum((validation[, i + 19] == "yes") & (validation$y == "no")) 
    +0*sum((validation[, i + 19] == "no") & (validation$y == "yes"))
    +1*sum((validation[, i + 19] == "no" & validation$y == "no"))
  )
}

ggplot(data = perform, aes(x = threshold, y = performance_measure)) + geom_line(col = "blue") + ggtitle("performance measure of random forest with threshold")
# [performance_measure가 최대인 threshold] random forest 
# 0.25 : 276

ggplot(data = perform2, aes(x = threshold, y = performance_measure)) + geom_line(col = "orange") + ggtitle("accuracy of random forest with threshold")
# [accuracy가 최대인 threshold] random forest
# 0.05 : 667

# 6.2. random forest : submission.csv ####

rf <- randomForest(y ~ ., data = train[, -12], method = "class", ntree = 500, mtry = 4)
rf_yhat <- predict(rf, newdata = test, type = "prob")
rf_yhat <- data.frame(rf_yhat)
rf_yhat$yes <- ifelse(rf_yhat$yes >= 0.25, "yes", "no")
rf_yhat$yes <- as.factor(rf_yhat$yes)
test$yhat <- rf_yhat$yes
table(test$yhat)
table(is.na(test$yhat))
submission_rf <- test[, c("ID", "yhat")]
# write.csv(submission_rf, "submission_rf.csv", row.names = FALSE)
# write.csv(submission, "submission.csv", row.names = FALSE)



# 7.1. Machine Learning : Bayesian Network, Naive Bayes ####

'''
[Bayesian Network]

for (i in c(1:19)) {
  if (class(training[, i]) == "character") {
    training[, i] <- as.factor(training[, i])
  }
}

for (i in c(1:19)) {
  if (class(training[, i]) == "factor") (print(i))
}
bn <- hc(training[, c(2,3,4,5,6,7,8,9,10,14,18,19)])
plot(bn)
bn$arcs

bn_fit <- bn.fit(bn, data = training[, c(2,3,4,5,6,7,8,9,10,14,18,19)])
bn_fit$y
  # 변수를 표현하는 노드 (node) 와 변수들 간의 의존관계를 표현하는 호 (arc) 의 방향성 비순환 그래프 (directed acyclic graph)
'''

naiveBayes <- naiveBayes(y ~ ., data = training[, -12])
nb_pred <- predict(naiveBayes, newdata = validation[, -12], type = "raw")
  # [사후확률값]
nb_pred <- data.frame(nb_pred)

perform <- data.frame(threshold = seq(0.05, 0.95, 0.05), performance_measure = 0)
threshold <- seq(0.05, 0.95, 0.05)
i = 0
validation[, 20:200] <- 0

for (c in threshold) {
  i <- i + 1
  validation[, i + 19] <- ifelse(nb_pred$yes >= c, "yes", "no")
  validation[, i + 19] <- as.factor(validation[, i + 19])
  
  perform[i, 2] <- c(
    5*sum((validation[, i + 19] == "yes") & (validation$y == "yes")) 
    -2*sum((validation[, i + 19] == "yes") & (validation$y == "no")) 
    -3*sum((validation[, i + 19] == "no") & (validation$y == "yes"))
    +0*sum((validation[, i + 19] == "no" & validation$y == "no"))
  )
}


perform2 <- data.frame(threshold = seq(0.05, 0.95, 0.05), performance_measure = 0)
threshold <- seq(0.05, 0.95, 0.05)
i = 0
validation[, 20:200] <- NULL
validation[, 20:200] <- 0

for (c in threshold) {
  i <- i + 1
  validation[, i + 19] <- ifelse(nb_pred$yes >= c, "yes", "no")
  validation[, i + 19] <- as.factor(validation[, i + 19])
  
  perform2[i, 2] <- c(
    1*sum((validation[, i + 19] == "yes") & (validation$y == "yes")) 
    +0*sum((validation[, i + 19] == "yes") & (validation$y == "no")) 
    +0*sum((validation[, i + 19] == "no") & (validation$y == "yes"))
    +1*sum((validation[, i + 19] == "no" & validation$y == "no"))
  )
}

ggplot(data = perform, aes(x = threshold, y = performance_measure)) + geom_line(col = "blue") + ggtitle("performance measure of Naive Bayes with threshold")
  # [performance_measure가 최대인 threshold] naive bayes 
  # 0.15 : 298

ggplot(data = perform2, aes(x = threshold, y = performance_measure)) + geom_line(col = "orange") + ggtitle("accuracy of Naive Bayes with threshold")
  # [accuracy가 최대인 threshold] naive bayes
  # 0.90 : 641

# 7.2. test : Naive Bayes ####

nb <- naiveBayes(y ~ ., data = train[, -12])
test$yhat <- NULL
nb_yhat <- predict(nb, newdata = test[, -13], type = "raw")
nb_yhat <- data.frame(nb_yhat)
nb_yhat$yhat <- ifelse(nb_yhat$yes >= 0.15, "yes", "no")
nb_yhat$yhat <- as.factor(nb_yhat$yhat)
table(nb_yhat$yhat)
nrow(nb_yhat)

setwd("C:/PDSR/RforStatistics_FinalProject")
  # 파일경로
load("data_test.Rda")
data_test$y <- as.factor(data_test$y)
table(data_test$y)
nrow(data_test)

confusionMatrix(nb_yhat$yhat, data_test$y, positive = "yes")
0*2227 -2*773 -3*321 + 5*679 
# 나이브 베이지안 [886]

confusionMatrix(submission_bg$yhat, data_test$y, positive = "yes")
0*2709 - 2*291 - 3*406 + 5*594 
# 배깅 500번 만든 트리 평균 [1170]

confusionMatrix(submission_rf$yhat, data_test$y, positive = "yes")
0*2418 - 2*582 - 3*339 + 5*661
MLmetrics::LogLoss(submission_rf$yhat, data_test$y)
# 랜덤포레스트_부스팅 [1124]

tree_test <- rpart(y ~ ., data = train[, -12], method = "class", control = rpart.control(cp = 0.00875))
tree_yhat <- predict(tree_test, newdata = test[, -13], type = "prob")
tree_yhat <- data.frame(tree_yhat)
  # [0.12, 0.18]
tree_yhat$yhat <- ifelse(tree_yhat$yes >= 0.15, "yes", "no")
tree_yhat$yhat <- as.factor(tree_yhat$yhat)
confusionMatrix(tree_yhat$yhat, data_test$y, positive = "yes")
0*2143 -2*857 - 3*303 + 5*697 
# 의사결정 트리 [862]

logit_yhat <- data.frame(logit_yhat)
logit_yhat$logit_yhat <- as.factor(logit_yhat$logit_yhat)
confusionMatrix(logit_yhat$logit_yhat, data_test$y, positive = "yes")
0*1913 -2*1087 -3*202 + 5*798 
# 로지스틱 회귀분석 [1210]



# 8.1. Machine Learning : Support vector machine ####

set.seed(12345)
train[, 12] <- NULL
for (i in c(1:19)) {
  if (class(train[, i]) == "integer") {
    print(i)
  }
}
for (i in c(1:19)) {
  if (class(train[, i]) == "numeric") {
    print(i)
  }
}


linear.svm <- tune.svm(y ~ ., data = train[, c(1, 11, 12, 14, 15, 16, 17)], kernel = "linear", cost = seq(0.01, 0.1, by = 0.01))
summary(linear.svm)
linear.svm$best.model
  # cost = 0.5
  # Number of Support Vectors: 2005 -> OUTLIER
  # cost = 0.03
  # 1894 -> OUTLIER

for (i in c(1:19)) {
  if (class(test[, i]) == "integer") {
    print(i)
  }
}
for (i in c(1:19)) {
  if (class(test[, i]) == "numeric") {
    print(i)
  }
}
linear_pred <- predict(linear.svm$best.model, newdata = test[, c(1, 2, 12, 13, 14, 16, 17, 18)])
length(linear_pred)
linear_pred <- data.frame(linear_pred)
confusionMatrix(sample(linear_pred$linear_pred, size = 4000, replace = TRUE), data_test$y, positive = "yes")

0*415 - 2*2585 -3*118 + 5*882 
  # -1114


