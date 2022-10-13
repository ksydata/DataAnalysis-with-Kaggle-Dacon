### 통계분석실습 통계학과 기말 프로젝트

## 은행에서 예금을 수신하기 위한 데이터 기반 상품홍보 전략수립 프로젝트 ####


# I. 신원미상 고객 제거 : 결측치 줄이기 또는 결측값 대체
# II. 채무불이행 (yes) 고객 예측 모델 구현 : 예측변수 결정, 혼동행렬(채점표)을 만들 검증용(validation) 데이터 셋을 훈련용(test) 데이터 셋과 분리할 비율 결정 (7:3, 8:2, 9:1)
# III. 은행 수신상품 가입 여부 marketing(try or defer) 예측 모델 구현 : 분류의 문제
# IV. 예측 모델을 근거로 은행의 수익성 평가


# 거절할 것 같은 고객은 보류, 예금하기로 결정할 것 같은 고객에 대해서만 연락)하여 classification을 하는 learner


library(dplyr)
library(forcats)
library(ggplot2)
library(mice)
library(VIM)
library(DMwR)
  # Error : to install.packages "Data Mining with R"
  # Solution : remotes::install_github("cran/DMwR")
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

# 제거 가능한 신원미상 고객 제거
which(is.na(data_train$job) & is.na(data_train$education) & data_train$y == "no")
data_train <- data_train[-c(1511, 1656, 2043, 2717, 3003, 3013, 3051, 3863), ]
which(is.na(data_train$marital) & is.na(data_train$education) & data_train$y == "no")
data_train <- data_train[-2270, ]
which(is.na(data_train$job) & is.na(data_train$marital) & data_train$y == "no")
data_train <- data_train[-c(2831, 3599), ]


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





# 1.5.1. 상관분석 ####

# numeric
names(data_train)
cor_trn <- cor(data_train[, c(1, 11, 12, 13, 16, 17)], use = "pairwise.complete.obs")
cor_trn <- round(cor_trn, 2)
corrplot(cor_trn)

# factor : [, c(2,3,4,5,6,7,8,9,10,14,18)]
# NA's : [, c(2,3,4,6,7)]
# default를 예측하기 위한 범주형 변수 상관분석

str(data_train)

# job, marital, education, y ####
freq2.3 <- table(data_train[,2], data_train[,3])
proportions(freq2.3, margin = 1)
chisq.test(freq2.3)

freq2.4 <- table(data_train[,2], data_train[,4])
proportions(freq2.4, margin = 1)
chisq.test(freq2.4)

#
freq2.6 <- table(data_train[,2], data_train[,6])
proportions(freq2.6, margin = 1)
chisq.test(freq2.6)
  # p-value = 0.7267

freq2.7 <- table(data_train[,2], data_train[,7])
proportions(freq2.7, margin = 1)
chisq.test(freq2.7)
  # p-value = 0.1792
#

freq2.18 <- table(data_train[,2], data_train[,18])
proportions(freq2.18, margin = 1)
chisq.test(freq2.18)

freq3.18 <- table(data_train[,3], data_train[,18])
proportions(freq3.18, margin = 1)
chisq.test(freq3.18)

freq4.18 <- table(data_train[,4], data_train[,18])
proportions(freq4.18, margin = 1)
chisq.test(freq4.18)


# housing :loan, contact ####
# loan : housing ####
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






# 1.5.2. 결측치 대체 ####

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





# 1.5.3. 채무불이행 연체 예측 ####

set.seed(12345)
data_train_df <- data_train %>% filter(default == "unknown")
data_sample <- sample(c(1:dim(data_train)[1]), dim(data_train)[1] * 0.7)
data_trn <- data_train[data_sample, ]
data_val <- data_train[-data_sample, ]

tree <- rpart(default ~ job + marital + education + housing + loan + y, data = data_trn)
?rpart
summary(tree)
rpart.plot(tree)
fancyRpartPlot(tree)



# 1.6. 탐색적 데이터 분석 (시각화) ####
  # 금융상품의 금리 관련 변수

ggplot(data = data_train, aes(x = y, y = default)) + geom_point() 
  # factor / factor 분할표 작성(빈도)
ggplot(data = data_train, aes(x = y, y = age)) + geom_point() 
  # 연령대와 상품 가입 여부
  # 데이터 패턴 발견 실패

ggplot(data = data_train, aes(x = month, y = cons.price.idx)) + geom_point() + ylim(90, 100)
ggplot(data = data_train, aes(x = month, y = cons.conf.idx)) + geom_point()
  # 월별 경기 추세(물가지수, 소비자신뢰지수)
  # 데이터 패턴 발견 실패



# 1.7. 파생변수 생성 및 홍보 배제된 고객의 특성 탐색 ####

# 1.8.1. 배제사유 : 과거 0건 홍보, 현재 1건 홍보된 고객 집단과 나머지 집단 비교 ####

data_train <- data_train %>% arrange(age)
data_train$ID <- c(1:3999)
data_train <- data_train %>% relocate(ID, .before = age)
  # 고객 나이순 정렬 후 ID 생성

data_no_contact <- data_train %>% filter(campaign == 1 & previous == 0)
  # 현재 홍보 1회, 과거 홍보 0회인 고객 데이터 프레임
ggplot(data_no_contact, aes(x = ID, y = housing)) + geom_point() + facet_wrap(~job)
  # 패턴 발견 실패
ggplot(data_no_contact, aes(x = ID, y = housing)) + geom_point() + facet_wrap(~marital)
  # 패턴 발견 실패

ggplot(data_no_contact, aes(x = ID, y = loan)) + geom_point() + facet_wrap(~job)
  # 주택담보대출을 받은 사람이 적은 직업별 집단(yes) : entrepreneur, housemaid, self-employed, unemployed

ggplot(data_no_contact, aes(x = ID, y = loan)) + geom_point() + facet_wrap(~marital)
  # 패턴 발견 실패

ggplot(data_no_contact, aes(x = ID, y = loan)) + geom_point() + facet_wrap(~education)
  # 신용대출을 받은 사람이 적은 가족유형별 집단 : basic.y

data_train %>% filter(campaign >= 2 & previous >= 0) %>% group_by(loan) %>% summarise(count = n())
  # 현재 홍보 2회 이상, 과거 홍보 0회 이상인 고객 중 신용대출 여부
  # 신용대출 받은 고객 수 352명, 15.5%

data_no_contact %>% group_by(loan) %>% summarise(count = n())
  # 신용대출 받은 고객 수 181명, 12.7%
  
data_train %>% filter(campaign >= 2 & previous >= 0) %>% group_by(housing) %>% summarise(count = n())
data_no_contact %>% group_by(housing) %>% summarise(count = n())

data_contact_edu <- data_train %>% filter(campaign >= 2 & previous >= 0) %>% group_by(education) %>% summarise(count = n())
data_no_edu <- data_no_contact %>% group_by(education) %>% summarise(count = n())
ggplot(data_contact_edu, aes(x = education, y = count)) + geom_col(col = "blue", fill = "skyblue") + coord_flip()
ggplot(data_no_edu, aes(x = education, y = count)) + geom_col(col = "red", fill = "orange") + coord_flip() + ylim(0,600)


# 1.8.2. 과거 홍보, 현재 홍보된 고객 집단 특성 탐색 ####

contact_num <- data_train %>% group_by(campaign, previous) %>% summarise(count = n())
  # 과거, 현재 홍보된 고객 수 데이터 프레임
data_promote <- data_train %>% filter(campaign >= 1 & previous >= 1)
  # 652명(전체 대비 16.3%) 
ggplot(data_promote, aes(x = ID, y = y)) + geom_point() + facet_wrap(~job)


# 1.8.3. 예측변수(IV) 간 관계 파악을 위한 이항 로지스틱 회귀분석 및 상관분석 ####

# 훈련용 및 검증용 데이터 분리 : (7:3) (6:4) (8:2)

set.seed(123)
data_glm <- sample(c(1:dim(data_train)[1]), 
                   dim(data_train)[1] * 0.7)
data_trn <- data_train[data_glm, ]

data_val1 <- data_train[-data_glm, ] %>% select(y, age, job, marital, education)
data_val2 <- data_train[-data_glm, ] %>% select(y, housing, loan)
data_val3 <- data_train[-data_glm, ] %>% select(y, contact, campaign, pdays, poutcome)
data_val4 <- data_train[-data_glm, ] %>% select(y, month, day_of_week, emp.var.rate, cons.price.idx, cons.conf.idx)


# 이항 로지스틱 회귀분석 모델링 1 ####

glm1_trn <- glm(y ~ age + job + marital + education, family = "binomial", data = data_trn)
  # 고객의 신원 데이터

summary(glm1_trn)
exp(coef(glm1_trn))
confint(glm1_trn)

# 모형 1 성능 평가 (혼동행렬)

glm1_pred <- predict(glm1_trn, data_val1, type = "response")
df_glm1_pred <- as.data.frame(glm1_pred)
  # 반환된 확률 결과의 데이터 프레임 작성
df_glm1_pred$default <- ifelse(df_glm1_pred$glm1_pred >= 0.5, df_glm1_pred$default <- "Yes", df_glm1_pred$default <- "No")
  # 0.5 임계값으로 설정하여 범주화
table(df_glm1_pred$default)

class(data_val1$y)
table(data_val1$y)
class(df_glm1_pred$default)
table(df_glm1_pred$default)
data_val1$y <- ifelse(data_val1$y == "yes", "Yes", "No")
data_val1$y <- as.factor(data_val1$y)
df_glm1_pred$default <- as.factor(df_glm1_pred$default)

confusionMatrix(data = df_glm1_pred$default, 
                reference = data_val1$y)


# 모형 1의 유의성 검정 (카이제곱 검정) / 이탈도

anova(glm1_trn, test = "Chisq")

# 모형 1 변수 간 다중공선성 확인(Variance Inflation Factor 즉, 분산팽창요인)

vif(glm1_trn)
  # 10보다 작아야 하는 vif 값(Kabocoff, R in Action)
  # 2보다 작아야 하는 vif의 양의 제곱근
  
# 모형 1에 영향력 있는 변수 내림차순 차트 시각화

vip(glm1_trn, num_features = 22)



# 이항 로지스틱 회귀분석 모델링 2 ####

glm2_trn <- glm(y ~ housing + loan, family = "binomial", data = data_trn)
  # 고객의 여신금융 데이터

summary(glm2_trn)
exp(coef(glm2_trn))

# 모형 2 성능 평가 (혼동행렬)

glm2_pred <- predict(glm2_trn, data_val2, type = "response")
df_glm2_pred <- as.data.frame(glm2_pred)
  # 반환된 확률 결과의 데이터 프레임 작성
df_glm2_pred$default <- ifelse(df_glm2_pred$glm2_pred >= 0.5, df_glm2_pred$default <- "Yes", df_glm2_pred$default <- "No")
  # 0.5 임계값으로 설정하여 범주화
table(df_glm2_pred$default)

class(data_val2$y)
table(data_val2$y)
class(df_glm2_pred$default)
table(df_glm2_pred$default)
data_val2$y <- ifelse(data_val2$y == "yes", "Yes", "No")
data_val2$y <- as.factor(data_val2$y)
df_glm2_pred$default <- as.factor(df_glm2_pred$default)

confusionMatrix(data = df_glm2_pred$default, 
                reference = data_val2$y)



# 모형 2의 유의성 검정 (카이제곱 검정) / 이탈도

anova(glm2_trn, test = "Chisq")

# 모형 2 변수 간 다중공선성 확인

vif(glm2_trn)

# 모형 2에 영향력 있는 변수 내림차순 차트 시각화

vip(glm2_trn, num_features = 2)



# 이항 로지스틱 회귀분석 모델링 3 ####

glm3_trn <- glm(y ~ contact + campaign + pdays + poutcome, family = "binomial", data = data_trn)
  # 고객의 마케팅 데이터

summary(glm3_trn)
exp(coef(glm3_trn))

# 모형 3 성능 평가 (혼동행렬)

glm3_pred <- predict(glm3_trn, data_val3, type = "response")
df_glm3_pred <- as.data.frame(glm3_pred)
# 반환된 확률 결과의 데이터 프레임 작성
df_glm3_pred$default <- ifelse(df_glm3_pred$glm3_pred >= 0.5, df_glm3_pred$default <- "Yes", df_glm3_pred$default <- "No")
# 0.5 임계값으로 설정하여 범주화
table(df_glm3_pred$default)

class(data_val3$y)
table(data_val3$y)
class(df_glm3_pred$default)
table(df_glm3_pred$default)
data_val3$y <- ifelse(data_val3$y == "yes", "Yes", "No")
data_val3$y <- as.factor(data_val3$y)
df_glm3_pred$default <- as.factor(df_glm3_pred$default)

confusionMatrix(data = df_glm3_pred$default, 
                reference = data_val3$y)


# 모형 3의 유의성 검정 (카이제곱 검정) / 이탈도

anova(glm3_trn, test = "Chisq")

# 모형 3 변수 간 다중공선성 확인

vif(glm3_trn)

# 모형 3에 영향력 있는 변수 내림차순 차트 시각화

vip(glm3_trn, num_features = 5)



# 이항 로지스틱 회귀분석 모델링 4 ####

glm4_trn <- glm(y ~ month + day_of_week + emp.var.rate + cons.price.idx + cons.conf.idx, family = "binomial", data = data_trn)
  # 수신상품 금리 관련 데이터

summary(glm4_trn)
exp(coef(glm4_trn))

# 모형 4 성능 평가 (혼동행렬)

glm4_pred <- predict(glm4_trn, data_val4, type = "response")
df_glm4_pred <- as.data.frame(glm4_pred)
  # 반환된 확률 결과의 데이터 프레임 작성
df_glm4_pred$default <- ifelse(df_glm4_pred$glm4_pred >= 0.5, df_glm4_pred$default <- "Yes", df_glm4_pred$default <- "No")
  # 0.5 임계값으로 설정하여 범주화
table(df_glm4_pred$default)

class(data_val4$y)
table(data_val4$y)
class(df_glm4_pred$default)
table(df_glm4_pred$default)
data_val4$y <- ifelse(data_val4$y == "yes", "Yes", "No")
data_val4$y <- as.factor(data_val4$y)
df_glm4_pred$default <- as.factor(df_glm4_pred$default)

confusionMatrix(data = df_glm4_pred$default, 
                reference = data_val1$y)


# 모형 4의 유의성 검정 (카이제곱 검정) / 이탈도

anova(glm4_trn, test = "Chisq")

# 모형 4 변수 간 다중공선성 확인

vif(glm4_trn)

# 모형 4에 영향력 있는 변수 내림차순 차트 시각화

vip(glm4_trn, num_features = 16)






