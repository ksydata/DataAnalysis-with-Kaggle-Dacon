
## 서울특별시 마포구 시간별 기상상황 및 따릉이(서울시 공공자전거) 대여수 탐색적 데이터 분석 ####
# 분석 목적 : 1시간 전 기상상황으로 "1시간 후" 시간대의" 따릉이 "대여수" 예측

# https://dacon.io/competitions/open/235576/data


library(readr)
train <- read.csv("train.csv")
{
  codebook <- data.frame(
    feature = c("id", "hour", "count", "hour_bef_temperature",
                "hour_bef_precipitation", "hour_bef_windspeed", 
                "hour_bef_humidity", "hour_bef_visibility", 
                "hour_bef_ozone", "hour_bef_pm10", "hour_bef_pm2.5"), 
    
    descript = c("고유ID(날짜, 시간별 id), 분석 제외", 
                 "시간대, 13(13시부터 14시)",
                 "시간별 따릉이 대여수", 
                 "1시간 전 기온", 
                 "1시간 전 비 정보, 비가 오면 1", 
                 "1시간 전 평균풍속", 
                 "1시간 전 습도", 
                 "1시간 전 시정, 시계(특정 기상 상태에 따른 가시성을 의미)", 
                 "1시간 전 오존", 
                 "1시간 전 미세먼지(머리카락 굵기의 1/5~1/7 크기의 미세먼지 입자)", 
                 "1시간 전 미세먼지(머리카락 굵기의 1/20~1/30 크기의 미세먼지 입자"))
} 
  # Codebook
train[,1] <- NULL
  # id변수 분석에서 제외


# 1. 데이터 전처리 ####

library(dplyr)
library(mice)
library(VIM)
library(descr)
library(psych)
library(ggplot2)
library(SmartEDA)
library(corrplot)
library(rpart)
library(DMwR)
library(RColorBrewer)
library(scatterplot3d)
library(car)
library(lmtest)


# 1.1. 결측값 처리 기준 ####

str(train)
train <- train %>% relocate(count, .after = hour)
train$hour_bef_precipitation <- as.factor(train$hour_bef_precipitation)
table(train$hour_bef_precipitation)
summary(train)
  # NA's col(300) : ozone(76), pm2.5(90), pm10(117)
  # Null(결측값)인지 0인지 판단

for (i in (8:10)) {
  print(freq(train[,i]))
}
md.pattern(train[, 3:10])
  # Visualization and Imputation of Missing Values
mice_plot <- aggr(train, col = c("grey", "lightgreen"), numbers = TRUE, sortVars = TRUE, labels = names(train), cex.axis = 0.7, gap = 3, ylab = c("결측치", "패턴"))


# 1.2. 결측값을 추정된 회귀식의 예측값으로 대체 ####

# NA's(300) : ozone(76), pm2.5(117), pm10(90)

# 1.2.1. pm10변수 결측값 대체_regression ####

anova_mod10 <- rpart(hour_bef_pm10 ~ . - count, 
                     data = train[!is.na(train$hour_bef_pm10), ], 
                     method = "anova", na.action = na.omit)
  # since hour_bef_pm10 is numeric
pm10_pred <- predict(anova_mod10, train[is.na(train$hour_bef_pm10), ])
pm10_pred

train$hour_bef_pm10[is.na(train$hour_bef_pm10)] <- pm10_pred
summary(train$hour_bef_pm10)

# 1.2.2. pm2.5변수 결측값 대체_regression ####

anova_mod2.5 <- rpart(hour_bef_pm2.5 ~ . - count,
                      data = train[!is.na(train$hour_bef_pm2.5), ], 
                      method = "anova", na.action = na.omit)
  # since hour_bef_pm2.5 is numeric
pm2.5_pred <- predict(anova_mod2.5, train[is.na(train$hour_bef_pm2.5), ])
length(pm2.5_pred)

train$hour_bef_pm2.5[is.na(train$hour_bef_pm2.5)] <- pm2.5_pred
table(is.na(train$hour_bef_pm2.5))
summary(train$hour_bef_pm2.5)

# 1.2.3. ozone 변수 결측값 대체_randomForest ####

ozone_mod <- mice(train[, !names(train) %in% "count"], 
                  method = "rf", printFlag = F)
miceOutput <- complete(ozone_mod)
  
actuals <- train$hour_bef_ozone[!is.na(train$hour_bef_ozone)]
predicteds <- miceOutput[is.na(train$hour_bef_ozone), "hour_bef_ozone"]
regr.eval(actuals, predicteds)
mean(actuals != predicteds)

train$hour_bef_ozone[is.na(train$hour_bef_ozone)] <- predicteds
summary(train$hour_bef_ozone)

mice_plot <- aggr(train[, 8:10], col = c("grey", "green"), numbers = TRUE, sortVars = TRUE, labels = names(train[, 8:10]), cex.axis = 0.7, gap = 3, ylab = c("결측값 대체", "패턴"))
EDA <- ExpData(train)


# 1.3. 이상치 처리 기준 (예측 모델 성능 향상 목적) ####

# 1.3.1. 이상치 탐색 ####

descr <- describe(train[, c(3, 5:10)])
descr <- descr %>% mutate(UL = mean + 2.58*sd, LL = mean - 2.58*sd)
  # 1.96 or 2.58

for (i in c(5:10)) {
  print(table(train[,i] >= descr[i-3, 14]))
}
table(train[,3] >= descr[1, 14])
  # outlier : windspeed(13), ozone(16), pm10(40), pm2.5(41)

for (i in c(5:10)) {
  print(table(train[,i] <= descr[i-3, 15]))
}
table(train[,3] <= descr[1, 15])
  # outlier : temperature(2)


# 1.3.2. lower limit인 이상치 처리 ####

which(train[,3] <= descr[1, 15])
train[785,]
train[928,]
summary(train$count)
summary(train$hour_bef_humidity)
  # windspeed, ozone, pm10, pm2.5 값이 outlier 아닌바 temperature변수의 outlier 값 존치
  # 결론 : 785행, 928행은 기온이 낮고, 습도가 높은 오전 시간대로 판단


# 1.3.3. upper limit인 이상치 정제 ####

histogram(train$hour_bef_windspeed)
histogram(train$hour_bef_ozone)
histogram(train$hour_bef_pm10)
histogram(train$hour_bef_pm2.5)

ggplot(data = train, aes(y = hour_bef_windspeed)) + geom_boxplot(col = "skyblue", outlier.color = "brown")
ggplot(data = train, aes(y = hour_bef_ozone)) + geom_boxplot(col = "dark blue", outlier.color =  "yellow")
ggplot(data = train, aes(y = hour_bef_pm10)) + geom_boxplot(col = "blue", outlier.color =  "red")
ggplot(data = train, aes(y = hour_bef_pm2.5)) + geom_boxplot(col = "purple", outlier.color = "orange")

# (Q1 - 1.5*IQR), (Q3 + 1.5*IQR)
boxplot(train$hour_bef_windspeed)$stat
  # 0.0, 6.4
boxplot(train$hour_bef_ozone)$stat
  # 0.0030, 0.0880
boxplot(train$hour_bef_pm10)$stat
  # 9, 119
boxplot(train$hour_bef_pm2.5)$stat
  # 8, 62

# (x - 2.58*sd), (x + 2.58*sd)
round(descr[2, 14], 2)
round(descr[2, 15], 2)
  # windspeed : -1.08, 6.03
round(descr[5, 14], 2)
round(descr[5, 15], 2)
  # ozone : -0.01, 0.09
round(descr[6, 14], 2)
round(descr[6, 15], 2)
  # pm10 : -23.16, 137.43
round(descr[7, 14], 2)
round(descr[7, 15], 2)
  # pm2.5 : -7.16, 67.91


# 1.3.4. 이상치 정제 및 변수 튜닝 목적의 상관분석 ####

str(train)
cor <- cor(scale(train[, c(3, 5:10)]), use = "pairwise.complete.obs")
  # 변수간 상이한 단위 고려한 정규화
  # NA 있는 위치에서의 연산만 넘어가는 파라미터
corrplot(cor)

corr <- corr.test(scale(train[, c(3, 5:10)]), method = "pearson", alpha = 0.05, use = "pairwise.complete.obs")
print(corr, short = FALSE)

  # (temperature, ozone) 0.54
  # (windspeed, temperature) 0.38
  # (windspeed, ozone) 0.52
  # (pm10, pm2.5) 0.51
  # (humidity, windspeed) -0.43


# 1.3.5. 파생변수 생성 ####

# 시간대 범주형 파생변수

nb.cols <- 24
hour.cols <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)
ggplot(data = train, aes(x = hour, y = count, fill = factor(hour))) + geom_col() + scale_fill_manual(values = hour.cols) + labs(x = "시간대", y = "따릉이 대여수", title = "서울특별시 마포구\n시간대별 따릉이 대여수")

train <- train %>% mutate(timeZone = case_when(hour <= 2 ~ "midnight", hour <= 5 ~ "daybreak", hour <= 10 ~ "morning", hour <= 13 ~ "noon", hour <= 16 ~ "afternoon", hour <= 19 ~ "evening", hour <= 23 ~ "lastevening"))
train$timeZone <- as.factor(train$timeZone)
table(train$timeZone)
train <- train %>% relocate(timeZone, .after = hour)

# 자전거 대여수 및 시간대 관계 파악

boxplot(train$count)$stat
histogram(train$count)
ggplot(data = train, aes(x = timeZone, y = count, , fill = factor(timeZone))) + geom_col() + labs(x = "시간대", y = "따릉이 대여수", title = "서울특별시 마포구\n따릉이 시간대별 대여수")
# ggplot(data = train, aes(x = count, y = timeZone, , fill = factor(timeZone))) + geom_col() + labs(x = "따릉이 대여수", y = "시간대", title = "서울특별시 마포구\n따릉이 대여수별 시간대")
train %>% group_by(timeZone) %>% summarise(mean.count = mean(count), count = n(), sum.count = mean.count * count) %>% arrange(-sum.count)


# 1.3.6. 풍속 변수의 이상치 처리 ####

trn_wind <- train %>% filter(hour_bef_windspeed >= 6.4)

# 풍속과 자전거 대여수(풍속이 이상치일 때 데이터 분포 탐색)
ggplot(data = trn_wind, aes(x = hour_bef_windspeed, y = count)) + geom_point() +  geom_smooth(se = FALSE, method = "loess", formula = "y ~ x")
  # 풍속이 이상치로 높을 때 자전거 대여수와의 관계
ggplot(data = train, aes(x = hour_bef_windspeed, y = count)) + geom_point() + geom_smooth(se = FALSE, method = "loess", formula = "y ~ x") + facet_wrap(~timeZone)
ggplot(data = trn_wind, aes(x = hour_bef_windspeed, y = count)) + geom_col() + facet_wrap(~timeZone)
  # afternoon_evening 풍속 이상치이면서 자전거 대여수 다른 시간대 대비 높은 수준
scatterplot3d(x = train$hour_bef_windspeed, y = train$hour_bef_temperature, z = train$count, color = "blue")

# 풍속과 습도
  # (windspeed, humidity) r = -0.43
ggplot(data = trn_wind, aes(x = hour_bef_windspeed, y = hour_bef_humidity)) + geom_point() + geom_smooth(se = FALSE, method = "loess", formula = "y ~ x")
  # 풍속이 이상치로 높을 때 습도와의 관계
ggplot(data = train, aes(x = hour_bef_windspeed, y = hour_bef_humidity)) + geom_point() + geom_smooth(se = FALSE, method = "loess", formula = "y ~ x") + facet_wrap(~timeZone)

# 풍속과 기온, 풍속과 O3
  # (windspeed, temperature) r = 0.38
  # (temperature, ozone) r = 0.54
  # (windspeed, ozone) r = 0.52
ggplot(data = trn_wind, aes(x = hour_bef_windspeed, y = hour_bef_ozone, fill = factor(hour_bef_ozone))) + geom_col()
ggplot(data = trn_wind, aes(x = hour_bef_windspeed, y = hour_bef_temperature, fill = factor(hour_bef_temperature))) + geom_col() + scale_fill_brewer(palette = "Paired")
ggplot(data = trn_wind, aes(x = hour_bef_windspeed, y = hour_bef_ozone, fill = factor(hour_bef_temperature))) + geom_col() + scale_fill_brewer(palette = "Set3")
ggplot(data = train, aes(x = hour_bef_windspeed, y = hour_bef_ozone, fill = factor(hour_bef_temperature))) + geom_col()

scatterplot3d(x = trn_wind$hour_bef_windspeed, y = trn_wind$hour_bef_temperature, z = trn_wind$ozone, color = "red")
  # 풍속 이상치 df 산점도
scatterplot3d(x = train$hour_bef_windspeed, y = train$hour_bef_temperature, z = train$ozone, color = "skyblue")
  # 전체 df 산점도와 전자 비교

train[which(train$hour_bef_windspeed >= descr[2, 14]), ]
  # 풍속 이상치 행의 값 확인
which(train$hour_bef_windspeed >= descr[2, 14]
      & train$hour_bef_ozone >= descr[5, 14])
  # train[1294, ] 존치
  # 오존 이상치, 풍속 이상치, 고온, 낮은 습도, 햇빛 : 저녁 시간대 자전거 280대 대여
which(train$hour_bef_windspeed >= descr[2, 14]
      & train$hour_bef_pm10 >= descr[6, 14])
  # train[c(832, 906), ] 존치
  # 미세먼지 이상치, 풍속 이상치, 서늘한 기온, 낮은 습도, 햇빛 : 저녁 시간대 자전거 106대 대여
  # 미세먼지 이상치, 풍속 이상치, 서늘한 기온, 낮은 습도, 햇빛 : 오후 시간대 자전거 74대 대여


# 1.3.7. 대기오염물질 변수(O3, pm)의 이상치 처리 ####
  # 이상치가 자전거 대여수에 영향을 미치는지 예측 목적 
  # 하위 데이터 프레임 및 파생변수(atomosphere) 생성

train$id <- c(1:1459)
train_outlier <- data.frame(
  train[which(train$hour_bef_ozone >= descr[5, 14]
              | train$hour_bef_pm2.5 >= descr[7, 14]
              | train$hour_bef_pm10 >= descr[6, 14]), ])
train_outlier <- train_outlier %>% mutate(atomosphere = "outlier")

train <- full_join(train, train_outlier, by = "id")
train[, c(12:23)] <- NULL
train$atomosphere <- ifelse(is.na(train$atomosphere), "general", train$atomosphere)
table(train$atomosphere)
train$atomosphere <- as.factor(train$atomosphere)
  # general(1365) outlier(94)
str(train)
train <- train %>% rename(
  hour = hour.x, timeZone = timeZone.x, count = count.x, 
  hour_bef_temperature = hour_bef_temperature.x, 
  hour_bef_precipitation = hour_bef_precipitation.x,
  hour_bef_windspeed = hour_bef_windspeed.x,
  hour_bef_humidity = hour_bef_humidity.x,
  hour_bef_visibility = hour_bef_visibility.x,
  hour_bef_ozone = hour_bef_ozone.x,
  hour_bef_pm10 = hour_bef_pm10.x,
  hour_bef_pm2.5 = hour_bef_pm2.5.x)


# 1.3.8. 다중회귀식 수립하여 선형성, 정규성, 등분산성을 개선하기 위해 제거할 행 추출 ####
  # 쿡의 거리로 다중회귀식의 그래프 개형에 영향을 미치는 값

lm <- lm(count # Yi(hat) = a + b1X1i + b2X2i + ... + b5X5i + b6X6i
         ~ hour_bef_temperature 
         + hour_bef_windspeed 
         + hour_bef_humidity 
         + hour_bef_ozone 
         + hour_bef_pm10 + hour_bef_pm2.5, 
         data = train)
summary(lm)
  # pm2.5는 2p-value > 0.05로 상관계수 추정치가 통계적으로 유의하지 않은바 다중회귀식 추정 시 pm2.5 제거

lm2 <- lm(count # Yi(hat) = a + b1X1i + b2X2i + ... + b5X5i
         ~ hour_bef_temperature 
         + hour_bef_windspeed 
         + hour_bef_humidity 
         + hour_bef_ozone 
         + hour_bef_pm10,
         data = train)
summary(lm2)
plot(lm2)
  # Residuals vs Fitted 그래프 (선형성)
  # Normal Q-Q 그래프 (정규성)
  # Scale-Location 그래프 (등분산성)
  # Residuals vs Leverage 그래프 (잔차의 독립성)
durbinWatsonTest(lm2)
dwtest(formula = lm2,  alternative = "two.sided")
  # 오차의 자기상관 (회귀로부터의 잔차가 자기상관관계가 없다는 귀무가설 채택)
vif(lm2)
  # 다중공선성(1/공차한계)

train_outlier2 <- train[c(453, 1136, 804, 326, 370, 381), ]
train <- train[-c(453, 1136, 804, 326, 370, 381), ]


# 1.4. 데이터 시각화 ####

# 기온(x) 대여수(y) 산점도 + 대기오염지수 이상치 여부로 면 분할

ggplot(data = train, aes(x = hour_bef_temperature, y = count, group = timeZone, col = timeZone)) + geom_point() + facet_wrap(~ atomosphere) + geom_smooth(se = FALSE, method = "gam")
  # 전체 df 기준

ggplot(data = train_outlier, aes(x = hour_bef_temperature, y = count, group = timeZone, col = timeZone)) + geom_point() + geom_smooth(se = FALSE, method = "loess", formula = "y ~ x")
table(train_outlier$hour_bef_precipitation)
ggplot(data = train_outlier, aes(x = hour_bef_windspeed, y = count, group = timeZone, col = timeZone)) + geom_point() + geom_smooth(se = FALSE, method = "loess", formula = "y ~ x")
  # 이상치 df 기준


# 기온(x) 대여수(y) 산점도 + 비오는지 여부로 면 분할

ggplot(data = train, aes(x = hour_bef_temperature, y = count)) + geom_point() + facet_wrap(~ hour_bef_precipitation) + geom_smooth(se = FALSE)

ggplot(data = train, aes(x = hour_bef_temperature, y = count, group = timeZone, col = timeZone)) + geom_point() + facet_wrap(~hour_bef_precipitation) + geom_smooth(se = FALSE, method = "gam") + scale_color_brewer(palette = "YlGnBu")


# 습도(x) 대여수(y) 산점도 + 대기오염지수 이상치 여부로 면 분할

ggplot(data = train, aes(x = hour_bef_humidity, y = count, group = timeZone, col = timeZone)) + geom_point() + facet_wrap(~atomosphere) + geom_smooth(se = FALSE, method = "gam")

ggplot(data = train_outlier, aes(x = hour_bef_humidity, y = count, group = timeZone, col = timeZone)) + geom_point() + geom_smooth(se = FALSE, method = "loess", formula = "y ~ x")
