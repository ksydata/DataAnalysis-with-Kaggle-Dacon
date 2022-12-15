
# 통계분석 실습 3차 과제 2번.

library(readr)
  # https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers
library(caret)
library(randomForest)
library(dplyr)
library(ggplot2)
# remotes::install_github("cardiomoon/multipleROC")
# library(multipleROC)
# library(ROCR)
# library(Epi)
library(pROC)
library(MLmetrics)

library(DMwR)
library(e1071)
library(ROSE)



creditCard <- read.csv("https://raw.githubusercontent.com/ksydata/DataAnalysis-with-Kaggle-Dacon/main/BankChurners.csv", header = TRUE, fileEncoding = "utf-8", sep = ",")
creditCard[, c(22, 23)] <- NULL
    # kaggle 권고사항에 따라 22, 23번째 열 제거
table(creditCard$Attrition_Flag)
    # target class(DV) : Attrition_Flag 신용카드가입고객의 이탈 여부
    # yes(attrited)와 no(existing)의 비율은 16:84
table(is.na(creditCard))
    # 결측값 확인
for (i in c(1:21)) {
  if (class(creditCard[, i]) == "character") {
    creditCard[, i] <- as.factor(creditCard[, i])
  }
}
class(creditCard$Attrition_Flag)
    # 문자형 변수를 범주형으로 변환

set.seed(2023)
split <- createDataPartition(creditCard$Attrition_Flag, p = 0.8, list = FALSE)
creditTrain <- creditCard[split, ]
creditValid <- creditCard[-split, ]
  # class 1(attrited)와 class 0(existing)의 비율 16:84가 되도록 
  # 훈련용 및 검증용 데이터 세트 분할
table(creditTrain$Attrition_Flag)/nrow(creditTrain)
table(creditValid$Attrition_Flag)/nrow(creditValid)



# 2.1. raw ####

rf_raw <- randomForest(Attrition_Flag ~ .,
                       data = creditTrain[, -1], 
                       mtry = 4,
                       ntree = 1000)
    # 고객번호(CLIENT NUM)은 분석에서 제외
    # mtry는 sqrt(ncol(rf_raw))를 소수점 첫째자리에서 반올림
rf_raw_pred <- predict(rf_raw, newdata = creditValid[, -1], method = "class")
creditValid$rf_raw_pred <- rf_raw_pred
creditValid$rf_raw_pred <- as.factor(rf_raw_pred)
confusionMatrix(creditValid$Attrition_Flag, creditValid$rf_raw_pred, positive = "Attrited Customer")


# ROC 1. Trial & Error
recall <- cumsum(creditValid$rf_raw_pred == "Attrited Customer") / sum(creditValid$Attrition_Flag == "Attrited Customer")
    # True Positive Ratio = TP/(TP+FN)
    # cumsum(labels) / sum(labels)
specificity <- (sum(creditValid$Attrition_Flag == "Existing Customer") - cumsum(creditValid$rf_raw_pred == "Existing Customer")) / sum(creditValid$Attrition_Flag == "Existing Customer")
    # False Positive Ratio = 1-TN/(TN+FP) = FP/(FP+TN)
    # cumsum(!labels) / sum(!labels)
ROC <- data.frame(TPR = recall, FPR = specificity)
# ROC <- ROC %>% arrange(-FPR)
  # 예측한 확률을 오름차순 정렬

ggplot(ROC, aes(x = FPR, y = TPR)) + geom_line(color = "green") + scale_x_reverse(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + geom_line(data = data.frame(x = (0:100) / 100), aes(x = x, y = 1-x), linetype = "dotted", color = "red") + labs(x = "False Positive Fraction", y = "True Positive Fraction", title = "ROC curve with random forest")
    # Receiver operating characteristic curve
    # line : no skill

# ROC 2.
rf_raw_prob <- predict(rf_raw, newdata = creditValid[, -1], type = "prob")
rf_raw_prob <- data.frame(rf_raw_prob)
rf_raw_prob <- rf_raw_prob %>% arrange(-Attrited.Customer)
    # 예측한 확률을 오름차순 정렬
raw_pred <- prediction(rf_raw_prob$Attrited.Customer, creditValid$Attrition_Flag)
raw_roc = performance(raw_pred, "tpr", "fpr")



# 2.2. under sampling ####

table(creditTrain$Attrition_Flag)
underTrain <- ovun.sample(Attrition_Flag ~ ., data = creditTrain, method = "under", N = 2604)$data
table(overTrain$Attrition_Flag)

rf_under <- randomForest(Attrition_Flag ~ ., data = underTrain[, -1], mtry = 4, ntree = 1000)

rf_under_pred <- predict(rf_under, newdata = creditValid[, -1], method = "class")
creditValid$rf_under_pred <- rf_under_pred
creditValid$rf_under_pred <- as.factor(rf_under_pred)
confusionMatrix(creditValid$Attrition_Flag, creditValid$rf_under_pred, positive = "Attrited Customer")

# ROC
rf_under_prob <- predict(rf_under, newdata = creditValid[, -1], type = "prob")
rf_under_prob <- data.frame(rf_under_prob)
rf_under_prob <- rf_under_prob %>% arrange(-Attrited.Customer)
under_pred <- prediction(rf_under_prob$Attrited.Customer, creditValid$Attrition_Flag)
under_roc = performance(under_pred, "tpr", "fpr")



# 2.3. over sampling ####

table(creditTrain$Attrition_Flag)
overTrain <- ovun.sample(Attrition_Flag ~ ., data = creditTrain, method = "over", N = 13600)$data
table(overTrain$Attrition_Flag)

rf_over <- randomForest(Attrition_Flag ~ ., data = overTrain[, -1], mtry = 4, ntree = 1000)

rf_over_pred <- predict(rf_over, newdata = creditValid[, -1], method = "class")
creditValid$rf_over_pred <- rf_over_pred
creditValid$rf_over_pred <- as.factor(rf_over_pred)
confusionMatrix(creditValid$Attrition_Flag, creditValid$rf_over_pred, positive = "Attrited Customer")

# ROC
rf_over_prob <- predict(rf_over, newdata = creditValid[, -1], type = "prob")
rf_over_prob <- data.frame(rf_over_prob)
rf_over_prob <- rf_over_prob %>% arrange(-Attrited.Customer)
over_pred <- prediction(rf_over_prob$Attrited.Customer, creditValid$Attrition_Flag)
over_roc = performance(over_pred, "tpr", "fpr")



# 2.4. both ####

table(creditTrain$Attrition_Flag)
bothTrain <- ovun.sample(Attrition_Flag ~ ., data = creditTrain, method = "both", p = 0.5, N = 8102)$data
table(bothTrain$Attrition_Flag)

rf_both <- randomForest(Attrition_Flag ~ ., data = bothTrain[, -1], mtry = 4, ntree = 1000)

rf_both_pred <- predict(rf_both,newdata = creditValid[, -1], method = "class")
creditValid$rf_both_pred <- rf_both_pred
creditValid$rf_both_pred <- as.factor(rf_both_pred)
confusionMatrix(creditValid$Attrition_Flag, creditValid$rf_both_pred, positive = "Attrited Customer")

# ROC
rf_both_prob <- predict(rf_both, newdata = creditValid[, -1], type = "prob")
rf_both_prob <- data.frame(rf_both_prob)
rf_both_prob <- rf_both_prob %>% arrange(-Attrited.Customer)
both_pred <- prediction(rf_both_prob$Attrited.Customer, creditValid$Attrition_Flag)
both_roc = performance(both_pred, "tpr", "fpr")



# 2.5. ROSE ####

# ROSE uses smoothed bootstrapping to draw artificial samples from the feature space neighborhood around the minority class.

roseTrain <- ROSE(Attrition_Flag ~ ., data = creditTrain, N = 8102, seed = 2022)$data
table(roseTrain$Attrition_Flag)

rf_rose <- randomForest(Attrition_Flag ~ ., data = roseTrain[, -1], mtry = 4, ntree = 1000)

rf_rose_pred <- predict(rf_rose,newdata = creditValid[, -1], method = "class")
creditValid$rf_rose_pred <- rf_rose_pred
creditValid$rf_rose_pred <- as.factor(rf_rose_pred)
confusionMatrix(creditValid$Attrition_Flag, creditValid$rf_rose_pred, positive = "Attrited Customer")

# ROC
rf_rose_prob <- predict(rf_rose, newdata = creditValid[, -1], type = "prob")
rf_rose_prob <- data.frame(rf_rose_prob)
rf_rose_prob <- rf_rose_prob %>% arrange(-Attrited.Customer)
rose_pred <- prediction(rf_rose_prob$Attrited.Customer, creditValid$Attrition_Flag)
rose_roc = performance(rose_pred, "tpr", "fpr")



# 2.6. SMOTE [합성 소수 과잉표본 기법] ####

# KNN 관측치 찾아 해당 관측값과 최근접 이웃 관측치들 사이의 값을 생성

table(creditTrain$Attrition_Flag)
Over <- (0.6*6800 - 1302) / 1302
Under <- (0.4*6800) / (1302*Over)
Over_pct <- round(Over, 1) * 100
Under_pct <- round(Under, 1) * 100

smoteTrain <- SMOTE(Attrition_Flag ~ ., data = creditTrain, 
                    perc.over = Over_pct, perc.under = Under_pct)

rf_smote <- randomForest(Attrition_Flag ~ ., data = smoteTrain[, -1], mtry = 4, ntree = 1000)

rf_smote_pred <- predict(rf_smote,newdata = creditValid[, -1], method = "class")
creditValid$rf_smote_pred <- rf_smote_pred
creditValid$rf_smote_pred <- as.factor(rf_smote_pred)
confusionMatrix(creditValid$Attrition_Flag, creditValid$rf_smote_pred, positive = "Attrited Customer")

# ROC
rf_smote_prob <- predict(rf_smote, newdata = creditValid[, -1], type = "prob")
rf_smote_prob <- data.frame(rf_smote_prob)
rf_smote_prob <- rf_smote_prob %>% arrange(-Attrited.Customer)
smote_pred <- prediction(rf_smote_prob$Attrited.Customer, creditValid$Attrition_Flag)
smote_roc = performance(smote_pred, "tpr", "fpr")



# 2.7. 가중치 부여 ####

weights <- ifelse(creditTrain$Attrition_Flag == "Existing Customer",
                  1 / mean(creditTrain$Attrition_Flag == "Existing Customer"), 1)

rf_wt <- randomForest(Attrition_Flag ~ ., data = creditTrain[, -1], mtry = 4, ntree = 1000, weight = weights)

rf_wt_pred <- predict(rf_wt, newdata = creditValid[, -1], method = "class")
creditValid$rf_wt_pred <- rf_wt_pred
creditValid$rf_wt_pred <- as.factor(rf_wt_pred)
confusionMatrix(creditValid$Attrition_Flag, creditValid$rf_wt_pred, positive = "Attrited Customer")

# ROC
rf_wt_prob <- predict(rf_wt, newdata = creditValid[, -1], type = "prob")
rf_wt_prob <- data.frame(rf_wt_prob)
rf_wt_prob <- rf_wt_prob %>% arrange(-Attrited.Customer)
wt_pred <- prediction(rf_wt_prob$Attrited.Customer, creditValid$Attrition_Flag)
wt_roc = performance(wt_pred, "tpr", "fpr")

# 2.8. par(mfrow = c(2,3)) ####

'''
plot(raw_roc, col = "green", lwd = 2, xlim = c(0, 1), ylim = c(0, 1)) 
abline(a = 0, b = 1)
par(new = TRUE)
plot(over_roc, col = "blue", lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(a = 0, b = 1)
par(new = TRUE)
plot(under_roc, col = "red", lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(a = 0, b = 1)
par(new = TRUE)
plot(both_roc, col = "purple", lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
par(new = TRUE)
plot(rose_roc, col = "orange", lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
par(new = TRUE)
plot(smote_roc, col = "skyblue", lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
'''

roc.curve(creditValid$Attrition_Flag, rf_raw_prob$Attrited.Customer, plotit = TRUE, col = "green")
    # Area under the curve (AUC): 0.599
par(new = TRUE)
roc.curve(creditValid$Attrition_Flag, rf_under_prob$Attrited.Customer, plotit = TRUE, col = "red")
    # Area under the curve (AUC): 0.596
par(new = TRUE)
roc.curve(creditValid$Attrition_Flag, rf_over_prob$Attrited.Customer, plotit = TRUE, col = "blue")
    # Area under the curve (AUC): 0.600
par(new = TRUE)
roc.curve(creditValid$Attrition_Flag, rf_both_prob$Attrited.Customer, plotit = TRUE, col = "purple")
    # Area under the curve (AUC): 0.596
par(new = TRUE)
roc.curve(creditValid$Attrition_Flag, rf_rose_prob$Attrited.Customer, plotit = TRUE, col = "orange")
    # Area under the curve (AUC): 0.603
par(new = TRUE)
roc.curve(creditValid$Attrition_Flag, rf_smote_prob$Attrited.Customer, plotit = TRUE, col = "skyblue")
    # Area under the curve (AUC): 0.600
par(new = TRUE)
roc.curve(creditValid$Attrition_Flag, rf_wt_prob$Attrited.Customer, plotit = TRUE, col = "gold")
    # Area under the curve (AUC): 0.597

par(mfrow = c(2, 4))

roc.curve(creditValid$Attrition_Flag, rf_raw_prob$Attrited.Customer, plotit = TRUE, col = "green", main = "ROC curve with original data")
roc.curve(creditValid$Attrition_Flag, rf_under_prob$Attrited.Customer, plotit = TRUE, col = "red", main = "ROC curve with under sampling data")
roc.curve(creditValid$Attrition_Flag, rf_over_prob$Attrited.Customer, plotit = TRUE, col = "blue", main = "ROC curve with over sampling data")
roc.curve(creditValid$Attrition_Flag, rf_both_prob$Attrited.Customer, plotit = TRUE, col = "purple", main = "ROC curve with both sampling data")
roc.curve(creditValid$Attrition_Flag, rf_rose_prob$Attrited.Customer, plotit = TRUE, col = "orange", main = "ROC curve with ROSE data")
roc.curve(creditValid$Attrition_Flag, rf_smote_prob$Attrited.Customer, plotit = TRUE, col = "skyblue", main = "ROC curve with SMOTE data")
roc.curve(creditValid$Attrition_Flag, rf_wt_prob$Attrited.Customer, plotit = TRUE, col = "gold", main = "ROC curve with weighted data")

par(mfrow = c(1, 1))
# dev.off()



# 2.9. Insight ####

'''

클래스가 10:1보다 나쁘지 않게 불균형할 경우 가중치 또는 샘플링 기술을 사용하는데
큰 이점이 없을 수 있다. (출처: McKinsey&Company의 수석 데이터 사이언티스트 깃허브)
단, 오버샘플링이나 언더샘플링의 경우 예측 성능의 편차가 증가한다. 

'''
