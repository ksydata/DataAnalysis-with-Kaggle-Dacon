
# 통계분석실습 3차 과제 1번 ####

library(dplyr)
library(ggplot2)
library(caret)
library(MLmetrics)
library(cvTools)
library(rpart)
library(rpart.plot)
library(e1071)
library(gbm)


library(survival)
clean_colon = na.omit(colon)
  # [전처리] 결측값 제거
clean_colon = clean_colon[c(TRUE, FALSE), ]
  # [전처리] 홀수 번째 데이터 추출
clean_colon$status <- factor(clean_colon$status)

set.seed(123)
training.samples <- clean_colon$status %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- clean_colon[training.samples, ]
test.data <- clean_colon[-training.samples, ]
table(clean_colon$status == "1") / nrow(clean_colon)
table(clean_colon[training.samples, "status"])
table(clean_colon[-training.samples, "status"])
  # [완치와 재발/사망 class 변수의 비율] 0.5158 : 0.4842 
ggplot(data = clean_colon, aes(x = time, y= status, col = status)) + geom_jitter()
  # [재발시점부터 사망시점까지 경과일을 나타내는 time 변수의 중요도] 
  # [완치 여부를 나타내는 status를 분류하는 문제]
train.data[, c(2, 15, 16)] <- NULL
test.data[, c(2, 16)] <- NULL
  # [study, time, etype 변수 모델 학습에서 제외]

set.seed(123)
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
  # [5겹 k-폴드 교차검증]
names(getModelInfo())
  # http://topepo.github.io/caret/train-models-by-tag.html


# [환자의 재발진단에서 오진(FN 거짓음성 비율)을 줄이기 위한 성능평가 지표] 민감도 Recall 채택
# [환자의 재발진단에서 재발환자를 맞추기(TP 참양성 비율) 위한 성능평가 지표]


# 1.1. binary logistic regression ####

set.seed(123)
glm <- train(status ~ ., 
             data = train.data[, -1], 
             trControl = train.control,
             method = "glm")
glm$finalModel
glm_pred <- predict(glm, newdata = test.data[, -1, 15], positive = "1")
test.data$glm_pred <- glm_pred
confusionMatrix(test.data$status, test.data$glm_pred, positive = "1")
Recall(test.data$status, test.data$glm_pred, positive = "1")
  # [Recall] 0.6279
  # [Accuracy] 0.6723

varImp(glm)


# 1.2. decision tree ####

set.seed(123)
tree_sample <- rpart(status ~ ., data = train.data[, -1], control = rpart.control(cp = 0))
tree_sample$cptable
  # [xerror + xstd가 최소인 cp] 0.0189

tree <- train(status ~ ., 
              data = train.data[, -1], 
              trControl = train.control,
              method = "rpart", 
              cp = 0.0189
)
tree$bestTune
rpart.plot(tree$finalModel)
tree_pred <- predict(tree, newdata = test.data[, -c(1, 14, 15)], positive = "1")
test.data$tree_pred <- tree_pred
confusionMatrix(test.data$status, test.data$tree_pred, positive = "1")
Recall(test.data$status, test.data$tree_pred, positive = "1")
  # [Recall] 0.7093
  # [Accuracy] 0.6667


# 1.3. bagged CART ####

set.seed(123)
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 200)
bagging <- train(status ~ ., 
                 data = train.data[, -1], 
                 trControl = train.control,
                 method = "treebag"
)
bagging$modelInfo
bagging_pred <- predict(bagging, newdata = test.data[, -c(1, 14, 15, 16)], positive = "1")
test.data$bagging_pred <- bagging_pred
confusionMatrix(test.data$status, test.data$bagging_pred, positive = "1")
Recall(test.data$status, test.data$bagging_pred, positive = "1")
  # [Recall] 0.7093
  # [Accuracy] 0.6102


# 1.4. random forest ####

set.seed(123)
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
randomforset <- train(status ~ ., 
                      data = train.data[, -1], 
                      trControl = train.control,
                      method = "rf"
)
randomforset$finalModel
rf_pred <- predict(randomforset, newdata = test.data[, -c(1, 14, 15, 16, 17)], positive = "1")
test.data$rf_pred <- rf_pred
confusionMatrix(test.data$status, test.data$rf_pred, positive = "1")
Recall(test.data$status, test.data$rf_pred, positive = "1")
  # [Recall] 0.6046512
  # [Accuracy] 0.661


# 1.5. Stochastic Gradient Boosting ####

'''
[Error]
ntree.grid <- expand.grid(n.trees = seq(10, 100, 10))
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
sgbm <- train(status ~ ., 
             data = train.data[, -1], 
             trControl = train.control,
             method = "gbm",
             # shrinkage = 0.1, 
             # interaction.depth = 1, 
             # n.minobsinnode = 10, 
             # n.trees = 150
)
warnings()
sgbm$finalModel
sgb_pred <- predict(sgbm, newdata = test.data[, -c(1, 14, 15, 16, 17)], positive = "1")
test.data$sgb_pred <- sgb_pred
confusionMatrix(test.data$status, test.data$sgb_pred, positive = "1")
Recall(test.data$status, test.data$sgb_pred, positive = "1")
  # [Recall]
  # [Accuracy] 
ggplot(data = test.data) + geom_jitter(aes(x = sgb_pred, y= status, col = status))
'''


# 1.6. Linear Support Vector Machines ####

set.seed(123)
tuneControl <- tune.control(sampling = "cross", cross = 5, nrepeat = 3)
linear.svm <- tune.svm(status ~ ., data = train.data[, -1],
                       kernel = "linear", cost = seq(0.1, 3.0, 0.1), 
                       tunecontrol = tuneControl)
summary(linear.svm)
linear.svm$best.model
  # [best cost = 0.1] [support vector 581 points]

linear.svm <- tune.svm(status ~ ., data = train.data[, -1],
                       kernel = "linear", cost = seq(0.001, 0.1, 0.001), 
                       tunecontrol = tuneControl)
summary(linear.svm)
linear.svm$best.model
  # [best cost = 0.1] [support vector 656 points]

linear.svm <- tune.svm(status ~ ., data = train.data[, -1],
                       kernel = "linear", cost = 0.1, 
                       tunecontrol = tuneControl)
linear.svm$best.model
svm_pred <- predict(linear.svm$best.model, newdata = test.data[, -c(1, 14, 15, 16, 17, 18)], positive = "1")
test.data$svm_pred <- svm_pred
confusionMatrix(test.data$status, test.data$svm_pred, positive = "1")
Recall(test.data$status, test.data$svm_pred, positive = "1")
  # [Recall] 0.5814
  # [Accuracy] 0.6667


# 1.7. Machine learner decision ####

ggplot(data = test.data) + geom_jitter(aes(y = status, x= time, col = status))
ggplot(data = test.data) + geom_jitter(aes(y = glm_pred, x= time, col = glm_pred))
ggplot(data = test.data) + geom_jitter(aes(y = tree_pred, x= time, col = tree_pred))
ggplot(data = test.data) + geom_jitter(aes(y = bagging_pred, x= time, col = bagging_pred))
ggplot(data = test.data) + geom_jitter(aes(y = rf_pred, x= time, col = rf_pred))
ggplot(data = test.data) + geom_jitter(aes(y = svm_pred, x= time, col = svm_pred))


Recall(test.data$status, test.data$glm_pred, positive = "1")
  # 0.627907
Recall(test.data$status, test.data$tree_pred, positive = "1")
  # 0.7093023
Recall(test.data$status, test.data$bagging_pred, positive = "1")
  # 0.7093023
Recall(test.data$status, test.data$rf_pred, positive = "1")
  # 0.6046512
Recall(test.data$status, test.data$svm_pred, positive = "1")
  # 0.5813953


bagging_tune <- predict(bagging, newdata = test.data[, -c(1, 14, 15, 16, 17, 18, 19)], positive = "1", type = "prob")
bagging_tune <- data.frame(bagging_tune)

test.data$bagging_tune <- ifelse(bagging_tune$X1 >= 0.35, "1", "0")
test.data$bagging_tune <- as.factor(test.data$bagging_tune)
confusionMatrix(test.data$status, test.data$bagging_tune, positive = "1")
Recall(test.data$status, test.data$bagging_tune, positive = "1")
  # [Recall] 0.7093023 -> 0.8837209
  # [Accuracy] 0.6102 -> 0.6215
