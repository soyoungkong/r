library("e1071")
data(iris)
 
svm.e1071 <- svm(Species ~ . , data = iris,
                  type = "C-classification", kernel = "radial",
                  cost = 10, gamma = 0.1) #코스트가 1보다 크면 꽤 큰 값 
 
summary(svm.e1071)
 
plot(svm.e1071, iris, Petal.Width ~ Petal.Length,
      slice = list(Sepal.Width = 3, Sepal.Length = 4))
 
plot(svm.e1071, iris, Sepal.Width ~ Sepal.Length,
      slice = list(Petal.Width = 2.5, Petal.Length = 3))
# slice= 변수가 2개 이상일 때 상수값을 할당함(assign)
# 아래 그림에서 x: 서포트벡터, o:데이터 점을 나타냄
 
pred <- predict(svm.e1071, iris, decision.values = TRUE)
(acc <- table(pred, iris$Species))
 
classAgreement(acc)
 

# tune() 함수는 제공된 모수 영역에서 격자 탐색을 #사용하여 통계적 방법의 초모수
#(hyperparameters)를 조절(tune)할 수 있다. 이 함수는 #최적의 모수를 제공해 주며, 동시에 여러
#모수 값에 대한 검정에 대한 자세한 결과를 제공해 준다.

tuned <- tune.svm(Species~., data = iris, gamma = 10^(-6:-1), cost = 10^(1:2))
# 6× 2 = 12개의 조합에서 모수조율이 이루어짐
summary(tuned) #튜닝 속도가 빠름 


# R 패키지 {kernlab}의 ksvm() 함수를 이용하여 SVM 분류를 수행

library("kernlab")
data(iris)
svm.kernlab <- ksvm(Species ~ ., data = iris, type = "C-bsvc",
                      kernel = "rbfdot", kpar = list(sigma = 0.1),
                      C = 10, prob.model = TRUE)
svm.kernlab
fit <- fitted(svm.kernlab)

# plot() 함수를 이용하여 분류된 결과에 대한 각 변수별 분포를 상자그림의 형태로 나타낼 수 있다
par(mfrow=c(2,2))
plot(fit, iris[,1], main="Sepal.Length")
plot(fit, iris[,2], main="Sepal.Width")
plot(fit, iris[,3], main="Petal.Length")
plot(fit, iris[,4], main="Petal.Width")
par(mfrow=c(1,1))

head(predict(svm.kernlab, iris, type= "probabilities"))

head(predict(svm.kernlab, iris, type = "decision"))

table(predict(svm.kernlab, iris), iris[,5]) #에러 3개. 커널을 바꿨는데도. 

#predict() 함수를 통해 새로운 자료에 대한 분류(예측)을 수행 할 수 있다. 여기서는 모형 구축
#에 사용된 자료를 재사용하여 분류를 수행하였다


# Tunning : 회귀분석 한 것 
# 분석용 자료 생성
x <-c(1:20)
y <- c(3,4,8,4,6,9,8,12,15,26,35,40,45,54,49,59,60,62,63,68)
data<-data.frame(x, y)

plot(data, pch=16)
model <- lm(y ~ x, data)
abline(model)
lm.error <- model$residuals # same as data$Y - predictedY
(lmRMSE <- sqrt(mean(lm.error^2)))

# svm() 함수를 이용하여 SVR 실행

model <- svm(y ~ x , data)
pred.y <- predict(model, data)
points(data$x, pred.y, col = "red", pch=4) # pch=4는 ‘x’임

error <- data$y - pred.y
svmRMSE <- sqrt(mean(error^2))
svmRMSE

# model selection

tuneResult <- tune(svm, y ~ x, data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
# 11× 8 = 88 개 모수 조합에서 조율
print(tuneResult)

 tunedModel <- tuneResult$best.model
 tpred.y <- predict(tunedModel, data)
 error <- data$y - tpred.y
 tsvmRMSE <- sqrt(mean(error^2))
 tsvmRMSE
 
 plot(data, pch=16)
 points(data$x, pred.y, col = "red", pch=4, type="b")
 points(data$x, tpred.y, col = "blue", pch=4, type="b")
 
 