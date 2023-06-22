#install.packages("ipred")
#install.packages("gbm") # gradient boosting
#install.packages("randomForest")

library(ipred)
library(gbm)
library(randomForest)
library(readxl)

#Define function] Boosting 횟수 M 정하기

find_M <- function(m){
  fit.boost         <- gbm(as.factor(SGG)~.,data=train,distribution="multinomial",
                           n.trees=m)
  pred.prob         <- predict(fit.boost,x_test,type="response",n.trees=m)  #확률로 나옴
  pred.prob         <- matrix(pred.prob, ncol = length(fit.boost$classes))
  pred              <- fit.boost$classes[apply(pred.prob,1,which.max)]
  err_rate          <- mean(pred!=y_test)
  return(err_rate)
}

path                  <- "C:/workspace/github/r/03machinelearning/data/3.아파트_주소기반_주택지역특성시세정보_DB.xlsx"
sheet.names           <- excel_sheets(path)

df1                   <- read_excel(path, sheet.names[1])
df2                   <- read_excel(path, sheet.names[2])
df3                   <- read_excel(path, sheet.names[3])
df4                   <- read_excel(path, sheet.names[4])
data                  <- Reduce(function(x, y) merge(x, y, all=FALSE), 
                                list(df1, df2, df3, df4))

#SGG : 시군구
#PRICE_GEN ; 일반거래시세
#FLOOR : 층명
#PRIV_AREA : 전유면적
#PUB_AREA : 공유면적
#SUM_AREA : 계약 면적
#UNIT_NUM : 총세대수
#PARK_NUM : 총주차대수
#LAND_ACCESS : 개별공시지가
#SUB_DIST : 지하철역과의 거리

data$LAND_ACCESS      <- as.numeric(as.character(data$LAND_ACCESS)) 
data$UNIT_NUM         <- as.numeric(as.character(data$UNIT_NUM)) 

# 전체 데이터에서 일부만 선택 
data                  <- data[complete.cases(data), c("SGG","PRICE_GEN", "FLOOR", 
                                                      "PRIV_AREA", "PUB_AREA", 
                                                      "SUM_AREA","UNIT_NUM", 
                                                      "PARK_NUM", "LAND_ACCESS", 
                                                      "SUB_DIST")]   


en_data               <- subset(data,!grepl("광진구", SGG)) # 광진구 제외 
en_data$SGG           <- ifelse(en_data$SGG == "성동구", "A",
                                ifelse(en_data$SGG == "용산구", "B", "C"))
table(en_data$SGG)
en_data$SGG           <- as.factor(as.character(en_data$SGG))

head(en_data)
str(en_data)
summary(en_data)


set.seed(10)
sample_num <- sample(1:nrow(en_data), 0.7*nrow(en_data))
train <- en_data[sample_num,]
test <- en_data[-sample_num,]
x_train <- train[, -1]
x_test <- test[,-1]
y_train <- train[,1]
y_test <- test[,1]

fit.bagg <- ipredbagg(as.factor(y_train), x_train, data=train, nbagg=1000)
fit.bagg
pred <- predict(fit.bagg, newdata = x_test)
table(pred, y_test) #예측값과 관측값 비교 
cat("오분류율 =", mean(pred!=y_test)*100, "%")

#확률예측 
pred2 <- predict(fit.bagg, x_test, type="prob")
head(pred2)
head(pred)

# Boosting
fit.boost <- gbm(SGG~., data=train, distribution="multinomial", n.trees=500)
summary(fit.boost)

pred.prob <- predict(fit.boost, x_test, type="response", n_trees=500)
pred.prob <- matrix(pred.prob, ncol=3)
pred.prob

colnames(pred.prob) <- levels(y_train)
head(pred.prob)

pred <- apply(pred.prob, 1, which.max)
pred <- ifelse(pred==1, "A", ifelse(pred==2, "B", "C"))
table(pred, y_test)

cat("오분류율 =", mean(pred!=y_test)*100, "%")

find_M(50)


#Random Forest 
set.seed(100)
fit.rf <- randomForest(as.factor(SGG)~., data=train, ntree=1000, mtry=3)
pred <- predict(fit.rf, x_test) #예측값
table(pred, y_test)
cat("오분류율 =", mean(pred!=y_test)*100, "%")

MAPE <- NULL
for(i in 1:ncol(x_train)) {
  temp_rf <- randomForest(as.factor(SGG)~., data=train, ntree=1000, mtry=i)
  pred <- predict(temp_rf, x_test)
  MAPE[i] <- mean(pred!=y_test)
}
plot(x=seq(1, ncol(x_train)), y=MAPE, type="l", xlab="변수개수")
