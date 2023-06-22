library(dplyr)
library(e1071)
library(readxl)

#데이터 불러오기
path                  <- "C:/workspace/github/r/03machinelearning/data/3.아파트_주소기반_주택지역특성시세정보_DB.xlsx"
sheet.names           <- excel_sheets(path)

df1                   <- read_excel(path, sheet.names[1])
df2                   <- read_excel(path, sheet.names[2])
df3                   <- read_excel(path, sheet.names[3])
df4                   <- read_excel(path, sheet.names[4])
data                  <- Reduce(function(x, y) merge(x, y, all=FALSE), 
                                list(df1, df2, df3, df4))


data$LAND_ACCESS      <- as.numeric(as.character(data$LAND_ACCESS)) 
data$UNIT_NUM         <- as.numeric(as.character(data$UNIT_NUM)) 


# 전체 데이터에서 일부만 선택 
data                  <- data[complete.cases(data), c("SGG","PRICE_GEN", "FLOOR", 
                                                      "PRIV_AREA", "PUB_AREA", 
                                                      "SUM_AREA","UNIT_NUM", 
                                                      "PARK_NUM", "LAND_ACCESS", 
                                                      "SUB_DIST")]   
#데이터 가공 : 데이터가 적은 지역 전처리 (광진구 제거, 종로구와 중구 통합)
svm_data               <- subset(data,!grepl("광진구", SGG)) # 광진구 제외 
svm_data$SGG           <- ifelse(svm_data$SGG == "성동구", "A",
                                ifelse(svm_data$SGG == "용산구", "B", "C"))
table(svm_data$SGG)

# 컬럼 타입 지정 
svm_data$SGG           <- as.factor(as.character(svm_data$SGG)) #값을 문자로 지정. 속성 변화 (옛날 언어는 속성 변경이 어려움움)
#as.factor : 분석 대상이라고 지칭한다. 
head(svm_data)
head(svm_data, 10) #10줄만 나옴 
str(svm_data)
summary(svm_data)

#데이터 분할 (df : data_frame)
set.seed(10)
sample_num <- sample(1:nrow(svm_data), 0.7*nrow(svm_data))
train_df <- svm_data[sample_num,]
test_df <- svm_data[-sample_num,]
x_train <- subset(train_df, select =setdiff(names(svm_data), "SGG"))
x_test <- subset(test_df, select =setdiff(names(svm_data), "SGG"))
nrow(train_df)
head(train_df)

# 모형적합 
fit.svm.linear <- svm(SGG~., data=train_df, kernel = "linear") #svm 모형 생성 
fit.svm.linear # 모형 결과 출력
str(fit.svm.linear) 
summary(fit.svm.linear)$gamma
summary(fit.svm.linear) # cost = 1 : 값이 아주 크면 0가 됨. r는 절편값. 
summary(fit.svm.linear$gamma)

#그래프 그리기 
par(mgp = c(4, 0.5, 0), mar=c(5,5,4,2))
# x,y를 뭐에 놓느냐에 따라 달라짐. 
plot(fit.svm.linear, train_df, PRICE_GEN ~ LAND_ACCESS)
plot(fit.svm.linear, train_df, PRICE_GEN ~ UNIT_NUM)
     

# 모형 튜닝 : 최적의 감마 파라미터 추정 
#선형 svm 튜닝
obj.linear <- tune.svm(as.factor(SGG)~., 
                       data=train_df, kernel = "linear", 
                       gamma=2^(-7:7), cost=2^(-7:7)) 
obj.linear$best.model # tunning 한 베스트 모델 #실행후 오래 걸리지 기다리기 

pred.linear <- predict(obj.linear$best.model, newdata = test_df, 
                       na.action = na.fail)

# 모형 정확도 확인 
table(pred.linear, test_df$SGG) #confusion matrix 출력
acc <-sum(diag(table(pred.linear, test_df$SGG))) / sum(table(pred.linear, test_df$SGG))
cat("모형 정확도 :", round(acc*100, 2), "%", "\n")     

par(mgp =c(4, 0.5, 0), mar= c(5,5,4,2))
plot(obj.linear$best.model, test_df, PRICE_GEN ~ LAND_ACCESS) #c가 좀 줄어듬 
plot(obj.linear$best.model, test_df, PRICE_GEN ~ UNIT_NUM) #c가 좀 늘어남



#Radial kernel

obj.radial            <- tune.svm(as.factor(SGG) ~., data=train_df, kernel="radial", 
                                  gamma=2^(-7:7), cost=2^(-7:7))  
obj.radial$best.model  # tuning 한 best model 출력

pred.radial           <- predict(obj.radial$best.model, test_df)
table(pred.radial, test_df$SGG)  # confusion matrix 출력

acc                   <- sum(diag(table(pred.radial, test_df$SGG))) / 
  sum(table(pred.radial, test_df$SGG))  
cat("모형 정확도 : ", round(acc*100, 2), "%", "\n")



