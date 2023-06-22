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



#Sigmoid kernel 

obj.sigmoid <- tune.svm(as.factor(SGG)~., data=train_df, kernel="sigmoid",
                         gamma=2^(-7:7), cost=2^(-7:7))

obj.sigmoid$best.model
pred.sigmoid <- predict(obj.sigmoid$best.model, test_df)
table(pred.sigmoid, test_df$SGG)  # confusion matrix 출력

acc                   <- sum(diag(table(pred.sigmoid, test_df$SGG))) / 
  sum(table(pred.sigmoid, test_df$SGG))
cat("모형 정확도 : ", round(acc*100, 2), "%", "\n")


par(mgp =c(4, 0.5, 0), mar= c(5,5,4,2))
plot(obj.sigmoid$best.model, test_df, PRICE_GEN ~ LAND_ACCESS) #c가 좀 줄어듬 
plot(obj.sigmoid$best.model, test_df, PRICE_GEN ~ UNIT_NUM) #c가 좀 늘어남


#polynominal

obj.polynominal <- tune.svm(as.factor(SGG)~., data=train_df, kernel="polynomial",
                           gamma=2^(-7:7), cost=2^(-7:7))

obj.polynominal
obj.polynominal$best.model
pred.polynominal <- predict(obj.polynominal$best.model, test_df)
table(pred.polynominal, test_df$SGG)  # confusion matrix 출력
acc                   <- sum(diag(table(pred.polynominal, test_df$SGG))) / 
  sum(table(pred.sigmoid, test_df$SGG))
cat("모형 정확도 : ", round(acc*100, 2), "%", "\n")

par(mgp =c(4, 0.5, 0), mar= c(5,5,4,2))
plot(obj.polynominal$best.model, test_df, PRICE_GEN ~ LAND_ACCESS) #c가 좀 줄어듬 
plot(obj.polynominal$best.model, test_df, PRICE_GEN ~ UNIT_NUM) #c가 좀 늘어남
