library(e1071)
library(readxl)
library(rpart)
library(class)
library(readxl)
path <- "C:/workspace/github/r/03machinelearning/data/3.아파트_주소기반_주택지역특성시세정보_DB.xlsx"

sheet.names <- excel_sheets(path)
df1 <- read_excel(path, sheet.names[1])
df2 <- read_excel(path, sheet.names[2])
df3 <- read_excel(path, sheet.names[3])
df4 <- read_excel(path, sheet.names[4])
data <- Reduce(function(x,y) merge(x,y,all=FALSE), list(df1, df2, df3, df4))
head(data)
data$LAND_ACCESS      <- as.numeric(as.character(data$LAND_ACCESS)) 

data$UNIT_NUM         <- as.numeric(as.character(data$UNIT_NUM))
data                  <- data[complete.cases(data), c("SGG","PRICE_GEN", "FLOOR",
                                                      "PRIV_AREA", "PUB_AREA",
                                                      "SUM_AREA","UNIT_NUM",
                                                      "PARK_NUM", "LAND_ACCESS",
                                                      "SUB_DIST")]   

head(data, 5)
                    

#[데이터 가공] 데이터가 적은 지역 전처리 (광진구 제거, 종로구와 중구 통합)

nb_data               <- subset(data,!grepl("광진구", SGG))
nb_data$SGG           <- ifelse(nb_data$SGG == "성동구", "A",
                                ifelse(nb_data$SGG == "용산구", "B", "C"))
table(nb_data$SGG)                

# column type 지정
nb_data$SGG           <- as.factor(as.character(nb_data$SGG))

head(nb_data)

#[데이터 분할] training data: 70% / test data : 30%
  
  set.seed(10)
sample_num            <- sample(1:nrow(nb_data), 0.7*nrow(nb_data))
train_df              <- nb_data[sample_num,]
test_df               <- nb_data[-sample_num,]

head(train_df)

model                 <- naiveBayes(SGG ~ ., data = train_df)

#[예측] 확률 예측
pred_prob             <- predict(model, test_df, type = "raw")
head(pred_prob)

#[예측] Class 예측

pred_class            <- predict(model, test_df, type = "class")
head(pred_class)

#[결과] 예측값 실제값 비교
table(pred_class, test_df$SGG)

cat("분류 정확도 = ", mean(pred_class == test_df$SGG)*100, "%")
