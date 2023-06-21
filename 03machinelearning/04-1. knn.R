#install.packages("readxl")
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
str(data) #500 obs. of  99 variables, 500개중 99개만 추려냄
data$LAND_ACCESS <- as.numeric(as.character(data$LAND_ACCESS))
data$UNIT_NUM <- as.numeric(as.character(data$UNIT_NUM))
data <- data[complete.cases(data), c("SGG", "PRICE_GEN", "FLOOR", "PRIV_AREA", "PUB_AREA", "SUM_AREA", "UNIT_NUM",
                                     "PARK_NUM", "LAND_ACCESS", "SUB_DIST")]
head(data, 5)
str(data) #간추려져서 나옴 'data.frame':	492 obs. of  10 variables: 492개 중 10개만 추려냄

#통합 전에 확인 
table(data$SGG) #table : 빈도수 확인 

#=================================================================
#KNN
#=================================================================
knn_data <- subset(data, !grepl("광진구", SGG)) #광진구 제외 
#성동구 : A, 용산구 : B, 그 외 : C
knn_data$SGG <- ifelse(knn_data$SGG == "성동구", "A",
                       ifelse(knn_data$SGG =="용산구", "B", "C"))
table(knn_data$SGG) 

knn_data <- as.factor(as.character(knn_data$SGG))
#head(knn_data) 
#tail(knn_data)

#데이터 분할
set.seed(10)
sample_num            <- sample(1:nrow(knn_data), 0.7*nrow(knn_data))
train_df              <- knn_data[sample_num,]
test_df               <- knn_data[-sample_num,]
x_train               <- subset(train_df, select = setdiff(names(knn_data), "SGG"))
x_test                <- subset(test_df,  select = setdiff(names(knn_data), "SGG"))
y_train               <- train_df$SGG
y_test                <- test_df$SGG

#decistion_tree <- rpart(play ~ Weather + Temperature + Humidity, data = weather, method = "class")
`)
