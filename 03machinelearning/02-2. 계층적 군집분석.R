#install.packages("kohonen")
#install.packages("dbscan")
library(kohonen)
library(dbscan)
library(readxl)
library(tidyverse)
options(repr.plot.width = 5, repr.plot.height = 4) #그림 크기 옵션
path                  <- "C:/workspace/github/r/03machinelearning/data/3.아파트_주소기반_주택지역특성시세정보_DB.xlsx"
sheet.names           <- excel_sheets(path)

df1                   <- read_excel(path, sheet.names[1])
df2                   <- read_excel(path, sheet.names[2])
df3                   <- read_excel(path, sheet.names[3])
df4                   <- read_excel(path, sheet.names[4])
data                  <- Reduce(function(x, y) merge(x, y, all=FALSE), 
                                list(df1, df2, df3, df4))

data$LAND_ACCESS      <- as.numeric(as.character(data$LAND_ACCESS))
data                  <- data[complete.cases(data), ]

gu_rows               <- data$SGG == "용산구"  # 용산구에 해당하는 데이터 행 
gu_data               <- data[gu_rows, ]

EMD_data              <- gu_data[!duplicated(gu_data$EMD), c(8, 52, 53)] 
head(EMD_data)


## 계층적 군집분석
attach(EMD_data)
x                     <- EMD_data[, 2:3]
D1                    <- dist(x)
D1 

# 유사도 거리계산 : 맨하탄거리
D2                    <- dist(x, method = "manhattan") 
D2

#[계층적 군집 분석] 최단 연결법
hc1                   <- hclust(dist(x)^2, method = "single")
plot(hc1, labels = EMD, hang = 1, main = "dendrogram : 최단 연결법")


#[계층적 군집 분석] 최장 연결법
hc2                   <- hclust(dist(x)^2, method = "complete")   
plot(hc2, labels = EMD, hang=1, main = "dendrogram : 최장 연결법")


#[군집 설정 및 시각화] 군집 수 설정 후 그룹 별 산점도로 결과 확인 : 최단 연결법
c1.num                <- 2  
hc1.result            <- cutree(hc1, k = c1.num) #최단 연결법 결과
plot(x, pch = hc1.result, col = hc1.result, main = "single")
text(x, labels = EMD, adj = -0.1, cex = 0.8)


#[군집 설정 및 시각화] 군집 수 설정 후 그룹 별 산점도로 결과 확인 : 최장 연결법
hc2.result            <- cutree(hc2, k = c1.num) 
plot(x, pch =  hc2.result, col = hc2.result, main = "complete")
text(x, labels = EMD, adj = -0.1, cex = 0.8)



#### 비계층적 군집분석 
data_k                <- kmeans(x ,centers = 3) 
attributes(data_k)
data_k$cluster

#[군집화] grouping
clus                  <- cbind(EMD_data, x, data_k$cluster)
clus1                 <- clus[(clus[,4] == 1), ]
clus2                 <- clus[(clus[,4] == 2), ]
clus3                 <- clus[(clus[,4] == 3), ]
kc                    <- table(data_k$cluster)
plot(x, pch = data_k$cluster, col = data_k$cluster, main = "K-means clustering")
text(x, labels = EMD, adj = -0.1, cex = 0.8)

# SOM(Self-Organization-Map)
#[데이터 호출]
som_data              <- data[!duplicated(EMD), c(48:53)]
head(som_data, 5)
