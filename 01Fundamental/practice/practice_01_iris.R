## 아이리스 데이터 실습 

#패키지 설치 : 최초 1회 수행 
install.packages("psych")

#패키지 로딩 
library(psych)

#패키지 확인 
search()

#작업 디렉토리 설정
setwd("C:/workspace/github/r/01Fundamental")

#행과 열을 갖는 데이터프레임임
iris_df <- read.csv(file = "data/DAT/iris_data.csv",
                    header = TRUE,
                    sep = ",",
                    stringsAsFactors = FALSE)

#iris 데이터에 대한 데이터 요약
describe(iris_df)

#품종에 따른 데이터 요약 
describeBy(iris_df[, c(1:4)], iris_df$Species)
