# 작업 디렉토리 설정
setwd("C:/Rproject")
# csv 형식 데이터 불러오기
data_df <- read.csv(file = "DAT/example_utf8.csv", 
                    header = TRUE, 
                    sep = ",",
                    encoding = "UTF-8")