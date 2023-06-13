# 패키지 설치 : 최초 1회만 수행
install.packages("dplyr")

# 패키지 로딩
library(dplyr)
# 패키지 확인
search()


# 작업 디렉토리 설정
setwd("C:/Rproject")
# 데이터 불러오기
iris_df <- read.csv(file = "DAT/iris_data.csv",
                    stringsAsFactors = TRUE)
# dataframe 형식에서 tibble 형식으로 바꿔 저장
iris_tbl_df <- as_tibble(iris_df)
iris_tbl_df


# select()
iris_tbl_df %>%
  select(Sepal.Length, Sepal.Width, Species)

iris_tbl_df %>%
  select(!(Species))


# filter()
iris_tbl_df %>%
  filter(Species == "setosa")

iris_tbl_df %>%
  filter(Sepal.Length < 5)


# arrange()
iris_tbl_df %>%
  arrange(Sepal.Length)

iris_tbl_df %>%
  arrange(Sepal.Length, desc(Sepal.Width))


# rename()
iris_tbl_df %>%
  rename(Iris.species = Species)


# mutate()
iris_tbl_df %>%
  mutate(New_col = Sepal.Length * 2)


# summarise()
iris_tbl_df %>% 
  summarise(mean.SL = mean(Sepal.Length))

iris_tbl_df %>% 
  summarise(mean.SL = mean(Sepal.Length), 
            mean.SW = mean(Sepal.Width),
            mean.PL = mean(Petal.Length),
            mean.PW = mean(Petal.Length))


# group_by()
iris_tbl_df %>% 
  group_by(Species) %>%
  summarise(mean.SL = mean(Sepal.Length))

iris_tbl_df %>% 
  group_by(Species) %>%
  summarise(mean.SL = mean(Sepal.Length), 
            mean.SW = mean(Sepal.Width),
            mean.PL = mean(Petal.Length),
            mean.PW = mean(Petal.Length))

