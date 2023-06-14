# =====================
# 모집단과 표본
# =====================
# =====================
# 1. 단순랜덤추출
# =====================
# - X가 1~100까지의 값을 가진다고 하자. 
# - 1. 비복원추출로 10개의 sample 데이터를 추출해 보자.
# - 2. 복원추출로 10개의 sample 데이터를 추출해 보자.

# 참고) sample 함수로 prob인자를 사용하면 가중치를 이용한 표본추출을 할 수 있다. 

# 1-1) 비복원추출
x <- c(1:100)
sample(x, size = 10, replace = FALSE) # default 

# 1-2) 복원추출
sample(x, size = 10, replace = TRUE)

# 참고) 가중치를 이용한 표본추출
sample(x, size = 10, prob = 1:100)

# =====================
# 2. 층화추출
# =====================
# - 'iris'데이터에서 'Species'에 따라 단순랜덤추출을 3개씩 해 보자.
# install.packages("sampling")
library(sampling)
m <- strata(data = iris,      # 층화추출 함수
	   stratanames = "Species",	# 층화추출에 사용될 변수
	   size = c(3,3,3),		# 각 층의 크기
	   method = "srswor")	# 비복원 단순임의추출 (srswr 복원 단순임의추출)

sampling_data1 <- getdata(iris, m)
sampling_data1

# =====================
# 위치 측도와 산포 측도
# =====================
#-시험성적 데이터에 대한 위치측도와 산포측도를 확인해 보자.

#시험 성적
exam <- c(50,53,53,55,55,72,73,73,73,73,58,58,59,
          59,60,73,73,73,74,74,60,62,62,62,62,75,75,76,76,
          77,63,64,64,65,65,77,77,78,78,78,66,66,66,67,67,
          78,79,79,79,80,67,67,67,67,67,67,80,71,82,82,83,
          68,68,68,68,69,84,84,84,86,87,70,70,70,71,72,89,90,91,92,98) 

#plot
plot(exam)

#기본 정보
summary(exam)

### 위치 측도 ###
#표본평균
mean(exam)
#중앙값
median(exam)
#p% 분위수
p <- 0.5
quantile(exam,p) # = 중앙값

### 산포 측도 ###
#표본분산
var(exam)
#표본표준편차
sd(exam)
#범위
range(exam)
#사분위수 범위
IQR(exam)

loc.para <- data.frame(mean=mean(exam), median=median(exam), percentile_20=quantile(exam,0.2))
dis.para <- data.frame(variance=var(exam), sd=sd(exam), mad=mad(exam), IQR=IQR(exam))

#==========================
# 도수분포표와 줄기 잎 그림 : 수치형 데이터의 분포 시각화 (히스토그램, density plot 등)
#==========================
#-시험성적 데이터에 대한 도수분포표 및 줄기 잎 그림을 확인해 보자.
#도수분포표 (cut함수와 table함수로 구할 수 있다)
#50부터 100까지 계급의 크기는 5로
frequency.table <- cut(exam, breaks = seq(50,100,5)) # 계급 구간
table(frequency.table)

#줄기 잎 그림 (stem 함수를 이용해서 그릴 수 있다)
sort(exam)
stem(exam) #6~7에 데이터가 모여있다 
stem(exam, scale = 0.5)


#===================
# 상관계수와 공분산
#===================
#데이터
x <- c(10,15,16,1,4,6,18,12,14,7)
y <- c(5,2,1,9,7,8,1,5,3,6)

#산점도
plot(x,y)

#표본상관계수
cor(x,y)

#표본공분산 (상관계수 * 표준편차)
cov(x,y)
cor(x,y)*sd(x)*sd(y)


#=============
# 왜도와 첨도
#=============
#e1071 패키지가 필요하다면 설치
#install.packages('e1071')
library(e1071)

#올드페이스풀 간헐천에서 분출 시간(분)을 나타냄
erup <- faithful$eruptions

#왜도
skewness(erup)
#첨도
kurtosis(erup)
#히스토그램
hist(erup) #대칭이 아님

# 왜도가 0보다 작으므로 좌측 긴 꼬리를 가진다고 유추할 수 있다.
# 첨도가 0보다 작으므로 정규분포보다 납작하다고 유추할 수 있다.
# 히스토그램을 통해 양봉을 가진 그래프로 확인할 수 있다. 


#===========
# 히스토그램 + 확률밀도함수 추정값
#===========
#e1071 패키지가 필요하다면 설치
# install.packages('e1071')
# install.packages('ggplot2')
library(e1071)
library(ggplot2)
#올드페이스풀간헐천에서 분출 시간(분)을 나타냄
#faithful 데이터의 eruption 변수를 사용
#ggplot에서는 기본 벡터로는 그려지지 않으므로 데이터프레임을 사용해야함

#빈도 히스토그램
#binwidth에서 계급의 폭 결정할 수 있음
ggplot(faithful, aes(x=eruptions)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")

#확률 분포 히스토그램
#aes(y=..density..)으로 Y축에는 상대도수가 나오며 
#geom_density()에 의해 확률밀도함수의 추정 값이 그려짐
ggplot(faithful, aes(x=eruptions)) +
  geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") + 
  geom_density()

#=========
# 상자그림
#=========
#e1071 패키지가 필요하다면 설치
#install.packages('e1071')
library(e1071)
library(ggplot2)
#올드페이스풀간헐천에서 분출 시간(분)을 나타냄
#faithful 데이터의 eruption 변수를 사용
#ggplot에서는 기본 벡터로는 그려지지 않으므로 데이터프레임을 사용해야함

#상자그림
ggplot(faithful, aes(x=1, y=eruptions)) + geom_boxplot()
#상자그림에서 나타내는 것
summary(faithful$eruptions)

#여러 그룹의 상자그림을 한번에 나타내기
#데이터는 표준정규분포에서 생성
set.seed(1234)
dat1 <- data.frame(Cond = factor(rep(c("A","B","C"), each=200)), 
                  Value = rnorm(600))

#Cond 그룹에 세개의 상자그림. 색은 fill=Cond 옵션에 의해 구분된 것
ggplot(dat1, aes(x=Cond, y=Value, fill=Cond)) + geom_boxplot()

#여러 그룹의 상자그림을 한번에 나타내기 (평균, 표준편차 다르게)
#데이터는 표준정규분포에서 생성
set.seed(1234)
dat2 <- data.frame(Cond = factor(rep(c("A","B","C"), each=200)), 
                  Value = c(rnorm(200), rnorm(200,1,1), rnorm(200, -1, 2)))

#Cond 그룹에 세개의 상자그림. 색은 fill=Cond 옵션에 의해 구분된 것
ggplot(dat2, aes(x=Cond, y=Value, fill=Cond)) + geom_boxplot()


