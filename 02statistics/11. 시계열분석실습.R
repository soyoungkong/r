#==================================================================
# title: "시계열 분석"
# subtitle: "Time Series Analysis"
# author: "Begas"
#==================================================================

## 목차============================================================
# 1.패키지 설치 및 로드
# 2.EDA
# 3.시계열분해 및 회귀분석을 이용한 예측
# 4.지수평활을 이용한 예측
# 5.이동평균을 이용한 예측
# 6.ARIMA를 이용한 예측
##=================================================================

# 1. install/load packages

# install.packages('forecast')
# install.packages('tseries')
# install.packages('ggplot2')
# install.packages('reshape')
# install.packages('zoo')
# install.packages('fUnitRoots')

library(forecast)
library(tseries)
library(ggplot2)
library(reshape)
library(zoo)
library(fUnitRoots)


#============================================================
# Data Load
# - 1949년 ~ 1960년 까지의 월별 비행기 탑승 고객 수
#============================================================ 

origin <- AirPassengers
origin

class(origin) # ts객체

# ts객체에만 존재하는 고유함수가 존재합니다.
start(origin)
end(origin)
frequency(origin) # 비슷한 패턴 주기 
cycle(origin)

# ts함수
# ts(data, start, end, frequency)
# data: 시계열 데이터로 변환 시키고자 하는 벡터
# start: 자료의 시작 시점 지정
# end: 자료의 끝점 지점
# freq: 단위시간 당 관측 수

# ts객체로 변환
ex1 <- rnorm(144) #임의 144개 데이터. 정규분포 144개를 뽑는다. 
class(ex1)

ex1 <- ts(ex1, start=c(2023,1), frequency = 12)
class(ex1)
plot(stl(ex1, s.window=12))

#============================================================
# EDA
#============================================================ 

# 시도표
plot(origin) # 점점 증가 


#분산 안정화를 위한 BoxCox 변환 (혹은 로그 변환) 제일 먼저 해줌
lambda <- BoxCox.lambda(origin) #정확하게 하기 위해서 람다 값을 씀.
tran_org <- BoxCox(origin, BoxCox.lambda(origin)) #추천받은 람다 값으로 함
#tran_org <- Boxcorx(origin, lambda)로 해도 됨. 
plot(tran_org) #분산이 일정해짐 

par(mfrow=c(1,2)) #창 나누기 
plot(origin)
plot(tran_org) #분산이 일정해짐 (상하 간격)
par(mfrow=c(1.1)) #창 되돌리기

# 정규성 및 Corr
# Hist Plot
hist(tran_org,prob=TRUE,12)
lines(density(tran_org))

# Q-Q PLOT
qqnorm(tran_org)
qqline(tran_org)

# 상관관계 확인
lag.plot(tran_org,12,do.lines=FALSE)


#============================================================
# 시계열 분해 및 회귀분석 이용 예측
# 시계열의4가지 요소 
#============================================================ 

#분해법 : 가법모형 (계절요인의 분산이 일정할 때) 
# y_t = season_t + trend_t + noise (cycle 요소는 찾기 힘드므로 2,3년 동안의 주기적 패턴이 나올 떄만 확인. 여기서는 무시 )
stl_tran_org <- stl(tran_org, s.window = 12)
plot(stl_tran_org) # 두번쨰 계절성, 세번쨰 추세, 네번쨰 2-3뺸 나머지

y <- as.data.frame(stl_tran_org$time.series)
y1 <- y$seasonal # seasonal
y2 <- y$trend # trend
y3 <- y$remainder # remainder

plot(y1+y2+y3, type='l')

# 회귀모형 
# 계절형 Dummy 변수 생성
M <- factor(cycle(tran_org))
M
stl_tran_org_df <- as.data.frame(stl_tran_org$time.series)
head(stl_tran_org_df) # 데이터 프레임 

# 회귀 모형 생성
# 모형식 : tran_org=trend∗β1+M1∗β2+...+M12∗β12+ϵ
# 뒤에는 나머지 값 trend+M, 0은 절편이 없다. 
model_stl <- lm(formula = tran_org~0+ stl_tran_org_df$trend+M, na.action = NULL)
summary(model_stl)

# 잔차 검정
# time Plot
plot(resid(model_stl))
abline(h=0, col='grey', lty=2, lwd=2)

par(mfrow=c(1,2))
# Hist Plot
hist(resid(model_stl),prob=TRUE,12, main = "Histogram of residuals")
lines(density(resid(model_stl)), col='red', lwd=2)
# Q-Q PLOT
qqnorm(resid(model_stl))
qqline(resid(model_stl))
par(mfrow=c(1,1))


# DW test
library(lmtest)
dwtest(model_stl) # white noise X, p값이 낮음. 귀무가설 기각. 
#독립성을 만족하지 않음. 회귀분석시 잔차분석과 맞지 않음. 
#mse를 최소로 한 값이라고 볼 수 없음. 


# 회귀모형 예측 결과 확인
plot(spline(time(origin), origin),type='l',xlab='Time',ylab='Pop')

# 원 데이터 및 fitted 데이터의 비교
# BoxCox 역변환 필요함
lines(InvBoxCox(model_stl$fitted.values, lambda = BoxCox.lambda(origin)), col='red')

# mse
mean((origin - InvBoxCox(model_stl$fitted.values, lambda = BoxCox.lambda(origin)))^2,na.rm = TRUE)  #MSE


#============================================================
# 지수평활을 이용한 예측
#============================================================
# 1. 단일지수 평활: 추세나 계절적 변동이 없는 시계열 예측에 사용.
# 2. 이중지수 평활: 추세가 있는 시계열 예측에 사용 
# 3. Holt-Winters: 추세와 계절요인이 있는 시계열 예측에 사용.

plot(stl(origin, s.window=12))
# Trend 및 Seasonality 존재
# Holt-Winter 지수평활 모형이 적합

# 이분산성을 띄므로 승법모형을 씀 (디폴트는 가법모형)
#HoltWinters 모형 생성
model_es <- HoltWinters(origin, seasonal = "multiplicative") # 승법모형을 적용 
#model_es <- HoltWinters(origin, beta=F, gamma=F, seasonal = "multiplicative") #단순지수 평활 모형을 사용하고 싶다면, beta=F, gamma=F 추가.
#model_es <- HoltWinters(origin, gamma=F, seasonal = "multiplicative") #이중지수 평활 모형을 사용하고 싶다면, gamma=F 추가.
model_es_addic <- HoltWinters(origin, seasonal="additive")

# 원 데이터 및 fitted 데이터의 비교
# plot
plot(spline(time(origin), origin),type='l',xlab='Time',ylab='Pop')
lines(model_es$fitted[,1], col='red', lwd=2)
lines(model_es_addic$fitted[,1], col='blue', lwd=2)

# mse 125.5
mean((origin-model_es$fitted[,1])^2)

# 예측
plot(forecast(model_es, h=36))  # 짙은 회색 : 95%신뢰구간, 옅은 회색 : 80% 신뢰구간, 36개월 예측


# 이동평균법 사용 방법
plot(origin)

library(TTR) #이동평균 SMA 함수 사용하기 위한 패키지 불러오기
originSMA3 <- SMA(origin,n=3) #window=3
originSMA3
lines(originSMA3, col='darkorange', lty=2, lwd=2)

originSMA8 <- SMA(origin,n=8) #window=8
lines(originSMA8, col='steelblue', lty=4, lwd=2)

originSMA12 <- SMA(origin,n=12) #window=12 #부드러워짐 계절성 제거
lines(originSMA12, col='darkgreen', lty=2, lwd=2)

legend("topleft", c("m=3", "m=8", "m=12"), col=c("darkorange", "steelblue","darkgreen"), lty=c(2,4,2), lwd=2)


# weighted moving average : WMA 가장 최근 데이터에 가중치를 많이 줌 
plot(origin)
lines(WMA(origin, n=3, wts=c(0.3,0.4,0.5)),col='red', lty=2, lwd=1)


#============================================================
# ARIMA를 이용한 예측
#============================================================

# 데이터 탐색 및 모형식별
# 시도표
plot(origin)

# 분산 안정화
tran_org <- BoxCox(origin, BoxCox.lambda(origin))
plot(tran_org)
# plot(log(tran_org))  


# 계절성분이 있으므로 계절차분
tran_sdiff_org <- diff(tran_org, lag=12)
plot(tran_sdiff_org) # 패턴이 제거가 됨 

# ACF, PACF를 통한 탐색
layout(1:2)
acf(tran_sdiff_org, lag.max = 100) # 차분 필요. 천천히 감소 (차분이 필요 없으면 빠르게 감소함)
pacf(tran_sdiff_org, lag.max=100)

# 차분이 필요한지 검정 : 단위근 검정  H0 : 단위근이 있다.(즉 차분이 필요하다) 
#install.packages("fUnitRoots")
library(fUnitRoots)  # library for function adfTest 단위근검증 함수
adfTest(tran_sdiff_org, lags = 1, type = "c")  
#주관적인 부분.유의 수준 1%로 놓고 차분이 필요하다는 판단하에 진행 

# 차분 실행 
tran_sdiff_diff_org <- diff(tran_sdiff_org, lag=1) #계절차분을 한 번 더 
par(mfrow=c(1,1))
plot(tran_sdiff_diff_org) #
abline(h=0, lty=2) #아까보다 패턴이 사라짐. 

tran_sdiff_diff_org <- diff(diff(tran_org), lag=12)
tran_sdiff_diff_org 
layout(1:2)
acf(tran_sdiff_diff_org, lag.max = 48) # lag=1,3,12에서 0이 아닌 값.
# 비계절 시차 4부터 절단 -> MA(3), Q=3, 계절 -> 시차 2부터 절단 SMA(1), Q=1

pacf(tran_sdiff_diff_org, lag.max = 48) # lag=1,9
# 시차 1와 9에서 0보다 큰 값을 가짐 -> AR(3), 계절 : 시차2부터 절단 ->  SAR(1) P=1
# 막대기가 길다 : 유의하다. 

# 1. 너무 높은 차수는 사용하지x max.q=1, trace=T는 step wise AIC값이 가장 낮은 것을 
auto.arima(tran_sdiff_diff_org, max.p = 3, max.q=3, max.Q=1, trace = T)  #trace = T: 모형 선택 과정 확인 가능
# arima(0,0,1) -> p,d,q임 
# 2
auto.arima(tran_org, max.p = 3, max.q=3, max.Q=1, trace = T)
# (0.1.1)(0.1.1) pdq를 구해줌 

# 두 개 모형의 MA, SMA 계수의 추정값이 동일

# 모형 구축
model_arima <- arima(tran_sdiff_diff_org, order=c(0,0,1), seasonal = list(order = c(0,0,1), period = 12))
model_arima <- arima(tran_org, order=c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))

# 모형 검진
# 잔차 검정
tsdiag(model_arima)

# 독립성 검정
Box.test(model_arima$residuals, lag=1, type="Ljung-Box") #H0 : rho1 = 0
Box.test(model_arima$residuals, lag=5, type="Ljung-Box") #H0 : rho1 = ... = rho5 = 0
Box.test(model_arima$residuals, lag=10, type="Ljung-Box") #H0 : rho1 = ... = rho10 = 0

# 잔차의 독립성, 등분산성, 정규성 만족

# 원 데이터 및 fitted 데이터의 비교
par(mfrow=c(1,1))
plot(spline(time(origin), origin),type='l',xlab='Time',ylab='Pop')
lines(InvBoxCox(fitted(model_arima), BoxCox.lambda(origin)), col='red') #역변환 필요
mean((origin - InvBoxCox(fitted(model_arima), BoxCox.lambda(origin)))^2) #MSE

# 12개월 예측
arima_fit <- predict(model_arima, n.ahead=12) #BoxCox 변환 데이터 사용
lambda <- BoxCox.lambda(origin)
ts.plot(origin, xlim=c(1950,1965), ylim = c(0, 1000))
lines(InvBoxCox(arima_fit$pred, lambda),col="red")
lines(InvBoxCox(arima_fit$pred+1.96*arima_fit$se, lambda),col="blue",lty=1)
lines(InvBoxCox(arima_fit$pred-1.96*arima_fit$se, lambda),col="blue",lty=1)


