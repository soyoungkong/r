#=========
# 신뢰구간
#=========
#앞의 문제의 예제의 상황에서 100(1-alpha)% 신뢰구간을 구한다.
# 99% 신뢰구간을 구하라 (p33)
n <- 64
sample.mean <- 27.75
alpha <- 0.01

#모표준편차가 5로 알려진 정규모집단인 경우
se <- qnorm(alpha/2, lower.tail=F)*5/sqrt(n)
#lower.tail = false, 오른쪽 변적을 계산. (+쪽)
#qnorm 정규분포
(ci1 <- data.frame(lower=sample.mean-se, upper=sample.mean+se))

#모표준편차가 알려져 있지 않은 정규모집단인 경우
se <- qt(alpha/2, n-1, lower.tail=F)*5.083/sqrt(n)
#qt: t분포
(ci2 <- data.frame(lower=sample.mean-se, upper=sample.mean+se))

#========================
# 모평균에 대한 가설 검정
#========================
#데이터 : 앞의 예제에서 사용한 창던지기 기록
data.test <- c(64,64.8,66,63.5,65,68,67,63.6,67.6,68.9)
t.test(data.test, alternative="greater", mu=65)

#==========================
# 이표본에 의한 모평균 검정
#==========================

#데이터 : 두 지역(A,B)에서 오존생성률(ozone concentration)을 독립적으로 측정한 자료
#두 지역의 오존생성률의 평균이 같은지 비교
sample <- data.frame(ozone=c(3,4,4,3,2,3,1,3,5,2,5,5,6,7,4,4,3,5,6,5),region=rep(c('A','B'),each=10) )

#상자그림
ggplot(sample, aes(x=region, y=ozone,  fill=region)) + geom_boxplot()

#이표본 비교
t.test(ozone~region, data=sample)

#==============================
# 대응표본에 의한 모평균의 비교
#==============================
#데이터 : 앞 예제에서 사용한 음식 조절 이후 체중 비교 자료
#음식 조절법의 효과가 있는지 검정
sample <- data.frame(before=c(82.1,78.1,86.2,84.8,95.2,91.6,75.3,78.5,83.0,83.5)
                     ,after=c(80.7,78.1,83.9,83.5,91.2,91.2,72.6,76.2,81.6,81.2))

d <- sample$after-sample$before

#대응비교 검정 
t.test(d, alternative='less')  #p-value : 귀무가설 기각. 효과가 있다. 

#=============================
# 이표본에 의한 모분산 비 검정
#=============================
#데이터 : 두 지역(B,C)에서 오존생성률(ozone concentration)을 독립적으로 측정한 자료
#두 지역의 오존생성률의 분산이 같은지 비교
sample <- data.frame(ozone=c(5,5,6,7,4,4,3,5,6,5,3,3,2,1,10,4,3,11,3,10),region=rep(c('B','C'),each=10) )

#분산 비교
var.test(ozone~region, data=sample) #p-value = 0.05보다 작으므로 귀무가설 기각. 
# 두 분산이 다르다 




#해석을 위해서 귀무가설들 옵션을 잘 넣어서 
#양측, 단측이냐 따라서 p value가 다르므로 이를 주의해서 진행할 것