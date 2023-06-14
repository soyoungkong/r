########################################
#  분산분석 
########################################

# 0. 환경설정 ##########################

# install.packages("ggplot2")
# install.packages("readxl")
library(ggplot2)
library(readxl)



# 1. 예제데이터 준비 ###################


## (Q) 직업그룹(0 ~ 4)별로 카드총이용금액합계가 같을까?


## [데이터 다운로드]
  
### - 카드(신용카드, 체크카드)를 이용하는 국내 거주자(거주지 주소기준)를 대상으로 수집된 데이터를 기반으로 
###   재작성된 데이터로써 공간적 범위(시도, 시군구, 행정동 및 각종 Block 등)를 기준으로 
###   성별, 연령별, 직업구분(KCB 기준), 카드총이용금액합계 10분위 및 각 분위에 따른 카드이용정보를 담고 있음


## [데이터 호출]

path                  <- "3. KCB_지역단위_신용분석정보_소비_샘플데이터.xlsx"
sheet.names           <- excel_sheets(path)
DF                    <- read_excel(path, sheet.names[1], na = c(NA, '', ' - '))
DF
examDF                <- na.omit(subset(DF, select = c(SEX, JOB_GB, Card_Index_C07)))    
examDF                <- as.data.frame(examDF)                                           
examDF$JOB_GB         <- as.factor(examDF$JOB_GB) # 직업구분 변경                                      
examDF$Card_Index_C07 <- as.numeric(examDF$Card_Index_C07)       #수치형으로 변경                      

head(examDF)

### - SEX: 성별
### - JOB_GB: 1(급여소득자), 2(자영업_일반), 3(자영업_전문직), 4(기타)
### - Card_Index_C07: 카드총이용금액합계

  
## [데이터 검토] 상자 그림

dev.off()
ggplot(examDF, aes(x=JOB_GB, y=Card_Index_C07, fill=JOB_GB)) + geom_boxplot()

### → 상자그림을 그렸을 때 직업그룹별로 다르다는 것을 볼 수 있다.

## log 변환

examDF$Card_Index_C07 <- log(examDF$Card_Index_C07)

ggplot(examDF, aes(x=JOB_GB, y=Card_Index_C07, fill=JOB_GB)) + 
  geom_boxplot()


# 2. 일원배치법 ##########################


## [분산분석표]

aov_res               <- aov(Card_Index_C07~JOB_GB, data=examDF)
summary(aov_res)

### → 유의확률이 매우 작으므로( <1.36e-14 ) 유의수준 0.05에서 귀무가설을 기각하여 각 직업그룹별 카드 총 이용금액의 차이가 있다.


## [시각화] 분산분석 PLOT 

par(mfrow=c(2,2))
par("mar")
par(mar=c(1,1,1,1))
plot(aov_res)



# 3. 이원배치법 ##########################


## (Q) 성별(M, F)과 직업그룹(0 ~ 4)의 효과가 같을까?


## [상자그림] 성별에 따른 카드총이용금액합계

ggplot(examDF, aes(x=SEX, y=Card_Index_C07, fill=SEX)) + 
  geom_boxplot()

### → 그림만으로 판단하기 어렵다.


## [상자그림] 직업그룹에 따른 카드총이용금액합계

ggplot(examDF, aes(x=JOB_GB, y=Card_Index_C07, fill=JOB_GB)) + 
  geom_boxplot()

### → 직업그룹에 따른 차이가 있다.


## [교호작용] Interaction plot

with(examDF, {
  interaction.plot(SEX, JOB_GB, Card_Index_C07, bty='l', col = JOB_GB, 
                   main='interaction plot')
})

with(examDF, {
  interaction.plot(JOB_GB, SEX, Card_Index_C07, bty='l', main='interaction plot') 
})

### → Interaction plot에서 선분들이 평행하면 교호작용이 완전히 없는 상태인데 주어진 plot으로 보면 교호작용이 강하진 않음을 알 수 있다.


## [분산분석표]

aov_res               <- aov(Card_Index_C07~JOB_GB*SEX,data=examDF)
summary(aov_res)

### → 검정결과 유의수준 5%에서 직업그룹 차이만 유의하다.



# ANCOVA ####################################

dataset <- read.csv("3. ancova_data.csv", fileEncoding = 'euc-kr')

fit0 <- lm(Cholesterol ~ as.factor(Group), data = dataset)
fit1 <- lm(Cholesterol ~ Age + as.factor(Group), data = dataset)

anova(fit0)
anova(fit1)

## 수식의 순서에 따른 차이 존재 -> 공변량을 항상 앞에 (주의)
fit1.2 <- lm(Cholesterol ~ as.factor(Group) + Age, data = dataset)

anova(fit1.2)



# MANOVA ####################################

data(iris)

## correlation
cor.test(iris$Sepal.Length, iris$Petal.Length)

## ANOVA
res1 <- lm(Sepal.Length~ Species, data = iris)
res2 <- lm(Petal.Length~ Species, data = iris)

summary.aov(res1)
summary.aov(res2)

## MANOVA
res.man <- manova(cbind(Sepal.Length, Petal.Length)~ Species, data = iris)
summary(res.man)
summary.aov(res.man)

