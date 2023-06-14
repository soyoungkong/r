#============================
# 확률밀도함수와 누적분포함수
#============================
#균일분포의 p.d.f. : dunif(x,0,1)
#균일분포의 c.d.f. : punif(x,0,1)
#균일분포의 p.d.f.와 c.d.f. 그리기
par(mfrow=c(1,2))
x <- seq(from=-1, to=2, length.out=1000)
plot(x, dunif(x,0,1), cex=0.1, xlab='', ylab='density'
     , main='p.d.f. of Uniform Distribution')
plot(x, punif(x,0,1), cex=0.1, xlab='', ylab='cumulative probability'
     , main='c.d.f. of Uniform Distribution')

#표준정규분포의 p.d.f. : dnrom(x, mean=0, sd=1)
#표준정규분포의 c.d.f. : rnorm(x, mean=0, sd=1)
#표준정규분포의 p.d.f.와 c.d.f. 그리기
par(mfrow=c(1,2))
x <- seq(from=-3, to=3, length.out=1000)
plot(x, dnorm(x,0,1), cex=0.1, xlab='', ylab='density'
     , main='p.d.f. of Normal Distribution')
abline(v=0, col=2, lty=2) #평균의 위치 표시
plot(x, pnorm(x,0,1), cex=0.1, xlab='', ylab='cumulative probability'
     , main='c.d.f. of Uniform Distribution')
