
#======== 워크샵 1 ===========

score <- c(85,90,93,86,82)
mean(score)
median(score)
var(score)
sd(score)


name <- c('주시현','최경우','이은주','허민성','홍미나')
height <- c(168,176,167,174,169)
weight <- c(52,68,47,82,51)

df <- data.frame(이름=name,키=height,몸무게=weight)

attach(df)

mean(키)
median(키)
mean(몸무게)
sd(키)
sd(몸무게)


mean(df$키)
median(df$키)
mean(df$몸무게)
sd(df$키)
sd(df$몸무게)


height <- rnorm(n=1000000,mean=168,sd=7)
hist(height,breaks=10,probability=T) 
hist(height,breaks=100,probability=T)
hist(height,breaks=1000,probability=T) 


score1 <-read.csv("c:/r/tdata.csv", header=T, stringsAsFactors=T)
score1


shapiro.test(score1$성적)

result <- t.test(score1$성적,alternative=c("greater"),mu=75)
result


score1 <-read.csv("c:/r/tdata2.csv", header=T, stringsAsFactors=T)
score1

shapiro.test(score1$성적)

result <- t.test(score1$성적,alternative=c("greater"),mu=75)
result


x<-c(15,10,13,7,9,8,21,9,14,8) 
y<-c(15,14,12,8,14,7,16,10,15,12) 
shapiro.test(x) 
shapiro.test(y) 

t.test(x,y) 


x<-c(52,60,63,43,46,56,62,50) 
y<-c(58,62,62,48,50,55,68,57)  
shapiro.test(x) 
shapiro.test(y) 
t.test(x,y,paired = TRUE) 


x <- c(1.09, 2.12, 2.92, 4.06, 4.90)
y <- c(1,2,3,4,5)
z <- c(1.10, 1.96, 12.98, 4.09, 4.92)
shapiro.test(x) 
shapiro.test(y) 
shapiro.test(z) 

mydata <-c(x,y,z)
mydata
group <-c(rep(1,5), rep(2,5), rep(3,5)) 
group
oneway.test(mydata~group, var=T)


x <- c(1.09, 2.12, 2.92, 4)
x1<-c(23,27,24,25,29,30,26) 
x2<-c(35,32,38,36,32,33,34) 
x3<-c(36,41,38,39,40,38,39) 
x4<-c(32,30,37,34,35,34,32)
shapiro.test(x1) 
shapiro.test(x2) 
shapiro.test(x3) 
shapiro.test(x4) 

mydata <-c(x1,x2,x3,x4)
group <-c(rep(1,7), rep(2,7), rep(3,7),rep(4,7))
oneway.test(mydata~group, var=T) 

















#======== 워크샵 2 ===========

fruits1 <- read.csv("c:/r/love_fruits.csv",header=T)
head(fruits1)

prop.table(table(fruits1$선호과일)) 
round(prop.table(table(fruits1$선호과일))*100,2)


table(fruits1$선호과일)
count <- c(table(fruits1$선호과일))
pct <- c(round(prop.table(table(fruits1$선호과일))*100,2))
love_fruits <- data.frame(건수=count,비율=pct)
love_fruits


barplot(love_fruits$건수, names.arg=c("바나나","복숭아","사과","체리","포도"),
        ylim=c(0,15), col=rainbow(5))


x <- c(3,5,8,11,13)
y <- c(1,2,3,4,5)
cor(x,y)
 
cor.test(x,y)

x <- c(110,120,130,140,150)
y <- c(100,105,128,115,142)
plot(x,y,pch=20,col="red")


line <- lm(y~x)
line

abline(line, col="blue")


score <- read.csv("c:/r/score.txt",header=T,sep=",")
attach(score)
score

lm1 <- lm(성적~IQ)
lm1 

y <- -5.292 + 0.671 * 125 
y

plot(IQ,성적,pch=20,col="red")
abline(lm1,col="blue")


x<-c(15,10,13,7,9,8,21,9,14,8) 
y<-c(15,14,12,8,14,7,16,10,15,12) 
out=lm(y~x)
summary(out)


lm3 <- lm(성적~IQ+다니는학원수+게임하는시간+TV시청시간)
lm3

y = 23.299 + (0.468*130) + (0.718 * 3) - (0.839*2) - (1.385*1)
y

summary(lm3)

x1<-c(100,90,98,79,81,69,80,77,68,54)
x2<-c(5,4,5,3,4,3,2,3,2,1)
x3<-c(5,3,4,3,4,3,2,3,2,1) 
x4<-c(5,3,3,2,3,3,4,3,2,1) 
mydata <-data.frame(y=x1, s1=x2, s2=x3, s3=x4)
mydata 

model <-lm(y~., data=mydata)
summary(model)


x1<-c(100,90,98,79,81,69,80,77,68,74) 
x2<-c(5,4,4,3,4,3,4,3,2,3) 
x3<-c(4,3,3,2,3,2,3,3,2,3) 
x4<-c(5,3,3,2,3,3,4,3,2,3) 
mydata <-data.frame(y=x1, s1=x2, s2=x3, s3=x4)
mydata 

model <-lm(y~., data=mydata)
summary(model)















#======== 워크샵 3 ===========

### 문제01 ###

A <- c(65, 87, 73, 79, 81, 69, 80, 77, 68, 74)
B <- c(75, 69, 83, 81, 72, 79, 90, 88, 76, 82)
C <- c(59, 78, 67, 62, 83, 76, 55, 75, 49, 68)
D <- c(94, 89, 80, 88, 90, 85, 79, 93, 88, 85)

## 정규분포 검사
shapiro.test(A)
shapiro.test(B)
shapiro.test(C)
shapiro.test(D)

## 정규분포 결과 해석
# A : p-value = 0.9465 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.
# B : p-value = 0.9553 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.
# C : p-value = 0.8865 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.
# D : p-value = 0.6054 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.

## 가설설정
# 귀무가설 : 4가지 교육방법의 효과는 동일하다.
# 대립가설 : 4가지 교육방법의 효과는 동일하지 않다.

## 검정방법 및 이유
# 2집단 이상의 표본이기 때문에 분산분석을 시행한다.
data1 <- c(A, B, C, D) ; data1

group1 <- c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10)) ; group1

oneway.test(data1 ~ group1, var = T)

## 결과해석
# F통계량 = 11.727, p-value = 1.653e-05 < 0.05 이므로 유의수준 0.05에 대해 네 집단의 평균이 동일하지 않다.
# 따라서, 4가지 교육방법의 효과는 동일하지 않다고 할 수 있다.








### 문제02 ###

x <- c(52, 60, 63, 43, 46, 56, 62, 50)
y <- c(58, 62, 62, 48, 50, 55, 68, 57)

## 정규분포 검사
shapiro.test(x)
shapiro.test(y)

## 정규분포 결과 해석
# x : p-value = 0.6037 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.
# y : p-value = 0.9042 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.

## 가설설정
# 귀무가설 : 교육 전과 교육 후는 동일하다.
# 대립가설 : 교육 전과 교육 후는 동일하지 않다.

## 검정방법 및 이유
# 같은 집단에 두 번의 실험을 진행한 경우이기 때문에 대응표본 t검정을 시행한다.
t.test(x, y, paired = TRUE)

## 결과해석
# t통계량 = -3.1305, p-value = 0.0166 < 0.05 이므로 유의수준 0.05에 대해 두 집단의 평균이 동일하지 않다.
# 따라서, 교육 전에 비해 교육 후의 타자 속도가 증가하였다고 할 수 있다.







### 문제03 ###

pre <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
post <- c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)

## 정규분포 검사
shapiro.test(pre)
shapiro.test(post)

## 정규분포 결과 해석
# pre : p-value = 0.8129 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.
# post : p-value = 0.6467 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.

## 가설설정
# 귀무가설 : 성형 전과 성형 후의 만족도는 동일하다.
# 대립가설 : 성형 전과 성형 후의 만족도는 동일하지 않다.

## 검정방법 및 이유
# 같은 집단에 두 번의 실험을 진행한 경우이기 때문에 대응표본 t검정을 시행한다.
t.test(pre, post, paired = TRUE)

## 결과해석
# t통계량 = -3.3489, p-value = 0.008539 < 0.05 이므로 유의수준 0.05에 대해 두 집단의 평균이 동일하지 않다.
# 따라서, 성형 전에 비해 성형 후의 만족도가 증가하였다고 할 수 있다.







### 문제04 ###

new <- c(15,10,13,7,9,8,21,9,14,8) 
old <- c(15,14,12,8,14,7,16,10,15,12) 

## 정규분포 검사
shapiro.test(new)
shapiro.test(old)

## 정규분포 결과 해석
# new : p-value = 0.09131 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.
# old : p-value = 0.2986 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.

## 가설설정
# 귀무가설 : 신약과 위약의 효과는 동일하다.
# 대립가설 : 신약과 위약의 효과는 동일하지 않다.

## 검정방법 및 이유
# 두 집단의 평균 비교 검정을 위해 t검정을 시행한다.
t.test(new, old)

## 결과해석
# t통계량 = -0.53311, p-value = 0.6012 < 0.05 이므로 유의수준 0.05에 대해 두 집단의 평균이 동일하지 않다.
# 따라서, 위약에 비해 신약의 치료효과가 더 빨리 나타난다고 할 수 있다.







### 문제05 ###

x1 <- c(23,27,24,25,29,30,26) 
x2 <- c(35,32,38,36,32,33,34) 
x3 <- c(36,41,38,39,40,38,39) 
x4 <- c(32,30,37,34,35,34,32)

## 정규분포 검사
shapiro.test(x1)
shapiro.test(x2)
shapiro.test(x3)
shapiro.test(x4)

## 정규분포 결과 해석
# x1 : p-value = 0.8366 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.
# x2 : p-value = 0.5554 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.
# x3 : p-value = 0.8766 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.
# x4 : p-value = 0.9147 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.

## 가설설정
# 귀무가설 : 열처리 온도에 따라서 제품의 강도에 차이가 없다.
# 대립가설 : 열처리 온도에 따라서 제품의 강도에 차이가 있다.

## 검정방법 및 이유
# 2집단 이상의 표본이기 때문에 분산분석을 시행한다.
data5 <- c(x1, x2, x3, x4) ; data5

group5 <- c(rep(1, 7), rep(2, 7), rep(3, 7), rep(4, 7)) ; group5

oneway.test(data5 ~ group5, var = T)

## 결과해석
# t통계량 = 38.352, p-value = 2.581e-09 < 0.05 이므로 유의수준 0.05에 대해 네 집단의 평균이 동일하지 않다.
# 따라서, 열처리 온도에 따라서 제품의 강도에 차이가 있다고 할 수 있다.







### 문제06 ###

x <- c(15,10,13,7,9,8,21,9,14,8) 
y <- c(15,14,12,8,14,7,16,10,15,12) 

## 가설설정
# 귀무가설 : 병원 직원들의 친절도가 병원 만족도에 영향을 미치지 않는다.
# 대립가설 : 병원 직원들의 친절도가 병원 만족도에 영향을 미친다.

## 검정방법 및 이유
# 독립변수가 종속변수에 영향을 미치는지 확인하기 위한 단일회귀분석을 시행한다.
data6 <- data.frame(y = y, x = x) ; data6

lm6 <- lm(y ~ ., data = data6)
summary(lm6)

## 결과해석
# 회귀식 : y = 0.5211 * x + 6.3592
# 회귀식에 대한 p-value = 0.01585 < 0.05 이므로 유의수준 0.05에서 친절도가 만족도에 영향을 미친다.
# 결정계수는 0.5375 로 약 53% 의 신뢰도를 가진다.







### 문제07 ###

y <- c(100,90,98,79,81,69,80,77,68,54)
x1 <- c(5,4,5,3,4,3,2,3,2,1)
x2 <- c(5,3,4,3,4,3,2,3,2,1) 
x3 <- c(5,3,3,2,3,3,4,3,2,1) 

## 가설설정
# 귀무가설 : 개인요인(인성, 얼굴, 성격)은 인맥관리에 영향을 미치지 않는다.
# 대립가설 : 개인요인(인성, 얼굴, 성격)은 인맥관리에 영향을 미친다.

## 검정방법 및 이유
# 독립변수가 종속변수에 영향을 미치는지 확인하기 위한 단일회귀분석을 시행한다.
data7 <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3) ; data7

lm7 <- lm(y ~ ., data = data7)
summary(lm7)

## 결과해석
# 회귀식 : y = 13.837 * x1 - 8.220 * x2 + 5.220 * x3 + 44.844
# 회귀식에 대한 p-value = 0.00102 < 0.05 이므로 유의수준 0.05에서 개인요인이 인맥관리에 영향을 미친다.
# 세부적으로 인성(x1)과 성격(x3)이 인맥관리에 영향을 미친다.
# 수정된 결정계수는 0.8825 로 약 88% 의 신뢰도를 가진다.







### 문제08 ###

y <- c(100,90,98,79,81,69,80,77,68,74) 
x1 <- (5,4,4,3,4,3,4,3,2,3) 
x2 <- c(4,3,3,2,3,2,3,3,2,3) 
x3 <- c(5,3,3,2,3,3,4,3,2,3)

## 가설설정
# 귀무가설 : 서비스요인(가격, 품질, 청결)은 고객충성도에 영향을 미치지 않는다.
# 대립가설 : 서비스요인(가격, 품질, 청결)은 고객충성도에 영향을 미친다.

## 검정방법 및 이유
# 독립변수가 종속변수에 영향을 미치는지 확인하기 위한 단일회귀분석을 시행한다.
data8 <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3) ; data8

lm8 <- lm(y ~ ., data = data8)
summary(lm8)

## 결과해석
# 회귀식 : y = 5.084 * x1 + 9.954 * x2 - 1.410 * x3 + 41.829
# 회귀식에 대한 p-value = 0.006759 <0.05 이므로 유의수준 0.05에서 서비스요인이 고객충성도에 영향을 미친다.
# 세부적으로 가격(x1)이 고객충성도에 영향을 미친다.
# 수정된 결정계수는 0.8514 로 약 85% 의 신뢰도를 가진다.







### 문제09 ###

x <- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)

## 정규분포 검사
shapiro.test(x)

## 정규분포 결과 해석
# x : p-value = 0.1058 > 0.05 이므로 유의수준 0.05에 대해 정규분포를 따른다.

## 가설설정
# 귀무가설 : 0교시 수업 전과 후의 수학 평균 성적에 차이가 없다.
# 대립가설 : 0교시 수업 후의 수학 평균 성적이 전보다 증가하였다.

## 검정방법 및 이유
# 평균 비교를 하기위해 t검정 단측검정을 시행한다.
t.test(x, alternative=c("greater"), mu=55)

## 결과해석
# t통계량 = 0.24546, p-value = 0.4046 > 0.05 이므로 유의수준 0.05에 대해 귀무가설을 체택한다.
# 따라서, 0교시 수업 전과 후의 수학 평균 성적에 차이가 없다고 할 수 있다.












#======== 워크샵 4 ===========


state <- read.csv("C:/r/state.csv")

mean(state[["Population"]])
mean(state[["Population"]], trim=0.1)
median(state[["Population"]])
weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])


mean(state[["Murder.Rate"]])
mean(state[["Murder.Rate"]], trim=0.1)
median(state[["Murder.Rate"]])
weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])


weighted_mean1 <- (200*0.09 + 1000*0.21)/(200+1000)
weighted_mean1

alcohol <- data.frame(volume=c(200, 1000), alcohol_rate=c(0.09, 0.21))
weighted_mean2 <- weighted.mean(alcohol$alcohol_rate, alcohol$volume)
weighted_mean2


x <-c(1,2,3,3,5,6,7,9)
mean(x)
median(x) 
max(x)
min(x)
summary(x)

var(x) 
sd(x)  
IQR(x)
mad(x)  
quantile(x, p=c(.05, .25, .5, .75, .95))


quantile(state[["Population"]], p=c(.05, .25, .5, .75, .95)) 
quantile(x, p=c(.05, .25, .5, .75, .95))

summary(state[["Population"]])

boxplot(state[["Population"]]/1000000, ylab="Population (millions)")
boxplot(x)


breaks <- seq(from=min(state[["Population"]]), to=max(state[["Population"]]), length=11)
pop_freq <- cut(state[["Population"]], breaks=breaks, right=TRUE, include.lowest = TRUE)
state['PopFreq'] <- pop_freq

table(pop_freq)

hist(state[["Population"]], breaks=breaks) 


x <-c(1,2,3,3,5,6,7,9)
breaks1 <- seq(from=min(x), to=max(x), length=5)
pop_freq1 <- cut(x, breaks=breaks1, right=TRUE, include.lowest = TRUE)

table(pop_freq1)

hist(x, breaks=breaks1) 


dfw <- read.csv("c:/r/dfw_airline.csv")
barplot(dfw)
barplot(as.matrix(dfw))

barplot(as.matrix(dfw)/6, cex.axis = 0.8, cex.names = 0.7)

barplot(as.matrix(dfw)/6, ylim=c(0,20000), cex.axis = 0.8, cex.names = 0.7)


sp500_px <- read.csv("c:/r/sp500_0.csv",row.names=1)
sp500_sym <- read.csv("c:/r/sp500_sym.csv", stringsAsFactors = FALSE)


telecom <- sp500_px[, sp500_sym[sp500_sym$sector=="telecommunications_services", 'symbol']]
telecom <- telecom[row.names(telecom)>"2012-07-01", ]

telecom_cor <- cor(telecom)
telecom_cor

#install.packages("corrplot")
library(corrplot)
corrplot(cor(telecom))

corrplot(cor(telecom), method="circle")
corrplot(cor(telecom), method = "square")
corrplot(cor(telecom), method = "ellipse")
corrplot(cor(telecom), method = "number")
corrplot(cor(telecom), method = "shade")
corrplot(cor(telecom), method = "color")
corrplot(cor(telecom), method = "pie")

plot(telecom$T, telecom$VZ, xlab="T", ylab="VZ")

cor(telecom$T, telecom$VZ)











#======== 워크샵 5 ===========


loans_income <- read.csv("c:/r/loans_income.csv")[,1]

samp_data <- data.frame(income=sample(loans_income, 1000), 
                        type='data_dist')
str(samp_data)


samp <- data.frame(name=factor(c("kim","choi","choi","lee","kim","lee")),
                   score=c(50,30,10,50,20,30))

tapply(samp$score, samp$name, length)
tapply(samp$score, samp$name, mean)
Ssss

samp_mean_05 <- data.frame(
  income = tapply(sample(loans_income, 1000*5), 
                  rep(1:1000, rep(5, 1000)), FUN=mean),
  type = 'mean_of_5')

str(samp_mean_05)


samp_mean_20 <- data.frame(
  income = tapply(sample(loans_income, 1000*20), 
                  rep(1:1000, rep(20, 1000)), FUN=mean),
  type = 'mean_of_20')


income <- rbind(samp_data, samp_mean_05, samp_mean_20)
str(income)
income$type = factor(income$type, 
                     levels=c('data_dist', 'mean_of_5', 'mean_of_20'),
                     labels=c('Data', 'Mean of 5', 'Mean of 20'))


table(income$type)

library(ggplot2)


ggplot(income, aes(x=income)) +
  geom_histogram(bins=40) +              
  facet_grid(type ~ .)            


ggplot(income, aes(x=income)) +
  geom_histogram(bins=400) +  
  facet_grid(. ~ type)      



library(boot)
loans_income <- read.csv("c:/r/loans_income.csv")[,1]

summary(loans_income)

stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(loans_income, R = 1000, statistic=stat_fun)

boot_obj