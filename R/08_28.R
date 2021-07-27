
#======== 워크샵 1 ===========

## t - 검정

# 세 종류 데이터
data1 <- c(30, -5, 55, -30, -20, 45)
data2 <- c(12, 13, 12, 13, 12, 13)
data3 <- c(30, -5, 55, -30, -20, 45, 30, -5, 55, -30, -20, 45)

#t - 검정
t.test(data1, alternative = c("greater"))
t.test(data2, alternative = c("greater"))
t.test(data3, alternative = c("greater"))

t.test(data3, alternative = c("greater"), mu = 0)  # mu = 0 생략 가능

# 예제
session_times <- read.csv("C:/r/web_page_data.csv")
head(session_times)

t.test(Time ~ Page, data = session_times, altetnative = 'less')  # Page A 와 Page B 비교


## 분산분석
four_sessions <- read.csv("C:/r/four_sessions.csv") 

summary(aov(Time ~ Page, data = four_sessions))

oneway.test(four_sessions$Time ~ four_sessions$Page, var = T)









#======== 워크샵 2 ===========

## 카이제곱 검정
click_rate <- read.csv("C:/r/click_rates.csv")
clicks <- matrix(click_rate$Rate, nrow = 3, ncol = 2, byrow = TRUE)

head(click_rate)
head(clicks)

dimnames(clicks) <- list("Headline" = c("Headline A", "Headline B", "Headline C"),
                           "Click" = c("Click", "No-click"))
clicks

chisq.test(clicks, simulate.p.value = TRUE)   # 헤드라인과 클릭 수는 독립이다.



## 적합도 검정
obs <- c(20, 40, 60)
obs.probs <- c(2/10, 3/10, 5/10)

(g.fit <- chisq.test(obs, p = obs.probs))

g.fit <- chisq.test(obs, p = obs.probs) ; g.fit   # 위 코드랑 동일 결과

chisq.test(obs, p = obs.probs)  # 위 코드랑 동일 결과
# 귀무가설 채택 = 멘텔 비율과 조사 비율은 차이가 없다



## 독립성 검정
raw_data <- c(7, 13, 9, 12, 13, 21, 10, 19, 11, 18, 12, 13)
data_mtx <- matrix(raw_data, byrow = TRUE, nrow = 3)
data_mtx

dimnames(data_mtx) <- list("Class" = c("Class1", "Class2", "Class3"),
                           "Scores" = c("ScoreA", "ScoreB", "ScoreC", "ScoreF"))
data_mtx

addmargins(data_mtx)   # sum 행, 열 추가

addmargins(prop.table(data_mtx))

barplot(t(data_mtx), beside = TRUE, legend = TRUE,
        ylim =  c(0, 30),
        ylab = "Observed frequencies in sample",
        main = "Frequency of math score by class")

(i.fit <- chisq.test(data_mtx))
# 귀무가설 체택 = 학습과 성적은 독립이다 = 연관성이 없다.



## 동질성 검정
raw_data <- c(50, 30, 20, 50, 80, 70)
data_mtx <- matrix(raw_data, byrow = TRUE, nrow = 2)
data_mtx

dimnames(data_mtx) <- list("성별" = c("남학생", "여학생"),
                           "DS교과목" = c("통계", "머신러닝", "딥러닝"))
data_mtx

barplot(t(data_mtx), beside = TRUE, legend = TRUE,
        ylim =  c(0, 120),
        ylab = "Observed frequencies in sample",
        main = "데이터 사이언스 교과목 선호 조사 결과")

(h.fit <- chisq.test(data_mtx))

1 - pchisq(q = c(19.318), df = 2, lower.tail = TRUE)
# 귀무가설 기각 = 성별에 다른 선호 교과목이 일치하지 않는다.









#======== 워크샵 3 ===========

lung <- read.csv("C:/r/LungDisease.csv")

# 산점도 그래프
plot(lung$Exposure, lung$PEFR, xlab = "Exposure", ylab = "PEFR")

# 선형 회귀 모델 생성
model <- lm(PEFR ~ Exposure, data = lung)
model

head(cars)
attach(cars)

# 1. plot
plot(dist ~ speed, data = cars)

# 2. lm() ~  abline()
m3 <- lm(dist ~ speed, cars)
abline(m3, col = "blue")

# 3. predict 함수를 사용하여 예측
yhat <- predict(m3)

# 4. 잘 예측되었는지 비교하기 위해 오른쪽에 실제 값을 붙여주기
cbind(dist, yhat)

join <- function(i){
  lines(c(speed[i],speed[i]), c(dist[i], yhat[i]), col = "green")
}
  
sapply(1:50, join)


## 회귀선과 잔차선 그리기
model <- lm(PEFR ~ Exposure, data = lung)
plot(lung$Exposure, lung$PEFR, xlab = "Exposure", ylab = "PEFR", pch = 64, col = "pink")
abline(model, col = "red")


attach(lung)
head(lung)
str(lung)

# predict 함수를 사용하여 예측
yhat <- predict(model)
head(yhat)

# 오른쪽에 실제 값을 붙여주기
cbind(Exposure, yhat)

# 직선 연결
join <- function(i) {
  lines(c(Exposure[i], Exposure[i]), c(PEFR[i], yhat[i]), col = "purple")
}


sapply(1:122, join)


# 회쉬 함수 학습
model <- lm(PEFR ~ Exposure, data = lung)
model

(fitted <- predict(model))
(resied <- residuals(model))











#======== 워크샵 4 ===========


# 1. 데이터 셋 읽어오기
std90 <- read.table("C:/r/student90.csv",
                    sep = ",",
                    stringsAsFactors = FALSE,
                    header = TRUE,
                    na.strings = "")
nrow(std90)
head(std90)


# 2. 회귀 모델 생성
(m <- lm(weight_kg ~ height_cm, data = std90))


# 3. 회귀 계수 구하기
coef(m)


# 4. 회귀 계수 값 검증하기
std90$weight_kg[1:4]   # 실제값
fitted(m)[1:4]        # 모델에 의한 예측값
((32.6604144) + (0.2246605) * (std90$height_cm[1:4]))   # 계수를 이용한 직접 계산


# 5. 이상값 진단
plot(m, which = 4)   # 이상값 그래프 (which = 4 : 그래프 종류 지정)  ## 이상치 4개

x_cooks.d <- cooks.distance(m)
x_cooks.d[1:4]

NROW(x_cooks.d)

x_cooks.d[which(x_cooks.d > qf(0.5, df1 = 2, df2 = 88))]   ## 이상치가 없다

install.packages("car")
library(car)
outlierTest(m)   ## 이상치가 없다


# 6. 잔차 구하기
residuals(m)[1:4]

std90$weight_kg[1:4]   # 실제값
fitted(m)[1:4] + residuals(m)[1:4]   # 예측값 + 잔차


# 7. 잔차 분석 
# -> Q-Q plot 을 이용한 잔차의 정규성 확인
qqnorm(residuals(m))
qqline(residuals(m))

# -> 샤피로 윌크 검정을 이용한 잔차의 정규성 확인
shapiro.test(residuals(m))


# 8. 신뢰구간
confint(m, level = 0.95)

m_coef <- predict(m, level = 0.95, interval = "confidence")
head(m_coef)

plot(weight_kg ~ height_cm, data = std90)
lwr <- m_coef[,2]
upr <- m_coef[,3]
sx <- sort(std90$height_cm, index.return = TRUE)
abline(coef(m), lwd = 2)
lines(sx$x, lwr[sx$ix], col = "orange", lty = 2)
lines(sx$x, upr[sx$ix], col = "orange", lty = 2)


# 9. 예측구간
m_pred <- predict(m, level = 0.95, interval = "predict")
head(m_pred)

# 키와 몸무게 산포도, 예측구간
p_lwr <- m_pred[,2]
p_upr <- m_pred[,3]
lines(std90$height_cm, p_lwr, col = "red", lty = 2)
lines(std90$height_cm, p_upr, col = "red", lty = 2)


# 10. 잔차제곱합
deviance(m)


# 11. 새로운 학생 키로 몸무게 예측하기
predict(m, newdata = data.frame(height_cm = 175), interval = "confidence")


# 12. 모델 평가
summary(m)

anova(m)

(m_a <- lm(weight_kg ~ height_cm, data = std90))

(m_b <- lm(weight_kg ~ 1, data = std90))

anova(m_a, m_b)  

install.packages("modelr")
library(modelr)

rmse(m_a, std90)
rmse(m_b, std90)

mae(m_a, std90)
mae(m_b, std90)











#======== 워크샵 5 ===========

house <- read.csv(("C:/r/house_sales.csv"), sep = "\t")

head(house[, c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bedrooms", "BldgGrade")])

house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade,
               data = house, na.action = na.omit)
house_lm

summary(house_lm)

house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms +
                   BldgGrade + PropertyType + NbrLivingUnits + SqFtFinBasement + 
                   YrBuilt + YrRenovated + NewConstruction,
                 data = house, na.action = na.omit)
install.packages("MASS")
library(MASS)

step_lm <- stepAIC(house_full, direction = "both")


lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade,
   data = house, na.action = na.omit)

library(dplyr)
zip_groups <- house %>%
  mutate(resid = residuals(house_lm)) %>%
  group_by(ZipCode) %>%
  summarize(med_resid = median(resid),
            cnt = n()) %>%
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt),
         ZipGroup = factor(ntile(cum_cnt, 5)))

house <- house %>%
  left_join(dplyr::select(zip_groups, ZipCode, ZipGroup), by = 'ZipCode')

house$ZipGroup


lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade +
   PropertyType + ZipGroup,
   data = house, na.action = na.omit)

lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade,
   data = house, na.action = na.omit)

data(mtcars)
head(mtcars)

fit_02 = lm(formula = mpg ~ hp*wt, data = mtcars)
summary(fit_02)


# 회귀식 결과
attach(mtcars)
mpg = 49.8 - 0.12 * hp - 8.22 * wt + 0.027 * hp * wt

# 상호작용을 그래프로 작성
plot(mpg ~ hp, data = mtcars, main = "Interaction of hp:wt")

# curve(함수식, add = TRUE) 기존 그래프에 겹쳐 그림
# wt에 2.2를 대입하면
# 49.8 - 0.12 * hp - 8.22 * wt(2.2) + 0.027 * hp*wt(2.2) = 31.71 - 0.06 * x

curve(31.71 - 0.06 * x, add = TRUE)
curve(23.49 - 0.03 * x, add = TRUE, lty = 2, col = 2)
curve(15.28 - 0.003 * x, add = TRUE, lty = 3, col = 3)

# lty(line type) 선의 모양(1 ~ 6)
legend("topright", c("2.2", "3.2", "4.2"), title = "wt", lty = 1:3, col = 1:3)

# 차의 중량이 늘어날수록 마력과 연비의 관계가 약해짐을 알 수 있다.

str(trees)

summary(trees)

install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(trees$Girth, trees$Height, trees$Volume)

attach(trees)
m <- lm(Volume ~ Girth + Height, data = trees)
m

s <- scatterplot3d(trees$Girth, trees$Height, trees$Volume, pch = 20, type = 'h',
                   angle = 55)
s$plane3d(m)

(n.data <- data.frame(Girth = c(8.5, 13.0, 19.0), Height = c(72, 86, 85)))
(n.y <- predict(m, newdata = n.data))

s <- scatterplot3d(c(8.5, 13.0, 19.0), c(72, 86, 85), n.y, pch = 20, type = 'h',
                   color = 'red', angle = 55)
s$plane3d(m)
