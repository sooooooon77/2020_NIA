str(cars)
plot(cars)
cars

getwd()
setwd()

x <- 1
y <- 2

z <- x + y
z

a <- 1
A
a@ <- 2

initial_value <- 2
initial_value
1a <- 1
_a <- 1
blood.type <- c("A", "B", "C", "AB")
blood.type

NULL
NA

x <- 5
y <- 2
x / y

xi <- 1 + 2i
yi <- 1 - 2i
xi + yi

x <- NA
x
x <- NULL
x

is.integer(1)
is.numeric(1)
is.inteager(1L)
is.integer(1L)
is.numeric(1L)

is.complex(1i)

x <- 1
is.integer(x)
x <- as.integer(x)
is.integer(x)

5^2
5%2
5//2
5%/%2
5%%2
5==5
5!=5
TLUE / FALSE
TRUE | FALSE
TRUE & FALSE
isTRUE
isTRUE(5==0)

1:7
c(1:5)
x <- 1:4
x
x <- 1, 2, 3, 4, 5
x<-c(1, 2,3 ,4 ,1:2)
x
seq(from = 1, to = 10, by = 2)
seq(1, 10, 2)
seq(1, 10, 3)
seq(0, 1, length.out = 3)
seq(1, 2, times = 2)
seq(c(1:3), times = 2)


x <- array(1:5, c(2, 3, 4))
x

x <- array(1:5, c(2, 4))
x

y <- matrix(x, nrow = 4)
y

x <- array(1:4, dim = c(2,2))
x

dimnamex = list(c('a', 'b'), c('A', 'B', 'C'))
x <- array(1:6, c(2, 3), dimnames = dimnamex)
x

x <- 1:12
x
matrix(x, nrow = 3)
matrix(x, nrow = 3, byrow = F)

x <- array(1:4, dim = c(2,2))
x

apply(x, 1, mean)
apply(x, 2, mean)
## array : 배열

dim(x)
sample(x)
sample(2)
sample(6)

name <- c("하나", "두나", "세나")
age <- c(22, 20, 25)
gender <- c("M", "F", "F")
blood <- factor(c("A", "B", "AB"))
patients <- data.frame(name, age, gender, blood)
patients

test <- data.frame()
test <- edit(test)
test

patients$name
patients[,1]
patients[1,]
patients[, patients$name == '하나']

patients[patients$name == "하나", c("name", "age")]

head(cars)
speed
attach(cars)
speed
detach(cars)
mean(cars$speed)
mean(cars$dist)
subset(cars, speed > 10)
subset(cars, speed > 10, select = c(dist))
subset(cars, speed > 10, select = -c(dist))

no.patients <- data.frame(day = c(1:6),
                          no = c(50, 60, 55, 52, 65, 58))
no.patients

listpatienst <- list(patients, no.patients)
listpatienst

listpatients$patients
listpatienst[[2]]

lapply(listpatienst[[2]], mean)
sapply(listpatienst[[2]], mean)



##### 실습 ######
name <- c("A", "B", "C", "D", "E")
gender <- c("F", "F", "F", "M", "F")
address <- c("서울", "부산", "NY", "LA", "서울")
height <- c(180, 170, 160, 175, 180)
weight <- c(80, 60, 50, 75, 70)

neighbor <- data.frame(name, gender, address, height, weight)
neighbor

#1. 주소(address)가 서울인 사람 출력
neighbor[neighbor$address == '서울',]

#2. 성별(gender)이 여자(F)인 사람의 이름(name)과 주소(address) 출력
subset(neighbor, gender == 'F', select = c(name, address))

#3. 몸무게(weight)가 70 이상인 사람의 이름(name)과 키(height) 출력
subset(neighbor, weight >= 70, select = c(name, height))



?stringAsFactors

print(paste(2, "z", "2", 2*3))




###### 실습 2 ######

#1. for문을 이용해 구구단 2~9단 만들기
for (i in 2:9) {
  print(paste(i, "단"))
  for (j in 1:9) {
    print(paste(i, "X", j, "=", i*j))
  }
}

#2. 1부터 100까지의 수 중에서 3의 배수이면서 4의 배수는 아닌 수의 합을 구하여라.
func1 <- function(x) {
  result <- 0
  for (i in 1:x) {
    if ((i %% 3 == 0) & (i %% 4 != 0)) {
      result = result + i
    }
  }
  return(result)
}

func1(100)

#3. x와 n을 입력하면 1부터 n까지의 수 중에서 x의 배수 합을 구해주는 사용자 정의 함수를 만들어라.
multipsum <- function(x, n) {
  result <- 0
  for (i in 1:n) {
    if (i %% x == 0) {
      result = result + i
    }
  }
  return(result)
}

multipsum(3, 100)








airquality
boxplot(airquality)

install.packages("gapminder")
install.packages("dplyr")
library(gapminder)
library(dplyr)
gapminder[, c("country", "lifeExp")]

gapminder %>%
  filter(country == "Croatia") %>%
  select(country, year, lifeExp) %>%
  summarise(lifeExp_avg = mean(lifeExp))
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library("lubridate")





###### 실습 3 #######

#1. 우리나라의 1990년도 이후의 기대 수명과 인구 추출
gapminder %>%
  filter((country == "Korea, Rep.") & (year >= 1990)) %>%
  select(lifeExp, pop)

#2. 북한의 1990년도 이후의 기대 수명과 인구 추출
gapminder %>%
  filter((country == "Korea, Dem. Rep.") & (year >= 1990)) %>%
  select(lifeExp, pop)




str(airquality)
head(is.na(airquality))
airquality[is.na(airquality)]

table(is.na(airquality))
table(is.na(airquality$Temp))
airquality[is.na(airquality$Temp)]

air_narm <- airquality[!is.na(airquality$Ozone),]
air_narm

airquality
air_narm1 <- na.omit(airquality)
air_narm1

patients$gender <- ifelse((patients$gender < 1 | patients$gender > 2),
                          NA, patients$gender)
patients$gender


boxplot(airquality[, c(1:4)])
boxplot(airquality[, 1])
