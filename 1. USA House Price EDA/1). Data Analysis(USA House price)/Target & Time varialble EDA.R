install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("car")

library(car)
library(ggplot2)
library(dplyr)
library(corrplot)
library(gridExtra)

## 데이터 불러오기 및 파악
train <- read.csv(file.choose(),header = T)
head(train)

# 원 데이터 손실이 있을 수 있으니 house에 붙혀넣기를 한다
house <- train

str(house)
sum(is.na(house))
# 결측치가 없음을 알 수 있다.

## Target variable
## 변수들과 집의 가격(price)간의 상관관계
cor.mat <- cor(house[,-c(1,2)])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
par(mfrow=c(1,2))
corrplot(cor.mat, method = "ellipse", type="upper", diag=F, col=col(200))
corrplot(cor.mat, method = "color", type="upper", diag=F, addCoef.col = "black", col=col(200))
# 가격과 다른 변수들관의 상관관계를 살펴본 결과
# sqft_living, grade, sqft_above, sqft_living15, bathroom 순으로 가장 높은 상관관계를 보였다.

summary(house$price)
par(mfrow=c(1,2));boxplot(house$price,main="boxplot of price");
boxplot(log(train$price),main="boxplot of log(price)")

price_p <- qplot(house$price,geom = "density")
lprice_p <- qplot(log(house$price),geom = "density")
grid.arrange(price_p,lprice_p,ncol=1)

# price의 분포가 오른쪽 긴꼬리 형태로, log변환하여 살펴보았다.
# log 변환한 그래프도 오른쪽꼬리가 좀더 길지만 원데이터의 그래프 보다 정규분포에 가까워졌다. 

qqnorm(house$price,main="price Q-Q Plot")+ 
  qqline(house$price,col="red")
qqnorm(log(house$price),main="log price Q-Q Plot")+
  qqline(log(house$price),col="red")

# Q-Q plot을 통해 log 변환한 price가 좀더 정규성을 띄는것을 확인했다.

## Input Variable
## 1) Time Variable
### 1. 구매날짜 yyymmdd형태로 바꾸기
house$date <- substr(house$date,1,8)
house$yyyy <- substr(house$date,1,4)
house$yyyymm <- substr(house$date,1,6)

house %>% select(yyyymm) %>% table

temp <- house %>% group_by(yyyymm) %>% summarise(Freq=n(),
                                                 Total_price=sum(price),
                                                 Mean_price=mean(price),
                                                 Median_price=median(price))
p1 <- temp %>% ggplot(aes(x=yyyymm, y=Freq, group=1))+ 
  geom_line() + 
  geom_point() +
  ggtitle("Freq by yyyymm")

p2 <- temp %>% ggplot(aes(x=yyyymm, y=Total_price,group=1)) + 
  geom_line() + 
  geom_point() +
  ggtitle("Total_price by yyyymm")  

p3 <- temp %>% ggplot(aes(x=yyyymm, y=Mean_price,group=1)) + 
  geom_line() + 
  geom_point() +
  ggtitle("Mean_price by yyyymm")  

p4 <- temp %>% ggplot(aes(x=yyyymm, y=Median_price,group=1)) + 
  geom_line() + 
  geom_point() +
  ggtitle("Median_price by yyyymm")  

grid.arrange(p1, p2, p3, p4, ncol=2)
# 구매시점에 변화에 따른 구매빈도(p1),총 가격(q2), 평균가격(q3), 가격의 중앙값(q4) 
# 2014년 12월과 2015년 1월,2월 사이에 집값이 대폭 떨어짐에 따라 한달사이에 구매빈도가 늘어났고, 이로 인해 총 거래액이 증가했다.
# 2015년 4월가 5월사이에 집값이 상승하면서 거래량이 급소함에 따라 총 거래량이 감소하였다. 


### 2. 건물이 지어진 연도에 따른 평균가격
# yr_bulit : 건물이 지어진 년도

table(house$yr_built)

yrb_price <- house %>% group_by(yr_built) %>% summarise(price_Mean=mean(price))
# 지어진 시점에 따른 평균가격

yrb_p <- yrb_price %>% ggplot(aes(x=yr_built, y=price_Mean,group=1)) + 
  geom_line() + 
  geom_point() +
  ggtitle("Mean_price by yr_built")  
grid.arrange(yrb_p)

diff <- yrb_price[c(1:nrow(yrb_price)-1),2] - yrb_price[c(2:nrow(yrb_price)),2]

# 지어진 시점에 따른 평균가격그래프
# 1940년도에 세계2차대전 발생으로 인해 집값이 급락했다.


### 3. 집을 재건축 여부에 따른 가격
# yr_renovate : 집을 재건축한 년도

nrow(house)
table(house$yr_renovated)
# 전체 약 15000개의 데이터 중 재건축하지 않은 건축물이 14000여개 있다.
# 재건축 연도에 따른 비교보다는 재건축한 건물과 재건축하지 않은 건물간의 비교가 더 의미있다 판단하여
# 재건축하지 않은 건물은 0, 재건축한 건물은 1로 범주형 자료를 생성하여 비교한다.

house$renovated <- ifelse(house$yr_renovated==0,"0","1")
table(house$renovated)
# 재건축하지 않은건물 = 0, 재건축한 건물= 1로 범주형 자료생성

qplot(renovated,price,data=house,geom="boxplot")
qplot(renovated,log(price),data=house,geom="boxplot")

# 재건축 여부에 따른 가격의 분포
# 대체적으로 재건축한 건물의 가격이 하지 않은 건물보다 높았다.

bartlett.test(price ~ renovated, data=house)
bartlett.test(log(price) ~ renovated, data=house)

summary(aov(price ~ renovated, data=house))
summary(aov(log(price)~ renovated, data=house))

# 일원배치 분산분석 전제조건인 오차의 등분산성을 만족하는지  검정을 실시했다
# price, log(price)모두 p-value가 매우 작기 때문에 귀무가설을 기각함으로써
# 오차의 등분산성 가정을 만족한다고 판단한다.

# ANOVA 결과, p-value가 매우 작기 대문에 귀무가설을 기각한다.
# 따라서 재건축 여부에 따라 price의 평균가격이 다르다고 할 수 있다.



