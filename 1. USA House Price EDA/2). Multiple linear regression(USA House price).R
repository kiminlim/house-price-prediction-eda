getwd()


hp <- read.csv('train.csv')
train <- read.csv('train.csv')

head(hp)

hp2=hp[,c(-1,-2,-18,-19)]
head(hp2)


hp2[,1]
names(hp2)
hp2$date

cor(train)

summary(train)
# 집값의 평균은 5억
# 방 개수는 보통3개 
# 화장실 개수는 2개
# 평방비트(평수 대신 피트로 집넓이)> 평균 58평




# 다중 회귀분석**

train <- read.csv('train.csv')
train$date
train$date <- substr(train$date,1,8)
train$date <- as.numeric(train$date)
names(train)
str(train)

(m <- lm(price~., data=train))
summary(m)


train$date <- as.numeric(train$date)
train$sqft_basement # 의미 없는 것으로 보임.(널값이 많고, 없어서 0인 집과 있는 집의 편차가 커서 독립변수에서 제외.)
train2 <- train[,c(-14)] # sqft_basement변수 제거
names(train2)
head(train$sqft_basement)
cor(train$sqft_basement)
train[,c(14)]
# train2로 다중회귀분석.
(m2 <- lm(price~., data=train2))

summary(m2) # 회귀분석 결과 floors는 피벨류가 작아서 회귀 변수로 적합하지 않음.

# summary를 했을 때 floors는 유의하지 않음을 알 수 있음
# 따라서 유의한 변수만 있도록 회귀모형을 수정.

train3 <- train2[,c(-8)]
length(names(train3)) # 변수 19ㄱ


####
(m3 <- lm(price~., data=train3))
summary(m3)

par(mfrow=c(1,1))
plot(m3$fitted.values, m3$y)
abline(0,1,col="blue", lty=2)
plot(m3, which=1)



# 정규성 검정 - 
library(car)
qqPlot(m3,labels=row.names(train3),id.method="identify",simulate=TRUE,main="Q-Q_ plot")
# > 종속변수(가격)에 로그변환을 취해주니 잔차가 정규성을 띄는 것을 확인할 수 있음.
qqPlot(log_m4,labels=row.names(train4),id.method="identify",simulate=TRUE,main="Q-Q_ plot")

hist(train$price)
hist(log(train$price))
qplot(train$price)
qplot(log(train$price))
# 로그변환 후 price가 정규분포에 가까워짐을 확인.

# 독립성 검정 -
durbinWatsonTest(m3) # p값이 의미가 없으므로 자기상관은 없다고 할 수 있다. lag의 값 1은 각각의 자료를 바로 다음 자료와 비교했다는 것은 뜻한다

# 선형성 
crPlots(m3)

# 등분산성 
ncvTest(m3)
# 여기서 유의한 결과 가 나온다면 오차의 등분산성 가정이 위배된다고 할 수 있다.
spreadLevelPlot(m3) #. Suggested power transformation 값은 일정하지 않은 오차의 분산을 안정화시키기 위해 필요한 power transformation 값을 제시해준다. ( 0.5의 경우 Y 대신 sqrt(Y) 사용, 0인 경우 log사용 등 ) 






###< 로그변환 >

(log_m3 <- lm(log(price)~., data=train3))
summary(log_m3)
# 로그변환후 결정계수도 0.77로 더 높아진 것을 확인할 수 있음. 



## 오차항 가정 4가지

par(mfrow=c(2,2))
plot(m3)


plot(log_m3)


# 정규성 검정 - 
library(car)
qqPlot(log_m2,labels=row.names(train2),id.method="identify",simulate=TRUE,main="Q-Q_ plot")
# > 종속변수(가격)에 로그변환을 취해주니 잔차가 정규성을 띄는 것을 확인할 수 있음.
qqPlot(log_m4,labels=row.names(train4),id.method="identify",simulate=TRUE,main="Q-Q_ plot")

hist(train$price)
hist(log(train$price))
qplot(train$price)
qplot(log(train$price))
# 로그변환 후 price가 정규분포에 가까워짐을 확인.

# 독립성 검정 -
durbinWatsonTest(log_m4) # p값이 의미가 없으므로 자기상관은 없다고 할 수 있다. lag의 값 1은 각각의 자료를 바로 다음 자료와 비교했다는 것은 뜻한다

# 선형성 
crPlots(log_m4)

# 등분산성 
ncvTest(log_m4)
# 여기서 유의한 결과 가 나온다면 오차의 등분산성 가정이 위배된다고 할 수 있다.
spreadLevelPlot(log_m4) #. Suggested power transformation 값은 일정하지 않은 오차의 분산을 안정화시키기 위해 필요한 power transformation 값을 제시해준다. ( 0.5의 경우 Y 대신 sqrt(Y) 사용, 0인 경우 log사용 등 ) 


# 선형모형에 대한 전반적인 검증
install.packages('gvlma')
library(gvlma)
gvmodel<-gvlma(log_m4)
summary(gvmodel)
# 출력물 중 Global stat 을 보면 p값이 0.597로 OLS 회귀의 모든 가정을 만족한다고 할 수 있다. 만일 p값이 0.05 이하인 경우에는 어느 부분이 위배되었는지 평가하여야 한다.
# > Global Stat  > 0.00000로 가정 만족하지 않음.
gvmodel<-gvlma(reduced.model)
summary(gvmodel) # 마찬가지로 가정 만족하지 않음.

# 이상값
par(mfrow=c(1,1))
library(car)
car::outlierTest(log_m2)
car::outlierTest(log_m4) 
#  이 함수는 가장 큰 잔차가 outlier인지의 통계적인 유의성을 보여준다. 
# 가장 큰 잔차가 유의하지 않다면 데이터에 더 이상 이상치는 없다는 뜻이다. 
# 만일 유의할 경우 이상치를 제거하고 다시 outliertest()를 실시해 보아야 한다.


car::influencePlot(log_m2, id.method="identify", main="Influence Plot",
                   sub="Circle size is proportional to Cook’s distance")
train4 <- train2[-c(1232,5469,6780,8757,8913),]
str(train4)
str(train2) # 5개의 이상값 제거됨을 확인
(log_m4 <-  lm(log(price)~., data=train4))
summary(log_m4) # Multiple R-squared:  0.7753,	Adjusted R-squared:  0.775 


## 모형 비교

anova(m2,m3)
# 두 모형을 비교한 F test에서 p값이 유의하지 않으면  fit1에서 두 변수를 제거하는 것은 정당하다. 두 모형의 R2 값과 adjusted R2값을 비교해보자.
summary(m2) # Multiple R-squared:  0.7007,	Adjusted R-squared:  0.7003 

(log_m2 <-  lm(log(price)~., data=train2))
summary(log_m2) # Multiple R-squared:  0.7741,	Adjusted R-squared:  0.7738 
summary(log_m4) # Multiple R-squared:  0.7753,	Adjusted R-squared:  0.775 

summary(m3) # Multiple R-squared:  0.7006,	Adjusted R-squared:  0.7003
summary(log_m3) # Multiple R-squared:  0.7712,	Adjusted R-squared:  0.7709 
# > 	Adjusted R-squared 로 비교시 train4를 로그변환한 'log_m4' 회귀모형이 더 유의한 것을 확인할 수 있음.






# 변수 5개에 로그변환
skew_columns = c('price','sqft_living', 'sqft_lot', 'sqft_basement')
train5 <- train2
for (c in skew_columns){
  train5[c] = log(train2[c])
}
str(train5)
length(train5)
# 로그변환 5가지에 한 것을 train5에 담음.


(log5_m5 <- lm(price~., data=train5))
summary(log5_m5) # Multiple R-squared:  0.7776,	Adjusted R-squared:  0.7773 로 지금까지의 다중회귀모형중 가장 결정계수가 크게 나옴.
plot(log5_m5)




#  모델 적합성 판정

# Backward stepwise selection: 모든 독립변수를 사용하는 모델에서 독립변수를 줄여가며 모델의 성능을 향상시키는 방식


## Automatic Backward Selection
backward <- step(m2, direction = "backward", trace = F)
summary(backward) # 이렇게 하면 유의미하지 않은 변수 제거?

#  다중공선성(Multicollinearity) 확인
install.packages("car")
library(car)
install.packages("psych")
library(psych)


pairs.panels(train3[names(train3)]) # 렉..

car::vif(m3) 
sqrt(car::vif(m3)) > 2
# 'sqrt(car::vif(fit))' 가 2보다 크면 다중공선성이 있는 것입니다. 
# false이면 다중공선성이 없다고 판단할 수 있습니다. 

# 또ㄴ
vif(backward)법
sqrt(vif(backward))>2

## >> sqft_living, sqft_above 는 다중공선성 문제가 있는 것으로 보임.





##< 4.) 매매가 예측하기. >##

# 먼저 상수항만 포함된 회귀모형을 만들어 줍니다.


m3.con <- lm(price~1,data=train2) 
m3.both <- step(m3.con,scope=list(lower=m3.con,upper=m3), direction = "both") # 로그변환 안한거
summary(m3.both)
# 0.05 유의수준으로 보았을 때, m3에 있는 변수 모두 설명변수(x)로 유의함. 


m3.con <- lm(price~1,data=train5) 
m3.both <- step(m3.con,scope=list(lower=m3.con,upper=log5_m5), direction = "both") # 로그변환 한거
m3.both
summary(m3.both)



# 마지막으로 지금까지 새운 모델로 집값(y, 종속변수)을 예측해 보겠습니다.
# pridict 함수를 이용해서 예측하고, 보기좋게 data frame 형태로 바꿔줍니다.

pre_price <- predict(m3.both, newdata = train5) 
pre_price <- as.data.frame(pre_price)
pre_price
head(pre_price) # 예측된 집값.

# 위 방법은 점추정이었으며 이번에는 구간추정을 해보겠습니다.

pre_price <- predict(m3.both, newdata = train5, interval = "confidence") 
pre_price <- as.data.frame(pre_price)
pre_price
head(pre_price) # lwr와 upr로 구간이 나타나게 됩니다.


# 그렇다면 얼마나 잘 예측했는지 실제값과 비교해 보겠습니다.

predict_price <- cbind(pre_price,train5$price) 
head(predict_price,10) # >> 실제값(train$price)이 예측한 구간안에 있는 것도 있고  아닌 것도 있음을 확인할 수 있다.

# 로그변환을 한log5_m5를 예측해 보았을 때 보다 더 예측이 잘되는 것을 확인할 수 있었다.
