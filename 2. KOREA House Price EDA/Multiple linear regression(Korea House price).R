
##< 1) 데이터 불러오기 및 파악
KD <- read.csv('KD.csv')
KD <- read.csv('KD2.csv')

# is.na(KD)
tail(KD)

#KD <- KD[which(complete.cases(KD)),]
str(KD)
sum(is.na(KD))
summary(KD)
summary(KD$price) # 평균 13억.서울시..

# KD$price <- as.integer(KD$price)
str(KD)

##< 2) 데이터 시각화 및 변수제거

# 정규화 
hist(KD$price) 
hist(log(KD$price)) # 로그변환 무의미.
hist(exp(KD$price)) #  =

hist(KD$price)
hist(KD$sqft_living)
hist(log(KD$sqft_living))
summary(KD$sqft_living)
K3 <- KD
K3$sqft_living <- log(KD$sqft_living)
K3$yr_built <- log(KD$yr_built)

hist(KD$yr_built)
hist(log(KD$yr_built))

str(K3)
K3 <- K3[-c(1,2,6)] # 회귀분석하기에 의미없는 변수 제거.



##< 3) 다중회귀분석

log_m1 <- lm(price~., data=K3)
summary(log_m1) #Multiple R-squared:  0.04092,	Adjusted R-squared:  0.03688 

log_m2 <- lm(price~., data=K3) # 로그변환 2개로 해서 다시돌려보았더니
summary(log_m2) #Multiple R-squared:  0.04112,	Adjusted R-squared:  0.03708  
# 결정계수가 더 높이 나오는 것을 확인.
cor(KD)


# 오차항 가정 4가지

plot(log_m1)


# 정규성 검정 - 
library(car)
qqPlot(log_m1,labels=row.names(train2),id.method="identify",simulate=TRUE,main="Q-Q_ plot")
# > 종속변수(가격)에 로그변환을 취해주니 잔차가 정규성을 띄는 것을 확인할 수 있음.
qqPlot(log_m2,labels=row.names(train2),id.method="identify",simulate=TRUE,main="Q-Q_ plot")

log_m1 <- log_m2 # 변수명 바꿔서 돌림.
shapiro.test(residuals(log_m1)) # 귀무가설 기각되면 정규성 만족 못한거?

# 독립성 검정 -
durbinWatsonTest(log_m1) 

# 선형성 
crPlots(log_m1)

# 등분산성 - 
ncvTest(log_m2)
# 여기서 유의한 결과 가 나온다면 오차의 등분산성 가정이 위배된다고 할 수 있다.
spreadLevelPlot(log_m2) #. Suggested power transformation 값은 일정하지 않은 오차의 분산을 안정화시키기 위해 필요한 power transformation 값을 제시해준다.
# ( 0.5의 경우 Y 대신 sqrt(Y) 사용, 0인 경우 log사용 등 ) 


# 선형모형에 대한 전반적인 검증
#install.packages('gvlma')
library(gvlma)
gvmodel<-gvlma(log_m1)
summary(gvmodel)
# 출력물 중 Global stat 을 보면 p값이 0.597로 OLS 회귀의 모든 가정을 만족한다고 할 수 있다. 만일 p값이 0.05 이하인 경우에는 어느 부분이 위배되었는지 평가하여야 한다.
# > Global Stat  > 3.277e-05ㄹ 가정 만족하지 않음.


# 이상값
par(mfrow=c(1,1))
library(car)
car::outlierTest(log_m1)
car::outlierTest(log_m1) # . 이 함수는 가장 큰 잔차가 outlier인지의 통계적인 유의성을 보여준다. 가장 큰 잔차가 유의하지 않다면 데이터에 더 이상 이상치는 없다는 뜻이다. 만일 유의할 경우 이상치를 제거하고 다시 outliertest()를 실시해 보아야 한다.

car::influencePlot(log_m1, id.method="identify", main="Influence Plot",
                   sub="Circle size is proportional to Cook’s distance")


## 대한민국 아파트 매매가 다중회귀분석


full.model=lm(price~.,data=K3)
reduced.model=step(full.model,direction="backward")
summary(reduced.model) # log2개 일때&변수2개 - ultiple R-squared:  0.04112,	Adjusted R-squared:  0.03708 > 
# 현재까지  log2개 일때&변수2개 일때 가 가장 잘 설명.


#  다중공선성(Multicollinearity) 확인
#install.packages("car")
library(car)
#install.packages("psych")
library(psych)

pairs.panels(K3[names(K3)]) # 없어보임.

car::vif(log_m1) 
sqrt(car::vif(log_m1)) > 2  # 모두 F 이무로 다중공선성문제 없음.



##< 4 ) KD데이터로 한국 강남구 지역의 집값 예측(회귀)

# 상수항만 포함된 회귀모형 

m3.con <- lm(price~1,data=K3) 
m3.both <- step(m3.con,scope=list(lower=m3.con,upper=log_m1), direction = "both")
m3.both
summary(m3.both)

# 점추정
pre_price <- predict(m3.both, newdata = K3) 
pre_price <- as.data.frame(pre_price)
pre_price
head(pre_price) # 예측된 집값.
summary(pre_price)
summary(K3$price)

# 구간추정

pre_price <- predict(m3.both, newdata = K3, interval = "confidence") 



pre_price <- as.data.frame(pre_price)
pre_price
head(pre_price) # lwr와 upr로 구간


# 얼마나 예측했는지 실제값과 비교

predict_price <- cbind(pre_price,K3$price) 
head(predict_price,10) # 이런 식으로 예측해볼 수 있음.

