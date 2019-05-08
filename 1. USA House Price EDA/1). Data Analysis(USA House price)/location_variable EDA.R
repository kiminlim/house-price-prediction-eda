## 2) Location Variable
### 위도(lat)에 따른 집의 가격(price)
# 두 변수간의 산점도 및 회귀직선
p1 <- house %>% ggplot(aes(x=lat, y=price))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="red")+
  ggtitle("price by lat")

p2 <- house %>% ggplot(aes(x=lat, y=log(price)))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="red")+
  ggtitle("log(price) by lat")

grid.arrange(p1, p1, ncol=2)
# log변환한 price변수와 위도에서 약한 상관관계가 있을것으로 예상된다.
# 

# price와 lat사이의 상관분석
cor.test(house$price, house$lat)
cor.test(log(house$price), house$lat)
# log 변환을 한 경우 좀더 유의하다고 나타나지만 상관관계가 높다고 판단하기 힘들다.


# 단순회귀분석
out <- lm(price ~ lat, data=house)
summary(out)

log_out <- lm(log(price) ~ lat, data=house)
summary(log_out)
#각각의 변수는 유의미 하지만 회귀선의 설명력이 낮음을 알 수 있다.

# 95% 신뢰구간
confint(out,level = 0.95); out$coefficients
confint(log_out,level = 0.95); log_out$coefficients
# 두 변수의 추정값 모두 95%신뢰구간에 속하기 때문에 95%수준에서 유의하다고 판단한다.

# 분산분석표(ANOVA Table)
anova(out)
anova(log_out)
# 결정계수(= SSR / SST)가 작다고 판단하여 회귀선의 설명력이 낮다고 할 수 있다.


### 경도(long)에 따른 집의 가격(price)
p1 <- house %>% ggplot(aes(x=long, y=price))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="blue")+
  ggtitle("Price by long")
# 경도-가격간의 산점도 및 회귀직선

p2 <- house %>% ggplot(aes(x=lat, y=log(price)))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="blue")+
  ggtitle("Log price by long")

grid.arrange(p1,p2,ncol=2)

### 2. 집의 전방에 강이 흐르는지 유무(waterfront)에 따른 집의 가격(price)
# waterfront : 집의 전방에 강이 흐르는지 유무 (a.k.a. 리버뷰)

house$waterfront <- as.character(house$waterfront)
# int형태의 waterfront데이터를 character형태로 변환한다.(ggplot으로 시각화를 위해)

p1 <- house %>% ggplot(aes(x=waterfront,y=price))+
  geom_boxplot()+
  ggtitle("Price by waterfront")
p2 <- house %>% ggplot(aes(x=waterfront,y=log(price)))+
  geom_boxplot()+
  ggtitle("Log price by waterfront")
grid.arrange(p1,p2,ncol=2)
# river view인 집의 가격의 전체적으로 높은것을 알 수있다.

# 일원배치 분산분석
# 오차의 등분산성 검정(bartlett.test)

analysis <- aov(price ~ waterfront, data=house)
summary(analysis)

analysis <- aov(log(price) ~ waterfront, data=house)
summary(analysis)

bartlett.test(log(price) ~ waterfront, data=house)

# 일원배치 분산분석을 통해 집의 전방에 강이 흐르는지 유무에 따른 집값의 평균이 유의미한지 판단한다.
# 일원배치 분산분석이 성립하기 위한 전제조건으로 오차의 등분산성을 만족해야한다.
# 그러나 오차의 등분산성 검정의 결과, p-value가 매우 작기때문에 귀무가설을 기각하여 오차의 등분산성 가정을 만족한다 판단할 수 없다.



