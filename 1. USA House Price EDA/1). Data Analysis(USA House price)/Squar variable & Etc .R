## 3) Sqaure feet variables
### 주거 공간 면적(sqft_living)에 따른 집의 가격(price)
### sqft_living : 주거 공간의 평방 피트(면적)

# 두 변수간의 산점도

p1 <- house %>% ggplot(aes(x=sqft_living, y=price))+
  geom_point(shape=1, alpha=0.2, color="#DC143C")+
  geom_smooth(method = lm,color="#6495ED")+
  ggtitle("Price by sqft_living")

p2 <- house %>% ggplot(aes(x=sqft_living, y=log(price)))+
  geom_point(shape=1, alpha=0.2, color="#DC143C")+
  geom_smooth(method = lm,color="#6495ED")+
  ggtitle("Log price by sqft_living")

grid.arrange(p1,p2,ncol=2)
# log변환전후에 회귀직선에서 멀리떨어져 면적대비 가격이 지나치게 낮은(혹은 높은) 데이터가 있다.
# 이상점이 의심됨으로  outlierTest 를 통해 확인한다.
# 산점도와 회귀직선을 통해 주거 공간 면적과 집의 가격에는 유의한 관계가 있음을 알 수 있다.
# 정확한 수치를 통해 두 변수간의 유의함을 검정한다.

# price와 sqft_living사이의 상관분석
cor.test(house$price, house$sqft_living)
cor.test(log(house$price), house$sqft_living)
# 검정통계량 t가 크고 p값이 매우 작기 때문에 두 변수사이에 유의한 상관관계가 있다고 판단한다.
# 상관계수 = 0.70로 1에 가깝다고 판단하여 두 변수간에 유의한 상관관계가 있다고 할 수 있다.
# log변환한 종속변수(price)와 의 상관계수 또한 변환이전의 상관계수와 매우 유사하다.

# 단순회귀분석 및 이상치 확인

out <- lm(price ~ sqft_living, data=house)
summary(out)

log_out <- lm(log(price) ~ sqft_living, data=house)
summary(log_out)

outlierTest(out)
rstudent(out) %>% summary()
outlierTest(log_out)
# y절편과 sqft_living변수에 대한 p값이 매우작고 결정계수는 0.49이다
# outlierTest를 한 결과 원래 price에 대한 이상치가 10개, log변환한 price에 대한 이상치 1개가 발견 되었다.
# rstudent를 통해 스튜던트화 잔차에 대해 summary해본 결과 Q3와 Max사이에 엄청난 차이가 있음을 알 수 있었다. 

# 로그변환후 outlierTest를 통해 나온 데이터만 제외하고 다시 확인해보았다.
test <- house[-c(8913),]
test_out <- lm(log(price) ~ sqft_living, data=test)
summary(test_out)

# 95% 신뢰구간
confint(out,level = 0.95)
# y절편과 x(sqrt_living)에 대한 추정값 모두 신뢰구간안에 속하기 때문에 95%수준에서 유의하다고 할 수 있다.


# 분산분석표(ANOVA Table)
anova(out)
anova(log_out)
# F통계량에 대한 유의확률이 매우 작기 때문에 두 변수 모두 회귀모형을 잘 설명해주고 있다고 판단한다.


### 지하실을 제외한 면적(sqft_above)에 따른 집의 가격(price)
### sqft_above : 지하실을 제외한  평방 피트(면적)

# 두 변수간의 산점도

p1 <- house %>% ggplot(aes(x=sqft_above, y=price))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="blue")+
  ggtitle("Price by sqft_above")

p2 <- house %>% ggplot(aes(x=sqft_above, y=log(price)))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="blue")+
  ggtitle("Log price by sqft_above")

grid.arrange(p1,p2,ncol=2)

# 산점도와 회귀직선을 통해 주거 공간 면적과 집의 가격에는 유의한 관계가 있을것이라 예상한다..
# 그러나 지하실을 제외한 면적이 2750피트보다 작은 구간에 집중되어 있어 그래프상의 관계와 수치상의 상관관계가 조금 다를것으로 예상된다.
# 따라서 정확한 수치를 통해 두 변수간의 유의함을 검정한다.


# price와 sqft_living사이의 상관분석
cor.test(house$price, house$sqft_above)
cor.test(log(house$price), house$sqft_above)
# 검정통계량 t가 크고 p값이 매우 작기 때문에 두 변수사이에 유의한 상관관계가 있다고 판단한다.
# 상관계수 = 0.60로 1에 가깝다고 판단하여 두 변수간에 유의한 상관관계가 있다고 할 수 있다.
# log변환한 종속변수(price)와 의 상관계수 또한 변환이전의 상관계수와 매우 유사하다.

# 단순회귀분석
out <- lm(price ~ sqft_above, data=house)
summary(out)

log_out <- lm(log(price) ~ sqft_above, data=house)
summary(log_out)
# y절편과 sqft_above변수에 대한 p값이 매우작고 결정계수가 약 0.4으로 
# p-value는 매우 작지만 추정된 회귀식의 설명력이 높다고 판단하기 모호하다.


# 95% 신뢰구간
confint(out,level = 0.95); out$coefficients
confint(log_out,level = 0.95); log_out$coefficients
# y절편과 x(sqrt_living)에 대한 추정값 모두 신뢰구간안에 속하기 때문에 
# 두 변수 모두 95%수준에서 유의하다고 할 수 있다.


# 분산분석표(ANOVA Table)
anova(out)
anova(log_out)

### sqft_lot: 부지의 평방피트(면적)
#he lot size is the living size + front yard, back yard (total land space)
# 두 변수간의 산점도
head(house$sqft_lot)

p1 <- house %>% ggplot(aes(x=sqft_lot, y=price))+
  geom_density(alpha=0.5)+
  ggtitle("Price by sqft_lot")

p2 <- house %>% ggplot(aes(x=sqft_lot, y=log(price)))+
  geom_density(alpha=0.5)+
  ggtitle("Log price by sqft_lot")

grid.arrange(p1,p2,ncol=2)


p3 <- house %>% ggplot(aes(sqft_lot))+
  geom_density(alpha=0.5)+
  ggtitle("density plot of sqft_lot")

grid.arrange(p3)

## sqft_basement
p1 <- house %>% ggplot(aes(sqft_basement)) + 
  geom_density(alpha=0.3) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Density plot of sqft_basement")

p2 <- house %>% ggplot(aes(x=sqft_basement, y=price)) +
  geom_point(shape=3, alpha=0.3) +
  geom_smooth(method=lm, linetype="dashed", color="red") +
 
  ggtitle("Scatter plot of sqft_basement and price")

p3 <- house %>% ggplot(aes(x=sqft_basement, y=log(price))) +
  geom_point(shape=3, alpha=0.3) +
  geom_smooth(method=lm, linetype="dashed", color="red") +
  ggtitle("Scatter plot of sqft_basement and log price")

grid.arrange(p1, p2, p3, ncol=3)

### sqft_living15: 2015년 기준 주거 공간의 평방 피트(면적)
# 단, 집을 재건축했다면 변화가 있을 수 있음

p1 <- house %>% ggplot(aes(sqft_living15))+ 
  geom_density(alpha=0.3)+
  ggtitle("Density plot of sqft_living15")

p2 <- house %>% ggplot(aes(x=sqft_living15, y=price)) +
  geom_point(shape=3, alpha=0.3) +
  geom_smooth(method=lm, linetype="dashed", color="red") +
  ggtitle("Scatter plot of sqft_living15 and price")

p3 <- house %>% ggplot(aes(x=sqft_living15, y=log(price))) +
  geom_point(shape=3, alpha=0.3) +
  geom_smooth(method=lm, linetype="dashed", color="red", fill="blue")+
  ggtitle("Scatter plot of sqft_living15 and price")

grid.arrange(p1,p2,p3, ncol=3)

# price와 sqft_living15사이의 상관분석
cor.test(house$price, house$sqft_living15)
cor.test(log(house$price), house$sqft_living15)
# 검정통계량 t가 크고 p값이 매우 작기 때문에 두 변수사이에 유의한 상관관계가 있다고 판단한다.
# 상관계수가 로그변환이전엔 0.58, 로그변환이후 0.62로 두 변수간에 유의한 상관관계가 있다고 할 수 있다.

# 단순회귀분석 및 이상치 확인

out <- lm(price ~ sqft_living15, data=house)
summary(out)

log_out <- lm(log(price) ~ sqft_living, data=house)
summary(log_out)

outlierTest(out)
rstudent(log_out) %>% summary()
outlierTest(log_out)
# y절편과 sqft_living15변수에 대한 p값이 매우작고 결정계수는 0.49이다
# outlierTest를 한 결과 sqft_living회귀식에 대해 이상치 검사를 했을때와 동일한 데이터가 출력되었다.

# 로그변환후 outlierTest를 통해 나온 데이터만 제외하고 다시 확인해보았다.
test <- house[-c(8913),]
test_out <- lm(log(price) ~ sqft_living15, data=test)
summary(test_out)

# 95% 신뢰구간
confint(out,level = 0.95)
# y절편과 x(sqrt_living)에 대한 추정값 모두 신뢰구간안에 속하기 때문에 95%수준에서 유의하다고 할 수 있다.


# 분산분석표(ANOVA Table)
anova(out)
anova(log_out)

# sqft_living과 sqft_living15의 유사도
sum(house$sqft_living == house$sqft_living15)/nrow(house)
# sqft_living과 sqft_living15의 데이터가 12%정도 유사한것으로 판단된다.
# 재건축 시에 값이 변할 수 있다고 했으므로 재건축 한 집의 sqft 값이 변한 것인지 확인필요



living_diff <- house %>% filter(renovated=="1") %>% select(sqft_living,sqft_living15,price)
sum(living_diff[,1]==living_diff[,2] )
# 재건축한 건물 500개 중 면적이 변한 데이터는 전체 500개 중 461개로 전체 92%가 면적이 변함을 확인

## Etc
### grade : King County grading 시스템 기준으로 매긴 집의 등급


house %>% select(grade) %>% table

p1 <- house %>% select(grade) %>% group_by(grade) %>% summarise(n_grade=n()) %>% 
  mutate(percentage = n_grade/sum(n_grade), total = sum(n_grade)) %>%
  ggplot(aes(x=factor(grade), group=factor(grade),y=percentage)) + 
  geom_bar(alpha=0.5, stat="identity") + 
  ggtitle("Barplot of grade")

p2 <- house %>% ggplot(aes(x=factor(grade),y=price),group=factor(grade))+
  geom_boxplot()+
  ggtitle("boxplot of grade")
p3 <- house %>% ggplot(aes(x=factor(grade), group=factor(grade),y= log(price)))+
  geom_boxplot()+
  ggtitle("boxplot of grade2")

grid.arrange(p1,p2,p3,ncol=3)

str(house)
