## 2) Location Variable
### ����(lat)�� ���� ���� ����(price)
# �� �������� ������ �� ȸ������
p1 <- house %>% ggplot(aes(x=lat, y=price))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="red")+
  ggtitle("price by lat")

p2 <- house %>% ggplot(aes(x=lat, y=log(price)))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="red")+
  ggtitle("log(price) by lat")

grid.arrange(p1, p1, ncol=2)
# log��ȯ�� price������ �������� ���� ������谡 ���������� ����ȴ�.
# 

# price�� lat������ ����м�
cor.test(house$price, house$lat)
cor.test(log(house$price), house$lat)
# log ��ȯ�� �� ��� ���� �����ϴٰ� ��Ÿ������ ������谡 ���ٰ� �Ǵ��ϱ� �����.


# �ܼ�ȸ�ͺм�
out <- lm(price ~ lat, data=house)
summary(out)

log_out <- lm(log(price) ~ lat, data=house)
summary(log_out)
#������ ������ ���ǹ� ������ ȸ�ͼ��� �������� ������ �� �� �ִ�.

# 95% �ŷڱ���
confint(out,level = 0.95); out$coefficients
confint(log_out,level = 0.95); log_out$coefficients
# �� ������ ������ ��� 95%�ŷڱ����� ���ϱ� ������ 95%���ؿ��� �����ϴٰ� �Ǵ��Ѵ�.

# �л�м�ǥ(ANOVA Table)
anova(out)
anova(log_out)
# �������(= SSR / SST)�� �۴ٰ� �Ǵ��Ͽ� ȸ�ͼ��� �������� ���ٰ� �� �� �ִ�.


### �浵(long)�� ���� ���� ����(price)
p1 <- house %>% ggplot(aes(x=long, y=price))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="blue")+
  ggtitle("Price by long")
# �浵-���ݰ��� ������ �� ȸ������

p2 <- house %>% ggplot(aes(x=lat, y=log(price)))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="blue")+
  ggtitle("Log price by long")

grid.arrange(p1,p2,ncol=2)

### 2. ���� ���濡 ���� �帣���� ����(waterfront)�� ���� ���� ����(price)
# waterfront : ���� ���濡 ���� �帣���� ���� (a.k.a. ������)

house$waterfront <- as.character(house$waterfront)
# int������ waterfront�����͸� character���·� ��ȯ�Ѵ�.(ggplot���� �ð�ȭ�� ����)

p1 <- house %>% ggplot(aes(x=waterfront,y=price))+
  geom_boxplot()+
  ggtitle("Price by waterfront")
p2 <- house %>% ggplot(aes(x=waterfront,y=log(price)))+
  geom_boxplot()+
  ggtitle("Log price by waterfront")
grid.arrange(p1,p2,ncol=2)
# river view�� ���� ������ ��ü������ �������� �� ���ִ�.

# �Ͽ���ġ �л�м�
# ������ ��л꼺 ����(bartlett.test)

analysis <- aov(price ~ waterfront, data=house)
summary(analysis)

analysis <- aov(log(price) ~ waterfront, data=house)
summary(analysis)

bartlett.test(log(price) ~ waterfront, data=house)

# �Ͽ���ġ �л�м��� ���� ���� ���濡 ���� �帣���� ������ ���� ������ ����� ���ǹ����� �Ǵ��Ѵ�.
# �Ͽ���ġ �л�м��� �����ϱ� ���� ������������ ������ ��л꼺�� �����ؾ��Ѵ�.
# �׷��� ������ ��л꼺 ������ ���, p-value�� �ſ� �۱⶧���� �͹������� �Ⱒ�Ͽ� ������ ��л꼺 ������ �����Ѵ� �Ǵ��� �� ����.


