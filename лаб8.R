iris <- read.csv("iris.csv")
#KNN
set.seed(4)
iris$variety <- factor(iris$variety)  
variety_vector <- iris[,5]
iris <- as.data.frame(scale(iris[,-5]))
iris <- data.frame(iris,variety_vector)
str(iris)
index <- sample(1:nrow(iris), round(0.6*nrow(iris)))
train <- iris[index, ]
test <- iris[-index, ]
knn1 <- knn(train, test, train$variety_vector, k = 5)
report_knn_table <- table(knn1, test$variety_vector)
#Lab8
library(ggplot2)
library(tidyverse)
ggplot(data = iris)+
  geom_point(mapping = aes(x = sepal.length, y = sepal.width, color = variety_vector))
ggplot(data = iris)+
  geom_point(mapping = aes(x= sepal.length, y = sepal.width))+
  facet_wrap(~variety_vector, nrow = 2)
ggplot(data = iris)+
  geom_point(mapping = aes(x= petal.length, y = petal.width))+
  facet_grid(sepal.length~sepal.width)
ggplot(data = iris)+
  geom_smooth(mapping = aes(x= sepal.length, y = sepal.width, linetype = variety_vector))
ggplot(data = iris)+
  geom_point(mapping = aes(x = petal.length, y = petal.width, color = variety_vector))+
  geom_smooth(mapping = aes(x= petal.length, y = petal.width))
ggplot(data = iris,
       mapping = aes(x =variety_vector, y = petal.width))+
  geom_boxplot()+
  coord_flip()
ggplot(data = iris)+
  geom_bar(mapping = aes(x =variety_vector))
ggplot(data = iris)+
  stat_summary(
    mapping = aes(x = variety_vector, y = sepal.width ),
    fun.min = min, # здесь мы видим диаграмму размахов по категориям
    fun.max = max,
    fun = median)
bar <- ggplot(data = iris)+
  geom_bar(
    mapping = aes(x = variety_vector , fill = variety_vector),
    show.legend = F,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

#Lab9
lm.fit = lm(sepal.length~sepal.width, data = iris)
attach(iris)
summary(lm.fit)
lm.fit$coefficients
lm.fit$residuals
names(lm.fit)

coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(sepal.width=(c(5,10,15))),
        interval = "confidence")
predict(lm.fit, data.frame(sepal.width=(c(5,10,15))),
        interval = "prediction")
plot(sepal.width, sepal.length)
abline(lm.fit)
ggplot(data = lm.fit)+
  geom_point(alpha = 1/3, mapping = aes(x = lm.fit$fitted.values, y=lm.fit$residuals, fill = lm.fit$effects))
ggplot(data = iris) +
  geom_point(mapping = aes(x = sepal.width, y = sepal.length, color = petal.length))+
  geom_smooth(mapping = aes(x= sepal.width, y = sepal.length))
ggplot(data = iris) +
  geom_point(mapping = aes(x = sepal.width, y = sepal.length, color = petal.width))
lm.fit <- lm(sepal.length~sepal.width+petal.length, data = iris)
summary(lm.fit)
