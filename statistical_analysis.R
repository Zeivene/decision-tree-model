


#------------IMPORT LIBRARIES

library(psych)
library(tidyverse)


#----------IMPORT DATASET

col_names= c('sepal_length','sepal_width','petal_length','petal_width','target')
read_csv = read.csv('C:/Users/asgua/Downloads/Project Report/R/data_iris.txt', header=TRUE,col.names = col_names)
read_csv


#------------DATA PREPROCESSING
df<-read_csv
dim(df)
str(df)
sum<-summary(df)
sum
class(df)
colnames(df)



#--------------Columns that have missing values
list_na<-colnames(df)[colSums(is.na(df)) > 0]
list_na


#-------------
print("Count of total missing values - ")
sum(is.na(df))

#------DATA ANALYSIS

hist(df$petal_length, col = "light blue")


qqnorm(df$petal_length, col = "red")
qqline(df$petal_length, col = "black")


pairs.panels(iris, method = "pearson", hist.col = "#5D6D7E")



ggplot(data=df,aes(x=sepal_width, y=sepal_length,color=target)) + geom_point() + theme_minimal()


ggplot(data=df,aes(x=petal_length, y=petal_width,color=target)) + geom_point() +geom_smooth() + theme_minimal()



options(repr.plot.width = 5, repr.plot.height = 4)

ggplot(data=df,aes(x=target, y=petal_length,color=target)) + geom_boxplot() +theme_minimal()+
  theme(legend.position="none")


#-------------MULTIPLE LINEAR REGRESSION

lm_fit <- lm(petal_length ~ petal_width + sepal_width, data=df)
summary(lm_fit)


predicted_df <- data.frame(Petal.width_pred = predict(lm_fit, df), Petal.Length=df$petal_length, Sepal.Width=df$sepal_width)


plot(lm_fit, which = 1)


#------------------T-TEST

x <- df[df$target == "Iris-setosa", ]$sepal_length
y <- df[df$target == "Iris-versicolor", ]$sepal_length
tt <- t.test(x, y, paired = FALSE, alternative = "two.sided", var.equal = FALSE)
tt

