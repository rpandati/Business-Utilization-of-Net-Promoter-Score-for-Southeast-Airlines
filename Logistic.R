##### LOGISTIC REGRESSION #####
df_new<-read.csv("C:\\Users\\Krishna\\Desktop\\data.csv")
df_new<-df_new[,-1]
library(caTools)

set.seed(88)
split <- sample.split(df_new$Likelihood.to.recommend, SplitRatio = 0.75)
train <- subset(df_new, split == TRUE)
test <- subset(df_new, split == FALSE)
model <- glm(Likelihood.to.recommend ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")

fitted.results <- predict(model,newdata=subset(test),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Likelihood.to.recommend,na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))

###"Accuracy 0.756420233463035"
library(caret)
varImp(model, scale = FALSE)
plot(varImp)
