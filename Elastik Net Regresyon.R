####ELASTKNET REGRESYON##
diamonds<-read.csv('diamonds.csv' , header = TRUE , sep = "," , dec = ".")
View(diamonds)
nrow(diamonds)
names(diamonds)

library(tidyverse)
library(mice)
library(glmnet)I
library(caret)
library(VIM)

unique(diamonds$color)
unique(diamonds$clarity)
unique(diamonds$cut)

which(is.na(diamonds))

md.pattern(diamonds)
fig <- aggr(diamonds , col = c("orange" , "red") , labels = names(diamonds),
            numbers = TRUE , sortVars = TRUE, cex.axis = 0.6 , 
            ylab(c("Histogram of Missing Values" , "Pattern"))
)
fig

class(diamonds$cut)
diamonds$cut<-as.factor(diamonds$cut)
class(diamonds$color)
diamonds$cut<-as.factor(diamonds$cut)
class(diamonds$clarity)
diamonds$clarity<-as.factor(diamonds$clarity)

## One Hot Encoding Dummy Degisken
#numerik degiskenlerde bir degisiklik olmaz
modelData<- model.matrix(price ~.  , data  = diamonds)
head(modelData)

#train set ve test set bolme
set.seed(145)
train_test_set<- sample(1:nrow(modelData),size=0.80*nrow(modelData))
trainsetx<-modelData[train_test_set,]
testsetx<-modelData[-train_test_set,]
trainsety<- diamonds$price[train_test_set]
testsety<- diamonds$price[-train_test_set]
#Elastiknet regresyonda lamda 0 ile 1 aras??nda tek deger alir
model<-glmnet(trainsetx,trainsety,alpha=0.5,lambda =10^seq(from=2,to=-2,by=-0.01))
model

plot(model , xvar = "lambda")
legend("bottomright" , lwd = 1 , col = 1:nrow(trainsetx) , legend = colnames(trainsetx) )

## Cross Validation
#uygun alpha ve lambda degerini bulmak icin cross validation yapariz.

library(caret)
model_control<-trainControl(method = "repeatedcv",number=3, repeats = 5,search = "random",verboseIter = TRUE)
model_control
elas_model<-train(trainsetx,trainsety,method = "glmnet",tuneLength = 30,trControl = model_control)
elas_model

elas_model$bestTune # en iyi alpha ve lambda degeri
elas_model$finalModel # en son bulunan en iyi tahmin edilen modeli verir.  %Dev en yuksek olan secilir.


model_control_grid<-trainControl(method = "repeatedcv",number=3, repeats = 5,verboseIter = TRUE)
model_control_grid

alpha_lambda<-expand.grid(alpha=seq(from=0,to=1,by=0.05),lambda=seq(from=0,to=1,by=0.1))
alpha_lambda

elas_model_grid<-train(trainsetx,trainsety,method = "glmnet",tuneGrid = alpha_lambda,trControl = model_control_grid)
elas_model_grid


elas_model_grid$bestTune # en iyi alpha ve lambda degeri
elas_model_grid$finalModel


modelLookup(model = "glmnet")

### Model Tahmin Performans Degerlendirmesi 
elas_model$bestTune
model_predict<-glmnet(trainsetx,trainsety,alpha = 0.8482166,lambda = 0.003651645)
pred<-predict(model_predict,testsetx)
pred
caret::R2(pred,testsety)
caret::MAE(pred,testsety)
caret::RMSE(pred,testsety)


model_predict_grid<-glmnet(trainsetx,trainsety,alpha =   0.45,lambda = 1)
pred_grid<-predict(model_predict_grid,testsetx)
caret::R2(pred_grid,testsety)
caret::MAE(pred_grid,testsety)
caret::RMSE(pred_grid,testsety)


##EKK Karsilastirmasi

model_ols <- glmnet(trainsetx,trainsety,alpha =1,lambda = 0)
model_ols
pred_ekk<-predict(model_ols,testsetx)
caret::R2(pred_ekk,testsety)
caret::MAE(pred_ekk,testsety)
caret::RMSE(pred_ekk,testsety)

data_predict<- data.frame("preditions"=pred,
                          "actuals"=testsety)

data_predict1<- data.frame("preditions"=pred_grid,
                          "actuals"=testsety)

data_predict2<- data.frame("preditions"=pred_ekk,
                          "actuals"=testsety)
library(formattable)
formattable(data_predict)
formattable(data_predict1)
formattable(data_predict)



