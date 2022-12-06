# SETED
setwd("C:/R DA-ML/8 DT")

rm(list=ls())
D.data <- read.csv("heartD.csv", header = T)
Tra <- read.csv("Tra1.csv", header = T)
Val <- read.csv("Val1.csv", header = T)
Tes <- read.csv("Tes1.csv", header = T)

head(Tra)
D.tra <- Tra[,-c(1)]
D.val <- Val[,-c(1)]
D.tes <- Tes[,-c(1)]

#install.packages("rpart")
library(rpart) # 속성개수 제한없고 pruning은 직접해야함
set.seed(1234) # 같은 시드를 주면 같은 값을 얻을 수 있다

e_DT <- rpart(Class~., D.tra, method = "class", 
              parms = list(split="information"))  # 회귀를 사용 가능 그러나 지금은 분류를 하므로 class라고 알려줘야함 
# split="information"  : 엔트로피의 gain을 information gain이라고 한다
plot(e_DT, margin = 0.2)
text(e_DT)

e_DT
# Thal=bc에서 bc는 Thal에서 범주형 속성을 a,b,c 라고 나눈 것이다 

Predict.tra <- predict(e_DT, D.tra, type = "class") # 회귀가 아닌 분류모델 
Actual.tra <- D.tra$Class 
head(Predict.tra)
head(Actual.tra)



library(caret)
# install.packages("e1071")
library(e1071)

cm1.tra <- confusionMatrix(Predict.tra, Actual.tra, positive = "Disease") # 우리가 예측할 것은 병이 있는 것 
cm1.tra 
# Accuracy : 0.8708  
# Sensitivity : 0.9024   
# Specificity : 0.8438  

cTab <- table(Actual.tra, Predict.tra)  
cTab  # 이게 표준형 


Predict.tra <- predict(e_DT, D.tra, type = "class") # 회귀가 아닌 분류모델 
Actual.tra <- D.tra$Class 
cm1.tra <- confusionMatrix(Predict.tra, Actual.tra, positive = "Disease") # 우리가 예측할 것은 병이 있는 것 
cTab <- table(Actual.tra, Predict.tra)  

CM.tra <- confusionMatrix(t(cTab), positive = "Disease")
# Accu.tra <- confusionMatrix(CM.tra, positive = "Disease")
CM.tra

str(Accu.tra)


#validation
Predict.tra <- predict(e_DT, D.tra, type = "class") # 회귀가 아닌 분류모델 
Actual.tra <- D.tra$Class 

cm1.tra <- confusionMatrix(Predict.tra, Actual.tra, positive = "Disease") # 우리가 예측할 것은 병이 있는 것 
cTab <- table(Actual.tra, Predict.tra)  

CM.tra <- confusionMatrix(t(cTab), positive = "Disease")
# Accu.tra <- confusionMatrix(CM.tra, positive = "Disease")
CM.tra

# 프루닝 
printcp(e_DT)
# xerror에서 가장 작은 값 가져오기 













