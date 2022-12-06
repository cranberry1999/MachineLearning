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
library(rpart) # �Ӽ����� ���Ѿ��� pruning�� �����ؾ���
set.seed(1234) # ���� �õ带 �ָ� ���� ���� ���� �� �ִ�

e_DT <- rpart(Class~., D.tra, method = "class", 
              parms = list(split="information"))  # ȸ�͸� ��� ���� �׷��� ������ �з��� �ϹǷ� class��� �˷������ 
# split="information"  : ��Ʈ������ gain�� information gain�̶�� �Ѵ�
plot(e_DT, margin = 0.2)
text(e_DT)

e_DT
# Thal=bc���� bc�� Thal���� ������ �Ӽ��� a,b,c ��� ���� ���̴� 

Predict.tra <- predict(e_DT, D.tra, type = "class") # ȸ�Ͱ� �ƴ� �з��� 
Actual.tra <- D.tra$Class 
head(Predict.tra)
head(Actual.tra)



library(caret)
# install.packages("e1071")
library(e1071)

cm1.tra <- confusionMatrix(Predict.tra, Actual.tra, positive = "Disease") # �츮�� ������ ���� ���� �ִ� �� 
cm1.tra 
# Accuracy : 0.8708  
# Sensitivity : 0.9024   
# Specificity : 0.8438  

cTab <- table(Actual.tra, Predict.tra)  
cTab  # �̰� ǥ���� 


Predict.tra <- predict(e_DT, D.tra, type = "class") # ȸ�Ͱ� �ƴ� �з��� 
Actual.tra <- D.tra$Class 
cm1.tra <- confusionMatrix(Predict.tra, Actual.tra, positive = "Disease") # �츮�� ������ ���� ���� �ִ� �� 
cTab <- table(Actual.tra, Predict.tra)  

CM.tra <- confusionMatrix(t(cTab), positive = "Disease")
# Accu.tra <- confusionMatrix(CM.tra, positive = "Disease")
CM.tra

str(Accu.tra)


#validation
Predict.tra <- predict(e_DT, D.tra, type = "class") # ȸ�Ͱ� �ƴ� �з��� 
Actual.tra <- D.tra$Class 

cm1.tra <- confusionMatrix(Predict.tra, Actual.tra, positive = "Disease") # �츮�� ������ ���� ���� �ִ� �� 
cTab <- table(Actual.tra, Predict.tra)  

CM.tra <- confusionMatrix(t(cTab), positive = "Disease")
# Accu.tra <- confusionMatrix(CM.tra, positive = "Disease")
CM.tra

# ����� 
printcp(e_DT)
# xerror���� ���� ���� �� �������� 












