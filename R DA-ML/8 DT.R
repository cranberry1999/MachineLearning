#UCI Machine Learning Repository�� ������ ��ó

#��ó�� ��
- record ����: 297��
- �Ӽ� ����: 15��/ ��ǥ�Ӽ�: class
- ���������� �����(Gender, CP, Exang ���)

############����#############

setwd("C:/R DA-ML/8 DT")
rm(list=ls())


D.data <- read.csv("HeartD.csv", header = TRUE)

prop.table(D.data$Class)  #������ ���� �˾Ƴ���

Tra <- read.csv("Tra1.csv", header = TRUE)
Val <- read.csv("Val1.csv", header = TRUE)
Tes <- read.csv("Tes1.csv", header = TRUE)

#ù��° Į�� ����
head(Tra)
D.tra <- Tra[,- c(1)]
D.val <- Val[,- c(1)]
D.tes <- Tes[,- c(1)]

install.packages("rpart")
library(rpart) #����ġ�� �Ҷ� ���� �ϳ��� ����

set.seed(12345) #�Ȱ��� ����� ����
e_DT <- rpart(Class~., D.tra, method="class",   #method=class�� ȸ��
              parms = list(split="information")) #Class ���� ��� ������ ���
plot(e_DT)
text(e_DT)

e_DT #Thal=bc�� ������ T6,T7�� �ǹ� T3�� a�� �ǹ�.

Predict.tra <- predict(e_DT, D.tra, type="class") #type=class�� �з�
Actual.tra <- D.tra$Class
head(Predict.tra)
head(Actual.tra)

#confusionMatrix(�з����ǥ)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

#####Tra ���߷�######
cM1 <- confusionMatrix(Predict.tra, Actual.tra, positive = "Disease")
cM1 #���߷�:0.8708 / �ΰ���: 0.9024 / Ư�̵�: 0.8438

cTab <- table(Actual.tra, Predict.tra)  #������ ������ �ٲٴ� ��
cTab

Predict.tra <- predict(e_DT, D.tra, type="class") #type=class�� �з�
Actual.tra <- D.tra$Class
cTab <- table(Actual.tra, Predict.tra)  #������ ������ �ٲٴ� ��
cM.tra <- confusionMatrix(t(cTab), positive = "Disease")
Accu.tra <- cM.tra$overall["Accuracy"]  #str(cM.tra)�� �غ��� overall�� Accuracy�� ����
cTab
Accu.tra *100

#######Val ���߷�#####
Predict.val <- predict(e_DT, D.val, type="class") #type=class�� �з�
Actual.val <- D.val$Class
cTab <- table(Actual.val, Predict.val)  #������ ������ �ٲٴ� ��
cM.val <- confusionMatrix(t(cTab), positive = "Disease")
Accu.val <- cM.val$overall["Accuracy"]  #str(cM.tra)�� �غ��� overall�� Accuracy�� ����
cTab
Accu.val *100 #���߷�: 68.3333


#########overfitting�� ���ִ� ��##########
printcp(e_DT)  #178�� �� 82�� ������ #����ġ�⿡ ������ ��
cpval <- e_DT$cptable[which.min(e_DT$cptable[,"xerror"]), "CP"]
cpval

pround.e_DT <- prune(e_DT, cp=cpval)
plot(pround.e_DT, margin = 0.2)
text(pround.e_DT, margin = 0.2)

##################pround.e_DT ����#######
#####Tra ���߷�######
Predict.tra <- predict(pround.e_DT, D.tra, type="class") #type=class�� �з�
Actual.tra <- D.tra$Class
cTab <- table(Actual.tra, Predict.tra)  #������ ������ �ٲٴ� ��
cM.tra <- confusionMatrix(t(cTab), positive = "Disease")
Accu.tra <- cM.tra$overall["Accuracy"]  #str(cM.tra)�� �غ��� overall�� Accuracy�� ����
cTab
Accu.tra *100 #���߷�:82.58427

#######Val ���߷�#####
Predict.val <- predict(pround.e_DT, D.val, type="class") #type=class�� �з�
Actual.val <- D.val$Class
cTab <- table(Actual.val, Predict.val)  #������ ������ �ٲٴ� ��
cM.val <- confusionMatrix(t(cTab), positive = "Disease")
Accu.val <- cM.val$overall["Accuracy"]  #str(cM.tra)�� �غ��� overall�� Accuracy�� ����
cTab
Accu.val *100 #���߷�: 71.66667


g_DT <- rpart(Class~., D.tra, method="class",   #method=class�� ȸ��
              parms = list(split="gini"))