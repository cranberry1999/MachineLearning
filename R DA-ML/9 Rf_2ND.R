setwd("C:/Users/ebiz/Downloads")
rm(list = ls())

Tra <- read.csv("Tra1.csv", header = TRUE)
Val <- read.csv("Val1.csv", header = TRUE)
Tes <- read.csv("Tes1.csv", header = TRUE)


R.tra <- Tra[, c(-1)]
R.val <- Val[, c(-1)]
R.tes <- Tes[, c(-1)]



#####----------------------------------[DT]------------------------------------------
#install.packages("rpart")
library(rpart) #����ġ�� �Ҷ� ���� �ϳ��� ����

D.tra <- R.tra
D.val <- R.val

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
Accu.tra *100  # ���߷�: 87.07865

#######Val ���߷�#####
Predict.val <- predict(e_DT, D.val, type="class") #type=class�� �з�
Actual.val <- D.val$Class
cTab <- table(Actual.val, Predict.val)  #������ ������ �ٲٴ� ��
cM.val <- confusionMatrix(t(cTab), positive = "Disease")
Accu.val <- cM.val$overall["Accuracy"]  #str(cM.tra)�� �غ��� overall�� Accuracy�� ����
cTab
Accu.val *100 #���߷�: 68.3333

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
Accu.val *100 #���߷�: 71.66667  ##### ����: 10.91


######---------------------[Random Forest]--------------------------

#install.packages("randomForest")
library(randomForest)  # �߿��Ķ���� : mtry, ntree(�� �� ������, �ټ����� ��Ģ�� �����Ƿ� Ȧ������ �ִ°� ����)
head(R.tra)

x <- subset(R.tra, select = -c(Class))
y <- R.tra$Class

mtrySeed <- 1234
set.seed(mtrySeed)

bestmtry <- tuneRF(x, y, stepFactor = 1.5, improve = 0.01, ntreeTry = 49)
print(bestmtry) # mrty�� 3���� �����ϰ� 1.5�� �������� ���� ���ϴٰ� improve�������� 3*15�ؼ� �� ���� ���� �ݺ�..�� ������ 3*1.5*1.5..

nTree <- 49 # ���� 200, 300�� 
mTry <- 4 # �տ��� ���� ��
rfSeed <-12345
set.seed(rfSeed)

rfModel <- randomForest(x, y, ntree = nTree, mtry = mTry)  # �з����� ȸ������ �ڵ����� �����ϱ� ������ �˷��� �ʿ� ���� 

########Tra############
Predict.tra <- predict(rfModel, R.tra)
cTab <- table(Actual=R.tra$Class, Predict.tra)
cTab
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra

########val############
head(R.tra) ; head(R.val)
levels(R.val$CP) <- levels(R.tra$CP) # Bootstrap���� ���� �Ӽ����� ���̰� ���ܼ� ���� �������
levels(R.val$Restecg) <- levels(R.tra$Restecg)
levels(R.val$Slope) <- levels(R.tra$Slope)
levels(R.val$Thal) <- levels(R.tra$Thal)

Predict.val <- predict(rfModel, R.val)
cTab <- table(Actual=R.val$Class, Predict.val)
Accu.val <- sum(diag(cTab))/sum(cTab)*100

variance <- abs(Accu.tra-Accu.val)

Accu.tra
Accu.val
variance  # �������� ���� �ʿ�

#########overfitting
maxLeaf <- 10
rfModel <- randomForest(x, y, ntree = nTree, mtry = mTry, maxnodes = maxLeaf)

Predict.tra <- predict(rfModel, R.tra)
cTab <- table(Actual=R.tra$Class, Predict.tra)
Accu.tra <- sum(diag(cTab))/sum(cTab)*100

Predict.val <- predict(rfModel, R.val)
cTab <- table(Actual=R.val$Class, Predict.val)
Accu.val <- sum(diag(cTab))/sum(cTab)*100

variance <- abs(Accu.tra-Accu.val)

nTree; mTry; maxLeaf  # �� �Ķ���͵��� �ٲ㰡�� �ְ������� rf�� ã�´� 
Accu.tra
Accu.val
variance