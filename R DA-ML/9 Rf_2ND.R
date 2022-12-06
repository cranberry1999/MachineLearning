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
library(rpart) #가지치기 할때 이중 하나를 선택

D.tra <- R.tra
D.val <- R.val

set.seed(12345) #똑같은 결과를 내줌
e_DT <- rpart(Class~., D.tra, method="class",   #method=class는 회귀
              parms = list(split="information")) #Class 빼고 모든 데이터 사용
plot(e_DT)
text(e_DT)

e_DT #Thal=bc인 이유는 T6,T7을 의미 T3은 a를 의미.

Predict.tra <- predict(e_DT, D.tra, type="class") #type=class는 분류
Actual.tra <- D.tra$Class
head(Predict.tra)
head(Actual.tra)

#confusionMatrix(분류결과표)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)


#####Tra 적중률######
cM1 <- confusionMatrix(Predict.tra, Actual.tra, positive = "Disease")
cM1 #적중률:0.8708 / 민감도: 0.9024 / 특이도: 0.8438

cTab <- table(Actual.tra, Predict.tra)  #실제와 예측을 바꾸는 것
cTab

Predict.tra <- predict(e_DT, D.tra, type="class") #type=class는 분류
Actual.tra <- D.tra$Class
cTab <- table(Actual.tra, Predict.tra)  #실제와 예측을 바꾸는 것
cM.tra <- confusionMatrix(t(cTab), positive = "Disease")
Accu.tra <- cM.tra$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.tra *100  # 적중률: 87.07865

#######Val 적중률#####
Predict.val <- predict(e_DT, D.val, type="class") #type=class는 분류
Actual.val <- D.val$Class
cTab <- table(Actual.val, Predict.val)  #실제와 예측을 바꾸는 것
cM.val <- confusionMatrix(t(cTab), positive = "Disease")
Accu.val <- cM.val$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.val *100 #적중률: 68.3333

##################pround.e_DT 적용#######
#####Tra 적중률######
Predict.tra <- predict(pround.e_DT, D.tra, type="class") #type=class는 분류
Actual.tra <- D.tra$Class
cTab <- table(Actual.tra, Predict.tra)  #실제와 예측을 바꾸는 것
cM.tra <- confusionMatrix(t(cTab), positive = "Disease")
Accu.tra <- cM.tra$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.tra *100 #적중률:82.58427

#######Val 적중률#####
Predict.val <- predict(pround.e_DT, D.val, type="class") #type=class는 분류
Actual.val <- D.val$Class
cTab <- table(Actual.val, Predict.val)  #실제와 예측을 바꾸는 것
cM.val <- confusionMatrix(t(cTab), positive = "Disease")
Accu.val <- cM.val$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.val *100 #적중률: 71.66667  ##### 편차: 10.91


######---------------------[Random Forest]--------------------------

#install.packages("randomForest")
library(randomForest)  # 중요파라미터 : mtry, ntree(몇 개 만들어라, 다수결의 원칙을 따르므로 홀수개로 주는게 좋다)
head(R.tra)

x <- subset(R.tra, select = -c(Class))
y <- R.tra$Class

mtrySeed <- 1234
set.seed(mtrySeed)

bestmtry <- tuneRF(x, y, stepFactor = 1.5, improve = 0.01, ntreeTry = 49)
print(bestmtry) # mrty를 3으로 시작하고 1.5씩 나눠가며 에러 구하다가 improve못넘으면 3*15해서 또 같은 과정 반복..또 다음에 3*1.5*1.5..

nTree <- 49 # 보통 200, 300개 
mTry <- 4 # 앞에서 나온 값
rfSeed <-12345
set.seed(rfSeed)

rfModel <- randomForest(x, y, ntree = nTree, mtry = mTry)  # 분류인지 회귀인지 자동으로 지정하기 때문에 알려줄 필요 없다 

########Tra############
Predict.tra <- predict(rfModel, R.tra)
cTab <- table(Actual=R.tra$Class, Predict.tra)
cTab
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra

########val############
head(R.tra) ; head(R.val)
levels(R.val$CP) <- levels(R.tra$CP) # Bootstrap으로 인해 속성값에 차이가 생겨서 같게 해줘야함
levels(R.val$Restecg) <- levels(R.tra$Restecg)
levels(R.val$Slope) <- levels(R.tra$Slope)
levels(R.val$Thal) <- levels(R.tra$Thal)

Predict.val <- predict(rfModel, R.val)
cTab <- table(Actual=R.val$Class, Predict.val)
Accu.val <- sum(diag(cTab))/sum(cTab)*100

variance <- abs(Accu.tra-Accu.val)

Accu.tra
Accu.val
variance  # 오버피팅 제거 필요

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

nTree; mTry; maxLeaf  # 이 파라미터들을 바꿔가며 최고성능의 rf를 찾는다 
Accu.tra
Accu.val
variance
