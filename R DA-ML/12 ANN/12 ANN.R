#stepmax 학습횟수
#threshold (오차의 감소치가 얼마일 떄 종료하나_)
#learningrate 학습률
#err.fct  sse=regression
#linear.output: 분류문제는 F

-----------
setwd("C:/Users/ebiz/Downloads12 ANN")
rm(list = ls())

D.tra <- read.csv("Tra1.csv")
D.val <- read.csv("Val1.csv")
D.tes <- read.csv("Tes1.csv")

D.tra <- D.tra[,-c(1)]
D.val <- D.val[,-c(1)]
D.tes <- D.tes[,-c(1)]

#install.packages("neuralnet")
library(neuralnet)

wSeed <- 13579
set.seed(wSeed)

D.tra$Class <- ifelse(D.tra$Class==2,0,1)
D.val$Class <- ifelse(D.val$Class==2,0,1)
D.tes$Class <- ifelse(D.tes$Class==2,0,1)


H42 <- c(4,2) ; Thres <- 0.1 ; Step <-100000; Rate <- 0.01

BPnn  <- neuralnet(Class~., D.tra, hidden = H42, threshold = Thres,
                   stepmax = Step, learningrate = Rate, 
                   algorithm = "backprop", err.fct = 'ce',
                   act.fct = 'logistic', linear.output = F)

# 그래프 
plot(BPnn)

# 적중률
cutoff <- 0.5
Output.tra <- predict(BPnn, D.tra)
Predict.tra <- ifelse(Output.tra>=cutoff,1,0)
cTab <- table(Actual=D.tra$Class, Predict.tra)
Accu.tra <- round(sum(diag(cTab))/sum(cTab)*100, digit=2)
Accu.tra # 97.80

Output.val <- predict(BPnn, D.val)
Predict.val <- ifelse(Output.val>=cutoff,1,0)
cTab <- table(Actual=D.val$Class, Predict.val)
Accu.val <- round(sum(diag(cTab))/sum(cTab)*100, digit=2)
Accu.val




#### 과제 


D.data <- read.csv("bCancerWC.csv", header = TRUE)
D.data$Class <- ifelse(D.data$Class==2, 0, 1)

library(doBy)
library(e1071)
#install.packages("neuralnet")
library(neuralnet)


pSeedlist <- c(12345, 2, 3, 4, 5, 6234, 7245, 8345, 9345, 2346)

for (pSeed in pSeedlist) {
  set.seed(pSeed)
  
  Training <- sampleBy(~Class, frac = 0.6, data = D.data)
  idx <- setdiff(D.data$Record, Training$Record)
  Data <- D.data[D.data$Record %in% idx,]
  Validation <- sampleBy(~Class, frac = 0.5, data = D.data)
  idx <- setdiff(D.data$Record, Validation$Record)
  Test <- D.data[D.data$Record %in% idx,]
  
  Training <- Training[c(order(Training$Record)),] 
  Validation <- Validation[c(order(Validation$Record)),] 
  Test <- Test[c(order(Test$Record)),]
  row.names(Training) <- Training$Record 
  row.names(Validation) <- Validation$Record 
  row.names(Test) <- Test$Record 
  
  D.tra <- Training
  D.val <- Validation
  D.tes <- Test
  
  D.tra <- D.tra[,-c(1)]
  D.val <- D.val[,-c(1)]
  D.tes <- D.tes[,-c(1)]
  
  D.tra$Class <- ifelse(D.tra$Class==2,0,1)
  D.val$Class <- ifelse(D.val$Class==2,0,1)
  D.tes$Class <- ifelse(D.tes$Class==2,0,1)
  
  
  set.seed(wSeed)
  H42 <- c(4,2) ; Thres <- 0.01 ; Step <-100000; Rate <- 0.01
  BPnn  <- neuralnet(Class~., D.tra, hidden=H42, threshold = Thres,
                     stepmax = Step, learningrate = Rate, 
                     algorithm = "backprop", err.fct = 'ce',
                     act.fct = 'logistic', linear.output = F)
  
  # Training
  cutoff <- 0.5
  Output.tra <- predict(BPnn, D.tra)
  Predict.tra <- ifelse(Output.tra>=cutoff,1,0)
  cTab <- table(Actual=D.tra$Class, Predict.tra)
  Accu.tra <- round(sum(diag(cTab))/sum(cTab)*100, digit=2)

  # Validation
  Output.val <- predict(BPnn, D.val)
  Predict.val <- ifelse(Output.val>=cutoff,1,0)
  cTab <- table(Actual=D.val$Class, Predict.val)
  Accu.val <- round(sum(diag(cTab))/sum(cTab)*100, digit=2)

  
  cat(sprintf('\n Thres=%4.2f  Seed=%5d', Thres, pSeed )) 
  cat('  Tra=', Accu.tra)
  cat('  Val=', Accu.val) } 
