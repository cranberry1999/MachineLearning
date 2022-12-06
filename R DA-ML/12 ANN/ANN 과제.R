setwd("C:/R DA-ML/2nd/12 ANN")
rm(list = ls())


D.data <- read.csv("bCancerWC.csv", header = TRUE)

library(doBy)
library(e1071)
library(neuralnet)


pSeedlist <- c(12345, 22345,32345,42345,52345,62345,72345,82345,92345,12346)

for (pSeed in pSeedlist) {
  set.seed(pSeed)
  
  n <- nrow(D.data)
  idx <- 1:n
  
  D.tra <- sampleBy(~Class, frac = 0.6, data = D.data)
  D.tra_idx <- D.tra$Record
  idx <- setdiff(idx, D.tra_idx)
  D.val <- D.data[D.data$Record %in% idx,]
  
  D.val <- sampleBy(~Class, frac = 0.5, data = D.val)
  D.val_idx <- D.val$Record
  idx <- setdiff(idx, D.val_idx)
  D.tes <- D.data[D.data$Record %in% idx,]
  
  D.tra <- D.tra[c(order(D.tra$Record)),] 
  D.val <- D.val[c(order(D.val$Record)),] 
  D.tes <- D.tes[c(order(D.tes$Record)),] 
  row.names(D.tra) <- D.tra$Record 
  row.names(D.val) <- D.val$Record 
  row.names(D.tes) <- D.tes$Record
  
  D.tra <- D.tra[,-c(1)]
  D.val <- D.val[,-c(1)]
  
  D.tra$Class <- ifelse(D.tra$Class==2,0,1)
  D.val$Class <- ifelse(D.val$Class==2,0,1)
  
  H42 <- c(4,2) ; Thres <- 0.1 ; Step <-800000; Rate <- 0.01
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
  cat('  Val=', Accu.val)
}



## Test 
## 
for (pSeed in pSeedlist) {
  set.seed(pSeed)
  
  n <- nrow(D.data)
  idx <- 1:n
  
  D.tra <- sampleBy(~Z, frac = 0.6, data = D.data)
  D.tra_idx <- D.tra$Record
  idx <- setdiff(idx, D.tra_idx)
  D.val <- D.data[D.data$Record %in% idx,]
  
  D.val <- sampleBy(~Z, frac = 0.5, data = D.val)
  D.val_idx <- D.val$Record
  idx <- setdiff(idx, D.val_idx)
  D.tes <- D.data[D.data$Record %in% idx,]
  
  D.tra <- D.tra[c(order(D.tra$Record)),] 
  D.val <- D.val[c(order(D.val$Record)),] 
  D.tes <- D.tes[c(order(D.tes$Record)),] 
  row.names(D.tra) <- D.tra$Record 
  row.names(D.val) <- D.val$Record 
  row.names(D.tes) <- D.tes$Record
  
  D.tra <- D.tra[,-c(1)]
  D.tes <- D.tes[,-c(1)]
  
  D.tra$Class <- ifelse(D.tra$Class==2,0,1)
  D.tes$Class <- ifelse(D.tes$Class==2,0,1)
  
  H42 <- c(8,5) ; Thres <- 0.01 ; Step <-1000000; Rate <- 0.01
  BPnn  <- neuralnet(Class~., D.tra, hidden=H42, threshold = Thres,
                     stepmax = Step, learningrate = Rate, 
                     algorithm = "backprop", err.fct = 'ce',
                     act.fct = 'logistic', linear.output = F)
  
  # Test 
  Output.tes <- predict(BPnn, D.tes)
  Predict.tes <- ifelse(Output.tes>=cutoff,1,0)
  cTab <- table(Actual=D.tes$Class, Predict.tes)
  Accu.tes <- round(sum(diag(cTab))/sum(cTab)*100, digit=2)
  
  
  cat(sprintf('\n Thres=%4.2f  Seed=%5d', Thres, pSeed )) 
  cat('  Tes=', Accu.tes)

}
