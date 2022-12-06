
#첫번째 split으로 오늘의 실습을 수행한다 
# 자신이 직접 parameter를 정하고 적어놓는다 

D.data <- read.csv("bCancerWC.csv", header = TRUE)

pSeed <- 01234
set.seed(pSeed)

#install.packages("doBy")
library(doBy)
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

D.tra
D.val
D.tes


D.tra <- D.tra[,-c(1)]
D.val <- D.val[,-c(1)]
D.tes <- D.tes[,-c(1)]



#install.packages("e1071")
library(e1071)

# tune
x <- subset(D.tra, select = -c(Class))
y <- D.tra$Class

pSeed <- 01234
set.seed(pSeed)

linear_tune <- tune(svm, train.x = x, train.y = y, 
                    kernel="linear", 
                    ranges = list(cost=c(50,100,150)))   # tune = 어떤값이 가장 괜찮은 parameter값인지 알려줌
# cost는 70이 될수도 있고 하는 사람 마음이다
print(linear_tune)  # best parameters: cost 50


set.seed(pSeed)
radial_tune <- tune(svm, train.x = x, train.y = y, 
                    kernel="radial", 
                    ranges = list(cost=c(50,100,150), 
                                  gamma=seq(0.1, 1, by=0.1)))
print(radial_tune)  # best parameters: cost 50 , gamma 0.4


set.seed(pSeed)
sigmoid_tune <- tune(svm, train.x = x, train.y = y, 
                     kernel="sigmoid", 
                     ranges = list(cost=c(50,100,150), 
                                   gamma=seq(0.1, 1, by=0.1), coef0=c(0,1,2)))
print(sigmoid_tune)  # best parameters: cost 50 , gamma 0.1, coef0 2




hSeedlist <- c(01234,12346,12347,12348,12349,12350,12351,12352,12353,12354)

for (hSeed in hSeedlist){
  set.seed(hSeed)
  library(doBy)
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
  D.tes <- D.tes[,-c(1)]
  
  
  modelSVM <- svm(Class~., D.tra, type="C-classification",
                  kernel="linear", cost=50, 
                  gamma= 0.4, coef0= 2,
                  scale = F)   
  
  Predict.tra <- predict(modelSVM, D.tra)
  cTab <- table(Actual=D.tra$Class, Predict.tra)
  Accu.tra <- sum(diag(cTab))/sum(cTab)*100
  
  Predict.val <- predict(modelSVM, D.val)
  cTab <- table(Actual=D.val$Class, Predict.val)
  Accu.val <- sum(diag(cTab))/sum(cTab)*100
  
  print(hSeed)
  print(Accu.tra); print(Accu.val)
}

