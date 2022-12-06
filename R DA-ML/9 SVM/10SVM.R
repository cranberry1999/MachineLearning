setwd("C:/Users/ebiz/Downloads")
rm(list = ls())

D.tra <- read.csv("Tra1.csv", header = T)
D.val <- read.csv("Val1.csv", header = T)
D.tes <- read.csv("Tes1.csv", header = T)

D.tra <- D.tra[,-c(1)]
D.val <- D.val[,-c(1)]
D.tes <- D.tes[,-c(1)]

#install.packages("e1071")
library(e1071)

# tune
x <- subset(D.tra, select = -c(Class))
y <- D.tra$Class

tSeed <- 12345
set.seed(tSeed)

linear_tune <- tune(svm, train.x = x, train.y = y, 
                    kernel="linear", 
                    ranges = list(cost=c(50,100,150)))   # tune = 어떤값이 가장 괜찮은 parameter값인지 알려줌
# cost는 70이 될수도 있고 하는 사람 마음이다

print(linear_tune) #best parameters: cost 100

modelSVM <- svm(Class~., D.tra, type="C-classification",
                kernel="linear", cost=50, 
                gamma= 0.4, coef0= 2,
                scale = F)  # scale하지 말아라 (속성 변환하는 것) / sigmoid는 고려해야함 

Predict.tra <- predict(modelSVM, D.tra)
cTab <- table(Actual=D.tra$Class, Predict.tra)
cTab
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra  # 96.82152

Predict.val <- predict(modelSVM, D.val)
cTab <- table(Actual=D.val$Class, Predict.val)
cTab
Accu.val <- sum(diag(cTab))/sum(cTab)*100
Accu.val  # 98.54015


#set.seed(tSeed)
#poly_tune <- tune(svm, train.x = x, train.y = y, 
#                  kernel="polynomial",    
#                  ranges = list(cost=c(50,100,150),
#                                coef0=c(0,1,2), 
#                                gamma=seq(0.1, 1, by=0.1), 
#                                degree=c(2,3)))    
#print(poly_tune) parameter가 여러개 들어가서 시간이 오래 걸리므로 과제에 포함하지 않아도 됨 


set.seed(tSeed)
radial_tune <- tune(svm, train.x = x, train.y = y, 
                    kernel="radial", 
                    ranges = list(cost=c(50,100,150), 
                                  gamma=seq(0.1, 1, by=0.1)))
print(radial_tune)  # best parameters: cost 50 , gamma 0.4


set.seed(tSeed)
sigmoid_tune <- tune(svm, train.x = x, train.y = y, 
                     kernel="sigmoid", 
                     ranges = list(cost=c(50,100,150), 
                                   gamma=seq(0.1, 1, by=0.1), coef0=c(0,1,2)))
print(sigmoid_tune)  # best parameters: cost 50 , gamma 0.1, coef0 2





