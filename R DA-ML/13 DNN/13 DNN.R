library(tensorflow)
library(keras)

Tra <- read.csv("Tra1.csv", header = TRUE)
Val <- read.csv("Val1.csv", header = TRUE)
Tes <- read.csv("Tes1.csv", header = TRUE)

Tra$Cancer <- ifelse(Tra$Cancer==2, 0, 1)
Val$Cancer <- ifelse(Val$Cancer==2, 0, 1)
Tes$Cancer <- ifelse(Tes$Cancer==2, 0, 1)

x_Tra <- as.matrix(subset(Tra, select = -c(Record,Cancer)))
y_Tra <- as.matrix(Tra$Cancer)
x_Val <- as.matrix(subset(Val, select = -c(Record,Cancer)))
y_Val <- as.matrix(Val$Cancer)
x_Tes <- as.matrix(subset(Tes, select = -c(Record,Cancer)))
y_Tes <- as.matrix(Tes$Cancer)

wSeed <- 13579
tf$random$set_seed(wSeed)

model <- keras_model_sequential() %>% 
  layer_dense(units=20, input_shape = c(9, NULL)) %>%
  layer_dense(units = 30, activation = "relu") %>%
  layer_dense(units = 15, activation = "relu") %>%
  layer_dense(units = 7, activation = 'relu' ) %>%
  layer_dense(units = 1, activation = 'sigmoid')

lRate <- 0.1
model %>% compile(optimizer=optimizer_sgd(lr=lRate), 
                  loss='binary_crossentropy',
                  metrics=c('accuracy'))


for (i in 1:10) {
  
nEpochs <- 50
model %>% fit(x_Tra, y_Tra, 
              epochs = nEpochs, verbose=0)  # It is not usable figure because it is not a table

accTra <- model %>% evaluate(x_Tra, y_Tra, verbose=0)
accVal <- model %>% evaluate(x_Val, y_Val, verbose=0)
Accu.tra <- round(accTra$accuracy*100, digits = 2)
Accu.val <- round(accVal$accuracy*100, digits = 2)

Upto <- nEpochs*i
cat(sprintf('\n Epoch : %5d   Train : %6.2f%%  Valid : %6.2f%%' ,
            Upto, Accu.tra, Accu.val))
}




###### assignment #######

D.data <- read.csv("bCancerWC.csv", header = TRUE)

#install.packages("doBy")
#install.packages("e1071")
library(doBy)
library(e1071)


pSeedlist <- c(12345, 22345,32345,42345,52345,62345,72345,82345,92345,12346)

for (pSeed in pSeedlist) {
  tf$random$set_seed(pSeed)
  
  n <- nrow(D.data)
  idx <- 1:n
  
  D.tra <- sampleBy(~Cancer, frac = 0.6, data = D.data)
  D.tra_idx <- D.tra$Record
  idx <- setdiff(idx, D.tra_idx)
  D.val <- D.data[D.data$Record %in% idx,]
  
  D.val <- sampleBy(~Cancer, frac = 0.5, data = D.val)
  D.val_idx <- D.val$Record
  idx <- setdiff(idx, D.val_idx)
  D.tes <- D.data[D.data$Record %in% idx,]
  
  D.tra <- D.tra[c(order(D.tra$Record)),] 
  D.val <- D.val[c(order(D.val$Record)),] 
  D.tes <- D.tes[c(order(D.tes$Record)),] 
  row.names(D.tra) <- D.tra$Record 
  row.names(D.val) <- D.val$Record 
  row.names(D.tes) <- D.tes$Record

  D.tra$Cancer <- ifelse(D.tra$Cancer==2,0,1)
  D.val$Cancer <- ifelse(D.val$Cancer==2,0,1)
  
  x_Tra <- as.matrix(subset(D.tra, select = -c(Record,Cancer)))
  y_Tra <- as.matrix(D.tra$Cancer)
  x_Val <- as.matrix(subset(D.val, select = -c(Record,Cancer)))
  y_Val <- as.matrix(D.val$Cancer)
  
  # cat('  Seed=', pSeed)
  
# modeling 
  
  
  model <- keras_model_sequential() %>% 
    layer_dense(units=20, input_shape = c(9, NULL)) %>%
    layer_dense(units = 15, activation = "relu") %>%
    layer_dense(units = 7, activation = 'relu' ) %>%
    layer_dense(units = 1, activation = 'sigmoid')
  
  lRate <- 0.1
  model %>% compile(optimizer=optimizer_sgd(lr=lRate), 
                    loss='binary_crossentropy',
                    metrics=c('accuracy'))
  
  
  for (i in 1:10) {
    
    nEpochs <- 50
    model %>% fit(x_Tra, y_Tra, 
                  epochs = nEpochs, verbose=0)  # It is not usable figure because it is not a table
    
    accTra <- model %>% evaluate(x_Tra, y_Tra, verbose=0)
    accVal <- model %>% evaluate(x_Val, y_Val, verbose=0)
    Accu.tra <- round(accTra$accuracy*100, digits = 2)
    Accu.val <- round(accVal$accuracy*100, digits = 2)
    
    Upto <- nEpochs*i
    cat(sprintf('\n Seed=%5d   Epoch : %5d   Train : %6.2f%%  Valid : %6.2f%%' ,
                 pSeed, Upto, Accu.tra, Accu.val))
  }
}



accTes <- model %>% evaluate(x_Tes, y_Tes, verbose=0)
Accu.tes <- round(accTes$accuracy*100, digits = 2)

cat(sprintf('\n Seed=%5d   Epoch : %5d   Test : %6.2f%%'  ,
            pSeed, Upto, Accu.tes))  


