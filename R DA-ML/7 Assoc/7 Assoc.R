setwd("C:/R DA-ML/7 Assoc")
rm(list=ls())

install.packages('arules')
library(arules)

A.data <- read.transactions("POS2020.csv", sep=",", 
                            format= 'basket', skip=1, cols=1)
A.data
summary(A.data)

inspect(A.data)
inspect(A.data[1:5])

itemFrequencyPlot(A.data)
itemFrequencyPlot(A.data, support=0.3)
itemFrequencyPlot(A.data, topN=10)

apriori(A.data)
apriori(A.data, parameter = list(support=0.2))
apriori(A.data, parameter = list(confidence=0.9))
apriori(A.data, parameter = list(support=0.2, confidence=0.5))

rule20 <- apriori(A.data, parameter = list(support=0.2, confidence=0.5))
inspect(rule20)
inspect(sort(rule20, by='lift'))
inspect(sort(rule20, by='lift')[1:5])
inspect(sort(rule20,decreasing = F, by='lift')[1:5])

sodaR <- subset(rule20, items%in% 'soda')
inspect(sodaR)
sodaRc9 <- subset(rule20, items %in% 'soda' & confidence>0.9)
inspect(sodaRc9)

scR <- subset(rule20, items%in% c('soda', 'cracker'))
inspect(scR)

scR <- subset(rule20, items%ain% c('soda', 'cracker'))
inspect(scR)


thanbeerR <- subset(rule20, rhs %in% c('beer'))
inspect(beerR)

ifwskR <- subset(rule20, lhs %in% c('whiskey'))
inspect(wskR)

#부분적으로 들어가는 것
erR <- subset(rule20, items%pin% 'er')
inspect(erR)
