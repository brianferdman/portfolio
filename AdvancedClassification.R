summary(bmf24_dividends)

table(bmf24_dividends$dividendPaid)
pie(table(bmf24_dividends$dividendPaid), main="bmf24 dividendPaid")

library(tidyverse)


bmf24_dividends %>%
  pivot_longer(2:6, names_to="attributes") %>%
  ggplot(aes(attributes, value, fill=attributes)) + 
  geom_boxplot() +ggtitle("bmf24 Original Boxplot")

normalize <- function(x) {return ((x - min(x))/(max(x)-min(x)))}

bmf24_normalizedDividends <- as.data.frame(lapply(bmf24_dividends, normalize))
View(bmf24_normalizedDividends)

bmf24_normalizedDividends %>%
  pivot_longer(2:6, names_to="attributes") %>% 
  ggplot(aes(attributes, value, fill=attributes)) + 
  geom_boxplot() +ggtitle("bmf24 Normalized Boxplot")

split <- sample(nrow(bmf24_normalizedDividends),floor(nrow(bmf24_normalizedDividends)*0.7))
bmf24_trainingSet <- bmf24_normalizedDividends[split,]
bmf24_testingSet <- bmf24_normalizedDividends[-split,]

library(neuralnet)

bmf24_nn <- neuralnet(dividendPaid ~ freeCashFlowPerShare + earningsGrowth +
                debtToEquityRatio + marketCapitalization + currentRatio,
                data=bmf24_trainingSet, hidden=c(2,1), linear.output = FALSE, threshold = 0.01)


bmf24_nn$result.matrix

plot(bmf24_nn)


bmf24_testingSubset <- subset(bmf24_testingSet,
                      select = c("freeCashFlowPerShare","earningsGrowth", "debtToEquityRatio", "marketCapitalization", "currentRatio"))

nn.results <- compute(bmf24_nn, bmf24_testingSubset)

#bmf24_results<-data.frame(actual = bmf24_testingSet$dividendPaid, prediction = nn.results$net.result)
#table(prediction,actual)

bmf24_results<-data.frame(actual = bmf24_testingSet$dividendPaid, prediction = nn.results$net.result)
roundedresults<-sapply(bmf24_results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(prediction,actual)

library(caret)
confusionMatrix(table(prediction,actual), positive = "1")

library(e1071)

#Convert the dividendPaid column to a factor
bmf24_normalizedDividends$dividendPaid<-as.factor(bmf24_normalizedDividends$dividendPaid)

bmf24_svmModelTuning.out <-tune (svm , dividendPaid ~., data = bmf24_normalizedDividends, kernel="radial",
                                      ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)), scale=TRUE)


#show the best model
bmf24_svmModel <-bmf24_svmModelTuning.out$best.model
bmf24_svmModel

bmf24_svmPredictions <- predict(bmf24_svmModel,bmf24_normalizedDividends)
confusionMatrix(table(actual=bmf24_dividends$dividendPaid, prediction=bmf24_svmPredictions), positive="1")


