library(rpart)
library(caret)
library(rattle)

# the training data
gtd_2019 = read_csv('https://raw.githubusercontent.com/Neilblund/APAN/main/gtd_2019.csv')%>%
  mutate(any_attacks = factor(any_attacks))

# The test data
gtd_2020 = read_csv('https://raw.githubusercontent.com/Neilblund/APAN/main/gtd_2020.csv')%>%
  mutate(any_attacks = factor(any_attacks))

# Codebook here: https://github.com/Neilblund/APAN/blob/main/included_gtd.csv

tune <- data.frame(cp = seq(0, .2, by=.02))
fitControl <- trainControl(method = "LOOCV")
rf_random <- train(any_attacks ~   wdi_unemployment +v2cacamps, # change these
                   data = gtd_2019,
                   method = 'rpart',
                   metric = 'Kappa',
                   tuneGrid = tune,
                   trControl = fitControl)

# plot the final results
fancyRpartPlot(rf_random$finalModel)


# predict the outcome: 
predictions = predict(rf_random, type='raw', newdata=gtd_2020)

# View the metrics on a new sample:
confusionMatrix(data = predictions, reference=gtd_2020$any_attacks)