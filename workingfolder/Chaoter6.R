library(tidyverse)
library(data.table)
## Splitting sample data for two purposes: Training and Testing ( or validating ). 
# K-fold Cross-Validation: Splitting sample data into a number ( typically k=3 ) of equivalent partitions. 
#     Select two for training of models and the third for testing. Swap the the partitions for different purposes
#     and making sure there are no overlaps and train the model using the different sets k-times. Using 
#     test evaluation scoring routines and baselines standard models ( NULL-models which is usually population means ) and 
#     standard bearers like single-variable models ( models based on specific single variable ) and other tools to 
#     obtain metrics to evaluate models quality. 
#
# 

# Classifier Model Example based on Logistic Regression - Spam/Non-Spam.
spamD <- read.table("Spambase/spamD.tsv", header=TRUE, sep='\t')
summary(spamD)
str(spamD)
View(spamD)
spamD_DT <- spamD %>% setDT() 
spamTrain <- spamD_DT[rgroup >= 10, ]
spamTest <- spamD_DT[rgroup < 10, ]
spamVars <- setdiff(colnames(spamD_DT), list("rgroup", "spam"))
spamFormula <- as.formula(paste('spam =="spam"', paste(spamVars, collapse=' + '), sep = ' ~ '))
spamModel <- glm(spamFormula, family = binomial(link = 'logit'), data=spamTrain)

spamTrain[, pred := predict(spamModel, newdata = spamTrain, type = 'response')]
spamTest[, pred := predict(spamModel, newdata = spamTest, type = 'response')]

confmat_spam <- table(truth = spamTest$spam, prediction = ifelse(spamTest$pred > 0.5, "spam", "non-spam"))
print(confmat_spam)
confmat_spam2 <- table(prediction = ifelse(spamTest$pred > 0.5, "spam", "non-spam"), truth = spamTest$spam)

# Confusion matrix here will indicate two dimensional matchup of Prediction v. Already Known Fact. 
# 50% probability threshold will be deemed as positive prediction ( spam marker ). The 50% threshold based on 
# the confusion matrix shows reasonably good accuracy. Accuracy, however, isnt always the end goal, 
# and especially for training data, 50% wont be good enough. 
# ACCURACY - accuracy of predictor. For a classifier, accuracy = # of correctly id'd spam / Total#Items
# so ACCR = [TP + TF] / [TP + TF + FP + FF] 
#                 truth
# prediction non-spam spam
# non-spam      264   22
# spam           14  158.
# [ 264 + 158 ] / [ 264 + 22 + 14 + 158] = 
# 
print(confmat_spam) 

sset.seed(234641)
N <- nrow(spamTest)
pull_out_ix <- sample.int(N, 100, replace = FALSE)
removed <- spamTest[pull_out_ix, ]
get_performance <- function(stest) {
  proportion <- stest[, mean(spam == "spam")]
  confmat_spam <- table(truth =stest[, spam], prediction = stest[, ifelse(pred > 0.5, "spam", "non-spam")])
  precision <- confmat_spam[2,2] / sum(confmat_spam[, 2])
  recall <- confmat_spam[2,2] / sum(confmat_spam[2, ])
  specificity <- confmat_spam[1, 1] / sum(confmat_spam[1, ])
  sensitivity <- confmat_spam[2, 1] / sum(confmat_spam[2, ])
  list(spam_proportion = proportion, confmat_spam = confmat_spam, precision = precision
       , recall = recall, specificity = specificity, sensitivity = sensitivity)
}

sTest <- spamTest[-pull_out_ix, ]
nrow(sTest)
get_performance(sTest)
get_performance(rbind(sTest, removed[spam=="spam",]))
get_performance(rbind(sTest, removed[spam=="non-spam"]))

#Cricket Chirps
crickets <- read.csv("cricketchirps/crickets.csv") %>% setDT()
cricketsLRModels <- crickets %>% lm(temperatureF ~ chirp_rate, data = .)
crickets$predTempF <- predict(cricketsLRModels, newdata = crickets)

controlTable <- wrapr::build_frame(
  "measurements", "temperature" |
    "actual", "temperatureF" |
    "predicted", "predTempF"
)
# tHERE was no need for this transformation... Was imagining ribbn chart between actual and prediccted temp. 
cricketsBlocks <- rowrecs_to_blocks(crickets, controlTable = controlTable, columnsToCopy = "chirp_rate") %>% setDT()
ggplot(data=cricketsBlocks[measurements=="actual", .(chirp_rate, temperature)]) + 
  geom_point(aes(x=chirp_rate, y=temperature)) + 
  stat_smooth(formula="y ~x", method="lm", aes(x=chirp_rate, y=temperature))

RMSE <- crickets[, .( (mean((temperatureF - predTempF)^2))^0.5 )]   #3.564149
total_delta_Square <- crickets[, sum( (mean(temperatureF) - temperatureF) ^ 2)]
total_err_Square <- crickets[, sum( (predTempF - temperatureF) ^ 2)]
R2 <- 1 - (total_err_Square / total_delta_Square)


library(WVPlots)
DoubleDensityPlot(spamTest, xvar="pred", truthVar = "spam", title="Dist. of scores for spam filters")
spamTest[, .N, by=c("spam", "pred")][spam=="non-spam", .SD][order(-rank(N))]
spamTest[, .N, by=c("spam", "pred")][, .SD][order(-rank(N))]
spamTest[, .N, by=c("spam", "pred")][spam=="non-spam", .SD][pred==min(pred), ]
                      

## Log likelihood. 
# the function assumes no negative probability. 
yTimesLogPy <- function(y, py) {
  logpy <- ifelse(py > 0, log(py), 0)
  y * logpy
}
spamTest %>% mapply(yTimesLogPy, (spam=='spam'), pred)

y <- spamTest$spam == "spam"
yTimesLogPy(spamTest[, 1*(spam=="spam")], spamTest[, pred])
yTimesLogPy(1-spamTest[, 1*(spam=="spam")], spamTest[, 1-pred])

logLikelihood1 <- spamTest %>% .[, yTimesLogPy(1*(spam=="spam"), pred) + 
                 yTimesLogPy(1-1*(spam=="spam"), 1-pred)] %>% sum()

logLikelihood2 <- sum(yTimesLogPy(spamTest[, 1*(spam=="spam")], spamTest[, pred]) + 
yTimesLogPy(1-spamTest[, 1*(spam=="spam")], spamTest[, 1-pred]))


#Normalizing logLikelihood to compare different dataset. One way to normalize is to 
# take the ratio of loglikelihood of the dataset with its NULL-likelihood. 
# The best single observable estimate of the probability of being spam is the observed
# rate of spam on Training dataset. 
pNull <- spamTrain[, mean(spam=='spam')]
deviance <- spamTest %>% .[, calcDeviance(pred, spam=='spam')]
nullDeviance <- spamTest %>% .[, calcDeviance(pNull, spam=='spam')]
#now for the ratio, psenduo-R-Squared... It is a normalized value, and generally 
# a larger value ( < 1 ) is favorable. In principal this value from different datasets 
# or models could be compared as scores to be compared to check which is better. 
pseudoR2 <- 1 - ( deviance / nullDeviance ) 

# A variant of deviance is AIC ( Akaike Infor Criterion ).
#  it adds 2 * (# of Parameters) to deviance value, penalizing a model for higher 
#  number of parameters used. More parameter used to add complexity to the model, 
#  more likely it is to overfit.  
# Pseudo R-Squared and AIC evaluate models on their overall rates of returning
# correct/incorrect predictions on test dataset...

## Another model evaluation tool that could explain its prediction sets is 
# LIME - Local Interpretable Model-Agnostic Explanation. 
# Single Decision-Tree, Human experts on domain knowledge could provide explanation for their
# prediction results by easily tracing back the logic behind the prediction but other more 
# complex and highly interconnected modeling methods will be far more difficult for humans
# to evaluate. One should be able to determine the end results of model predictions are 
# based on learning of actual intended "features" of data, and not just observed 
# idiosyncracies. eg ( analysis of documents to determine its topic 
# ( Christian Study or Atheism ). The analysis could be based on authors name, counts of 
# used words, minor but obvious factors as such and not really on its contents or subject 
# analysis. Some error could be algorithm based, and other more-likely errors are based on
# training data used, on which the algorithm could latch on and produced biased tendencies.
# Techniques, including LIME, exist that could potentially aid in identifying such issues 
# present in models behaviors. LIME produces exlanation on model prediction on specific 
# datum by determining which features of that datum contributed the most to the model's 
# decision. 

irisDT <- iris %>% setDT()
irisDT[, unique(Species)]
irisDT[, class:=(1*(Species=='setosa'))]

set.seed(2345)
intrain <- runif(nrow(irisDT)) < 0.75
train <- irisDT[intrain, ]
test <- irisDT[!intrain, ]
test2 <- irisDT[(1-intrain*1), ]

source("LIME_iris/lime_iris_example.R")
input <- train[, 1:4] %>% as.matrix()
model <- fit_iris_example(input, train$class)
prediction <- predict(model, newdata=as.matrix(test[, 1:4]))
teframe <- data.frame(isSetosa = ifelse(test$class == 1, "setosa", "not setosa"), 
                      pred = ifelse(prediction > 0.5, "setosa", "not setosa"))

table(truth=teframe$isSetosa, prediction=teframe$pred)

library(lime)
explainer <- lime(train[, 1:4], model = model, bin_continuous=TRUE, n_bins = 10)
example <- test[5, 1:4, drop=FALSE]
test[5, class]  # 1 - A Setosa...
round(predict(model, newdata = as.matrix(example))) # 1 - Predicted A Setosa...
explanation <- lime::explain(example, explainer, n_labels = 1, n_features = 4)
plot_features(explanation)

example2 <- test[c(13, 24), 1:4]
test$class[c(13, 24)] # Both are non-Sepals...
round(predict(model, newdata = as.matrix(example2))) # 0 - Predicted Non-Setosas
explanation2 <- lime::explain(example2, explainer, 
                              n_labels = 1, n_features = 4, 
                              kernel_width=0.5 )

library(cdata)
control_table <- wrapr::build_frame(
    "parts", "length", "width" |
    "petal", "Petal.Length", "Petal.Width" |
    "sepal", "Sepal.Length", "Sepal.Width" 
) 
transform <- cdata::rowrecs_to_blocks_spec(
  controlTable = control_table, 
  recordKeys=c("Species", "iris_id", "class")
)

irisDT_blocks <- irisDT %.>% transform %>% setDT()
irisDT_blocks[, dotLabel:=ifelse(iris_id %in% c(58, 30, 110), as.character(iris_id), '')]
ggplot(data=irisDT_blocks, aes(x=length, y=width, shape=Species, color=Species)) + 
    geom_point() +
    geom_point(data=irisDT_blocks[(iris_id %in% c(58, 30, 110)), .SD], 
               aes(size=4, label=iris_id), show.legend = FALSE) + 
                geom_text(aes(label=dotLabel), colour="black",hjust=-0.5, vjust=0) + 
    facet_wrap( ~parts, scales = "free" )

geom_point()






# NOT RUN {
x <- 1:9; 
names(x) <- x
# Multiplication & Power Tables
x %o% x
y <- 2:8; 
names(y) <- paste(y,":", sep = "")
max(outer(y, x, "^"))
outer(y, x, "^")-> a
y %o% x

outer(month.abb, 1999:2003, FUN = "paste")

## three way multiplication table:
x %o% x %o% y[1:3]
# }







