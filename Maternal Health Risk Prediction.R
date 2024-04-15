## TOPIC - MATERNAL HEALTH RISK PREDICTION ####

#This project is aimed to help health care providers in rural Bangladesh by providing
# a simple code using R

# This project consists of Data import, Exploratory Data Analysis, Modeling & Prediction

# Install general required packages and libraries -----
# Please note that these libraries are required for visualization
install.packages(c("dplyr", "tidyr", "ggplot2", "RColorBrewer","plotly"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)
# Data Import -----

# load data
data <- read.csv("Maternal Health Risk Data Set.csv")

# view data
View(data)

# data structure
str(data)

# data dimensions
dim(data)

# Exploratory Data Analysis -----

# view head of the data
head(data)

# data summary
summary(data)
# the data summary gives us an insight into the numerical columns
# we can immediately notice an error in the data
# the minimum in the heart rate column is 7 which is not possible
# On an average, the human heart beats at a rate of 69-90 bpm

# how many records are wrong?
subset(data, 
       subset = data$HeartRate == min(data$HeartRate))

# remove wrong data
data <- data[-c(500, 909), ]

# review dimensions
dim(data)

# Visualize and understand each variable(except risk level) =====

# Variable 1 - Age
agePlot <- ggplot(data = data, aes(x = Age)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Histogram showing distribution of Age",
       x = "Age",
       y = "Frequency") +
  theme_bw()
agePlot

# There are some observations below 20 years of age
# count of pregnancies under 20 years
print(paste("The number of pregnancies under 20 years of age are: ",count(subset(data, subset = data$Age < 20))))

# Variable 2 - SystolicBP
systolicPlot <- ggplot(data = data, aes(x = SystolicBP)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Histogram showing distribution of Systolic BP",
       x = "Systolic BP",
       y = "Frequency") + 
  theme_bw()
systolicPlot

# systolic bp between 110-139 can be considered normal during pregnancy
# count patients with high & low Systolic BP
print(paste("The number of patients who have Systolic BP higher than normal: ", count(subset(data, subset = data$SystolicBP > 139))))
print(paste("The number of patients who have Systolic BP lower than normal: ", count(subset(data, subset = data$SystolicBP < 110))))

# Variable 3 - Diastolic BP
diastolicPlot <- ggplot(data = data, aes(x = DiastolicBP)) +
  geom_histogram(binwidth = 10)+
  labs(title = "Histogram showing distribution of Diastolic BP",
       x = "Diastolic BP",
       y = "Frequency") + 
  theme_bw()
diastolicPlot

# Diastolic BP between 70-89 can be considered normal during pregnancy
# count high & low diastolic BP patients
print(paste("The number of patients who have Diastolic BP higher than normal: ", count(subset(data, subset = data$DiastolicBP > 89))))
print(paste("The number of patients who have Diastolic BP lower than normal: ", count(subset(data, subset = data$DiastolicBP < 70))))

# Variable 4 - Blood Sugar
blSugarPlot <- ggplot(data = data, aes(x = BS)) +
  geom_histogram(binwidth = 5)+
  labs(title = "Histogram showing distribution of Blood Sugar",
       x = "Blood Sugar",
       y = "Frequency") + 
  theme_bw()
blSugarPlot

# The normal blood sugar value for pregnant women is 7.8 mmol/L
# count patients with high and low blood sugar level
print(paste("The number of patients with blood sugar above normal: ", count(subset(data, subset = data$BS > 7.8))))
print(paste("The number of patients with blood sugar lower than normal: ", count(subset(data, subset = data$BS < 7.8))))

# Variable 5 - Body Temperature
bodyTempPlot <- ggplot(data = data, aes(x = BodyTemp)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram showing distribution of Body Temperature",
       x = "Body Temperature",
       y = "Frequency") + 
  theme_bw()
bodyTempPlot

# According to a study, temperature usually peaks around the 12th week (96-99.5F) and drops after the 33rd week (95.5 - 99.1F)
# For this study, we can consider between 95.5-99.5F to be normal because we do not know the patient's pregnancy stage
# Count of patients with above or below normal body temperature
print(paste("The number of patients with above normal body temp: ", count(subset(data, subset = data$BodyTemp > 99.5))))
print(paste("The number of patients with below normal body temp: ", count(subset(data, subset = data$BodyTemp < 95.5))))

# Variable 6 <- Heart Rate
heartRatePlot <- ggplot(data = data, aes(x = HeartRate)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram showing distribution of Heart Rate",
       x = "Heart Rate",
       y = "Frequency") + 
  theme_bw()
heartRatePlot

# Heartbeat during a normal period of pregnancy ranges from 69 beats/min lowest to 90 beats/min highest
# For our study, we can assume anything between 69-90 as normal
print(paste("Number of patients with heart rate above normal: ", count(subset(data, subset = data$HeartRate > 90))))
print(paste("Number of patients with heart rate below normal: ", count(subset(data, subset = data$HeartRate < 69))))

# Count the number of patients in each risk category =====
riskCat <- ggplot(data = data, aes(x = RiskLevel)) + 
  geom_bar() +
  labs(title = "Count of Observations for `Risk Level`", 
       x = "Risk Level", 
       y = "Count of observations") + 
  theme_bw()
riskCat

# Boxplots =====

# install packages and load libraries
install.packages("gridExtra")
library(gridExtra)

plot1 <- ggplot(data = data, mapping = aes(x = RiskLevel, y = Age)) +
  geom_jitter(alpha = 0.3, color = "royalblue1") +
  geom_boxplot() +
  theme_bw()

plot2 <-ggplot(data = data, mapping = aes(x = RiskLevel, y = SystolicBP)) +
  geom_jitter(alpha = 0.3, color = "salmon") +
  geom_boxplot() +
  theme_bw()

plot3 <- ggplot(data = data, mapping = aes(x = RiskLevel, y = DiastolicBP)) +
  geom_jitter(alpha = 0.3, color = "salmon") +
  geom_boxplot() +
  theme_bw()

plot4 <- ggplot(data = data, mapping = aes(x = RiskLevel, y = BS)) +
  geom_jitter(alpha = 0.3, color = "mediumorchid") +
  geom_boxplot() +
  theme_bw()

plot5 <- ggplot(data = data, mapping = aes(x = RiskLevel, y = BodyTemp)) +
  geom_jitter(alpha = 0.3, color = "orangered") +
  geom_boxplot() +
  theme_bw()

plot6 <- ggplot(data = data, mapping = aes(x = RiskLevel, y = HeartRate)) +
  geom_jitter(alpha = 0.3, color = "seagreen1") +
  geom_boxplot() +
  theme_bw()

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)

# Scatterplots =====
ggplot(data = data, aes(x = Age, y = SystolicBP, col = factor(RiskLevel))) + 
  geom_point() +
  labs(title = "Age versus Systolic BP",
       x = "Age",
       y = "Systolic BP") +
  scale_color_manual(values = c("darkred", "forestgreen", "lightgoldenrod1")) +
  guides(color = guide_legend(title = "Risk Level")) + 
  theme_bw()
  
ggplot(data = data, aes(x = Age, y = DiastolicBP, col = factor(RiskLevel))) + 
  geom_point() +
  labs(title = "Age versus Diastolic BP",
       x = "Age",
       y = "Diastolic BP") +
  scale_color_manual(values = c("darkred", "forestgreen", "lightgoldenrod1")) +
  guides(color = guide_legend(title = "Risk Level")) + 
  theme_bw()

ggplot(data = data, aes(x = Age, y = BS, col = factor(RiskLevel))) + 
  geom_point() +
  labs(title = "Age versus Blood Sugar",
       x = "Age",
       y = "Blood Sugar") +
  scale_color_manual(values = c("darkred", "forestgreen", "lightgoldenrod1")) +
  guides(color = guide_legend(title = "Risk Level")) + 
  theme_bw()

ggplot(data = data, aes(x = Age, y = BodyTemp, col = factor(RiskLevel))) + 
  geom_point() +
  labs(title = "Age versus Body Temperature",
       x = "Age",
       y = "Body Temperature") +
  scale_color_manual(values = c("darkred", "forestgreen", "lightgoldenrod1")) +
  guides(color = guide_legend(title = "Risk Level")) + 
  theme_bw()

ggplot(data = data, aes(x = Age, y = HeartRate, col = factor(RiskLevel))) + 
  geom_point() +
  labs(title = "Age versus Heart Rate",
       x = "Age",
       y = "Heart Rate") +
  scale_color_manual(values = c("darkred", "forestgreen", "lightgoldenrod1")) +
  guides(color = guide_legend(title = "Risk Level")) + 
  theme_bw()

# Correlation Matrix =====
install.packages("corrplot")
library(corrplot)

str(data)
df <- data[,c(1:6)]
df$Age <- as.numeric(df$Age)
df$SystolicBP <- as.numeric(df$SystolicBP)
df$DiastolicBP <- as.numeric(df$DiastolicBP)
df$HeartRate <- as.numeric(df$HeartRate)
str(df)
data_cor <- cor(df)
corrplot(data_cor)

# Data Modeling -----

# This section focuses on building models with our data
# These models will be later used for prediction

# The first model is a multinomial regression model
# Since the target/dependent variable is categorical and has multiple levels (high risk, low risk & mid risk) - we go with this type of regression

# Multinomial Regression Model =====

# install packages and load libraries
install.packages(c("MASS","caret","Hmisc", "tidymodels"))
install.packages("cli")
library(cli)
library(caret) 
library(Hmisc)
library(MASS)


# set seed
set.seed(1234)

# convert risk level to type factor with levels
data$RiskLevel <- as.factor(data$RiskLevel)
levels(data$RiskLevel)

# split data into train and test (80%-20%)
data_partition <- createDataPartition(data$RiskLevel, p = 0.80, list = FALSE)

# create training set and testing set
train_data <- data[data_partition, ]
test_data <- data[-data_partition, ]

# re-level the data
train_data$RiskLevel <- relevel(train_data$RiskLevel, ref = "high risk")

# Regression Model
require(nnet)

regression_model <- multinom(RiskLevel ~., data = data)

# model summary
summary(regression_model)

# convert coefficients to odds
exp(coef(regression_model))

# examine the likelihood of each observation to fall under a particular category, for the first six observations
head(round(fitted(regression_model), 2))
# in this example, the first two examples have the highest likelihood to fall under the high risk category

# predicting on the train data
train_data$PredictedRiskLevel <- predict(regression_model, newdata = train_data, "class")

# classification table - train
classification_table_train <- table(train_data$RiskLevel, train_data$PredictedRiskLevel)
classification_table_train

# calculate training accuracy
training_accuracy <- round((sum(diag(classification_table_train)) / sum(classification_table_train)) * 100, 2)
print(paste("The training accuracy is", training_accuracy))

# predicting on the test data
test_data$PredictedRiskLevel <- predict(regression_model, newdata = test_data, "class")

# classification table - test
classification_table_test <- table(test_data$RiskLevel, test_data$PredictedRiskLevel)
classification_table_test

# calculate test accuracy
test_accuracy <- round((sum(diag(classification_table_test)) / sum(classification_table_test)) * 100, 2)
print(paste("The test accuracy is", test_accuracy))

# Decision Trees =====

# install packages and load libraries
install.packages(c("DAAG", "party", "rpart", "mlbench", "pROC", "tree", "rpart.plot"))
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(tree)
library(tidymodels)
library(pROC)

# This is our second model

# set seed
set.seed(1234)

# split into train and test (80% - 20%)
index <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))

# create training set and testing set
train1 <-data[index == 1,]
test1 <- data[index==2,]

# Decision Tree Model
decision_tree_model <- rpart(RiskLevel ~., data = train1)
rpart.plot(decision_tree_model, trace=1)

# predicting on training set
train_pred <- predict(decision_tree_model, train1, type = "class")

# confusion matrix - train
conf_mat_train <- confusionMatrix(train_pred, train1$RiskLevel, positive = "y")
print(conf_mat_train)

# predicting on test set
test_pred <- predict(decision_tree_model, test1, type = "class")

# confusion matrix - test
conf_mat_test <- confusionMatrix(test_pred, test1$RiskLevel, positive = "y")
print(conf_mat_test)

# K-Nearest Neighbors =====

# install packages and load libraries
install.packages(c("e1071", "FNN", "gmodels", "psych"))
library(caret)
library(class)
library(e1071)
library(FNN)
library(gmodels)
library(psych)
# This is the final part of modeling
# Re-import fresh data
knn_data <- read.csv("Maternal Health Risk Data Set.csv")
head(knn_data)

# Data Preprocessing for KNN
# remove the two rows with error
subset(knn_data, subset = knn_data$HeartRate == min(knn_data$HeartRate))
knn_data <- knn_data[-c(500, 909), ]

# change variable names to lower case
variable_names <- tolower(colnames(knn_data))
colnames(knn_data) <- variable_names
head(knn_data)

# create a class for the target variable
knn_class <- knn_data
head(knn_class)
str(knn_class)

# convert the categorical variable type to factor
knn_class$risklevel <- as.factor(knn_class$risklevel)
levels(knn_class$risklevel)

# split the independent and dependent variables
target_var <- knn_class %>% dplyr::select(risklevel)
knn_class <- knn_class %>% dplyr::select(-risklevel)

str(knn_class)

# convert the variables body temperature and blood sugar to integer
knn_class$bs <- as.integer(knn_class$bs)
knn_class$bodytemp <- as.integer(knn_class$bodytemp)

str(knn_class)

# if we had factor variables as independent variables, we would have to recode them as dummies
# since we do not have such variables, we may skip this step

# KNN Modeling

# set seed
set.seed(1234)

# sample and split 80% of the data into train
# the remaining is test
sample <- floor(0.80 * nrow(knn_class))
index_knn <- sample(seq_len(nrow(knn_class)), size = sample)

# split into test and train
predictors_train <- knn_class[index_knn,]
predictors_test <- knn_class[-index_knn,]

target_train <- target_var[index_knn,]
target_test <- target_var[-index_knn,]

# KNN Model
knn_model <- knn(train = predictors_train,
                 test = predictors_test,
                 cl = target_train,
                 k = 10)

# model evaluation
target_test <- data.frame(target_test)

# merge predictions and actual
comparison <- data.frame(knn_model, target_test)

# column names
names(comparison) <- c("Predicted Risk Level", "Actual Risk Level")
head(comparison)

# check model accuracy
CrossTable(x = comparison$`Actual Risk Level`,
           y = comparison$`Predicted Risk Level`,
           prop.chisq = FALSE,
           prop.c = FALSE,
           prop.r = FALSE,
           prop.t = FALSE)

# find the optimal k
knn_caret_model <- train(predictors_train, 
                        target_train, 
                        method = "knn", 
                        preProcess = c("center", "scale"))
knn_caret_model

plot(knn_caret_model,
     main = "Number of Neighbors vs Accuracy",
     xlab = "Number of Neighbors",
     ylab = "Accuracy")

# predictions
knn_predictions <- predict(knn_caret_model,
                           newdata = predictors_test)
# confusion matrix - test
confusionMatrix(knn_predictions, target_test$target_test)

# Predictions using User Input ------

# To summarize the accuracy scores, 
# Test accuracies of:
# Multinomial Regression Model - 60.20%
# Decision Tree Model - 66.98%
# KNN Model - 72.41%

{
  # user inputs
  print("INSTRUCTIONS")
  print("Hello User, please enter patient details below to calculate maternal health risk")
  print("Please ensure you have familiarized yourself with the models and their accuracy scores")
  print("Please make sure to enter positive numbers only.")
  first_in = tolower(readline("Do you want to proceed? (Yes/No)"))
  
  if(first_in == "yes" | first_in=="y"){
    age_input = as.numeric(readline("Please enter the patient's age here----->"))
    sysbp_input = as.numeric(readline("Please enter the patient's systolic bp here----->"))
    diasbp_input = as.numeric(readline("Please enter the patient's diastolic bp here----->"))
    bs_input = as.numeric(readline("Please enter the patient's blood sugar here----->"))
    bodytemp_input = as.numeric(readline("Please enter the patient's body temperature here----->"))
    heartrate_input = as.numeric(readline("Please enter the patient's heartrate here----->"))
    
    # data frame conversion
    newTest <- data.frame(Age = age_input,
                          SystolicBP = sysbp_input,
                          DiastolicBP = diasbp_input,
                          BS = bs_input,
                          BodyTemp = bodytemp_input,
                          HeartRate = heartrate_input)
    newTest
    
    # regression prediction
    pred_reg = predict(regression_model, newdata = newTest)
    print(paste("According to the Regression Model, the patient risk level is ------>"))
    print(pred_reg)
    
    # decision tree prediction
    pred_dt = predict(decision_tree_model, newdata = newTest)
    print(paste("According to the Decision Tree Model, the patient risk level is ------>"))
    print(pred_dt)
    
    # preprocess for knn
    var_names <- tolower(colnames(newTest))
    colnames(newTest) <- var_names
    
    # convert all the variables to integer type
    newTest$age = as.integer(newTest$age)
    newTest$systolicbp = as.integer(newTest$systolicbp)
    newTest$diastolicbp = as.integer(newTest$diastolicbp)
    newTest$bs = as.integer(newTest$bs)
    newTest$bodytemp = as.integer(newTest$bodytemp)
    newTest$heartrate = as.integer(newTest$heartrate)
    
    # prediction
    pred_knn <- predict(knn_caret_model, newdata = newTest)
    print(paste("According to the KNN Model, the patient risk level is ------>"))
    print(pred_knn)
    
    # additional warning messages
    print(paste("**********ADDITIONAL WARNING MESSAGES**********"))
   
      if(age_input < 20){print("Patient is below 20 years of age")}
      if(sysbp_input < 110 | sysbp_input > 139){print("Patient Systolic BP varies from considered normal")}
      if(diasbp_input < 70 | diasbp_input > 89){print("Patient Diastolic BP varies from considered normal")}
      if(bs_input < 7.8 | bs_input > 7.8){print("Patient Blood Sugar varies from considered normal")}
      if(bodytemp_input < 95.5 | bodytemp_input > 99.5){print("Patient Body Temperature varies from considered normal")}
      if(heartrate_input < 69 | heartrate_input > 90){print("Patient Heart Rate varies from considered normal")}
    
  }else{
    print("Please re-run this function once you are ready")
    }
}
