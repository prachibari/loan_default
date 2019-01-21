##############################################################################
# 1. Import libraries
##############################################################################
library(gmodels) #for CrossTable
library(ggplot2) #for ploting graphs
library(caTools) #for splitting data
library(ROCR) #plotting ROC Curves
library(caret) #for using K fold cross validation
library(ROSE) #for upsampling
library(stringr) #for extracting numeric values from emp_lenght
library(tidyr)
library(dplyr) #for pipe operations and subset
library(corrplot) #for plotting US map
library(choroplethrMaps)
library(choroplethr)
library(reshape2) #used melt function for corr_data
library(egg) #for arranging plots side by side

#------------------------------------------------------------------------------------
# 2. Import data files
#------------------------------------------------------------------------------------
#raw <- read_excel("C:\\Users\\Rohan\\Desktop\\work\\machine_learning\\rohit_class_2\\RAW.csv")
#raw <- read_excel("C:/Users/Rohan/Desktop/work/machine_learning/rohit_class_2/Evaluation/Docs/raw.xlsx")
# raw <- read_csv("C:/Users/Rohan/Desktop/work/machine_learning/rohit_class_2/Evaluation/Docs/final.csv")
# raw2 = read.csv('ML_final.csv',header = T)
# raw1 = read.csv('final_19012019.csv',header = T)
#raw <- read_csv("C:/Users/Rohan/Desktop/work/machine_learning/rohit_class_2/Evaluation/Docs/final.csv")
raw2 = read.csv('D:\\Aegis\\Machine_Learning\\Evaluation\\Docs\\By_Prachi\\ML_final.csv',header = T)
raw1 = read.csv('D:\\Aegis\\Machine_Learning\\Evaluation\\Docs\\By_Prachi\\final_19012019\\final_19012019.csv',header = T)
raw = rbind(raw2,raw1)
names(raw)
raw_bckup <- raw
#raw = raw_bckup

###############################################################################
# 3. Data preparation
###############################################################################
summary(raw)
#Only considering the following status for analysis
raw = subset(raw, raw$loan_status %in% c("Fully Paid", "Charged Off", "Default"))
nrow(raw)

#Added default column and changed loan_status into 0 & 1.
raw$default = ifelse(raw$loan_status %in% c("Fully Paid"),0,1)
raw$default = as.factor(raw$default)
summary(raw$default)

#Purpose column has 14 levels. Considering only top 4 and grouping rest into others
summary(raw$purpose)
raw$purpose = as.character(raw$purpose)
raw$c_purpose = ifelse(raw$purpose %in% c("credit_card"
                                          ,"debt_consolidation"
                                          ,"home_improvement"
                                          ,"major_purchase")
                                          ,raw$purpose
                                          ,"others")

raw$c_purpose = as.factor(raw$c_purpose)
summary(raw$c_purpose)

#We can see home_ownership has 1 records with ANY level, so dropping it
summary(raw$home_ownership)
raw$home_ownership = as.character(raw$home_ownership)
raw <- raw %>% subset(raw$home_ownership == "MORTGAGE" | raw$home_ownership == "OWN" | raw$home_ownership == "RENT")
raw$home_ownership = as.factor(raw$home_ownership)
summary(raw$home_ownership)

#Added emp_cat column and changed into buckets.
summary(raw$emp_length)
raw$emp_cat <- rep(NA, length(raw$emp_length))
raw$emp_cat[which(raw$emp_length == "< 1")] <- "0-1"
raw$emp_cat[which(raw$emp_length == "1" | raw$emp_length=="2" | raw$emp_length=="3")] <- "1-3"
raw$emp_cat[which(raw$emp_length == "4" | raw$emp_length=="5" | raw$emp_length=="6")] <- "4-6"
raw$emp_cat[which(raw$emp_length == "7" | raw$emp_length=="8" | raw$emp_length=="9")] <- "7-9"
raw$emp_cat[which(raw$emp_length == "10+")] <- "10+"
raw$emp_cat <- as.factor(raw$emp_cat)
summary(raw$emp_cat)

#converting term to factor
raw$term = as.factor(raw$term)
summary(raw$term)

raw$id = NULL
#Checking for N/A's.
summary(raw)

##########################################################################
#4.  Exploratory Data Analysis
##########################################################################
#Checking Outliers

boxplot(raw$loan_amnt,notch = TRUE, col = "red",main="Box Plot of Loan Amount")
boxplot(raw$int_rate,notch = TRUE, col = "red",main="Box Plot of Interest Rate") #has outliers but wont remove them
boxplot(raw$annual_inc,notch = TRUE, col = "red",main="Box Plot of Annual Income") #Has too many outliers 
boxplot(raw$delinq_2yrs,notch = TRUE, col = "red",main="Box Plot of Delinq 2years") #Has outliers but wont remove them as numbers are valid.
boxplot(raw$inq_last_6mths,notch = TRUE, col = "red",main="Box Plot of inq_last6mnths") #has few outliers but wont remove them as numbers are valid
boxplot(raw$revol_bal,notch = TRUE, col = "red",main="Box Plot of Revolving Bal") #Has too many outliers
boxplot(raw$revol_util,notch = TRUE, col = "red",main="Box Plot of Revolving util")
boxplot(raw$open_acc,notch = TRUE, col = "red",main="Box Plot of Open Account") #Has too many outliers 
boxplot(raw$pub_rec,notch = TRUE, col = "red",main="Box Plot of Public Records") #Has too many outliers 

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  if(!is.na(x)){
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
  }
  y
}

#Removing outliers in Loan Amount
#----------------------------------------------------------
hist(raw$loan_amnt, col = "red", main = "Loan Amount",xlab = "Loan Amount")
y = as.data.frame(remove_outliers(raw$loan_amnt))
colnames(y)= 'loa'
raw<-cbind(raw,y)
#remove NA values
raw = subset(raw,!is.na(loa))
hist(raw$loan_amnt, col = "red", main = "Distribution of Loan Amount", xlab = "Loan Amount")


#Removing outliers in Annual Income
#----------------------------------------------------------
ggplot(data=raw,aes(raw$annual_inc))+geom_histogram(col="red",aes(fill=..count..))+labs(x="Annual Income",y="Count",title="Annual Income With Outliers")
y = as.data.frame(remove_outliers(raw$annual_inc))
colnames(y)= 'inc'
raw<-cbind(raw,y)
#remove NA values
raw = subset(raw,!is.na(inc))
ggplot(data=raw,aes(raw$annual_inc))+geom_histogram(col="red",aes(fill=..count..))+labs(x="Annual Income",y="Count",title="Annual Income After Outlier Removal")

#Removing outliers in Revolving Balance
#----------------------------------------------------------
ggplot(data=raw,aes(raw$revol_bal))+geom_histogram(col="red",fill="pink")+labs(x="Revol Balance",y="Count",title="Revol Balance With Outliers")
y = as.data.frame(remove_outliers(raw$revol_bal))
colnames(y)= 'rev_bal'
raw<-cbind(raw,y)
colnames(raw)
#remove NA values
raw = subset(raw,!is.na(rev_bal))
ggplot(data=raw,aes(raw$revol_bal))+geom_histogram(col="red",fill="pink")+labs(x="Revol Balance",y="Count",title="Revol Balance With Outlier Removal")

#Removing outliers in Open Accounts
#----------------------------------------------------------
ggplot(data=raw,aes(raw$open_acc))+geom_histogram(col="red",fill="lightblue")+labs(x="Open Account",y="Count",title="Open Accounts With Outliers")
y = as.data.frame(remove_outliers(raw$open_acc))
colnames(y)= 'ope'
raw<-cbind(raw,y)
colnames(raw)
#remove NA values
raw = subset(raw,!is.na(ope))
ggplot(data=raw,aes(raw$open_acc))+geom_histogram(col="red",fill="lightblue")+labs(x="Open Account",y="Count",title="Open Accounts Without Outliers")


#Removing outliers in revol_util
#--------------------------------------------------------
ggplot(data=raw,aes(raw$revol_util))+geom_histogram(col="blue",fill="lightgreen")+labs(x="Revol Util",y="Count",title="Revol Util With Outliers")
y = as.data.frame(remove_outliers(raw$revol_util))
colnames(y)= 're'
raw<-cbind(raw,y)
#remove NA values
raw = subset(raw,!is.na(re))
ggplot(data=raw,aes(raw$revol_util))+geom_histogram(col="blue",fill="lightgreen")+labs(x="Revol Util",y="Count",title="Revol Util Without Outliers")


#Removing redundant columns
raw$purpose = NULL
raw$rev_bal = NULL
raw$inc = NULL
raw$ope = NULL
raw$loan_status = NULL
raw$loa= NULL
raw$inq = NULL
raw$re = NULL
summary(raw)


#Plotting Default State Wise
#--------------------------------------------
summary(raw$default)
raw$default = as.character(raw$default)
raw$default = as.numeric(raw$default)
unique(raw$default)

Default_By_state <-
  raw %>% group_by(addr_state)  %>%
  select(addr_state,default) %>%
  summarize_if(is.numeric,sum)

utils::data(state.map)
str(state.map)
#View(state.map)
unique(state.map$region)

states <- readr::read_tsv("S.txt")
str(states)
names(states) <- c("name", "fips_code", "usps_code")

#View(Default_By_state)
dplyr::setdiff(Default_By_state$addr_state, states$usps_code)

Default_By_state <-
  Default_By_state %>%
  left_join(states[, c("usps_code", "name")],
            by = c("addr_state" = "usps_code")) %>%
  dplyr::rename(region = name, value = default) %>%
  mutate(region = tolower(region)) %>%
  select(region, value)

choroplethr::state_choropleth(df = Default_By_state,
                              title = "Deafult by State")

raw$default = as.factor(raw$default)
unique(raw$default)
#---------------------------------------------

corr_data = melt(cor(subset(raw, select = c(loan_amnt,int_rate,installment,annual_inc,dti
                                            ,delinq_2yrs,inq_last_6mths,open_acc,pub_rec,revol_bal
                ))))

#NOT WORKING
qplot(x = Var1, y = Var2, data = corr_data, fill = value, geom = "tile")

ggplot(data = raw, aes(grade)) +
  geom_bar(aes(fill = default), color="black", stat = "count", alpha=0.6) +
  theme_light() +
  scale_fill_manual("default",values = c("green","red")) +
  labs(y="Percent", x="Loan Grades from A (best) to G (poor)") +
  ggtitle("Distribution of Loans By Grading Scores and Loan Status")

#Making Contingency Table to check percentage of grading score in relation with unpaid loans 
CrossTable(raw$grade, raw$default,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,
           prop.chisq = FALSE )

# df_grade_def = raw %>% count(grade, default) %>% filter(default == 0) %>% rename(default_0 = n) %>%
#   inner_join(raw %>% count(grade, default) %>% filter(default == 1) %>% rename(default_1 = n), by = "grade") %>%
#   select(grade, default_0, default_1) %>%
#   mutate(default_0_per = default_0/(default_0 + default_1)) %>%
#   mutate(default_1_per = default_1/(default_0 + default_1))

#Making distribution of loans by purpose
ggplot(data = raw, aes(c_purpose)) +
  geom_bar(aes(fill=default), position = "dodge", stat = "count") +
  theme_bw() +
  scale_fill_manual("default",values = c("green", "red")) +
  labs(y="Frequency", x="Loan Purpose") +
  ggtitle("Distribution of Loans By Purpose") + 
  theme(plot.background = element_rect(fill = "lightblue")) + 
  theme(axis.text.x=element_text(angle=45,hjust=0.7,vjust=0.7))


#Making Contingency Table to check percentage of grading score in relation with unpaid loans 
CrossTable(raw$c_purpose, raw$default,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,
           prop.chisq = FALSE )

#Making scatter diagram to control relation between interest rates and loans grades
# plot3 <- ggplot(raw, aes(x=grade, y=int_rate)) + geom_point(aes(color=default, alpha=0.4))
# plot3 <- plot3 + theme_bw() + scale_fill_manual("default", values = c("red", "green")) +
#   labs(y="Interest Rates" , x="Grades")
# plot3

#Making scatter diagram to control relation between interest rates and loans grades
plot3 <- ggplot(raw, aes(x=grade, y=int_rate)) + geom_boxplot(aes(color=int_rate))
plot3 <- plot3 + theme_bw() + labs(y="Interest Rates" , x="Grades") +
  ggtitle("Grades vs Interest Rate")
plot3

##################################################################
#5 .MODEL BUILDING
##################################################################
loan = raw #taking backup in a new dataframe
colnames(loan)

#Converting grades from A-G to 1-7 since its Ordinal Data
summary(loan)
loan$grade = as.character(loan$grade)

loan = loan %>% mutate(grade = (recode(grade, 'A' = 7,
                                 'B'=6,
                                 'C'=5,
                                 'D'=4,
                                 'E'=3,
                                 'F'=2,
                                 'G'=1)))

loan$grade = as.numeric(loan$grade)
summary(loan$grade)


#removed < and + from emp_length since model cant deal with special characters
loan$emp_length = as.numeric(str_extract(loan$emp_length,"[0-9]{1,2}"))

#removed earliest_cr_line as ROSE cannot handle dates
loan$earliest_cr_line = NULL
#Removing emp_Cat for now since we are considering emp_length in model
loan$emp_cat = NULL
loan$purpose = NULL 
loan$id = NULL
loan$addr_state = NULL

#checking co-relation
num_vars <- 
  loan %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()


corrplot::corrplot(cor(loan[, num_vars], use = "complete.obs"), 
                   method = "number", type = "upper",bg='black')


loan$installment = NULL #removing as there is high correlation with installment
loan$int_rate = NULL #since it has high correlation

summary(loan)

#Using Techniques for Over and Under Sampling
#--------------------------------------------------------------------------------
CrossTable(loan$default)  #We can see only 19% is default rate
set.seed(77)
summary(loan$default)
loan$default = as.factor(loan$default)
summary(loan$default)

loan.sample <- sample.split(loan$default, 0.7)
loan.train.data <- subset(loan, loan.sample==TRUE)
loan.test.data <- subset(loan, loan.sample==FALSE)


#Using ROSE generate data synthetically
library(ROSE)
data.rose <- ROSE(default ~ ., data = loan.train.data, seed = 7)$data
CrossTable(data.rose$default)

#over sampling
data_balanced_over <- ovun.sample(default ~ ., data = loan.train.data, method = "over",N = 238376)$data
table(data_balanced_over$default)

#under sampling
data_balanced_under <- ovun.sample(default ~ ., data = loan.train.data, method = "under", N = 59548, seed = 1)$data
table(data_balanced_under$default)

#both with 0.5 probability
data_balanced_both <- ovun.sample(default ~ ., data = loan.train.data, method = "both", p=0.5, seed = 1)$data
table(data_balanced_both$default)

# Scaling data
#-------------------------------------------------------
#Scaling numeric columns
summary(loan$default)

scale_df <- function(df){
  # Loop over each column.
  for (colName in names(df)) {
    # Check if the column contains numeric data.
    if(class(df[,colName]) == 'integer' | class(df[,colName]) == 'numeric') {
      # Scale this column (scale() function applies z-scaling).
      df[,colName] <- scale(df[,colName])
    }
  }
  return(df)
}


loan.train.data_sc = scale_df(loan.train.data)
loan.test.data_sc = scale_df(loan.test.data) 
data.rose_sc = scale_df(data.rose)
data_balanced_over_sc = scale_df(data_balanced_over)
data_balanced_under_sc = scale_df(data_balanced_under)
data_balanced_both_sc = scale_df(data_balanced_both)

###################################################
# Fitting Logistic Regression model to all the new datasets
################################################################
#1. Creating model with the original dataset
#----------------------------------------------------------------------------------
logistic.regressor.loan <- glm(default ~., family = "binomial", data = loan.train.data_sc)
summary(logistic.regressor.loan)

fitted.probabilities.loan <- predict(logistic.regressor.loan,newdata=loan.test.data_sc,type='response')
pred_cut_off <- ifelse(fitted.probabilities.loan > 0.5, 1,0) 
library(caret) # for confusionMatrix
confusionMatrix(loan.test.data$default, as.factor(pred_cut_off))
library(ROCR) #for prediction() and performance()


#2. Creating model with the over sampled dataset
#----------------------------------------------------------------------------------
logistic.regressor.data_balanced_over <- glm(default ~., family = "binomial", data = data_balanced_over_sc)
fitted.probabilities.data_balanced_over <- predict(logistic.regressor.data_balanced_over,newdata=loan.test.data_sc,type='response')
pred_data_balanced_over <- ifelse(fitted.probabilities.data_balanced_over > 0.5, 1,0) #Setting cut-off to be at 0.5
confusionMatrix(loan.test.data$default, as.factor(pred_data_balanced_over))

#2. Creating model with the under sampled dataset
#----------------------------------------------------------------------------------
logistic.regressor.data_balanced_under <- glm(default ~., family = "binomial", data = data_balanced_under_sc)
fitted.probabilities.data_balanced_under <- predict(logistic.regressor.data_balanced_under,newdata=loan.test.data_sc,type='response')
pred_data_balanced_under <- ifelse(fitted.probabilities.data_balanced_under > 0.5, 1,0) #Setting cut-off to be at 0.5
confusionMatrix(loan.test.data$default, as.factor(pred_data_balanced_under))

#3. Creating model with the under-over(both) sampled dataset
#----------------------------------------------------------------------------------
logistic.regressor.data_balanced_both <- glm(default ~., family = "binomial", data = data_balanced_both_sc)
fitted.probabilities.data_balanced_both <- predict(logistic.regressor.data_balanced_both,newdata=loan.test.data_sc,type='response')
pred_data_balanced_both <- ifelse(fitted.probabilities.data_balanced_both > 0.5, 1,0) #Setting cut-off to be at 0.5
confusionMatrix(loan.test.data$default, as.factor(pred_data_balanced_both))

#4. Creating model with the rose sampled dataset (FINAL MODEL TO BE USED)
#----------------------------------------------------------------------------------
logistic.regressor.data_rose <- glm(default ~.-open_acc, family = "binomial"(link = "logit"), data = data.rose_sc)
summary(logistic.regressor.data_rose)

fitted.probabilities.data_rose <- predict(logistic.regressor.data_rose,newdata=loan.test.data_sc,type='response')
pred_data_rose <- ifelse(fitted.probabilities.data_rose > 0.5, 1,0)
cm_rose = confusionMatrix(loan.test.data$default, as.factor(pred_data_rose))
cm_rose
cm_rose$table

# precision = cm_rose$table[4] /(cm_rose$table[4]+cm_rose$table[3])
# precision
# 
# recall = cm_rose$table[4] /(cm_rose$table[4]+cm_rose$table[2])
# recall

precision = cm_rose$table[1] /(cm_rose$table[1]+cm_rose$table[3])
precision

recall = cm_rose$table[1] /(cm_rose$table[1]+cm_rose$table[2])
recall


f1 = 2*((precision*recall)/(precision+recall))
f1

varImp(logistic.regressor.data_rose)
library(car)
vif(logistic.regressor.data_rose)

# pred <- prediction(pred_data_rose,loan.test.data$default)
# perf <- performance(pred, "tpr", "fpr")
# perf
# #Printing AUC Value
# perf1 <- performance(pred, "auc")
# perf1


#Plotting all ROC curves together
roc.curve(loan.test.data$default, pred_data_rose,col="red",main="The ROC-curve for Logistic Regression with cut-off 0.5")
roc.curve(loan.test.data$default, pred_data_balanced_both,col="blue" ,add.roc = TRUE)
roc.curve(loan.test.data$default, pred_data_balanced_under,col="green",add.roc = TRUE)
roc.curve(loan.test.data$default, pred_data_balanced_over,col="magenta",add.roc = TRUE)
roc.curve(loan.test.data$default, pred_cut_off,col="black",add.roc = TRUE)
legend(x = "bottomright", legend=c("rose = 0.65", "both = 0.65",
                                   "under = 0.60",
                                   "over = 0.58","original = 0.51"), 
       col = c("red", "blue", "green","magenta","black"), lty = 1, cex = 0.82)

#--------------------------------------------------------------

#Plotting Actual And Predicted Graphs of some important variables
ownership_pred =  ggplot(data = loan.test.data, aes(as.factor(home_ownership))) + 
                  geom_bar(aes(fill = as.factor(pred_data_rose)))+
                  labs(y="Count", x="Home Ownership",fill="default") +
                  ggtitle("Prediction Graph")
ownership_actual = ggplot(data = loan.test.data, aes(as.factor(home_ownership))) + 
                  geom_bar(aes(fill = default))+
                  labs(y="Count", x="Home Ownership") +
                  ggtitle("Home Ownership wise Default - Actual Graph")

grade_pred =   ggplot(data = loan.test.data, aes(as.factor(grade))) + 
               geom_bar(aes(fill = as.factor(pred_data_rose)))+
               labs(y="Count", x="Grades",fill="default") +
               ggtitle("Prediction Graph")+scale_fill_brewer(palette="Dark2")
grade_actual = ggplot(data = loan.test.data, aes(as.factor(grade))) + 
               geom_bar(aes(fill = default))+
               labs(y="Count", x="Grades") +
               ggtitle("Grade Wise Default - Actual Graph")+scale_fill_brewer(palette="Dark2")

figure <- egg::ggarrange(grade_actual, grade_pred,ownership_actual, ownership_pred, 
                         nrow = 2,ncol = 2)

emp_actual = ggplot(data = loan.test.data, aes(as.factor(emp_length))) + 
  geom_bar(aes(fill = default))+
  labs(y="Count", x="Emp Length") +
  ggtitle("Emp Length Wise Default - Actual Graph")

emp_pred = ggplot(data = loan.test.data, aes(as.factor(emp_length))) + 
  geom_bar(aes(fill = as.factor(pred_data_rose)))+
  labs(y="Count", x="Emp Length",fill="default") +
  ggtitle("Predicted Graph")

figure <- egg::ggarrange(emp_actual, emp_pred, nrow = 1)
#employment length 1 and 10 are more likely to default


####################################################################
#Using ROSE Data with Lasso and Ridge Regularization
###################################################################
#---------------------------------------------------------------------------------
# Dumy code categorical predictor variables
# x <- model.matrix(default~.-addr_state-open_acc-revol_bal, data.rose)[,-1]s
x <- model.matrix(default~., data.rose)[,-1]
# Convert the outcome (class) to a numerical variable
y <- as.numeric(data.rose$default)

library(glmnet)
# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial") #using lasso regularization
# Fit the final model on the training data
model_lasso <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model_lasso)
# Make predictions on the test data
x.test <- model.matrix(default ~., loan.test.data)[,-1]
probabilities <- model_lasso %>% predict(newx = x.test)
predicted.classes_lasso <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- loan.test.data$default
mean(predicted.classes_lasso == observed.classes)
cv.lasso$lambda.min

#------Using Ridge Regularization ----------------------------
cv.ridge <- cv.glmnet(x, y, alpha = 0, family = "binomial") #using ridge regularization
model_ridge <- glmnet(x, y, alpha = 0, family = "binomial",
                lambda = cv.ridge$lambda.min)
# Display regression coefficients
coef(model_ridge)
# Make predictions on the test data
probabilities <- model_ridge %>% predict(newx = x.test)
predicted.classes_ridge <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
mean(predicted.classes_ridge == observed.classes)
cv.lasso$lambda.min

#----------------------------------------------------------------------
confusionMatrix(loan.test.data$default, as.factor(predicted.classes_ridge))
confusionMatrix(loan.test.data$default, as.factor(predicted.classes_lasso))

plot(cv.ridge)
plot(cv.lasso)

#Plotting the ROC-curve
roc.curve(loan.test.data$default, predicted.classes_lasso,col="red", main="ROC-curve for Model with Lasso and Ridge Regularization")
roc.curve(loan.test.data$default, predicted.classes_ridge,col="blue",add.roc = T)
legend(x = "bottomright", legend=c("lasso = 0.599", "ridge = 0.597"), 
       col = c("red", "blue"), lty = 1, cex = 1.0)


########################################################################
##3. Using k fold cross validation on ROSE Dataset
#-------------------------------------------------------------------------
#Using K fold cross validation using Caret requires variable names in soecific format
#for eg , default had to be converted to yes and no instead of 0 and 1 since it gives
# error during the training phase.Due to this the predicted values are yes and no and 
# we have no control over adjusting the probability of default like we did in glm.
# We do manage to get good accuracy and decent ROC but the specificity and sensitivity gets really worse
# hence we wouldnt use k fold cross validation

data.rose.kfold <- ROSE(default ~ ., data = loan.train.data, seed = 7)$data
CrossTable(data.rose.kfold$default)

data.rose.kfold = data.rose 
data.rose.kfold$default = ifelse(data.rose.kfold$default == 1,"yes","no")
data.rose.kfold$default = as.factor(data.rose.kfold$default)
summary(data.rose.kfold)
str(new)

train_index <- 
  caret::createDataPartition(y = data.rose.kfold$default, times = 1, 
                             p = .7, list = FALSE)

train <- data.rose.kfold[train_index, ]
test <- data.rose.kfold[-train_index, ]

ctrl <- 
  trainControl(method = "cv", 
               number = 10,
               repeats = 1,
               classProbs = TRUE,
               summaryFunction = twoClassSummary, #use ROC, default Accuracy
               savePredictions = TRUE,
               verboseIter = FALSE)


glm_kfold <- caret::train(default~.-open_acc,data=train,
                                method = "glm",
                                family="binomial",
                                trControl = ctrl)
summary(glm_kfold)
pred = predict(glm_kfold, newdata=test)
confusionMatrix(data=as.factor(pred),as.factor(test$default))
roc.curve(test$default, pred,col="red", main="ROC-curve for Model with Cross Validation")
text(0.8,0.8,paste("AUC=68"))


#############################
#Trying to plot learning curve
set.seed(29510)
lda_data <- learing_curve_dat(dat = train,
                              outcome = "default",
                              test_prop = 1/4,
                              ## `train` arguments:
                              method = "glm",
                              metric = "ROC",
                              trControl = trainControl(classProbs = TRUE,
                                                       summaryFunction = twoClassSummary))



ggplot(lda_data, aes(x = Training_Size, y = ROC, color = Data)) +
  geom_smooth(method = loess, span = .8) +
  theme_bw()
