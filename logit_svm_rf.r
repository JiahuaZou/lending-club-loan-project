
library(dplyr)
library(tidyr)
library(ISLR)
library(MASS)
library(ggplot2)
library(e1071)

df <- read.csv('cleaned_loan_data.csv')
head(df$loan_rate, 20)

df2 <- df %>% filter(loan_rate != 2) %>% dplyr::select(-c(policy_code, id, X, member_id, term, out_prncp_inv)) %>% select_if(is.numeric) 
df2$loan_rate <- as.factor(df2$loan_rate)
head(df2$loan_rate)

useful_var = c("loan_amnt", "funded_amnt", "funded_amnt_inv", 
               "int_rate", "installment", "dti", "delinq_2yrs", "inq_last_6mths", 
               "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", 
               "total_rec_int", "total_rec_late_fee", "collections_12_mths_ex_med", 
               "acc_now_delinq", "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim", "loan_rate")

df3 <- df2[, useful_var]
colnames(df2)

set.seed(1)
train=sample(nrow(df3), round(0.75 * nrow(df3)))
trainset=df3[train,]
testset=df3[-train,]

glm.fit2 = glm(loan_rate~., data = trainset, family = binomial)
summary(glm.fit2)

glm.probs=predict(glm.fit2, testset, type="response")
glm.pred=rep(0, nrow(testset)) 
glm.pred[glm.probs>0.5]=1
table(glm.pred, testset$loan_rate) 
1-mean(glm.pred==testset$loan_rate)

colnames(testset)

testset$loan_rate <- as.numeric(as.character(testset$loan_rate))

fit = glm(loan_rate ~ int_rate, data=trainset, family=binomial)
newdat <- data.frame(int_rate=seq(min(testset$int_rate), max(testset$int_rate),len=100))
newdat$loan_rate = predict(fit, newdata=newdat, type="response")
plot(loan_rate~int_rate, data=testset, col="red4")
lines(loan_rate~int_rate, newdat, col="green4", lwd=2)

ggplot(testset, aes(x=int_rate, y=loan_rate)) + geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

hist(df3$numeric_loan_rate)

temp <- sample(nrow(df3), 30000)
df4 <- df3[temp, ]

train = sample(nrow(df4), round(0.8*nrow(df4)))
length(train)

#set.seed(1) 
#tune.out=tune(svm, loan_rate~., data=df4[train,], kernel = "sigmoid", 
              #ranges=list(cost=c(0.1, 1, 10, 100, 1000))) 
#summary(tune.out)

svmfit=svm(loan_rate~., data=df4[train,], kernel = "radial", cost = 0.1) 
y_pred2 = predict(svmfit, newdata=df4[-train,])

true=df4[-train, "loan_rate"]
table(true, y_pred2)
1-(sum(true == y_pred2)/nrow(df4[-train,]))

temp <- df4[-train,]
head(temp)

plot(svmfit, temp, delinq_2yrs~loan_amnt) 

true2=df2[-train, "loan_rate"]
pred2=predict(svmfit, newdata=df2[-train,])
table(true2, pred2)

1-(sum(true2 == pred2)/nrow(df2[-train,]))

svmfit3 = svm(loan_rate~., data=df4[train,], kernel = "sigmoid", cost = 0.1) 
y_pred3 = predict(svmfit3, newdata=df4[-train,])
true3=df2[-train, "loan_rate"]
pred3=predict(svmfit3, newdata=df2[-train,])
table(true3, pred3)
1-(sum(true3 == pred3)/nrow(df2[-train,]))

1-((15881 + 110444)/(15881+34603+25384+110444))

plot(svmfit3, temp, delinq_2yrs~loan_amnt) 

colnames(df4)

train2 = sample(nrow(df3), round(0.8 * nrow(df3)))
length(train2)

trainset2 <- df3[train2,]
dim(trainset2)

testset2 <- df3[-train2, ]
dim(testset2)

library(randomForest)
rf_model <- randomForest(loan_rate~.,data = trainset2, keep.forest=TRUE)

rf_model

varImpPlot(rf_model)

importance(rf_model)

yhat_rf <- predict(rf_model, newdata = testset2)
plot(yhat_rf, testset2$loan_rate)
abline(0, 1)

table(testset2$loan_rate, yhat_rf)
1-(sum(testset2$loan_rate == yhat_rf)/nrow(testset2))
