library(readxl)

df <- read_xlsx("ML_ready/data_ML.xlsx")

main <- read_xls("economy.xls", sheet = '2011-2019 NACE 2')
colnames(main) <- main[3,]
main <- main[4,]
main <- main[,c(16:103)]

df = as.data.frame(df)
rownames(df) = df$months
df$months = NULL

df = df[, colSums(df != 0) > 0]


df = df[, (df[86,] != 0) > 0]


df_sum <- colSums(df)
df_sum <- sort(df_sum,decreasing = T)

top_20 = head(df_sum,n = 20)

top_20_names = colnames(t(as.data.frame(top_20)))

top_20_loc = which(colnames(df) %in% top_20_names)



try <- df[, top_20_loc]

try <- try[-c(1:33),]

###

main <- main[,c(34:88)]
# main <- as.data.frame(t(main))
# main <- main[,c(1:53)]
main <- t(main)



# library(neuralnet)
attach(try)
names <- colnames(try)
colnames(try) <- paste0("name",c(1:20))

try <- try[-c(56,57),]

try$main <- as.numeric(main)
try$main <- try$main * 100000000

index <- sample(1:nrow(try),round(0.85*nrow(try)))

train <- try[index,]
test <- try[-index,]

n <- names(train)

f <- as.formula(paste("main ~",paste(n[!n %in% "main"], collapse = " + ")))

# nn <- neuralnet(formula = f, data=train, hidden = c(3,2), linear.output = T, stepmax=1e+09,
#                 rep = 1, threshold = 0.2)
# 
# nn
# 
# nn$result.matrix
# 
# out <- cbind(nn$covariate,nn$net.result[[1]])
# out
# 
# head(nn$generalized.weights[[1]])
# 
# 
# plot(nn)
# 
# pr.nn <- compute(nn,test)
# 
# MSE.nn <- sum((test - pr.nn$net.result)^2)/nrow(test)
# 
# plot(test$what,pr.nn$net.result,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
# abline(0,1,lwd=2)
# legend('bottomright',legend='NN',pch=18,col='red', bty='n')

## linear

library(MASS)
library(caret)
fit <- lm(main~., train)
pred1<-predict(fit, newdata = test)
RMSE1<-RMSE(test$main, pred1)
RMSE1
MAE1 <- MAE(test$main, pred1)
MAE1
# KNN

ctrl <- trainControl(method = "cv", number = 10)

knn_c <- train(f, data = train, method = "knn",
               trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 10)
knn_c$results

ggplot(knn_c, metric = "RMSE")

model_predict_test = predict(knn_c, newdata = test)
which.min(c(sqrt(mean(abs(model_predict_test - test$main)^2)),sqrt(mean((test$main- predict(fit, test))^2))))

RMSE_KNN <- RMSE(test$main, model_predict_test)
RMSE_KNN

MAE_KNN <- MAE(test$main, model_predict_test)
MAE_KNN

# tree

library(rpart)
# train2 = sample (1:nrow(try), nrow(try)/1.2)
my_model <- rpart(f,subset = index, data= try)
library(rpart.plot)
rpart.plot(my_model, type = 4)

predictions<-predict(my_model,newdata=test)

RMSE_Tree <- RMSE(predictions, test$main)
RMSE_Tree
MAE_Tree <- MAE(predictions, test$main)
MAE_Tree

# forrest

library(randomForest)
set.seed(1)
bag.black <- randomForest(f,data=try, subset=index,importance =TRUE)

prediction_forest = predict(bag.black, newdata=test[1,])

{plot(prediction_forest, test$main)
abline (0,1)}

MAE_forest <- MAE(prediction_forest, test$main)
RMSE_forest <- RMSE(prediction_forest, test$main)
MAE_forest
RMSE_forest


# Ridge

x = model.matrix(f, data = try)[,c(-1,-2)]
y = try$main
library(glmnet)
grid = 10^seq(10,-2, length = 100)
ridge.mod = glmnet(x[index,], y[index], alpha = 0, lambda = grid,
                   thresh = 1e-12)
ridge.pred =predict(ridge.mod, s = 4, newx = x[-index,])
RMSE_ridge <- RMSE(ridge.pred, y[-index])
RMSE_ridge
MAE_ridge <- MAE(ridge.pred, y[-index])

# Lasso 

lasso.mod=glmnet(x[index,],y[index],alpha=0.9,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[index,],y[index],alpha=0.9)

lasso.mod=glmnet(x[index,],y[index],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[index,],y[index],alpha=1)

plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[-index,])
RMSE_lasso <- RMSE(lasso.pred, y[-index])
RMSE_lasso
MAE_lasso <- MAE(lasso.pred, y[-index])
MAE_lasso


# xgboost

library(xgboost)
set.seed(1)
# xgb <- xgboost(x[train,],y[train], max.depth = 10,nrounds = 25)
# set.seed(123)
# train = sample(1:nrow(x), nrow(x)/2)
# test = (-train)
# dtrain <- xgb.DMatrix(data = x[train,], label = y[train])
# dtest <- xgb.DMatrix(data = x[test,], label = y[test])

dtrain2 <- xgb.DMatrix(data = x[index,], label = y[index])
dtest2 <- xgb.DMatrix(data = x[-index,], label = y[-index])
watchlist <- list(train= dtrain2, test= dtest2)
set.seed(1)
bst2 <- xgb.train(data= dtrain2, max.depth=20, eta=0.09, nrounds=120,watchlist=watchlist,
                  base_score = 0.1)

xgb_test <- predict(bst2, data.matrix(test[,-c(1,21)]))

RMSE_xgboost <- RMSE(test$main, xgb_test)

MAE_xgboost <- RMSE(test$main,xgb_test)

rmse_table <- table(RMSE1, RMSE_forest, RMSE_lasso, RMSE_ridge, RMSE_Tree, RMSE_xgboost)
rmse <- as.data.frame(rmse_table)
rmse <- rmse[-c(7)]
rmse <- t(rmse)

sum(ridge.pred)/sum(y[-index])

RMSE_forest/mean(y[-index])

# 
RMSE_lasso/mean(y[-index])

RMSE_xgboost/mean(y[-index])

RMSE_ridge/mean(y[-index])

RMSE_forest/mean(y[-index])


difference <- function(x1,x2) {
  abs(x1-x2)/((x1+x2)/2)
}

difference(ridge.pred,test$main)

mean_error <- function(x) {
  sum(x)/length(x)
}

mean_error(difference(ridge.pred,test$main))
