###########################################
### NEED TO BE ASSIGNED FROM THE USER ####

library(glmnet)

#set seed number
SNUM=3
set.seed(SNUM)

#set number of folds for Shrinkage methods
NFOLDS=10

#variable count
NUM<- ncol(dataset)-2
#create an expression of response-predictor - in case of all use dot (.)
EXP =  house_prices_mean ~ .
RES = dataset$house_prices_mean
#MATRIX TO STORE OUR PLOTS
par(mfrow=c(2,1))


###########################################
### END ####




####### CODE IS NOW AUTOMATED ###########

#BLOCK 1
#DATA - LAMBDA MODIFICATIONS
#EXTENDED LAMBDA VALUES
grid=10^seq(10,-2,length=100)

#DATA PREPARATION
#assign x & y
x= model.matrix(as.formula(EXP) ,dataset)[,-1]
y= RES
# CV - DATA PREPARATION
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

#BLOCK 2
#CV ON TRAIN DATA - MSE ESTIMATION
lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
plot(lasso.mod, xvar="lambda", label=TRUE, ,main="Lambda-Coefficients plot - TRAIN DATA")

set.seed (SNUM)
cv.out=cv.glmnet(x[train ,],y[train],alpha=1, , nfolds=NFOLDS)
plot(cv.out, main="Lambda - MSE Error - TRAIN DATA")

#BLOCK 2.1
# LOWEST MSE
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
TestMseWithLambaOfLowTrainMse=mean((lasso.pred-y.test)^2)
format(TestMseWithLambaOfLowTrainMse, big.mark=".", decimal.mark="," , scientific=FALSE)
#coeff for every variable
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef.lmse=predict(out,type="coefficients",s=bestlam)[1:NUM+2,]

#exclude all zero coeff variables
lasso.coef.lmse[lasso.coef.lmse!=0]


#BLOCK 2.2
#ONE STANDARD ERROR RULE
oneselam=cv.out$lambda.1se
oneselam
lasso.pred=predict(lasso.mod,s=oneselam ,newx=x[test,])
TestMseWithLambaOfOneStandardErrorTrainMse=mean((lasso.pred-y.test)^2)
format(TestMseWithLambaOfOneStandardErrorTrainMse, big.mark=".", decimal.mark="," , scientific=FALSE)
#coeff for every variable
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef.1se=predict(out,type="coefficients",s=oneselam)[1:NUM+2,]

#exclude all zero coeff variables
lasso.coef.1se[lasso.coef.1se!=0]


##### RESULTS #####

##########################################################
############### LASSO with LOWEST MSE ####################
bestlam
format(TestMseWithLambaOfLowTrainMse, big.mark=".", decimal.mark="," , scientific=FALSE)
##########################################################
#selected features for lowestMSE
names(lasso.coef.lmse[lasso.coef.lmse!=0])

##########################################################
############### LASSO with 1SE RULE   ####################
oneselam
format(TestMseWithLambaOfOneStandardErrorTrainMse, big.mark=".", decimal.mark="," , scientific=FALSE)
##########################################################
#selected features for 1 st error rule
names(lasso.coef.1se[lasso.coef.1se!=0])


##########################################
############### LASSO ####################
##########################################
TestMseWithLambaOfLowTrainMse
sqrt(TestMseWithLambaOfLowTrainMse)
format(TestMseWithLambaOfLowTrainMse, big.mark=".", decimal.mark="," , scientific=FALSE)      #we use format() to seperate numbers by dot
#number of total selected variables
sum(!lasso.coef.lmse == 0)

TestMseWithLambaOfOneStandardErrorTrainMse
format(TestMseWithLambaOfOneStandardErrorTrainMse, big.mark=".", decimal.mark="," , scientific=FALSE)      #we use format() to seperate numbers by dot
#Coefficients for 1 st error rule
sqrt(TestMseWithLambaOfOneStandardErrorTrainMse)
#number of total selected variables
sum(!lasso.coef.1se == 0)
