library(reshape2) 
library(glmnet)

source("getCubeObservations.R")
source("runQuery.R")

calculateFeatures <- function (yDataset, xDatasets, year) {
  
  #initialize some variables
  results <-reactiveValues(SME=NULL, NV=NULL,SE=NULL)
  start.time <- Sys.time()

  #get the observations of the response dataset
  df_y_full<-getCubeObservations('dataset_house_prices',c(year,'reference_area:"dz-2001"'),c('reference_area','mean'))
  
  end.time <- Sys.time()
  
  time.taken <- end.time - start.time
  print(paste('Time to get obs dataset_house_prices:',time.taken))
  
  start.time <- Sys.time()
  #### MANIPULATION OF RESPONSE (Y) DATAFRAME
  
  ###### THE FIRST COLUMN REFER TO THE REFERENCE AREA ####
  ###### TEMPORARY ASSUMPTION: THE SECOND COLUMN WILL BE OUR RESPONSE (Y), EVEN IF THERE ARE MORE COLUMNS
  df_y <- df_y_full[,c(1,2)]
  ###### END OF TEMPORARY ASSUMPTION
  
  #change column names 
  colnames(df_y)[1] <- "reference.area"
  colnames(df_y)[2] <- sub(".*?data\\.(.*?)(.observations.page.result*|$)", "\\1", colnames(df_y)[2])
  
  #### END MANIPULATION OF RESPONSE (Y) DATAFRAME      
  
  #########################################################################################
  #CREATE A TEMPORARY DATAFRAME WITH ONLY THE REFERENCE AREAS - WHICH WILL STORE ALL OUR COLUMNS (Y+X)
  #########################################################################################
  dataset_full <- df_y[1]
  
  #########################################################################################
  #COMBINE THE dataset_full with the values of df_y (Y)
  #########################################################################################      
  dataset_full <-merge(x = dataset_full, y = df_y,  by = "reference.area", all.x = TRUE) 
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste('Time to get create data full obs dataset_house_prices:',time.taken))
  
  #for each of the selected compatible datasets
  for (i in xDatasets()) {
    start.time <- Sys.time()

    #get their dimensions
    q5<- paste('{', i, '{  dimensions { enum_name }}}')
    df5<-runQuery(q5)
    
    #get their measures
    q6<- paste('{', i, '{  measures{ enum_name }}}')
    df6<-runQuery(q6)
    
    df5<-rbind(df5,df6)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste('Time to get get dims and measures:',i,' ',time.taken))
    
    start.time <- Sys.time()
    
    fixedvalues<-c('reference_area:"dz-2001"',year)
    df<-getCubeObservations(i,fixedvalues,tolower(c(t((df5)))))
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste('Time to get get obs:',i,' ',time.taken))
    
    start.time <- Sys.time()
    #simos
    xnam <- paste( "df$", colnames(df)[c(-1,-ncol(df))] , sep="")
    fmla <- paste(xnam, collapse=" + ")
    ref <- paste ("df$", colnames(df)[1] , sep="")
    measure <- colnames(df)[ncol(df)]
    scen <- as.formula(paste(ref , "~ " , fmla))
    df_formated <- dcast(df ,scen, value.var=measure)
    dataset_name <-sub(".*?data\\.(.*?)\\.observations.*", "\\1", colnames(df)[1])
    colnames(df_formated)[-1] <- paste( dataset_name , "." , colnames(df_formated)[-1] , sep="")
    colnames(df_formated)[1] <- "reference.area"
    dataset_clean <- df_formated[ lapply( df_formated, function(x) sum(is.na(x)) / length(x) ) < 0.1 ]
    dataset_full <-merge(x = dataset_full, y = dataset_clean,  by = "reference.area", all.x = TRUE)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste('Time to create data_full:',i,' ',time.taken))
    
    start.time <- Sys.time()

    dataset <- dataset_full
    dataset <- dataset[,-1]
    dataset <- data.matrix(dataset)
    dataset <- as.data.frame(dataset)
    dataset <- na.omit(dataset)
    
    #set seed number
    SNUM=1
    set.seed(SNUM)
    
    #set number of folds for Shrinkage methods
    NFOLDS=10
    #variable count
    NUM<- ncol(dataset)-2
    #create an expression of response-predictor - in case of all use dot (.)
    name_y = colnames(dataset[1])
    EXP =  paste( name_y ,  " ~ ." , sep="")
    RES =  dataset[1]
    #MATRIX TO STORE OUR PLOTS
    par(mfrow=c(2,1))
    
    ### END: NEED TO BE ASSIGNED ####
    ### EXECUTION ####
    
    #BLOCK 1
    #DATA - LAMBDA MODIFICATIONS
    #EXTENDED LAMBDA VALUES
    grid=10^seq(10,-2,length=100)
    
    #DATA PREPARATION
    #assign x & y
    x= model.matrix(as.formula(EXP) ,dataset)[,-1]
    #y= dataset[,ncol(dataset)]
    y= dataset[,1]
    # CV - DATA PREPARATION
    train=sample(1:nrow(x), nrow(x)/2)
    test=(-train)
    y.test=y[test]
    
    #BLOCK 2
    #CV ON TRAIN DATA - MSE ESTIMATION
    lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
    plot(lasso.mod, xvar="lambda", label=TRUE, main="Lambda-Coefficients plot - TRAIN DATA")
    #plotMSE <- plot(lasso.mod, xvar="lambda", label=TRUE, main="Lambda-Coefficients plot - TRAIN DATA")
    
    set.seed (SNUM)
    cv.out=cv.glmnet(x[train ,],y[train],alpha=1, nfolds=NFOLDS)
    plot(cv.out, main="Lambda - MSE Error - TRAIN DATA")
    #plotCV <-   plot(cv.out, main="Lambda - MSE Error - TRAIN DATA")
    
    
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
    
    ##########################################################
    ############### LASSO with LOWEST MSE ####################
    bestlam
    format(TestMseWithLambaOfLowTrainMse, big.mark=".", decimal.mark="," , scientific=FALSE)
    ##########################################################
    #Coefficients for lowestMSE
    lasso.coef.lmse[lasso.coef.lmse!=0]
    
    ##########################################################
    ############### LASSO with 1SE RULE   ####################
    oneselam
    format(TestMseWithLambaOfOneStandardErrorTrainMse, big.mark=".", decimal.mark="," , scientific=FALSE)
    ##########################################################
    #Coeeficients for 1 st error rule
    lasso.coef.1se[lasso.coef.1se!=0]
    
    
    ##########################################
    ############### LASSO with 1SE ###########
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
    
    nvarmse <- sum(lasso.coef.lmse!=0)
    nvar1se <- sum(lasso.coef.1se != 0)
    
    results$MSE = TestMseWithLambaOfLowTrainMse
    results$SE = TestMseWithLambaOfOneStandardErrorTrainMse
    results$NV = nvarmse
    results$NV1 = nvar1se
    
    print(c ("Lowest MSE:" , TestMseWithLambaOfLowTrainMse , "Number of Variables:" , nvarmse  ))
    print(c ("1 Standard Error :" , TestMseWithLambaOfOneStandardErrorTrainMse , "Number of Variables:" , nvarmse  ))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste('LASSO time',time.taken))
    
  }
}