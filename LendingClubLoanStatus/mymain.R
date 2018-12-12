library(xgboost)
library(glmnet)


DataClean <- function(data){
  rownames(data) = data$id
  
  # loan_amnt
  data$loan_amnt = log(data$loan_amnt)
  
  # term
  data$term = ifelse(data$term == "36 months",36,60)
  
  #int_rate
  #installment
  #grade
  #sub_grade
  #emp_title: removed
  
  # emp_length
  data$emp_length = as.character(data$emp_length)
  data$emp_length[is.na(data$emp_length)] = "0 years"
  data$emp_length[data$emp_length == "< 1 year"] = "0 years"
  data$emp_length[data$emp_length == "10+ years"] = "10 years"
  data$emp_length = as.numeric(matrix(unlist(strsplit(data$emp_length, " ")), ncol = 2, byrow = TRUE)[, 1])
  
  # home_ownership
  data$home_ownership[data$home_ownership == "ANY" | data$home_ownership == "NONE"] = "OTHER"
  
  # anual_inc
  data$annual_inc[data$annual_inc < 500] = 500
  data$annual_inc = log(data$annual_inc)
  
  # loan status
  # data$loan_status = ifelse(data$loan_status == "Fully Paid",1,0)
  
  # title: removed
  # zipcode: removed
  # addr_state: removed
  
  # dti
  data$dti[is.na(data$dti)] = 0
  
  # earliest_cr_line
  data$earliest_cr_line = as.numeric(matrix(unlist(strsplit(as.character(data$earliest_cr_line), "-")), ncol = 2, byrow = TRUE)[ ,2])
  
  #fico
  data$fico = (data$fico_range_low + data$fico_range_high) / 2
  
  # revol_util
  data$revol_util[is.na(data$revol_util)] = 0
  
  # mort_acc
  data$mort_acc[is.na(data$mort_acc)] = 0
  
  # pub_rec_bankruptcies
  data$pub_rec_bankruptcies[is.na(data$pub_rec_bankruptcies)] = 0
  
  # Remove emp_title
  data=subset(data,select = -c(id,title,emp_title,zip_code,fico_range_low,fico_range_high, grade,application_type,initial_list_status,addr_state))
  
  return(data)
}

DataFrameToNumericMatrix <- function(data){
  m = matrix(0, nrow = nrow(data), ncol = ncol(data))
  for (i in 1:ncol(data)) {
    m[,i] = as.numeric(data[,i])
  }
  return(m)
}

logLoss = function(y, p){
  if (length(p) != length(y)){
    stop('Lengths of prediction and labels do not match.')
  }
  
  if (any(p < 0)){
    stop('Negative probability provided.')
  }
  
  p = pmax(pmin(p, 1 - 10^(-15)), 10^(-15))
  mean(ifelse(y == 1, -log(p), -log(1 - p)))
}


DataTrain <- read.csv(paste0("train.csv"))
DataTrain = DataClean(DataTrain)
DataTrain$loan_status = ifelse(DataTrain$loan_status == 'Fully Paid', 0, 1)
DataTrainMatrix = DataFrameToNumericMatrix(subset(DataTrain,select = -c(loan_status)))
DataTrainLabel = DataTrain$loan_status
  
DataTest <- read.csv(paste0("test.csv"))
TestID = DataTest$id
DataTest = DataClean(DataTest)
DataTestMatrix = DataFrameToNumericMatrix(DataTest)

  
lgm.model = glm(loan_status ~ ., data = DataTrain, family = "binomial")
LgmPrediction = predict(lgm.model,DataTest, type="response")
result1 = cbind(TestID,LgmPrediction)
write.table(result1,file = "mysubmission1.txt",col.names = c("id","prob"),row.names = FALSE,sep = ',',quote = FALSE)
  
lasso.model = cv.glmnet(DataTrainMatrix,DataTrain$loan_status, family = "binomial")
LassoPrediction = predict(lasso.model,s = lasso.model$lambda.min,newx = DataTestMatrix,type="response")
result2 = cbind(TestID,LassoPrediction)
write.table(result2,file = "mysubmission2.txt",col.names = c("id","prob"),row.names = FALSE,sep = ',',quote = FALSE)
  
xgb.model = xgboost(data=DataTrainMatrix,label = DataTrain$loan_status, objective = "binary:logistic",eval_metric = "logloss",eta = 0.15,nrounds = 200)
XgboostPrediction = predict(xgb.model, DataTestMatrix, type="response")
result3 = cbind(TestID,XgboostPrediction)
write.table(result3,file = "mysubmission3.txt",col.names = c("id","prob"),row.names = FALSE,sep = ',',quote = FALSE)
