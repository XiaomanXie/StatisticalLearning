rm(list=ls())

data_split = function(data.all, data.test.id){
  data.test = data.all[data.all[,1] %in% data.test.id,]
  data.test$loan_status = ifelse(data.test$loan_status == 'Fully Paid', 0, 1)
  # data.test = data.all[data.test.id,]
  data.test.value = subset(data.test, select = -loan_status)
  data.test.label = subset(data.test, select = c(id,loan_status))
  data.train = data.all[!(data.all[,1] %in% data.test.id), ]
  # data.train = data.all[-data.test.id,]
  
  write.csv(data.train, paste0("train-", i, ".csv"), row.names=FALSE)
  write.csv(data.test.value, paste0("test-", i, ".csv"), row.names=FALSE)
  write.csv(data.test.label, paste0("label-", i, ".csv"), row.names=FALSE,sep = ',')
}


data.all = read.csv("loan_stat542.csv")

#data.all$loan_status <- ifelse(data.all$loan_status == 'Fully Paid', 0, 1)

Project3_test_id = read.csv("Project3_test_id.csv")

for (i in 1:ncol(Project3_test_id)) {
  data.test.id = Project3_test_id[,i]
  data_split(data.all, data.test.id)
  
}