#Setting working directory

setwd("C:\\Users\\rishabh.parihar\\Documents\\Churn_prediction_hack")

#Loading required libraries

library(mlr)
library(data.table)
library(h2o)
library(mlr)
library(dummies)

#Reading train and test datasets.

train = fread("train.csv" , stringsAsFactors = TRUE , na.strings = c(" " , "NA" , "missing" , "missings" , "NULL"))
test = fread("test.csv" , stringsAsFactors = TRUE , na.strings = c(" " , "NA" , "missing" , "missings" , "NULL"))

###Code to check variables having misssing values.

missing_count = function(x){
  missing_cnt = data.frame(variable = colnames(x))
  missing_cnt$missing_count = numeric(length = ncol(x))
  missing_cnt$class = numeric(length = ncol(x))
  missing_cnt$frac_missing = numeric(length = ncol(x))

for (i in (1:nrow(missing_cnt)))
{
  
  missing_cnt$missing_count[i] = sum(is.na(x[[i]]))
  missing_cnt$class[i] = class(x[[i]])
  missing_cnt$frac_missing[i] = missing_cnt$missing_count[i]/nrow(x)

}
return(missing_cnt[missing_cnt$missing_count > 0 , ])
}

var_rem = c("ATM_amt_prev1" , "ATM_CW_Amt_prev1" , "ATM_CW_Cnt_prev1" , "BRN_CW_Amt_prev1" , "BRN_CW_Cnt_prev1" , "BRN_CASH_Dep_Amt_prev1" , "BRN_CASH_Dep_Cnt_prev1" , 
            "ATM_amt_prev2" , "ATM_CW_Amt_prev2" ,  "ATM_CW_Cnt_prev2" , "BRN_CW_Amt_prev2" , "BRN_CW_Cnt_prev2" , "BRN_CASH_Dep_Amt_prev2" , "BRN_CASH_Dep_Cnt_prev2" , 
            "ATM_amt_prev3" , "ATM_CW_Amt_prev3" , 	"ATM_CW_Cnt_prev3" , "BRN_CW_Amt_prev3"	 , "BRN_CW_Cnt_prev3" , "BRN_CASH_Dep_Amt_prev3" , "BRN_CASH_Dep_Cnt_prev3" , 
            "ATM_amt_prev4" , "ATM_CW_Amt_prev4" , 	"ATM_CW_Cnt_prev4" , "BRN_CW_Amt_prev4"	 , "BRN_CW_Cnt_prev4" , "BRN_CASH_Dep_Amt_prev4" , "BRN_CASH_Dep_Cnt_prev4" , 
            "ATM_amt_prev5" , "ATM_CW_Amt_prev5" , 	"ATM_CW_Cnt_prev5" , "BRN_CW_Amt_prev5"	 , "BRN_CW_Cnt_prev5" , "BRN_CASH_Dep_Amt_prev5" , "BRN_CASH_Dep_Cnt_prev5" , 
            "ATM_amt_prev6" , "ATM_CW_Amt_prev6" , 	"ATM_CW_Cnt_prev6" , "BRN_CW_Amt_prev6"	 , "BRN_CW_Cnt_prev6" , "BRN_CASH_Dep_Amt_prev6" , "BRN_CASH_Dep_Cnt_prev6")

train = train[ ,-c("ATM_amt_prev1" , "ATM_CW_Amt_prev1" , "ATM_CW_Cnt_prev1" , "BRN_CW_Amt_prev1" , "BRN_CW_Cnt_prev1" , "BRN_CASH_Dep_Amt_prev1" , "BRN_CASH_Dep_Cnt_prev1" , 
                     "ATM_amt_prev2" , "ATM_CW_Amt_prev2" ,  "ATM_CW_Cnt_prev2" , "BRN_CW_Amt_prev2" , "BRN_CW_Cnt_prev2" , "BRN_CASH_Dep_Amt_prev2" , "BRN_CASH_Dep_Cnt_prev2" , 
                     "ATM_amt_prev3" , "ATM_CW_Amt_prev3" , 	"ATM_CW_Cnt_prev3" , "BRN_CW_Amt_prev3"	 , "BRN_CW_Cnt_prev3" , "BRN_CASH_Dep_Amt_prev3" , "BRN_CASH_Dep_Cnt_prev3" , 
                     "ATM_amt_prev4" , "ATM_CW_Amt_prev4" , 	"ATM_CW_Cnt_prev4" , "BRN_CW_Amt_prev4"	 , "BRN_CW_Cnt_prev4" , "BRN_CASH_Dep_Amt_prev4" , "BRN_CASH_Dep_Cnt_prev4" , 
                     "ATM_amt_prev5" , "ATM_CW_Amt_prev5" , 	"ATM_CW_Cnt_prev5" , "BRN_CW_Amt_prev5"	 , "BRN_CW_Cnt_prev5" , "BRN_CASH_Dep_Amt_prev5" , "BRN_CASH_Dep_Cnt_prev5" , 
                     "ATM_amt_prev6" , "ATM_CW_Amt_prev6" , 	"ATM_CW_Cnt_prev6" , "BRN_CW_Amt_prev6"	 , "BRN_CW_Cnt_prev6" , "BRN_CASH_Dep_Amt_prev6" , "BRN_CASH_Dep_Cnt_prev6")]

test = test[ ,-c("ATM_amt_prev1" , "ATM_CW_Amt_prev1" , "ATM_CW_Cnt_prev1" , "BRN_CW_Amt_prev1" , "BRN_CW_Cnt_prev1" , "BRN_CASH_Dep_Amt_prev1" , "BRN_CASH_Dep_Cnt_prev1" , 
                   "ATM_amt_prev2" , "ATM_CW_Amt_prev2" ,  "ATM_CW_Cnt_prev2" , "BRN_CW_Amt_prev2" , "BRN_CW_Cnt_prev2" , "BRN_CASH_Dep_Amt_prev2" , "BRN_CASH_Dep_Cnt_prev2" , 
                   "ATM_amt_prev3" , "ATM_CW_Amt_prev3" , 	"ATM_CW_Cnt_prev3" , "BRN_CW_Amt_prev3"	 , "BRN_CW_Cnt_prev3" , "BRN_CASH_Dep_Amt_prev3" , "BRN_CASH_Dep_Cnt_prev3" , 
                   "ATM_amt_prev4" , "ATM_CW_Amt_prev4" , 	"ATM_CW_Cnt_prev4" , "BRN_CW_Amt_prev4"	 , "BRN_CW_Cnt_prev4" , "BRN_CASH_Dep_Amt_prev4" , "BRN_CASH_Dep_Cnt_prev4" , 
                   "ATM_amt_prev5" , "ATM_CW_Amt_prev5" , 	"ATM_CW_Cnt_prev5" , "BRN_CW_Amt_prev5"	 , "BRN_CW_Cnt_prev5" , "BRN_CASH_Dep_Amt_prev5" , "BRN_CASH_Dep_Cnt_prev5" , 
                   "ATM_amt_prev6" , "ATM_CW_Amt_prev6" , 	"ATM_CW_Cnt_prev6" , "BRN_CW_Amt_prev6"	 , "BRN_CW_Cnt_prev6" , "BRN_CASH_Dep_Amt_prev6" , "BRN_CASH_Dep_Cnt_prev6")]

#treating city variable.

levels(train$city)[1] = "Other"
levels(test$city)[levels(test$city) == 'X'] = "Other"
levels(test$city)[1] = "Other"

#treating email_unsubscribe variable.

levels(train$EMAIL_UNSUBSCRIBE)[1] = "N"
levels(test$EMAIL_UNSUBSCRIBE)[1] = "N"

##treating missing values in occup_all_new variable.

levels(train$OCCUP_ALL_NEW)[1] = "MISSING"
levels(test$OCCUP_ALL_NEW)[1] = "MISSING"

##treating missing values in dependents variable.

train$dependents[is.na(train$dependents)] = 0
test$dependents[is.na(test$dependents)] = 0

##treating missing values in engagement_tag variable.

levels(train$ENGAGEMENT_TAG_prev1)[1] = "NO"
levels(test$ENGAGEMENT_TAG_prev1)[1] = "NO"

##treating missing values in final_worth variable.

levels(train$FINAL_WORTH_prev1)[1] = "OTHER"
levels(test$FINAL_WORTH_prev1)[1] = "OTHER"

#Imputing missing values for var_c and var_c_cnt.

var_C = c("custinit_CR_amt_prev1" , "custinit_CR_amt_prev2" , "custinit_CR_amt_prev3" , "custinit_CR_amt_prev4" , 
        "custinit_CR_amt_prev5" , "custinit_CR_amt_prev6")
branch_C = c("BRANCH_C_prev1" , "BRANCH_C_prev2" , "BRANCH_C_prev3" , "BRANCH_C_prev4" , "BRANCH_C_prev5" , 
          "BRANCH_C_prev6")
IB_C = c("IB_C_prev1" , "IB_C_prev2" , "IB_C_prev3" , "IB_C_prev4" , "IB_C_prev5" , "IB_C_prev6")

var_C_cnt = c("custinit_CR_cnt_prev1" , "custinit_CR_cnt_prev2" , "custinit_CR_cnt_prev3" , "custinit_CR_cnt_prev4" , 
          "custinit_CR_cnt_prev5" , "custinit_CR_cnt_prev6")
branch_C_cnt = c("COUNT_BRANCH_C_prev1" , "COUNT_BRANCH_C_prev2" , "COUNT_BRANCH_C_prev3" , "COUNT_BRANCH_C_prev4" ,
                 "COUNT_BRANCH_C_prev5" , "COUNT_BRANCH_C_prev6")
IB_C_cnt = c("COUNT_IB_C_prev1" , "COUNT_IB_C_prev2" , "COUNT_IB_C_prev3" , "COUNT_IB_C_prev4" , "COUNT_IB_C_prev5" , 
             "COUNT_IB_C_prev6")

for (i in 1:6){
  
  train[[var_C[i]]] = ifelse(is.na(train[[var_C[i]]]) , (train[[branch_C[i]]] + train[[IB_C[i]]]) , train[[var_C[i]]])
  test[[var_C[i]]] = ifelse(is.na(test[[var_C[i]]]) , (test[[branch_C[i]]] + test[[IB_C[i]]]) , test[[var_C[i]]])
  
  train[[var_C_cnt[i]]] = ifelse(is.na(train[[var_C_cnt[i]]]) , (train[[branch_C_cnt[i]]] + train[[IB_C_cnt[i]]]) , train[[var_C_cnt[i]]])
  test[[var_C_cnt[i]]] = ifelse(is.na(test[[var_C_cnt[i]]]) , (test[[branch_C_cnt[i]]] + test[[IB_C_cnt[i]]]) , test[[var_C_cnt[i]]])

}

#Imputing missing values for var_d and var_d_cnt.

var_D = c("custinit_DR_amt_prev1" , "custinit_DR_amt_prev2" , "custinit_DR_amt_prev3" , "custinit_DR_amt_prev4" , 
          "custinit_DR_amt_prev5" , "custinit_DR_amt_prev6")
branch_D = c("BRANCH_D_prev1" , "BRANCH_D_prev2" , "BRANCH_D_prev3" , "BRANCH_D_prev4" , "BRANCH_D_prev5" , 
             "BRANCH_D_prev6")
IB_D = c("IB_D_prev1" , "IB_D_prev2" , "IB_D_prev3" , "IB_D_prev4" , "IB_D_prev5" , "IB_D_prev6")

MB_D = c("MB_D_prev1" , "MB_D_prev2" , "MB_D_prev3" , "MB_D_prev4" , "MB_D_prev5" , "MB_D_prev6")

POS_D = c("POS_D_prev1" , "POS_D_prev2" , "POS_D_prev3" , "POS_D_prev4" , "POS_D_prev5" , "POS_D_prev6")

ATM_D = c("ATM_D_prev1" , "ATM_D_prev2" , "ATM_D_prev3" , "ATM_D_prev4" , "ATM_D_prev5" , "ATM_D_prev6")

var_D_cnt = c("custinit_DR_cnt_prev1" , "custinit_DR_cnt_prev2" , "custinit_DR_cnt_prev3" , "custinit_DR_cnt_prev4" , 
              "custinit_DR_cnt_prev5" , "custinit_DR_cnt_prev6")

branch_D_cnt = c("COUNT_BRANCH_D_prev1" , "COUNT_BRANCH_D_prev2" , "COUNT_BRANCH_D_prev3" , "COUNT_BRANCH_D_prev4" ,
                 "COUNT_BRANCH_D_prev5" , "COUNT_BRANCH_D_prev6")

IB_D_cnt = c("COUNT_IB_D_prev1" , "COUNT_IB_D_prev2" , "COUNT_IB_D_prev3" , "COUNT_IB_D_prev4" , "COUNT_IB_D_prev5" , 
             "COUNT_IB_D_prev6")

MB_D_cnt = c("COUNT_MB_D_prev1" , "COUNT_MB_D_prev2" , "COUNT_MB_D_prev3" , "COUNT_MB_D_prev4" , "COUNT_MB_D_prev5" , "COUNT_MB_D_prev6")

POS_D_cnt = c("COUNT_POS_D_prev1" , "COUNT_POS_D_prev2" , "COUNT_POS_D_prev3" , "COUNT_POS_D_prev4" , "COUNT_POS_D_prev5" , "COUNT_POS_D_prev6")

ATM_D_cnt = c("COUNT_ATM_D_prev1" , "COUNT_ATM_D_prev2" , "COUNT_ATM_D_prev3" , "COUNT_ATM_D_prev4" , "COUNT_ATM_D_prev5" , "COUNT_ATM_D_prev6")

for (i in 1:6){
  
  train[[var_D[i]]] = ifelse(is.na(train[[var_D[i]]]) , (train[[branch_D[i]]] + train[[IB_D[i]]] + train[[MB_D[i]]] + train[[POS_D[i]]] + train[[ATM_D[i]]]) , 
                             train[[var_D[i]]])
  
  test[[var_D[i]]] = ifelse(is.na(test[[var_D[i]]]) , (test[[branch_D[i]]] + test[[IB_D[i]]] + test[[MB_D[i]]] + test[[POS_D[i]]] + test[[ATM_D[i]]]) ,
                            test[[var_D[i]]])
  
  train[[var_D_cnt[i]]] = ifelse(is.na(train[[var_D_cnt[i]]]) , (train[[branch_D_cnt[i]]] + train[[IB_D_cnt[i]]] + train[[MB_D_cnt[i]]] 
                          + train[[POS_D_cnt[i]]] + train[[ATM_D_cnt[i]]]) , train[[var_D_cnt[i]]])
  
  test[[var_D_cnt[i]]] = ifelse(is.na(test[[var_D_cnt[i]]]) , (test[[branch_D_cnt[i]]] + test[[IB_D_cnt[i]]] + test[[MB_D_cnt[i]]] 
                          + test[[POS_D_cnt[i]]] + test[[ATM_D_cnt[i]]]) , test[[var_D_cnt[i]]])
  
}

missing_count(train)

##Imputing zip variable.

train$zip[is.na(train$zip)] = 122
test$zip[is.na(test$zip)] = 122

####Imputing frx_prevq1 variable.

train$FRX_PrevQ1[is.na(train$FRX_PrevQ1)] = 0
test$FRX_PrevQ1[is.na(test$FRX_PrevQ1)] = 0

##Function to count missing values in a column.

no_miss = function(x){
  return(sum(is.na(x)))
}

train$Billpay_Active_PrevQ1[is.na(train$Billpay_Active_PrevQ1)] = 0
test$Billpay_Active_PrevQ1[is.na(test$Billpay_Active_PrevQ1)] = 0


train$Billpay_Reg_ason_Prev1[is.na(train$Billpay_Reg_ason_Prev1)] = 0
test$Billpay_Reg_ason_Prev1[is.na(test$Billpay_Reg_ason_Prev1)] = 0

df = missing_count(train)
zero_impute_cols = df$variable[1:37]

for (i in (zero_impute_cols)){
  train[[i]][is.na(train[[i]])] = 0
}

for (i in (zero_impute_cols)){
  test[[i]][is.na(test[[i]])] = 0
}

train = train[ , -c("AGRI_DATE" , "AL_CNC_DATE" , "AL_DATE" , "BL_DATE" , "CE_DATE" , "CV_DATE" , "EDU_DATE" , "GL_DATE" , "LAP_DATE" ,
"LAS_DATE" , "OTHER_LOANS_DATE" , "PL_DATE" , "TL_DATE" , "TWL_DATE")]

test = test[ , -c("AGRI_DATE" , "AL_CNC_DATE" , "AL_DATE" , "BL_DATE" , "CE_DATE" , "CV_DATE" , "EDU_DATE" , "GL_DATE" , "LAP_DATE" ,
                    "LAS_DATE" , "OTHER_LOANS_DATE" , "PL_DATE" , "TL_DATE" , "TWL_DATE")]


##Imputing tag_variables.

tag_vars = c("AGRI_TAG_LIVE" , "AL_CNC_TAG_LIVE" , "AL_TAG_LIVE" , "BL_TAG_LIVE" , "CC_TAG_LIVE" , "CE_TAG_LIVE" , "CV_TAG_LIVE" ,
             "DEMAT_TAG_LIVE" , "EDU_TAG_LIVE" , "GL_TAG_LIVE" , "HL_TAG_LIVE" , "SEC_ACC_TAG_LIVE" , "INS_TAG_LIVE" , "LAS_TAG_LIVE"	 ,
             "MF_TAG_LIVE" , "OTHER_LOANS_TAG_LIVE" , "PL_TAG_LIVE" , "RD_TAG_LIVE" , "FD_TAG_LIVE" , "TL_TAG_LIVE" , "TWL_TAG_LIVE" ,
             "lap_tag_live")

for (i in tag_vars){
  levels(train[[i]])[1] = 'N'
  levels(test[[i]])[1] = 'N'
}

##Imputing recency variables.

recency_vars = c("Recency_of_CR_TXN" , "Recency_of_DR_TXN" , "Recency_of_IB_TXN" , "Recency_of_ATM_TXN" ,
         "Recency_of_BRANCH_TXN" , "Recency_of_POS_TXN" , "Recency_of_MB_TXN" , "Recency_of_Activity")

train$Recency_of_CR_TXN[is.na(train$Recency_of_CR_TXN)] = ifelse(train$count_C_prev1 != 0 , 15 , 
ifelse(train$count_C_prev2 != 0 , 45 , ifelse(train$count_C_prev3 != 0 , 75 , 100)))

train$Recency_of_DR_TXN[is.na(train$Recency_of_DR_TXN)] = ifelse(train$count_D_prev1 != 0 , 15 , 
ifelse(train$count_D_prev2 != 0 , 45 , ifelse(train$count_D_prev3 != 0 , 75 , 100)))

train$Recency_of_ATM_TXN[is.na(train$Recency_of_ATM_TXN)] = ifelse(train$ATM_C_prev1 != 0 , 15 , 
ifelse(train$ATM_C_prev2 != 0 , 45 , ifelse(train$ATM_C_prev3 != 0 , 75 , 100)))

train$Recency_of_IB_TXN[is.na(train$Recency_of_IB_TXN)] = 100
train$Recency_of_BRANCH_TXN[is.na(train$Recency_of_BRANCH_TXN)] = 100
train$Recency_of_POS_TXN[is.na(train$Recency_of_POS_TXN)] = 100
train$Recency_of_MB_TXN[is.na(train$Recency_of_MB_TXN)] = 100
train$Recency_of_Activity[is.na(train$Recency_of_Activity)] = 100

test$Recency_of_CR_TXN[is.na(test$Recency_of_CR_TXN)] = ifelse(test$count_C_prev1 != 0 , 15 , 
                                                               ifelse(test$count_C_prev2 != 0 , 45 , ifelse(test$count_C_prev3 != 0 , 75 , 100)))

test$Recency_of_DR_TXN[is.na(test$Recency_of_DR_TXN)] = ifelse(test$count_D_prev1 != 0 , 15 , 
                                                               ifelse(test$count_D_prev2 != 0 , 45 , ifelse(test$count_D_prev3 != 0 , 75 , 100)))

test$Recency_of_ATM_TXN[is.na(test$Recency_of_ATM_TXN)] = ifelse(test$ATM_C_prev1 != 0 , 15 , 
                                                                 ifelse(test$ATM_C_prev2 != 0 , 45 , ifelse(test$ATM_C_prev3 != 0 , 75 , 100)))

test$Recency_of_IB_TXN[is.na(test$Recency_of_IB_TXN)] = 100
test$Recency_of_BRANCH_TXN[is.na(test$Recency_of_BRANCH_TXN)] = 100
test$Recency_of_POS_TXN[is.na(test$Recency_of_POS_TXN)] = 100
test$Recency_of_MB_TXN[is.na(test$Recency_of_MB_TXN)] = 100
test$Recency_of_Activity[is.na(test$Recency_of_Activity)] = 100

train[is.na(train)] = 0
test[is.na(test)] = 0

#Complaint_Resolved_PrevQ1

levels(train$Complaint_Resolved_PrevQ1)[1] = 0
train$Complaint_Resolved_PrevQ1[train$Complaint_Resolved_PrevQ1 == '>'] = 0
train$Complaint_Resolved_PrevQ1 = as.integer(substr(as.character(train$Complaint_Resolved_PrevQ1) , 1 , 1))

levels(test$Complaint_Resolved_PrevQ1)[1] = 0
test$Complaint_Resolved_PrevQ1[test$Complaint_Resolved_PrevQ1 == '>'] = 0
test$Complaint_Resolved_PrevQ1 = as.integer(substr(as.character(test$Complaint_Resolved_PrevQ1) , 1 , 1))

#"Query_Resolved_PrevQ1"

levels(train$Query_Resolved_PrevQ1)[1] = 0
train$Query_Resolved_PrevQ1[train$Query_Resolved_PrevQ1 == '>'] = 0
train$Query_Resolved_PrevQ1 = as.integer(substr(as.character(train$Query_Resolved_PrevQ1) , 1 , 1))

levels(test$Query_Resolved_PrevQ1)[1] = 0
test$Query_Resolved_PrevQ1[test$Query_Resolved_PrevQ1 == '>'] = 0
test$Query_Resolved_PrevQ1 = as.integer(substr(as.character(test$Query_Resolved_PrevQ1) , 1 , 1))


#"Req_Resolved_PrevQ1"

levels(train$Req_Resolved_PrevQ1)[1] = 0
train$Req_Resolved_PrevQ1[train$Req_Resolved_PrevQ1 == '>'] = 0
train$Req_Resolved_PrevQ1 = as.integer(substr(as.character(train$Req_Resolved_PrevQ1) , 1 , 1))

levels(test$Req_Resolved_PrevQ1)[1] = 0
test$Req_Resolved_PrevQ1[test$Req_Resolved_PrevQ1 == '>'] = 0
test$Req_Resolved_PrevQ1 = as.integer(substr(as.character(test$Req_Resolved_PrevQ1) , 1 , 1))

train_final = train[ , -c("UCIC_ID" , "city" , "zip" , "brn_code")]
test_final = test[ , -c("UCIC_ID" , "city" , "zip" , "brn_code")]
test_final$Responders = sample(0:1 , nrow(test_final) , TRUE)

#rm(train , test) #save memory



##Code to separate factors and numeric variables.

write.csv(data.frame(variable = colnames(train_final)) , "var.csv" , row.names = FALSE)

var_typ = read.csv("var.csv")

fac_vars = var_typ$variable[var_typ$type == 'fac']
fac_vars = as.vector(fac_vars)

num_vars = setdiff(colnames(train_final) , fac_vars)

##Converting to dataframes.

train_final = as.data.frame(train_final)
test_final = as.data.frame(test_final)

##Converting factors to numeric.

for (i in fac_vars){
  train_final[[i]] = as.numeric(train_final[[i]])
  test_final[[i]] = as.numeric(test_final[[i]])
}

## Converting target variable to numeric variable.

train_final$Responders = as.factor(train_final$Responders)
test_final$Responders = as.factor(test_final$Responders)
#sapply(train_final , class)

##Creating Credit ratio variables.

train_final$Cr_prev_1_frac = ifelse(train_final$C_prev2 != 0 , train_final$C_prev1/train_final$C_prev2 ,
                                     ifelse((train_final$C_prev1 == 0) &(train_final$C_prev2 == 0) , -1 , -100))

test_final$Cr_prev_1_frac = ifelse(test_final$C_prev2 != 0 , test_final$C_prev1/test_final$C_prev2 ,
                                   ifelse((test_final$C_prev1 == 0) &(test_final$C_prev2 == 0) , -1 , -100))

train_final$Cr_prev_2_frac = ifelse(train_final$C_prev3 != 0 , train_final$C_prev2/train_final$C_prev3 ,
                                    ifelse((train_final$C_prev2 == 0) &(train_final$C_prev3 == 0) , -1 , -100))

test_final$Cr_prev_2_frac = ifelse(test_final$C_prev3 != 0 , test_final$C_prev2/test_final$C_prev3 ,
                                   ifelse((test_final$C_prev2 == 0) &(test_final$C_prev3 == 0) , -1 , -100))

train_final$Cr_prev_3_frac = ifelse(train_final$C_prev4 != 0 , train_final$C_prev3/train_final$C_prev4 ,
                                    ifelse((train_final$C_prev4 == 0) &(train_final$C_prev3 == 0) , -1 , -100))

test_final$Cr_prev_3_frac = ifelse(test_final$C_prev4 != 0 , test_final$C_prev3/test_final$C_prev4 ,
                                   ifelse((test_final$C_prev3 == 0) &(test_final$C_prev4 == 0) , -1 , -100))

train_final$Cr_prev_4_frac = ifelse(train_final$C_prev5 != 0 , train_final$C_prev4/train_final$C_prev5 ,
                                    ifelse((train_final$C_prev4 == 0) &(train_final$C_prev5 == 0) , -1 , -100))

test_final$Cr_prev_4_frac = ifelse(test_final$C_prev5 != 0 , test_final$C_prev4/test_final$C_prev5 ,
                                   ifelse((test_final$C_prev4 == 0) &(test_final$C_prev5 == 0) , -1 , -100))

train_final$Cr_prev_5_frac = ifelse(train_final$C_prev6 != 0 , train_final$C_prev5/train_final$C_prev6 ,
                                    ifelse((train_final$C_prev5 == 0) &(train_final$C_prev6 == 0) , -1 , -100))

test_final$Cr_prev_5_frac = ifelse(test_final$C_prev6 != 0 , test_final$C_prev5/test_final$C_prev6 ,
                                   ifelse((test_final$C_prev5 == 0) &(test_final$C_prev6 == 0) , -1 , -100))

##Creating Debit ratio variables.

train_final$Dr_prev_1_frac = ifelse(train_final$D_prev2 != 0 , train_final$D_prev1/train_final$D_prev2 ,
                                    ifelse((train_final$D_prev1 == 0) &(train_final$D_prev2 == 0) , -1 , -100))

test_final$Dr_prev_1_frac = ifelse(test_final$D_prev2 != 0 , test_final$D_prev1/test_final$D_prev2 ,
                                   ifelse((test_final$D_prev1 == 0) &(test_final$D_prev2 == 0) , -1 , -100))

train_final$Dr_prev_2_frac = ifelse(train_final$D_prev3 != 0 , train_final$D_prev2/train_final$D_prev3 ,
                                    ifelse((train_final$D_prev2 == 0) &(train_final$D_prev3 == 0) , -1 , -100))

test_final$Dr_prev_2_frac = ifelse(test_final$D_prev3 != 0 , test_final$D_prev2/test_final$D_prev3 ,
                                   ifelse((test_final$D_prev2 == 0) &(test_final$D_prev3 == 0) , -1 , -100))

train_final$Dr_prev_3_frac = ifelse(train_final$D_prev4 != 0 , train_final$D_prev3/train_final$D_prev4 ,
                                    ifelse((train_final$D_prev4 == 0) &(train_final$D_prev3 == 0) , -1 , -100))

test_final$Dr_prev_3_frac = ifelse(test_final$D_prev4 != 0 , test_final$D_prev3/test_final$D_prev4 ,
                                   ifelse((test_final$D_prev3 == 0) &(test_final$D_prev4 == 0) , -1 , -100))

train_final$Dr_prev_4_frac = ifelse(train_final$D_prev5 != 0 , train_final$D_prev4/train_final$D_prev5 ,
                                    ifelse((train_final$D_prev4 == 0) &(train_final$D_prev5 == 0) , -1 , -100))

test_final$Dr_prev_4_frac = ifelse(test_final$D_prev5 != 0 , test_final$D_prev4/test_final$D_prev5 ,
                                   ifelse((test_final$D_prev4 == 0) &(test_final$D_prev5 == 0) , -1 , -100))

train_final$Dr_prev_5_frac = ifelse(train_final$D_prev6 != 0 , train_final$D_prev5/train_final$D_prev6 ,
                                    ifelse((train_final$D_prev5 == 0) &(train_final$D_prev6 == 0) , -1 , -100))

test_final$Dr_prev_5_frac = ifelse(test_final$D_prev6 != 0 , test_final$D_prev5/test_final$D_prev6 ,
                                   ifelse((test_final$D_prev5 == 0) &(test_final$D_prev6 == 0) , -1 , -100))

##Creating Branch Credit ratio variables.

train_final$BRN_Cr_prev_1_frac = ifelse(train_final$BRANCH_C_prev2 != 0 , train_final$BRANCH_C_prev1/train_final$BRANCH_C_prev2 ,
                                        ifelse((train_final$BRANCH_C_prev1 == 0) &(train_final$BRANCH_C_prev2 == 0) , -1 , -100))

test_final$BRN_Cr_prev_1_frac = ifelse(test_final$BRANCH_C_prev2 != 0 , test_final$BRANCH_C_prev1/test_final$BRANCH_C_prev2 ,
                                       ifelse((test_final$BRANCH_C_prev1 == 0) &(test_final$BRANCH_C_prev2 == 0) , -1 , -100))

train_final$BRN_Cr_prev_2_frac = ifelse(train_final$BRANCH_C_prev3 != 0 , train_final$BRANCH_C_prev2/train_final$BRANCH_C_prev3 ,
                                        ifelse((train_final$BRANCH_C_prev2 == 0) &(train_final$BRANCH_C_prev3 == 0) , -1 , -100))

test_final$BRN_Cr_prev_2_frac = ifelse(test_final$BRANCH_C_prev3 != 0 , test_final$BRANCH_C_prev2/test_final$BRANCH_C_prev3 ,
                                       ifelse((test_final$BRANCH_C_prev2 == 0) &(test_final$BRANCH_C_prev3 == 0) , -1 , -100))

train_final$BRN_Cr_prev_3_frac = ifelse(train_final$BRANCH_C_prev4 != 0 , train_final$BRANCH_C_prev3/train_final$BRANCH_C_prev4 ,
                                        ifelse((train_final$BRANCH_C_prev4 == 0) &(train_final$BRANCH_C_prev3 == 0) , -1 , -100))

test_final$BRN_Cr_prev_3_frac = ifelse(test_final$BRANCH_C_prev4 != 0 , test_final$BRANCH_C_prev3/test_final$BRANCH_C_prev4 ,
                                       ifelse((test_final$BRANCH_C_prev3 == 0) &(test_final$BRANCH_C_prev4 == 0) , -1 , -100))

train_final$BRN_Cr_prev_4_frac = ifelse(train_final$BRANCH_C_prev5 != 0 , train_final$BRANCH_C_prev4/train_final$BRANCH_C_prev5 ,
                                        ifelse((train_final$BRANCH_C_prev4 == 0) &(train_final$BRANCH_C_prev5 == 0) , -1 , -100))

test_final$BRN_Cr_prev_4_frac = ifelse(test_final$BRANCH_C_prev5 != 0 , test_final$BRANCH_C_prev4/test_final$BRANCH_C_prev5 ,
                                       ifelse((test_final$BRANCH_C_prev4 == 0) &(test_final$BRANCH_C_prev5 == 0) , -1 , -100))

train_final$BRN_Cr_prev_5_frac = ifelse(train_final$BRANCH_C_prev6 != 0 , train_final$BRANCH_C_prev5/train_final$BRANCH_C_prev6 ,
                                        ifelse((train_final$BRANCH_C_prev5 == 0) &(train_final$BRANCH_C_prev6 == 0) , -1 , -100))

test_final$BRN_Cr_prev_5_frac = ifelse(test_final$BRANCH_C_prev6 != 0 , test_final$BRANCH_C_prev5/test_final$BRANCH_C_prev6 ,
                                       ifelse((test_final$BRANCH_C_prev5 == 0) &(test_final$BRANCH_C_prev6 == 0) , -1 , -100))

##Creating Branch Debit ratio variables.

train_final$BRN_Dr_prev_1_frac = ifelse(train_final$BRANCH_D_prev2 != 0 , train_final$BRANCH_D_prev1/train_final$BRANCH_D_prev2 ,
                                        ifelse((train_final$BRANCH_D_prev1 == 0) &(train_final$BRANCH_D_prev2 == 0) , -1 , -100))

test_final$BRN_Dr_prev_1_frac = ifelse(test_final$BRANCH_D_prev2 != 0 , test_final$BRANCH_D_prev1/test_final$BRANCH_D_prev2 ,
                                       ifelse((test_final$BRANCH_D_prev1 == 0) &(test_final$BRANCH_D_prev2 == 0) , -1 , -100))

train_final$BRN_Dr_prev_2_frac = ifelse(train_final$BRANCH_D_prev3 != 0 , train_final$BRANCH_D_prev2/train_final$BRANCH_D_prev3 ,
                                        ifelse((train_final$BRANCH_D_prev2 == 0) &(train_final$BRANCH_D_prev3 == 0) , -1 , -100))

test_final$BRN_Dr_prev_2_frac = ifelse(test_final$BRANCH_D_prev3 != 0 , test_final$BRANCH_D_prev2/test_final$BRANCH_D_prev3 ,
                                       ifelse((test_final$BRANCH_D_prev2 == 0) &(test_final$BRANCH_D_prev3 == 0) , -1 , -100))

train_final$BRN_Dr_prev_3_frac = ifelse(train_final$BRANCH_D_prev4 != 0 , train_final$BRANCH_D_prev3/train_final$BRANCH_D_prev4 ,
                                        ifelse((train_final$BRANCH_D_prev4 == 0) &(train_final$BRANCH_D_prev3 == 0) , -1 , -100))

test_final$BRN_Dr_prev_3_frac = ifelse(test_final$BRANCH_D_prev4 != 0 , test_final$BRANCH_D_prev3/test_final$BRANCH_D_prev4 ,
                                       ifelse((test_final$BRANCH_D_prev3 == 0) &(test_final$BRANCH_D_prev4 == 0) , -1 , -100))

train_final$BRN_Dr_prev_4_frac = ifelse(train_final$BRANCH_D_prev5 != 0 , train_final$BRANCH_D_prev4/train_final$BRANCH_D_prev5 ,
                                        ifelse((train_final$BRANCH_D_prev4 == 0) &(train_final$BRANCH_D_prev5 == 0) , -1 , -100))

test_final$BRN_Dr_prev_4_frac = ifelse(test_final$BRANCH_D_prev5 != 0 , test_final$BRANCH_D_prev4/test_final$BRANCH_D_prev5 ,
                                       ifelse((test_final$BRANCH_D_prev4 == 0) &(test_final$BRANCH_D_prev5 == 0) , -1 , -100))

train_final$BRN_Dr_prev_5_frac = ifelse(train_final$BRANCH_D_prev6 != 0 , train_final$BRANCH_D_prev5/train_final$BRANCH_D_prev6 ,
                                        ifelse((train_final$BRANCH_D_prev5 == 0) &(train_final$BRANCH_D_prev6 == 0) , -1 , -100))

test_final$BRN_Dr_prev_5_frac = ifelse(test_final$BRANCH_D_prev6 != 0 , test_final$BRANCH_D_prev5/test_final$BRANCH_D_prev6 ,
                                       ifelse((test_final$BRANCH_D_prev5 == 0) &(test_final$BRANCH_D_prev6 == 0) , -1 , -100))

##Creating ATM Debit ratio variables.

train_final$ATM_Dr_prev_1_frac = ifelse(train_final$ATM_D_prev2 != 0 , train_final$ATM_D_prev1/train_final$ATM_D_prev2 ,
                                        ifelse((train_final$ATM_D_prev1 == 0) &(train_final$ATM_D_prev2 == 0) , -1 , -100))

test_final$ATM_Dr_prev_1_frac = ifelse(test_final$ATM_D_prev2 != 0 , test_final$ATM_D_prev1/test_final$ATM_D_prev2 ,
                                       ifelse((test_final$ATM_D_prev1 == 0) &(test_final$ATM_D_prev2 == 0) , -1 , -100))

train_final$ATM_Dr_prev_2_frac = ifelse(train_final$ATM_D_prev3 != 0 , train_final$ATM_D_prev2/train_final$ATM_D_prev3 ,
                                        ifelse((train_final$ATM_D_prev2 == 0) &(train_final$ATM_D_prev3 == 0) , -1 , -100))

test_final$ATM_Dr_prev_2_frac = ifelse(test_final$ATM_D_prev3 != 0 , test_final$ATM_D_prev2/test_final$ATM_D_prev3 ,
                                       ifelse((test_final$ATM_D_prev2 == 0) &(test_final$ATM_D_prev3 == 0) , -1 , -100))

train_final$ATM_Dr_prev_3_frac = ifelse(train_final$ATM_D_prev4 != 0 , train_final$ATM_D_prev3/train_final$ATM_D_prev4 ,
                                        ifelse((train_final$ATM_D_prev4 == 0) &(train_final$ATM_D_prev3 == 0) , -1 , -100))

test_final$ATM_Dr_prev_3_frac = ifelse(test_final$ATM_D_prev4 != 0 , test_final$ATM_D_prev3/test_final$ATM_D_prev4 ,
                                       ifelse((test_final$ATM_D_prev3 == 0) &(test_final$ATM_D_prev4 == 0) , -1 , -100))

train_final$ATM_Dr_prev_4_frac = ifelse(train_final$ATM_D_prev5 != 0 , train_final$ATM_D_prev4/train_final$ATM_D_prev5 ,
                                        ifelse((train_final$ATM_D_prev4 == 0) &(train_final$ATM_D_prev5 == 0) , -1 , -100))

test_final$ATM_Dr_prev_4_frac = ifelse(test_final$ATM_D_prev5 != 0 , test_final$ATM_D_prev4/test_final$ATM_D_prev5 ,
                                       ifelse((test_final$ATM_D_prev4 == 0) &(test_final$ATM_D_prev5 == 0) , -1 , -100))

train_final$ATM_Dr_prev_5_frac = ifelse(train_final$ATM_D_prev6 != 0 , train_final$ATM_D_prev5/train_final$ATM_D_prev6 ,
                                        ifelse((train_final$ATM_D_prev5 == 0) &(train_final$ATM_D_prev6 == 0) , -1 , -100))

test_final$ATM_Dr_prev_5_frac = ifelse(test_final$ATM_D_prev6 != 0 , test_final$ATM_D_prev5/test_final$ATM_D_prev6 ,
                                       ifelse((test_final$ATM_D_prev5 == 0) &(test_final$ATM_D_prev6 == 0) , -1 , -100))


##Creating POS debit variables.

train_final$POS_Dr_prev_1_frac = ifelse(train_final$POS_D_prev2 != 0 , train_final$POS_D_prev1/train_final$POS_D_prev2 ,
                                        ifelse((train_final$POS_D_prev1 == 0) &(train_final$POS_D_prev2 == 0) , -1 , -100))

test_final$POS_Dr_prev_1_frac = ifelse(test_final$POS_D_prev2 != 0 , test_final$POS_D_prev1/test_final$POS_D_prev2 ,
                                       ifelse((test_final$POS_D_prev1 == 0) &(test_final$POS_D_prev2 == 0) , -1 , -100))

train_final$POS_Dr_prev_2_frac = ifelse(train_final$POS_D_prev3 != 0 , train_final$POS_D_prev2/train_final$POS_D_prev3 ,
                                        ifelse((train_final$POS_D_prev2 == 0) &(train_final$POS_D_prev3 == 0) , -1 , -100))

test_final$POS_Dr_prev_2_frac = ifelse(test_final$POS_D_prev3 != 0 , test_final$POS_D_prev2/test_final$POS_D_prev3 ,
                                       ifelse((test_final$POS_D_prev2 == 0) &(test_final$POS_D_prev3 == 0) , -1 , -100))

train_final$POS_Dr_prev_3_frac = ifelse(train_final$POS_D_prev4 != 0 , train_final$POS_D_prev3/train_final$POS_D_prev4 ,
                                        ifelse((train_final$POS_D_prev4 == 0) &(train_final$POS_D_prev3 == 0) , -1 , -100))

test_final$POS_Dr_prev_3_frac = ifelse(test_final$POS_D_prev4 != 0 , test_final$POS_D_prev3/test_final$POS_D_prev4 ,
                                       ifelse((test_final$POS_D_prev3 == 0) &(test_final$POS_D_prev4 == 0) , -1 , -100))

train_final$POS_Dr_prev_4_frac = ifelse(train_final$POS_D_prev5 != 0 , train_final$POS_D_prev4/train_final$POS_D_prev5 ,
                                        ifelse((train_final$POS_D_prev4 == 0) &(train_final$POS_D_prev5 == 0) , -1 , -100))

test_final$POS_Dr_prev_4_frac = ifelse(test_final$POS_D_prev5 != 0 , test_final$POS_D_prev4/test_final$POS_D_prev5 ,
                                       ifelse((test_final$POS_D_prev4 == 0) &(test_final$POS_D_prev5 == 0) , -1 , -100))

train_final$POS_Dr_prev_5_frac = ifelse(train_final$POS_D_prev6 != 0 , train_final$POS_D_prev5/train_final$POS_D_prev6 ,
                                        ifelse((train_final$POS_D_prev5 == 0) &(train_final$POS_D_prev6 == 0) , -1 , -100))

test_final$POS_Dr_prev_5_frac = ifelse(test_final$POS_D_prev6 != 0 , test_final$POS_D_prev5/test_final$POS_D_prev6 ,
                                       ifelse((test_final$POS_D_prev5 == 0) &(test_final$POS_D_prev6 == 0) , -1 , -100))


##Creating IB credit variables

train_final$IB_Cr_prev_1_frac = ifelse(train_final$IB_C_prev2 != 0 , train_final$IB_C_prev1/train_final$IB_C_prev2 ,
                                       ifelse((train_final$IB_C_prev1 == 0) &(train_final$IB_C_prev2 == 0) , -1 , -100))

test_final$IB_Cr_prev_1_frac = ifelse(test_final$IB_C_prev2 != 0 , test_final$IB_C_prev1/test_final$IB_C_prev2 ,
                                      ifelse((test_final$IB_C_prev1 == 0) &(test_final$IB_C_prev2 == 0) , -1 , -100))

train_final$IB_Cr_prev_2_frac = ifelse(train_final$IB_C_prev3 != 0 , train_final$IB_C_prev2/train_final$IB_C_prev3 ,
                                       ifelse((train_final$IB_C_prev2 == 0) &(train_final$IB_C_prev3 == 0) , -1 , -100))

test_final$IB_Cr_prev_2_frac = ifelse(test_final$IB_C_prev3 != 0 , test_final$IB_C_prev2/test_final$IB_C_prev3 ,
                                      ifelse((test_final$IB_C_prev2 == 0) &(test_final$IB_C_prev3 == 0) , -1 , -100))

train_final$IB_Cr_prev_3_frac = ifelse(train_final$IB_C_prev4 != 0 , train_final$IB_C_prev3/train_final$IB_C_prev4 ,
                                       ifelse((train_final$IB_C_prev4 == 0) &(train_final$IB_C_prev3 == 0) , -1 , -100))

test_final$IB_Cr_prev_3_frac = ifelse(test_final$IB_C_prev4 != 0 , test_final$IB_C_prev3/test_final$IB_C_prev4 ,
                                      ifelse((test_final$IB_C_prev3 == 0) &(test_final$IB_C_prev4 == 0) , -1 , -100))

train_final$IB_Cr_prev_4_frac = ifelse(train_final$IB_C_prev5 != 0 , train_final$IB_C_prev4/train_final$IB_C_prev5 ,
                                       ifelse((train_final$IB_C_prev4 == 0) &(train_final$IB_C_prev5 == 0) , -1 , -100))
test_final$IB_Cr_prev_4_frac = ifelse(test_final$IB_C_prev5 != 0 , test_final$IB_C_prev4/test_final$IB_C_prev5 ,
                                         ifelse((test_final$IB_C_prev4 == 0) &(test_final$IB_C_prev5 == 0) , -1 , -100))

train_final$IB_Cr_prev_5_frac = ifelse(train_final$IB_C_prev6 != 0 , train_final$IB_C_prev5/train_final$IB_C_prev6 ,
                                       ifelse((train_final$IB_C_prev5 == 0) &(train_final$IB_C_prev6 == 0) , -1 , -100))

test_final$IB_Cr_prev_5_frac = ifelse(test_final$IB_C_prev6 != 0 , test_final$IB_C_prev5/test_final$IB_C_prev6 ,
                                      ifelse((test_final$IB_C_prev5 == 0) &(test_final$IB_C_prev6 == 0) , -1 , -100))

##Creating MB debit variables.

train_final$MB_Dr_prev_1_frac = ifelse(train_final$MB_D_prev2 != 0 , train_final$MB_D_prev1/train_final$MB_D_prev2 ,
                                       ifelse((train_final$MB_D_prev1 == 0) &(train_final$MB_D_prev2 == 0) , -1 , -100))

test_final$MB_Dr_prev_1_frac = ifelse(test_final$MB_D_prev2 != 0 , test_final$MB_D_prev1/test_final$MB_D_prev2 ,
                                      ifelse((test_final$MB_D_prev1 == 0) &(test_final$MB_D_prev2 == 0) , -1 , -100))

train_final$MB_Dr_prev_2_frac = ifelse(train_final$MB_D_prev3 != 0 , train_final$MB_D_prev2/train_final$MB_D_prev3 ,
                                       ifelse((train_final$MB_D_prev2 == 0) &(train_final$MB_D_prev3 == 0) , -1 , -100))

test_final$MB_Dr_prev_2_frac = ifelse(test_final$MB_D_prev3 != 0 , test_final$MB_D_prev2/test_final$MB_D_prev3 ,
                                      ifelse((test_final$MB_D_prev2 == 0) &(test_final$MB_D_prev3 == 0) , -1 , -100))

train_final$MB_Dr_prev_3_frac = ifelse(train_final$MB_D_prev4 != 0 , train_final$MB_D_prev3/train_final$MB_D_prev4 ,
                                       ifelse((train_final$MB_D_prev4 == 0) &(train_final$MB_D_prev3 == 0) , -1 , -100))

test_final$MB_Dr_prev_3_frac = ifelse(test_final$MB_D_prev4 != 0 , test_final$MB_D_prev3/test_final$MB_D_prev4 ,
                                      ifelse((test_final$MB_D_prev3 == 0) &(test_final$MB_D_prev4 == 0) , -1 , -100))

train_final$MB_Dr_prev_4_frac = ifelse(train_final$MB_D_prev5 != 0 , train_final$MB_D_prev4/train_final$MB_D_prev5 ,
                                       ifelse((train_final$MB_D_prev4 == 0) &(train_final$MB_D_prev5 == 0) , -1 , -100))

test_final$MB_Dr_prev_4_frac = ifelse(test_final$MB_D_prev5 != 0 , test_final$MB_D_prev4/test_final$MB_D_prev5 ,
                                      ifelse((test_final$MB_D_prev4 == 0) &(test_final$MB_D_prev5 == 0) , -1 , -100))

train_final$MB_Dr_prev_5_frac = ifelse(train_final$MB_D_prev6 != 0 , train_final$MB_D_prev5/train_final$MB_D_prev6 ,
                                       ifelse((train_final$MB_D_prev5 == 0) &(train_final$MB_D_prev6 == 0) , -1 , -100))

test_final$MB_Dr_prev_5_frac = ifelse(test_final$MB_D_prev6 != 0 , test_final$MB_D_prev5/test_final$MB_D_prev6 ,
                                      ifelse((test_final$MB_D_prev5 == 0) &(test_final$MB_D_prev6 == 0) , -1 , -100))

##Creating CNR ratio variables.

train_final$CNR_prev_1_frac = ifelse(train_final$CNR_prev2 != 0 , train_final$CNR_prev1/train_final$CNR_prev2 ,
                                     ifelse((train_final$CNR_prev1 == 0) &(train_final$CNR_prev2 == 0) , -1 , -100))

test_final$CNR_prev_1_frac = ifelse(test_final$CNR_prev2 != 0 , test_final$CNR_prev1/test_final$CNR_prev2 ,
                                    ifelse((test_final$CNR_prev1 == 0) &(test_final$CNR_prev2 == 0) , -1 , -100))

train_final$CNR_prev_2_frac = ifelse(train_final$CNR_prev3 != 0 , train_final$CNR_prev2/train_final$CNR_prev3 ,
                                     ifelse((train_final$CNR_prev2 == 0) &(train_final$CNR_prev3 == 0) , -1 , -100))

test_final$CNR_prev_2_frac = ifelse(test_final$CNR_prev3 != 0 , test_final$CNR_prev2/test_final$CNR_prev3 ,
                                    ifelse((test_final$CNR_prev2 == 0) &(test_final$CNR_prev3 == 0) , -1 , -100))

train_final$CNR_prev_3_frac = ifelse(train_final$CNR_prev4 != 0 , train_final$CNR_prev3/train_final$CNR_prev4 ,
                                     ifelse((train_final$CNR_prev4 == 0) &(train_final$CNR_prev3 == 0) , -1 , -100))

test_final$CNR_prev_3_frac = ifelse(test_final$CNR_prev4 != 0 , test_final$CNR_prev3/test_final$CNR_prev4 ,
                                    ifelse((test_final$CNR_prev3 == 0) &(test_final$CNR_prev4 == 0) , -1 , -100))

train_final$CNR_prev_4_frac = ifelse(train_final$CNR_prev5 != 0 , train_final$CNR_prev4/train_final$CNR_prev5 ,
                                     ifelse((train_final$CNR_prev4 == 0) &(train_final$CNR_prev5 == 0) , -1 , -100))

test_final$CNR_prev_4_frac = ifelse(test_final$CNR_prev5 != 0 , test_final$CNR_prev4/test_final$CNR_prev5 ,
                                    ifelse((test_final$CNR_prev4 == 0) &(test_final$CNR_prev5 == 0) , -1 , -100))

train_final$CNR_prev_5_frac = ifelse(train_final$CNR_prev6 != 0 , train_final$CNR_prev5/train_final$CNR_prev6 ,
                                     ifelse((train_final$CNR_prev5 == 0) &(train_final$CNR_prev6 == 0) , -1 , -100))

test_final$CNR_prev_5_frac = ifelse(test_final$CNR_prev6 != 0 , test_final$CNR_prev5/test_final$CNR_prev6 ,
                                    ifelse((test_final$CNR_prev5 == 0) &(test_final$CNR_prev6 == 0) , -1 , -100))

##Creating BAL ratio variables.

train_final$BAL_prev_1_frac = ifelse(train_final$BAL_prev2 != 0 , train_final$BAL_prev1/train_final$BAL_prev2 ,
                                     ifelse((train_final$BAL_prev1 == 0) &(train_final$BAL_prev2 == 0) , -1 , -100))

test_final$BAL_prev_1_frac = ifelse(test_final$BAL_prev2 != 0 , test_final$BAL_prev1/test_final$BAL_prev2 ,
                                    ifelse((test_final$BAL_prev1 == 0) &(test_final$BAL_prev2 == 0) , -1 , -100))

train_final$BAL_prev_2_frac = ifelse(train_final$BAL_prev3 != 0 , train_final$BAL_prev2/train_final$BAL_prev3 ,
                                     ifelse((train_final$BAL_prev2 == 0) &(train_final$BAL_prev3 == 0) , -1 , -100))

test_final$BAL_prev_2_frac = ifelse(test_final$BAL_prev3 != 0 , test_final$BAL_prev2/test_final$BAL_prev3 ,
                                    ifelse((test_final$BAL_prev2 == 0) &(test_final$BAL_prev3 == 0) , -1 , -100))

train_final$BAL_prev_3_frac = ifelse(train_final$BAL_prev4 != 0 , train_final$BAL_prev3/train_final$BAL_prev4 ,
                                     ifelse((train_final$BAL_prev4 == 0) &(train_final$BAL_prev3 == 0) , -1 , -100))

test_final$BAL_prev_3_frac = ifelse(test_final$BAL_prev4 != 0 , test_final$BAL_prev3/test_final$BAL_prev4 ,
                                    ifelse((test_final$BAL_prev3 == 0) &(test_final$BAL_prev4 == 0) , -1 , -100))

train_final$BAL_prev_4_frac = ifelse(train_final$BAL_prev5 != 0 , train_final$BAL_prev4/train_final$BAL_prev5 ,
                                     ifelse((train_final$BAL_prev4 == 0) &(train_final$BAL_prev5 == 0) , -1 , -100))

test_final$BAL_prev_4_frac = ifelse(test_final$BAL_prev5 != 0 , test_final$BAL_prev4/test_final$BAL_prev5 ,
                                    ifelse((test_final$BAL_prev4 == 0) &(test_final$BAL_prev5 == 0) , -1 , -100))

train_final$BAL_prev_5_frac = ifelse(train_final$BAL_prev6 != 0 , train_final$BAL_prev5/train_final$BAL_prev6 ,
                                     ifelse((train_final$BAL_prev5 == 0) &(train_final$BAL_prev6 == 0) , -1 , -100))

test_final$BAL_prev_5_frac = ifelse(test_final$BAL_prev6 != 0 , test_final$BAL_prev5/test_final$BAL_prev6 ,
                                    ifelse((test_final$BAL_prev5 == 0) &(test_final$BAL_prev6 == 0) , -1 , -100))

##Creating EOP ratio variables.

train_final$EOP_prev_1_frac = ifelse(train_final$EOP_prev2 != 0 , train_final$EOP_prev1/train_final$EOP_prev2 ,
                                     ifelse((train_final$EOP_prev1 == 0) &(train_final$EOP_prev2 == 0) , -1 , -100))

test_final$EOP_prev_1_frac = ifelse(test_final$EOP_prev2 != 0 , test_final$EOP_prev1/test_final$EOP_prev2 ,
                                    ifelse((test_final$EOP_prev1 == 0) &(test_final$EOP_prev2 == 0) , -1 , -100))

train_final$EOP_prev_2_frac = ifelse(train_final$EOP_prev3 != 0 , train_final$EOP_prev2/train_final$EOP_prev3 ,
                                     ifelse((train_final$EOP_prev2 == 0) &(train_final$EOP_prev3 == 0) , -1 , -100))

test_final$EOP_prev_2_frac = ifelse(test_final$EOP_prev3 != 0 , test_final$EOP_prev2/test_final$EOP_prev3 ,
                                    ifelse((test_final$EOP_prev2 == 0) &(test_final$EOP_prev3 == 0) , -1 , -100))

train_final$EOP_prev_3_frac = ifelse(train_final$EOP_prev4 != 0 , train_final$EOP_prev3/train_final$EOP_prev4 ,
                                     ifelse((train_final$EOP_prev4 == 0) &(train_final$EOP_prev3 == 0) , -1 , -100))

test_final$EOP_prev_3_frac = ifelse(test_final$EOP_prev4 != 0 , test_final$EOP_prev3/test_final$EOP_prev4 ,
                                    ifelse((test_final$EOP_prev3 == 0) &(test_final$EOP_prev4 == 0) , -1 , -100))

train_final$EOP_prev_4_frac = ifelse(train_final$EOP_prev5 != 0 , train_final$EOP_prev4/train_final$EOP_prev5 ,
                                     ifelse((train_final$EOP_prev4 == 0) &(train_final$EOP_prev5 == 0) , -1 , -100))

test_final$EOP_prev_4_frac = ifelse(test_final$EOP_prev5 != 0 , test_final$EOP_prev4/test_final$EOP_prev5 ,
                                    ifelse((test_final$EOP_prev4 == 0) &(test_final$EOP_prev5 == 0) , -1 , -100))

train_final$EOP_prev_5_frac = ifelse(train_final$EOP_prev6 != 0 , train_final$EOP_prev5/train_final$EOP_prev6 ,
                                     ifelse((train_final$EOP_prev5 == 0) &(train_final$EOP_prev6 == 0) , -1 , -100))

test_final$EOP_prev_5_frac = ifelse(test_final$EOP_prev6 != 0 , test_final$EOP_prev5/test_final$EOP_prev6 ,
                                    ifelse((test_final$EOP_prev5 == 0) &(test_final$EOP_prev6 == 0) , -1 , -100))

##Creating cust_init ratio variables.

train_final$custinit_CR_amt_1_frac = ifelse(train_final$custinit_CR_amt_prev2 != 0 , train_final$custinit_CR_amt_prev1/train_final$custinit_CR_amt_prev2 ,
                            ifelse((train_final$custinit_CR_amt_prev2 == 0) &(train_final$custinit_CR_amt_prev1 == 0) , -1 , -100))

test_final$custinit_CR_amt_1_frac = ifelse(test_final$custinit_CR_amt_prev2 != 0 , test_final$custinit_CR_amt_prev1/test_final$custinit_CR_amt_prev2 ,
                            ifelse((test_final$custinit_CR_amt_prev2 == 0) & (test_final$custinit_CR_amt_prev1 == 0) , -1 , -100))

train_final$custinit_CR_amt_2_frac = ifelse(train_final$custinit_CR_amt_prev3 != 0 , train_final$custinit_CR_amt_prev2/train_final$custinit_CR_amt_prev3 ,
                                            ifelse((train_final$custinit_CR_amt_prev3 == 0) &(train_final$custinit_CR_amt_prev2 == 0) , -1 , -100))

test_final$custinit_CR_amt_2_frac = ifelse(test_final$custinit_CR_amt_prev3 != 0 , test_final$custinit_CR_amt_prev2/test_final$custinit_CR_amt_prev3 ,
                                           ifelse((test_final$custinit_CR_amt_prev3 == 0) & (test_final$custinit_CR_amt_prev2 == 0) , -1 , -100))

train_final$custinit_CR_amt_3_frac = ifelse(train_final$custinit_CR_amt_prev4 != 0 , train_final$custinit_CR_amt_prev3/train_final$custinit_CR_amt_prev4,
                                            ifelse((train_final$custinit_CR_amt_prev4 == 0) &(train_final$custinit_CR_amt_prev3 == 0) , -1 , -100))

test_final$custinit_CR_amt_3_frac = ifelse(test_final$custinit_CR_amt_prev4 != 0 , test_final$custinit_CR_amt_prev3/test_final$custinit_CR_amt_prev4 ,
                                           ifelse((test_final$custinit_CR_amt_prev4 == 0) & (test_final$custinit_CR_amt_prev3 == 0) , -1 , -100))

train_final$custinit_CR_amt_4_frac = ifelse(train_final$custinit_CR_amt_prev5 != 0 , train_final$custinit_CR_amt_prev4/train_final$custinit_CR_amt_prev5 ,
                                            ifelse((train_final$custinit_CR_amt_prev5 == 0) &(train_final$custinit_CR_amt_prev4 == 0) , -1 , -100))

test_final$custinit_CR_amt_4_frac = ifelse(test_final$custinit_CR_amt_prev5 != 0 , test_final$custinit_CR_amt_prev4/test_final$custinit_CR_amt_prev5 ,
                                           ifelse((test_final$custinit_CR_amt_prev5 == 0) & (test_final$custinit_CR_amt_prev4 == 0) , -1 , -100))

train_final$custinit_CR_amt_5_frac = ifelse(train_final$custinit_CR_amt_prev6 != 0 , train_final$custinit_CR_amt_prev5/train_final$custinit_CR_amt_prev6 ,
                                            ifelse((train_final$custinit_CR_amt_prev6 == 0) &(train_final$custinit_CR_amt_prev5 == 0) , -1 , -100))

test_final$custinit_CR_amt_5_frac = ifelse(test_final$custinit_CR_amt_prev6 != 0 , test_final$custinit_CR_amt_prev5/test_final$custinit_CR_amt_prev6 ,
                                           ifelse((test_final$custinit_CR_amt_prev6 == 0) & (test_final$custinit_CR_amt_prev5 == 0) , -1 , -100))

##Creating CR_AMB variables.


train_final$CR_AMB_prev_1_frac = ifelse(train_final$CR_AMB_Prev2 == 0 , -1 , train_final$CR_AMB_Prev1/train_final$CR_AMB_Prev2)

test_final$CR_AMB_prev_1_frac = ifelse(test_final$CR_AMB_Prev2 == 0 , -1 , test_final$CR_AMB_Prev1/test_final$CR_AMB_Prev2)

train_final$CR_AMB_prev_2_frac = ifelse(train_final$CR_AMB_Prev3 == 0 , -1 , train_final$CR_AMB_Prev2/train_final$CR_AMB_Prev3)

test_final$CR_AMB_prev_2_frac = ifelse(test_final$CR_AMB_Prev3 == 0 , -1 , test_final$CR_AMB_Prev2/test_final$CR_AMB_Prev3)

train_final$CR_AMB_prev_3_frac = ifelse(train_final$CR_AMB_Prev4 == 0 , -1 , train_final$CR_AMB_Prev3/train_final$CR_AMB_Prev4)

test_final$CR_AMB_prev_3_frac = ifelse(test_final$CR_AMB_Prev4 == 0 , -1 , test_final$CR_AMB_Prev3/test_final$CR_AMB_Prev4)

train_final$CR_AMB_prev_4_frac = ifelse(train_final$CR_AMB_Prev5 == 0 , -1 , train_final$CR_AMB_Prev4/train_final$CR_AMB_Prev5)

test_final$CR_AMB_prev_4_frac = ifelse(test_final$CR_AMB_Prev5 == 0 , -1 , test_final$CR_AMB_Prev4/test_final$CR_AMB_Prev5)

train_final$CR_AMB_prev_5_frac = ifelse(train_final$CR_AMB_Prev6 == 0 , -1 , train_final$CR_AMB_Prev5/train_final$CR_AMB_Prev6)

test_final$CR_AMB_prev_5_frac = ifelse(test_final$CR_AMB_Prev6 == 0 , -1 , test_final$CR_AMB_Prev5/test_final$CR_AMB_Prev6)

##Creating I_AQB ratio variables.


train_final$I_AQB_Q1_frac = ifelse(train_final$I_AQB_PrevQ2 != 0 , train_final$I_AQB_PrevQ1/train_final$I_AQB_PrevQ2 ,
                                   ifelse((train_final$I_AQB_PrevQ1 == 0) &(train_final$I_AQB_PrevQ2 == 0) , -1 , -100))

test_final$I_AQB_Q1_frac = ifelse(test_final$I_AQB_PrevQ2 != 0 , test_final$I_AQB_PrevQ1/test_final$I_AQB_PrevQ2 ,
                                  ifelse((test_final$I_AQB_PrevQ1 == 0) &(test_final$I_AQB_PrevQ2 == 0) , -1 , -100))

##Creating I_CR_AQB ratio variables.

train_final$I_CR_AQB_Q1_frac = ifelse(train_final$I_CR_AQB_PrevQ2 != 0 , train_final$I_CR_AQB_PrevQ1/train_final$I_CR_AQB_PrevQ2 ,
                                      ifelse((train_final$I_CR_AQB_PrevQ1 == 0) &(train_final$I_CR_AQB_PrevQ2 == 0) , -1 , -100))

test_final$I_CR_AQB_Q1_frac = ifelse(test_final$I_CR_AQB_PrevQ2 != 0 , test_final$I_CR_AQB_PrevQ1/test_final$I_CR_AQB_PrevQ2 ,
                                     ifelse((test_final$I_CR_AQB_PrevQ1 == 0) &(test_final$I_CR_AQB_PrevQ2 == 0) , -1 , -100))

##Creating I_CNR ratio variables.

train_final$I_CNR_Q1_frac = ifelse(train_final$I_CNR_PrevQ2 != 0 , train_final$I_CNR_PrevQ1/train_final$I_CNR_PrevQ2 ,
                                   ifelse((train_final$I_CNR_PrevQ1 == 0) &(train_final$I_CNR_PrevQ2 == 0) , -1 , -100))

test_final$I_CNR_Q1_frac = ifelse(test_final$I_CNR_PrevQ2 != 0 , test_final$I_CNR_PrevQ1/test_final$I_CNR_PrevQ2 ,
                                  ifelse((test_final$I_CNR_PrevQ1 == 0) &(test_final$I_CNR_PrevQ2 == 0) , -1 , -100))

##Creating I_NRV ratio variables.

train_final$I_NRV_Q1_frac = ifelse(train_final$I_NRV_PrevQ2 != 0 , train_final$I_NRV_PrevQ1/train_final$I_NRV_PrevQ2 ,
                                   ifelse((train_final$I_NRV_PrevQ1 == 0) &(train_final$I_NRV_PrevQ2 == 0) , -1 , -100))

test_final$I_NRV_Q1_frac = ifelse(test_final$I_NRV_PrevQ2 != 0 , test_final$I_NRV_PrevQ1/test_final$I_NRV_PrevQ2 ,
                                  ifelse((test_final$I_NRV_PrevQ1 == 0) &(test_final$I_NRV_PrevQ2 == 0) , -1 , -100))


##Creating I_req ratio variables.


train_final$I_req_Q1_frac = ifelse(train_final$Req_Logged_PrevQ1 != 0 , train_final$Req_Resolved_PrevQ1/train_final$Req_Logged_PrevQ1 ,
                                   ifelse((train_final$Req_Logged_PrevQ1 == 0) &(train_final$Req_Resolved_PrevQ1 == 0) , -1 , -100))

test_final$I_req_Q1_frac = ifelse(test_final$Req_Logged_PrevQ1 != 0 , test_final$Req_Resolved_PrevQ1/test_final$Req_Logged_PrevQ1 ,
                                  ifelse((test_final$Req_Logged_PrevQ1 == 0) &(test_final$Req_Resolved_PrevQ1 == 0) , -1 , -100))

##Creating I_Query ratio variables.

train_final$I_Query_Q1_frac = ifelse(train_final$Query_Logged_PrevQ1 != 0 , train_final$Query_Resolved_PrevQ1/train_final$Query_Logged_PrevQ1 ,
                                     ifelse((train_final$Query_Logged_PrevQ1 == 0) &(train_final$Query_Resolved_PrevQ1 == 0) , -1 , -100))

test_final$I_Query_Q1_frac = ifelse(test_final$Query_Logged_PrevQ1 != 0 , test_final$Query_Resolved_PrevQ1/test_final$Query_Logged_PrevQ1 ,
                                    ifelse((test_final$Query_Logged_PrevQ1 == 0) &(test_final$Query_Resolved_PrevQ1 == 0) , -1 , -100))

##Creating I_Complaint ratio variables.

train_final$I_Complaint_Q1_frac = ifelse(train_final$Complaint_Logged_PrevQ1 != 0 , train_final$Complaint_Resolved_PrevQ1/train_final$Complaint_Logged_PrevQ1 ,
                                         ifelse((train_final$Complaint_Logged_PrevQ1 == 0) &(train_final$Complaint_Resolved_PrevQ1 == 0) , -1 , -100))

test_final$I_Complaint_Q1_frac = ifelse(test_final$Complaint_Logged_PrevQ1 != 0 , test_final$Complaint_Resolved_PrevQ1/test_final$Complaint_Logged_PrevQ1 ,
                                        ifelse((test_final$Complaint_Logged_PrevQ1 == 0) &(test_final$Complaint_Resolved_PrevQ1 == 0) , -1 , -100))

##Creating FD ratio variables.

train_final$FD_Q1_frac = ifelse(train_final$FD_AMOUNT_BOOK_PrevQ2 != 0 , train_final$FD_AMOUNT_BOOK_PrevQ1/train_final$FD_AMOUNT_BOOK_PrevQ2 ,
                                ifelse((train_final$FD_AMOUNT_BOOK_PrevQ1 == 0) &(train_final$FD_AMOUNT_BOOK_PrevQ2 == 0) , -1 , -100))

test_final$FD_Q1_frac = ifelse(test_final$FD_AMOUNT_BOOK_PrevQ2 != 0 , test_final$FD_AMOUNT_BOOK_PrevQ1/test_final$FD_AMOUNT_BOOK_PrevQ2 ,
                               ifelse((test_final$FD_AMOUNT_BOOK_PrevQ1 == 0) &(test_final$FD_AMOUNT_BOOK_PrevQ2 == 0) , -1 , -100))

##Creating FD_cnt ratio variables.

train_final$FD_cnt_Q1_frac = ifelse(train_final$NO_OF_FD_BOOK_PrevQ2 != 0 , train_final$NO_OF_FD_BOOK_PrevQ1/train_final$NO_OF_FD_BOOK_PrevQ2 ,
                                    ifelse((train_final$NO_OF_FD_BOOK_PrevQ1 == 0) &(train_final$NO_OF_FD_BOOK_PrevQ2 == 0) , -1 , -100))

test_final$FD_cnt_Q1_frac = ifelse(test_final$NO_OF_FD_BOOK_PrevQ2 != 0 , test_final$NO_OF_FD_BOOK_PrevQ1/test_final$NO_OF_FD_BOOK_PrevQ2 ,
                                   ifelse((test_final$NO_OF_FD_BOOK_PrevQ1 == 0) &(test_final$NO_OF_FD_BOOK_PrevQ2 == 0) , -1 , -100))

##Creating RD ratio variables.

train_final$RD_Q1_frac = ifelse(train_final$RD_AMOUNT_BOOK_PrevQ2 != 0 , train_final$RD_AMOUNT_BOOK_PrevQ1/train_final$RD_AMOUNT_BOOK_PrevQ2 ,
                                ifelse((train_final$RD_AMOUNT_BOOK_PrevQ1 == 0) &(train_final$RD_AMOUNT_BOOK_PrevQ2 == 0) , -1 , -100))

test_final$RD_Q1_frac = ifelse(test_final$RD_AMOUNT_BOOK_PrevQ2 != 0 , test_final$RD_AMOUNT_BOOK_PrevQ1/test_final$RD_AMOUNT_BOOK_PrevQ2 ,
                               ifelse((test_final$RD_AMOUNT_BOOK_PrevQ1 == 0) &(test_final$RD_AMOUNT_BOOK_PrevQ2 == 0) , -1 , -100))

##Creating RD cnt ratio variables.

train_final$RD_cnt_Q1_frac = ifelse(train_final$NO_OF_RD_BOOK_PrevQ2 != 0 , train_final$NO_OF_RD_BOOK_PrevQ1/train_final$NO_OF_RD_BOOK_PrevQ2 ,
                                    ifelse((train_final$NO_OF_RD_BOOK_PrevQ1 == 0) &(train_final$NO_OF_RD_BOOK_PrevQ2 == 0) , -1 , -100))

test_final$RD_cnt_Q1_frac = ifelse(test_final$NO_OF_RD_BOOK_PrevQ2 != 0 , test_final$NO_OF_RD_BOOK_PrevQ1/test_final$NO_OF_RD_BOOK_PrevQ2 ,
                                   ifelse((test_final$NO_OF_RD_BOOK_PrevQ1 == 0) &(test_final$NO_OF_RD_BOOK_PrevQ2 == 0) , -1 , -100))

##Creating MF ratio variables.

train_final$MF_Q1_frac = ifelse(train_final$Total_Invest_in_MF_PrevQ2 != 0 , train_final$Total_Invest_in_MF_PrevQ1/train_final$Total_Invest_in_MF_PrevQ2 ,
                                ifelse((train_final$Total_Invest_in_MF_PrevQ1 == 0) &(train_final$Total_Invest_in_MF_PrevQ2 == 0) , -1 , -100))

test_final$MF_Q1_frac = ifelse(test_final$Total_Invest_in_MF_PrevQ2 != 0 , test_final$Total_Invest_in_MF_PrevQ1/test_final$Total_Invest_in_MF_PrevQ2 ,
                               ifelse((test_final$Total_Invest_in_MF_PrevQ1 == 0) &(test_final$Total_Invest_in_MF_PrevQ2 == 0) , -1 , -100))

##Creating MF cnt ratio variables.

train_final$MF_cnt_Q1_frac = ifelse(train_final$count_No_of_MF_PrevQ2 != 0 , train_final$count_No_of_MF_PrevQ1/train_final$count_No_of_MF_PrevQ2 ,
                                    ifelse((train_final$count_No_of_MF_PrevQ1 == 0) &(train_final$count_No_of_MF_PrevQ2 == 0) , -1 , -100))

test_final$MF_cnt_Q1_frac = ifelse(test_final$count_No_of_MF_PrevQ2 != 0 , test_final$count_No_of_MF_PrevQ1/test_final$count_No_of_MF_PrevQ2 ,
                                   ifelse((test_final$count_No_of_MF_PrevQ1 == 0) &(test_final$count_No_of_MF_PrevQ2 == 0) , -1 , -100))

##Creating MF ratio variables.

train_final$DEMAT_Q1_frac = ifelse(train_final$Dmat_Investing_PrevQ2 != 0 , train_final$Dmat_Investing_PrevQ1/train_final$Dmat_Investing_PrevQ2 ,
                                   ifelse((train_final$Dmat_Investing_PrevQ1 == 0) &(train_final$Dmat_Investing_PrevQ2 == 0) , -1 , -100))

test_final$DEMAT_Q1_frac = ifelse(test_final$Dmat_Investing_PrevQ2 != 0 , test_final$Dmat_Investing_PrevQ1/test_final$Dmat_Investing_PrevQ2 ,
                                  ifelse((test_final$Dmat_Investing_PrevQ1 == 0) &(test_final$Dmat_Investing_PrevQ2 == 0) , -1 , -100))

#df_ratio_cr = data.frame(cr_1 = Cr_prev_1_frac_tr , cr_2 = Cr_prev_2_frac_tr , cr_3 = Cr_prev_3_frac_tr , cr_4 = Cr_prev_4_frac_tr , cr_5 = Cr_prev_5_frac_tr)
#df_ratio_dr = data.frame(dr_1 = Cr_prev_1_frac_tr , dr_2 = Cr_prev_2_frac_tr , dr_3 = Cr_prev_3_frac_tr , dr_4 = Cr_prev_4_frac_tr , dr_5 = Cr_prev_5_frac_tr)

#load library for machine learning

final_vars = data.frame(vars = colnames(train_final))
write.csv(final_vars , "final_vars.csv" , row.names = FALSE)
final_vars = read.csv("final_vars.csv")
vars = as.vector(final_vars$vars[final_vars$include == 1])

train_final_1 = train_final[ , vars]
test_final_1 = test_final[ , vars]

#Creating cr_cont_dec_flag.

train_final_1$cr_cont_dec_flag = ifelse(((train_final_1$Cr_prev_5_frac > 0) & (train_final_1$Cr_prev_5_frac < 1)) &
                                          ((train_final_1$Cr_prev_4_frac > 0) & (train_final_1$Cr_prev_4_frac < 1)) &
                                          ((train_final_1$Cr_prev_3_frac > 0) & (train_final_1$Cr_prev_3_frac < 1)) &
                                          ((train_final_1$Cr_prev_2_frac > 0) & (train_final_1$Cr_prev_2_frac < 1)) &
                                          ((train_final_1$Cr_prev_1_frac > 0) & (train_final_1$Cr_prev_1_frac < 1)) , 1 ,
                                        ifelse((train_final_1$Cr_prev_5_frac == -1) & (train_final_1$Cr_prev_4_frac == -1) &
                                          (train_final_1$Cr_prev_3_frac == -1) & (train_final_1$Cr_prev_2_frac == -1) &
                                          (train_final_1$Cr_prev_1_frac == -1) , 2 , 3))

test_final_1$cr_cont_dec_flag = ifelse(((test_final_1$Cr_prev_5_frac > 0) & (test_final_1$Cr_prev_5_frac < 1)) &
                                         ((test_final_1$Cr_prev_4_frac > 0) & (test_final_1$Cr_prev_4_frac < 1)) &
                                         ((test_final_1$Cr_prev_3_frac > 0) & (test_final_1$Cr_prev_3_frac < 1)) &
                                         ((test_final_1$Cr_prev_2_frac > 0) & (test_final_1$Cr_prev_2_frac < 1)) &
                                         ((test_final_1$Cr_prev_1_frac > 0) & (test_final_1$Cr_prev_1_frac < 1)) , 1 ,
                                       ifelse((test_final_1$Cr_prev_5_frac == -1) & (test_final_1$Cr_prev_4_frac == -1) &
                                                (test_final_1$Cr_prev_3_frac == -1) & (test_final_1$Cr_prev_2_frac == -1) &
                                                (test_final_1$Cr_prev_1_frac == -1) , 2 , 3))

table(train_final_1$cr_cont_dec_flag)
table(test_final_1$cr_cont_dec_flag)

##Creating dr con dec flag.

train_final_1$dr_cont_dec_flag = ifelse(((train_final_1$Dr_prev_5_frac > 0) & (train_final_1$Dr_prev_5_frac < 1)) &
                                          ((train_final_1$Dr_prev_4_frac > 0) & (train_final_1$Dr_prev_4_frac < 1)) &
                                          ((train_final_1$Dr_prev_3_frac > 0) & (train_final_1$Dr_prev_3_frac < 1)) &
                                          ((train_final_1$Dr_prev_2_frac > 0) & (train_final_1$Dr_prev_2_frac < 1)) &
                                          ((train_final_1$Dr_prev_1_frac > 0) & (train_final_1$Dr_prev_1_frac < 1)) , 1 ,
                                        ifelse((train_final_1$Dr_prev_5_frac == -1) & (train_final_1$Dr_prev_4_frac == -1) &
                                                 (train_final_1$Dr_prev_3_frac == -1) & (train_final_1$Dr_prev_2_frac == -1) &
                                                 (train_final_1$Dr_prev_1_frac == -1) , 2 , 3))

test_final_1$dr_cont_dec_flag = ifelse(((test_final_1$Dr_prev_5_frac > 0) & (test_final_1$Dr_prev_5_frac < 1)) &
                                         ((test_final_1$Dr_prev_4_frac > 0) & (test_final_1$Dr_prev_4_frac < 1)) &
                                         ((test_final_1$Dr_prev_3_frac > 0) & (test_final_1$Dr_prev_3_frac < 1)) &
                                         ((test_final_1$Dr_prev_2_frac > 0) & (test_final_1$Dr_prev_2_frac < 1)) &
                                         ((test_final_1$Dr_prev_1_frac > 0) & (test_final_1$Dr_prev_1_frac < 1)) , 1 ,
                                       ifelse((test_final_1$Dr_prev_5_frac == -1) & (test_final_1$Dr_prev_4_frac == -1) &
                                                (test_final_1$Dr_prev_3_frac == -1) & (test_final_1$Dr_prev_2_frac == -1) &
                                                (test_final_1$Dr_prev_1_frac == -1) , 2 , 3))

#cr count ratio variables.

train_final_1$Cr_cnt_prev_1_frac = ifelse(train_final_1$count_C_prev2 != 0 , train_final_1$count_C_prev1/train_final_1$count_C_prev2 ,
                                          ifelse((train_final_1$count_C_prev2 == 0) &(train_final_1$count_C_prev21 == 0) , -1 , -100))

test_final_1$Cr_cnt_prev_1_frac = ifelse(test_final_1$count_C_prev2 != 0 , test_final_1$count_C_prev1/test_final_1$count_C_prev2 ,
                                         ifelse((test_final_1$count_C_prev1 == 0) &(test_final_1$count_C_prev2 == 0) , -1 , -100))

train_final_1$Cr_cnt_prev_2_frac = ifelse(train_final_1$count_C_prev3 != 0 , train_final_1$count_C_prev2/train_final_1$count_C_prev3 ,
                                          ifelse((train_final_1$count_C_prev3 == 0) &(train_final_1$count_C_prev2 == 0) , -1 , -100))

test_final_1$Cr_cnt_prev_2_frac = ifelse(test_final_1$count_C_prev3 != 0 , test_final_1$count_C_prev2/test_final_1$count_C_prev3 ,
                                         ifelse((test_final_1$count_C_prev2 == 0) &(test_final_1$count_C_prev3 == 0) , -1 , -100))

train_final_1$Cr_cnt_prev_3_frac = ifelse(train_final_1$count_C_prev4 != 0 , train_final_1$count_C_prev3/train_final_1$count_C_prev4 ,
                                          ifelse((train_final_1$count_C_prev4 == 0) &(train_final_1$count_C_prev3 == 0) , -1 , -100))

test_final_1$Cr_cnt_prev_3_frac = ifelse(test_final_1$count_C_prev4 != 0 , test_final_1$count_C_prev3/test_final_1$count_C_prev4 ,
                                         ifelse((test_final_1$count_C_prev3 == 0) &(test_final_1$count_C_prev4 == 0) , -1 , -100))

train_final_1$Cr_cnt_prev_4_frac = ifelse(train_final_1$count_C_prev5 != 0 , train_final_1$count_C_prev4/train_final_1$count_C_prev5 ,
                                          ifelse((train_final_1$count_C_prev4 == 0) &(train_final_1$count_C_prev5 == 0) , -1 , -100))

test_final_1$Cr_cnt_prev_4_frac = ifelse(test_final_1$count_C_prev5 != 0 , test_final_1$count_C_prev4/test_final_1$count_C_prev5 ,
                                         ifelse((test_final_1$count_C_prev4 == 0) &(test_final_1$count_C_prev5 == 0) , -1 , -100))

train_final_1$Cr_cnt_prev_5_frac = ifelse(train_final_1$count_C_prev6 != 0 , train_final_1$count_C_prev5/train_final_1$count_C_prev6 ,
                                          ifelse((train_final_1$count_C_prev5 == 0) &(train_final_1$count_C_prev6 == 0) , -1 , -100))

test_final_1$Cr_cnt_prev_5_frac = ifelse(test_final_1$count_C_prev6 != 0 , test_final_1$count_C_prev5/test_final_1$count_C_prev6 ,
                                         ifelse((test_final_1$count_C_prev5 == 0) &(test_final_1$count_C_prev6 == 0) , -1 , -100))

#debit count variables.

train_final_1$Dr_cnt_prev_1_frac = ifelse(train_final_1$count_D_prev2 != 0 , train_final_1$count_D_prev1/train_final_1$count_D_prev2 ,
                                          ifelse((train_final_1$count_D_prev2 == 0) &(train_final_1$count_D_prev1 == 0) , -1 , -100))

test_final_1$Dr_cnt_prev_1_frac = ifelse(test_final_1$count_D_prev2 != 0 , test_final_1$count_D_prev1/test_final_1$count_D_prev2 ,
                                         ifelse((test_final_1$count_D_prev1 == 0) &(test_final_1$count_D_prev2 == 0) , -1 , -100))

train_final_1$Dr_cnt_prev_2_frac = ifelse(train_final_1$count_D_prev3 != 0 , train_final_1$count_D_prev2/train_final_1$count_D_prev3 ,
                                          ifelse((train_final_1$count_D_prev3 == 0) &(train_final_1$count_D_prev2 == 0) , -1 , -100))

test_final_1$Dr_cnt_prev_2_frac = ifelse(test_final_1$count_D_prev3 != 0 , test_final_1$count_D_prev2/test_final_1$count_D_prev3 ,
                                         ifelse((test_final_1$count_D_prev2 == 0) &(test_final_1$count_D_prev3 == 0) , -1 , -100))

train_final_1$Dr_cnt_prev_3_frac = ifelse(train_final_1$count_D_prev4 != 0 , train_final_1$count_D_prev3/train_final_1$count_D_prev4 ,
                                          ifelse((train_final_1$count_D_prev4 == 0) &(train_final_1$count_D_prev3 == 0) , -1 , -100))

test_final_1$Dr_cnt_prev_3_frac = ifelse(test_final_1$count_D_prev4 != 0 , test_final_1$count_D_prev3/test_final_1$count_D_prev4 ,
                                         ifelse((test_final_1$count_D_prev4 == 0) &(test_final_1$count_D_prev3 == 0) , -1 , -100))

train_final_1$Dr_cnt_prev_4_frac = ifelse(train_final_1$count_D_prev5 != 0 , train_final_1$count_D_prev4/train_final_1$count_D_prev5 ,
                                          ifelse((train_final_1$count_D_prev4 == 0) &(train_final_1$count_D_prev5 == 0) , -1 , -100))

test_final_1$Dr_cnt_prev_4_frac = ifelse(test_final_1$count_D_prev5 != 0 , test_final_1$count_D_prev4/test_final_1$count_D_prev5 ,
                                         ifelse((test_final_1$count_D_prev4 == 0) &(test_final_1$count_D_prev5 == 0) , -1 , -100))

train_final_1$Dr_cnt_prev_5_frac = ifelse(train_final_1$count_D_prev6 != 0 , train_final_1$count_D_prev5/train_final_1$count_D_prev6 ,
                                          ifelse((train_final_1$count_D_prev5 == 0) &(train_final_1$count_D_prev6 == 0) , -1 , -100))

test_final_1$Dr_cnt_prev_5_frac = ifelse(test_final_1$count_D_prev6 != 0 , test_final_1$count_D_prev5/test_final_1$count_D_prev6 ,
                                         ifelse((test_final_1$count_D_prev5 == 0) &(test_final_1$count_D_prev6 == 0) , -1 , -100))
#colnames(train_final_1)
#branch debit counts ratio

train_final_1$Dr_brn_cnt_prev_1_frac = ifelse(train_final_1$COUNT_BRANCH_D_prev2 != 0 , train_final_1$COUNT_BRANCH_D_prev1/train_final_1$COUNT_BRANCH_D_prev2 ,
                                              ifelse((train_final_1$COUNT_BRANCH_D_prev2 == 0) &(train_final_1$COUNT_BRANCH_D_prev1 == 0) , -1 , -100))

test_final_1$Dr_brn_cnt_prev_1_frac = ifelse(test_final_1$COUNT_BRANCH_D_prev2 != 0 , test_final_1$COUNT_BRANCH_D_prev1/test_final_1$COUNT_BRANCH_D_prev2 ,
                                             ifelse((test_final_1$COUNT_BRANCH_D_prev1 == 0) &(test_final_1$COUNT_BRANCH_D_prev2 == 0) , -1 , -100))

train_final_1$Dr_brn_cnt_prev_2_frac = ifelse(train_final_1$COUNT_BRANCH_D_prev3 != 0 , train_final_1$COUNT_BRANCH_D_prev2/train_final_1$COUNT_BRANCH_D_prev3 ,
                                              ifelse((train_final_1$COUNT_BRANCH_D_prev3 == 0) &(train_final_1$COUNT_BRANCH_D_prev2 == 0) , -1 , -100))

test_final_1$Dr_brn_cnt_prev_2_frac = ifelse(test_final_1$COUNT_BRANCH_D_prev3 != 0 , test_final_1$COUNT_BRANCH_D_prev2/test_final_1$COUNT_BRANCH_D_prev3 ,
                                             ifelse((test_final_1$COUNT_BRANCH_D_prev2 == 0) &(test_final_1$COUNT_BRANCH_D_prev3 == 0) , -1 , -100))

train_final_1$Dr_brn_cnt_prev_3_frac = ifelse(train_final_1$COUNT_BRANCH_D_prev4 != 0 , train_final_1$COUNT_BRANCH_D_prev3/train_final_1$COUNT_BRANCH_D_prev4 ,
                                              ifelse((train_final_1$COUNT_BRANCH_D_prev4 == 0) &(train_final_1$COUNT_BRANCH_D_prev3 == 0) , -1 , -100))

test_final_1$Dr_brn_cnt_prev_3_frac = ifelse(test_final_1$COUNT_BRANCH_D_prev4 != 0 , test_final_1$COUNT_BRANCH_D_prev3/test_final_1$COUNT_BRANCH_D_prev4 ,
                                             ifelse((test_final_1$COUNT_BRANCH_D_prev4 == 0) &(test_final_1$COUNT_BRANCH_D_prev3 == 0) , -1 , -100))

train_final_1$Dr_brn_cnt_prev_4_frac = ifelse(train_final_1$COUNT_BRANCH_D_prev5 != 0 , train_final_1$COUNT_BRANCH_D_prev4/train_final_1$COUNT_BRANCH_D_prev5 ,
                                              ifelse((train_final_1$COUNT_BRANCH_D_prev4 == 0) &(train_final_1$COUNT_BRANCH_D_prev5 == 0) , -1 , -100))

test_final_1$Dr_brn_cnt_prev_4_frac = ifelse(test_final_1$COUNT_BRANCH_D_prev5 != 0 , test_final_1$COUNT_BRANCH_D_prev4/test_final_1$COUNT_BRANCH_D_prev5 ,
                                             ifelse((test_final_1$COUNT_BRANCH_D_prev4 == 0) &(test_final_1$COUNT_BRANCH_D_prev5 == 0) , -1 , -100))

train_final_1$Dr_brn_cnt_prev_5_frac = ifelse(train_final_1$COUNT_BRANCH_D_prev6 != 0 , train_final_1$COUNT_BRANCH_D_prev5/train_final_1$COUNT_BRANCH_D_prev6 ,
                                              ifelse((train_final_1$COUNT_BRANCH_D_prev5 == 0) &(train_final_1$COUNT_BRANCH_D_prev6 == 0) , -1 , -100))

test_final_1$Dr_brn_cnt_prev_5_frac = ifelse(test_final_1$COUNT_BRANCH_D_prev6 != 0 , test_final_1$COUNT_BRANCH_D_prev5/test_final_1$COUNT_BRANCH_D_prev6 ,
                                             ifelse((test_final_1$COUNT_BRANCH_D_prev5 == 0) &(test_final_1$COUNT_BRANCH_D_prev6 == 0) , -1 , -100))

#credit branch count ratio 

train_final_1$Cr_brn_cnt_prev_1_frac = ifelse(train_final_1$COUNT_BRANCH_C_prev2 != 0 , train_final_1$COUNT_BRANCH_C_prev1/train_final_1$COUNT_BRANCH_C_prev2 ,
                                              ifelse((train_final_1$COUNT_BRANCH_C_prev2 == 0) &(train_final_1$COUNT_BRANCH_C_prev1 == 0) , -1 , -100))

test_final_1$Cr_brn_cnt_prev_1_frac = ifelse(test_final_1$COUNT_BRANCH_C_prev2 != 0 , test_final_1$COUNT_BRANCH_C_prev1/test_final_1$COUNT_BRANCH_C_prev2 ,
                                             ifelse((test_final_1$COUNT_BRANCH_C_prev1 == 0) &(test_final_1$COUNT_BRANCH_C_prev2 == 0) , -1 , -100))

train_final_1$Cr_brn_cnt_prev_2_frac = ifelse(train_final_1$COUNT_BRANCH_C_prev3 != 0 , train_final_1$COUNT_BRANCH_C_prev2/train_final_1$COUNT_BRANCH_C_prev3 ,
                                              ifelse((train_final_1$COUNT_BRANCH_C_prev3 == 0) &(train_final_1$COUNT_BRANCH_C_prev2 == 0) , -1 , -100))

test_final_1$Cr_brn_cnt_prev_2_frac = ifelse(test_final_1$COUNT_BRANCH_C_prev3 != 0 , test_final_1$COUNT_BRANCH_C_prev2/test_final_1$COUNT_BRANCH_C_prev3 ,
                                             ifelse((test_final_1$COUNT_BRANCH_C_prev2 == 0) &(test_final_1$COUNT_BRANCH_C_prev3 == 0) , -1 , -100))

train_final_1$Cr_brn_cnt_prev_3_frac = ifelse(train_final_1$COUNT_BRANCH_C_prev4 != 0 , train_final_1$COUNT_BRANCH_C_prev3/train_final_1$COUNT_BRANCH_C_prev4 ,
                                              ifelse((train_final_1$COUNT_BRANCH_C_prev4 == 0) &(train_final_1$COUNT_BRANCH_C_prev3 == 0) , -1 , -100))

test_final_1$Cr_brn_cnt_prev_3_frac = ifelse(test_final_1$COUNT_BRANCH_C_prev4 != 0 , test_final_1$COUNT_BRANCH_C_prev3/test_final_1$COUNT_BRANCH_C_prev4 ,
                                             ifelse((test_final_1$COUNT_BRANCH_C_prev4 == 0) &(test_final_1$COUNT_BRANCH_C_prev3 == 0) , -1 , -100))

train_final_1$Cr_brn_cnt_prev_4_frac = ifelse(train_final_1$COUNT_BRANCH_C_prev5 != 0 , train_final_1$COUNT_BRANCH_C_prev4/train_final_1$COUNT_BRANCH_C_prev5 ,
                                              ifelse((train_final_1$COUNT_BRANCH_C_prev4 == 0) &(train_final_1$COUNT_BRANCH_C_prev5 == 0) , -1 , -100))

test_final_1$Cr_brn_cnt_prev_4_frac = ifelse(test_final_1$COUNT_BRANCH_C_prev5 != 0 , test_final_1$COUNT_BRANCH_C_prev4/test_final_1$COUNT_BRANCH_C_prev5 ,
                                             ifelse((test_final_1$COUNT_BRANCH_C_prev4 == 0) &(test_final_1$COUNT_BRANCH_C_prev5 == 0) , -1 , -100))

train_final_1$Cr_brn_cnt_prev_5_frac = ifelse(train_final_1$COUNT_BRANCH_C_prev6 != 0 , train_final_1$COUNT_BRANCH_C_prev5/train_final_1$COUNT_BRANCH_C_prev6 ,
                                              ifelse((train_final_1$COUNT_BRANCH_C_prev5 == 0) &(train_final_1$COUNT_BRANCH_C_prev6 == 0) , -1 , -100))

test_final_1$Cr_brn_cnt_prev_5_frac = ifelse(test_final_1$COUNT_BRANCH_C_prev6 != 0 , test_final_1$COUNT_BRANCH_C_prev5/test_final_1$COUNT_BRANCH_C_prev6 ,
                                             ifelse((test_final_1$COUNT_BRANCH_C_prev5 == 0) &(test_final_1$COUNT_BRANCH_C_prev6 == 0) , -1 , -100))

#library(dplyr)
#colnames(train_final)[!colnames(train_final) %in% colnames(test_final)]

model_rf = ranger(Responders ~ . , data = train_final_1 , num.trees = 500 , mtry = ncol(train_final_1)^(1/2) , importance = "impurity" , write.forest = TRUE , 
                  probability = TRUE)
mode_rf_pred = predict(model_rf , test_final_1[ , -160])

#head(mode_rf_pred$predictions[ , 1])

submit_rf_1 = data.frame(UCIC_ID = test$UCIC_ID , Responders = mode_rf_pred$predictions[,2])
write.csv(submit_rf_1 , "submit_rf_2.csv" , row.names = FALSE)

#create task

train.task <- makeClassifTask(data = as.data.frame(train_final_1) , target = "Responders" , positive = '1')
test.task <- makeClassifTask(data = as.data.frame(test_final_1) , target = "Responders" , positive = '1')

train.under <- undersample(train.task , rate = 0.3) #keep only 10% of majority class
table(getTaskTargets(train.under))
train.under

#oversampling

train.over <- oversample(train.task , rate = 4) #make minority class 15 times

#SMOTE

#train.smote <- smote(train.task , rate = 2 , nn = 2)

#xgboost

set.seed(2002)

xgb_learner <- makeLearner("classif.xgboost" , predict.type = "prob")
getParamSet("classif.xgboost")

xgb_learner$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 400,
  print_every_n = 50 , 
  max_depth = 7 , 
  eta = 0.12 , 
  lambda = .4 , 
  subsample = .7 , 
  min_child_weight = 7.38 ,
  colsample_bytree = 0.6724
  )

#define hyperparameters for tuning

xg_ps <- makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
)

#define search function

rancontrol <- makeTuneControlRandom(maxit = 10) #do 10 iterations

#5 fold cross validation

set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)

#tune parameters

xgb_tune <- tuneParams(learner = xgb_learner, task = train.task, resampling = set_cv, 
                       measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, control = rancontrol)

#set optimal parameters

xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)

#train model

xgmodel <- train(xgb_learner, train.task)

#test model

predict.xg <- predict(xgmodel, test.task)

#make prediction
xgb_2 = read.csv("submit_xgb_2.csv")
xgb_2_probs = xgb_2$Responders

xg_prediction <- predict.xg$data$prob.1

#model_rf_xg_pred = (0.8*xgb_2_probs + 0.2*xg_prediction)

model_rf_xg_pred = (0.65*xgb_2_probs + 0.35*xg_prediction)
model_xgb_2_rf_xg_pred = ((2/3)*xgb_2_probs + (1/6)*xg_prediction + (1/6)*mode_rf_pred$predictions[,2])

submit_xg_rf_xgb_1 = data.frame(UCIC_ID = test$UCIC_ID , Responders = model_xgb_2_rf_xg_pred)
write.csv(submit_xg_rf_xgb_1 , "submit_xg_rf_xgb_1.csv" , row.names = FALSE)

#load GBM

g.gbm <- makeLearner("classif.gbm", predict.type = "prob")
g.gbm$par.vals = list(
  distribution = "bernoulli" , 
  n.trees = 300 , 
  interaction.depth = 7  , 
  shrinkage = 0.15 
)

#specify tuning method

rancontrol <- makeTuneControlRandom(maxit = 50L)

#3 fold cross validation

set_cv <- makeResampleDesc("CV",iters = 3L)

#parameters

gbm_par<- makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage",lower = 0.01, upper = 1)
)

#n.minobsinnode refers to the minimum number of observations in a tree node. 
#shrinkage is the regulation parameter which dictates how fast / slow the algorithm should move.

#tune parameters
tune_gbm <- tuneParams(learner = g.gbm, task = trainTask,resampling = set_cv,measures = acc,par.set = gbm_par,control = rancontrol)

#check CV accuracy
tune_gbm$y

#set parameters
final_gbm <- setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)

#train
to.gbm <- train(g.gbm, train.under)

#test 

pr.gbm <- predict(to.gbm, test.task)

#submission file

submit_gbm = data.frame(UCIC_ID = test$UCIC_ID , Responders = pr.gbm$data$prob.1)
write.csv(submit_gbm , "submit_gbm_1.csv" , row.names = FALSE)

model_gbm_xg_pred = (0.7*xgb_2_probs + 0.3*pr.gbm$data$prob.1)
submit_gbm_xg = data.frame(UCIC_ID = test$UCIC_ID , Responders = model_gbm_xg_pred)
write.csv(submit_gbm_xg , "submit_gbm_xg_5.csv" , row.names = FALSE)
