
## First import the Hospitals_Train.cvs file using the R Studio Interface

## what are the columns and how many ?
colnames(Hospitals_Train)
ncol(Hospitals_Train)

## Addressing missing values
## Number of NA values
sum(is.na(Hospitals_Train))
## % of NA values compared to total values
sum(is.na(Hospitals_Train))/(nrow(Hospitals_Train) * ncol(Hospitals_Train)) * 100

## NA values in each column
sapply(Hospitals_Train, function(x) sum(is.na(x)))
sapply(Hospitals_Train, class)


## Convert all the variables into numeric or factors
Hospitals_Train$CHARGES <- as.numeric(Hospitals_Train$CHARGES)

library(magrittr)
library(dplyr)
library(plyr)


cols <- c("HOSPITAL", "GENDER","RACE", "ETHNICITY", "FINANCIAL_CLASS", 
          "WEEKDAY_ARR", "HOUR_ARR", "MONTH_ARR", "WEEKDAY_DEP", "HOUR_DEP", 
          "MONTH_DEP", "SAME_DAY", "ED_RESULT", "ACUITY_ARR", "ADMIT_RESULT",
          "CONSULT_ORDER", "CONSULT_CHARGE", "CONSULT_IN_ED", "DIAGNOSIS", "RISK", 
          "SEVERITY")
Hospitals_Train %<>%
  mutate_each_(funs(factor(.)),cols)
str(Hospitals_Train)
sapply(Hospitals_Train, class)



## Delete rows that have all NA values
df <- Hospitals_Train[rowSums(is.na(Hospitals_Train)) != ncol(Hospitals_Train),]

## Again NA values in each column
sapply(df, function(x) sum(is.na(x)))



## Analysis of Variables
## Return
##return_na <- df[df$RETURN == '#N/A', ]
count(df$RETURN)
df$RETURN[df$RETURN == '#N/A'] = 'No'


sapply(df, function(x) sum(is.na(x)))

df$RETURN[df$RETURN=='Yes'] <- 1
df$RETURN[df$RETURN=='No'] <- 0
df$RETURN <- as.factor(df$RETURN)


table(df$RETURN, df$HOSPITAL)


## Hospital
count(df$HOSPITAL)
table(df$HOSPITAL, df$RETURN, dnn=c('Hospital', 'Return'))

## Gender
count(df$GENDER)
table(df$GENDER, df$RETURN, dnn=c('Gender', 'Return'))

df$GENDER[is.na(df$GENDER)] <- 'Male'

## Age
count(df$AGE)
hist(df$AGE, labels = TRUE)
median(df$AGE)
df$AGE[df$AGE > 120] <- median(df$AGE)
max(df$AGE)

## Race
count(df$RACE)
race_na <- df[is.na(df$RACE), ]
df$RACE[is.na(df$RACE)] <- 'Unknown'
table(df$RACE, df$RETURN, dnn=c('Race', 'Return'))
boxplot(df$CHARGES~df$RACE)

## Ethnicity
count(df$ETHNICITY)
df$ETHNICITY[is.na(df$ETHNICITY)] <- 'Unknown'
table(df$ETHNICITY, df$RETURN, dnn=c('Ethnicity', 'Return'))


## Financial Class
count(df$FINANCIAL_CLASS)
table(df$FINANCIAL_CLASS, df$RETURN, dnn=c('Financial Class', 'Return'))
boxplot(df$CHARGES~df$FINANCIAL_CLASS)

## Weekday Arr
count(df$WEEKDAY_ARR)
table(df$WEEKDAY_ARR, df$RETURN, dnn=c('Weekday_Arr', 'Return'))

## Hour Arr
count(df$HOUR_ARR)
table(df$HOUR_ARR, df$RETURN, dnn=c('Hour_Arr', 'Return'))

## Month Arr
count(df$MONTH_ARR)
table(df$MONTH_ARR, df$RETURN, dnn=c('Month_Arr', 'Return'))

## Weekday Dep
count(df$WEEKDAY_DEP)
table(df$WEEKDAY_DEP, df$RETURN, dnn=c('Weekday_Dep', 'Return'))

## Hour Dep
count(df$HOUR_DEP)
table(df$HOUR_DEP, df$RETURN, dnn=c('Hour_Dep', 'Return'))

## Month Dep
count(df$MONTH_DEP)
table(df$MONTH_DEP, df$RETURN, dnn=c('Month_Dep', 'Return'))


table(df$MONTH_ARR, df$MONTH_DEP, dnn=c('Month_Arr', 'Month_Dep'))
table(df$WEEKDAY_ARR, df$WEEKDAY_DEP, dnn=c('Weekday_Arr', 'Weekday_Dep'))
table(df$HOUR_ARR, df$HOUR_DEP, dnn=c('Hour_Arr', 'Hour_Dep'))

## Same Day
count(df$SAME_DAY)
table(df$SAME_DAY, df$RETURN, dnn=c('Same_Day', 'Return'))

## ED Result
count(df$ED_RESULT)
table(df$ED_RESULT, df$RETURN, dnn=c('ED Result', 'Return'))
Ed_result_na <- df[is.na(df$ED_RESULT), ]
df$ED_RESULT[is.na(df$ED_RESULT)] <- 'Discharge'
table(df$ED_RESULT, df$DC_RESULT, dnn=c('ED Result', 'DC Result'))


## Acuity Arr
count(test_df$ACUITY_ARR)
table(df$ACUITY_ARR, df$RETURN, dnn=c('Acuity_Arr', 'Return'))
acuity_arr_na <- df[is.na(df$ACUITY_ARR), ]

df$ACUITY_ARR[is.na(df$ACUITY_ARR)] <- '5-Non-Urgent'
df$ACUITY_ARR[df$ACUITY_ARR=='5 Purple'] <- '3-Urgent'



## DC Result
count(df$DC_RESULT)
table(df$DC_RESULT, df$RETURN, dnn=c('DC Result', 'Return'))

df$DC_RESULT[is.na(df$DC_RESULT)] <- 'Home or Self Care'


df$DC_RESULT=ifelse(df$DC_RESULT=='Home or Self Care','Home or Self Care',
                    ifelse(df$DC_RESULT=='LEFT W/O BEING SEEN AFTER TRIAGE','LEFT W/O BEING SEEN AFTER TRIAGE',
                           ifelse(df$DC_RESULT=='LEFT W/O BEING SEEN BEFORE TRIAGE','LEFT W/O BEING SEEN BEFORE TRIAGE','Other')))
df$DC_RESULT=as.factor(df$DC_RESULT)


## Admit Result
count(df$ADMIT_RESULT)
table(df$ADMIT_RESULT, df$RETURN, dnn=c('Admit Result', 'Return'))
admit_result_na <- df[is.na(df$ADMIT_RESULT), ]

table(df$ADMIT_RESULT, df$ED_RESULT, dnn=c('Admit Result', 'ED Result'))

df$ADMIT_RESULT <- as.character(df$ADMIT_RESULT)
df$ADMIT_RESULT[is.na(df$ADMIT_RESULT)] <- 'Not Admitted to Hospital'
df$ADMIT_RESULT <- as.factor(df$ADMIT_RESULT)


## Consult Order
count(df$CONSULT_ORDER)
table(df$CONSULT_ORDER, df$RETURN, dnn=c('Consult Order', 'Return'))

## Consult Charge
count(df$CONSULT_CHARGE)
table(df$CONSULT_CHARGE, df$CONSULT_ORDER, dnn=c('Consult Charge', 'Return'))

## Consult In ED
count(df$CONSULT_IN_ED)
table(df$CONSULT_IN_ED, df$RETURN, dnn=c('Consult In Ed', 'Return'))
df$CONSULT_IN_ED = as.numeric(df$CONSULT_IN_ED)
df$CONSULT_IN_ED[is.na(df$CONSULT_IN_ED)] <- 0
df$CONSULT_IN_ED = as.factor(df$CONSULT_IN_ED)


## Diagnosis
count(df$DIAGNOSIS)
table(df$DIAGNOSIS, df$RETURN, dnn=c('Diagnosis', 'Return'))

## Diag Details
count(df$DIAG_DETAILS)
table(df$DIAG_DETAILS, df$RETURN, dnn=c('Diag Details', 'Return'))

## Risk 
count(df$RISK)
table(df$RISK, df$RETURN, dnn=c('Risk', 'Return'))
df$RISK = as.character(df$RISK)
df$RISK[is.na(df$RISK)] <- 'Others'
df$RISK = as.factor(df$RISK)



## Severity
count(df$SEVERITY)
table(df$SEVERITY, df$RETURN, dnn=c('Severity', 'Return'))
df$SEVERITY = as.character(df$SEVERITY)
df$SEVERITY[is.na(df$SEVERITY)] <- 'Others'
df$SEVERITY = as.factor(df$SEVERITY)



## Charges
count(df$CHARGES)
table(df$CHARGES, df$RETURN, dnn=c('Charges', 'Return'))

median(df$CHARGES[!is.na(df$CHARGES)])
df$CHARGES[is.na(df$CHARGES)] <- median(df$CHARGES[!is.na(df$CHARGES)])

sapply(df, class)
sapply(df, function(x) sum(is.na(x)))



## Save to file
write.csv(df[,-c(1,8,9)], file='training_data.csv', row.names=FALSE)














