library(readxl)
mydata<-read_excel("C:\\Users\\ASUS\\Documents\\Data Science\\Dataset.xlsx")
mydata

 
is.na(mydata)
colSums(is.na(mydata))

which(is.na(mydata$person_age))
which(is.na(mydata$person_gender))
which(is.na(mydata$person_education))
which(is.na(mydata$person_income))
which(is.na(mydata$loan_percent_income))
which(is.na(mydata$loan_status))



mean_age<- round(mean(mydata$person_age,na.rm =TRUE))
mean_age
mydata$person_age[is.na(mydata$person_age)] <- mean_age

mean_income<- round(mean(mydata$person_income,na.rm =TRUE))
mean_income
mydata$person_income[is.na(mydata$person_income)] <- mean_income

mean_loanpercentincome<-round(mean(mydata$loan_percent_income,na.rm =TRUE))
mean_loanpercentincome
mydata$loan_percent_income[is.na(mydata$loan_percent_income)] <- mean_loanpercentincome

mean_loanstatus<-round(mean(mydata$loan_status,na.rm =TRUE))
mean_loanstatus
mydata$loan_status[is.na(mydata$loan_status)] <- mean_loanstatus


library(modeest)
mode_gender<- mfv(mydata$person_gender)
mode_gender
mydata$person_gender[is.na(mydata$person_gender)] <- mode_gender

mode_education<- mfv(mydata$person_education)
mode_education
mydata$person_education[is.na(mydata$person_education)] <- mode_education




Detect_NoisyValue <- levels(factor(mydata$person_home_ownership))
Detect_NoisyValue


mydata$person_home_ownership[which(mydata$person_home_ownership == "OOWN")] <- "OWN"
mydata$person_home_ownership[which(mydata$person_home_ownership == "RENTT")] <- "RENT"
library(dplyr)
mydata %>%
  select(person_home_ownership)


library(dplyr)
invalid <- mydata$person_age < 0 | mydata$person_age > 100
invalid_ages <- mydata %>%
  filter(invalid)
invalid_ages
age_median <-median(mydata$person_age, na.rm = TRUE)
mydata$person_age[invalid] <- NA 
age_median
mydata$person_age[is.na(mydata$person_age)] <- age_median
factor(mydata$person_age)


duplicated(mydata)
sum(duplicated(mydata)) 
which(duplicated(mydata))
fixed_mydata <- distinct(mydata)
fixed_mydata
which(duplicated(fixed_mydata))
duplicated(fixed_mydata)



summary(fixed_mydata$person_income)
s <- summary(fixed_mydata$person_income)
Q1 <- as.numeric(s["1st Qu."])
Q3 <- as.numeric(s["3rd Qu."])
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val
outlier_values <- fixed_mydata$person_income[
  fixed_mydata$person_income < lower_bound | fixed_mydata$person_income > upper_bound]
outlier_values

q1_exp <- quantile(fixed_mydata$person_emp_exp, 0.25, na.rm = TRUE) 
q3_exp <- quantile(fixed_mydata$person_emp_exp, 0.75, na.rm = TRUE) 
iqr_exp <- q3_exp - q1_exp    
lower_bound_exp <- q1_exp - 1.5 * iqr_exp  
upper_bound_exp <- q3_exp + 1.5 * iqr_exp 
outliers_exp <- fixed_mydata$person_emp_exp[fixed_mydata$person_emp_exp < 
                                              lower_bound_exp | fixed_mydata$person_emp_exp > upper_bound_exp]  
outliers_exp 

q1_age <- quantile(fixed_mydata$person_age, 0.25, na.rm = TRUE)
q3_age <- quantile(fixed_mydata$person_age, 0.75, na.rm = TRUE)
iqr_age <- q3_age - q1_age
lower_bound_age <- q1_age - 1.5 * iqr_age
upper_bound_age <- q3_age + 1.5 * iqr_age
outliers_age <- fixed_mydata$person_age[fixed_mydata$person_age <lower_bound_age
                                        | fixed_mydata$person_age > upper_bound_age]
outliers_age

q1_cre_score <- quantile(fixed_mydata$credit_score, 0.25, na.rm = TRUE) 
q3_cre_score <- quantile(fixed_mydata$credit_score, 0.75, na.rm = TRUE) 
iqr_cre_score <- q3_cre_score- q1_cre_score   
lower_bound_cre_score <- q1_cre_score- 1.5 * iqr_cre_score   
upper_bound_cre_score <- q3_cre_score + 1.5 * iqr_cre_score 
outliers_cre_score <- fixed_mydata$credit_score[fixed_mydata$credit_score < 
                                                  lower_bound_cre_score | fixed_mydata$credit_score > upper_bound_cre_score]   
outliers_cre_score

q1_loan_amnt <- quantile(fixed_mydata$loan_amnt, 0.25, na.rm = TRUE) 
q3_loan_amnt <- quantile(fixed_mydata$loan_amnt, 0.75, na.rm = TRUE) 
iqr_loan_amnt <- q3_loan_amnt - q1_loan_amnt   
lower_bound_loan_amnt <- q1_loan_amnt - 1.5 * iqr_loan_amnt   
upper_bound_loan_amnt <- q3_loan_amnt+ 1.5 * iqr_loan_amnt 
outliers_loan_amnt <- fixed_mydata$loan_amnt[fixed_mydata$loan_amnt <      
                                               lower_bound_loan_amnt | fixed_mydata$loan_amnt > upper_bound_loan_amnt]   
outliers_loan_amnt


fixed_mydata$person_emp_exp <- ifelse( 
  fixed_mydata$person_emp_exp < lower_bound_exp, round(lower_bound_exp), 
  ifelse(fixed_mydata$person_emp_exp > upper_bound_exp, round(upper_bound_exp), 
         fixed_mydata$person_emp_exp) 
) 

fixed_mydata$credit_score <- ifelse( 
  
  fixed_mydata$credit_score < lower_bound_cre_score, round(lower_bound_cre_score), 
  
  ifelse(fixed_mydata$credit_score > upper_bound_cre_score, round(upper_bound_cre_score), 
         
         fixed_mydata$credit_score) 
  
) 

fixed_mydata$person_income <- ifelse( 
  
  fixed_mydata$person_income < lower_bound, round(lower_bound), 
  
  ifelse(fixed_mydata$person_income > upper_bound, round(upper_bound), 
         
         fixed_mydata$person_income) 
  
) 
fixed_mydata$person_emp_exp
fixed_mydata$credit_score
fixed_mydata$person_income


normalizeddata_income <- fixed_mydata
normalizeddata_income$person_income<- round((normalizeddata_income$person_income -
                                     min(normalizeddata_income$person_income, na.rm = TRUE)) /
                                     (max(normalizeddata_income$person_income, na.rm = TRUE) - 
                                     min(normalizeddata_income$person_income, na.rm = TRUE)),2)
normalizeddata_income$person_income



fixed_mydata$loan_status <- factor( 
  
  fixed_mydata$loan_status, 
  
  levels = c(0, 1), 
  
  labels = c("No", "Yes") 
  
) 

fixed_mydata$loan_status


fixed_mydata$previous_loan_defaults_on_file <- ifelse(
  
  fixed_mydata$previous_loan_defaults_on_file == "Yes", 1, 0
  
)
fixed_mydata$previous_loan_defaults_on_file


filtered_data <- filter(fixed_mydata, person_education == "Master")
head(filtered_data)

filtered_data <- filter(fixed_mydata, loan_status == "Yes")
filtered_data[, c("person_age", "person_gender", "loan_status")]


str(fixed_mydata)
imbalanced_data <- fixed_mydata %>%
  mutate(across(where(is.character), as.factor))
str(imbalanced_data)

library(ROSE)
set.seed(199)


table(imbalanced_data$loan_status)

balanced_data <- ROSE(loan_status ~ ., 
                      data = imbalanced_data, 
                      N = 400,   
                      p = 0.5)$data   

table(balanced_data$loan_status)
balanced_data
balanced_data$person_age <- round(balanced_data$person_age) 


set.seed(199) 
index <- sample(1:nrow(balanced_data), 0.7 * nrow(balanced_data))

train_data <- balanced_data[index, ]
test_data  <- balanced_data[-index, ]

train_data
test_data

table(train_data$loan_status)
table(test_data$loan_status)




mean_loan_int_rate   <- mean(fixed_mydata$loan_int_rate)
median_loan_int_rate <- median(fixed_mydata$loan_int_rate)
mode_loan_int_rate   <- mfv(fixed_mydata$loan_int_rate)


mean_loan_percent_income   <- mean(fixed_mydata$loan_percent_income)
median_loan_percent_income <- median(fixed_mydata$loan_percent_income)
mode_loan_percent_income   <- mfv(fixed_mydata$loan_percent_income)


mode_home_ownership <- mfv(fixed_mydata$person_home_ownership)
mode_home_ownership

mode_loan_intent <- mfv(fixed_mydata$loan_intent)
mode_loan_intent




range_income <- range(fixed_mydata$person_income)
iqr_income   <- IQR(fixed_mydata$person_income)
var_income   <- var(fixed_mydata$person_income)
sd_income    <- sd(fixed_mydata$person_income)

range_income
iqr_income
var_income
sd_income

range_loan   <- range(fixed_mydata$loan_amnt)
iqr_loan     <- IQR(fixed_mydata$loan_amnt)
var_loan     <- var(fixed_mydata$loan_amnt)
sd_loan      <- sd(fixed_mydata$loan_amnt)

range_loan
iqr_loan
var_loan
sd_loan







