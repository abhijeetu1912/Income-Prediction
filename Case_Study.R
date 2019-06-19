#required libraries
library(readr)
library(dplyr)
library(Hmisc)

#problem statement - to predict whether a person's income is more than $50K or not

#reading data
data <- read_csv("C:/Users/Abhijeet/Desktop/Case Study/data.csv") #First column's name was empty, so given default name X1
str(data)  #rows - 40935 & columns - 14 
View(head(data, 200))  #viewing first 200 rows #missing values are present
data <- data[,-1]  #removing first column


table(data$Income)
#correcting dependent variable
data$Income_gt_50K <- ifelse(data$Income == ">50K" | data$Income == ">50K.", 1, 0)

char_var <- sapply(data, is.character) 
sum(char_var)  #7 character variables 
num_var <- sapply(data, is.numeric) 
sum(num_var)  #6 numeric variables 

################### Data Analysis #####################

describe(data)  #summary of data

sapply(data, function(x) mean(is.na(x))*100)  #missing percentage for each column
#every variable has around 7% of missing values

#checking distribution of categorical variables including dependent variable
lapply(data[, char_var], function(x) sort(table(x)*100/length(x), decreasing = T))
#observations 
#1. around 65 % of people are employed in private sector & more than 12% people have government jobs
#2. around 65 % of people have educational qualification of high school or more
#3. 15.25% people have bachelors, 5.13% have masters and 1.13% have doctorate degrees, so every 1 out of 5th person has degree required for sophisticated jobs
#4. around 42% people are married and live with their partner and rest don't have partners
#5. around 5% people have unknown occupation, so in total more than 12% people have missing value for occupation
#6. there is a good represantion wrt occupation and most of them are part of skilled labour force
#7. males are twice in proportion compared to females
#8. more than 80% people are american 

#outlier treatment
#checking for outliers
boxplot(data$Age) #outliers present
range(data$Age, na.rm = T)
boxplot(data$fnlwgt) #outliers present
boxplot(data$EducationNum) #no outlier
range(data$EducationNum, na.rm = T)
boxplot(data$CapitalGain) #outliers present
boxplot(data$CapitalLoss) #outliers present
boxplot(data$HoursPerWeek) #outliers present
range(data$HoursPerWeek, na.rm = T)
quantile(data$HoursPerWeek, c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.97, 0.99, 0.995, 0.997, 0.999), na.rm = T)
hist(data$HoursPerWeek)

#capping and flooring outliers
outlier <- function(x){
  p_99_5 <- quantile(x, 0.995, na.rm = T)
  ifelse(x > p_99_5, p_99_5, x)
}

data[, num_var] <- sapply(data[, num_var], outlier)

#missing value treatment
#categorical variables
#function to imputer missing values for categorical variables
aggregate(data$EducationNum, list(data$Education), mean, na.rm = T) #there is lot of difference in fnlwgt wrt education
data %>% filter(is.na(EducationNum)) %>% distinct(Education)
data$Education <- ifelse(!is.na(data$Education), data$Education, ifelse(data$EducationNum == 16, "Doctorate", 
               ifelse(data$EducationNum == 15, "Prof-school", ifelse(data$EducationNum == 14, "Masters", 
               ifelse(data$EducationNum == 13, "Bachelors", ifelse(data$EducationNum == 12, "Assoc-acdm",              
               ifelse(data$EducationNum == 11, "Assoc-voc", ifelse(data$EducationNum == 10, "Some-college",       
               ifelse(data$EducationNum == 9, "HS-grad", ifelse(data$EducationNum == 8, "12th",       
               ifelse(data$EducationNum == 7, "11th", ifelse(data$EducationNum == 6, "10th",       
               ifelse(data$EducationNum == 5, "9th", ifelse(data$EducationNum ==  4,"7th-8th",                        
               ifelse(data$EducationNum == 3, "5th-6th", ifelse(data$EducationNum == 2, "1st-4th",              
               ifelse(data$EducationNum == 1, "Preschool", "No-Information" )))))))))))))))))         

data$Gender <- ifelse(!is.na(data$Gender), data$Gender, ifelse(data$Relationship == "Husband", "Male", 
                  ifelse(data$Relationship == "Wife", "Female", "No-Information")))                                                                                                      

char_miss <- function(x){
  ifelse(is.na(x)|x=="?", "No-Information", x)
}

data[, char_var] <- sapply(data[, char_var], char_miss)


#numeric variables
data$Age <- ifelse(is.na(data$Age), mean(data$Age, na.rm = T), data$Age)

aggregate(data$fnlwgt, list(data$Education), mean, na.rm = T) #there is not much difference in fnlwgt wrt education
aggregate(data$fnlwgt, list(data$MaritalStatus), mean, na.rm = T) #there is not much difference in fnlwgt wrt marital status
data$fnlwgt <- ifelse(is.na(data$fnlwgt), mean(data$fnlwgt, na.rm = T), data$Age)

aggregate(data$EducationNum, list(data$Education), mean, na.rm = T) #there is lot of difference in fnlwgt wrt education
data %>% filter(is.na(EducationNum)) %>% distinct(Education)
data$EducationNum <- ifelse(!is.na(data$EducationNum), data$EducationNum, ifelse(data$Education == "Doctorate", 16, 
               ifelse(data$Education == "Prof-school", 15, ifelse(data$Education == "Masters", 14,
               ifelse(data$Education == "Bachelors", 13, ifelse(data$Education == "Assoc-acdm", 12,              
               ifelse(data$Education == "Assoc-voc", 11, ifelse(data$Education == "Some-college", 10,       
               ifelse(data$Education == "HS-grad", 9, ifelse(data$Education == "12th", 8,       
               ifelse(data$Education == "11th", 7, ifelse(data$Education == "10th", 6,       
               ifelse(data$Education == "9th", 5, ifelse(data$Education == "7th-8th", 4,                        
               ifelse(data$Education == "5th-6th", 3, ifelse(data$Education == "1st-4th", 2,              
               ifelse(data$Education == "Preschool", 1, 0 )))))))))))))))))         
                                        
aggregate(data$CapitalGain, list(data$Education), mean, na.rm = T)  #there is lot of difference in CapitalGain wrt education
data %>% filter(is.na(CapitalGain)) %>% distinct(Education)
data$CapitalGain <- ifelse(!is.na(data$CapitalGain), data$CapitalGain, ifelse(data$Education == "Doctorate", 5807.2702, 
                    ifelse(data$Education == "Prof-school", 10805.5409,  ifelse(data$Education == "Masters", 2464.9975,
                    ifelse(data$Education == "Bachelors", 1825.8445, ifelse(data$Education == "Assoc-acdm", 655.67092,              
                    ifelse(data$Education == "Assoc-voc", 760.3385, ifelse(data$Education == "Some-college", 533.3229,       
                    ifelse(data$Education == "HS-grad", 580.8885, ifelse(data$Education == "12th", 235.3090,       
                    ifelse(data$Education == "11th", 204.0898, ifelse(data$Education == "10th", 270.0119,       
                    ifelse(data$Education == "9th", 354.5136, ifelse(data$Education == "7th-8th", 260.2276,                        
                    ifelse(data$Education == "5th-6th", 428.8179, ifelse(data$Education == "1st-4th", 150.4341,              
                    ifelse(data$Education == "Preschool", 954.9524, 0 )))))))))))))))))                         

aggregate(data$CapitalLoss, list(data$Education), mean, na.rm = T)  #there is lot of difference in CapitalGain wrt education
data %>% filter(is.na(CapitalLoss)) %>% distinct(Education)
data$CapitalLoss <- ifelse(!is.na(data$CapitalLoss), data$CapitalLoss, ifelse(data$Education == "Doctorate", 251.99536, 
                    ifelse(data$Education == "Prof-school", 255.39835,  ifelse(data$Education == "Masters", 168.79190,
                    ifelse(data$Education == "Bachelors", 128.99709, ifelse(data$Education == "Assoc-acdm", 85.19900,              
                    ifelse(data$Education == "Assoc-voc", 78.35926, ifelse(data$Education == "Some-college", 73.71705,       
                    ifelse(data$Education == "HS-grad", 69.95553, ifelse(data$Education == "12th", 10.45474,       
                    ifelse(data$Education == "11th", 47.66886, ifelse(data$Education == "10th", 58.27498,       
                    ifelse(data$Education == "9th", 21.30212, ifelse(data$Education == "7th-8th", 50.15182,                        
                    ifelse(data$Education == "5th-6th", 60.70166, ifelse(data$Education == "1st-4th", 53.30220,              
                    ifelse(data$Education == "Preschool", 28.65000, 0 )))))))))))))))))                         

aggregate(data$HoursPerWeek, list(data$Education), mean, na.rm = T)  #there is not much difference in HoursPerWeek wrt education
aggregate(data$HoursPerWeek, list(data$Occupation), mean, na.rm = T)  #there is not much difference in HoursPerWeek wrt Occupation
aggregate(data$HoursPerWeek, list(data$NativeCountry), mean, na.rm = T)  #there is not much difference in HoursPerWeek wrt NativeCountry
data %>% filter(is.na(HoursPerWeek)) %>% distinct(Occupation)
data$HoursPerWeek <- ifelse(!is.na(data$HoursPerWeek), data$HoursPerWeek, mean(data$HoursPerWeek, na.rm = T))

sum(is.na(data))  #no missing value

#bivariate analysis
#char_var
aggregate(data$Income_gt_50K, list(data$WorkClass), mean) #people working in gov jobs have better proportion of Income_gt_50K > 50K
aggregate(data$Income_gt_50K, list(data$Education), mean) #advanced degrees pay  >50K
aggregate(data$Income_gt_50K, list(data$MaritalStatus), mean)  #married people have better chances of earning >50K
aggregate(data$Income_gt_50K, list(data$Occupation), mean) #manaegrial and speciality services pay >50K
aggregate(data$Income_gt_50K, list(data$Relationship), mean)#married people are more likely to get paid >50K
aggregate(data$Income_gt_50K, list(data$Gender), mean)  #males are morelikely to be paid >50K
aggregate(data$Income_gt_50K, list(data$NativeCountry), mean)  #immigrants are morelikely to be paid >50K

#num_var
#correlation check
cor(data[, num_var])  #variables are not correlated

boxplot(Age~Income_gt_50K, data)
boxplot(fnlwgt~Income_gt_50K, data)
boxplot(CapitalGain~Income_gt_50K, data)
boxplot(CapitalLoss~Income_gt_50K, data)
boxplot(HoursPerWeek~Income_gt_50K, data)
boxplot(Age~Income_gt_50K, data)

#feature engineering
data <- data %>% mutate(WorkClass_Gov_Flag = ifelse(grepl("gov", WorkClass), 1, 0),
                 WorkClass_Private_Flag = ifelse(WorkClass == "Private", 1, 0),
                 Degree_Flag = ifelse(Education=="Bachelors"|Education=="Masters"|Education=="Doctorate"|Education=="Prof-school", 1, 0),
                 Married_Flag = ifelse(grepl("spouse", MaritalStatus), 1, 0),
                 Occupation_high_level_Flag = ifelse(Occupation=="Exec-managerial"|Occupation=="Prof-specialty", 1, 0),
                 Occupation_mid_level_Flag = ifelse(Occupation=="Armed-Forces"|Occupation=="Protective-serv"|Occupation=="Tech-support"|Occupation=="Sales", 1, 0),
                 Occupation_lower_mid_level_Flag = ifelse(Occupation=="Craft-repair"|Occupation=="Transport-moving", 1, 0),
                 Relationship_husb_wife_Flag = ifelse(Relationship=="Husband"|Relationship=="Wife", 1, 0),
                 Relationship_no_info_Flag = ifelse(Occupation=="No-Information", 1, 0),
                 Relationship_not_family_Flag = ifelse(Occupation=="Not-in-family", 1, 0),
                 High_Paying_Countries = ifelse(NativeCountry %in% high_paying_countries$Group.1, 1, 0))
                 

names(data)
data1 <- data %>% select(Age, fnlwgt, Degree_Flag, EducationNum, Married_Flag, Occupation_high_level_Flag, 
                 Occupation_mid_level_Flag, Occupation_lower_mid_level_Flag, Relationship_husb_wife_Flag,
                 CapitalGain, CapitalLoss, Income_gt_50K, High_Paying_Countries)

#dividing data into train and test
data1 <- data1 %>% mutate(id = row_number())
str(data1)
set.seed(1756)
train <- data1 %>% group_by(Income_gt_50K) %>% sample_frac(0.7) 
test  <- anti_join(data1, train, by = 'id')

mean(train$Income_gt_50K)  #0.2396873 event
mean(test$Income_gt_50K)  #0.2397199 event
#train and test is well distributed wrt dependent flag

#model building

#baseline decision tree
library(rpart)
tree_mod <- rpart(Income_gt_50K ~ ., data = train[, -ncol(train)], method = "class")
summary(tree_mod)
rpart.plot::rpart.plot(tree_mod)

#complexity plot
printcp(tree_mod)
plotcp(tree_mod)

pr_train <- predict(tree_mod, train, type = "class")
mean(train$Income_gt_50K==pr_train)*100   #83.9 %

pr_test <- predict(tree_mod, test, type = "class")
mean(test$Income_gt_50K==pr_test)*100   #83.7 %

#by decision tree, getting around 84% accuracy

#baseline random forest
library(randomForest)
rf_mod <- randomForest(factor(Income_gt_50K) ~ ., data = train[, -ncol(train)], mtry = 3, nodesize = 100)

pr_train_rf <- predict(rf_mod, train, type = "class")
mean(train$Income_gt_50K==pr_train_rf)*100   #85.8 % 

pr_test_rf <- predict(rf_mod, test, type = "class")
mean(test$Income_gt_50K==pr_test_rf)*100   #85.2 %

#variable importance
importance(rf_mod)


prob_train_rf <- predict(rf_mod, train, type = "prob")[,2]
prob_test_rf <- predict(rf_mod, test, type = "prob")[,2]
prob_train_rf
library(pROC)
roc_train <- roc(train$Income_gt_50K, prob_train_rf)
auc(roc_train)
roc_train <- roc(test$Income_gt_50K, prob_test_rf)
auc(roc_train)

###predicting for new data
#reading data
data <- read_csv("C:/Users/Abhijeet/Desktop/Case Study/final.csv") #First column's name was empty, so given default name X1
glimpse(data)  #rows - 40935 & columns - 14 
View(head(data, 200))  #viewing first 200 rows #missing values are present
ID <- data$X1
data <- data[,-1]  #removing first column

char_var <- sapply(data, is.character) 
sum(char_var)  #7 character variables 
num_var <- sapply(data, is.numeric) 
sum(num_var)  #6 numeric variables 

################### Data Analysis #####################

describe(data)  #summary of data

sapply(data, function(x) mean(is.na(x))*100)  #missing percentage for each column
#every variable has around 7% of missing values

lapply(data[, char_var], function(x) sort(table(x)*100/length(x), decreasing = T))

data[, num_var] <- sapply(data[, num_var], outlier)

#missing value treatment
#categorical variables
#function to imputer missing values for categorical variables
aggregate(data$EducationNum, list(data$Education), mean, na.rm = T) #there is lot of difference in fnlwgt wrt education
data %>% filter(is.na(EducationNum)) %>% distinct(Education)
data$Education <- ifelse(!is.na(data$Education), data$Education, ifelse(data$EducationNum == 16, "Doctorate", 
                  ifelse(data$EducationNum == 15, "Prof-school", ifelse(data$EducationNum == 14, "Masters", 
                  ifelse(data$EducationNum == 13, "Bachelors", ifelse(data$EducationNum == 12, "Assoc-acdm",              
                  ifelse(data$EducationNum == 11, "Assoc-voc", ifelse(data$EducationNum == 10, "Some-college",       
                  ifelse(data$EducationNum == 9, "HS-grad", ifelse(data$EducationNum == 8, "12th",       
                  ifelse(data$EducationNum == 7, "11th", ifelse(data$EducationNum == 6, "10th",       
                  ifelse(data$EducationNum == 5, "9th", ifelse(data$EducationNum ==  4,"7th-8th",                        
                  ifelse(data$EducationNum == 3, "5th-6th", ifelse(data$EducationNum == 2, "1st-4th",              
                  ifelse(data$EducationNum == 1, "Preschool", "No-Information" )))))))))))))))))         

data$Gender <- ifelse(!is.na(data$Gender), data$Gender, ifelse(data$Relationship == "Husband", "Male", 
               ifelse(data$Relationship == "Wife", "Female", "No-Information")))                                                                                                      

char_miss <- function(x){
  ifelse(is.na(x)|x=="?", "No-Information", x)
}

data[, char_var] <- sapply(data[, char_var], char_miss)


#numeric variables
data$Age <- ifelse(is.na(data$Age), mean(data$Age, na.rm = T), data$Age)

data$fnlwgt <- ifelse(is.na(data$fnlwgt), mean(data$fnlwgt, na.rm = T), data$Age)

data$EducationNum <- ifelse(!is.na(data$EducationNum), data$EducationNum, ifelse(data$Education == "Doctorate", 16, 
                     ifelse(data$Education == "Prof-school", 15, ifelse(data$Education == "Masters", 14,
                     ifelse(data$Education == "Bachelors", 13, ifelse(data$Education == "Assoc-acdm", 12,              
                     ifelse(data$Education == "Assoc-voc", 11, ifelse(data$Education == "Some-college", 10,       
                     ifelse(data$Education == "HS-grad", 9, ifelse(data$Education == "12th", 8,       
                     ifelse(data$Education == "11th", 7, ifelse(data$Education == "10th", 6,       
                     ifelse(data$Education == "9th", 5, ifelse(data$Education == "7th-8th", 4,                        
                     ifelse(data$Education == "5th-6th", 3, ifelse(data$Education == "1st-4th", 2,              
                     ifelse(data$Education == "Preschool", 1, 0 )))))))))))))))))         

data$CapitalGain <- ifelse(!is.na(data$CapitalGain), data$CapitalGain, ifelse(data$Education == "Doctorate", 5807.2702, 
                    ifelse(data$Education == "Prof-school", 10805.5409,  ifelse(data$Education == "Masters", 2464.9975,
                    ifelse(data$Education == "Bachelors", 1825.8445, ifelse(data$Education == "Assoc-acdm", 655.67092,              
                    ifelse(data$Education == "Assoc-voc", 760.3385, ifelse(data$Education == "Some-college", 533.3229,       
                    ifelse(data$Education == "HS-grad", 580.8885, ifelse(data$Education == "12th", 235.3090,       
                    ifelse(data$Education == "11th", 204.0898, ifelse(data$Education == "10th", 270.0119,       
                    ifelse(data$Education == "9th", 354.5136, ifelse(data$Education == "7th-8th", 260.2276,                        
                    ifelse(data$Education == "5th-6th", 428.8179, ifelse(data$Education == "1st-4th", 150.4341,              
                    ifelse(data$Education == "Preschool", 954.9524, 0 )))))))))))))))))                         

data$CapitalLoss <- ifelse(!is.na(data$CapitalLoss), data$CapitalLoss, ifelse(data$Education == "Doctorate", 251.99536, 
                    ifelse(data$Education == "Prof-school", 255.39835,  ifelse(data$Education == "Masters", 168.79190,
                    ifelse(data$Education == "Bachelors", 128.99709, ifelse(data$Education == "Assoc-acdm", 85.19900,              
                    ifelse(data$Education == "Assoc-voc", 78.35926, ifelse(data$Education == "Some-college", 73.71705,       
                    ifelse(data$Education == "HS-grad", 69.95553, ifelse(data$Education == "12th", 10.45474,       
                    ifelse(data$Education == "11th", 47.66886, ifelse(data$Education == "10th", 58.27498,       
                    ifelse(data$Education == "9th", 21.30212, ifelse(data$Education == "7th-8th", 50.15182,                        
                    ifelse(data$Education == "5th-6th", 60.70166, ifelse(data$Education == "1st-4th", 53.30220,              
                    ifelse(data$Education == "Preschool", 28.65000, 0 )))))))))))))))))                         

data$HoursPerWeek <- ifelse(!is.na(data$HoursPerWeek), data$HoursPerWeek, mean(data$HoursPerWeek, na.rm = T))

sum(is.na(data))  #no missing value

#feature engineering
data <- data %>% mutate(WorkClass_Gov_Flag = ifelse(grepl("gov", WorkClass), 1, 0),
                        WorkClass_Private_Flag = ifelse(WorkClass == "Private", 1, 0),
                        Degree_Flag = ifelse(Education=="Bachelors"|Education=="Masters"|Education=="Doctorate"|Education=="Prof-school", 1, 0),
                        Married_Flag = ifelse(grepl("spouse", MaritalStatus), 1, 0),
                        Occupation_high_level_Flag = ifelse(Occupation=="Exec-managerial"|Occupation=="Prof-specialty", 1, 0),
                        Occupation_mid_level_Flag = ifelse(Occupation=="Armed-Forces"|Occupation=="Protective-serv"|Occupation=="Tech-support"|Occupation=="Sales", 1, 0),
                        Occupation_lower_mid_level_Flag = ifelse(Occupation=="Craft-repair"|Occupation=="Transport-moving", 1, 0),
                        Relationship_husb_wife_Flag = ifelse(Relationship=="Husband"|Relationship=="Wife", 1, 0),
                        Relationship_no_info_Flag = ifelse(Occupation=="No-Information", 1, 0),
                        Relationship_not_family_Flag = ifelse(Occupation=="Not-in-family", 1, 0),
                        High_Paying_Countries = ifelse(NativeCountry %in% high_paying_countries$Group.1, 1, 0))


names(data)
data1 <- data %>% select(Age, fnlwgt, Degree_Flag, EducationNum, Married_Flag, Occupation_high_level_Flag, 
                          Occupation_mid_level_Flag, Occupation_lower_mid_level_Flag, Relationship_husb_wife_Flag,
                          CapitalGain, CapitalLoss, High_Paying_Countries)

Income <- predict(rf_mod, data1, type = "class")

df <- cbind.data.frame(ID, Income)
df$Income <- as.numeric(df$Income)
df$Income <- ifelse(df$Income == 1, "<=50K", ">50K")
View(df)

write.csv(df, "C:/Users/Abhijeet/Desktop/Case Study/result.csv", row.names = F)
