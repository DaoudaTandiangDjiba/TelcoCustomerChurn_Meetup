
#==================================Importing packages========================================
library(plyr)
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)
library(reshape)

#========================================WHAT'S ABOUT TELCO DATASET===================================
list.files(path = "../input")
# Load and view the dataset
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
View(telco)
# See the variables types
glimpse(telco)
str(telco)
# Uniforming the variables names (tenure, gender, customerID) by putting the first letter to uppercase 
telco <- rename(telco,c("tenure"= "Tenure","gender"="Gender", "customerID"="CustomerID"))
# Change the value of SeniorCitizen to "YES" or "NO" and set up in factor
telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen==1, 'YES', 'NO'))
table(telco$SeniorCitizen)
View(telco)

#===========================Take a look to missing data in telco dataset================================= 
# Take the percent of the count missing data in each column
missing_data <- telco %>% summarise_all(funs(sum(is.na(.))/n()))
View(missing_data)

# Transform it as a dataframe with 2 columns 
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")

# visualize the missing data percent for each variables
# Plot dimension
options(repr.plot.width = 6, repr.plot.height = 4) 
# Missing data barplot
Missingdata_barplot <- ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('White')), size = 0.3)+
  xlab('variables')+
  coord_flip()+
  theme_bw()
Missingdata_barplot
summary(telco$TotalCharges)
# Create a complete dataset without NAs
telco_full <- telco[complete.cases(telco),]
glimpse(telco_full)
str(telco_full)

#==================================================== DATA VISUALIZATION =====================================================
# Take a look to the Customer Churn Rate (CCR)
options(repr.plot.width = 6, repr.plot.height = 4)
telco_full %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Customer Churn Rate")

# Visualize descriptive variables by Customer Churn Rate 
theme1 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
theme2 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")
# First plotgrid
options(repr.plot.width = 12, repr.plot.height = 8)
First_plotgrid <- plot_grid(ggplot(telco_full, aes(x=Gender,fill=Churn))+ geom_bar(position = 'fill')+ theme1, 
                            ggplot(telco_full, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                            ggplot(telco_full, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                            ggplot(telco_full, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                            ggplot(telco_full, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                            ggplot(telco_full, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
                              scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                            align = "h")
First_plotgrid

# Second plotgrid
options(repr.plot.width = 12, repr.plot.height = 8)
Second_plotgrid <- plot_grid(ggplot(telco_full, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+ theme1, 
                             ggplot(telco_full, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                             ggplot(telco_full, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                             ggplot(telco_full, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                             ggplot(telco_full, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme1,
                             ggplot(telco_full, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
                               scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                             align = "h")
Second_plotgrid

# Third plotgrid
options(repr.plot.width = 12, repr.plot.height = 8)
Third_plotgrid <- plot_grid(ggplot(telco_full, aes(x=StreamingMovies,fill=Churn))+ geom_bar(position = 'fill')+ theme1, 
                            ggplot(telco_full, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill')+ theme1,
                            ggplot(telco_full, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill')+ theme1,
                            ggplot(telco_full, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')+ theme_bw()+
                              scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                            align = "h")
Third_plotgrid

# Visualize continuous variables
# Tenure, MonthlyCharges, TotalCharges
options(repr.plot.width =12, repr.plot.height = 8)
plot_grid(ggplot(telco_full, aes(y= Tenure, x = Churn, fill = Churn)) + 
            geom_boxplot()+ 
            theme1+
            xlab("Churn"),
          ggplot(telco_full, aes(y= MonthlyCharges, x = Churn, fill = Churn)) + 
            geom_boxplot()+ 
            theme1+
            xlab("Churn"),
          ggplot(telco_full, aes(y= TotalCharges, x = Churn, fill = Churn)) + 
            geom_boxplot()+ 
            theme_bw()+
            xlab("Churn"))

# Checking the correlation between countinuous variables
options(repr.plot.width =6, repr.plot.height = 4)
telco_full_cor <- round(cor(telco_full[,c("Tenure", "MonthlyCharges", "TotalCharges")]), 1)
ggcorrplot(telco_full_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

telco_full$Churn <- as.numeric(ifelse(telco_full$Churn=="No", 0, 1))
telco_full_cor2 <- round(cor(telco_full[,c("Tenure", "MonthlyCharges", "TotalCharges", "Churn")]), 1)
ggcorrplot(telco_full_cor2,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))
telco_full$Churn <- as.factor(ifelse(telco_full$Churn==1, "Yes", "No"))


# Checking outliers
# Tenure outliers
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(telco$Tenure)$out
# MonthlyCharges
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(telco$MonthlyCharges)$out
# TotalCharges
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(telco$TotalCharges)$out

#================================ DATA PREPARATION =================================
# Cleaning data
telco_full <- data.frame(lapply(telco_full, function(x) {
  gsub("No internet service", "No", x)}))
table(telco_full$OnlineSecurity)
table(telco_full$OnlineBackup)
table(telco_full$DeviceProtection)
table(telco_full$TechSupport)
table(telco_full$StreamingTV)
table(telco_full$StreamingMovies)

telco_full <- data.frame(lapply(telco_full, function(x) {
  gsub("No phone service", "No", x)}))
table(telco_full$MultipleLines)

num_columns <- c("Tenure", "MonthlyCharges", "TotalCharges")
telco_full[num_columns] <- sapply(telco_full[num_columns], as.numeric)

telco_int <- telco_full[,c("Tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))

telco_full <- mutate(telco_full,Tenure_bin = Tenure)

telco_full$Tenure_bin[telco_full$Tenure_bin >=0 & telco_full$Tenure_bin <= 12] <- '0-1 year'
telco_full$Tenure_bin[telco_full$Tenure_bin > 12 & telco_full$Tenure_bin <= 24] <- '1-2 years'
telco_full$Tenure_bin[telco_full$Tenure_bin > 24 & telco_full$Tenure_bin <= 36] <- '2-3 years'
telco_full$Tenure_bin[telco_full$Tenure_bin > 36 & telco_full$Tenure_bin <= 48] <- '3-4 years'
telco_full$Tenure_bin[telco_full$Tenure_bin > 48 & telco_full$Tenure_bin <= 60] <- '4-5 years'
telco_full$Tenure_bin[telco_full$Tenure_bin > 60 & telco_full$Tenure_bin <= 72] <- '5-6 years'


telco_full$Tenure_bin <- as.factor(telco_full$Tenure_bin)

ggplot(telco_full, aes(x = Tenure_bin, fill = Tenure_bin))+
  geom_bar()+ theme1

# The Tenure by the churn rate
ggplot(telco_full, aes(x = Tenure_bin, fill = Tenure_bin))+
         geom_bar()+ 
         theme_bw() +
         facet_wrap(~Churn)+
         ggtitle("Distribution of Tenure_bin by the churn rate")+
         ylab("Count") 

# The churn rate by the Tenure_bin
ggplot(telco_full, aes(x = Tenure_bin, fill = Churn))+
         geom_bar()+ 
         theme_bw() +
         ggtitle("Distribution of Tenure_bin by the churn rate")+
         ylab("Count")

# The churn rate by the Contract
ggplot(telco_full, aes(x = Tenure_bin, fill = Churn))+
         geom_bar()+ 
         theme_bw() +
         facet_wrap(~Contract)+
         ggtitle("Distribution of Churn rate by contract")+
         ylab("Count")

telco_cat <- telco_full[,-c(1,6,19,20)]
#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))
head(dummy)

# Create the final dataset by combining the data
# Combining the data
telco_final <- cbind(telco_int,dummy)
head(telco_final)
telco_final$Churn <- as.factor(telco_final$Churn)

#=============================== Exploratory Modeling ======================================
library(randomForest)
set.seed(123)

#Splitting the data
indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]

#Training the RandomForest Model
model.rf <- randomForest(Churn ~ ., data=train, proximity=FALSE,importance = FALSE,
                         ntree=500,mtry=4, do.trace=FALSE)
model.rf
#Predicting on the train set and checking the Confusion Matrix to see how it performs on the same data
trainPred <- predict(model.rf, train)
confusionMatrix(trainPred, train$Churn) 
head(trainPred)
head(train$Churn)

#Predicting on the validation set and checking the Confusion Matrix.
testPred <- predict(model.rf, validation)
confusionMatrix(validation$Churn, testPred)
table(testPred, validation$Churn)
head(testPred)
head(train$Churn)

table(train$Churn)
3614/4922

#Checking the variable Importance Plot
varImpPlot(model.rf)

'''Below is the variable importance plot,
that shows the most significant attribute 
in decreasing order by mean decrease in Gini. 
The Mean decrease Gini measures how pure 
the nodes are at the end of the tree. Higher the Gini Index, 
better is the homogeneity.'''

model.rf <- randomForest(Churn ~ ., 
                           data=train, proximity=FALSE,importance = FALSE,
                         ntree=500,mtry=4, do.trace=FALSE)
model.rf



importance(model.rf)
varUsed(model.rf)
getTree(model.rf, 1, labelVar = TRUE)
plot(model.rf)

# ROC
# true positive rate 
261 / (261+146)
# false positive rate 
300/(300+1403)
options(repr.plot.width =10, repr.plot.height = 8)
rf.roc <- roc(response = validation$Churn, predictor = as.numeric(testPred))
plot(rf.roc) 


#Attributes of Radom Forest
attributes(model.rf)
model.rf$confusion
#Model tunning 
t <- tuneRF(train[,-29], train[,29],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 500,
            trace = TRUE,
            improve = 0.05)

model.rf <- randomForest(Churn ~ ., data=train, proximity=FALSE,importance = TRUE,
                         ntree=500,mtry=18, do.trace=TRUE)
model.rf




