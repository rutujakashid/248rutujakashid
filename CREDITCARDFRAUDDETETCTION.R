#PROJECT :- CREDIT CARD FRAUD DETECTION

#loading dataset
credit_card <-read.csv('C:\\Users\\DELL\\Documents\\creditcard1.csv')

#Glance at the structure of data set
str(credit_card)

#class is actual categorical column
#first convert the class column to factor column with two levels 0 and 1
credit_card$Class <- factor(credit_card$Class, levels = c(0, 1))
#str(credit_card)

#get the summary of dataset
summary(credit_card)

#count missing values
sum(is.na(credit_card))

#get the distribution of fraud and legitimate transaction in the dataset
table(credit_card$Class)

#get the percentage of fraud and legitimate transaction in the dataset
prop.table(table(credit_card$Class))


#pie chart of credit card transacion
labels <- c("legit","fraud")
labels <-paste(labels,round(100*prop.table(table(credit_card$Class)),2))
labels <-paste(labels, "%")
pie(table(credit_card$Class), labels , col = c("orange","red"),
    main = "pie chart of credit card transaction")

# no model predictions
#if we predict that every single transaction in this data set is legitimate we can what type of accuracy we get
#first of all suppose all the transaction are legitimate transaction
predictions <- rep.int(0, nrow(credit_card))
predictions <- factor(predictions, levels = c(0, 1))
#predictions

#now we'll use the confusion maatrix to get the accuracy for this particular prediction that we done without
#..using any model
install.packages('caret')
install.packages('e1071', dependencies=TRUE)
library(caret)
confusionMatrix( data = predictions , reference = credit_card$Class)
#true positive 284315 
#true negative 0




#here our aim is to maximize true negative ../to consider fraud transaction as fraud transaction
--------------------------------------------------------------------------------------------------------
#firstly we will take smaller version of dataset and then apply the model on whole dataset later on
library(dplyr)
set.seed(1)
credit_card <- credit_card %>% sample_frac(0.1)#takes 10% of the rows

table(credit_card$Class)
#0-28437
#1-44




#scatter plot between variable v1 and v2
library(ggplot2)
ggplot(data = credit_card, aes(x = V1 , y = V2, col = Class)) +
  geom_point()+
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2', 'red'))

#--------------------------------------------
#creating training and test sets for fraud detection model

install.packages('caTools')
set.seed(123)
data_sample = sample.split(credit_card$Class,SplitRatio = 0.80)
train_data = subset(credit_card, data_sample == TRUE)
test_data = subset(credit_card, data_sample == FALSE)

dim(train_data)
dim(test_data)
#---------------------------------------------------
#before building the model we will balance the dataset
#sampling always done on training set

#if we build model on any unbalanced dataset ..classifier tends to favour majority class
#..which cause large classification error over fraud cases as it did not learn much from the data available

#classifier learn better from balanced distributtion of data


#Random Over- Sampling(ROS)
#we over sample the minority class i.e fradulent class
#create duplicates of already present fraud cases
#just copy the same data multiple times till we reach particular threshold

table(train_data$Class)

n_legit <- 22750
new_frac_legit<- 0.50
new_n_total <- n_legit/new_frac_legit
#45500

install.packages('ROSE')
library(ROSE)
oversampling_result <- ovun.sample(Class ~ . ,
                                   data = train_data,
                                   method = "over",
                                   N = new_n_total,
                                   seed = 2019)

oversampled_credit <- oversampling_result$data
table(oversampled_credit$Class)
ggplot(data = oversampled_credit, aes(x= V1, y=V2 ,col = Class))+
  geom_point(position = position_jitter(width=0.1)) +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2','red'))
#--------------------------------------------------------------------

#we under sample the majority class legitimate one
#remove some data from legitimate class to distribute data equally between legitimate and fraudulent
#Random Under- Sampling(RUS)
table(train_data$Class)

n_fraud <- 35
new_frac_fraud<- 0.50
new_n_total <- n_fraud/new_frac_fraud
#new_n_total
#70

#install.packages('ROSE')
library(ROSE)
undersampling_result <- ovun.sample(Class ~ . ,
                                   data = train_data,
                                   method = "under",
                                   N = new_n_total,
                                   seed = 2019)

undersampled_credit <- undersampling_result$data

table(undersampled_credit$Class)
ggplot(data = undersampled_credit, aes(x= V1, y=V2 ,col = Class))+
  geom_point(position = position_jitter(width=0.1)) +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2','red'))
#-----------------------------------------------------------------



#SMOTE synthetic minority over sampling technique
#over sample minority class by creating synthetic fraud cases
# we find the k nearest neighours of a fraud case suppose X
#find three nearest neighbor i,e k=3
#randomly choose X's nearest neighbor ed.Y
#than synthetic sample between X and Y
#find x and y coordinates of both the point ang find x and y coordinate of synthetic point
#synthetic point lie on the line joining point X and Y
#we do this multiple times to get balanced dataset

install.packages("smotefamily")
library(smotefamily)
table(train_data$Class)

#set the number of fraud and legitimate cases , and the desired percentage of legitimate cases
n0<-22750
n1<-35
r0<-0.6
#n0-legitimate cases
#n1-fraud cases
#r0<-it is the ratio we want after the SMOTE ...we want 60% of rows should be legitimate cases and 40% should be fraudulent


# we will add synthetic sample in such a way that after adding synthetic sample we'll get 60% legitimate cases and 40% fraudulent


#now  get the no. of times we want to perform smote
#calculate the value for dup_size parameter of smote
ntimes<-((1-r0)/r0)*(n0/n1)-1
#ntimes

smote_output = SMOTE(X=train_data[ , -c(1,31)],
                     target = train_data$Class,
                     K=5,
                     dup_size = ntimes)

credit_smote<- smote_output$data

#changing column name from class to Class
colnames(credit_smote)[30]<-"Class"

#using prop.table function we can see the distributtion of legitimate and fraudeulent case
prop.table(table(credit_smote$Class))


#class distribution for original dataset
library(ggplot2)
ggplot(data=train_data, aes(x=V1 , y=V2, color=Class))+
  geom_point()+
  scale_color_manual(values = c('blue','red'))
  
#class distribution for oversampled  dataset using smote 
ggplot(data=credit_smote, aes(x=V1 , y=V2, color=Class))+
  geom_point()+
  scale_color_manual(values = c('blue','red'))

#-----------------------------------------------------------------------------------------
#now built decision tree on top of this data...so that we can predict whether the transaction fradulent or not

install.packages('rpart')
install.packages('rpart.plot')

#we will use rpart to bulit classification and regression trees
library(rpart)
#rpart.plot to plot the classification tree
library(rpart.plot)

CART_model <- rpart(Class ~ . , credit_smote)
rpart.plot(CART_model, extra = 0 , type = 5 , tweak =1.2)
#extra=0 .if don't want any extraa information onleaves
#type=5...will display the shapes used
#tweak =1.2 ...enlarges the text by 120%

#now predict the values on top of the test dataset..to see how many transactions are correctly and incorrectly classified
#pedict fraud cases
predicted_val <- predict(CART_model, test_data,type= 'class')
#predicted_val


#build confusion matrix
#to know how many predictions are correct and in correct
library(caret)
confusionMatrix(predicted_val , test_data$Class)
#out 9 fradulent transaction 7 cases are correctly classified




#now predicting on whole dataset
predicted_val <- predict(CART_model, credit_card[,-1],type= 'class')
confusionMatrix(predicted_val , credit_card$Class)


