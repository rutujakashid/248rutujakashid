#upload dataset
dataset<-read.csv('C:\\Users\\DELL\\Documents\\covid-19.csv')

#to view first 10 rows
dataset[1:10, ]

#dimensions of dataset
dim(dataset)


#summary of all columns of dataset
summary(dataset)

x=dataset$Positives.Monthly
print(x)
y=dataset$Deaths.Monthly
print(y)
z=dataset$Recovered.Monthly
print(z)

#scatter plot
plot(x,y,xlab="positive cases",ylab="death cases")


#line of best fit
abline(lm(y~x))


#computing corelation
cor(x,y)

cor.test(x,y)


#linear regression model
model = lm(y~x)
print(model)


#more information about model
summary(model)

#multiple regression model
model1 = lm(y~x+z)
print(model1)

a<-coef(model1)[1]
print(a)

x1<-coef(model1)[2]
z1<-coef(model1)[3]
print(x1)
print(z1)
#based on the intercept and coefficient values ,create the mathematical equation
R=a+x1*x+z1*z
#R=12.693421+0.004667*x+0.021619*z
#above equation can be used for predicting new values
R=12.693421+0.004667*48+0.021619*14
print(R)
