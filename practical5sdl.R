
#upload dataset
dataset<-read.csv('C:\\Users\\DELL\\Documents\\datafile1.csv')

#to view first 10 rows
dataset[1:10, ]

#dimensions of dataset
dim(dataset)

#str shows variable types
str(dataset)



#summary of all columns of dataset
summary(dataset)
#summary of an individual column
summary(dataset$modal_price)


#mean
mean(dataset$modal_price)
mean(dataset$modal_price,na.rm=TRUE)

#median
median(dataset$modal_price,na.rm=TRUE)    

#standard deviation
sd(dataset$modal_price,na.rm=TRUE)

#percentiles/quantiles
quantile(dataset$modal_price, probs = c(.25,.75), na.rm = TRUE)

#minimum value in column modal_price
min(dataset$modal_price)

#maximum value in column modal_price
max(dataset$modal_price)


