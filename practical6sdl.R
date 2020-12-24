#upload dataset
mydata<-read.csv('C:\\Users\\DELL\\Documents\\Maharashtra-Table2007.csv')

#show few rows of dataset
mydata[1:10, ]

#dimensions of dataset
dim(mydata)

#str shows variable types
str(mydata)


#summary of all columns of dataset
summary(mydata)


#checks the entire data set for NAs and return logical output
is.na(mydata)

#gives total NA present in the columns
colSums(is.na(mydata))


#replace na to 0 in column breed.of.horse.malani
mydata[is.na(mydata)] = 0


#data visualization
col_total<-mydata[,11]
View(col_total)
boxplot(col_total)

