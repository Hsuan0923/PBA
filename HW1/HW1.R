# a) Importing dara
data<-read.csv("/Users/hsuan/Desktop/PBA/HW/online_retail.csv")
head(data)
str(data)

# b)
# Convert InvoiceDate to date class
data$InvoiceDate<-strptime(data$InvoiceDate,"%m/%d/%y %H:%M")
data$InvoiceDate<-as.Date(data$InvoiceDate)
head(data)
# Subset data from 2011/07 to 2011/08
begin<-which(data$InvoiceDate == as.Date("2011-07-01"))[1] #245904
endnum<-which(data$InvoiceDate == as.Date("2011-09-01"))[1]-1 #320705
newdata<-data[begin:endnum,] # data from 2011/07 to 2011/08
head(newdata)
str(newdata)
tail(newdata)
length(unique(newdata$InvoiceNo)) #3664

# c) Use for-loops
for(i in 1:ncol(newdata)){
  print(names(newdata[i]))
  print(paste("column type:",class(newdata[[i]]))) # 2.types of each column
  print(paste("unique values:",length(unique(newdata[[i]])))) # 3.number of unique values in each column
  if (names(newdata[i]) == "Quantity"){
    print(paste("mean:",mean(newdata$Quantity))) # 1.mean of Quantity
  }else if (names(newdata[i])=="UnitPrice"){
    print(paste("mean:",mean(newdata$UnitPrice))) # 1.mean of UnitPrice
  }
}

# d) Subset the data for which the transactions took place in the U.K., Netherlands, and Australia.
coudata<-subset(newdata,Country == "Australia" | Country == "Netherlands" | Country == "United Kingdom" )
round(mean(coudata$UnitPrice),3) # 4. average of the UnitPrice 
round(sd(coudata$UnitPrice),3) # 4. standard deviation of the UnitPrice 
length(unique(coudata$InvoiceNo)) # 5. number of unique transactions
length(unique(coudata$CustomerID))# 6. customers residing in these countries

# e) who made a refund?
refundnum<-grep("C",coudata$InvoiceNo)
cust_refund<-na.omit(coudata$CustomerID[refundnum])
length(unique(cust_refund)) #337

# f) transactions for which the CustomerID is missing
totalsales<-0
trans<-c()
for (j in 1:length(is.na(coudata$CustomerID))){
  if (is.na(coudata$CustomerID)[j] == TRUE){
    Sales<-coudata$Quantity[j] * coudata$UnitPrice[j]
    totalsales<-totalsales + Sales
    trans[j]<-coudata$InvoiceNo[j]
  }
} #166454.3
print(totalsales) # 8. total sales amount
print(length(unique(trans))-1) # 9. number of transactions

# Extra Credit
# EC1) Create a variable containing the monthly aggregate spending for each customer
for(k in 1:nrow(newdata))
  newdata$totalspen[k]<-newdata$Quantity[k] * newdata$UnitPrice[k]

uni_cusID<-na.omit(unique(newdata$CustomerID))
cusID<-1:length(uni_cusID)
cost<-rep(0,length(uni_cusID))
result<-data.frame(cusID,cost,stringsAsFactors = FALSE)
for(m in 1:length(uni_cusID)){
  result$cusID[m]<- (uni_cusID)[m]
    for(l in 1:length(newdata$CustomerID)){
      if (is.na(newdata$CustomerID[l])){}
      else{
      if (newdata$CustomerID[l] == (uni_cusID)[m] ){
        result$cost[m] <- result$cost[m]+newdata$totalspen[l]
      }
    }
  }
}
head(result)

# EC2) the five customers who have spent the most money
data_july<-which(data$InvoiceDate == as.Date("2011-07-01"))[1] #245904
data_august<-which(data$InvoiceDate == as.Date("2011-08-01"))[1]-1 #320705
one_mon<-data[b:e,]
for(k in 1:nrow(one_mon))
  one_mon$totalspen[k]<-one_mon$Quantity[k] * one_mon$UnitPrice[k]
head(one_mon$totalspen)
unique(one_mon$CustomerID)
class(unique(one_mon$CustomerID))

uni_cusID<-na.omit(unique(one_mon$CustomerID))
cusID<-1:length(uni_cusID)
cost<-rep(0,length(uni_cusID))
arr<-data.frame(cusID,cost,stringsAsFactors = FALSE)
for(m in 1:length(uni_cusID)){
  arr$cusID[m]<- (uni_cusID)[m]
    for(l in 1:length(one_mon$CustomerID)){
      if (is.na(one_mon$CustomerID[l])){}
      else{
      if (one_mon$CustomerID[l] == (uni_cusID)[m] ){
        arr$cost[m] <- arr$cost[m]+one_mon$totalspen[l]
      }
    }
  }
}
result <-arr[order(arr$cost,decreasing = T),]
result[1:5,] # five customers who have spent the most money



