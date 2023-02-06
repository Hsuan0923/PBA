require(dplyr)
require(data.table)
library(lubridate)
library(ggplot2)
require(DescTools)
require(gridExtra)
require(vtable)
require(factoextra)

# 1) Import and Examine the Data
# a) Import the CSV file into R using fread() and take a look at the data
data <- fread("/Users/hsuan/Desktop/PBA/HW4/onlineRetail.csv")
dim(data)
head(data)
summary(data)

# b) Examine the data by printing out the unique number of customers, products purchased and transactions.
length(unique(data$CustomerID))
length(unique(data$StockCode)) # not sure
length(unique(data$InvoiceNo))

# 2) Compute the RFM Variables
# c) Convert the InvoiceDate into a date obj. then create a variable called Recency
data$InvoiceDate <- mdy_hm(data$InvoiceDate)
data$InvoiceDate <- as.Date(data$InvoiceDate) #only mdy left

sort_data <- arrange(data, desc(InvoiceDate))
cust_refund <- sort_data[!grep("C",sort_data$InvoiceNo),]
cus_data <- distinct(cust_refund, CustomerID, .keep_all = TRUE)
cus_data$Recency <- ymd(20111209)-cus_data$InvoiceDate

# d) Create a variable called Frequency and Monetary for each customer in the data
freq <- group_by(cust_refund, CustomerID) %>%
  summarise(Frequency = length(unique(InvoiceNo)))
RF_data <- full_join(cus_data, freq, by=c("CustomerID"));

money <- group_by(cust_refund, CustomerID) %>%
  summarise(Monetary = sum(UnitPrice*Quantity))
RFM_data <- full_join(RF_data, money, by=c("CustomerID")); RFM_data

# 3) Removing Outliers (i.e., Winsorizing)
# e) Visualize the RFM variables with box plots.

par(mfrow=c(1,3))
boxplot(RFM_data$Recency, xlab ="Recency")
boxplot(RFM_data$Frequency, xlab="Frequency")
boxplot(RFM_data$Monetary,xlab="Monetary")

# f) Remove these extreme values/outliers by keeping only the values that are within the 99th percentile.
RFM_data$Recency <- Winsorize(RFM_data$Recency,probs = c(0.01, 0.99))
RFM_data$Frequency <- Winsorize(RFM_data$Frequency,probs = c(0.01, 0.99))
RFM_data$Monetary <- Winsorize(RFM_data$Monetary,probs = c(0.01, 0.99))

par(mfrow=c(1,3))
boxplot(RFM_data$Recency, xlab ="Recency")
boxplot(RFM_data$Frequency, xlab="Frequency")
boxplot(RFM_data$Monetary,xlab="Monetary")

# 4) Scaling the Variables
# g) Create another data.table obj. called RFM_Scaled which contains the CustomerID and the standardized RFM variables.
RFM_Scaled <- data.frame(RFM_data$CustomerID,scale(RFM_data$Recency),scale(RFM_data$Frequency),scale(RFM_data$Monetary))

# 5) Running K-Means Clustering
# h) Convert RFM_Scaled to a matrix.
RFM_Matrix <- as.matrix(RFM_Scaled[,-1])
colnames(RFM_Matrix) <- c("Recency","Frequency", "Monetary")

# i) Set seed at 2021 and run k-means clustering (set k = 4).
set.seed(2021)
km.out <- kmeans(RFM_Matrix, centers = 4); km.out

# j) Attach the cluster numbers (i.e., km.out$cluster) onto RFM_Scaled.
RFM_Scaled$cluster <- km.out$cluster
# data before scaling
RFM <- data.frame(RFM_data$Recency,RFM_data$Frequency,RFM_data$Monetary)
colnames(RFM) <- c("Recency","Frequency", "Monetary")
RFM$cluster <- km.out$cluster

# 6) Examining the Clusters
# k) Compute the average of RFM for each cluster. Do we observe any difference between the clusters? Can we label them? Which of the clusters do you think are the most suitable for us to run target marketing campaigns and how?
RFM$Recency <- as.numeric(RFM$Recency)
st(RFM, group = 'cluster') # Summary Statistics
cat("As shown on the table, cluster 2 has the shorter average on Recency, and higher average on Frequency and Monetary. 
    They are the most valuable customer, also have high activity and loyalty to the brand. 
    Hence, I think cluster 2 is the most suitable for us to run target marketing campaigns.
    We need to develope strategies to maintain these customer.")

# l) Print out the top 5 most selling products in terms of sales revenue (i.e., sum of sales amount = quantity x unit price) for each cluster.
RFM_data$cluster <- km.out$cluster
top_sell <- select(RFM_data, InvoiceNo, StockCode, Description, Quantity, UnitPrice, CustomerID, cluster)
top_sell$amount <- top_sell$UnitPrice*top_sell$Quantity
top_sell <- arrange(top_sell,desc(amount))

cl1 <- subset(top_sell,cluster==1)
head(cl1[,c(2,3,7,8)],5)
cl2 <- subset(top_sell,cluster==2)
head(cl2[,c(2,3,7,8)],5)
cl3 <- subset(top_sell,cluster==3)
head(cl3[,c(2,3,7,8)],5)
cl4 <- subset(top_sell,cluster==4)
head(cl4[,c(2,3,7,8)],6)

# Extra Credit
# EC1) Compute purchase frequency of the top 5 selling products by month and visualize it using ggplot2.
cust_refund$amount <- cust_refund$Quantity*cust_refund$UnitPrice
cust_refund_na <- na.omit(cust_refund)
top <- sort(tapply(cust_refund_na$amount, cust_refund_na$StockCode, sum),decreasing=TRUE)
head(top,5)
top5 <- names(top)[1:5]
mon_top5 <- filter(cust_refund_na, StockCode %in% top5)
mon_top5$mon <- month(mon_top5$InvoiceDate)

ggplot(mon_top5, aes(x=mon, y= length(unique(InvoiceNo)))) + 
  facet_wrap(~Description, ncol=3) + 
  geom_bar(stat="identity") + 
  labs(title = "Sales by month", x = "Month", y = "Number of Purchases") 

# EC2) Do we observe any seasonality? Please explain verbally.
cat("Yes, we observe some seasonalities. Take WHITE HANGING HEART T-LIGHT HOLDER for example, we see that there is a better selling in the winter.")

# EC3) Explain whether k = 4 is a reasonable decision using the Elbow/Silhouette method.
fviz_nbclust(RFM_Matrix, FUNcluster = kmeans, method = "wss") +
  labs(title="Elbow Method for K-Means") +
  geom_vline(xintercept = 3, linetype = 2)   
fviz_nbclust(RFM_Matrix, FUNcluster = kmeans, method = "silhouette") +
  labs(title="Silhouette Method for K-Means")
cat("Based on Elbow and Silhouette method, we could say that k = 4 is not a reasonable decision.")