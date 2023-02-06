require(dplyr)
require(data.table)
require(lubridate)
require(ggplot2)

# 1) Import and Preprocess Data
# a) First, import the datasets using the following links
data_1999 <- fread("https://bit.ly/3c4AHbL",colClasses = c(rep("character",5), rep("numeric",7)))
data_2012 <- fread("https://bit.ly/3nZicL2",colClasses = c(rep("character",5), rep("numeric",7)))
str(data_1999)
str(data_2012)

# b) Take a look at the 1999 data
dim(data_1999) # (1) dimensions 
head(data_1999,3) # (2) first 3 rows

# c) The variable of our interest is Sample.Value
summary(data_1999$Sample.Value) # (3) summary statistics
# i) Compute the number of NAs and calculate the proportions.
na_table <- table(is.na(data_1999$Sample.Value))
totalnum_ob <- length(data_1999$Sample.Value)
propotion <- na_table/totalnum_ob;propotion
# ii) (4) percentage of the PM2.5 observations that are missing
paste("PM2.5 NA: ", round(propotion[2],5)*100,"%")

# d) Bind the 1999 data and 2012 data and subset the years from the Date variable
pm <- rbind(data_1999, data_2012)
pm$year <- year(ymd(pm$Date))
pm$year <- as.factor(pm$year)
str(pm)

# e) rename the Sample.Value variable to PM
colnames(pm)[which(names(pm) == "Sample.Value")] <- "PM"
colnames(pm)

# 2) Data Exploration with Visualization using ggplot2
# a) # (5) set the seed at 2021 and draw 1,000 randomly selected samples
set.seed(2021)
ran_sam <- sample_n(pm,1000)
str(ran_sam)

# b) create boxplots of all monitor values in 1999 and 2012 using the randomly sampled data
# (6) take the log of the PM values (with base 2)
ran_sam$Log2PM2.5 <- log2(ran_sam$PM)
# (7) label the title, x-axis & y-axis & (8) use the base white theme
x <- ggplot(ran_sam, aes(year, Log2PM2.5, col = year))
x + 
  geom_boxplot() + 
  theme_bw() + 
  xlab("Year") + ylab("Log2 PM2.5") +
  ggtitle("Boxplots of PM values in 1999 and 2012")

# c) (9) Describe what you observe in terms of the average and variance
print("The average of the log of the PM values is lower in 2012 than 1999.")
print("The variance of the log of the PM values is larger in 2012 than 1999.")

# d) identify a monitor in New York State that has data in 1999 and 2012 
# (10) Subset data
pm_NY <- filter(pm, State.Code == 36)
new_pm_NY <- select(pm_NY, County.Code, Site.ID)
str(new_pm_NY)

# e) (11) Create a new variable called Site.Code
pm_NY$Site.Code <- paste(new_pm_NY$County.Code,new_pm_NY$Site.ID,sep = '.')
head(pm_NY$Site.Code)

# f) (12) Find the intersection of the sites in between 1999 and 2012
s <- split(pm_NY,pm_NY$year); s
monitors <- intersect(s$`1999`$Site.Code,s$`2012`$Site.Code); monitors

# g) choose one of 10 monitors that had the most observations.
# 13) identify the monitor in the original data (pm) that had the most data
pm$Site.Code <- paste(pm$County.Code,pm$Site.ID,sep = '.')
pm_monitors <- filter(pm, Site.Code %in% monitors) 
pm_monitors$count <- 1
group_by(pm_monitors, Site.Code) %>%
  summarise(num = sum(count)) %>%
  arrange(desc(num))

# h) (14) Subset the data(pm)
pmsub <- subset(pm, State.Code == '36' & County.Code == '101' & Site.ID == '0003')
str(pmsub)

# i) (15) convert the Date variable into a date obj. and create a variable called yday
pmsub$Date <- ymd(pmsub$Date)
pmsub$yday <- yday(pmsub$Date)
head(pmsub)

# j) Draw a scatter plot
# (16) label the x-axis 
# (17) separate the plots using the facet function
# (18) use the base white theme
g <- ggplot(pmsub, aes(yday, PM))
g +
  geom_point() +
  facet_grid(. ~ year) +
  xlab("Day of the Year") +
  theme_bw()







