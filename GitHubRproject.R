
library(readr) 
library(dplyr) 
library(ggplot2) 
library(DataExplorer) 
library(methods) 
library(recommenderlab) 
library(data.table) 
library(ggplot2) 
library(knitr) 
library(readxl)
library(arules) 
library(arulesViz) 
library(tidyverse)

#Data Exploration

Online_Retail <- read_excel("C:/Users/dathg/Desktop/BA with R/Project/Online R etail.xlsx") 
View(Online_Retail) 
str(Online_Retail) 
summary(Online_Retail) 
dim(Online_Retail) 
names(Online_Retail) 
#Explore individual variables 
length(unique(Online_Retail$InvoiceNo)) #we see there are 25900 unique invoice s i.e transactions made
# Loading the Dataset 
Online_Retail <- read_excel("C:/Users/anush/Desktop/BA with R/Project/Online R etail.xlsx") View(Online_Retail) str(Online_Retail) 
summary(Online_Retail) 
missmap(Online_Retail, main = "Missing values vs observed") 
Online_Retail1 <- na.omit(Online_Retail) #We create Online_Retail1 dataset aft er cleanign our data View(Online_Retail1) 
colSums(is.na(Online_Retail1))    
dim(Online_Retail1)    
class(Online_Retail1$InvoiceDate) 
head(Online_Retail1$InvoiceDate)                            
Date_Only <- as.Date(Online_Retail1$InvoiceDate)  #We separate date from time 
Online_Retail1$Date_Only <- as.Date(Online_Retail1$InvoiceDate) #create new da te only column head(Date_Only) 
Online_Retail1$Year <- year(Online_Retail1$Date_Only)     
Online_Retail1$Month <- month(Online_Retail1$Date_Only, label=T) 
Online_Retail1$Day <- day(Online_Retail1$Date_Only) 
Online_Retail1$Revenue <- Online_Retail1$Quantity * Online_Retail1$UnitPrice  
View(Online_Retail1) 
#After cleaning the data we plot it 

##Plot 1 - Month vs Invoices library(ggplot2) library(dplyr) filter <- Online_Retail1 %>% filter(Year==2011) %>% count(Month) View(filter) ##We see number of invoices is highest for Nov = 64,531 
ggplot(filter, aes(Month, n)) + ggtitle("Number of Invoices per Month")    +  
  geom_col() +   
  labs(x="Month", y="Number of invoices" #We confirm that transactions         were highest in the month of November 
       
       
 ##Plot 2 - Month vs Day of Invoices 
       filter_1 <- Online_Retail1 %>% filter(Year==2011) %>% group_by(Month, Day) %>%  count(Month) View(filter_1) 
       ggplot(filter_1, aes(Month, Day, size=n)) + geom_point() + ggtitle("Invoices i n 2011 by Month and Day") 
       
##Plot 3 - Revenue raked in by country 
       filter_2 <- Online_Retail1 %>% group_by(Country) %>% summarise(Revenue = sum(R evenue))  
       
       ggplot(filter_2, aes(y=Revenue/1000, x=Country)) + geom_bar(stat = "identity")
       + ggtitle("Revenue by Country") +   labs(x="Country", y="Sales in thousands") + coord_flip()  
#Plot 4- As the data is of UK we remove UK and check again for highest contrib utors 
       filter_3 <- Online_Retail1 %>% group_by(Country) %>% summarise(Revenue = sum(R evenue)) %>% filter(Country!="United Kingdom") 
       ggplot(filter_3, aes(y=Revenue/1000, x=Country)) + geom_bar(stat = "identity")
       + ggtitle("Revenue by Country except UK") +     labs(x="Country", y="Sales in thousands") + coord_flip() 
       
       
#Plot 5 - Revenue by Month 
       filter_4 <- Online_Retail1 %>% group_by(Month) %>% summarise(Revenue = sum(Rev enue)) 
       ggplot(filter_4, aes(y=Revenue/1000, x=Month)) + geom_bar(stat = "identity") +  ggtitle("Revenue by Month") +       labs(x="Month", y="Revenue") 
       
       
#plot 6 - Split transactions by weekday 
       Online_Retail1$Week_day <- wday(Online_Retail1$Date_Only, label = T) filter_5 <- Online_Retail1 %>% filter(Year==2011) %>% count(Week_day) 
       ggplot(filter_5, aes(Week_day, n)) + ggtitle("Invoices by Days of the Week") +  geom_col() + labs(x = "Days of the Week", y = "Number of Invoices")
       
       #Plot 7 - Split revenue by weekday 
       filter_6 <- Online_Retail1 %>% group_by(Week_day) %>% summarise(Revenue = sum( Revenue)) 
       ggplot(filter_6, aes(y=Revenue/1000, x=Week_day)) + geom_bar(stat = "identity"
       ) + ggtitle("Revenue by Week") +       labs(x="Days of the week", y="Revenue") 
       
       
#PLot 8 - Revenue raked by Country in each month 
       filter_7 <- Online_Retail1 %>% filter(Year==2011) %>% filter(Country!="United Kingdom") %>% group_by(Month, Country) %>% count(Month) 
       ggplot(filter_7, aes(Month, Country, size=n)) + geom_point() + ggtitle("Invoic es in 2011 by Month and Country") 
       
       
#Plot 9 - Most popular product 
       filter_8 <- Online_Retail1 %>% group_by(StockCode, Description) %>% summarise( count= n()) %>% arrange(desc(count)) %>% head(n=15) 
       ggplot(filter_8, aes(x=Description, y=count)) + geom_bar(stat= "identity") + c oord_flip() +    labs(y="Number of items purchased", x="Product") 
       
       # Algorithm1 : Market Basket Analysis
       
       ## Creating final data set for the model ## Removing the missing entries in the item field flag1<-Retail_data$Description !="" 
       Online_Retail1wip2<- Online_Retail[flag1,c(1,3)] 
       ##converting Data frame to transactions 
       write.csv(Online_Retail1wip2, file = "Retail_trans.csv") 
       trans = read.transactions("Retail_trans.csv", format = "single", sep = ",", co ls = c("InvoiceNo", "Description")) inspect(trans[1:5]) 
       ## running the model and sorting the results by confidence 
       ## inspecting the rules 
       
       rules = apriori(trans,parameter = list(supp = 0.001,conf=0.8)) 
       rules<- sort(rules,by= "confidence",decreasing = "T") 
       inspect(rules[1:10]) 
       
       
#Algorithm 2  : Collaborative Filtering
       
       
       #Loading the Data set  
       EData_df <- fread("C:/Users/gowri/Desktop/Business Analytics with R/RProject/O nline Retail.csv") 
       class(EData_df) 
       
       head(EData_df,1) 
       
       EData_df[UnitPrice<=0,UnitPrice:=NA] 
       EData_df <- na.omit(EData_df) 
       
       #Sorting the data by stockcode   
       setkeyv(EData_df, c('StockCode', 'Description')) head(EData_df,3) 
      itemCode <- unique(EData_df[, c('StockCode', 'Description')]) 
       
       
       setkeyv(EData_df, NULL) 
       
       #Creation of a buying matrix  
       cast_df <- dcast(EData_df, CustomerID ~ StockCode, value.var = 'Quantity',fun. aggregate = sum, fill=0) head(cast_df[,3504:3508]) 
       
       CustId <- cast_df[,1] # Storing the Customer ID's in one table 
       cast_df <- cast_df[,-c(1,3504:3508)]   #Dropping the columns where customers h ave not bought an item 
       for (i in names(cast_df)) cast_df[is.na(get(i)), (i):= 0]
       split_train <- sample(x = c(TRUE, FALSE), size = nrow(df_train),replace = TRUE , prob = c(0.8, 0.2)) 
       Valid <- df_train[!split_train] Train <- df_train[split_train] 
       
       #Checking the default parameters of the recommender system 
       recommender_models <- recommenderRegistry$get_entries(dataType ="binaryRatingM atrix") recommender_models$IBCF_binaryRatingMatrix$parameters 
        
       IBCF_model <- Recommender(data = Train, method = method, parameter = parameter
       ) model_details <- getModel(IBCF_model) 
       
       
       #Prediction using the validation dataset 
       IBCF_predicted <-predict(object = IBCF_model, newdata=Valid,n = n_recommended,  type="topNList") as(IBCF_predicted,"list")[1:5] 

       
       user1 <- CustId[as.integer(names(IBCF_predicted@items[1]))] user1 
        
       vvv <- rownames(model_details$sim)[vvv] vvv 
        
       itemCode[vvv] 
       
       user1_buy <- EData_df[CustomerID==12349, sum(Quantity), by=StockCode] merge(itemCode,user1_buy, by='StockCode') 
       
       
                       
    
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
    