install.packages("haven")            
library(haven)
install.packages("rpart")         
library(rpart)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")         
library(tidyr)
install.packages("lubridate")            
library(lubridate)
install.packages("timeDate")          
library(timeDate)
install.packages('readxl')         
library(readxl)


##taking in data ##################
data_analytic <- read_sas("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment//AnalyticDataInternetGambling.sas7bdat")
head(data_analytic)


data_demographics <- read_sas("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/RawDataIDemographics.sas7bdat")
head(data_demographics)



data_pokerchips <- read_sas("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/RawDataIIIPokerChipConversions.sas7bdat")
head(data_pokerchips)

data_aggregation <- read_sas("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/RawDataIIUserDailyAggregation.sas7bdat")

head(data_aggregation)

count(data_aggregation, ProductID)

### spliting aggregate date into month, year and day  ################
sapply(data_aggregation, class)


data_aggregation_1 = data_aggregation %>% separate(Date, into = c('Year', 'Date'), sep = 4, convert = TRUE)
data_aggregation_2 =  data_aggregation_1%>% separate(Date, into = c('Month', 'Day'), sep = -2, convert = TRUE)
head(data_aggregation_2)

data_aggregation_4 <- mutate(data_aggregation_2, Date = paste(Day,Month))
data_aggregation_4$Date <- paste(data_aggregation_4$Day, data_aggregation_4$Month, data_aggregation_4$Year, sep="/")

sapply(data_aggregation_4, class)
##### Specifiying aggregation dates as weekdays and weekends ################


data_aggregation_4$Date <- as.Date(data_aggregation_4$Date)
sapply(data_aggregation_4, class)
data_aggregation_4$weekday = wday(data_aggregation_4$Date, label=TRUE)


data_aggregation_4$weekday2 = isWeekday(data_aggregation_4$Date, wday=1:5)
data_aggregation_6 <- data_aggregation_4 %>% group_by(UserID,weekday2) %>% summarise( Sum_bets = sum(Bets))
data_aggregation_6 <- spread(data_aggregation_6,weekday2, Sum_bets)

names(data_aggregation_6)[2]<-paste("Number of bets in Weekend")
names(data_aggregation_6)[3]<-paste("Number of bets in Weekdays")


#############cleaning of pokerchips dataset###################

sapply(data_pokerchips, class)


data_pokerchips_2 <- data_pokerchips %>% group_by(UserID, TransType) %>% summarise( Sum_trans_amt = sum(TransAmount))
data_pokerchips_2 <-spread(data_pokerchips_2,TransType, Sum_trans_amt)

names(data_pokerchips_2)[2]<-paste("Sell")
names(data_pokerchips_2)[3]<-paste("Buy")


data_pokerchips_2$Sell[is.na(data_pokerchips_2$Sell)]<-0
data_pokerchips_2$Buy[is.na(data_pokerchips_2$Buy)]<-0
data_pokerchips_2$total_trans <- data_pokerchips_2$Buy - data_pokerchips_2$Sell
data_pokerchips_2$product_id  <- 3 

data_pokerchips_2$freq<- count(data_pokerchips, UserID)  

data_pokerchips_3<- data_pokerchips %>% group_by(UserID) %>% summarise( count_userid = n())

##########  Counting products played by different customers ######################

data_aggregation_5<- data_aggregation%>% group_by(UserID, ProductID) %>% summarise( count_productid = n())

data_aggregation_5 <- merge(data_aggregation_5, data_pokerchips_3, all = TRUE)

data_aggregation_5 <-spread(data_aggregation_5,ProductID, count_productid)
names(data_aggregation_5)[2] <- paste("3")

data_aggregation_5 <-data_aggregation_5[c(1,3,4,2,5,6,7,8)]

sapply(data_aggregation_5, class)

data_aggregation_7 <- data_aggregation_5[c(3,4,2,5,6,7,8)]
data_aggregation_7 <- data_aggregation_7[c(2,1,4,3,5,6,7)]



#######Returning maximum producrtID used by every customer################
data_aggregation_7$max <- apply(data_aggregation_7,1,max,na.rm=TRUE)
data_aggregation_7$max_pro <- colnames(data_aggregation_7)[apply(data_aggregation_7,1,which.max)]

data_aggregation_7 <- data_aggregation_7[9]


####### Merging the max product used by the customer into the basetable #############


data_aggregation_7  =  cbind(data_aggregation_7, data_aggregation_5)
data_aggregation_7  =  data_aggregation_7[c(2,1)]
basetable  =  merge(data_aggregation_6, data_aggregation_7, by.x = "UserID", all = TRUE)



###### Calculating Average spending behaviour of customer per month from Aggregation table############

data_aggregation_2 <- data_aggregation_2 %>% group_by(UserID, Month) %>% summarise( mean(Stakes))
names(data_aggregation_2)[3] <- paste("mean stakes per month")
data_aggregation_2 <-spread(data_aggregation_2,Month, "mean stakes per month")
data_aggregation_2 <- data_aggregation_2[c(2,3,4,5,6,7,8,9)]
data_aggregation_2$max_spending_month <- colnames(data_aggregation_2)[apply(data_aggregation_2,1,which.max)]


sapply(data_aggregation_2,class)
data_aggregation_2[is.na(data_aggregation_2)]<-0
data_aggregation_2$avg_spending_per_month <- rowMeans(data_aggregation_2[,c(1,2,3,4,5,6,7,8)], na.rm=TRUE)

data_aggregation_2 <- data_aggregation_2[c(9,10)]
data_aggregation_2_1 <- data_aggregation_2[c(1)]
data_aggregation_2_2 <- data_aggregation_2[c(2)]
#### MErging the metrics into the basetable ###########

length(data_aggregation_2$max_spending_month)
length(data_aggregation_2$avg_spending_per_month)
length(data_aggregation_6$`Number of bets in Weekend`)
length(data_aggregation_6$`Number of bets in Weekdays`)
length(data_aggregation_6)

data_aggregation_2_3 <- cbind(data_aggregation_2_1, data_aggregation_2_2) 


data_aggregation_2_3 <- cbind(data_aggregation_6$UserID,data_aggregation_2_3)
names(data_aggregation_2_3)[1] = "UserID"


###### Merging some demographics of customers in the basetable ####################

basetable_2 <- merge( basetable,  data_demographics, by.x = "UserID", all= TRUE)
basetable <- basetable_2[c(1,2,3,4,5,6,14,15)]

basetable <- merge( basetable,  data_aggregation_2_3, by.x = "UserID", all= TRUE)


##### Renaming the columns in the basetable ###############

#names(basetable)[10] <- paste("Max spending per month")
#names(basetable)[2] <- paste("Number of bets in Weekend")
#names(basetable)[3] <- paste("Number of bets in weekdays")
#names(basetable)[4] <- paste("Maximum Product used")


########Calculating Recency of the customers##################


data_aggregation_4$date <- as.Date(with(data_aggregation_4, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
data_aggregation_recency <- data_aggregation_4[c("UserID","date")] %>% group_by(UserID) %>% summarise(Last_played =  max(date))
sapply(data_aggregation_recency, class)

data_aggregation_recency$last_date <- as.Date("2005-09-30")

data_aggregation_recency$recency <- data_aggregation_recency$last_date - data_aggregation_recency$Last_played

basetable_3<- merge(basetable,data_aggregation_recency, by.x = "UserID", all = TRUE )
basetable_3 = basetable_3[c(1,13)]

basetable <- merge(basetable, basetable_3, by.x = "UserID", all = TRUE)
names(basetable)[10] <- paste("Days since customer played last")

####### Replacing country codes with country names #######

country <- read_excel("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/Country_data.xls", col_names = TRUE)
head(country)

basetable <- merge(basetable, country, by = "Country", all = TRUE)
basetable<- basetable[-which(is.na(basetable$UserID)), ]
basetable<- basetable[,-1 ]


#############################################################################################################
######### Shaping final basetable ###############
basetable <- basetable[c(1,2,3,4,5,6,7,8,10,11)] 

basetable <- merge(basetable, data_aggregation_21, by = "UserID", all = TRUE)
basetable <- basetable[c(1,2,3,4,5,6,7,8,9,10,15)] 

######### Renaming the columns of basetable ###########


names(basetable)[2] <- paste("Number of bets in Weekend")
names(basetable)[3] <- paste("Number of bets in weekdays")
names(basetable)[4] <- paste("Maximum Product used")
names(basetable)[5] <- paste("Country")
names(basetable)[6] <- paste("Language")
names(basetable)[7] <- paste("ApplicationID")
names(basetable)[8] <- paste("Gender")
names(basetable)[9] <- paste("Recency")
names(basetable)[10] <- paste("Maximum spending month")

                                                                          
###### Replacing codes with relevant values ############
datatab <- read.csv("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/basetable_main.csv")

product_codes <- read_excel("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/pro_codes.xlsx", col_names = TRUE)
names(product_codes)[1] <- paste("Maximum.Product.used")
datatab <- merge(datatab, product_codes, by.x = "Maximum.Product.used", all = TRUE)
datatab <- datatab[,-1]
names(datatab)[12] <- paste("Product_used_most")


language_codes <- read_excel("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/pro_lang.xlsx", col_names = TRUE)

datatab <- merge(datatab, language_codes, by.x = "Language", all = TRUE)
datatab <- datatab[,-1]
names(datatab)[12] <- paste("Language")

application_codes <- read_excel("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/pro_app.xlsx", col_names = TRUE)
datatab <- merge(datatab, application_codes, by.x = "ApplicationID", all = TRUE)
datatab <- datatab[,-1]
datatab<- datatab[-which(is.na(datatab$Country.Name)), ]
datatab$Gender[datatab$Gender == 0] <- "Female"
datatab$Gender[datatab$Gender == 1] <- "Male"

month_codes <- read_excel("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/pro_month.xlsx", col_names = TRUE)
datatab <- merge(datatab, month_codes, by.x = "Maximum.spending.month", all = TRUE)
datatab <- datatab[,-1]
datatab<- datatab[-which(is.na(datatab$UserID)), ]

write.csv(datatab, "C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/basetable_update.csv")
#### Adding total amount spent by customer variable ######

data_total_spent <- data_aggregation %>% group_by(UserID) %>% summarise( total_spent = sum(Stakes))
datatab <- merge(datatab, data_total_spent, by.x = "UserID", all = TRUE)

datatab2 <- datatab
datatab2 <- (datatab2)[-2] 
names(datatab2)[2] <- paste("No_bets_weekend")
names(datatab2)[3] <- paste("No_bets_weekdays")
names(datatab2)[7] <- paste("Country")
datatab2<- datatab2[-which(is.na(datatab2$Country)), ]
datatab2$Gender[datatab2$Gender == 0] <- "Female"
datatab2$Gender[datatab2$Gender == 1] <- "Male"
write.csv(datatab2, "C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/basetable_update.csv")

## testing the final basetable ###
test22 <- read.csv("C:/Users/hpaliwal/Desktop/Open-source-programming-master/Group Assignment/basetable_update.csv")
