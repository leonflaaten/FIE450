
#Removing all variables from environment
rm(list=ls())

#Import data
NHY.daily = read.csv("NHY.OL-daily.csv")

#Cleaning data to get column names
colnames(NHY.daily) = NHY.daily[1,]
NHY.daily = NHY.daily[-1, ]
row.names(NHY.daily) = NULL

#Dropping all columns except date and closing price
NHY.daily = NHY.daily[, c("Date", "Price Close")]

#Rename columns
names(NHY.daily) = c("Date", "p")

#Setting date into date format
NHY.daily$Date = as.Date(NHY.daily$Date, format = "%d/%m/%Y")

#Ordering data
df = NHY.daily[order(NHY.daily$Date), ]
