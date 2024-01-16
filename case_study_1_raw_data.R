library(tidyverse)
library(dplyr)
library(plyr)
library(readr)
library(ggplot2)
library(scales)
data_all_2 <- list.files(path = "c:/downloads/project_1", pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%                              
  bind_rows         # joins all the csv files  in the directory

View(data_all_2)
colnames(data_all_2)
glimpse(data_all_2)
str(data_all_2)

data_all_2 <- na.omit(data_all_2)     #drops the na's

data_all_2$ride_length <- (data_all_2$ended_at - data_all_2$started_at)#ride length
data_all_2$weekday <- weekdays(data_all_2$started_at)#puts a weekday column in
mean(data_all_2$ride_length, na.rm=TRUE) #Time difference of 957.1109secs
max(data_all_2$ride_length, na.rm=TRUE)#Time difference of 728178 secs

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
data_all_2_mode_weekday <- find_mode(data_all_2$weekday)#Saturday is the mode
data_all_2_mode_start_lat <- find_mode(data_all_2$start_lat)
data_all_2_mode_ride_length_minutes <- find_mode(data_all_2$ride_length_minutes)
dfm_mode_ride_length_minutes <- find_mode(dfm$ride_length_minutes)
dfc_mode_ride_length_minutes <- find_mode(dfc$ride_length_minutes)

colnames(data_all_2)

mean(data_all_casual$ride_length, na.rm=TRUE)#1376.173 sec
mean(data_all_member$ride_length, na.rm=TRUE)#727.8564 sec
data_all_casual_mode_weekday <- find_mode(data_all_2$weekday)#Saturday
data_all_member_mode_weekday <- find_mode(data_all_2$weekday)#Saturday

options(scipen=999) 

ggplot(data=data_all_2)+
  geom_bar(position="identity", alpha=.5, mapping = aes(x = weekday, fill = member_casual))+ 
  scale_fill_brewer(palette = "Pastel1", labels=c('Member', 'Casual')) + xlab("Weekday") +
  ylab("Count")+ theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white",color = "black"))+labs(fill = "Member or Casual")


# This section makes a single digit month column, then turns that into a season column
data_all_2$month<-as.numeric(format(data_all_2$started_at,"%m"))

data_all_2$season[data_all_2$month==12]<-"winter"
data_all_2$season[data_all_2$month==1]<-"winter"
data_all_2$season[data_all_2$month==2]<-"winter"
data_all_2$season[data_all_2$month==9]<-"autumn"
data_all_2$season[data_all_2$month==10]<-"autumn"
data_all_2$season[data_all_2$month==11]<-"autumn"
data_all_2$season[data_all_2$month==3]<-"spring"
data_all_2$season[data_all_2$month==4]<-"spring"
data_all_2$season[data_all_2$month==5]<-"spring"
data_all_2$season[data_all_2$month==6]<-"summer"
data_all_2$season[data_all_2$month==7]<-"summer"
data_all_2$season[data_all_2$month==8]<-"summer"
data_all_2_mode_season <- find_mode(data_all_2$season) #finds the mode of the season column

ggplot(data=data_all_2)+ 
  geom_bar(position="identity", alpha=.5, mapping = aes(x = season, fill = member_casual))

data_all_2$ride_length_minutes <- (data_all_2$ride_length/60)
data_all_2$ride_length_minutes <- as.numeric(as.character(data_all_2$ride_length_minutes))

data_all_2$ride_length_cat <- cut(data_all_2$ride_length_minutes, seq(0, 19, 30))
lapply(data_all_2, summary)
data_all_member <- filter(data_all_2, member_casual == 'member')

dfm <- data_all_member
dfm[dfm$ride_length_minutes <=0,] <-NA # turns negative numbers to NA. Makes histograms look good
dfc <- data_all_casual
dfc[dfc$ride_length_minutes <=0,] <-NA # turns negative numbers to NA. Makes histograms look good
dfx <- data_all_2
dfx[dfx$ride_length_minutes<=0,] <-NA # turns negative numbers to NA. Makes histograms look good

df2= dfx[dfx$ride_length_minutes<60,] # dumps values over 60
df3= dfm[dfm$ride_length_minutes<60,] # dumps values over 60
df4= dfc[dfc$ride_length_minutes<60,] # dumps values over 60

df2<- na.omit(df2)#dumps NA in the df2

any(is.na(df2$ride_length_minutes)) #bool if there are any NA in the column

#'Ride Length in Minutes'
hist(df2$ride_length_minutes, main='Histogram of Ride Length in Minutes(M&C)',
     xlab='Ride Length in Minutes', col='#98F5FF', breaks= 250)

#these two are just for my confirmation that M and C are nearly identical
#hist(df3$ride_length_minutes, main='Histogram of Ride Length in Minutes(M)',
#     xlab='Ride Length in Minutes', col='#98F5FF', breaks= 250)
#hist(df4$ride_length_minutes, main='Histogram of Ride Length in Minutes(C)',
#     xlab='Ride Length in Minutes', col='#98F5FF', breaks= 250)

#The season/rideable type bar plot
ggplot(data=df2)+ 
     geom_bar(position="identity", alpha=.8, mapping = aes(x = season, fill= rideable_type))+ 
     scale_fill_brewer(palette = "Pastel2", labels=c('Classic', 'Docked', 'Elecrtric')) + xlab("Season") +
     ylab("Ride Count") + theme(
       plot.margin = margin(1, 1, 1, 1, "cm"),
       panel.background = element_rect(fill = "white"),
       plot.background = element_rect(fill = "white",color = "black"))+labs(fill = "Rideable Type")

mean(data_all_casual$ride_length_minutes, na.rm=TRUE)#22.936 minutes
mean(data_all_member$ride_length_minutes, na.rm=TRUE)#12.13 minutes
mean(data_all_casual$ride_length_minutes, na.rm=TRUE)#22.936 minutes
