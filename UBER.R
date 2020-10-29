#The data of a driver's uber trips are available for year 2016. 
#Your manager wants you to explore this data to give him some useful 
#insights about the trip behaviour of a Uber driver.

Dataset -
#The dataset contains Start Date, End Date, Start Location, 
#End Location, Miles Driven and Purpose of drive (Business, Personal, Meals, Errands, Meetings, Customer Support etc.)
rm(list = ls())
library(tidyverse)
ub=read.csv("uberdrive.csv",na.strings =c("",NA))
ub
dim(ub)
summary(ub)
str(ub)
sapply(ub, function(x) sum(is.na(x)))
table(ub$PURPOSE.)
sapply(ub, function(x) sum(is.na(x)))

#Renaming the columns
colnames(ub)=c("START_DATE","END_DATE","CATEGORY","START","STOP","MILES","PURPOSE")
colnames(ub)
str(ub)
table(ub$PURPOSE)
ub$PURPOSE=as.factor(ub$PURPOSE)

#checking for na in purpose
ub%>%filter(is.na(PURPOSE))

#filtering data
ub%>%filter(MILES>30)
ub%>%filter(MILES==30)
ub%>%filter(MILES!=30)

# Show the top 10 rides (*in terms of distance driven)
ub%>%arrange(desc(MILES))%>%top_n(n=10,wt=MILES)

#Remove the last row which has some anomalous number in MILES
ub%>%filter(MILES < 1000)->ub1
dim(ub1)
str(ub1)

#Dropping rows which have null values
ub2=na.omit(ub1)
dim(ub2)

# Get the unique starting point, unique destination
# names of unique start points
ub1%>%distinct(START)%>%nrow()
ub1%>%distinct(STOP)%>%nrow()

#Stations which are appeared in both start and stop locations
ub1%>%distinct(START)->start
ub1%>%distinct(STOP)->stop

ub1 %>% distinct(START, STOP, .keep_all = TRUE)->mm
ub1 %>% group_by(START, STOP) %>% filter(row_number() == 1)->mm1
ub1 %>% group_by(START, STOP) %>% slice(1)

mm=start$START[match(start$START,stop$STOP)]

#Identify popular start points - top 10
ub%>%group_by(START)%>%summarize(count=n())%>%arrange(desc(count))%>%top_n(10,wt=count)

#Identify popular stop points - top 10
ub1%>%group_by(STOP)%>%summarize(count=n())%>%arrange(desc(count))%>%top_n(10,wt=count)

ub1$START=as.character(ub1$START)
ub1$STOP=as.character(ub1$STOP)
str(ub1)

# Are there cases where the start and the stop location are the same
ub1$same=ifelse(ub1$START==ub1$STOP,ub1$START,0)
ub1%>%filter(ub1$same!=0)

# Favorite starting point wrt the total miles covered 
ub1%>%group_by(START)%>%summarize(total=sum(MILES))%>%arrange(desc(total))%>%top_n(n=10,wt=total)

#Find out most farthest start and stop pair - top10 ( aggregation ) ( BY TOTAL miles COVERED EVER ! )
ub1%>%group_by(START)%>%summarize(total=sum(MILES))%>%arrange(desc(total))%>%top_n(n=10,wt=total)%>%ggplot(aes(reorder(x=START,-total),y=total))+geom_bar(stat="identity",fill="Blue")+xlab("START")

ub1%>%group_by(START,STOP)%>%summarize(Total=sum(MILES))%>%arrange(desc(Total))

#Dropping Unknown Location Value
ub1%>%filter(START!="Unknown Location" & STOP!="Unknown Location")->ubb

ubb%>%group_by(START,STOP)%>%summarize(Total=sum(MILES))%>%arrange(desc(Total))

#The most popular start and stop pair - ( BY COUNT of travels! )
ubb%>%group_by(START,STOP)%>%summarize(count=n())%>%arrange(desc(count))

#Manipulating date & time objects
head(ub1$START_DATE,5)
head(ub1$END_DATE)

ub1$st_Time=format(as.POSIXct(ub1$START_DATE,format="%m/%d/%Y %H:%M"),format="%H:%M")
ub1$stp_Time=format(as.POSIXct(ub1$END_DATE,format="%m/%d/%Y %H:%M"),format="%H:%M")
ub1$st_date=format(as.Date(ub1$START_DATE,format="%m/%d/%Y %H:%M"),format="%m-%d-%Y")
ub1$stp_date=format(as.Date(ub1$END_DATE,format="%m/%d/%Y %H:%M"),format="%m-%d-%Y")
ub1$st_hr=format(as.POSIXct(ub1$START_DATE,format="%m/%d/%Y %H:%M"),format="%H")


library(lubridate)
ub1$month=month(mdy(ub1$st_date),label = T)
ub1$day=day(mdy(ub1$st_date))
ub1$wday=wday(mdy(ub1$st_date),label = T)

# Which month did he get most drives  ?
ub1%>%group_by(month)%>%summarize(count=n())%>%arrange(desc(count))

# Which month did he get most drives BASED ON miles
ub1%>%group_by(month)%>%summarize(total=sum(MILES))%>%arrange(desc(total))

# Getting the average distance covered each month
ub1%>%group_by(month)%>%summarize(avg=mean(MILES))%>%arrange(desc(avg))

#Which day did he get most drives  ? 
ub1%>%group_by(wday)%>%summarize(count=n())%>%arrange(desc(count))

#When does he usually start the trip ?
ub1%>%group_by(st_hr)%>%summarize(count=n())%>%arrange(desc(count))%>%ggplot(aes(st_hr,count))+geom_bar(stat="identity",fill="blue")

#Duration of the trips
ub1$START_DATE=as.POSIXct(ub1$START_DATE,format="%m/%d/%Y %H:%M")
ub1$END_DATE=as.POSIXct(ub1$END_DATE,format="%m/%d/%Y %H:%M")
str(ub1)
ub1$diff=as.numeric(ub1$END_DATE-ub1$START_DATE)
summary(ub1$diff)

#speed
ub1%>%filter(ub1$START!="Unknown Location" & ub1$STOP!="Unknown Location")->ub2
ub2$dur=ub2$diff/60
ub2$speed=(ub2$MILES/ub2$dur)*60
summary(ub2$speed)

#Category & Purpose
table(ub1$CATEGORY)
table(ub1$PURPOSE)

#Average distance traveled for each activity
ub1=na.omit(ub1)
ub1%>%group_by(PURPOSE)%>%summarize(avg=mean(MILES))%>%arrange(desc(avg))

#Question1: How many miles was earned per category and purpose ?
ub1%>%group_by(CATEGORY)%>%summarize(sum=sum(MILES))%>%arrange(desc(sum))
ub1%>%group_by(PURPOSE)%>%summarize(sum=sum(MILES))%>%arrange(desc(sum))

#Question2: What is percentage of business miles vs personal?
ub1%>%group_by(CATEGORY)%>%summarise(tot_M=sum(MILES))%>%mutate(pct_M=tot_M/sum(tot_M)*100)

#Question3: How much time was spend for drives per category and purpose?
ub2%>%group_by(CATEGORY)%>%summarize(tot_dur=sum(dur)/60)%>%arrange(desc(tot_dur))

#Question3: How much time was spend for drives per category and purpose? 
ub1%>%group_by(PURPOSE)%>%summarize(tot_dur=sum(diff)/60)%>%arrange(desc(tot_dur))


q()
