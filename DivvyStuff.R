install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("data.table")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(data.table)



#Read the .csv file 
#Taken directly from https://www.divvybikes.com/system-data
#remove irrelevant columns for clarity
DivvyQ4 <- read.csv("Divvy_Trips_2018_Q4.csv")
DivvyQ4 <- select(DivvyQ4, c(start_time, end_time, tripduration,
                             from_station_id, to_station_id))


# We'll graph the inflow and outflow of bikes kon 57th street
trips57 <- DivvyQ4 %>%
  filter((from_station_id == 423)|(to_station_id == 423) )



#Arrange the trips by when they interacted with the 57th street station.
trips57 <- trips57 %>%
  mutate(station_time = if_else(from_station_id==423, as.POSIXct(start_time, origin = "1970-01-01 05:00:00") , as.POSIXct(end_time, origin = "1970-01-01 05:00:00")))

trips57 <- trips57 %>% arrange(station_time)


#create a list
num_bikes_rel <- c()
num_bikes_rel[1] = 0
for(i in 2:6170) {
  num_bikes_rel[i] = ifelse(i%%500 == 0, 0, ifelse(trips57[i-1,"from_station_id"]==423,num_bikes_rel[i-1]-1,num_bikes_rel[i-1]+1))
}


trips57 <- trips57 %>% mutate(num_bikes_rel = num_bikes_rel)
trips57
trips57$idu <- as.numeric(row.names(trips57))

ggplot(trips57, aes(x=station_time, y=num_bikes_rel))+
  geom_line()+ 
  scale_x_datetime(limits = 
                       c(as.POSIXct(1538956800, origin = "1970-01-01 05:00:00"),
                     as.POSIXct(1539043200, origin = "1970-01-01 05:00:00"))) +
  labs(y="Change in Fullness",x="Time", 
       title = "Inflow and Outflow of Riders to 57th Street Station")


    
    
##Sees the distribution of trip lengths from the station on 
#60th and Ellis (near south campus)
# and 58th and ellis (a station on campus, not the central station to campus,
# which is the one on 57th street.)

summary(trips_ellis_55[,3])
trips_ellis_55 <- DivvyQ4 %>%
  filter((from_station_id == 426 )&(to_station_id == 328))

durations<-as.numeric(paste(trips_ellis_55[,3]))

summary(durations)
trips_ellis_55 <- trips_ellis_55 %>% mutate(durations = durations)


trips_ellis_55

ggplot(trips_ellis_55, aes(x=durations))+
  geom_histogram(bins=30) +
  labs(y="Count",x="Trip Duration (s)", 
       title = "Distribution of Trip Durations from RGG to 58th Street")

  


