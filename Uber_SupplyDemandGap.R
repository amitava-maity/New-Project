#Visually identify the most pressing problems for Uber
#Load data from into dataframe
uber_data <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)
View(uber_data)

#Fetch Cancelled Cab/ Car Not availbel dataset
unique_status <- unique(uber_data$Status)

uber_problematic_types <- uber_data

View(uber_problematic_types)

library(lubridate)

# Data Cleaning - Date Formating

uber_problematic_types$Request.timestamp <- parse_date_time(uber_data$Request.timestamp,c("dmY HM","dmY HMS"))

# Narrow down the analysis only on Cancelled and No Car Available status

uber_problematic_types_data <- subset(uber_problematic_types,uber_problematic_types$Status == "Cancelled" | uber_data$Status =="No Cars Available")

View(uber_problematic_types_data)

str(uber_problematic_types)

#Fetch timing for cancelled or car not avilable status

uber_problematic_types_data$Request_hour <- format(uber_problematic_types_data$Request.timestamp, "%H")

View(uber_problematic_types_data)

# ggplot for cancelled or no car available

library(ggplot2)

#for Airport to city 

uber_problematic_types_to_airort <- subset(uber_problematic_types_data,uber_problematic_types_data$Pickup.point == "Airport")

uber_problematic_types_plot_airpot <- ggplot(uber_problematic_types_to_airort,aes(x=Request_hour,col= Status)) + geom_bar()

#for City to Airport

uber_problematic_types_to_city <- subset(uber_problematic_types_data,uber_problematic_types_data$Pickup.point == "City")

uber_problematic_types_plot_city <- ggplot(uber_problematic_types_to_city,aes(x=Request_hour,col= Status)) + geom_bar()

# Analyze supply and demand

View(uber_problematic_types)

uber_problematic_types$Request_hour <- format(uber_problematic_types$Request.timestamp, "%H")

# Group the Request.id based on time slot to identify total number request made in a particular time slot
library(tidyr)
library(dplyr)
library(gsubfn)

uber_problematic_types$Request_hour <- as.character(paste(as.character(uber_problematic_types$Request_hour),'h'))

uber_problematic_types$total_request <- uber_problematic_types$Request.id

#For all cab request group by request time range

uber_problematic_types_grp <- group_by(uber_problematic_types,Request_hour)
View(uber_problematic_types_grp)

uber_problematic_types_grp <- as.data.frame(summarise(uber_problematic_types_grp, total_request = n_distinct(total_request)))
View(uber_problematic_types_grp)
str(uber_problematic_types_grp)

#For all complete request group by request time range

uber_problematic_types_comp_req <- subset(uber_problematic_types,uber_problematic_types$Status == "Trip Completed")

View(uber_problematic_types_comp_req)
uber_problematic_types_comp_req_grp <- group_by(uber_problematic_types_comp_req,Request_hour)

uber_problematic_types_comp_req_grp <- as.data.frame(summarise(uber_problematic_types_comp_req_grp, total_request = n_distinct(total_request)))
View(uber_problematic_types_comp_req_grp)

colnames(uber_problematic_types_comp_req_grp)[2] <- "total_request_comp"

# merge two dataset to determine supply demand gap

uber_supply_demand_gap <- merge(x=uber_problematic_types_grp,y=uber_problematic_types_comp_req_grp,by="Request_hour",all.x = TRUE)
View(uber_supply_demand_gap)

uber_supply_demand_gap$supply_demand_gap <- (uber_supply_demand_gap$total_request-uber_supply_demand_gap$total_request_comp)

# R ggplot for supply demand gap

library(reshape2)

uber_supply_demand_gap_1 <- melt(uber_supply_demand_gap,id.vars = "Request_hour")

uber_supply_demand_gap_plot <- ggplot(uber_supply_demand_gap_1,aes(x=Request_hour,y =value ,fill=variable)) + geom_bar(stat = 'identity',position = 'dodge')

# Generate dataset for tableau analysis

write.csv(uber_problematic_types,"uber_problematic_data.csv")

write.csv(uber_supply_demand_gap,"uber_supply_demand_gap.csv")

# Try to understand the resons of this suppy and demand gap
# Try to understand trip timing and road traffic analysis

View(uber_problematic_types)

uber_data_trip_time <- uber_problematic_types

uber_data_trip_time$Drop.timestamp <- parse_date_time(uber_data_trip_time$Drop.timestamp,c("dmY HM","dmY HMS"))

uber_data_trip_time$trip_end_time <- format(uber_data_trip_time$Drop.timestamp, "%H:%M")

uber_data_trip_time$trip_start_time <- format(uber_data_trip_time$Request.timestamp, "%H:%M")

View(uber_data_trip_time)

#Fetch the trip completion time thoughout the day

uber_data_trip_time <- subset(uber_data_trip_time,!is.na(uber_data_trip_time$Drop.timestamp))

uber_data_trip_time$trip_time <- as.numeric(with(uber_data_trip_time,difftime(Drop.timestamp,Request.timestamp,units = 'mins')))

write.csv(uber_data_trip_time,"uber_data_trip_time.csv")

write.csv(uber_data_trip_time,"uber_data_trip_time.csv")

# Find out number of driver avialbel to meet the demand

uber_problematic_types_driver <- uber_problematic_types

uber_problematic_types_driver_grp <- group_by(uber_problematic_types_driver,Request_hour)

uber_problematic_types_driver_grp <- as.data.frame(summarise(uber_problematic_types_driver_grp,total_request = n_distinct(Request.id), no_of_drivers = n_distinct(Driver.id)))

View(uber_problematic_types_driver_grp)

write.csv(uber_problematic_types_driver_grp,"uber_problematic_types_driver_grp.csv")

uber_problematic_types_each_driver_grp <- group_by(uber_problematic_types_driver,Driver.id)

uber_problematic_types_each_driver_grp <- as.data.frame(summarise(uber_problematic_types_each_driver_grp, drivers_no_of_request = n_distinct(Request.id)))

View(uber_problematic_types_each_driver_grp)

# Write data into csv for tableau analysis

write.csv(uber_problematic_types_each_driver_grp,"uber_problematic_types_each_driver_grp.csv")
