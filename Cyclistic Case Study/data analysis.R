# load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame

### PREPARATION ###

# original .csv files for all 2022 months
jan_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-jan-divvy-tripdata.csv") 
feb_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-feb-divvy-tripdata.csv") 
mar_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-mar-divvy-tripdata.csv")
apr_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-apr-divvy-tripdata.csv") 
may_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-may-divvy-tripdata.csv")
jun_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-jun-divvy-tripdata.csv") 
jul_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-jul-divvy-tripdata.csv") 
aug_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-aug-divvy-tripdata.csv")
sep_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-sep-divvy-tripdata.csv")
oct_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-oct-divvy-tripdata.csv") 
nov_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-nov-divvy-tripdata.csv") 
dec_df <- read_csv("/Users/audrysurendra/Desktop/Google Data Analysis Course/2022 Bike Data/CSV Data/2022-dec-divvy-tripdata.csv") 

# merge all of the monthly data frames into one year 
year_df <- rbind (jan_df,feb_df,mar_df,apr_df,may_df,jun_df,jul_df,aug_df,sep_df,oct_df,nov_df,dec_df)

# remove individual month data frames to clear up space in the environment 
remove(jan_df,feb_df,mar_df,apr_df,may_df,jun_df,jul_df,aug_df,sep_df,oct_df,nov_df,dec_df)

# create a new data frame with all 12 months to perform analysis on
cyclistic_df <- year_df

# create a column called “ride_length.” Calculate the length of each ride by subtracting the column “started_at” from the column “ended_at”
cyclistic_df$ride_length <- difftime(year_df$ended_at, cyclistic_df$started_at, units="mins")

# create a column called “day_of_week,” and calculate the day of the week that each ride started 
# format as General or as a number with no decimals, noting that 1 = Sunday and 7 = Saturday
cyclistic_df$day_of_week <- wday(year_df$started_at)

# create a column called "month" and determine what month each ride took place in
cyclistic_df$month <- format(as.Date(cyclistic_df$started_at), "%m")

# create a column called "hour" and determine what hour of the day each ride started at (24hr format)
cyclistic_df$hour <- hour(cyclistic_df$started_at)

# clean the data
cyclistic_df <- na.omit(cyclistic_df) # remove rows with NA values
cyclistic_df <- distinct(cyclistic_df) # remove duplicate rows 
cyclistic_df <- cyclistic_df[!(cyclistic_df$ride_length <=0),] # remove where ride_length is less than 0


### ANALYSIS ###

# total casual rides vs total member rides
total_rider_type <- cyclistic_df %>%
                      group_by(member_casual) %>% 
                      count(member_casual)


# total type of bikes taken by casual vs members
total_rideable_type <- cyclistic_df %>%
                        group_by(member_casual) %>% 
                        count(rideable_type)


# analysis on ride_length for casual vs members 
mean_ride_length <- cyclistic_df %>%
                      group_by(member_casual) %>% 
                      summarise(mean_len = mean(ride_length))

max_ride_length <- cyclistic_df %>%
                      group_by(member_casual) %>% 
                      summarise(max_len = max(ride_length))

min_ride_length <- cyclistic_df %>%
                      group_by(member_casual) %>% 
                      summarise(min_len = min(ride_length))


## analysis using day_of_week 

# function to get the mode of a vector
# gotten from https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# most common day of the week for rides 
mode_day_of_week <- cyclistic_df %>%
                        group_by(member_casual) %>% 
                        summarise(mode_day = getmode(day_of_week))

# total rides for each day of the week 
total_day_of_week <- cyclistic_df %>%
                        group_by(member_casual) %>% 
                        count(day_of_week)

# average ride length for each day of the week 
mean_ride_by_day <- cyclistic_df %>%
                        group_by(member_casual,day_of_week) %>% 
                        summarise(av_ride_len = mean(ride_length))


## analysis by month

# most common month for rides
mode_month <- cyclistic_df %>%
                  group_by(member_casual) %>% 
                  summarise(mode_day = getmode(month))

# number of rides by month
total_by_month <- cyclistic_df %>%
                      group_by(member_casual) %>% 
                      count(month)

# average length of ride by month
mean_ride_by_month <- cyclistic_df %>%
                          group_by(member_casual,month) %>% 
                          summarise(av_ride_len = mean(ride_length))


## analysis by hour

# most common hour for rides
mode_hour <- cyclistic_df %>%
                group_by(member_casual) %>% 
                summarise(mode_day = getmode(hour))

# number of rides by hour
total_by_hour <- cyclistic_df %>%
                    group_by(member_casual) %>% 
                    count(hour)

# average length of ride by hour
mean_ride_by_hour <- cyclistic_df %>%
                          group_by(member_casual,hour) %>% 
                          summarise(av_ride_len = mean(ride_length))






