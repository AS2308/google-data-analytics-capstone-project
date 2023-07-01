## convert the cleaned data to a csv file to use in tableau

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

# view the data
View(cyclistic_df)

#create a new dataframe to use in Tableau
cyclistic_tableau <- cyclistic_df

#download the new data as a .csv file
write.csv(cyclistic_tableau,file='/Users/audrysurendra/Desktop/Google Data Analysis Course/cyclistic_data.csv', row.names=FALSE)



