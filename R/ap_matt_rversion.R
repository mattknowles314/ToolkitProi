library(shiny)
library(leaflet)
library(tidyverse)
library(stringr)
library(readxl)
library(rgdal)
library(purrr)
set.seed(5)

# download data to file directory if not already on computer

dir_name <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_name)
filenames <- c("dft-road-casualty-statistics-vehicle-2020.csv","dft-road-casualty-statistics-casualty-2020.csv","dft-road-casualty-statistics-accident-2020.csv")

if(length(na.omit(match(filenames,list.files(getwd()))))==3)
{
# great data is already available and no need to re-download
}else{
  vehicle_data_loc <- paste(dir_name,"/dft-road-casualty-statistics-vehicle-2020.csv",sep="")
  casualty_data_loc <- paste(dir_name,"/dft-road-casualty-statistics-casualty-2020.csv",sep="")
  accidents_data_loc <- paste(dir_name,"/dft-road-casualty-statistics-accident-2020.csv",sep="")
  
  download.file("https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-vehicle-2020.csv",vehicle_data_loc,method='libcurl', mode = "wb")
  download.file("https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-casualty-2020.csv",casualty_data_loc,method='libcurl', mode = "wb")
  download.file("https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-accident-2020.csv",accident_data_loc,method='libcurl', mode = "wb")
}

if(length(na.omit(match("region_codes.xlsx",list.files(getwd()))))==3)
{
  # great region codes data is already available and no need to re-download
}else{
  download.file("https://harvardready.com/res/files/region_codes.xlsx",paste(dir_name,"/region_codes.xlsx",sep=""),method='libcurl', mode = "wb")
}

vehicles <- read.csv("dft-road-casualty-statistics-vehicle-2020.csv")
casualty <- read.csv("dft-road-casualty-statistics-casualty-2020.csv")
accidents <- read.csv("dft-road-casualty-statistics-accident-2020.csv")
region_codes <- read_excel("region_codes.xlsx")

# bin severity classes

accidents <- accidents %>% 
  mutate(bin_severity = ifelse(.data$accident_severity == 3, 0, 1))

# We now reduce the size of the dataset to contain only variables we care about. 

sub_accidents <- accidents %>% 
  select(
    "ï..accident_index",
    "local_authority_ons_district",
    "bin_severity",
    "day_of_week",
    "road_type",
    "speed_limit",
    "time",
    "light_conditions",
    "weather_conditions",
    "road_surface_conditions"
  )

sub_vehicles <- vehicles %>% 
  select(
    "ï..accident_index",
    "sex_of_driver",
    "age_band_of_driver"
  )

sub_casualties <- casualty %>% 
  select(
    "ï..accident_index",
    "sex_of_casualty",
    "age_band_of_casualty"
  )


colnames(sub_accidents)[1] <- "accident_index"
colnames(sub_vehicles)[1] <- "accident_index"
colnames(sub_casualties)[1] <- "accident_index"

df_sub <- merge.data.frame(sub_vehicles, sub_casualties, by = "accident_index")
df_sub_unique <- unique(df_sub)
df <- merge.data.frame(sub_accidents, df_sub_unique, by = "accident_index")

# translate authority codes into region designations
region_record <- c()
for(i in 1:nrow(df))
{
  if(identical(unlist(region_codes[which(region_codes[,2]==df$local_authority_ons_district[i]),1]),character(0))==FALSE)
  {
    df$local_authority_ons_district[i] <- unlist(region_codes[which(region_codes[,2]==df$local_authority_ons_district[i]),1])
  }else
  {
    region_record <- c(region_record,i)
  }
}
# check region_record to see that no issues were found

# Split time into 6 chunks of four hours
split_time <- stringr::str_split(df$time, ":", simplify = TRUE)
hour <- as.integer(split_time[,1])
hour[0<=hour & hour<4] <- 1
hour[4<=hour & hour<8] <- 2
hour[8<=hour & hour<12] <- 3
hour[12<=hour & hour<16] <- 4
hour[16<=hour & hour<20] <- 5
hour[20<=hour & hour<=24] <- 6

df$time_group <- hour

df <- df %>% 
  select(-time)

# We want all variables, except speed limit, to be factors. To achieve this we mutate across all integer columns and change the type to factor. 
# However, we need speed limit to stay as an integer to make the model adaptable to non-standard speed limits. 
# The "bin_severity" column needs to be a factor, so we ensure this by calling the as.factor function once more just to ensure it is in fact a factor. 
# By viewing the head of the data as a tibble we can check that the columns are of expected type.

df <- df %>%
  mutate(across(where(is.integer), as.factor))
df$speed_limit <- as.integer(df$speed_limit) #Speed limit goes back to integer
df$bin_severity <- as.factor(df$bin_severity)
df$time_group <- as.factor(df$time_group)
# head(tibble(df))

# clean data: remove non recorded values (-1 in dataset)
has.neg <- apply(df, 1, function(row) any(row == -1))
df <- df[-which(has.neg), ] %>% 
  select(-accident_index)

# clean data: remove unknown sex of casualty datapoints (3 total) since the estimate is unstable (SE: 41 or so)
df <- df[-which(df$sex_of_casualty==9),]

# split data into test / training sets (25% / 75%)
df$id <- 1:nrow(df)
df_train <- df %>% sample_frac(0.75)
df_test <- anti_join(df, df_train, by = "id")

df_train <- df_train %>% 
  select(-id)

df_test <- df_test %>% 
  select(-id)

# fitting the GLM to location specific data 

fit.local <- glm(
  data = df_train,
  formula = bin_severity ~ .,
  family = binomial(link = "logit"))

# fitting the GLM to national level data

df_train_national <- df_train %>% 
  select(-local_authority_ons_district)

df_test_national <- df_test %>% 
  select(-local_authority_ons_district)

fit.national <- glm(
  data = df_train_national,
  formula = bin_severity ~ .,
  family = binomial(link = "logit"))

# fitting national glm with speed_limit * road_type interaction

fit.national.interaction <- glm(
  data = df_train_national,
  formula = bin_severity ~ . + speed_limit*road_type,
  family = binomial(link = "logit"))
