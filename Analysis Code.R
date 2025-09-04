#Name:-Sruthi Kondra
#ALY6010: Probability Theory and Introductory Statistics
#Final Project

# Clear the environment at the start of a session
cat("\014") # Clears Console
rm(list = ls()) # Clears Global Environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Clears Plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # Clears Packages
options(scipen = 100) # Disables scientific notation for entire R Session

#downloading and loading different packages 
library(pacman)
library(dplyr)
library(tidyverse)
library(flextable)
library(janitor)
library(ggplot2)
p_load(tidyverse)
p_load(janitor)
p_load(lubridate)

#reads the dataframe into parcel_dataset
crime_dataset <- read.csv("tmpdfeo3qy2.csv")
crime_dataset

#cleaning the column names and printing them
crime_dataset <- clean_names(crime_dataset)
names(crime_dataset)

glimpse(crime_dataset)

# Changing data types
crime_dataset$offense_code <- as.integer(crime_dataset$offense_code)
crime_dataset$offense_description <- as.factor(crime_dataset$offense_description)
crime_dataset$location <- as.factor(crime_dataset$location)

# Dropping the offense_code_group column
crime_dataset <- crime_dataset %>%
  select(-offense_code_group)

# View the cleaned dataset
head(crime_dataset)

summary(crime_dataset)

str(crime_dataset)

#Creating a dataset of offence_code and their respective Description
offense_details <- crime_dataset %>%
  select(offense_code, offense_description) %>%
  arrange(offense_code) %>%
  distinct()

# Define offense codes for each crime category
violent_crimes <- c(111,121,301,423,520,530,531,540,2500,2511,2604,2618,2622)
moderate_crimes <- c(611,612,613,614,615,616,617,618,619,706,724,727,801,900,2610,2670)
fraud_crimes <- c(1001,1102,1106,1107,1108,1109,1201,2636)

# Categorize each offense
offense_details$crime_category <- case_when(
  offense_details$offense_code %in% violent_crimes ~ "Violent",
  offense_details$offense_code %in% moderate_crimes ~ "Moderate",
  offense_details$offense_code %in% fraud_crimes ~ "Fraud",
  TRUE ~ "Other"
)

head(offense_details)

# Counting the occurrences of each crime category
# This time, we'll use the complete dataset to count, rather than the offense_details subset
crime_counts <- offense_details %>%
  group_by(crime_category) %>%
  summarize(count = n())

# Creating a bar chart to display the count of different crimes
ggplot(crime_counts, aes(x = crime_category, y = count, fill = crime_category)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +  # Using a color palette for differentiation
  labs(title = "Number of Offenses by Crime Category",
       x = "Crime Category",
       y = "Number of Offenses") +
  theme_minimal()

# Analysing the top 20 most dangerous streets 
# Counting crimes on each street
street_crime_counts <- crime_dataset %>%
  group_by(street) %>%
  summarize(crime_count = n()) %>%
  arrange(desc(crime_count)) %>%
  top_n(20, crime_count)

# Creating a bar chart for the top 20 most dangerous streets
ggplot(street_crime_counts, aes(x = reorder(street, crime_count), y = crime_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +  # Flips the axes to make the chart horizontal
  labs(title = "Top 20 Most Dangerous Streets Based on Crime Reports",
       x = "Street",
       y = "Number of Crimes Reported") +
  theme_minimal()

# Histogram of Crime Frequency by Offense Code
ggplot(crime_dataset, aes(x = offense_code)) +
  geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
  labs(title = "Histogram of Crime Frequency by Offense Code",
       x = "Offense Code",
       y = "Frequency") +
  theme_minimal()


# Descriptive Statistics for Each Crime Category
flextable(offense_details %>%
            group_by(crime_category) %>%
            summarise(
              count = n(),
              mean_offense_code = mean(offense_code, na.rm = TRUE),
              median_offense_code = median(offense_code, na.rm = TRUE)
            )
)

# Subset for Violent Crimes
violent_crime_data <- subset(offense_details, crime_category == "Violent")

# Histogram for Violent Crimes by Offense Code
ggplot(violent_crime_data, aes(x = offense_code)) +
  geom_histogram(binwidth = 1, color = "black") + # binwidth determines the width of the bins in the histogram
  labs(title = "Histogram of Violent Crimes by Offense Code",
       x = "Offense Code",
       y = "Frequency") +
  theme_minimal()

#___QUESTION 1____
#Is there a significant relationship between the time of day (hour) and the frequency of violent crimes?

# Filtering for violent crimes
violent_crime_data <- crime_dataset %>%
  filter(offense_code %in% violent_crimes)

# Aggregating data by hour
hourly_crime_frequency <- violent_crime_data %>%
  group_by(hour) %>%
  summarize(frequency = n())

# Linear regression analysis
lm_result <- lm(frequency ~ hour, data = hourly_crime_frequency)
summary(lm_result)

# Creating a scatterplot with a linear regression line
ggplot(hourly_crime_frequency, aes(x = hour, y = frequency)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship Between Time of Day and Frequency of Violent Crimes",
       x = "Hour of Day",
       y = "Frequency of Violent Crimes") +
  theme_minimal()

#___QUESTION 2___
#Is there a significant correlation between the latitude of a crime incident and the frequency of crimes in that area?

# Aggregating data by latitude
latitude_crime_frequency <- crime_dataset %>%
  group_by(lat) %>%
  summarize(frequency = n())

# Pearson's correlation test
cor_test_result <- cor.test(latitude_crime_frequency$lat, latitude_crime_frequency$frequency, method = "pearson")

# Output the result of the correlation test
print(cor_test_result)

# Creating a scatterplot with a linear regression line
ggplot(latitude_crime_frequency, aes(x = lat, y = frequency)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation Between Latitude and Frequency of Crimes",
       x = "Latitude",
       y = "Frequency of Crimes") +
  theme_minimal()

#___Question 3___
#Does the frequency of total crimes significantly vary across different months of the year?

# Aggregating data by month
monthly_crime_frequency <- crime_dataset %>%
  group_by(month) %>%
  summarize(frequency = n())

# Convert month to a numerical variable
monthly_crime_frequency$month_num <- as.numeric(as.factor(monthly_crime_frequency$month))

# Correlation test
cor_test_result <- cor.test(monthly_crime_frequency$month_num, monthly_crime_frequency$frequency)

# Output the result of the correlation test
print(cor_test_result)

# Creating a bar chart to visually represent crime frequency by month
ggplot(monthly_crime_frequency, aes(x = factor(month), y = frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Total Crime Frequency by Month",
       x = "Month",
       y = "Frequency of Total Crimes") +
  theme_minimal()

