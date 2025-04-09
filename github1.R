# Import data
covidin <- read.csv("C:/Users/Ishita/OneDrive/Desktop/R studio/Rstudio/ca2 covid 19 india.csv", stringsAsFactors=TRUE)
View(covidin)
library(janitor)#Used for cleaning data.
library(lubridate)#dates and times.
library(tidyverse)#import → clean → manipulate → visualize
library(dplyr)# data manipulation
library(ggplot2)#data visualization.
# View structure
glimpse(covidin)#glimpse(covidin) is a function from the dplyr package that gives a quick overview of the dataset covidin.
names(covidin)  #names(covidin) returns the column names of the data frame covidin

# Data integration
names(covidin)

# Create two dataframes with selected variables
df1 <- covidin %>% select(Date, `State.UT`, Confirmed)#Selects only the Date, State.UT, and Confirmed columns from covidin and stores them in df1.
print(df1)
df2 <- covidin %>% select(Date, `State.UT`, Deaths)#Selects only the Date, State.UT, and Deaths columns and stores them in df2.
print(df2)
# Integrate (merge) the two datasets
integrated_data <- inner_join(df1, df2, by = c("Date", "State.UT"))#Performs an inner join on df1 and df2 using Date and State.UT as keys.

#Keeps only rows where both df1 and df2 have matching Date and State.UT.

# Print integrated dataset
print(integrated_data)
#data cleaning
#Clean column names
covidin <- covidin %>% clean_names()  #You're using the clean_names() function from the janitor package.Converts all column names to lowercase.

#Replaces spaces and special characters with underscores (_).

#Makes names consistent and programming-friendly.
names(covidin)  # Check cleaned column names
# Remove empty rows or columns (if any)
covidin <- covidin %>%
  remove_empty("rows") %>%
  remove_empty("cols")#You're using the remove_empty() function from the janitor package to clean the covidin data frame.

#It removes completely empty rows and columns (those with all NA or blank values).

print(covidin)
# Convert character columns to factors 
covidin$state_ut <- as.factor(covidin$state_ut)#Converts the state_ut column in the covidin data frame into a factor 
print(covidin$state_ut)
# Check for and handle missing values
summary(covidin)
covidin <- covidin %>% drop_na()  # It removes all rows in the covidin data frame that contain any missing (NA) values.
#
glimpse(covidin)
head(covidin)
summary(covidin)
# Data Reduction
covid_reduced <- covidin %>% 
  select(date, state_ut, confirmed, deaths, recovered = cured) %>%  # Keep only useful columns
  filter(confirmed > 10000) %>%                                      # Filter high case counts
  distinct()                                                         # Remove duplicates

# Print reduced dataset
print(covid_reduced)
# Transform the data
covid_transformed <- covidin %>%
  mutate(
    active_rate = round((active / confirmed) * 100, 2),
    death_rate = round((deaths / confirmed) * 100, 2),
    recovery_rate = round((cured / confirmed) * 100, 2)
  ) %>%
  rename(
    state = state_ut,
    recovered = cured
  ) %>%
  select(date, state, confirmed, active, deaths, recovered, active_rate, death_rate, recovery_rate)

# View transformed data
print(head(covid_transformed))
# Data Manipulation
covid_manipulated <- covidin %>%
  mutate(
    active_rate = round((active / confirmed) * 100, 2),
    death_rate = round((deaths / confirmed) * 100, 2),
    recovery_rate = round((cured / confirmed) * 100, 2)
  ) %>%
  rename(
    state = state_ut,
    recovered = cured
  ) %>%
  filter(confirmed > 1000) %>%                    # Filter: Only states with >1000 confirmed cases
  arrange(desc(confirmed)) %>%                    # Sort by confirmed cases descending
  group_by(state) %>%                             # Group by state
  summarise(
    total_cases = sum(confirmed, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE),
    total_recovered = sum(recovered, na.rm = TRUE),
    avg_death_rate = mean(death_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Print manipulated data
print(covid_manipulated)
#data visualization(line chart)
library(ggplot2)

ggplot(covidin, aes(x = date, y = confirmed, color = state_ut)) +
  geom_line(linewidth = 1) +
  labs(title = "COVID-19 Confirmed Cases Over Time",
       x = "Date", y = "Confirmed Cases") +
  theme_minimal()
#data visualization(line chart)

#data visualization(Scatter Plot: Deaths vs. Confirmed)
ggplot(covidin, aes(x = confirmed, y = deaths, color = state_ut)) +
  geom_point(alpha = 0.6) +#60% opacity for better visibility in overlapping areas.
  labs(title = "Deaths vs. Confirmed Cases",
       x = "Confirmed Cases", y = "Deaths") +
  theme_light()


 




