# install.packages("dplyr")
# install.packages("countrycode")

# Load the dplyr package
library(dplyr)
library(countrycode)

# Read the database

votes <- readRDS("Data/votes.rds")


# Print the votes dataset
votes

# ----------------------------------------------------------------------
# MANIPULACION DE DATOS - REPORTERIA
# ----------------------------------------------------------------------


# Filter for votes that are "yes", "abstain", or "no"
votes %>%
  filter(vote %in% c(1,2,3))

# Add another %>% step to add a year column 1945 starts and add with sessions
votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945)


# Add a country column within the mutate: votes_processed
votes_processed <- votes %>%
  filter(vote <= 3) %>%
  mutate(
    year = session + 1945,
    country = countrycode(ccode, "cown", "country.name")
  )

# ccode = 260 dont have a name, we found the name of the code that is "indiana"

votes_processed <- votes_processed %>% 
  filter(ccode == 260) %>% 
  mutate(country = "Indiana")
  

# Print votes_processed
votes_processed

# Find total and fraction of "yes" votes
votes_processed %>%
  summarise(
    total = n(),
    percent_yes = mean(vote == 1) )
  
# Change this code to summarize by year
votes_processed %>%
  group_by(year)%>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))
  
# Summarize by country: by_country
by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))
  
  
# Print the by_country dataset
by_country

# Sort in ascending order of percent_yes
by_country %>% 
  arrange(percent_yes)

# Now sort in descending order
by_country %>% 
  arrange(desc(percent_yes))

# Filter out countries with fewer than 100 votes
by_country %>%
  arrange(percent_yes) %>%
  filter(total > 100)

# ----------------------------------------------------------------------
# GRAPHS
# ----------------------------------------------------------------------

# Define by_year
by_year <- votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Load the ggplot2 package
library(ggplot2)

# Create line plot
ggplot(by_year, aes(x = year, y = percent_yes)) +
  geom_line()

# Change to scatter plot and add smoothing curve
ggplot(by_year, aes(year, percent_yes)) +
  geom_smooth() +
  geom_point()


# ----------------------------------------------------------------------
# COUNTRY
# ----------------------------------------------------------------------

# Group by year and country: by_year_country
by_year_country <- votes_processed %>%
  group_by(year, country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))


# Print by_year_country
by_year_country

# Create a filtered version: UK_by_year
UK_by_year <- by_year_country %>%
  filter(country == "United Kingdom")

# Line plot of percent_yes over time for UK only
ggplot(UK_by_year, aes(x= year, y = percent_yes)) +
  geom_line()

# Vector of four countries to examine
countries <- c("United States", "United Kingdom",
               "France", "India")

# Filter by_year_country: filtered_4_countries
filtered_4_countries<- by_year_country %>% 
  filter(country %in% countries)

# Line plot of % yes in four countries
ggplot(filtered_4_countries, aes(x= year ,y=percent_yes, color = country)) +
  geom_line()

# ----------------------------------------------------------------------
# FACET WRAP
# ----------------------------------------------------------------------

# Vector of six countries to examine
countries <- c("United States", "United Kingdom",
               "France", "Japan", "Brazil", "India")

# Filtered by_year_country: filtered_6_countries
filtered_6_countries <- by_year_country %>%
  filter(country %in% countries)

# Line plot of % yes over time faceted by country
ggplot(filtered_6_countries, aes(x=year,y =percent_yes)) +
  geom_line() + 
  facet_wrap(~ country)

# Line plot of % yes over time faceted by country
ggplot(filtered_6_countries, aes(year, percent_yes)) +
  geom_line() +
  facet_wrap(~ country, scales = "free_y")



# ----------------------------------------------------------------------
# LINEAR REGRESSION
# ----------------------------------------------------------------------

# Percentage of yes votes from the US by year: US_by_year
US_by_year <- by_year_country %>%
  filter(country == "United States")

# Print the US_by_year data
US_by_year

# Perform a linear regression of percent_yes by year: US_fit
US_fit <- lm(percent_yes ~ year, data = US_by_year)

# Perform summary() on the US_fit object
summary(US_fit)














