# install.packages("dplyr")
# install.packages("countrycode")

# Load the dplyr package
library(dplyr)
library(countrycode)

# Read the database

votes <- readRDS("Data/votes.rds")


# Print the votes dataset
votes

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

