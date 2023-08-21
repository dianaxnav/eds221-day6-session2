#-------Section 1: Filter-------#

library(tidyverse)
library(palmerpenguins)
library(lterdatasampler)

#Look for an exact match 

penguins %>%
  filter(island == "Biscoe")

penguins_2007 <- penguins %>% 
  filter(year == 2007)
penguins_2007

#Practice conditional statement
adelie_torg <- penguins %>% 
  filter(species == "Adelie" & island == "Torgersen")

#Create a subset from penguins that only contain gentoo penguins observed in 2008
gentoos_2008 <- penguins %>% 
  filter(species == "Gentoo" & year == 2008)

#create a subset that contains gentoos and adelies 
gentoo_adelie <- penguins %>% 
  filter(species == "Gentoo" | species == "Adelie")

#Create a subset that contains observations where island is DREAM or the year is 2009
dream_2009 <- penguins %>% 
  filter(island == "Dream" | year == 2009)


#install pie crab,
#Create a ggplot chart that has water temperature on x axis and y = crab size 
ggplot(data = pie_crab, aes(x = water_temp, y = size)) +
  geom_point()

#Keep observations for sites NIB, ZI, DB, JC and we can use the %>% operator to ask: does the value in our col match any of the values in this vector?
pie_sites <- pie_crab %>% 
  filter(site %in% c("NIB", "ZI", "DB", "JC"))

sites <- c("CC", "BB", "PIE")
#%in% order of things doesnt matter which is what you want- check to see if value matches any of the things for any individual row
pie_sites_2 <- pie_crab %>% 
  filter(site %in% sites)

#Create a subset using the %in% where includes sites PIE, ZI, NIB, BB, and CC

pie_sites_3 <- pie_crab %>% 
  filter(site %in% c("PIE", "ZI", "NIB", "CC", "BB"))

#Exluding filter statements 

# != (asks is this not equal to that value?)
exclude_zi <- pie_crab %>% 
  filter(site != "ZI")

#What if i want to exclude sites BB, CC, and PIE?
exclude_bb_cc_pie <- pie_crab %>% 
  filter(!site %in% c("BB", "CC","PIE"))

# Create a subset from pie_crab that only contains observations from NIB, CC, and ZI, for crabs with carapice size exceeding 13.
car_pie <- pie_crab %>% 
  filter(site %in% c("NIB", "CC", "ZI") & size > 13)


#--------Selecting Col--------#

#Select individual cols by name, separate them by a column
crabs_subset <- pie_crab %>%
  select(latitude, size, water_temp)

#Select a range of cols:
crabs_subset2 <- pie_crab %>%
  select(site:air_temp)

#Select a range and an individual col
crabs_subset3 <- pie_crab %>% 
  select(date:water_temp, name)

pie_crab %>%
  select(name, water_temp, size)


#--------Mutate-------#

#Use dplyr::mutate() to add or update a column, while keeping all existing cols

#mm/10 = cm
crabs_cm <- pie_crab %>% 
  mutate(size_cm = size / 10)

#What happens if I use mutate to add new column containing the mean of the size col

crabs_mean <- pie_crab %>% 
  mutate(mean_size = mean(size, na.rm = TRUE))

crabs_awesome <- pie_crab %>%
  mutate(name = "Teddy is awesome")

#Mutate where we combine it with a group by 

#Reminder: group_by + summarize
mean_size_by_site <- pie_crab %>% 
  group_by(site) %>% 
  summarize(mean_size = mean(size, na.rm = TRUE))

#What about a group by then mutate?
#First recognize that there are different levels win size cols, and then whatever comes next add the mean size
group_mutate <- pie_crab %>% 
  group_by(site) %>% 
  mutate(mean_size = mean(size, na.rm = TRUE))

#What if I want to create a new col in pie_crab that contains "giant" if the size is greater than 35, or "not giant" of the size is less than or equal to 35?

#Use dplyr::case_when() to write if-else statements more easily
crabs_bin <- pie_crab %>%
  mutate(size_binned = case_when(
    size > 20 ~ "giant",
    size <= 20 ~ "not giant"
  ))

sites_binned <- pie_crab %>% 
  mutate(region = case_when(
    site %in% c("ZI", "CC", "PIE") ~ "Low",
    site %in% c("BB", "NIB") ~ "Middle",
    TRUE ~ "High"
  ))

