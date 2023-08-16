library(tidyverse)
library(dplyr)
library(stringi)
library(scales)

setwd("/Users/snehaaadhikari/Desktop/Sneha_DataScience")
uncleanedhouseprices = read_csv('Cleaning/cleaned datasets/Combined_House_Pricing_2019-2022.csv')

Population = read_csv("Datasets/Population2011_1656567141570.csv", show_col_types = FALSE)
Population

view(uncleanedhouseprices)

FilteredTown = filter(uncleanedhouseprices, County == 'OXFORDSHIRE' | County == 'YORK' | County == 'WEST YORKSHIRE' | County == 'NORTH YORKSHIRE' | County == 'SOUTH YORKSHIRE' )
pattern = ' .*$'

Population = Population %>%  
  mutate(shortPostcode=gsub(pattern,"",Postcode)) %>%
  group_by(shortPostcode) %>%
  summarise_at(vars(Population),list(Population2011 = sum)) %>%
  mutate(Population2012= (1.00695353132322269 * Population2011)) %>%
  mutate(Population2013= (1.00669740535540783 * Population2012)) %>%
  mutate(Population2014= (1.00736463978721671 * Population2013)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2016= (1.00757874492811929 * Population2015)) %>%
  mutate(Population2017= (1.00679374473924223 * Population2016)) %>%
  mutate(Population2018= (1.00605929132212552 * Population2017)) %>%
  mutate(Population2019= (1.00561255390388033 * Population2018)) %>%
  mutate(Population2020= (1.00561255390388033 * Population2019)) %>%
  mutate(Population2021= (1.00561255390388033 * Population2020)) %>%
  mutate(Population2022= (1.00561255390388033 * Population2021)) %>%
  
  select(shortPostcode,Population2019,Population2020,Population2021,Population2022)

FilteredTown = FilteredTown %>% 
  mutate(shortPostcode=gsub(pattern,"",PostCode)) %>%
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  left_join(Population,by="shortPostcode") %>% 
  select(PostCode, shortPostcode, Year, Town, District, County, Population2019,Population2020,Population2021,Population2022) %>% 
  group_by(shortPostcode) %>%
  arrange(County) %>% 
  as_tibble() %>% 
  na.omit() %>% 
  distinct()


# Replace "YORK" with "YORKSHIRE" in the COUNTY column
FilteredTown$County[FilteredTown$County == "YORK"] <- "YORKSHIRE"

View(FilteredTown)

write.csv(FilteredTown, "Cleaning/cleaned datasets/Cleaned_Town_population.csv",row.names = FALSE)
