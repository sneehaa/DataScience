library(tidyverse)
library(dplyr)
library(stringi)
library(scales)

setwd("/Users/snehaaadhikari/Desktop/Sneha_DataScience")

hh2019 = read_csv("Datasets/House Price-2019.csv", show_col_types = FALSE)
hh2020 = read_csv("Datasets/House Price-2020.csv", show_col_types = FALSE)
hh2021 = read_csv("Datasets/House Price-2021.csv",show_col_types = FALSE)
hh2022= read_csv("Datasets/House Price-2022.csv",show_col_types = FALSE)

colnames(hh2019) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County", "Type1", "Type2" )
colnames(hh2020) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County", "Type1", "Type2")
colnames(hh2021) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County" , "Type1", "Type2")
colnames(hh2022) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County" , "Type1", "Type2")

HousePrices = rbind(hh2019,hh2020,hh2021,hh2022) %>% 
  na.omit() %>% 
  distinct() %>% 
  as_tibble()
View(HousePrices)

write.csv(HousePrices, "Cleaning/cleaned datasets/Combined_House_Pricing_2019-2022.csv")



FilteredHousePrices = filter(HousePrices, County == 'OXFORDSHIRE' | County == 'YORK' | County == 'WEST YORKSHIRE' | County == 'NORTH YORKSHIRE' | County == 'SOUTH YORKSHIRE' )

# Replace "YORK" with "YORKSHIRE" in the COUNTY column
FilteredHousePrices$County[FilteredHousePrices$County == "YORK"] <- "YORKSHIRE"


view(FilteredHousePrices)

pattern = ' .*$'

FilteredHousePrices = FilteredHousePrices %>% 
  mutate(shortPostcode=gsub(pattern,"",PostCode)) %>%
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  select(PostCode,shortPostcode,Year,PAON,Price) %>% 
  na.omit() %>% 
  distinct() %>% 
  as_tibble()
View(FilteredHousePrices)


# exporting filteredhouseprices data set to  csv
write.csv(FilteredHousePrices, "Cleaning/Cleaned datasets/Cleaned_House_Pricing_2019-2022.csv",row.names = FALSE)

