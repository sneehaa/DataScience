library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)

setwd("/Users/snehaaadhikari/Desktop/Sneha_DataScience")

euro <- dollar_format(prefix = "\u20ac", big.mark = ",")

Towns = read_csv("Cleaning/Cleaned datasets/Cleaned_Town_population.csv")%>% 
  select(shortPostcode, Town, District, County)
BroadbandCleaned = read_csv("Cleaning/cleaned datasets/Cleaned_Broadband_Speed.csv", show_col_types = FALSE)

broadband=Towns %>% 
  left_join(BroadbandCleaned,by="shortPostcode")
View(broadband)

#-----------Oxfordshire Broadband speed visualization-----------------------

ggplot(broadband,aes(y=Town)) +
  labs(x="Speeds (Mbits/s)",y="Town",title=" OXfordshire Broadband Speeds")+
  geom_bar(data=filter(broadband,County=="OXFORDSHIRE"),aes(x=Avgdownload,fill="Average"),stat="Identity")+
  geom_bar(data=filter(broadband,County=="OXFORDSHIRE"),aes(x=Mindownload,fill="Minimum"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))

#-----------Yorkshire Broadband speed visualization-----------------------

ggplot(broadband,aes(y=Town)) +
  labs(x="Speeds (Mbits/s)",y="Town",title=" Yorkshire Broadband Speeds")+
  geom_bar(data=filter(broadband,County=="YORKSHIRE" | County=="WEST YORKSHIRE" | County=="SOUTH YORKSHIRE" | County=="NORTH YORKSHIRE")
           ,aes(x=Avgdownload,fill="Average"),stat="Identity")+
  geom_bar(data=filter(broadband,County=="YORKSHIRE" | County=="WEST YORKSHIRE" | County=="SOUTH YORKSHIRE" | County=="NORTH YORKSHIRE")
           ,aes(x=Mindownload,fill="Minimum"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))



#Average download speed
broadband %>% 
  group_by(County) %>% 
  ggplot(aes(x = County, y = `Avgdownload`, fill=County)) +
  scale_y_continuous(breaks = seq(0,200,10)) +
  geom_boxplot() +
  labs(title = "Average download speed (Mbit/s) by district", x = "District",
       y = "Average Download Speed (Mbit/s)")+
  coord_flip()




#House Prices


Towns_Population = read_csv("Cleaning/Cleaned datasets/Cleaned_Town_population.csv") %>% 
  select(-Year) 
HousePrices=read_csv("Cleaning/Cleaned datasets/Cleaned_House_Pricing_2019-2022.csv", show_col_types = FALSE)


HousePricesclean <- HousePrices %>% 
  select(-PostCode) %>% 
  left_join(Towns_Population, by ="shortPostcode")


HousePricesclean$County[HousePricesclean$County == "WEST YORKSHIRE" ] <- "YORKSHIRE"
HousePricesclean$County[HousePricesclean$County == "NORTH YORKSHIRE" ] <- "YORKSHIRE"
HousePricesclean$County[HousePricesclean$County == "SOUTH YORKSHIRE" ] <- "YORKSHIRE"




House_town = HousePricesclean %>% 
  #filter(County=="OXFORDSHIRE"|County=="YORKSHIRE" | County=="WEST YORKSHIRE" | County=="SOUTH YORKSHIRE" | County=="NORTH YORKSHIRE") %>% 
  filter(County=="OXFORDSHIRE"|County=="YORKSHIRE") %>% 
  group_by(Town,District,County,Year) %>% 
  summarise(AveragePrice= mean(Price)) %>% 
  ungroup(Town,District,County,Year) %>%
  na.omit()




#write.csv(House_town,"Cleaning/cleaned datasets/HOUSE_TOWN.csv",row.names = FALSE)



# BOXPLOT Average house prices (2020-2022)
House_town %>% 
  filter(Year == 2020  | Year == 2021 |Year==2022) %>% 
  group_by(County) %>% 
  ggplot(aes(x = County, y = AveragePrice, fill=County)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2020-2022 house prices by county")



# BARGRAPH house prices of both county (2020-2022)
HousePricesclean %>% 
  filter(Year == 2020  | Year == 2021 |Year==2022) %>% 
  group_by(County) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = County, y = AveragePrice)) +
  geom_bar(position = "stack",stat = "identity", fill = "cornflowerblue") +
  scale_y_continuous(limits=c(0,1000000),breaks = seq(0, 5000000, 50000), 
                     label = euro) +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.25) +
  labs(title = "2022 Average house prices by County") +
  coord_flip()





#LINEGRAPH Average house prices  (2020-2022)

HousePricesclean %>%
  filter(Year %in% c(2020, 2021, 2022)) %>%
  group_by(County) %>%
  summarise(AveragePrice = mean(Price)) %>%
  ggplot(aes(x = factor(County), y = AveragePrice, label = euro(AveragePrice))) +
  geom_line(aes(group = 1), color = "lightgrey", size = 1.5) +
  geom_text(aes(y = AveragePrice), vjust = -0.85, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(0, 700000, 50000), labels = euro) +
  scale_x_discrete(labels = unique(HousePricesclean$County)) + 
  geom_point(size = 3, color = "steelblue") +
  labs(title = "2020-2022 Average House Prices", y = "Average Price (€)")


#-------------end------------------------------------------

#-----------crime graph-------------------


Town = read_csv("Cleaning/Cleaned datasets/Cleaned_Town_population.csv")%>% 
  select(-Year)
crime_Data = read_csv("Cleaning/Cleaned datasets/Cleaned_Crime_Data.csv")

View(Town)


crimeData = crime_Data %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit()


crimeData$County[crimeData$County == "WEST YORKSHIRE" ] <- "YORKSHIRE"
crimeData$County[crimeData$County == "NORTH YORKSHIRE" ] <- "YORKSHIRE"
crimeData$County[crimeData$County == "SOUTH YORKSHIRE" ] <- "YORKSHIRE"

view(crimeData)


#========Drug Offence Rate per 10000 in 2021-2022 =========

filtered_data <- crimeData %>%
  filter((County %in% c("OXFORDSHIRE", "YORKSHIRE")) & (Year %in% c(2021, 2022))) %>% 
  filter(CrimeType == "Drugs") %>% 
  mutate(DrugOffenceRate = (CrimeCount / (Population2021+Population2022)) * 10000) %>% 
  as_tibble()

ggplot(data = filtered_data, aes(x = County, y = CrimeCount, fill = CrimeType)) +
  geom_boxplot() +
  labs(title = "Drug Offence Rate per 10000 in 2021-2022") +
  coord_flip()



# Piechart for 2021 Robbery 

RobberyData <- crimeData %>% 
  filter(CrimeType=="Robbery", Year == 2022) %>%
  group_by(District) %>%
  mutate(sumCount = sum(CrimeCount)) %>% 
  ungroup() %>%
  mutate(perc =sumCount / sum(CrimeCount)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(District, sumCount, perc, labels) %>% 
  select(District, sumCount, perc, labels)


RobberyData %>% 
  ggplot(aes(x = "", y = perc, fill = District)) +
  geom_col(color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  theme_void()+
  labs(title="2022 Robbery by District")




#LINEGRAPH of drug offence rate per 10000 people from 2021 -2022 of both city

# Filter and calculate drug offense rate per town and county for years 2022 and 2022

Drugs_2021_2022 <- crimeData %>%
  filter((County %in% c("OXFORDSHIRE", "YORKSHIRE")) & (Year %in% c(2021, 2022))) %>% 
  filter(CrimeType == "Drugs") %>% 
  mutate(DrugOffenceRate = (CrimeCount / (Population2021+Population2022)) * 10000) %>% 
  as_tibble()

# Create a line chart
ggplot(Drugs_2021_2022, aes(x = Year, y = DrugOffenceRate, color = County)) +
  geom_line() +
  labs(x = "Year", y = "Drug Offense Rate per 10000 People", title = "Drug Offense Rate per 10000 People from 2021 to 2022", color = "County")















#radar chart

# Step 1: Filter the dataset
filtered_data <- crimeData %>%
  filter(Year %in% c(2021, 2022) & CrimeType == "Vehicle crime")

# Step 2: Calculate the total crime count per county and year
crime_totals <- filtered_data %>%
  group_by(County, Year) %>%
  summarise(TotalCrime = sum(CrimeCount))

# Step 3: Pivot the data to have years as columns
pivot_data <- crime_totals %>%
  pivot_wider(names_from = Year, values_from = TotalCrime)

# Step 4: Install and load the fmsb package
install.packages("fmsb")
library(fmsb)

# Step 5: Create the radar chart
radar_chart <- radarchart(pivot_data[, -1],
                          axistype = 1,
                          seg = 4,
                          plty = 1:2,
                          pcol = c("blue", "red"),
                          plwd = 2,
                          vlabels = colnames(pivot_data)[-1])

# Step 6: Add a legend
legend("topright",
       legend = c("2021", "2022"),
       col = c("blue", "red"),
       lty = 1,
       lwd = 2,
       bty = "n")

# Step 7: Set a title for the chart
title(main = "Vehicle Crime Rate 2021-2022")

# Step 8: Print the radar chart
print(radar_chart)

















# =======================school start================================

schoolData = read_csv('Cleaning/Cleaned Datasets/Cleaned_School_Data.csv', show_col_types = FALSE)
OXFORDSHIREschool = read_csv("Cleaning/Cleaned Datasets/OXFORSSHIRESchoolData.csv")
YORKSHIREschool = read_csv('Cleaning/Cleaned Datasets/YORKSHIRESchoolData.csv')


Town = read_csv("Cleaning/Cleaned datasets/Cleaned_Town_population.csv")%>% 
  select(shortPostcode,District)

view(Town)
view(schoolData)
view(YORKSHIREschool)
view(group_by(OXFORDSHIREschool,shortPostcode))


schoolData = schoolData %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit() 


#- box plot= average attenment score in 2022 both country district

schoolData %>%
  group_by(District) %>%
  summarise(AverageAttainment = mean(Attainment8Score)) %>%
  ggplot(aes(x = factor(District), y = AverageAttainment)) +
  geom_boxplot(color = "red", fill = "steelblue") +  
  geom_text(aes(label = AverageAttainment), vjust = -0.85) +
  scale_x_discrete() +
  labs(title = "Average Attainment8Score by district")



#   - line chart = oxfordshire district average attenment score  from 2020 - 2022

merged_data <- OXFORDSHIREschool %>%
  inner_join(Town, by ="shortPostcode") %>% 
  na.omit()
view(merged_data)

filtered_data <- merged_data %>%
  filter(Year %in% c(2020, 2021, 2022)) %>%
  group_by(District, Year) %>%
  summarise(AverageAttainment = mean(Attainment8Score))


filtered_data

ggplot(filtered_data, aes(x = Year, y = AverageAttainment, group = District)) +
  geom_line(size = 1, color = "red") +
  geom_text(aes(label = AverageAttainment), vjust = -0.85) +
  geom_point(size = 2, color = "steelblue") +
  labs(title = "Oxfordshire District Average Attainment Score from 2020 to 2022")


#   - line chart = yorkshire  district average attenment score from 2020-2022

merged_data_york <- YORKSHIREschool %>%
  inner_join(Town, by ="shortPostcode")
view(merged_data_york)

filtered_data_york <- merged_data_york %>%
  filter(Year == 2020 | Year == 2021 | Year == 2022) %>%
  group_by(District,Year) %>%
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  
  filtered_data_york

ggplot(filtered_data_york, aes(x = Year, y = AverageAttainment, group = District)) +
  geom_line(size = 1, color = "red") +
  geom_text(aes(label = AverageAttainment), vjust = -0.85) +
  geom_point(size = 2, color = "steelblue") +
  labs(title = "Yorkshire District Average Attainment Score from 2020 to 2022")


#===========school_end==================


#=====================  liner regression ==================================

Towns = read_csv("Cleaning/Cleaned datasets/Cleaned_Town_population.csv")%>%
  select(shortPostcode, Town, District, County)

prices = read_csv("Cleaning/cleaned datasets/Cleaned_House_Pricing_2019-2022.csv")

speeds = read_csv("Cleaning/Cleaned datasets/Cleaned_Broadband_Speed.csv") %>% 
  na.omit()  

crime=read_csv("Cleaning/cleaned datasets/Cleaned_Crime_Data.csv")

schools=read_csv("Cleaning/Cleaned Datasets/Cleaned_School_Data.csv") %>% 
  na.omit()


#------------------------------House prices vs Download Speed----------------------------------------

options(scipen=999)

HousePrices = prices %>%
  filter(Year=="2021" | Year=="2022") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))


BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  summarise(AverageDownload=mean(Avgdownload))

lm_res = HousePrices %>% left_join(BroardbandSpeeds,by="Town")
model = lm(data= lm_res, Price~AverageDownload)
summary(model)

color= c("OXFORDSHIRE" = "red", "YORKSHIRE" = "blue")

ggplot(lm_res,aes(x=AverageDownload,y=Price)) +
  geom_point(data = filter(lm_res,County.x=="YORKSHIRE"),aes(color="YORKSHIRE"))+
  geom_point(data = filter(lm_res,County.x=="OXFORDSHIRE"), aes(color="OXFORDSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="lightgreen")+
  labs(x="Download Speed (Mbit/s)",y="Price",title="House Prices vs Download Speed",color="County")


#-----------------------------------------------------------------------------------------
#----------------------------------House price and drug offence--------------------------------------------------

HousePrices = prices %>%
  filter(Year=="2021" | Year=="2022" ) %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  na.omit()

lm_res1 = HousePrices %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model1 = lm(data= lm_res1, Price~CrimeCount)
summary(model1)

color= c("OXFORDSHIRE" = "yellow", "YORKSHIRE" = "Green")

ggplot(lm_res1,aes(x=CrimeCount,y=Price)) +
  geom_point(data = filter(lm_res1,County.x=="YORKSHIRE"),aes(color="YORKSHIRE"))+
  geom_point(data = filter(lm_res1,County.x=="OXFORDSHIRE"), aes(color="OXFORDSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="peachpuff4")+
  labs(x="count",y="Price",title="House Prices vs Drug",color="County")






#==================   average attainment  vs house prices ==================


attainment= schools %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(meanAttainment=mean(Attainment8Score))

attainment

HousePrices = prices %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

lm_res2 = HousePrices %>% left_join(attainment ,by="Town") %>% 
  na.omit()
lm_res2
model1 = lm(data= lm_res2, Price~meanAttainment)
summary(model1)

color= c("OXFORDSHIRE" = "yellow", "YORKSHIRE" = "Green")

ggplot(lm_res2,aes(x=meanAttainment,y=Price)) +
  geom_point(data = filter(lm_res2,County.x=="YORKSHIRE"),aes(color="YORKSHIRE"))+
  geom_point(data = filter(lm_res2,County.x=="OXFORDSHIRE"), aes(color="OXFORDSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="peachpuff4")+
  labs(x="attainment",y="Price",title="average attainment  vs house prices",color="County")




#===========   average attenment score vs drug offence per 10000 people ========

Towns_Populations = read_csv("Cleaning/Cleaned datasets/Cleaned_Town_population.csv")

attainment= schools %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(meanAttainment=mean(Attainment8Score))



Drugs = crime %>%
  left_join(Towns_Populations,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  mutate(DrugOffenceRate = (CrimeCount / (Population2019+Population2020+Population2021+Population2022)) * 10000) %>% 
  as_tibble() %>% 
  na.omit()


lm_res3 = Drugs %>% left_join(attainment ,by="Town") %>% 
  na.omit()

lm_res3

model3 = lm(data= lm_res3, DrugOffenceRate~meanAttainment)
summary(model3)

color= c("OXFORDSHIRE" = "yellow", "YORKSHIRE" = "Green")

ggplot(lm_res3,aes(x=meanAttainment,y=DrugOffenceRate)) +
  geom_point(data = filter(lm_res3,County.x=="YORKSHIRE"),aes(color="YORKSHIRE"))+
  geom_point(data = filter(lm_res3,County.x=="OXFORDSHIRE"), aes(color="OXFORDSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="peachpuff4")+
  labs(x="attainment",y="Drups per 10000",title="average attenment score vs drug offence per 10000 people ",color="County")



#=============drug rate vs average attenment score per 10000 people =======

attainment <- schools %>%
  left_join(Towns, by = "shortPostcode") %>%  
  group_by(Town, County) %>%
  summarise(meanAttainment = mean(Attainment8Score))

# Filter and calculate drug offense rate per town and county
Drugs <- crime %>%
  left_join(Towns_Populations, by = "shortPostcode") %>%
  group_by(Town, County) %>%
  filter(CrimeType == "Drugs") %>% 
  mutate(DrugOffenceRate = (CrimeCount / (Population2019 + Population2020 + Population2021 + Population2022)) * 10000) %>% 
  as_tibble() %>% 
  na.omit()

# Merge the data frames and remove rows with missing values
lm_res4 <- Drugs %>%
  left_join(attainment, by = "Town") %>% 
  na.omit()

# Fit linear regression model
model4 <- lm(DrugOffenceRate ~ meanAttainment, data = lm_res4)

summary(model4)

# Create a scatter plot
color <- c("OXFORDSHIRE" = "yellow", "YORKSHIRE" = "Green")

ggplot(lm_res4, aes(x = meanAttainment, y = DrugOffenceRate)) +
  geom_point(data = filter(lm_res4, County.x == "YORKSHIRE"), aes(color = "YORKSHIRE")) +
  geom_point(data = filter(lm_res4, County.x == "OXFORDSHIRE"), aes(color = "OXFORDSHIRE")) +
  geom_smooth(method = lm, se = FALSE, color = "peachpuff4") +
  labs(x = "Attainment", y = "Drugs per 10000", title = "Average Attainment Score vs Drug Offense per 10000 People", color = "County")



#========= average download speed vs average attainment score ======


# Calculate mean attainment score per town and county
attainment <- schools %>%
  left_join(Towns, by = "shortPostcode") %>%  
  group_by(Town, County) %>%
  summarise(meanAttainment = mean(Attainment8Score))


# Calculate average download speed per town and county
download_speeds <- speeds %>%
  left_join(Towns_Populations, by = "shortPostcode") %>% 
  group_by(Town, County) %>%
  summarise(meanDownloadSpeed = mean(Avgdownload)) %>%
  na.omit()

# Merge the data frames and remove rows with missing values
lm_res5 <- attainment %>%
  left_join(download_speeds, by = "Town") %>% 
  na.omit()

model <- lm(meanDownloadSpeed ~ meanAttainment, data = lm_res5)

summary(model)

# Create a scatter plot
color <- c("OXFORDSHIRE" = "yellow", "YORKSHIRE" = "Green")

ggplot(lm_res5, aes(x = meanAttainment, y = meanDownloadSpeed)) +
  geom_point(data = filter(lm_res5, County.x == "YORKSHIRE"), aes(color = "YORKSHIRE")) +
  geom_point(data = filter(lm_res5, County.x == "OXFORDSHIRE"), aes(color = "OXFORDSHIRE")) +
  geom_smooth(method = lm, se = FALSE, color = "peachpuff4") +
  labs(x = "Average Attainment Score", y = "Average Download Speed", title = "Average Attainment Score vs Average Download Speed", color = "County")