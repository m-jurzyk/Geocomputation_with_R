#Guy Carpenter - Graduate - Analyst / Catastrophe Modelling / 2 openings - zadanie rekrutacyjne

## MGR INZ MACIEJ JURZYK 

#TASK 1 ----

#In the data below you can see observed historical losses of two insurance companies - 
#ABC Insurance and XYZ Insurance - accumulated by individual years.													
#The observed historical losses represent payments the insurance companies paid to
#indemnify losses caused by natural catastrophes like windstorms or floods.													
#Using basic statistics compare loss records of both companies and comment.													
#Which company do you think can more easily manage risk coming from natural catastrophe events?													

# 0. Conversion excel file into CSV ----

## CSV file is clearly unorganized with string characters instead of numeric format

# R workflow 

# 0.5 Libraries loading ----

library(tidyverse)
library(dplyr)
library(ggplot2)


# Organizational changes in data frame new columns names 

# 1. Read Data  ----

t1 <- read_csv("/Users/maciejjurzyk/Downloads/task1fixed.csv") %>%
  mutate(
    year_id = as.integer(`Year ID`),
    year = as.integer(`Year`),
    loss_abc = `Loss - ABC Insurance`,
    loss_xyz = `Loss - XYZ Insurance`
  ) %>%
  select(year_id, year, loss_abc, loss_xyz)

# When we have organised data frame we can mutate columns from character into numeric values
# by removing space characters #str_remove_all(loss_abc, "\\s") and changing data 
# into numeric format


#Erase space characters and transform into numeric using simple regex in stringr package

library(stringr)


t1 <- t1 %>% mutate(
  loss_abc = str_remove_all(loss_abc, "\\s") %>% as.numeric(),
  loss_xyz = str_remove_all(loss_xyz, "\\s") %>%  as.numeric()
)


# 2. View table ----

view(t1)

#All good right now, we can start to summary data 

# 3. Data summary ----

summary(t1)

# year_id           year         loss_abc            loss_xyz        
#Min.   : 1.00   Min.   :1972   Min.   :  2394688   Min.   : 20074711  
#1st Qu.:13.25   1st Qu.:1984   1st Qu.:  7592236   1st Qu.: 35623527  
#Median :25.50   Median :1996   Median : 26154674   Median : 58142676  
#Mean   :25.50   Mean   :1996   Mean   : 62751446   Mean   : 62814197  
#3rd Qu.:37.75   3rd Qu.:2009   3rd Qu.: 93711731   3rd Qu.: 92341382  
#Max.   :50.00   Max.   :2021   Max.   :291118403   Max.   :109494135  

# 4. Ploting data using GGPLOT ---- 

t1 %>%  ggplot(mapping = aes(x=year, y=loss_abc),)+
  geom_boxplot()+
  geom_point()+
  geom_smooth()+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Year", y = "Loss of ABC Insurance company") +
  ggtitle("Loss of ABC Insurance company Over Time") +
  theme_minimal()

t1 %>%  ggplot(mapping = aes(x=year, y=loss_xyz),)+
  geom_boxplot()+
  geom_smooth()+
  geom_point()+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Year", y = "Loss of XYZ Insurance company") +
  ggtitle("Loss of XYZ Insurance company Over Time") +
  theme_minimal()

#5.  Task summary ---- 

#Based on the basic statistical analysis made in R studio, it appears that XYZ Company has a higher:
#mean, median, and third quartile then ABC Company,
#suggesting that XYZ Company has a higher potential risk from natural catastrophe events. 

#ABC Company has a higher minimum value, indicating that it may have a lower potential risk overall. 
#Moreover, the dispersion of data and the downward trend recorded since
#the mid-1990s indicate a lower risk of running a business

#Company XYZ is characterized by an extremely uneven dispersion of observations,
#making it difficult to observe any trend and in fact make any predictions based on a given database 


# TASK 2 ---- 

## 0. Library loading ----

library(tidyverse)
library(stats)

## 1.  CSV file loading + changes in organization of tables ---- 

a1 <- read_csv("/Users/maciejjurzyk/Downloads/tab2.csv") %>% 
  mutate(
    year_id=as.integer(`Year ID`),
    year=(`Year`),
    loss_abc=`Loss - ABC Insurance`) %>% 
  select(year_id, year, loss_abc)

# When we have organised data frame we can mutate columns from character into numeric values
# by removing space characters #str_remove_all(loss_abc, "\\s") and changing data 
# into numeric format


#Erase space characters and transform into numeric using simple regex in stringr package

##2. Space character erasing and saving columns as numeric ----

a1 <- a1 %>% mutate(
  year = str_remove_all(year, "\\s") %>% as.integer(),
  loss_abc= str_remove(loss_abc, "\\s") %>% as.numeric()
)

a1 <- a1 %>% mutate(
  loss_abc = str_remove_all(loss_abc, "\\s") %>% as.numeric(),
  year = str_remove_all(year, "\\s") %>%  as.numeric()
)

a1 %>% ggplot(mapping = aes(y=loss_abc, x=year))+
  geom_point()+
  geom_smooth()+
  geom_boxplot(alpha=0.1)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Year", y = "Loss of ABC Insurance company") +
  ggtitle("Loss of ABC Insurance company Over Time") +
  theme_minimal()


#From the historical losses of ABC Insurance, calculate the probability of annual indemnification

#a/ 50,000,000 or larger							
#b/ 100,000,000 or larger		

# Load the data
loss_abc <- a1$loss_abc

mean_loss_abc <- mean(loss_abc)

sd_loss_abc <- sd(loss_abc)

# Calculate the CDF of the loss_abc equal to 50,000,000 or larger

pnorm(50000000, mean = mean_loss_abc, sd = sd_loss_abc)

#Probability of annual indemnification for  50,000,000 or larger = 0.435 

# Calculate the CDF of the loss_abc equal to 100,000,000 or larger

pnorm(100000000, mean = mean_loss_abc, sd = sd_loss_abc)

#Probability of annual indemnification for  100,000,000 or larger = 0.6838 


# TASK 2 ---- 

## 0. Library loading ----

library(tidyverse)
library(stats)

## 1.  CSV file loading + changes in organization of tables ---- 

a1 <- read_csv("/Users/maciejjurzyk/Downloads/tab2.csv") %>% 
  mutate(
    year_id=as.integer(`Year ID`),
    year=(`Year`),
    loss_abc=`Loss - ABC Insurance`) %>% 
  select(year_id, year, loss_abc)

# When we have organised data frame we can mutate columns from character into numeric values
# by removing space characters #str_remove_all(loss_abc, "\\s") and changing data 
# into numeric format


#Erase space characters and transform into numeric using simple regex in stringr package

##2. Space character erasing and saving columns as numeric ----

a1 <- a1 %>% mutate(
  year = str_remove_all(year, "\\s") %>% as.integer(),
  loss_abc= str_remove(loss_abc, "\\s") %>% as.numeric()
)

a1 <- a1 %>% mutate(
  loss_abc = str_remove_all(loss_abc, "\\s") %>% as.numeric(),
  year = str_remove_all(year, "\\s") %>%  as.numeric()
)

a1 %>% ggplot(mapping = aes(y=loss_abc, x=year))+
  geom_point()+
  geom_smooth()+
  geom_boxplot(alpha=0.1)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Year", y = "Loss of ABC Insurance company") +
  ggtitle("Loss of ABC Insurance company Over Time") +
  theme_minimal()


#From the historical losses of ABC Insurance, calculate the probability of annual indemnification

#a/ 50,000,000 or larger							
#b/ 100,000,000 or larger		

# Load the data
loss_abc <- a1$loss_abc

mean_loss_abc <- mean(loss_abc)

sd_loss_abc <- sd(loss_abc)

# Calculate the CDF of the loss_abc equal to 50,000,000 or larger

pnorm(50000000, mean = mean_loss_abc, sd = sd_loss_abc)

#Probability of annual indemnification for  50,000,000 or larger = 0.435 

# Calculate the CDF of the loss_abc equal to 100,000,000 or larger

pnorm(100000000, mean = mean_loss_abc, sd = sd_loss_abc)

#Probability of annual indemnification for  100,000,000 or larger = 0.6838 


#Please get familiar with return period concept. 

#The "return period" is a statistical term used to describe the frequency of an event 
#with a certain magnitude that is expected to happen. In the context of catastrophe modeling, 
#it refers to the estimated frequency of occurrence of extreme weather events, such as hurricanes, 
#earthquakes, or flooding, that have the potential to cause significant damage to people and property.


#The return period is typically expressed in years and is calculated based on the historical data of
#the events of interest. For example, if an event with a return period of 
#100 years is defined as one that has a 1% chance of occurring in any given year,
#it means that such an event is expected to occur once every 100 years, on average.
#Return period is the reciprocal value of expected frequency, therefore a 2-year event
#(insurance loss, flood discharge, wind speed etc.) 


#has a 1/2 = 0.5 or 50% chance of being reached or exceeded in any one year.																	
#Similarly, a 5-year event has a 1/5 = 0.2 or 20% chance of being reached or exceeded in any one year.	

#Plot the chart showing dependence between return period and annual indemnification.							

#We also assess the company's annual indemnifications related to natural catastrophes 
#by our own catastrophe model.										
#The model was calibrated based on performance of all companies in the market
#it is not based on performance of ABC Insurance only.		


#Would you consider the model accurate for ABC Insurance if it calculates annual payout of										
#a/ 26 million with return period of 2 years?										
# b/ 211 million with return period of 5 years?		


#TASK 3 ----

#For an insurance company, the loss from natural catastrophe event occurs when 
#the insured property is exposed to hazard of the event.															

#When the hazard is strong enough, it may cause damages on insured 
#property. In the catastrophe models, the damage on insured property 
#is a product of insured value and damage ratio.															

#Damage ratio represent damageability of hazard. On the same property, 
#for instance, wind speed 35 m/s is likely to cause larger damage than wind speed 20 m/s.		


## 1.  CSV file loading + changes in organization of tables ---- 


#Portfolio table 

p1 <- read_csv("/Users/maciejjurzyk/Downloads/tab_portfolio.csv") %>% 
  mutate(
    country =as.character(   Country ),
    cresta =as.character(`CRESTA Name`),
    cresta_number=as.numeric( `CRESTA Number`),
    risk_type =as.character(`Risk Type`),
    coverage =as.character( Coverage),
    sum_insured = `Sum Insured`,
    units =as.character(`Units of Sum Insured`)) %>% 
  select(country, cresta, cresta_number, risk_type, coverage, sum_insured, units)

p1 <- p1 %>% mutate(
  sum_insured = str_remove_all(sum_insured, "\\s") %>% as.numeric())


# Hazard table 

h1 <- read_csv("/Users/maciejjurzyk/Downloads/tab_hazard.csv") %>%
  mutate(
    country = Country ,
    cresta = `CRESTA Name`  ,
    cresta_number = `CRESTA Number`,
    hazard_intensity = `Hazard Intensity`,
    hazard_units = `Hazard Units`
  ) %>% 
  select(country,cresta,cresta_number,hazard_intensity,hazard_units)

h1

#Vulnerability table 

v1 <-  read_csv("/Users/maciejjurzyk/Downloads/tab_vulnerability.csv") %>% 
  mutate(
    hazard_intensity_ms= `Hazard Intensity [m/s]`,
    mean_damage_ratio_percent = `Mean Damage Ratio [% of Sum Insured]`
  ) %>% 
  select(hazard_intensity_ms, mean_damage_ratio_percent)

# Remove the "%" symbol using sub()

vnew =  sub("%", "", v1$mean_damage_ratio_percent)


# Convert the resulting string to a numeric data type using as.numeric()

numeric_string

# Convert , into . 

vnew2 = sub(",", ".", vnew)


# Convert the character strings to numeric values using as.numeric() 
# with the correct decimal separator and rewrite to table

vnew2 = as.numeric((vnew2))

v1$mean_damage_ratio_percent = vnew2

v1
p1
h1



##2. Calculations ----




#Calculate expected loss from windstorm Lothar which crossed France in December 1999.															
#Information on insured values can be found in sheet "Task 3_Portfolio"															
#Information on hazard can be found in sheet "Task 3_Hazard"															
#Information on damage ratios can be found in sheet "Task 3_Vulnerability"															

#Hint: use lookup functions in excel - e.g. VLOOKUP or OFFSET - MATCH - or feel free to convert to other tools.															


