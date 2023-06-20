
#Family friendly population
#Food insecurity

library(dplyr)
library(tidyverse)
#library(ggplot2)
#library(epiR)

library(splitstackshape)
###################################################

#Import data sets 
setwd("C:/Users/75LPYOTT/OneDrive - West Chester University of PA/Desktop/JRA2022")
allJRA=read.csv("allJRA.csv", header=T)
jra2022march24=read.csv("jra2022march24.csv", header=T)
nrow(jra2022march24)



##########################################
#2022

jra22=jra2022march24 %>%
  rename(benefits=Q14) 

jra22$year="2022"


#################################################
#create one variable per benefit
benefits <- jra22  %>%
  mutate(enough=case_when(grepl("enough", benefits)~"Yes", TRUE~"No")) %>%
  mutate(bills=case_when(grepl("bills", benefits)~"Yes", TRUE~"No")) %>%
  mutate(secure=case_when(grepl("secure", benefits)~"Yes", TRUE~"No")) %>%
  mutate(nutritious=case_when(grepl("nutritious", benefits)~"Yes", TRUE~"No")) %>%
  mutate(illness=case_when(grepl("illness", benefits)~"Yes", TRUE~"No")) %>%
  mutate(transport=case_when(grepl("transportation", benefits)~"Yes", TRUE~"No"))

table(benefits$enough)
table(benefits$bills)
table(benefits$secure)
table(benefits$nutritious)
table(benefits$illness)
table(benefits$transport)

################################################################
###2022 Holocaust survivors

#HC3 Holocaust=yes is respondents who identified as HC
HC3 <-filter(jra22, Q30=="Yes")

#HC4 Famil_holo is respondents who live with a HC
HC4 <-filter(jra22, Q31=="Yes")

#HC is the union of HC1 and HC2 removing duplicates
HC22 <- union(HC3, HC4)

#remove missing values
nrow(HC22)
HC22 <- HC22 %>%
  drop_na(Q18) 
nrow(HC22)

benefitsHC <- HC22  %>%
  mutate(enough=case_when(grepl("enough", benefits)~"Yes", TRUE~"No")) %>%
  mutate(bills=case_when(grepl("bills", benefits)~"Yes", TRUE~"No")) %>%
  mutate(secure=case_when(grepl("secure", benefits)~"Yes", TRUE~"No")) %>%
  mutate(nutritious=case_when(grepl("nutritious", benefits)~"Yes", TRUE~"No")) %>%
  mutate(illness=case_when(grepl("illness", benefits)~"Yes", TRUE~"No")) %>%
  mutate(transport=case_when(grepl("transportation", benefits)~"Yes", TRUE~"No"))

table(benefitsHC$enough)
table(benefitsHC$bills)
table(benefitsHC$secure)
table(benefitsHC$nutritious)
table(benefitsHC$illness)
table(benefitsHC$transport)

###################################################################################
#Get FF responses from 2022 and remove missing values
bluebag2022 <-filter(jra22, Q23=="Yes")
nrow(bluebag2022)
bluebag2022 <- bluebag2022 %>%
  drop_na(Q18) 
nrow(bluebag2022)

benefitsBB <- bluebag2022  %>%
  mutate(enough=case_when(grepl("enough", benefits)~"Yes", TRUE~"No")) %>%
  mutate(bills=case_when(grepl("bills", benefits)~"Yes", TRUE~"No")) %>%
  mutate(secure=case_when(grepl("secure", benefits)~"Yes", TRUE~"No")) %>%
  mutate(nutritious=case_when(grepl("nutritious", benefits)~"Yes", TRUE~"No")) %>%
  mutate(illness=case_when(grepl("illness", benefits)~"Yes", TRUE~"No")) %>%
  mutate(transport=case_when(grepl("transportation", benefits)~"Yes", TRUE~"No"))

table(benefitsBB$enough)
table(benefitsBB$bills)
table(benefitsBB$secure)
table(benefitsBB$nutritious)
table(benefitsBB$illness)
table(benefitsBB$transport)

############
#Everyday essentials

benefitsEE <-filter(jra22, Q34=="Yes")
benefitsEE$benefitsEE=benefitsEE$Q37

benefitsEE <- benefitsEE  %>%
  mutate(bills=case_when(grepl("bills", benefitsEE)~"Yes", TRUE~"No")) %>%
  mutate(health=case_when(grepl("health", benefitsEE)~"Yes", TRUE~"No")) %>%
  mutate(illness=case_when(grepl("illness", benefitsEE)~"Yes", TRUE~"No")) %>%
  mutate(transport=case_when(grepl("transportation", benefitsEE)~"Yes", TRUE~"No"))
nrow(benefitsEE)

table(benefitsEE$bills)
table(benefitsEE$health)
table(benefitsEE$illness)
table(benefitsEE$transport)