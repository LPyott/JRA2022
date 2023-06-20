library(dplyr)
library(splitstackshape)

setwd("C:/Users/75LPYOTT/OneDrive - West Chester University of PA/Desktop/JRA2022")
jra2022march24 <- read.csv("jra2022march24.csv", header=T)

jra2022march24 <- jra2022march24 %>% 
  select(Q36, Q26, Q13, Q8)
  #Q36 is Everyday Essentials
  #Q26 is Family Friendly
  #Q13 is Standard Box
  #Q8 is Country
jra2022march24 <- jra2022march24 %>% 
  rename(EE.Food = Q36) %>%
  rename(FF.Food = Q26) %>%
  rename(Standard.Food = Q13) %>%
  rename(Country = Q8) 

Split <- jra2022march24 %>% 
  cSplit("EE.Food", ",") %>%
  cSplit("FF.Food", ",") %>%
  cSplit("Standard.Food", ",")

Split <- Split %>% 
    mutate(Ukraine = ifelse(Country == "Ukraine", "Yes", "No")) 

Split <- Split %>% 
  filter(!is.na(EE.Food_01))

Split <- Split %>%
  filter(!is.na(FF.Food_1))

Split <- Split %>% 
  filter(!is.na(Standard.Food_01)) %>%
  filter(!is.na(Country))

#Everyday Essentials Picks for Ukrainians
EE.Ukraine <- Split %>%
  filter(Ukraine==c("Yes"))
nrow(EE.Ukraine)

table(EE.Ukraine$EE.Food_01)
table(EE.Ukraine$EE.Food_02)
table(EE.Ukraine$EE.Food_03)
table(EE.Ukraine$EE.Food_04)
table(EE.Ukraine$EE.Food_05)
table(EE.Ukraine$EE.Food_06)
table(EE.Ukraine$EE.Food_07)
table(EE.Ukraine$EE.Food_08)
table(EE.Ukraine$EE.Food_09)
table(EE.Ukraine$EE.Food_10)
table(EE.Ukraine$EE.Food_11)

#Everyday Essentials Picks for Non-Ukrainians
EE.Others <- Split %>%
  filter(!(Country==c("No")))
nrow(EE.Others)

table(EE.Others$EE.Food_01)
table(EE.Others$EE.Food_02)
table(EE.Others$EE.Food_03)
table(EE.Others$EE.Food_04)
table(EE.Others$EE.Food_05)
table(EE.Others$EE.Food_06)
table(EE.Others$EE.Food_07)
table(EE.Others$EE.Food_08)
table(EE.Others$EE.Food_09)
table(EE.Others$EE.Food_10)
table(EE.Others$EE.Food_11)

#Family Friendly Picks for Ukrainians
FF.Ukraine <- Split %>%
  filter(Ukraine==c("Yes"))
nrow(FF.Ukraine)

table(FF.Ukraine$FF.Food_1)
table(FF.Ukraine$FF.Food_2)
table(FF.Ukraine$FF.Food_3)
table(FF.Ukraine$FF.Food_4)
table(FF.Ukraine$FF.Food_5)
table(FF.Ukraine$FF.Food_6)
table(FF.Ukraine$FF.Food_7)

#Family Friendly Picks for Non-Ukrainians
FF.Others <- Split %>%
  filter(!(Country==c("No")))
nrow(FF.Others)

table(FF.Others$FF.Food_1)
table(FF.Others$FF.Food_2)
table(FF.Others$FF.Food_3)
table(FF.Others$FF.Food_4)
table(FF.Others$FF.Food_5)
table(FF.Others$FF.Food_6)
table(FF.Others$FF.Food_7)

#Standard Box Picks for Ukrainians
Standard.Ukraine <- Split %>%
  filter(Ukraine==c("Yes"))
nrow(Standard.Ukraine)

table(Standard.Ukraine$Standard.Food_01)
table(Standard.Ukraine$Standard.Food_02)
table(Standard.Ukraine$Standard.Food_03)
table(Standard.Ukraine$Standard.Food_04)
table(Standard.Ukraine$Standard.Food_05)
table(Standard.Ukraine$Standard.Food_06)
table(Standard.Ukraine$Standard.Food_07)
table(Standard.Ukraine$Standard.Food_08)
table(Standard.Ukraine$Standard.Food_09)
table(Standard.Ukraine$Standard.Food_10)
table(Standard.Ukraine$Standard.Food_11)
table(Standard.Ukraine$Standard.Food_12)
table(Standard.Ukraine$Standard.Food_13)

#Standard Box Picks for Non-Ukrainians
Standard.Others <- Split %>%
  filter(!(Country==c("No")))
nrow(Standard.Others)

table(Standard.Others$Standard.Food_01)
table(Standard.Others$Standard.Food_02)
table(Standard.Others$Standard.Food_03)
table(Standard.Others$Standard.Food_04)
table(Standard.Others$Standard.Food_05)
table(Standard.Others$Standard.Food_06)
table(Standard.Others$Standard.Food_07)
table(Standard.Others$Standard.Food_08)
table(Standard.Others$Standard.Food_09)
table(Standard.Others$Standard.Food_10)
table(Standard.Others$Standard.Food_11)
table(Standard.Others$Standard.Food_12)
table(Standard.Others$Standard.Food_13)


Ukrainian <-c(rep("yes", 14), rep("no", 14))
Food <- rep(c("toilet paper","masks", "soap","crackers", "tuna", 
                 "canned fruit","canned vegetables",
                 "oil","canned salmon","canned beans",
                  "pasta","canned fruit","soup mix", "oatmeal"), 2)
          
Count <- c(30, 40, 50, 83, 22, 91, 14, 54, 44, 55, 66, 77,
           12, 45, 54, 66, 44, 33, 22, 66, 77, 88, 55, 44,
           12, 23, 34, 45)
data <- data.frame(Ukrainian, Food, Count)
dev.off()
library(ggplot2)
ggplot(data, aes(fill=Ukrainian, y=Count, x=Food)) + 
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(size=7, angle=45))

