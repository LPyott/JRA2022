
#Family friendly population
#Food insecurity

library(dplyr)
library(tidyverse)
#library(ggplot2)
#library(epiR)

library(splitstackshape)
###################################################

#Import data sets 


#filter out households >2
#then find senior households
jra20=allJRA %>%
  rename(benefits=Q.16) %>%
  select(c(benefits))


jra20$year="2020"


##########################################
#2022

jra22=jra2022march24 %>%
  rename(benefits=Q14) %>%
  select(c(benefits))

jra22$year="2022"


#####
# combine 2020 and 2022

benefits=rbind(jra20, jra22)


#################################################
#create one variable per benefit
benefits <- benefits %>%
  mutate(enough=case_when(grepl("enough", benefits)~"Yes", TRUE~"No")) %>%
  mutate(bills=case_when(grepl("bills", benefits)~"Yes", TRUE~"No")) %>%
  mutate(secure=case_when(grepl("secure", benefits)~"Yes", TRUE~"No")) %>%
  mutate(nutritious=case_when(grepl("nutritious", benefits)~"Yes", TRUE~"No")) %>%
  mutate(illness=case_when(grepl("illness", benefits)~"Yes", TRUE~"No")) %>%
  mutate(transport=case_when(grepl("transportation", benefits)~"Yes", TRUE~"No"))

prop.table(table(benefits$enough, benefits$year))
prop.table(table(benefits$bills,benefits$year))
table(benefits$secure)
table(benefits$nutritious)
table(benefits$illness)
table(benefits$transport)



chisq.test(benefits$year, benefits$transport)
t<-table(benefits$year, benefits$transport)
t


#### https://r-coder.com/barplot-r/
barplot(t,
        main="Transportation vs. Year",
        xlab="trans", ylab="Relative Frequency",
        col=c('darkblue', 'red'),
        width = c(1, 1),
        legend.text=rownames(t),
        beside=FALSE,
        args.legend = list(title="year", x = "center", inset = c(0.10, 0)),
        space=c(1))

