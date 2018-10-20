library(tidyverse)

e_data <- read_csv("~/Downloads/usa_00009.csv")

source("../../scripts/create_hists.R")

RentCat3 <- e_data %>% 
  select(-(1:2)) %>% 
  mutate(median_rent = median(RENT))

e_data %>% group_by(REGION) %>%
  summarize(regions = n())

RentCat2 <- e_data %>% 
  select(-(1:2)) %>%
  filter(RENT>0, AGE %in% 15:50, SEX ==2) %>%
  filter(CITYPOP > 1) %>%
  arrange(desc(CITYPOP)) %>%
  mutate(rentc=cut(RENT, breaks=quantile(RENT,prob=c( 0,0.25,0.5, 0.75, 1)),include.lowest=T))%>%
  group_by (COUNTY) %>%
  mutate(medRent=median(RENT),
         Ncnty=n()) %>%
  filter(Ncnty > 1000)  %>%
  group_by(AGE,COUNTY) %>%
  summarize (numer = sum(FERTYR==2),
             denom= n(),
             asfr=numer/denom,
             medRent=mean(medRent)) %>%
  group_by(COUNTY) %>% summarize(tfr=sum(asfr),
                                 medRent=mean(medRent))

RentCat2 %>% 
  ggplot(aes(x = COUNTY, y = tfr)) + geom_point() +
  geom_line(aes(medRent, col = "red"))


rent_cat <- e_data %>% filter(RENT>0, AGE %in% 15:50, SEX ==2)

rent_cat %>% group_by(COUNTY) %>%
  summarize(count = n())
