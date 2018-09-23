library(tidyverse)

e_data <- read_csv("~/Downloads/usa_00009.csv")

RentCat2 <- e_data %>% filter(RENT>0, AGE %in% 15:50, SEX ==2) %>%
  #mutate(rentc=cut(RENT, breaks=quantile(RENT,prob=c( 0,0.25,0.5, 0.75, 1)),include.lowest=T))%>% 
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

RentCat2 %>% ggplot(aes(x = medRent, y = tfr, col = COUNTY)) + geom_point()
