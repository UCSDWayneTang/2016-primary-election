library(tidyverse)

Clean_data <- read_csv('./data/GaofengCleaning.csv')

all_state_wnh <- Clean_data %>%
  group_by(state_abbreviation) %>%
  summarise(white_nohisp = sum(white_nohisp*pop14)/sum(pop14)) %>%
  ggplot(aes(x=state_abbreviation, y = white_nohisp)) +
  geom_histogram(stat = "identity")

East_Coast <- c('FL',
                'GA',
                'SC',
                'NC',
                'VA',
                'MD',
                'DE',
                'NJ',
                'NY',
                'CT',
                'RI',
                'MA',
                'NH',
                'ME')

Age18 <- Clean_data %>%
  mutate(iage18 = cut(age18,breaks = c(0,5,10,15,20,25,30,35,40,45)), Coast = ifelse(pop14 > mean(pop14),'urban', 'rural')) %>%
  ggplot(aes(x=iage18,fill=as.factor(Coast))) + geom_bar(position = 'dodge')
  




# No_farm -- for democract
Clean_data %>%
  filter(Private_nonfarm_establishments_per_person >= 0.04) %>%
  mutate(no_farm= cut(Private_nonfarm_establishments_per_person, breaks=c(0,0.02,0.04,0.06,0.08,10))) %>%
  ggplot(aes(x=no_farm,fill=as.factor(partywin))) + geom_bar(position='dodge') + xlab('Percent non_farm_establishments_per_person') + ylab('Number of county win')

# White
Clean_data %>%
  mutate(White = cut(white, breaks=c(0,5,10,15,20,100))) %>%
  ggplot(aes(x=White,fill=as.factor(partywin))) + geom_bar(position='dodge') + xlab('percentage of white people') + ylab('Number of county win')

Clean_data %>%
  mutate(afAm = cut(AfAm, breaks=c(0,5,10,15,20,100))) %>%
  ggplot(aes(x=afAm,fill=as.factor(partywin))) + geom_bar(position='dodge') + xlab('percentage of AfAm people') + ylab('Number of county win')



# age 18
Clean_data %>%
  mutate(age18 = cut(age18, breaks=c(10,15,20,21,22,23,24,25,26,27,28,29,30,35,40,50))) %>%
  ggplot(aes(x=age18,fill=as.factor(partywin))) + geom_bar(position='dodge') + xlab('percentage of people under 18') + ylab('Number of county win')

# age 5
Clean_data %>%
  mutate(age5 = cut(age5, breaks=c(10,15,20,21,22,23,24,25,26,27,28,29,30,35,40,50))) %>%
  ggplot(aes(x=age5,fill=as.factor(partywin))) + geom_bar(position='dodge') + xlab('percentage of people under 18') + ylab('Number of county win')



# income house
Clean_data %>%
  mutate(income_house = cut(income_house, breaks=c(0,30000,60000,90000,120000,200000))) %>%
  ggplot(aes(x=income_house, fill=as.factor(partywin))) + geom_bar(position='dodge') + xlab('income per house') + ylab('Number of county win')



# Check
AL <- Clean_data %>%
  filter(state_abbreviation == 'AL')
sum(AL$white_nohisp*AL$pop14)/sum(AL$pop14)


# Clean

