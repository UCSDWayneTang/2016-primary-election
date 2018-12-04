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


#
list <- c()
list2 <- c()
for (col in colnames(county_results)){
  list <- append(list,col)
  list2 <- append(list2,sum(county_results[col] == 0))
}
df <- data.frame(column = list,
                 nzero = list2)

write.csv(county_results, file = "county_results.csv",row.names=FALSE)
county_results$housing_multi <- ifelse(county_results$housing_multi == 0, mean(county_results$housing_multi),county_results$housing_multi)

collist = c('SBO001207',
            'AFN120207',
            'BPS030214,SBO015207',
            'WTN220207',
            'hawaii',
            'MAN450207',
            'SBO315207',
            'SBO415207',
            'SBO215207',
            'SBO115207',
            'SBO515207',
            'SBO015207',
            'BPS030214')

filllist = c('BZA115213',
             'RTN130207',
             'RTN131207',
             'popchange',
             'votes',
             'fraction_votes',
             'BZA110213',
             'asian',
             'foreign',
             'AfAm',
             'language',
             'tworaces',
             'AmInd',
             'BZA010213',
             'NES010213')



for (col in collist) {
  county_results[col] = NULL
}

for (col in filllist) {
  for( i in c(1:nrow(county_results))){
    if(county_results[i,col] == 0){
      county_results[i,col] <- sum(county_results[col])/nrow(county_results[col])
    }
  }
}


county_winner <- county_results %>%
  group_by(state_abbreviation.x, fips,party) %>%
  summarise(votes = sum(votes))

county_winner <- county_winner %>%
  group_by(fips) %>%
  summarise(votes = max(votes))

a <- county_winner %>% inner_join(Clean_data, by = 'fips')

county_winner <- county_winner %>%
  inner_join(county, by = c('county.x' = 'county'))

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


a %>%
  filter(votes/(white*pop14) >= 0.0025) %>%
  mutate(white = cut(votes/(white*pop14), breaks=c(0.0025,0.003,0.004,0.005,0.01))) %>%
  ggplot(aes(x=white,fill=as.factor(partywin))) + geom_bar(position='dodge') + xlab('percentage of white people') + ylab('Number of county win')


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


# margin benefit of no_farm_establishments_per_person
a %>%
  filter(votes/(Private_nonfarm_establishments_per_person*pop14) >= 20) %>%
  mutate(marginb_no_farm= cut(votes/(Private_nonfarm_establishments_per_person*pop14), breaks = c(20,25,30,35,40,55))) %>%
  ggplot(aes(x=marginb_no_farm,fill=as.factor(partywin))) + geom_bar(position='dodge') + xlab('Percent non_farm_establishments_per_person') + ylab('Number of county win')


# Check
AL <- Clean_data %>%
  filter(state_abbreviation == 'AL')
sum(AL$white_nohisp*AL$pop14)/sum(AL$pop14)


# Clean

