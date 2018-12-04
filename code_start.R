#libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#read in data with NA's instead of blanks
county <- read.csv("data/county_facts.csv", na.strings = c("","NA"))
results <- read.csv("data/primary_results.csv")
dictionary <- read.csv("data/county_facts_dictionary.csv")


na_list <- c(NA, NA, 'T' ,1, 2,3)

#cleaning
#putting all totals in to a new DF
totals <-subset(county,is.na(state_abbreviation))
# removing totals from county_na DF
county <- county[!is.na(county$state_abbreviation),]

#rename some columns
county <- county %>% rename(county = area_name, 
                                  pop14 = PST045214, 
                                  pop10est = PST040210, 
                                  popchange = PST120214, 
                                  pop10 = POP010210, 
                                  age5 = AGE135214, 
                                  age18 = AGE295214, 
                                  age65 = AGE775214, 
                                  female = SEX255214, 
                                  white = RHI125214, 
                                  AfAm = RHI225214, 
                                  AmInd = RHI325214, 
                                  asian = RHI425214, 
                                  hawaii = RHI525214, 
                                  tworaces = RHI625214, 
                                  hispanic = RHI725214, 
                                  white_nohisp = RHI825214, 
                                  samehouse = POP715213, 
                                  foreign = POP645213,
                                  language = POP815213, 
                                  education1 = EDU635213, 
                                  education2 = EDU685213, 
                                  vet = VET605213, 
                                  travel = LFE305213, 
                                  housing = HSG010214, 
                                  homeowner = HSG445213, 
                                  housing_multi = HSG096213, 
                                  housing_value = HSG495213, 
                                  households = HSD410213, 
                                  perhousehold = HSD310213, 
                                  income_cap = INC910213, 
                                  income_house = INC110213, 
                                  poverty = PVY020213)

# join county and results and clean up
county_results <- results %>% inner_join(county, by = "fips")
# county_results <- county_results[, !names(county_results) %in% c("state_abbreviation.y", "county.y")] 

# for each state, what fraction of the votes for that state did each party get? (votes column tells us this)
# note: we don't have data on republican votes in CO or WY...take them out?

votes_state_party <- county_results %>%
  group_by(state_abbreviation.x, county.x, party) %>%
  summarise(votes_by_party = sum(votes))

votes_state_party %>%
  group_by(state_abbreviation.x) %>%
  mutate(sum(votes_by_party), votes = (votes_by_party/sum(votes_by_party)*100)) %>%
  ggplot(aes(x=state_abbreviation.x, y = votes, fill = party)) + 
  geom_bar(stat="identity", position=position_dodge())


#exploratory data analysis

results %>% 
  group_by(county, state_abbreviation, party) %>%
  summarise (sum(fraction_votes))

#####
##Possible Questions
#####

# Is there a correlation between race and candidate? What about race and party? Can we visalize this?

county_results %>% 
  group_by(candidate) %>%
  filter(!is.na(white), !is.na(AfAm), !is.na(AmInd), !is.na(asian), !is.na(hawaii)) %>%
  summarise(mean(white), mean(AfAm), mean(AmInd), mean(asian), mean(hawaii))

county_results %>% 
  group_by(party) %>%
  filter(!is.na(white), !is.na(AfAm), !is.na(AmInd), !is.na(asian), !is.na(hawaii)) %>%
  summarise(mean(white), mean(AfAm), mean(AmInd), mean(asian), mean(hawaii))

# Is there is a correlation between education level and candidate? What about education level and party?


# Are veterans more likely to vote republican or democrat? Can we visualize this?
# Does percent of people below poverty level have an effect on who they vote for?

########
## Options for assignment ideas: 
######## 

# First part of the project will be visualizations (a map of red counties, blue counties)
# Maybe also some smaller charts showing interesting relationships between voter characteristics and party
# Second part of the project will be predictive analytics:
# Predicting which counties will vote dem/rep based on the characteristics of that county 
# (find out who won each county (by party only) and group counties by common characteristics 
# and then try to predict who will win based on characteristics)
# Find out which counties are "close" as far as dem/rep and focus campaign efforts on those counties



