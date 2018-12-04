library(dplyr)
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)
library(DT)
library(GGally)
library(randomForest)
library(readr)
library(rgeos)
library(rgdal)
library(maptools)
library(maps)
library(RColorBrewer)
library(scales)
library(rgdal)
library(maptools)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

primary <- read.csv("primary_results.csv", stringsAsFactors = FALSE)
demographics <- read.csv("county_facts.csv", stringsAsFactors = FALSE)

stateResults <- primary %>%
  group_by(state_abbreviation, state, county, candidate) %>%
  summarise(votes=sum(votes), fips = fips) %>%
  mutate(fraction_votes=ifelse(votes/sum(votes)>0.4,0.4,votes/sum(votes)))

State_Trump <- stateResults %>%
  group_by(state_abbreviation, county, candidate) %>%
  filter(candidate == "Donald Trump")

candidate = 'Bernie Sanders'

counties <- readOGR(dsn="county_shapefiles", layer="cb_2014_us_county_500k")
counties@data$id <- rownames(counties@data)
counties.points <- fortify(counties, region="id")
counties.df <- inner_join(counties.points, counties@data, by="id")
counties.df$fips <- as.integer(paste0(counties.df$STATEFP, counties.df$COUNTYFP))

data(state.fips)

resultsMap <- data.frame()
for (stateAbbreviation in unique(stateResults$state_abbreviation)) {
  if (stateAbbreviation %in% c("AK", "HI")) {
    next
  }
  stateFips <- state.fips$fips[state.fips$abb==stateAbbreviation]
  state <- stateResults[stateResults$state_abbreviation==stateAbbreviation,]$state[[1]]
  stateCounties <- counties.df[counties.df$STATEFP==sprintf("%02d", stateFips),]
  thisStatePrimary <- stateResults[stateResults$state_abbreviation == stateAbbreviation & stateResults$candidate == candidate, c("fips", "fraction_votes")]
  stateResultsMap <- inner_join(stateCounties, thisStatePrimary, by="fips")
  if (nrow(stateResultsMap)==0) {
    stateResultsMap <- stateCounties[,]
    stateResultsMap$fips <- 0
    stateResultsMap$fraction_votes <- stateResults[stateResults$state_abbreviation==stateAbbreviation & stateResults$candidate==candidate,]$fraction_votes[[1]]
  }
  resultsMap <- rbind(resultsMap, stateResultsMap)
}

resultsMap <- resultsMap[order(resultsMap$order),]


p <- ggplot(resultsMap) + 
  aes(long,lat,group=group,fill=fraction_votes) + 
  geom_polygon() +
  geom_path(color="white", size=0.1) +
  coord_equal() +
  scale_fill_gradientn(name="Votes",
                       colours=brewer.pal(11,"RdYlBu"),
                       limits=c(0, 0.4),
                       labels=percent) + 
  theme_light(base_size=12) +
  theme(strip.text.x = element_text(size=12, colour="black"),
        strip.background = element_rect(colour="white", fill="white"),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ggtitle(paste("2016", candidate, "Primary Results"))
ggsave(paste0(tolower(gsub(" ", "_", candidate)), ".png"), p, height=6, width=12, units="in")
