
# Load libraries

library(dplyr)
library(caret)
library(class)
library(jsonlite)
library(ggplot2)
library(plotly)
library(ggthemes)
library(tidyverse)
library(e1071)

######################################
##  Read in Files
######################################

BeerFile <- read.csv("/Users/marcc/Documents/R/SMUMastersTestProject/Unit 8 Breweries Project/Beers.txt", 
                     strip.white = TRUE,
                     header = TRUE)
BreweriesFile <- read.csv("/Users/marcc/Documents/R/SMUMastersTestProject/Unit 8 Breweries Project/Breweries.txt", 
                         strip.white = TRUE,
                         header = TRUE)

StatesFile <- read.csv("/Users/marcc/Documents/R/SMUMastersTestProject/Unit 8 Breweries Project/states_lat_long.csv", 
                       strip.white = TRUE,
                       header = TRUE)

######################################
##  Create Color Variables.
######################################

BudRed = rgb(200,16,46, 200, max = 255)
BudWhite = rgb(255,255,255, max = 255)
BudBlue = rgb(19,41,75, max = 255)
BudSilver = rgb(177,179,179, max = 255)
BudBlack  = rgb(0,0,0, max = 255)

######################################
##  How many Breweries per state?
######################################

Breweries_By_State <- BreweriesFile %>% count(State)
names(Breweries_By_State)[2] = "BreweriesCount"  # Rename Column to ensure understanding.


# This plot is too messy to show, and we determined the usefull information would 
# be the top and bottom 10.

ggplot(data = Breweries_By_State, 
         mapping = aes(x = State, y = BreweriesCount, fill = BudRed)) +
  geom_bar(stat = "Identity", show.legend = FALSE, colour = BudSilver) +
  labs(title="Breweries By State", x="States",y="Count",fill="") +
  theme(panel.background = element_rect(fill = BudWhite,
                                        colour = BudBlue,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.25, linetype = "solid",
                                          colour = BudBlue),
        panel.grid.major.x = element_blank())


######################################
##  Plot Breweries by State Top 10 
#####################################

# Get the Top 10 States with the most breweries.
Top10Breweries_By_State <- top_n(Breweries_By_State, 10, Breweries_By_State$BreweriesCount)

# Order the Top 10 Breweries by BreweriesCount.
Top10BreweriesOrdered <- Top10Breweries_By_State[order(-Top10Breweries_By_State$BreweriesCount),]

# Plot the Top 10 States with the most breweries.
ggplot(data = Top10BreweriesOrdered, 
       mapping = aes(x = reorder(State, -BreweriesCount), y = BreweriesCount, fill = BudRed)) +
  geom_bar(stat = "Identity", show.legend = FALSE, colour = BudSilver) +
  geom_text(aes(label=BreweriesCount), vjust = 1.25, color = BudWhite) +
  labs(title="Breweries By State (Top 10)", x="States",y="Count",fill="") +
  theme(panel.background = element_rect(fill = BudWhite,
                                        colour = BudBlue,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.25, linetype = "solid",
                                          colour = BudBlue),
        panel.grid.major.x = element_blank())

######################################
##  Plot Breweries by State Bottom 10 
#####################################

# Get the Bottom 10 States with the most breweries.
Bottom10Breweries_By_State <- top_n(Breweries_By_State, 10, -Breweries_By_State$BreweriesCount)

# Order the Bottom 10 Breweries by BreweriesCount.
Bottom10BreweriesOrdered <- Bottom10Breweries_By_State[order(-Bottom10Breweries_By_State$BreweriesCount),]

# Plot the Bottom 10 States with the most breweries.

ggplot(data = Bottom10BreweriesOrdered, 
                          mapping = aes(x = reorder(State, -BreweriesCount), y = BreweriesCount, fill = BudRed)) +
  geom_bar(stat = "Identity", show.legend = FALSE, colour = BudSilver) +
  geom_text(aes(label=BreweriesCount), vjust = 1.25, color = BudWhite) +
  labs(title="Breweries By State (Bottom 10)", x="States",y="Count",fill="") +
  theme(panel.background = element_rect(fill = BudWhite,
                                        colour = BudBlue,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.25, linetype = "solid",
                                          colour = BudBlue),
        panel.grid.major.x = element_blank())

######################################
##  Plot Breweries By State on a Map
#####################################

names(StatesFile)[1] = "State"  # Rename Column to be able to merge the two data sets.
st <- merge(StatesFile, Breweries_By_State, by=c("State")) # Merge Data Frames by State.


g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = BudSilver, #toRGB("gray95"),
  showlakes = TRUE,
  lakecolor = BudBlue,  #toRGB("BudBlue"),
  subunitcolor = BudRed, #toRGB("Red"),
  countrycolor= toRGB("gray85"),
  contrywidth = 0.5,
  subunitwidth = 0.5
)

plot_geo(st, locationmode = 'USA-states') %>%
  add_markers(x=st$Long, y=st$Lat, size = st$BreweriesCount, 
              color = st$BreweriesCount, colors = "Reds", hoverinfo = "text",
              text = paste0(
                "State : ", st$StateName, "<br />",
                "Population = ", format(st$Over21Pop, big.mark = ","), "<br />",
                "Breweries Count : ", st$BreweriesCount, "<br />")) %>% 
  layout(title = "Breweries By State", geo = g)


######################################
##  Create a single Data Frame (Merge the two data frames)
######################################

head(BeerFile)
head(BreweriesFile)

# Change the BreweriesFile "Brew_ID" column header to "Brewery_ID" to merge.
names(BeerFile)[1] <- "BeerName"
names(BeerFile)[5] <- "Brewery_ID"
names(BreweriesFile)[1] <- "Brewery_ID"
names(BreweriesFile)[2] <- "BreweryName"

BeersData <- merge(BreweriesFile,BeerFile, by=c("Brewery_ID"))

head(BeersData)
tail(BeersData)



######################################
##  Address missing values in the ABV, IBU, and Style columns.
##  Tried to use the below Brewrey DB to address missing values.
##  Could not get the data matched up correctly to fill in missing values.
######################################
{
# KEY
beers_key = "?key=2a09d01c9778bccb6ec25596aa5a8fb9"
# Endpoint to Query
endpoint = "beers/"
# URL
base_url = "https://sandbox-api.brewerydb.com/v2/"

final_url = paste0(base_url, endpoint, beers_key)
# Query the URL
beer_data <- fromJSON(final_url)

# Gets first 50 rows into a data frame
beer_df <- beer_data$data

# Pull out the Style Data Frame
beer_style <- beer_df$style

# Create a Beer Data Frame from the JSON Data.
BeerDB = data.frame(Beer_ID = beer_df$id, BeerName = beer_df$nameDisplay, ABV = beer_df$abv, 
           
      IBU = beer_df$ibu, Style = beer_style$shortName, 
                 stringsAsFactors = FALSE)


for(i in 1:beer_data$numberOfPages)
{
  # Gets next page of data.
  beer_data <- fromJSON(paste0(final_url, "&page=", i))
  message("Retrieving page ", i)
  
  # Pull out needed data.
  beer_df <- beer_data$data
  beer_style <- beer_df$style
  
  BeerDB = rbind(BeerDB, data.frame(Beer_ID = beer_df$id, BeerName = beer_df$nameDisplay, ABV = beer_df$abv, 
                                    IBU = beer_df$ibu, Style = beer_style$shortName, 
                                    stringsAsFactors = FALSE))
}


BeerDB
}


######################################
##  Calculate the statistics of Alcohol By Volume (ABV) & International Bitterness Units (IBU).
######################################

BeersByState <- group_by(BeersData, State)

BeerStatsByState <- summarise(BeersByState,
                       abv_mean = mean(ABV, na.rm = TRUE),
                       abv_sd = sd(ABV, na.rm = TRUE),
                       abv_med = median(ABV, na.rm = TRUE),
                       abv_min = min(ABV, na.rm = TRUE),
                       abv_max = max(ABV, na.rm = TRUE),
                       ibu_mean = mean(IBU, na.rm = TRUE),
                       ibu_sd = sd(IBU, na.rm = TRUE),
                       ibu_med = median(IBU, na.rm = TRUE),
                       ibu_min = min(IBU, na.rm = TRUE),
                       ibu_max = max(IBU, na.rm = TRUE),
                       rec_cnt = n())
BeerStatsByState

ABVStats <- summarise(BeersData,
                      mean_abv = mean(ABV, na.rm = TRUE),
                      med_abv = median(ABV, na.rm = TRUE),
                      max_abv = max(ABV, na.rm = TRUE),
                      min_abv = min(ABV, na.rm = TRUE),
                      range_abv = max(ABV, na.rm = TRUE) - min(ABV, na.rm = TRUE))
ABVStats

######################################
##  Median ABV and IBU by State in Bar Graph
#####################################

barp <- BeerStatsByState %>% plot_ly() %>%
  add_trace(x = BeerStatsByState$State, y = BeerStatsByState$abv_med * 1000, type = 'bar',
            name = "ABV",
            text = paste0("Median ABV: ",round(BeerStatsByState$abv_med * 100,2),"%"),
            textposition = 'auto',
            marker = list(color = BudRed, line = list(color = BudSilver, width = 1.5))) %>%
  add_trace(x = BeerStatsByState$State, y = BeerStatsByState$ibu_med, type = 'bar',
            name = "IBU",
            text = paste0("Median IBU: ",BeerStatsByState$ibu_med),
            textposition = 'auto',
            marker = list(color = BudBlue, line = list(color = BudSilver, width = 1.5))) %>%
  layout(title = "Median ABV (Adjusted) and IBU by State",
         barmode = 'group')
barp

######################################
##  State with the highest ABV Beer 
#####################################

Top10ABV <- top_n(BeerStatsByState, 10, BeerStatsByState$abv_max)
Top10ABVOrdered <- Top10ABV[order(-Top10ABV$abv_max),]

ggplot(data = Top10ABVOrdered, 
       mapping = aes(x = reorder(State, -abv_max), y = abv_max, fill = BudRed)) +
  geom_bar(stat = "Identity", show.legend = FALSE, colour = BudSilver) +
  geom_text(aes(label=paste0(round(abv_max*100,2),"%")), vjust = 1.25, color = BudWhite, size = 3) +
  labs(title="Higest Alcohol by Volume by State", x="States",y="ABV",fill="") +
  scale_y_continuous(labels = scales::percent_format(accurac = 1)) +
  theme(panel.background = element_rect(fill = BudWhite,
                                        colour = BudBlue,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.25, linetype = "solid",
                                          colour = BudBlue),
        panel.grid.major.x = element_blank())


######################################
##  State with the most bitter Beer.
#####################################

Top10IBU <- top_n(BeerStatsByState, 10, BeerStatsByState$ibu_max)
Top10IBUOrdered <- Top10IBU[order(-Top10IBU$ibu_max),]

ggplot(data = Top10IBUOrdered, 
       mapping = aes(x = reorder(State, -ibu_max), y = ibu_max, fill = BudRed)) +
  geom_bar(stat = "Identity", show.legend = FALSE, colour = BudSilver) +
  geom_text(aes(label=ibu_max), vjust = 1.25, color = BudWhite) +
  labs(title="Most Bitter Beers by State", x="States",y="IBU",fill="") +
  theme(panel.background = element_rect(fill = BudWhite,
                                        colour = BudBlue,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.25, linetype = "solid",
                                          colour = BudBlue),
        panel.grid.major.x = element_blank())

######################################
##  Distribution of ABV
#####################################
# Histogram of ABV
h1 <- ggplot(BeersData, aes(x = ABV)) +
  geom_histogram(color = BudRed, fill = BudSilver, binwidth = 0.01) +
  labs(title = "Histogram", x = "ABV", y = "Count") +
  scale_x_continuous(labels = scales::percent_format(accurac = 1))
h1


bp <- ggplot(BeersData, aes(y = ABV)) +
  geom_boxplot(color = BudRed, fill = BudSilver) +
  labs(title="Alcohol by Volume Distrabution Statistics", x="",y="ABV",fill="") +
  geom_text(data = ABVStats, aes(x = 0, y = ABVStats$mean_abv, 
                                 label = paste0("Mean: ",round(ABVStats$mean_abv*100,2),"%"))) +
  geom_text(data = ABVStats, aes(x = 0, y = ABVStats$med_abv,
                                 label = paste0("Median: ",round(ABVStats$med_abv*100,2),"%")), vjust = 1.25) +
  geom_text(data = ABVStats, aes(x = 0, y = ABVStats$min_abv,
                                 label = paste0("Min: ",round(ABVStats$min_abv*100,2),"%")), vjust = 1.5) +
  geom_text(data = ABVStats, aes(x = 0, y = ABVStats$max_abv,
                                 label = paste0("Max: ",round(ABVStats$max_abv*100,2),"%")), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(accurac = 1.1)) +
  theme(panel.background = element_rect(fill = BudWhite,
                                        colour = BudBlue,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.25, linetype = "solid",
                                          colour = BudBlack),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
bp

######################################
##  Plot USA 
#####################################

st_stats <- merge(st, BeerStatsByState, by=c("State")) # Merge Data Frames by State.

g2 <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = BudSilver, #toRGB("gray95"),
  showlakes = TRUE,
  lakecolor = BudBlue, #toRGB("blue"),
  subunitcolor = BudRed, #toRGB("Red"),
  countrycolor= BudSilver, #toRGB("gray85"),
  contrywidth = 0.5,
  subunitwidth = 0.5
)

plot_geo(st_stats, locationmode = 'USA-states') %>%
  add_markers(x=st_stats$Long, y=st_stats$Lat, size = st_stats$ibu_mean,
              color = st_stats$abv_mean, colors = "Reds", hoverinfo = "text",
              text = paste0(
                "State : ", st_stats$State, "<br />",
                "Population = ", format(st_stats$Over21Pop,big.mark = ","), "<br />",
                "Breweries Count : ", st_stats$BreweriesCount, "<br />",
                "Mean ABV : ", sprintf("%.2f %%",100*st_stats$abv_mean), "<br />",
                "Mean IBU : ", round(st_stats$ibu_mean,2))) %>%
  layout(title = "Mean Statistics by State (Marker Size = IBU, Color = ABV)",
         geo = g2)


######################################
##  Relationship between ABV and IBU
#####################################

ggplot(BeersData, mapping = aes(x = BeersData$ABV, y = BeersData$IBU)) + 
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Relationship between Alcohol Content & Bitterness", x = "Alcohol Content", y = "Bitterness") +
  scale_x_continuous(labels = scales::percent_format(accurac = 1))
  
######################################
##  Analysis of "Ales" specifically "India Pale Ales" vs all other Ales.
#####################################

# Carve out IPA's into a new data set.
IPAs <- BeersData %>% filter(str_detect(BeersData$Style, "[:alpha:]*IPA[:alpha:]*"))

# Carve out Ale's into a new data set.
ALEs_temp <- BeersData %>% filter(str_detect(BeersData$Style, "[:alpha:]*Ale[:alpha:]*"))

# Finally make sure all IPA's are removed from the Ale's data set.
ALEs <- ALEs_temp %>% filter(str_detect(ALEs_temp$Style, "[^IPA]"))

# Add a factor variable for determining variable.
IPAs$Type = as.factor("IPA")
ALEs$Type = as.factor("ALE")

# Combine back into a single Data set.
AllAles = merge.data.frame(IPAs, ALEs, all = TRUE)

# Remove all rows with blanks for IBU or ABV.

AllAlesClean <- filter(AllAles, !is.na(AllAles$ABV) & !is.na(AllAles$IBU))

# Spit Data set into a training Data set and a testing dataset. @ 70/30
sp = 0.70  # Split percentage

TrainingRows = sample(1:dim(AllAlesClean)[1],round(sp * dim(AllAlesClean)[1])) # Calculate Training Rows
ales_train = AllAlesClean[TrainingRows,]  # Split into 2 seperate data frames. Include Training Rows
ales_test = AllAlesClean[-TrainingRows,]  # Exclude Training Rows (Testing Rows)

# KNN Test
ales_test$TypeClalc =  knn(ales_train[,c(7,8)], ales_test[,c(7,8)],
                           ales_train$Type, k=5, prob = TRUE)

# classifications

classifications = knn(ales_train[,c(7,8)], ales_test[,c(7,8)],
                      ales_train$Type, k=5, prob = TRUE)


table(ales_test$Type, classifications)
cm = confusionMatrix(table(ales_test$Type, classifications))

AccValue = ((cm$table[1,1] + cm$table[2,2])) / ((cm$table[1,1] + cm$table[1,2]) + 
                                                  (cm$table[2,1] + cm$table[2,2]))

MisClassValue = ((cm$table[1,2] + cm$table[2,1])) / ((cm$table[1,1] + cm$table[1,2]) + 
                                                       (cm$table[2,1] + cm$table[2,2]))

SensitivityValue = cm$table[1,1] / (cm$table[1,1] + cm$table[2,1])
SpecifictityValue = cm$table[2,2] / (cm$table[1,2] + cm$table[2,2])

cm
AccValue
MisClassValue
SensitivityValue
SpecifictityValue


###################
## Breweries per 1 Million Capa
###################

st$Breweries_per_Mill = (st$BreweriesCount / (st$Over21Pop/1000000))

Top10 <- top_n(st, 10, st$Breweries_per_Mill)  # Gets the Top 10 Breweries per Capa

Bottom10 <- top_n(st, -10, st$Breweries_per_Mill) # Gets the bottom 10 Breweries per Capa

ggplot(Top10, mapping = aes(x = reorder(State,-Breweries_per_Mill), y = Breweries_per_Mill, fill = BudRed)) +
  geom_bar(stat = "Identity", show.legend = FALSE, colour = BudSilver) +
  geom_text(aes(label=round(Breweries_per_Mill,2)), vjust = 1.25, color = BudWhite) +
  labs(title="Top 10 Breweries Per 1 Mill Capa", x="States",y="Breweries Per Million Capa",fill="") +
  
  theme(panel.background = element_rect(fill = BudWhite,
                                        colour = BudBlue,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.25, linetype = "solid",
                                          colour = BudBlue),
        panel.grid.major.x = element_blank())


ggplot(Bottom10, mapping = aes(x = reorder(State,-Breweries_per_Mill), y = Breweries_per_Mill, fill = BudRed)) +
  geom_bar(stat = "Identity", show.legend = FALSE, colour = BudSilver) +
  geom_text(aes(label=round(Breweries_per_Mill,2)), vjust = 1.25, color = BudWhite) +
  labs(title="Bottom 10 Breweries Per 1 Mill Capa", x="States",y="Breweries Per Million Capa",fill="") +
  
  theme(panel.background = element_rect(fill = BudWhite,
                                        colour = BudBlue,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.25, linetype = "solid",
                                          colour = BudBlue),
        panel.grid.major.x = element_blank())
