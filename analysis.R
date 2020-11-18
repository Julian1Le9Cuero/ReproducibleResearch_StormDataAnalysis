setwd("C:/Users/julia/OneDrive/Escritorio/RepData_PeerAssessment2")

## Title: Measuring the impact of weather events

## Synopsis
# The following analysis is about the consequences of severe weather events
# in the United States during the period that goes from 1950
# to 2011. The data comes from the National Weather Service (NWS) Storm Data, which consists of
# 37 variables and around 900000 observations. Accordingly, the NWS does not guarantee the accuracy
# or validity of the information that will be presented here. However, it may be useful
# to prioritize resources for different types of events.

# For additional details you can check the Storm Data Documentation (https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

## Setup
library(dplyr)
library(ggplot2)
library(scales)

## Research Questions
# As could have been suggested from above, the
# research questions are outlined in the following way:
# 1. Across the United States, which types of events
# (as indicated in the EVTYPE variable) are most
# harmful with respect to population health?
# 2. Across the United States, which types of events
# have the greatest economic consequences?

## Loading data
raw_data <- read.csv("StormData.csv", header=TRUE)

## Data Processing
# Because of the bulk of data and guided by the the research questions, only relevant variables will be
# selected. In this case, these are the chosen variables from the original dataset:
# -EVTYPE: Represents the event types, which represent the main variable of interest.
# -INJURIES: Number of injuries of an event in a specific date
# -FATALITIES: Number of fatalities of an event in a specific date
# -CROPDMG: Crop damage
# -CROPDMGEXP: Crop damage exponent
# -PROPDMG: Property damage
# -PROPDMGEXP: Property damage exponent
new_data <- raw_data[,c("EVTYPE","INJURIES", "FATALITIES", "CROPDMG", "CROPDMGEXP", "PROPDMG", "PROPDMGEXP")]

# First, the columns are renamed, so they are a bit easier to access.
names(new_data) <- tolower(names(new_data))

# Then, it's necessary to do some cleanup for three variables (EVTYPE, CROPDMGEXP and PROPDMG).
# To begin with, according to the NWS docs, in Table 1 of Section 2.1.1,  there are 48 event types:
# 1. Astronomical Low Tide
# 2. Avalanche
# 3. Blizzard
# 4. Coastal Flood
# 5. Cold/Wind Chill
# 6. Debris Flow
# 7. Dense Fog
# 8. Dense Smoke
# 9. Drought
# 10. Dust Devil
# 11. Dust Storm
# 12. Excessive Heat
# 13. Extreme Cold/Wind Chill
# 14. Flash Flood
# 15. Flood
# 16. Freezing Fog
# 17. Frost/Freeze
# 18. Funnel Cloud
# 19. Hail
# 20. Heat
# 21. Heavy Rain
# 22. Heavy Snow
# 23. High Surf
# 24. High Wind
# 25. Hurricane/Typhoon
# 26. Ice Storm
# 27. Lakeshore Flood
# 28. Lake-Effect Snow
# 29. Lightning
# 30. Marine Hail
# 31. Marine High Wind
# 32. Marine Strong Wind
# 33. Marine Thunderstorm Wind
# 34. Rip Current
# 35. Seiche
# 36. Sleet
# 37. Storm Tide
# 38. Strong Wind
# 39. Thunderstorm Wind
# 40. Tornado
# 41. Tropical Depression
# 42. Tropical Storm
# 43. Tsunami
# 44. Volcanic Ash
# 45. Waterspout
# 46. Wildfire
# 47. Winter Storm
# 48. Winter Weather

# However, when looking at the data there are 985 different event types
# because they were entered differently.
length(unique(raw_data$EVTYPE))

# Therefore, I performed some string processing. The way I classified a specific entry to a category was by the criteria that a chosen event name was the one that most accurately
# describes the meteorological event. If a specific category does not appear in the code is because I did not find any anomalies
# in the entries related to it.

data_clean <- new_data %>% mutate(evtype=case_when(
    grepl("astro", evtype, ignore.case = TRUE) ~ toupper("Astronomical Low Tide"),
    grepl("^aval", evtype, ignore.case = TRUE) ~ toupper("Avalanche"),
    grepl("^blizzard", evtype, ignore.case = TRUE) ~ toupper("Blizzard"),
    grepl("^[ ]*coastal", evtype, ignore.case = TRUE) ~ toupper("Coastal Flood"),
    grepl("Record cold|^cold", evtype, ignore.case = TRUE) ~ toupper("Cold/Wind Chill"),
    grepl("^freezing Fog", evtype, ignore.case = TRUE) ~ toupper("Freezing Fog"),
    grepl("fog", evtype, ignore.case = TRUE) ~ toupper("Dense Fog"),
    grepl("smoke", evtype, ignore.case = TRUE) ~ toupper("Dense Smoke"),
    grepl("^drought|EXCESSIVELY DRY", evtype, ignore.case = TRUE) ~ toupper("Drought"),
    grepl("devil", evtype, ignore.case = TRUE) ~ toupper("Dust Devil"),
    grepl("^dust storm", evtype, ignore.case = TRUE) ~ toupper("Dust Storm"),
    grepl("^EXCESSIVE HEAT|RECORD/EXCESSIVE HEAT", evtype, ignore.case = TRUE) ~ toupper("Excessive Heat"),
    grepl("^Extreme Cold|^prolong cold", evtype, ignore.case = TRUE) ~ toupper("Extreme Cold/Wind Chill"),
    grepl("LOCAL FLASH FLOOD|FLOOD FLASH|Flash Flood", evtype, ignore.case = TRUE) ~ toupper("Flash Flood"),
    grepl("^Flood|Breakup flooding|River flood", evtype, ignore.case = TRUE) ~ toupper("Flood"),
    grepl("^Frost|^Freeze", evtype, ignore.case = TRUE) ~ toupper("Frost/Freeze"),
    grepl("^Funnel|funnels$|funnel$", evtype, ignore.case = TRUE) ~ toupper("Funnel Cloud"),
    grepl("^hail|small hail|NON SEVERE HAIL|deep hail", evtype, ignore.case = TRUE) ~ toupper("Hail"),
    grepl("^heat|^record heat", evtype, ignore.case = TRUE) ~ toupper("Heat"),
    grepl("^Heavy rain|^FREEZING RAIN|rainfall|^rain|^HVY RAIN|TORRENTIAL RAIN", evtype, ignore.case = TRUE) ~ toupper("Heavy Rain"),
    grepl("^snow|^Heavy snow|^BLOWING SNOW|^Record May Snow|^Light snow|^MODERATE SNOW|^FALLING SNOW|^Mountain Snows|^Thundersnow|late snow", evtype, ignore.case = TRUE) ~ toupper("Heavy Snow"),
    grepl("^heavy surf|^high surf|HAZARDOUS SURF|^[ ]*HIGH SURF|^ROUGH SURF", evtype, ignore.case = TRUE) ~ toupper("High Surf"),
    grepl("^high wind|Gusty wind|GUSTY LAKE WIND", evtype, ignore.case = TRUE) ~ toupper("High Wind"),
    grepl("typhoon|^hurricane", evtype, ignore.case = TRUE) ~ toupper("Hurricane/Typhoon"),
    grepl("^Ice|GLAZE ICE|^Glaze|PATCHY ICE|^Black Ice", evtype, ignore.case = TRUE) ~ toupper("Ice Storm"),
    grepl("^LAKESHORE", evtype, ignore.case = TRUE) ~ toupper("Lakeshore Flood"),
    grepl("^LAKE FLOOD|^Lake-Effect|^HEAVY LAKE SNOW", evtype, ignore.case = TRUE) ~ toupper("Lake-Effect Snow"),
    grepl("^[ ]*Lightning", evtype, ignore.case = TRUE) ~ toupper("Lightning"),
    grepl("Marine Hail", evtype, ignore.case = TRUE) ~ toupper("Marine Hail"),
    grepl("^rip current", evtype, ignore.case = TRUE) ~ toupper("Rip Current"),
    grepl("^sleet", evtype, ignore.case = TRUE) ~ toupper("Sleet"),
    grepl("^STORM SURGE|^HIGH TIDES|^BLOW-OUT TIDE", evtype, ignore.case = TRUE) ~ toupper("Storm Tide"),
    grepl("^Strong wind", evtype, ignore.case = TRUE) ~ toupper("Strong Wind"),
    grepl("GUSTY THUNDERSTORM WIND|^Thunderstorm Wind|^THUNDERSTORM|^SEVERE THUNDERSTORM|^[ ]*TSTM", evtype, ignore.case = TRUE) ~ toupper("Thunderstorm Wind"),
    grepl("^tornado", evtype, ignore.case = TRUE) ~ toupper("Tornado"),
    grepl("^Tropical Storm", evtype, ignore.case = TRUE) ~ toupper("Tropical Storm"),
    grepl("volcanic", evtype, ignore.case = TRUE) ~ toupper("Volcanic Ash"),
    grepl("^Waterspout|^[ ]*Waterspout", evtype, ignore.case = TRUE) ~ toupper("Waterspout"),
    grepl("Wildfire", evtype, ignore.case = TRUE) ~ toupper("Wildfire"),
    grepl("^Winter storm|^Record Winter Snow", evtype, ignore.case = TRUE) ~ toupper("Winter Storm"),
    grepl("^Winter Mix|^Wintery Mix|^WINTER WEATHER", evtype, ignore.case = TRUE) ~ toupper("Winter Weather"),
    TRUE ~ evtype
))

# Although I did not reduced the 985 different event types to
# 48, I reduced it by more than half (444 specifically). It must be highlighted that I didn't do
# so because the purpose of the project is about reproducible research and data analysis and not just data preprocessing Besides that, it was
# difficult to cover all possible cases
length(unique(data_clean$evtype))

# Second, I also found necessary to clean the PROPDMGEXP and CROPDMGEXP variables. Because
# these are the exponents/multipliers for the PROPDMG or CROPDMG, which will serve as a
# measure of the economic impact of weather events.

# These are possible values of CROPDMGEXP and PROPDMGEXP:
# H,h,K,k,M,m,B,b,+,-,?,0,1,2,3,4,5,6,7,8, and blank-character
unique(raw_data$PROPDMGEXP)

# According to the National Oceanic and Atmospheric Administration's (NOAA) Storm Database, I also decided to classify those values as follows:
# - H,h = hundreds = 100
# - K,k = kilos = thousands = 1,000
# - M,m = millions = 1,000,000
# - B,b = billions = 1,000,000,000
# - numeric (0-8) = 10
# - (+) = 1
# - (-) = 0
# - (?) = 0
# - empty character ("") = 0

# Thus, this is what the replace_num function does, it replaces the character with
# its respective numeric value.

replace_num <- function(vec){
    nums <- as.character(0:8)
    for(i in 1:length(vec)){
        if(tolower(vec[i]) == "k") vec[i] <- 1000
        else if(tolower(vec[i]) == "m") vec[i] <- 1000000
        else if(vec[i] %in% nums) vec[i] <- 10
        else if(tolower(vec[i]) == "h") vec[i] <- 100
        else if(tolower(vec[i]) == "b") vec[i] <- 1000000000
        else if(vec[i] == "+") vec[i] <- 1
        else {
            vec[i] <- 0
        }
    }
    as.integer(vec)
}

# And now, to calculate economic impact I created
# two more variables: property_damage and crop_damage, both measured in billions.
# property_damage = propdmgexp * propdmg
# crop_damage = cropdmgexp * cropdmg
data_clean <- data_clean %>% mutate(propdmgexp=replace_num(propdmgexp),
                                    cropdmgexp=replace_num(cropdmgexp),
                                    crop_damage=cropdmg * cropdmgexp,
                                    property_damage=propdmg * propdmgexp)

## Data analysis
### Research question 1
# Now that the data is cleaner we can start with the analysis. In order to quantify health consequences,
# total injuries and total fatalities will serve for this purpose.
# Since we're only interested in the ones that most affected health and there's quite a lot of data
# it's helpful to filter events that contain at least 1 injury or 1 fatality
harmful <- subset(data_clean, injuries > 0 | fatalities > 0)

# As a result, total fatalities and injuries are calculated by event type and we just
# get the top 6.
most_harmful <- harmful %>% group_by(evtype) %>%
    summarise(total_injuries=sum(injuries), total_fatalities=sum(fatalities)) %>%
    arrange(desc(total_fatalities)) %>%
    top_n(6)


#### Fatalities
# In this first plot we start seeing that Tornadoes are the event that unfortunately kill people the most,
# followed by excessive heat and hurricane. Suggesting that thes may be the events
# that harm American health the most.
most_harmful %>% ggplot(aes(x=reorder(evtype, -total_fatalities),y=total_fatalities,fill=evtype)) +
    geom_bar(stat="identity") + labs(x="Event type", y="Total fatalities") +
    scale_fill_brewer(palette = "RdGy") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

#### Injuries
# Similarly, Tornadoes occupy the first place in the total number of injuries, and
# we can also find excessive heat and X, which seeems the confirm the previous findings.
most_harmful %>% ggplot(aes(x=reorder(evtype, -total_injuries),y=total_injuries,fill=evtype)) +
    geom_bar(stat="identity") + labs(x="Event type", y="Total injuries") +
    scale_fill_brewer(palette = "RdGy") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

### Research question 2
# On the other hand in the second question, I calculated total_crop_damage and
# total_prop_damage, where the first is the sum of crop_damage measured in billions
# and the second is also the sum of property_damage in billions.
worst_econ <- data_clean %>% group_by(evtype) %>%
    summarise(total_crop_damage=sum(crop_damage)/10^9, total_prop_damage=sum(property_damage)/10^9) %>%
    arrange(desc(total_prop_damage)) %>%
    top_n(5)

# In this final plot, we now find that floods top the list, then there are hurricanes or typhoons,
# which both caused property damage of around 150 billions and W respectively.
worst_econ_plot <- with(worst_econ,
                          data.frame(evtype=c(evtype,evtype),
                                     total=c(total_prop_damage,total_crop_damage),
                                     type=c(rep("Property damage",nrow(worst_econ)),
                                            rep("Crop damage",nrow(worst_econ)))))

worst_econ_plot %>% ggplot(aes(x=reorder(evtype, -total),y=total,fill=type)) +
    geom_bar(stat="identity",position = "dodge") +
    expand_limits(x = c(0, NA), y = c(0, NA)) +
    scale_y_continuous(labels = unit_format(unit = "B")) +
    labs(x="Event type",y="Total losses (in billions)") +
    scale_fill_manual(values=c("#af2d2d","#f05454")) +
    scale_x_continuous(guide = guide_axis(n.dodge = 2))


## Results
# Finally, I obtained two conclusions based on the
# research questions:
# 1. As we saw above, if measured by fatalities, the event that
# damaged American health the most in this period were the tornados, followed
# by the heat in general, excessive heat and flash flood.
# If we also see the number of injuries, we can confirm that tornados
# are the most significant threat to health as well as excessive heat, but it should
# also be regarded the thunderstorm wind
# 2. Measured by property and crop damage, the types of events that have the most negative economic impact
# across the USA are floods, hurricanes or typhoons and, once again,
# tornados.