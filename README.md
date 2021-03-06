# Population Health and Economic Consequences of Weather Events in the U.S. from 1996 - 2011
Lawrence A. Tomaziefski  


#####***Synopsis***
The purpose of this study is two fold.  First determine what type of weather events are most harmful to population health.  Secondly, determine what type of events have the greatest economic consequences.  To explore these questions, historical data was obtained through the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. Since 1950, the NOAA has been classifying storms, and providing estimates on injuries, fatalities, crop damage, and property damage.  Due to lack of standardization and automation, records before 1996 in the NOAA storm database tend to be incomplete.  Therefore, the analysis presented focuses on the years 1996 to 2011.  The analysis is presented up front and shows that the population health and economic impact of storms differs by region of the U.S..  Steps on how the data was processed is in the second section of this report. The intent of this report is to be a self contained product, all R-Code used to produce this report is included.  

***  

#####***Section 1: Analysis and Results***
Weather has a profound effect on everyone's everyday life.  In most cases, the weather can dictate what one chooses to do or don't do based on the risk presented by a the weather outside.  But in extreme cases, weather events can have a devastating impact on the overall health and economic viability of a population.  Between 1996 and 2011, the U.S. National Oceanic and Atmospheric Administration (NOAA) estimates that weather events have caused 65,884 casualties and nearly $400 billion property and crop damages.  This report looks at those population health and economic impacts of weather events by region of the U.S..  

Regions of the U.S. are as follows, the states associated with each region is listed in Appendix 1 of this report.      
    *  Northeast  
    *  South  
    *  North Central  
    *  West  

Since 1996, the NOAA has classified weather events into 48 categories.  Based on similarity of weather events in those 48 categories, this report aggregates the NOAA's categories into  8 broader classifications.  The following list is a description of the types of events in each aggregated category.  
*  Winter Weather: blizzard, snow storms, and ice storms.  
*  Severe Storms: thunder storms, tornadoes, or storms that involve high winds.  
*  Flooding Events: rain events that cause flash flooding or flooding in general, tsunamis and landslides are included in this category.  
*  Extreme Temperatures: extremely hot or cold weather, an example is a drought.  
*  Maritime Events: weather events associated with large bodies of water.  
*  Wildfires: fires caused by natural events.  
*  Particulates: fog and dust.  
*  Tropical Storms: weather events associated with tropical depressions, and hurricanes.  

***  
**Weather Event Impact on Population Health**
The NOAA storm data estimates fatalities and injuries per weather event. Although, the vast majority of weather events do not cause the loss of life or injury, there are instances when this does occur. This report compares compares the number of causalities (fatalities + injuries) by region. Figure 1 below demonstrates that for most of the U.S., severe storms are the most causality producing weather events per capita. The western region of the U.S. stands out here due to the more uniform distribution of events that cause casualties. However over the fifteen years that this data accounts for, the Southern U.S. sustained more casualties that the other three regions combined.  

![Figure 1, comparison of weather impact on population health by weather event and region](README_files/figure-html/population1-1.png)
  


Region           Casualties
--------------  -----------
Northeast              5273
West                   8078
North Central         16138
South                 35064

***    
  
**Economic Impact of Weather Events**
The NOAA storm data estimates delineates economic damage by with two measures, property damage and crop damage.  Nearly 80% of the property damage caused by weather events are concentrated in the Western and Southern regions of the U.S..  In the South, tropical storms had the largest impact on property.  Whereas in the West, property damage is driven by a multitude of weather events (see figure 2).  The vast majority of the crops destroyed by weather events in the South was due to tropical storms, and in the West extreme temperatures drives weather related crop damage.  


Region           Property Damage ($ billions)   Crop Damage ($ billions)
--------------  -----------------------------  -------------------------
Northeast                            13.34484                  0.8205215
South                               180.64221                 18.5481199
North Central                        34.98289                 10.1559719
West                                134.21632                  3.1776096
  
***  
  
![Figure 2, comparison of property damage by weather event and region](README_files/figure-html/economic1-1.png)
***

![Figure 3, comparison of crop damage by weather event and region](README_files/figure-html/economic3-1.png)
***  

#####***Section 2: Data Processing***
**System Information**  
The information of the system used to produce the analysis.  


```
##                _                           
## platform       x86_64-apple-darwin13.4.0   
## arch           x86_64                      
## os             darwin13.4.0                
## system         x86_64, darwin13.4.0        
## status                                     
## major          3                           
## minor          3.2                         
## year           2016                        
## month          10                          
## day            31                          
## svn rev        71607                       
## language       R                           
## version.string R version 3.3.2 (2016-10-31)
## nickname       Sincere Pumpkin Patch
```


**Getting and Loading the Data**  
The following code describes where the NOAA data was obtained and how it was loaded into R for further analysis.  In order to reproduce the results of this analysis, the researcher must set their own working directory as appropriate.  Additionally, the required packages and libraries included in the flowing code are essential to the analysis.  The researcher must have access to the CRAN directory to ensure the packages are installed properly.  


```r
#Population Health and Economic Consequences of Weather Events in the U.S. from 1996 - 2011
#storm.R
#by Lawrence Tomaziefski
#2016-12-18
#_______________________________________________________________________________
#Script Begins
#-------------------------------------------------------------------------------------
#Getting the Data

#Clear workspace of prior objects to free memory.
rm(list = ls())

#Set working directory
setwd('/Users/lawrence_tomaziefski/GitHub/Reproducible_Research')
path ='/Users/lawrence_tomaziefski/GitHub/Reproducible_Research/'

#Function to install and load libraries that are not already installed or loaded
#using very cool approach found here https://gist.github.com/stevenworthington/3178163
ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}
print(paste("started at :", Sys.time()))

print("loading libraries.")

#Create vector of libraries and pass into the above function.
libraries <- c("tibble","data.table","dtplyr","readr","acs",
               "lubridate","ggplot2","RColorBrewer","gridExtra",
               "devtools","ggthemes", "tidyr","knitr","R.utils","stringr",
               "reshape2","data.table","XLConnect","xlsx","maps","choroplethr",
               "choroplethrMaps","noncensus","dplyr","openxlsx")
ipak(libraries)

#Check for data folder.  Create one if none exists
if (!file.exists("./data")) { dir.create("./data")}
```


```r
#Get the  "NOAA data" from the course website:
url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

#Create a sourcefile variable for reference when downloading and unzipping
sourceFile <- "./data/storm_data.zip"

#Check if file has already been downloaded.  If it does not exist, then download it.
if (!file.exists(sourceFile)) {
        download.file(url, destfile = "./data/storm_data.zip", method = "libcurl")
}
#There are storm data is provided in two formats, one is a .csv, and the other is a .bz2.  
#The .bz2 is basically a compressed .csv file and will load faster.  
bunzip2(sourceFile, "StormData.csv.bz2",remove = FALSE, skip = TRUE)

#Read the loaded data into a dataframe which will serve as the base for this analysis.  
storm_data = read.csv("StormData.csv.bz2",header = TRUE, stringsAsFactors = FALSE) 
```


The data frame "storm_data" contains 902297 observations of 37 variables.  For the purposes of this analysis, the data set was reduced to variables that support the determination of which types of weather events are most harmful to population health, and have the greatest economic consequences.  Much of the administrative information included in the original data set that did not support analysis into health and economic outcomes was not kept.  The following section describes how the data was cleaned in preparation for follow-on analysis.  

**Cleaning the Data**  
Although the NOAA storm data base contains records dating back to 1950, the NOAA did not begin recording 48 event types in accordance with NWS Directive 10-1065 issued in 1996.  Data from earlier than 1996, was collected on certain weather events (https://www.ncdc.noaa.gov/storm events/details.jsp).  Over reporting of certain weather types will skew the analysis.  In light of this, the analysis is focused to the years after NWS Directive 10-1065 was implemented.  Note that this report will not take inflation into account. 


```r
storm_data_sub = storm_data %>%
        select(37,1,2,-(5:7),8,21:28)  #Removes most of the administrative data
        
storm_data_date = colsplit(storm_data_sub$BGN_DATE," ",c("date","time")) #Extract the date from the BGN_DATE Column
storm_data_date = storm_data_date %>%
        mutate(date = mdy(date)) %>%
        select(date)

storm_data_sub = bind_cols(storm_data_sub, storm_data_date)

storm_data_sub = storm_data_sub %>%
        mutate(year = year(date)) %>%
        filter(year >= 1996) %>%
        select(1,2,year,4:12)
  
#Rename the column names to be more intuitive and user friendly. 
colnames(storm_data_sub) = c("REFNUM","fips","year","event_type","fscale","magnitude","fatalities",
                             "injuries","property_damage","property_damage_exp","crop_damage","crop_damage_exp")
```
        

The NOAA data set uses exponential multipliers for crop and property damage estimates.  For the most part they are fairly self explanatory, but if more information is desired refer to the reference page at the following site https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html. A total casualty count per event was calculated simply by adding number of injuries to the number of fatalities. Similarly, the total economic cost by event was calculated by adding crop and property damage estimates.  


```r
property_damage_exps = unique(storm_data_sub$property_damage_exp)
prop_multiplier = data.frame(property_damage_exp= c("B","M", "K", 0,""), prop_multiplier = c(1000000000,1000000,1000,10,0))
storm_data_sub = left_join(storm_data_sub,prop_multiplier, by = "property_damage_exp")
crop_multiplier = data.frame(crop_damage_exp= c("B","M", "K", 0,""), crop_multiplier = c(1000000000,1000000,1000,10,0))
storm_data_sub = left_join(storm_data_sub, crop_multiplier, by = "crop_damage_exp")

storm_data_estimates = storm_data_sub %>%
        mutate(property_damage = property_damage * prop_multiplier) %>%
        mutate(crop_damage = crop_damage * crop_multiplier) %>%
        mutate(total_casualties = fatalities + injuries) %>% 
        mutate(total_economic_cost = crop_damage + property_damage) %>%
        select(1:8, total_casualties, 9, 11, 16)
```


Storm event types were grouped into categories in which they best fit.  The following Wikipedia page found at https://en.Wikipedia.org/wiki/Severe_weather_terminology_(United_States) was useful in determining categories.  


```r
event_type_table = data.table(event_type = unique(storm_data_estimates$event_type))
event_type = unique(event_type_table$event_type)
maritime_event = unique(grep("astromomical|surf|marine|rip|seiche|tide|waterspout|surge", event_type, ignore.case = TRUE, value = TRUE))
particulates = unique(grep("fog|smoke|dust|volcanic", event_type, ignore.case = TRUE, value = TRUE))
fire = unique(grep("fire", event_type, ignore.case = TRUE, value = TRUE))
flooding = unique(grep("flood|flow|rain|tsunami|slide", event_type, ignore.case = TRUE, value = TRUE))
severe_storm = unique(grep("lightning|thunderstorm|tornado|tstm|funnel|strong|high wind", event_type, ignore.case = TRUE, value = TRUE))
winter = unique(grep("avalanche|blizzard|freeze|hail|snow|sleet|winter|ice", event_type, ignore.case = TRUE, value = TRUE))
temperature =  unique(grep("chill|drought|heat", event_type, ignore.case = TRUE, value = TRUE))
tropical = unique(grep("tropical|hurricane", event_type, ignore.case = TRUE, value = TRUE))
storm_class = data.frame(event_type = c(maritime_event, particulates, fire, flooding, severe_storm, winter, temperature, tropical),
                         classification = c(
                        c(rep("Maritime Events", length(maritime_event))),
                        c(rep("Particulates", length(particulates))),
                        c(rep("Wildfires", length(fire))), 
                        c(rep("Flooding Events", length(flooding))),  
                        c(rep("Severe Storms", length(severe_storm))),
                        c(rep("Winter Weather", length(winter))),
                        c(rep("Extereme Temperatures", length(temperature))),
                        c(rep("Tropical Storms", length(tropical)))))

storm_class = storm_class %>%
        filter(!event_type %in% c(grep("marine", storm_class$event_type, ignore.case = T, value = TRUE)) | !classification == "Severe Storms") %>%
        filter(!event_type %in% c(grep("marine", storm_class$event_type, ignore.case = T, value = TRUE)) | !classification == "Winter Weather")
#Join the categories to the data set that has the damage estimates calculated in the previous section.
storm_data_estimates_class = left_join(storm_data_estimates, storm_class, by = "event_type")
```


To ensure that the states are accounted for properly, the fips viewfinder was referenced against the 2010 fips codes found at the 2010 U.S. census website.  An .xls file containing the fips codes is available for download using this link http://www.census.gov/2010census/xls/fips_codes_website.xls.


```r
fips_url = "http://www.census.gov/2010census/xls/fips_codes_website.xls"
download.file(fips_url,destfile = "./data/states.xls",method="curl")
list.files("./data")
states_fips = read.xlsx2("./data/states.xls",sheetIndex=1,colIndex = 2:1, header = TRUE, stringsAsFactors = FALSE)
colnames(states_fips) = c("state","fips")
states_fips$fips = as.numeric(states_fips$fips)
states_fips = states_fips %>%
        distinct() %>%
        select(2,1)
storm_clean = left_join(storm_data_estimates_class, states_fips, by = "fips")

#Join the states to the data set that has the damage estimates calculated and storms classified in the previous section.
storm_clean = storm_clean %>%
        select(1,state, 3:4, 13:14, 5:12) %>%
        filter(!is.na(classification))
```


The final cleaned and compiled data set should have 648785 observations of 13 variables. 

**Making Charts**  
This section of code demonstrates how the choropleths and graphs used for analysis were created.  


```r
####choropleths
library(datasets)
library(dplyr)
data(states)
data(state)

###download and read in planted area data by state, 2010 data from the USDA website
acre_url = "https://www.fsa.usda.gov/Assets/USDA-FSA-Public/usdafiles/NewsRoom/eFOIA/crop-acre-data/zips/2010-crop-acre-data/2010_fsa_acres_detail_final_5.zip"
download.file(acre_url,destfile = "./data/2010_fsa_acres_detail_final_5.zip",method="curl")
list.files("./data")
unzip("./data/2010_fsa_acres_detail_final_5.zip")
acre_data = read.xlsx("2010_fsa_acres_detail_final_5.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

acre_data = acre_data %>%
        filter(State.Abbrev %in% state.abb) %>%
        group_by(State.Abbrev) %>%
        summarize(planted_acres = sum(Planted.Acres))
```


```r
library(datasets)
library(dplyr)
states = states %>%
        filter (state %in% state.abb) %>%
        select(1,6:7) %>%
        mutate(area = as.numeric(area)) %>%
        mutate(population = as.numeric(population)/1000000)

#Use the states dataset built into R to develop a state reference data frame, and join with the clean data set.  
state_reference = data.frame(state = state.abb, 
                             region = tolower(state.name),
                             state_region = state.region,
                             population = states$population,
                             area = states$area,
                             planted_acres = acre_data$planted_acres)

storm_clean_states = left_join(storm_clean, state_reference, by = "state") %>% select(-area,-population,-planted_acres)

library(dplyr)
casualties = storm_clean_states %>%
        group_by(region) %>%
        summarize(value = sum(total_casualties)/16) %>%
        filter(!is.na(region))

casualties = left_join(casualties, state_reference, by = "region") %>% 
        mutate(value = value/population) %>%
        select(1,2)

property_damage = storm_clean_states %>%
        group_by(region) %>%
        summarize(value = sum(property_damage)/16) %>%
        filter(!is.na(region))

property_damage  = left_join(property_damage , state_reference, by = "region") %>% 
        mutate(value = value/area) %>%
        select(1,2)

crop_damage = storm_clean_states %>%
        group_by(region) %>%
        summarize(value = sum(crop_damage)/16) %>%
        filter(!is.na(region))

crop_damage = left_join(crop_damage, state_reference, by = "region") %>% 
        mutate(value = value/planted_acres) %>%
        select(1,2)
```


```r
#Choropleths 
choro_cas = StateChoropleth$new(casualties)
choro_cas$ggplot_scale = scale_fill_brewer("Casualties per 1M People",palette= "Reds",type = "div", drop = FALSE)
choro_cas$show_labels = FALSE

choro_prop = StateChoropleth$new(property_damage)
choro_prop$ggplot_scale = scale_fill_brewer("$ per Square Mile",palette= "Reds",type = "div", drop = FALSE)
choro_prop$show_labels = FALSE

choro_crop = StateChoropleth$new(crop_damage)
choro_crop$ggplot_scale = scale_fill_brewer("$ per Planted Acre",palette= "Reds",type = "div", drop = FALSE)
choro_crop$show_labels = FALSE

#Stacked Bar Charts

storm_density_data = storm_clean_states %>%
        filter(total_casualties > 0 & total_economic_cost > 0 & !is.na(state_region)) 
colnames(storm_density_data)[5] = c("Classification")    

stack_casualties = ggplot(data = storm_density_data, aes(x = state_region, y= total_casualties, group = Classification, color = Classification, fill = Classification)) +
        geom_col(position = "fill") +
        scale_fill_stata() +
        scale_color_stata() +
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = "black", 2),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black"),
              plot.caption = element_text(hjust = 0.5)) +
                labs(title = "% of Casualties Caused by Weather Events",
                     x = "U.S. Region")        

stack_property = ggplot(data = storm_density_data, aes(x = state_region, y= property_damage, group = Classification, color = Classification, fill = Classification)) +
        geom_col(position = "fill") +
        scale_fill_stata() +
        scale_color_stata() +
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = "black", 2),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black"),
              plot.caption = element_text(hjust = 0.5)) +
        labs(title = "% of Property Damage Caused by Weather Events",
             x = "U.S. Region")        

stack_crop = ggplot(data = storm_density_data, aes(x = state_region, y= crop_damage, group = Classification, color = Classification, fill = Classification)) +
        geom_col(position = "fill") +
        scale_fill_stata() +
        scale_color_stata() +
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = "black", 2),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black"),
              plot.caption = element_text(hjust = 0.5)) +
        labs(title = "% of Crop Damage Caused by Weather Events",
             x = "U.S. Region")        

#facet the chorolpeths and the stacked bar charts.

casualty_estimate = grid.arrange(choro_cas$render(),stack_casualties, ncol = 1, top = "Average Annual Casualties Caused by Weather Related Events from 1996-2011")

property_estimate = grid.arrange(choro_prop$render(),stack_property, ncol = 1, top = "Average Annual Property Damage Caused by Weather Related Events from 1996-2011")

crop_estimate = grid.arrange(choro_crop$render(), stack_crop, ncol = 1, top = "Average Annual Crop Damage Caused by Weather Related Events from 1996-2011")

#_______________________________________________________________________________
#Script Ends
#-------------------------------------------------------------------------------------
```
#####***Appendix 1 U.S. Regions***


State Abbreviation   Region        
-------------------  --------------
CT                   Northeast     
ME                   Northeast     
MA                   Northeast     
NH                   Northeast     
NJ                   Northeast     
NY                   Northeast     
PA                   Northeast     
RI                   Northeast     
VT                   Northeast     
AL                   South         
AR                   South         
DE                   South         
FL                   South         
GA                   South         
KY                   South         
LA                   South         
MD                   South         
MS                   South         
NC                   South         
OK                   South         
SC                   South         
TN                   South         
TX                   South         
VA                   South         
WV                   South         
IL                   North Central 
IN                   North Central 
IA                   North Central 
KS                   North Central 
MI                   North Central 
MN                   North Central 
MO                   North Central 
NE                   North Central 
ND                   North Central 
OH                   North Central 
SD                   North Central 
WI                   North Central 
AK                   West          
AZ                   West          
CA                   West          
CO                   West          
HI                   West          
ID                   West          
MT                   West          
NV                   West          
NM                   West          
OR                   West          
UT                   West          
WA                   West          
WY                   West          



 









