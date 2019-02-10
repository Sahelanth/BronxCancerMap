####FEATURES TO ADD:
#https://twitter.com/Sahelanth/status/1051281234142355456



#Prep for previous:
#Chloropleth map
#Want to do NY cancer incidence by census tract, look for hot spots
install.packages("censusapi")
library(censusapi)
library(sf)
library(leaflet)
#Request a census API key at https://api.census.gov/data/key_signup.html ; this one's mine.
Sys.setenv(CENSUS_KEY= "745a31864da14e4b53a4c020f47ba45895e750f3")
library(rvest)
library(tidyverse)
install.packages("tigris")

###Get the shape files for New York State census tracts
library(tigris)
options(tigris_class = "sf")
ny <- tracts("NY")


###Get cancer data from NY census tracts

url <- "https://www.health.ny.gov/statistics/cancer/registry/tract/index.htm"
session <- html_session(url)
censustractlinks <- session %>% read_html() %>% 
  html_nodes(xpath='//ul[@id="countylist"]') %>% html_nodes('li a') %>%
  html_attr('href')
censustractlinks <- paste0("https://www.health.ny.gov",
                           censustractlinks)
#Fix their broken link
censustractlinks <- gsub("st.lawrence", "stlawrence", censustractlinks)

#Define get_table function
get_table <- function(url){
  cancerdata <- jump_to(session, url) %>% html_nodes('table') %>%
    html_table(fill=TRUE) %>% .[[1]]
  
  names(cancerdata)[seq(3, 13, by=2)] <- paste(names(cancerdata)[seq(3, 13, by=2)], "observed")
  names(cancerdata)[seq(4, 14, by=2)] <- paste(names(cancerdata)[seq(4, 14, by=2)], "expected")
  cancerdata$`Primary Census Tract` <- gsub(".", "", cancerdata$`Primary Census Tract`, fixed=TRUE)
  cancerdata$`Primary Census Tract`<- str_pad(cancerdata$`Primary Census Tract`, width=6, side="left", pad="0")
  cancerdata <- cancerdata[-1,]
  cancerdata[,3:14] <- apply(cancerdata[,3:14], 2, as.numeric)
  
  #Add county name to table
  cancerdata$County <- str_extract(url, 
                                   "(?<=/tract/)[a-z]+(?=\\.htm)")
  cancerdata
}

#Apply that function to all the links
cancertable <- map_dfr(censustractlinks, get_table)

#Result has 62 counties, perfect! I may need to append
#the county code for each to its tract codes.
#length(unique(ny$TRACTCE))
#[1] 2701
#length(unique(cancertable$`Primary Census Tract`))
#[1] 2651
#length(unique(cancertable$`Included Census Tracts`))
#90

#Okay, proper is combine county code and tract code
#Need to do this in both ny and in cancertable
#Do this after have padded tract codes to 6 digits,
#so should be okay to do it outside the function.

#Separate st. lawrence in cancertable$County, then convert 

cancertable$County <- gsub("stlawrence", "St. Lawrence", cancertable$County)
cancertable$County <- str_to_title(cancertable$County)

cancertable$County <- paste(cancertable$County, "County")

test <- inner_join(fips_codes[fips_codes$state=="NY",], cancertable,
                   by=c("county" = "County"))
test$county_and_tract_code <- paste(test$county_code, test$`Primary Census Tract`, sep="")

ny$county_and_tract_code <- paste(ny$COUNTYFP, ny$TRACTCE, sep="")

test2 <- inner_join(ny, test,
                    by="county_and_tract_code")

test2 <- test2 %>% mutate(Colorectal.Ratio = `Colorectal observed`/ `Colorectal expected`,
                          Lung.and.Bronchus.Ratio = `Lung and Bronchus observed`/ `Lung and Bronchus expected`,
                          Female.Breast.Ratio = `Female Breast observed`/ `Female Breast expected`,
                          Prostate.Ratio = `Prostate observed`/ `Prostate expected`,
                          Urinary.Bladder.Ratio = `Urinary Bladder (incl. in situ) observed`/ `Urinary Bladder (incl. in situ) expected`,
                          Non.Hodgkin.Lymphoma.Ratio = `Non-Hodgkin Lymphoma observed`/ `Non-Hodgkin Lymphoma expected`)


#Leaflet interactive map example

library(leaflet)
library(mapview)
library(tidyverse)
library(readxl)


#Modify Bronx cancer map

#I doubt there's a way to grab names by census tract, but "X number of people live here, including Y"
#would be a great way to bring impact home.
library(sf)

#NYC counties: Bronx, Kings, Queens, New York, Richmond

bronxdata <- test2[test2$county %in% "Bronx County",]

library(lattice)
#Make a stripplot with data for all the hospitals in grey
p <- stripplot(`Lung and Bronchus observed` ~ `Lung and Bronchus expected`, 
               data=bronxdata, col = "grey", pch = 20, cex = 5, alpha=0.2,
               xlim=c(0,50), ylim=c(0,35), jitter=TRUE)
#Make a copy of p for each hospital
p <- mget(rep("p", length(unique(bronxdata$NAMELSAD))))
#For each hospital, make a vector of 108 grey color points, 
#and change the proper 4 points red
clr <- rep("grey", length(bronxdata$NAMELSAD))
alph <- rep(0.2, length(bronxdata$NAMELSAD))
p <- lapply(1:length(p), function(i) {
  clr[i] <- "purple"
  alph[i] <- 1
  update(p[[i]], col = clr, alpha=alph)
})

pal <- colorQuantile(palette = "viridis", domain = bronxdata$Lung.and.Bronchus.Ratio, n = 10)

popgrph <- popupGraph(p)

bronxdata <- mutate(bronxdata, `Typical`=case_when(
                                      Lung.and.Bronchus.Ratio < mean(bronxdata$Lung.and.Bronchus.Ratio) - sd(bronxdata$Lung.and.Bronchus.Ratio)/2 ~ paste("Fewer cases than typical for Bronx"),
                                      Lung.and.Bronchus.Ratio > mean(bronxdata$Lung.and.Bronchus.Ratio) + sd(bronxdata$Lung.and.Bronchus.Ratio)/2 ~ paste("More cases than typical for Bronx"),
                                      TRUE ~ paste("Typical number of cases for Bronx")),
                    `Data Source`='<a href="https://www.health.ny.gov/statistics/cancer/registry/tract/index.htm">NY Dept of Health</a>')


bronxdata <- mutate(bronxdata, `Location`=NAMELSAD)
  
  "Ratio of observed to expected cases"
poptab <- popupTable(bronxdata,
                     zcol=c("Location",
                            "Typical",
                            "Data Source"),
                     row.numbers=FALSE, feature.id=FALSE)

#save(list=ls(), file="cancer_map_data.Rdata")
load("cancer_map_data.Rdata")

poptab <- map_chr(poptab, str_replace, 
                  "<head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"></head>", "")
poptab <- map_chr(poptab, str_replace, "<body><div class=\"scrollableContainer\">", "")
poptab <- map_chr(poptab, str_replace, "</div></body>", "")
poptab <- map_chr(poptab, str_replace, 'class=\"popup scrollable\" id=\"popup\"', '')

popgrph <- map(popgrph, str_replace, "<head>.+</head>", "")
popgrph <- map(popgrph, str_replace, "<body>  <div class=\"scrollableContainer\"> ", "")
popgrph <- map(popgrph, str_replace, "</div> </body>", "")
popgrph <- map(popgrph, str_replace, '<table class=\"popup scrollable\" id=\"popup\">  ', '')
popgrph <- map(popgrph, str_replace, '</table>  ', '')

tst <- map2(poptab, popgrph, ~paste('<div>', .x, '</div>', '<div>', .y, '</div>') )

m <- bronxdata %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(Lung.and.Bronchus.Ratio),
              label = htmltools::htmlEscape(unique(bronxdata$NAMELSAD)),
              popup=tst) %>%
  setView(lng=-73.8648, lat=40.8448, zoom = 13) %>% setMaxBounds(-76, 38, -70, 45)


#Table above graph (or vice versa) is workable. Side-by-side is too hard to make look good. Think about exactly what should be in the table - limited space! Dynamically recommending a device based on what the hospital is worse at than its neighbors would kick ass. May be tricky, but if generate table of recommendations per county in advance, shouldn't be too hard.

Likely table layout:
- $HOSPITAL is $BETTER/WORSE/ABOUTAVERAGE at $METRIC compared to other hospitals in $COUNTY.
- Devices that may improve $METRIC include $<link to device page>

Best way to summarize outcomes might be a mini-heatmap - red, grey, green for better vs worse than other local hospitals, rather than exact numbers. (Charge for numbers!)

#Demo version:
#Get stats for a particular disease in a particular county
#Make an interactive map where people can hover over hospitals to see their stats,
#plus a table with the stats below, that then links out to device pages relevant to outcomes

#Presenting a summary table of devices with demonstrated outcomes would be really powerful

#Simplest deploy: Rstudio viewer -> Export -> save as Web Page -> send the HTML to team.
#Later can give them a Docker container or whatever to generate all.

library(leaflet)
library(mapview)
library(tidyverse)
library(readxl)
#Yale New Haven Hospital coordinates: 41.304666, -72.935687
data <- read_xlsx("C:/Users/dexur1/Desktop/James Pitt/Pulmonary/COPD/Connecticut COPD readmissions.xlsx")
data <- tidyr::fill(data, 1:6)
data$`30 Day COPD Readmission Rate` <- as.numeric(data$`30 Day COPD Readmission Rate`)

yaledata <- data %>% filter(Hospital=="Yale-New Haven Hospital")
yaleplot <-  ggplot(data=yaledata, 
aes(x=DRG,
y=`30 Day COPD Readmission Rate`, 
fill=DRG)) +
geom_bar(stat="identity", position="dodge") +
theme(legend.position = "none")

#Check OSM's commercial use terms for details
#https://wiki.openstreetmap.org/wiki/Introduction_to_OSM_for_Business_Users
#https://help.openstreetmap.org/questions/4669/can-i-use-openstreetmap-in-a-commercial-product

#Get geocodes
load("C:/Users/dexur1/Desktop/James Pitt/zz-Datasets/Geocodes/ConnecticutGeocodes.Rdata")
#Match geocodes to hospitals

data$lon <- NA
data$lat <- NA
for (i in seq_along(unique(data$Hospital))){
  
  data[data$Hospital==unique(data$Hospital)[i],]$lon <- geocodes$lon[i]
  data[data$Hospital==unique(data$Hospital)[i],]$lat <- geocodes$lat[i]
}

library(lattice)
#Make a stripplot with data for all the hospitals in grey
p <- stripplot(`30 Day COPD Readmission Rate` ~ DRG, 
               data=data, col = "grey", pch = 20, cex = 5, alpha=0.5,
               jitter=TRUE)
#Make a copy of p for each hospital
p <- mget(rep("p", length(unique(data$Hospital))))
#For each hospital, make a vector of 108 grey color points, 
#and change the proper 4 points red
clr <- rep("grey", length(data$Hospital))
p <- lapply(1:length(p), function(i) {
  clr[i*4 -3] <- "blue"
  clr[i*4 -2] <- "blue"
  clr[i*4 -1] <- "blue"
  clr[i*4] <- "blue"
  update(p[[i]], col = clr)
})

#data[c(1:4),c(1,7,12)]

m <- leaflet() %>%
  addTiles() %>%  
  setView(lng=-72.935687, lat=41.304666, zoom = 16) %>%
  addMarkers(lng=unique(data$lon), lat=unique(data$lat), 
             label = htmltools::htmlEscape(unique(data$Hospital)),
             popup=popupGraph(p)) %>% setMaxBounds(-74.5, 41, -70.5, 43)
#Set fairly generous map bounds so it doesn't bounce you away when you try to read a popup
#from a hospital at the edge.


###Refinement:
popupTable(breweries,
           zcol = c("brewery",
                    "village",
                    "founded")))
poptab2 <- sub("<head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"></head>", "", poptab)
mapview(breweries[1, ], popup = poptab2)
#Removing head from poptab makes little difference
poptab2 <- sub("<body><div class=\"scrollableContainer\">", "", poptab2)
poptab2 <- sub("</div></body>", "", poptab2)
poptab2 <- sub('class=\"popup scrollable\" id=\"popup\"', '', poptab2) #This one DOES kill the nice formatting

mapview(breweries[1, ], popup = popgrph2)
popgrph2 <- str_replace(popgrph[[1]], "<head>.+</head>", "")
#Removing the head element makes the shape a bit weird but seems to cause no trouble.
popgrph2 <- str_replace(popgrph2[[1]], "<body>  <div class=\"scrollableContainer\"> ", "")
popgrph2 <- str_replace(popgrph2[[1]], "</div> </body>", "")
popgrph2 <- str_replace(popgrph2[[1]], '<table class=\"popup scrollable\" id=\"popup\">  ', '')
popgrph2 <- str_replace(popgrph2[[1]], '</table>  ', '')

#A couple pages to try linking:
#https://dexur.com/md/5002582/
#https://dexur.com/md/5002581/
#https://dexur.com/md/5002580/
<b>Yale New Haven</b><br><a href='https://dexur.com/md/5020163/'>Link to med device page</a>