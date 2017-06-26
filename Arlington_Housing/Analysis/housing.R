# Analysis of Housing Data in Arlington, VA
# Author: Randy Smith

library(tidyverse)
library(lubridate)
library(zoo)
library(rgdal)
library(ggmap)
library(ggthemes)
source('G:/DC Policy Center/Handy Scripts/fivethirtyeight.r')

# Read in data from Arlington Open Data
real_estate_log <- read.csv('https://data.arlingtonva.us/rest/datastreams/231872/data.csv', 
                            stringsAsFactors = FALSE) 
  
  
# Formating Dates
# Prep addresses for geocoding
house_sales <- eal_estate_log %>% 
  mutate(Year = year(saleDate),
         Month = month(saleDate),
         Day = day(saleDate),
         ym = as.Date(as.yearmon(saleDate)),
         address = stringr::str_c(propertyStreetNbrNameText, 'Arlington, VA', sep = ", "))


# single address geocode with data sciences toolkit
geocode_dsk <- function(addr){ 
  require(httr)
  require(rjson)
  url      <- "http://www.datasciencetoolkit.org/maps/api/geocode/json"
  response <- GET(url,query = list(sensor = "FALSE", address = addr))
  json <- fromJSON(content(response, type = "text"))
  loc  <- json['results'][[1]][[1]]$geometry$location
  return(c(address = addr,long = loc$lng, lat = loc$lat))
}
result <- do.call(rbind, lapply(as.character(house_sales$address), geo.dsk))
result <- data.frame(result) %>% 
  select(-(address))
write.csv(result, 'G:/ARLnow/Housing/Data/Tab/result.csv', row.names = FALSE)
result <- read_csv("G:/ARLnow/Housing/Data/Tab/result.csv")


# Bind Long/Lat to sales data
# Filter out commercial sales, $0 transfers, foreclosures
# Filter addresses with no house number
house_sales_geo <- cbind(house_sales, result) %>% 
  filter(address != ', Arlington, VA',
         saleAmt > 0 & saleAmt <= 3400000,
         salesTypeDsc != 'T-Transfer of Development Righ',
         salesTypeCode != '1',
         salesTypeCode != 'L',
         salesTypeCode != '4',
         deedBookNbr != '4543') %>% 
  arrange(propertyStreetNbrNameText) %>% 
  head(34895)



#-------------------------------------------------------------------------------------------------------------
# Spatial Join to add 

sale <- house_sales_geo %>% 
  select(long, lat)

hood <- readOGR(dsn = "G:/DC Policy Center/Car2Go/Data/Spatial/dc_arlington_hoods.GeoJSON", layer = "OGRGeoJSON")

# Add pickup/dropff points to Spatial points
addAll <- SpatialPoints(sale, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Perfom Spatial Join
saleHood <- over(addAll, hood)

house_sales_NBH <- cbind(house_sales_geo, saleHood)


#-------------------------------------------------------------------------------------------------------------
# Calculating trends and Stats

# Yearly average sales price by neighborhood
sales_NBH_year <- house_sales_NBH %>% 
  group_by(Year, subhood, id) %>% 
  summarise(sales = n(),
            avg_sale_amt = mean(saleAmt),
            std = sd(saleAmt)) %>% 
  arrange(subhood, Year)

# Percent change in average sales prices since 2016
sales_NBH_change <- sales_NBH_year %>%
  select(-(sales), -(std)) %>% 
  spread(key = Year, value = "avg_sale_amt") %>% 
  mutate(change = (`2016` - `2007`) / `2007` * 100) %>%  
  select(-(`2006`:`2017`))
write.csv(sales_NBH_change, 'G:/ARLnow/Housing/Data/Tab/Tidy/nbh_change.csv')


# Forclosures by month
foreclosures <- house_sales %>% 
  filter(salesTypeCode == '1') %>% 
  group_by(ym) %>% 
  summarise(count = n(),
            sale_amt = mean(saleAmt),
            std = sd(saleAmt))

ggplot(foreclosures, aes(ym, count)) +
  geom_line(size = 1, color = "#000000") +
  geom_smooth(se = FALSE, size = 1.5, color = "#527394") +
  labs(x = "Year", y = "Total Forclosures",
       title = "Total Monthly Foreclosures",
       subtitle = "Arlington County, November 2006 to June 2017",
       caption = "Source: data.arlingtonva.us\nRandy Smith\nwww.rhsmithjr.com") +
  scale_x_date(labels = date_format("%Y"), date_breaks = "1 year") +
  theme_fivethirtyeight()


